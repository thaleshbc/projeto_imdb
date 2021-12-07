remotes::install_github("curso-r/basesCursoR")
imdb <- basesCursoR::pegar_base("imdb_completa")
imdb_pessoas <- basesCursoR::pegar_base("imdb_pessoas")
imdb_avaliacoes <- basesCursoR::pegar_base("imdb_avaliacoes")

library(magrittr)

colSums(is.na(imdb))

dplyr::glimpse(imdb)
dplyr::glimpse(imdb_pessoas)
dplyr::glimpse(imdb_avaliacoes)


imdb <- imdb %>%
  tidyr::separate(
    col = receita,
    into = c("moeda_receita", "valor_receita"),
    sep = " ") %>%
  tidyr::separate(
    col = orcamento,
    into = c("moeda_orcamento", "valor_orcamento"),
    sep = " ") %>%
  dplyr::mutate(
    moeda_receita = dplyr::if_else(moeda_receita == "$", "USD", moeda_receita),
    moeda_orcamento = dplyr::if_else(moeda_orcamento == "$", "USD", moeda_orcamento),
    valor_receita = as.numeric(valor_receita),
    valor_orcamento = as.numeric(valor_orcamento),
    lucro = valor_receita - valor_orcamento,
    data_lancamento = lubridate::as_date(data_lancamento)
  )

imdb_pessoas <- imdb_pessoas %>%
  dplyr::mutate(
    data_nascimento = lubridate::as_date(data_nascimento),
    data_falecimento = lubridate::as_date(data_falecimento)
  )



# -------------------------------------------------------------------------

# 1. Qual o mês do ano com o maior número de filmes? E o dia do ano?

# Tabela com a quantidade de filmes lançados por mes em ordem decrescente
imdb %>%
  dplyr::mutate(
    mes = lubridate::month(data_lancamento, label = TRUE)
  ) %>%
  dplyr::filter(!is.na(data_lancamento)) %>%
  dplyr::count(mes) %>%
  dplyr::arrange(desc(n))

# Tabela com a qauntidade de filmes lançados por mes em ordem decrescente
imdb %>%
  dplyr::mutate(
    dia = lubridate::day(data_lancamento)
  ) %>%
  dplyr::filter(!is.na(data_lancamento)) %>%
  dplyr::count(dia) %>%
  dplyr::arrange(desc(n))

# Treemap com a quantidade de filmes lançados por mes
imdb %>%
  dplyr::mutate(
    mes = lubridate::month(data_lancamento, label = TRUE, abbr = FALSE),
    mes = stringr::str_to_title(mes)
  ) %>%
  dplyr::filter(!is.na(data_lancamento)) %>%
  dplyr::count(mes) %>%
  dplyr::rename(quantidade = n) %>%
  ggplot2::ggplot(ggplot2::aes(
    fill = quantidade,
    area = quantidade,
    label = paste(mes, quantidade, sep = "\n"))
  ) +
  treemapify::geom_treemap() +
  treemapify::geom_treemap_text(
    color = 'white',
    size = 1,
    grow = TRUE
  ) +
  ggplot2::labs(
    title = 'Quantidade de filmes lançados por mês',
    caption = 'Fonte: IMDB',
    fill = "Quantidade"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = 22,
      face = "bold"),
    plot.caption = ggplot2::element_text(face = "italic"),
  ) +
  ggplot2::scale_fill_viridis_c()

# Grafico de linha com a quantidade de filmes lançados por dia
imdb %>%
  dplyr::mutate(
    dia = lubridate::day(data_lancamento)
  ) %>%
  dplyr::filter(!is.na(data_lancamento)) %>%
  dplyr::count(dia) %>%
  dplyr::rename(quantidade = n) %>%
  ggplot2::ggplot(ggplot2::aes(x = dia, y = quantidade)) +
  ggplot2::geom_line(
    stat = 'identity',
    size = 1,
    color = '#7FFFD4'
  ) +
  ggplot2::geom_point(
    stat = 'identity',
    size = 3,
    color = '#66CDAA'
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = quantidade),
    hjust = 1.2,
    vjust = 0,
    size = 3
  ) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = 'Quantidade de filmes lançados por dia',
    x = 'Dia',
    y = 'Quantidade',
    caption = 'Fonte: IMDB'
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = 22,
      face = "bold"),
    plot.caption = ggplot2::element_text(face = "italic"),
    axis.title.x = ggplot2::element_text(
      size = 12,
      face = "bold"),
    axis.title.y = ggplot2::element_text(
      size = 12,
      face = "bold"),
    axis.text.x = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(size = 12)
  )

# Tabela agrupada por dia para sumarizar o total de lucro quando a receita e o
# orcamento sao em USD
imdb %>%
  dplyr::mutate(
    dia = lubridate::day(data_lancamento)
  ) %>%
  dplyr::group_by(dia) %>%
  dplyr::filter(moeda_receita == 'USD' & moeda_orcamento == 'USD') %>%
  dplyr::summarise(
    lucro_por_dia = sum(lucro, na.rm = TRUE)
  ) %>%
  dplyr::select(dia, lucro_por_dia)

# Nuvem de palavras com o totalde lucro por mes, onde a label são os meses.
imdb %>%
  dplyr::mutate(
    mes = lubridate::month(data_lancamento,
                           label = TRUE,
                           abbr = FALSE),
    mes = stringr::str_to_title(mes)
  ) %>%
  dplyr::group_by(mes) %>%
  dplyr::filter(moeda_receita == 'USD' & moeda_orcamento == 'USD') %>%
  dplyr::summarise(
    lucro_por_dia = sum(lucro, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    angle = 90 * sample(c(0, 1), dplyr::n(), replace = TRUE, prob = c(60, 40))
  ) %>%
  dplyr::filter(!is.na(mes)) %>%
  ggplot2::ggplot(ggplot2::aes(label = mes,
                               size = lucro_por_dia,
                               angle = angle,
                               color = lucro_por_dia)
  ) +
  ggwordcloud::geom_text_wordcloud_area(
    shape = 'square',
    seed = 100,
    area_corr_power = 1/0.5
  ) +
  ggplot2::scale_size_area(max_size = 18) +
  ggplot2::scale_color_gradient(low = '#0000FF', high = '#FF0000') +
  ggplot2::theme_bw()

# -------------------------------------------------------------------------

# 2. Qual o top 5 países com mais filmes na base?

imdb %>%
  dplyr::count(pais) %>%
  dplyr::arrange(desc(n)) %>%
  head(50)

sf_world <- giscoR::gisco_get_countries()

qte_filmes_pais <- imdb %>%
  tidyr::separate(
    col = pais,
    into = c("pais", "pais_2", "pais_3", "pais_4"),
    sep = ","
  ) %>%
  dplyr::mutate(
    pais_2 = NULL,
    pais_3 = NULL,
    pais_4 = NULL
  ) %>%
  dplyr::mutate(
    pais = dplyr::case_when(
      pais == "USA" ~ "United States",
      pais == "UK" ~ "United Kingdom",
      pais == "Tanzania" ~ "United Republic of Tanzania",
      pais == "West Germany" ~ "Germany",
      pais == "East Germany" ~ "Germany",
      pais == "Federal Republic of Yugoslavia" ~ "Yugoslavia",
      pais == "Soviet Union" ~ "Russian Federation",
      pais == "Russia" ~ "Russian Federation",
      pais == "The Democratic Republic Of Congo" ~ "Democratic Republic of The Congo",
      pais == "Korea" ~ "South Korea",
      pais == "Serbia and Montenegro" ~ "Montenegro",
      pais == "Côte d'Ivoire" ~ "Côte D’Ivoire",
      pais == "Isle Of Man" ~ "Isle of Man",
      pais == "North Vietnam" ~ "Vietnam",
      pais == "Myanmar" ~ "Myanmar/Burma",
      pais == "Republic of North Macedonia" ~ "North Macedonia",
      pais == "Czechoslovakia" ~ "Czechia",
      pais == "Czech Republic" ~ "Czechia",
      pais == pais ~ pais
    )
  ) %>%
  dplyr::rename(NAME_ENGL = pais) %>%
  dplyr::count(NAME_ENGL) %>%
  dplyr::full_join(sf_world) %>%
  dplyr::rename(
    pais = NAME_ENGL,
    qte_filmes = n
  ) %>%
  dplyr::mutate(
    qte_filmes = dplyr::coalesce(qte_filmes, 0)
  ) %>%
  dplyr::filter(!is.na(pais)) %>%
  dplyr::filter(pais != "Yugoslavia") %>%
  dplyr::arrange(desc(qte_filmes))

ggplot2::ggplot(qte_filmes_pais, ggplot2::aes(fill = qte_filmes)) +
  ggplot2::geom_sf(ggplot2::aes(geometry = geometry)) +
  ggplot2::scale_fill_gradient(
    low = "#FFFAFA",
    high = "#8B0000"
  ) +
  ggplot2::labs(
    title = "Os 5 paises com mais filmes",
    caption = "Fonte: IMDB",
    fill = "Qte_Filmes"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = 22,
      face = "bold"),
    plot.caption = ggplot2::element_text(face = "italic")
  ) +
  gghighlight::gghighlight(qte_filmes > 3500)

# ggplot2::coord_sf(
#   xlim = c(-20, 40),
#   ylim = c(30, 80),
#   expand = TRUE
# ) +

sf_europe <- giscoR::gisco_get_countries(region = 'Europe')

qte_filmes_europe <- imdb %>%
  tidyr::separate(
    col = pais,
    into = c("pais", "pais_2", "pais_3", "pais_4"),
    sep = ","
  ) %>%
  dplyr::mutate(
    pais_2 = NULL,
    pais_3 = NULL,
    pais_4 = NULL
  ) %>%
  dplyr::mutate(
    pais = dplyr::case_when(
      pais == "USA" ~ "United States",
      pais == "UK" ~ "United Kingdom",
      pais == "Tanzania" ~ "United Republic of Tanzania",
      pais == "West Germany" ~ "Germany",
      pais == "East Germany" ~ "Germany",
      pais == "Federal Republic of Yugoslavia" ~ "Yugoslavia",
      pais == "Soviet Union" ~ "Russian Federation",
      pais == "Russia" ~ "Russian Federation",
      pais == "The Democratic Republic Of Congo" ~ "Democratic Republic of The Congo",
      pais == "Korea" ~ "South Korea",
      pais == "Serbia and Montenegro" ~ "Montenegro",
      pais == "Côte d'Ivoire" ~ "Côte D’Ivoire",
      pais == "Isle Of Man" ~ "Isle of Man",
      pais == "North Vietnam" ~ "Vietnam",
      pais == "Myanmar" ~ "Myanmar/Burma",
      pais == "Republic of North Macedonia" ~ "North Macedonia",
      pais == "Czechoslovakia" ~ "Czechia",
      pais == "Czech Republic" ~ "Czechia",
      pais == pais ~ pais
    )
  ) %>%
  dplyr::rename(NAME_ENGL = pais) %>%
  dplyr::count(NAME_ENGL) %>%
  dplyr::full_join(sf_europe) %>%
  dplyr::rename(
    pais = NAME_ENGL,
    qte_filmes = n
  ) %>%
  dplyr::mutate(
    qte_filmes = dplyr::coalesce(qte_filmes, 0)
  ) %>%
  dplyr::filter(!is.na(pais)) %>%
  dplyr::filter(pais != "Yugoslavia") %>%
  dplyr::filter(!is.na(FID)) %>%
  dplyr::arrange(desc(qte_filmes))

ggplot2::ggplot(qte_filmes_europe, ggplot2::aes(fill = qte_filmes)) +
  ggplot2::geom_sf(ggplot2::aes(geometry = geometry)) +
  ggplot2::theme_bw() +
  ggplot2::scale_fill_gradient(
    low = '#FFD700',
    high = '#8B0000'
  ) +
  ggplot2::labs(
    title = "Países com mais filmes na Europa",
    caption = "Fonte: IMDB",
    fill = "Quantidade \nFilmes"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = 22,
      face = "bold"),
    plot.caption = ggplot2::element_text(face = "italic")
  ) +
  ggplot2::coord_sf(
    xlim = c(-30, 70),
    ylim = c(30, 85),
    expand = TRUE
  )
#gghighlight::gghighlight(qte_filmes < 30000)


# -------------------------------------------------------------------------

# 3. Liste todas as moedas que aparecem nas colunas `orcamento` e `receita` da base `imdb_completa`.

imdb %>%
  dplyr::select(moeda_orcamento) %>%
  dplyr::filter(!is.na(moeda_orcamento)) %>%
  dplyr::distinct(moeda_orcamento)

imdb %>%
  dplyr::count(moeda_orcamento) %>%
  dplyr::filter(!is.na(moeda_orcamento)) %>%
  dplyr::arrange(desc(n))


imdb %>%
  dplyr::select(moeda_receita) %>%
  dplyr::filter(!is.na(moeda_receita)) %>%
  dplyr::distinct(moeda_receita)

imdb %>%
  dplyr::count(moeda_receita) %>%
  dplyr::filter(!is.na(moeda_receita)) %>%
  dplyr::arrange(desc(n))

# -------------------------------------------------------------------------

# 4. Considerando apenas orçamentos e receitas em dólar ($), qual o gênero com maior lucro? E com maior nota média?

imdb %>%
  dplyr::filter(!is.na(lucro)) %>%
  dplyr::filter(moeda_receita == "USD" & moeda_orcamento == "USD") %>%
  dplyr::select(genero, lucro) %>%
  dplyr::arrange(desc(lucro))

imdb %>%
  dplyr::select(genero, nota_imdb) %>%
  dplyr::group_by(genero) %>%
  dplyr::summarise(
    nota_media = mean(nota_imdb, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(nota_media))



# -------------------------------------------------------------------------

# 5. Dentre os filmes na base `imdb_completa`, escolha o seu favorito. Então faça os itens a seguir:

imdb %>%
  dplyr::filter(titulo == "Interstellar")

  # a) Quem dirigiu o filme? Faça uma ficha dessa pessoa: idade (hoje em dia ou data de falecimento), onde nasceu, quantos filmes já dirigiu, qual o lucro médio dos filmes que dirigiu (considerando apenas valores em dólar) e outras informações que achar interessante (base `imdb_pessoas`).

imdb %>%
  dplyr::filter(titulo == "Interstellar") %>%
  dplyr::select(direcao)

imdb_pessoas %>%
  dplyr::filter(nome == "Christopher Nolan") %>%
  dplyr::mutate(
    idade = lubridate::as.period(Sys.Date() - data_nascimento),
    idade = idade / lubridate::years(1)
  ) %>%
  dplyr::select(idade)

  imdb_pessoas %>%
  dplyr::filter(nome == "Christopher Nolan") %>%
  dplyr::select(local_nascimento)

imdb %>%
  dplyr::count(direcao) %>%
  dplyr::filter(direcao == "Christopher Nolan")

imdb %>%
  dplyr::filter(direcao == "Christopher Nolan") %>%
  dplyr::select(ano, titulo) %>%
  dplyr::arrange(desc(ano))

imdb %>%
  dplyr::filter(!is.na(lucro)) %>%
  dplyr::filter(moeda_receita == "USD" & moeda_orcamento == "USD") %>%
  dplyr::filter(direcao == "Christopher Nolan") %>%
  dplyr::group_by(direcao) %>%
  dplyr::summarise(
    media_lucro = mean(lucro, na.rm = TRUE)
  )

  # b) Qual a posição desse filme no ranking de notas do IMDB? E no ranking de lucro (considerando apenas valores em dólar)?

imdb %>%
  dplyr::filter(titulo == "Interstellar") %>%
  dplyr::select(titulo, id_filme)

imdb_avaliacoes %>%
  dplyr::arrange(desc(nota_media)) %>%
  dplyr::mutate(
    ranking_nota = 1:nrow(imdb_avaliacoes)
  ) %>%
  dplyr::filter(id_filme == "tt0816692") %>%
  dplyr::select(ordem, nota_media)

tmp <- imdb %>%
  dplyr::filter(!is.na(lucro)) %>%
  dplyr::filter(moeda_receita == "USD" & moeda_orcamento == "USD") %>%
  dplyr::arrange(desc(lucro))

tmp %>%
  dplyr::mutate(
    ranking_lucro = 1:nrow(tmp)
  ) %>%
  dplyr::filter(titulo == "Interstellar") %>%
  dplyr::select(ranking_lucro, lucro)

rm('tmp')

  # c) Em que dia esse filme foi lançado? E dia da semana? Algum outro filme foi lançado no mesmo dia? Quantos anos você tinha nesse dia?

imdb %>%
  dplyr::filter(titulo == "Interstellar") %>%
  dplyr::mutate(
    dia_lancamento = lubridate::day(data_lancamento),
    dia_semana_lancamento = lubridate::wday(data_lancamento, label = TRUE)
  ) %>%
  dplyr::select(titulo, dia_lancamento, dia_semana_lancamento)

imdb %>%
  dplyr::mutate(
    dia_lancamento = lubridate::day(data_lancamento),
    dia_semana_lancamento = lubridate::wday(data_lancamento, label = TRUE)
  ) %>%
  dplyr::filter(dia_lancamento == 6) %>%
  dplyr::select(titulo, dia_lancamento, dia_semana_lancamento)

imdb  %>%
  dplyr::filter(titulo == "Interstellar") %>%
  dplyr::mutate(
    meu_aniversario = lubridate::as_date("1989-04-12"),
    idade_dia_lancamento = lubridate::as.period(data_lancamento - meu_aniversario),
    idade_dia_lancamento = idade_dia_lancamento / lubridate::years(1)
  ) %>%
  dplyr::select(titulo, data_lancamento, meu_aniversario, idade_dia_lancamento)

 # d) Faça um gráfico representando a distribuição da nota atribuída a esse filme por idade (base `imdb_avaliacoes`).

imdb_avaliacoes %>%
  dplyr::filter(id_filme == "tt0816692") %>%
  dplyr::select(
    nota_media_idade_0_18,
    nota_media_idade_18_30,
    nota_media_idade_30_45,
    nota_media_idade_45_mais
  ) %>%
  dplyr::rename(
    idade_0_18 = nota_media_idade_0_18,
    idade_18_30 = nota_media_idade_18_30,
    idade_30_45 = nota_media_idade_30_45,
    idade_45_mais = nota_media_idade_45_mais
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("idade"),
    names_to = "faixa_etaria",
    values_to = "nota_media") %>%
  ggplot2::ggplot(ggplot2::aes(x = faixa_etaria, y = nota_media)) +
  ggplot2::geom_bar(
    stat = "identity",
    width = 0.5,
    fill = "#6959CD") +
  ggplot2::geom_text(
    stat = "identity",
    ggplot2::aes(label = nota_media),
    vjust = 1.5,
    color = "white",
    size = 6,
  ) +
  ggplot2::labs(
    title = "Notas por faixa etária",
    x = "Faixa Etária",
    y = "Nota Média",
    caption = "Fonte: IMDB Avaliações"
  ) +
  ggplot2::scale_x_discrete(
    breaks = c("idade_0_18", "idade_18_30", "idade_30_45", "idade_45_mais"),
    labels = c("0 a 18", "18 a 30", "30 a 45", "45 +")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = 22,
      face = "bold"),
    plot.caption = ggplot2::element_text(face = "italic"),
    axis.title.x = ggplot2::element_text(
      size = 12,
      face = "bold"),
    axis.title.y = ggplot2::element_text(
      size = 12,
      face = "bold"),
    axis.text.x = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(size = 12)
  )




lubridate::period(5, "years" )
