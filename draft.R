remotes::install_github("curso-r/basesCursoR")
imdb <- basesCursoR::pegar_base("imdb_completa")
imdb_pessoas <- basesCursoR::pegar_base("imdb_pessoas")
imdb_avaliacoes <- basesCursoR::pegar_base("imdb_avaliacoes")

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
    moeda_receita = dplyr::if_else(moeda_receita == "$", "US$", moeda_receita),
    moeda_orcamento = dplyr::if_else(moeda_orcamento == "$", "US$", moeda_orcamento),
    valor_receita = as.numeric(valor_receita),
    valor_orcamento = as.numeric(valor_orcamento),
    lucro = valor_receita - valor_orcamento
  )


# -------------------------------------------------------------------------

# 1. Qual o mês do ano com o maior número de filmes? E o dia do ano?

imdb %>%
  dplyr::mutate(
    data_lancamento = lubridate::as_date(data_lancamento),
    mes = lubridate::month(data_lancamento, label = TRUE)
  ) %>%
  dplyr::filter(!is.na(data_lancamento)) %>%
  dplyr::count(mes) %>%
  dplyr::arrange(desc(n))

imdb %>%
  dplyr::mutate(
    data_lancamento = lubridate::as_date(data_lancamento),
    dia = lubridate::day(data_lancamento)
  ) %>%
  dplyr::filter(!is.na(data_lancamento)) %>%
  dplyr::count(dia) %>%
  dplyr::arrange(desc(n))


# -------------------------------------------------------------------------

# 2. Qual o top 5 países com mais filmes na base?

imdb %>%
  dplyr::count(pais) %>%
  dplyr::arrange(desc(n)) %>%
  head(5)


# -------------------------------------------------------------------------

# 3. Liste todas as moedas que aparecem nas colunas `orcamento` e `receita` da base `imdb_completa`.

imdb %>%
  dplyr::select(moeda_orcamento) %>%
  dplyr::filter(!is.na(moeda_orcamento)) %>%
  dplyr::distinct(moeda_orcamento)

imdb %>%
  dplyr::select(moeda_receita) %>%
  dplyr::filter(!is.na(moeda_receita)) %>%
  dplyr::distinct(moeda_receita)



# -------------------------------------------------------------------------

# 4. Considerando apenas orçamentos e receitas em dólar ($), qual o gênero com maior lucro? E com maior nota média?

imdb %>%
  dplyr::filter(!is.na(lucro)) %>%
  dplyr::filter(moeda_receita == "US$" & moeda_orcamento == "US$") %>%
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
    data_nascimento = lubridate::as_date(data_nascimento),
    data_falecimento = lubridate::as_date(data_falecimento),
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
  dplyr::filter(moeda_receita == "US$" & moeda_orcamento == "US$") %>%
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
  dplyr::filter(moeda_receita == "US$" & moeda_orcamento == "US$") %>%
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
    data_lancamento = lubridate::as_date(data_lancamento),
    dia_lancamento = lubridate::day(data_lancamento),
    dia_semana_lancamento = lubridate::wday(data_lancamento, label = TRUE)
  ) %>%
  dplyr::filter(dia_lancamento == 6) %>%
  dplyr::select(titulo, dia_lancamento, dia_semana_lancamento)

imdb  %>%
  dplyr::filter(titulo == "Interstellar") %>%
  dplyr::mutate(
    data_lancamento = lubridate::as_date(data_lancamento),
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
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(
    stat = "identity",
    ggplot2::aes(label = nota_media),
    vjust = 1.5,
    color = "white",
    size = 5) +
  ggplot2::labs(
    title = "Notas por faixa etária",
    x = "Faixa Etária",
    y = "Nota Média"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5)
  )




lubridate::period(5, "years" )
