remotes::install_github("curso-r/basesCursoR")
imdb <- basesCursoR::pegar_base("imdb_completa")
imdb_pessoas <- basesCursoR::pegar_base("imdb_pessoas")
imdb_avaliacoes <- basesCursoR::pegar_base("imdb_avaliacoes")

colSums(is.na(imdb))

dplyr::glimpse(imdb)
dplyr::glimpse(imdb_pessoas)
dplyr::glimpse(imdb_avaliacoes)



# -------------------------------------------------------------------------

1. Qual o mês do ano com o maior número de filmes? E o dia do ano?

imdb |>
  dplyr::mutate(
    data_lancamento = lubridate::as_date(data_lancamento),
    mes = lubridate::month(data_lancamento, label = TRUE)
  ) |>
  dplyr::filter(!is.na(data_lancamento)) |>
  dplyr::count(mes) |>
  dplyr::arrange(desc(n))

imdb |>
  dplyr::mutate(
    data_lancamento = lubridate::as_date(data_lancamento),
    dia = lubridate::day(data_lancamento)
  ) |>
  dplyr::filter(!is.na(data_lancamento)) |>
  dplyr::count(dia) |>
  dplyr::arrange(desc(n))


# -------------------------------------------------------------------------

2. Qual o top 5 países com mais filmes na base?

imdb |>
  dplyr::count(pais) |>
  dplyr::arrange(desc(n)) |>
  head(5)


# -------------------------------------------------------------------------

3. Liste todas as moedas que aparecem nas colunas `orcamento` e `receita` da base `imdb_completa`.

imdb |>
  dplyr::select(orcamento) |>
  dplyr::filter(!is.na(orcamento)) |>
  tidyr::separate(
    col = orcamento,
    into = c("moeda", "valor_orcamento"),
    sep = " ") |>
  dplyr::mutate(
    moeda = dplyr::if_else(moeda == "$", "US$", moeda)
  ) |>
  dplyr::distinct(moeda)

imdb |>
  dplyr::select(receita) |>
  dplyr::filter(!is.na(receita)) |>
  tidyr::separate(
    col = receita,
    into = c("moeda", "valor_receita"),
    sep = " ") |>
  dplyr::mutate(
    moeda = dplyr::if_else(moeda == "$", "US$", moeda)
  ) |>
  dplyr::distinct(moeda)



# -------------------------------------------------------------------------

4. Considerando apenas orçamentos e receitas em dólar ($), qual o gênero com maior lucro? E com maior nota média?

imdb |>
  tidyr::separate(
    col = receita,
    into = c("moeda_receita", "valor_receita"),
    sep = " ") |>
  tidyr::separate(
    col = orcamento,
    into = c("moeda_orcamento", "valor_orcamento"),
    sep = " ") |>
  dplyr::mutate(
    moeda_receita = dplyr::if_else(moeda_receita == "$", "US$", moeda_receita),
    moeda_orcamento = dplyr::if_else(moeda_orcamento == "$", "US$", moeda_orcamento),
    valor_receita = as.numeric(valor_receita),
    valor_orcamento = as.numeric(valor_orcamento),
    lucro = valor_receita - valor_orcamento
  ) |>
  dplyr::filter(!is.na(lucro)) |>
  dplyr::filter(moeda_receita == "US$" & moeda_orcamento == "US$") |>
  dplyr::select(genero, lucro) |>
  dplyr::arrange(desc(lucro))

imdb |>
  dplyr::select(genero, nota_imdb) |>
  dplyr::group_by(genero) |>
  dplyr::summarise(
    nota_media = mean(nota_imdb, na.rm = TRUE)
  ) |>
  dplyr::arrange(desc(nota_media))



# -------------------------------------------------------------------------

5. Dentre os filmes na base `imdb_completa`, escolha o seu favorito. Então faça os itens a seguir:

imdb |>
  dplyr::filter(titulo == "Interstellar")

  a) Quem dirigiu o filme? Faça uma ficha dessa pessoa: idade (hoje em dia ou data de falecimento), onde nasceu, quantos filmes já dirigiu, qual o lucro médio dos filmes que dirigiu (considerando apenas valores em dólar) e outras informações que achar interessante (base `imdb_pessoas`).

imdb |>
  dplyr::filter(titulo == "Interstellar") |>
  dplyr::select(direcao)

imdb_pessoas |>
  dplyr::filter(nome == "Christopher Nolan") |>
  dplyr::mutate(
    data_nascimento = lubridate::as_date(data_nascimento),
    data_falecimento = lubridate::as_date(data_falecimento),
    idade = Sys.Date() - data_nascimento
  ) |>
  dplyr::select(idade)

imdb_pessoas |>
  dplyr::filter(nome == "Christopher Nolan") |>
  dplyr::select(local_nascimento)

imdb |>
  dplyr::count(direcao) |>
  dplyr::filter(direcao == "Christopher Nolan")

imdb |>
  dplyr::filter(direcao == "Christopher Nolan") |>
  dplyr::select(ano, titulo) |>
  dplyr::arrange(desc(ano))

imdb |>
  tidyr::separate(
    col = receita,
    into = c("moeda_receita", "valor_receita"),
    sep = " ") |>
  tidyr::separate(
    col = orcamento,
    into = c("moeda_orcamento", "valor_orcamento"),
    sep = " ") |>
  dplyr::mutate(
    moeda_receita = dplyr::if_else(moeda_receita == "$", "US$", moeda_receita),
    moeda_orcamento = dplyr::if_else(moeda_orcamento == "$", "US$", moeda_orcamento),
    valor_receita = as.numeric(valor_receita),
    valor_orcamento = as.numeric(valor_orcamento),
    lucro = valor_receita - valor_orcamento
  ) |>
  dplyr::filter(!is.na(lucro)) |>
  dplyr::filter(moeda_receita == "US$" & moeda_orcamento == "US$") |>
  dplyr::filter(direcao == "Christopher Nolan") |>
  dplyr::group_by(direcao) |>
  dplyr::summarise(
    media_lucro = mean(lucro, na.rm = TRUE)
  )

  b) Qual a posição desse filme no ranking de notas do IMDB? E no ranking de lucro (considerando apenas valores em dólar)?

  c) Em que dia esse filme foi lançado? E dia da semana? Algum outro filme foi lançado no mesmo dia? Quantos anos você tinha nesse dia?

  d) Faça um gráfico representando a distribuição da nota atribuída a esse filme por idade (base `imdb_avaliacoes`).
