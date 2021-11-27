remotes::install_github("curso-r/basesCursoR")
imdb <- basesCursoR::pegar_base("imdb_completa")
imdb_pessoas <- basesCursoR::pegar_base("imdb_pessoas")
imdb_avaliacoes <- basesCursoR::pegar_base("imdb_avaliacoes")

colSums(is.na(imdb))

dplyr::glimpse(imdb)
dplyr::glimpse(imdb_pessoas)
dplyr::glimpse(imdb_avaliacoes)


# -------------------------------------------------------------------------

1. Qual o mês do ano com o maior númedo de filmes? E o dia do ano?
  
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
