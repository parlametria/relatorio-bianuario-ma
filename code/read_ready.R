library(readr)

read_proposicoes = function() {
  read_csv(
    here::here("data/ready/proposicoes.csv"),
    col_types = cols(
      .default = col_character(),
      data_apresentacao = col_datetime(format = "")
    )
  )
}

read_autorias_res = function() {
  read_csv(
    here::here("data/ready/autorias-resumo.csv"),
    col_types = cols(
      .default = col_character(),
      em_exercicio = col_logical(),
      is_parlamentar = col_logical(),
      governismo = col_double(),
      assinadas = col_double(),
      peso_politico = col_double(),
      autorias_ponderadas = col_double()
    )
  )
}

read_autorias_det = function() {
  read_csv(
    here::here("data/ready/autorias-detalhes.csv"),
    col_types = cols(
      .default = col_character(),
      governismo = col_double(),
      assinadas = col_integer(),
      autorias_ponderadas = col_double(),
      coautores = col_integer()
    )
  )
}

read_atuacao <- function() {
  read_csv(
    here::here("data/ready/atuacao.csv"),
    col_types = cols(
      .default = col_character(),
      peso_total_documentos = col_double(),
      num_documentos = col_double(),
      governismo = col_double(),
      peso_politico = col_double(),
      is_important = col_logical()
    )
  )
}

read_relatoria <- function() {
  read_csv(
    here::here("data/ready/relatorias.csv"),
    col_types = cols(
      .default = col_character(),
      governismo = col_double(),
      peso_politico = col_double()
    )
  )
}