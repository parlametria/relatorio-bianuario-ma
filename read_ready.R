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
      autorias_ponderadas = col_double()
    )
  )
}

read_autorias_det = function() {
  read_csv(
    here::here("data/ready/autorias-detalhes.csv"),
    col_types = cols(
      .default = col_character(),
      em_exercicio = col_logical(),
      is_parlamentar = col_logical(),
      governismo = col_double(),
      assinadas = col_double(),
      autorias_ponderadas = col_double()
    )
  )
}

