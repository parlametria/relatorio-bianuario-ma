


#' Lê dados raw de detalhes dos parlamentares
#'
read_parlamentares_raw <- function() {
  read_csv(
    here::here("data/leggo_data/entidades.csv"),
    col_types = cols(
      .default = col_character(),
      em_exercicio = col_double(),
      is_parlamentar = col_double()
    )
  ) %>%
    select(-legislatura) %>%
    distinct()
}


read_governismo_raw <- function() {
  ideal_deputados = read_csv(here::here("data/externo/governismo-ideal-deputados.csv"),
                             col_types = "cdd") %>%
    select(id_parlamentar = id, governismo = d1) %>%
    mutate(casa = "camara", governismo = -governismo)
  
  ideal_senadores = read_csv2(here::here("data/externo/governismo-ideal-senadores.csv"),
                              col_types = "cccccdddd") %>%
    select(id_parlamentar = id, governismo = ideal) %>%
    mutate(casa = "senado")
  
  bind_rows(ideal_deputados, ideal_senadores) %>%
    group_by(casa) %>%
    mutate(governismo = scales::rescale(governismo, to = c(-10, 10))) %>%
    ungroup() %>%
    select(-casa)
}

read_peso_raw <- function() {
  read_csv(here::here("data/raw/peso_politico/peso_politico.csv"),
           col_types = "cd")
}

read_autorias_raw <- function() {
  read_csv(
    here::here("data/leggo_data/autores_leggo.csv"),
    col_types = cols(.default = col_character())
  )
}

read_proposicoes_raw <-
  function(raw_data = "data/leggo_data/proposicoes.csv") {
    read_csv(
      here::here(raw_data),
      col_types = cols(
        .default = col_character(),
        data_apresentacao = col_datetime(format = "")
      )
    ) %>%
      filter(!duplicated(id_leggo)) %>%
      select(id_leggo,
             sigla_tipo,
             numero,
             ementa,
             data_apresentacao,
             casa_origem,
             status)
  }

read_proposicoes_input_raw <- function() {
  read_csv(
    here::here("data/raw/proposicoes_input.csv"),
    col_types = cols(.default = col_character())
  )
}

read_atuacao_raw <- function() {
  read_csv(
    here::here("data/leggo_data/atuacao.csv"),
    col_types = cols(
      .default = col_character(),
      peso_total_documentos = col_double(),
      num_documentos = col_double(),
      is_important = col_logical()
    )
  )
}