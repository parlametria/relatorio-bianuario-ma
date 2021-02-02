




#' Lê dados raw de detalhes dos parlamentares
#'
read_parlamentares_raw <-
  function(parlamentares_file = "data/raw/leggo_data/entidades.csv") {
    read_csv(
      here::here(parlamentares_file),
      col_types = cols(
        .default = col_character(),
        em_exercicio = col_double(),
        is_parlamentar = col_double()
      )
    ) %>%
      select(-legislatura) %>%
      distinct()
  }


read_governismo_raw <- function(deputados_file = "data/externo/governismo-ideal-deputados.csv",
                                senadores_file = "data/externo/governismo-ideal-senadores.csv") {
  ideal_deputados = read_csv(here::here(deputados_file),
                             col_types = "cdd") %>%
    select(id_parlamentar = id, governismo = d1) %>%
    mutate(casa = "camara", governismo = -governismo)
  
  ideal_senadores = read_csv2(here::here(senadores_file),
                              col_types = "cccccdddd") %>%
    select(id_parlamentar = id, governismo = ideal) %>%
    mutate(casa = "senado")
  
  bind_rows(ideal_deputados, ideal_senadores) %>%
    group_by(casa) %>%
    mutate(governismo = scales::rescale(governismo, to = c(-10, 10))) %>%
    ungroup() %>%
    select(-casa)
}

read_peso_raw <- function(peso_file) {
  read_csv(here::here(peso_file),
           col_types = "cd")
}

read_autorias_raw <-
  function(autores_leggo = "data/raw/leggo_data/autores_leggo.csv") {
    read_csv(here::here(autores_leggo),
             col_types = cols(.default = col_character()))
  }

read_proposicoes_raw <-
  function(data = "data/raw/leggo_data/proposicoes.csv") {
    read_csv(
      here::here(data),
      col_types = cols(
        .default = col_character(),
        data_apresentacao = col_datetime(format = "")
      )
    ) %>%
      filter(!duplicated(id_leggo)) %>%
      select(id_leggo,
             id_ext,
             sigla_tipo,
             numero,
             ementa,
             data_apresentacao,
             casa_origem,
             status)
  }

read_proposicoes_input_raw <- function(arquivo) {
  read_csv(here::here(arquivo),
           col_types = cols(.default = col_character()))
}

read_atuacao_raw <- function(arquivo = "data/raw/leggo_data/atuacao.csv") {
  read_csv(
    here::here(arquivo),
    col_types = cols(
      .default = col_character(),
      peso_total_documentos = col_double(),
      num_documentos = col_double(),
      is_important = col_logical()
    )
  )
}

read_relatoria_raw <- function(arquivo = "data/leggo_data/relatores_leggo.csv") {
  read_csv(
    here::here(arquivo),
    col_types = cols(.default = col_character())
  )
}