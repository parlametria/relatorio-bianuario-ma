
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

read_autorias_raw <- function() {
  read_csv(
    here::here("data/leggo_data/autores_leggo.csv"), 
    col_types = cols(
      .default = col_character()
    )
  )
}

read_proposicoes_raw <- function(raw_data) {
  read_csv(
    here::here(raw_data),
    col_types = cols(
      .default = col_character(),
      data_apresentacao = col_datetime(format = "")
    )
  ) %>%
    filter(sigla_tipo == "PDL") %>%
    filter(!duplicated(id_leggo)) %>%
    select(id_leggo,
           sigla_tipo,
           numero,
           ementa,
           data_apresentacao,
           casa_origem,
           status)
}
