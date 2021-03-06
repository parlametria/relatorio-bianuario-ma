

#' Lê dados raw de detalhes dos parlamentares
#'
read_parlamentares_raw <-
  function(parlamentares_file = "data/raw/leggo_data/entidades.csv") {
    read_csv(
      here::here(parlamentares_file),
      col_types = cols(
        .default = col_character(),
        em_exercicio = col_double(),
        is_parlamentar = col_double(), 
        legislatura = col_double()
      )
    ) %>%
      filter(is_parlamentar == 1, legislatura == 56) %>% 
      select(-situacao) %>%
      group_by(id_entidade,id_entidade_parlametria,casa,nome,sexo,partido,uf, em_exercicio) %>% 
      summarise(legislatura = max(legislatura), .groups = "drop") %>% 
      distinct() %>% 
      select(-legislatura)
  }

read_governismo_raw <-
  function(deputados_file = "data/externo/governismo/governismo-ideal-deputados.csv",
           senadores_file = "data/externo/governismo/governismo-ideal-senadores.csv") {
    ideal_deputados = read_csv(here::here(deputados_file),
                               col_types = "cdd") %>%
      select(id_parlamentar, governismo = D1) %>%
      mutate(casa = "camara", governismo = -governismo)
    
    ideal_senadores = read_csv(here::here(senadores_file),
                                col_types = "cdd") %>%
      select(id_parlamentar, governismo = D1) %>%
      mutate(casa = "senado", governismo = -governismo)
    
    bind_rows(ideal_deputados, ideal_senadores) %>%
      group_by(casa) %>%
      mutate(governismo = scales::rescale(governismo, to = c(-10, 10))) %>%
      ungroup() %>%
      select(-casa)
  }

read_governismo_ma_raw <-
  function(governismo_ma_file = "data/raw/governismo/governismo_ma.csv") {
    governismo_ma_raw = read_csv(here::here(governismo_ma_file),
                                 col_types = "cddc")
    
    governismo_ma_camara = governismo_ma_raw %>% 
      filter(casa == "camara") %>% 
      select(id_parlamentar, casa, governismo_ma = D1) %>% 
      mutate(governismo_ma = -governismo_ma)
    
    governismo_ma_senado = governismo_ma_raw %>% 
      filter(casa == "senado") %>% 
      select(id_parlamentar, casa, governismo_ma = D1) %>% 
      mutate(governismo_ma = -governismo_ma)
    
    bind_rows(governismo_ma_camara, governismo_ma_senado) %>%
      group_by(casa) %>%
      mutate(governismo_ma = scales::rescale(governismo_ma, to = c(-10, 10))) %>%
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
      group_by(id_leggo) %>%
      mutate(status_collapsed = paste(status, collapse = ',')) %>%
      ungroup() %>%
      mutate(status_final = if_else(str_detect(status_collapsed, 'Lei'), 'Lei', status)) %>%
      filter(!duplicated(id_leggo)) %>%
      select(
        id_leggo,
        id_ext,
        sigla_tipo,
        numero,
        ementa,
        data_apresentacao,
        casa_origem,
        status = status_final
      )
  }

read_proposicoes_input_raw <- function(arquivo) {
  read_csv(here::here(arquivo),
           col_types = cols(.default = col_character()))
}

read_atuacao_raw <-
  function(arquivo = "data/raw/leggo_data/autorias.csv") {
    read_csv(
      here::here(arquivo),
      col_types = cols(
        .default = col_character(),
        peso_autor_documento = col_double(),
        data = col_datetime()
      )
    )
  }

read_relatoria_raw <-
  function(arquivo = "data/leggo_data/relatores_leggo.csv") {
    read_csv(here::here(arquivo),
             col_types = cols(.default = col_character()))
  }

read_destaques_raw <- function(arquivo) {
  read_csv(
    here::here(arquivo),
    col_types = cols(
      .default = col_character(),
      criterio_aprovada_em_uma_casa = col_logical(),
      criterio_avancou_comissoes = col_logical(),
      criterio_req_urgencia_apresentado = col_logical(),
      criterio_req_urgencia_aprovado = col_logical(),
      data_aprovacao = col_datetime(format = ""),
      data_req_urgencia_apresentado = col_datetime(format = ""),
      data_req_urgencia_aprovado = col_datetime(format = "")
    )
  )
}

read_votos_raw <- function(arquivo) {
  read_csv(
    here::here(arquivo),
    col_types = cols(
      .default = col_character()
    )
  )
}
