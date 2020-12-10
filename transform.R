library(tidyverse)
source(here::here("code/read_raw.R"))

#' Transforma dados das proposições acompanhadas para ready.
#'
#' @param raw_data Caminho do dado raw
#' @return As proposições salvas como ready
#'
transform_proposicoes <-
  function(raw_data = "data/leggo_data/proposicoes.csv") {
    proposicoes = read_csv(
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
    
    out_props = "data/ready/proposicoes.csv"
    proposicoes %>%  # temos uma linha por casa da proposição
      write_csv(here::here(out_props))
    message("Dados das proposições consideradas salvo em ", out_props)
    
    proposicoes
  }

#' Transforma autorias das proposições acompanhadas para ready.
#'
#' @param proposicoes Lista das proposições consideradas
#' @return As proposições salvas como ready
#'
transform_autorias <- function(proposicoes) {
  # Ler
  parlamentares_raw = read_parlamentares_raw()
  autores_leggo_raw = read_autorias_raw()
  governismo = read_governismo_raw()
  
  # Cruzar
  autores_leggo = autores_leggo_raw %>%
    inner_join(proposicoes, by = "id_leggo")
  
  parlamentares = parlamentares_raw %>%
    left_join(governismo,
              by = c("id_entidade" = "id_parlamentar"))
  
  autorias = autores_leggo %>%
    left_join(parlamentares,
              by = c("id_autor_parlametria" = "id_entidade_parlametria"))
  
  resumo_autores = autorias %>%
    resume_autorias()
  
  resumo_todos = parlamentares %>%
    filter(em_exercicio == 1) %>%
    left_join(resumo_autores, by = c("casa", "nome", "partido", "uf", "governismo")) %>%
    mutate(across(assinadas:autorias_ponderadas, replace_na, 0))
  
  # Salvar
  out_detalhes = "data/ready/autorias-detalhes.csv"
  out_resumo = "data/ready/autorias-resumo.csv"
  
  autorias %>%
    detalha_autorias() %>% 
  write_csv(here::here(out_detalhes))
  message("Detalhes de autorias em ", out_detalhes)
  
  resumo_todos %>%
    write_csv(here::here(out_resumo))
  message("Resumo de autorias em ", out_resumo)
}


detalha_autorias = function(data) {
  data %>%
    filter(!is.na(nome)) %>%
    mutate(proposicao = str_glue("{sigla_tipo} {numero}")) %>%
    group_by(id_leggo, proposicao) %>%
    mutate(autores = n()) %>%
    ungroup() %>%
    select(nome,
           id_entidade,
           partido,
           uf,
           casa,
           sigla_tipo,
           proposicao,
           autores,
           governismo) %>%
    mutate(
      assinadas = 1,
      autorias_ponderadas = 1 / autores,
      coautores = autores - 1
    )
}

resume_autorias = function(data) {
  data %>%
    detalha_autorias() %>%
    group_by(nome, partido, uf, casa, governismo) %>%
    summarise(
      assinadas = sum(assinadas),
      autorias_ponderadas = sum(autorias_ponderadas),
      .groups = "drop"
    )
}

props = transform_proposicoes()
transform_autorias(props)
