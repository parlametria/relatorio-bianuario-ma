library(tidyverse)
source(here::here("code/read_raw.R"))

#' Transforma dados das proposições acompanhadas para ready.
#'
#' @param raw_data Caminho do dado raw
#' @return As proposições salvas como ready
#'
transform_proposicoes <-
  function(raw_data = "data/raw/leggo_data/proposicoes.csv",
           arquivo_props_input = "data/inputs/3-transform-input/proposicoes/proposicoes_input.csv") {
    proposicoes_leggo = read_proposicoes_raw(raw_data)
    proposicoes_input = read_proposicoes_input_raw(arquivo_props_input)
    
    proposicoes_tudo = proposicoes_leggo %>%
      mutate(
        nome_proposicao = str_glue(
          "{sigla_tipo} {numero}/{lubridate::year(data_apresentacao)}"
        )
      ) %>%
      inner_join(proposicoes_input, by = c("nome_proposicao" = "proposicao"))
    
    proposicoes_tudo
  }


#' Código comum para transform_autorias_*
#'
le_cruza_autorias <- function(proposicoes,
                              autores_file,
                              parlamentares) {
  # Ler
  autores_leggo_raw = read_autorias_raw(autores_file)
  
  # Cruzar
  autores_leggo = autores_leggo_raw %>%
    inner_join(proposicoes, by = "id_leggo")
  
  autorias = autores_leggo %>%
    left_join(parlamentares,
              by = c("id_autor_parlametria" = "id_entidade_parlametria"))
  
  autorias
}

#' Transforma autorias detalhada das proposições acompanhadas para ready.
#'
#' @param proposicoes Lista das proposições consideradas
#'
transform_autorias_detalhes <- function(proposicoes,
                                        autores_file,
                                        parlamentares) {
  autorias = le_cruza_autorias(proposicoes,
                               autores_file,
                               parlamentares)
  autorias %>%
    detalha_autorias()
}

#' Transforma autorias resumida das proposições acompanhadas para ready.
#'
#' @param proposicoes Lista das proposições consideradas
#'
transform_autorias_resumo <- function(proposicoes,
                                      autores_file,
                                      parlamentares) {
  autorias = le_cruza_autorias(proposicoes,
                               autores_file,
                               parlamentares)
  
  resumo_autores = autorias %>%
    resume_autorias()
  
  resumo_todos = parlamentares %>%
    filter(em_exercicio == 1) %>%
    left_join(resumo_autores,
              by = c("casa", "nome", "partido", "uf", "governismo")) %>%
    mutate(across(assinadas:autorias_ponderadas, replace_na, 0))
  
  resumo_todos
}

parlamentares_data <-
  function(parlamentares_file,
           governismo_deps_file,
           governismo_sens_file,
           peso_file) {
    parlamentares_raw = read_parlamentares_raw(parlamentares_file)
    governismo = read_governismo_raw(governismo_deps_file, governismo_sens_file)
    peso = read_peso_raw(peso_file)
    
    parlamentares_raw %>%
      left_join(governismo,
                by = c("id_entidade" = "id_parlamentar")) %>%
      left_join(peso,
                by = c("id_entidade_parlametria" = "id_parlamentar_parlametria"))
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
           proposicao = nome_proposicao,
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

transform_atuacao <- function(atuacao_file = "data/raw/leggo_data/atuacao.csv", 
                              parlamentares) {
  atuacao = read_atuacao_raw(atuacao_file)
  parlamentares = parlamentares %>%
    select(id_entidade_parlametria, governismo, peso_politico)
  
  atuacao %>%
    left_join(parlamentares,
              by = c("id_autor_parlametria" = "id_entidade_parlametria")) 
}

transform_relatorias <-
  function(props, relatorias_file, parlamentares) {
    relatorias = read_relatoria_raw(relatorias_file)
    
    t = relatorias %>%
      left_join(
        parlamentares,
        by = c(
          "relator_id_parlametria" = "id_entidade_parlametria",
          "casa",
          "relator_id" = "id_entidade"
        )
      ) %>%
      select(-situacao)
    
    props %>%
      left_join(t, by = "id_leggo") 
  }

filter_lei <-
  function() {
    proposicoes_leggo = read_proposicoes_raw()
    leis = proposicoes_leggo %>% filter(status == 'Lei')
  }


#' Main do script para uso em CLI.
#' Para uso interativo, chame main() no console
#'
main <- function(argv = NULL) {
  out_props = "data/ready/proposicoes.csv"
  out_autorias_detalhes = "data/ready/autorias-detalhes.csv"
  out_autorias_resumo = "data/ready/autorias-resumo.csv"
  out_relatorias = "data/ready/relatorias.csv"
  out_atuacao = "data/ready/atuacao.csv"
    
  # PROPOSIÇÕES
  props = transform_proposicoes(
    "data/raw/leggo_data/proposicoes.csv",
    "data/inputs/3-transform-input/proposicoes/proposicoes_input.csv"
  )
  
  props %>% # temos uma linha por casa da proposição
    write_csv(here::here(out_props))
  message("Dados prontos das proposições em ", out_props)
  
  # PARLAMENTARES
  parlamentares = parlamentares_data(
    parlamentares_file = "data/raw/leggo_data/entidades.csv",
    "data/externo/governismo/governismo-ideal-deputados.csv",
    "data/externo/governismo/governismo-ideal-senadores.csv",
    "data/raw/peso_politico/peso_politico.csv"
  )
  
  # AUTORIAS
  autorias = transform_autorias_detalhes(props,
                                         "data/raw/leggo_data/autores_leggo.csv",
                                         parlamentares)
  autorias %>%
    write_csv(here::here(out_autorias_detalhes))
  message("Detalhes de autorias em ", out_autorias_detalhes)
  
  autorias_resumo = transform_autorias_resumo(props,
                                              "data/raw/leggo_data/autores_leggo.csv",
                                              parlamentares)
  autorias_resumo %>%
    write_csv(here::here(out_autorias_resumo))
  message("Resumo de autorias em ", out_autorias_resumo)
  
  # RELATORIAS
  relatorias = transform_relatorias(props,
                                    "data/raw/leggo_data/relatores_leggo.csv",
                                    parlamentares)
  relatorias %>% 
    write_csv(here::here(out_relatorias))
  message("Relatorias salvas em ", out_relatorias)    
  
  # ATUAÇÃO
  atuacao = transform_atuacao("data/raw/leggo_data/atuacao.csv", 
                              parlamentares)
  atuacao %>%
    write_csv(here::here(out_atuacao))
  message("Atuação salva em ", out_atuacao)
}

if (!interactive()) {
  argv <- commandArgs(TRUE)
  main(argv)
}