library(tidyverse)
library(futile.logger)
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
    n_leggo = proposicoes_leggo %>% pull(id_leggo) %>% n_distinct()
    flog.info(str_glue("{n_leggo} proposições nos dados do leggo"))
    
    proposicoes_input = read_proposicoes_input_raw(arquivo_props_input)
    n_input = proposicoes_input %>% pull(proposicao) %>% n_distinct()
    flog.info(str_glue("{n_input} proposições na planilha de input"))
    
    pi_long = pivot_longer(
      proposicoes_input,
      c(id_camara, id_senado),
      names_to = "tipo_id",
      values_to = "id_ext"
    ) %>% 
      filter(!is.na(id_ext)) %>% 
      select(-tipo_id)
    
    proposicoes_tudo = proposicoes_leggo %>% 
      inner_join(pi_long, by = c("id_ext")) %>% 
      rename(nome_proposicao = proposicao)
    
    ## CRUZAR PI_LONG COM PORPS_LEGGO DEVE DAR 1069 matches
    
    n_cruzado = proposicoes_tudo %>% pull(id_leggo) %>% n_distinct()
    flog.info(str_glue("{n_cruzado} proposições após cruzar input e leggo"))
    
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
           classificacao_ambientalismo,
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
      positivas = sum(classificacao_ambientalismo == "Positivo"),
      negativas = sum(classificacao_ambientalismo == "Negativo"),
      neutras = sum(classificacao_ambientalismo == "Neutro"),
      .groups = "drop"
    )
}

transform_atuacao <-
  function(atuacao_file = "data/raw/leggo_data/atuacao.csv",
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
