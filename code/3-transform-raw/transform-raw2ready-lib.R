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
           arquivo_props_input = "data/inputs/3-transform-input/proposicoes/proposicoes_input.csv",
           arquivo_destaques_tramitacao = "data/raw/leggo_data/proposicoes_destaques.csv") {
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
      rename(nome_proposicao = proposicao) %>%
      filter(lubridate::year(data_apresentacao) >= 2019) %>% 
      mutate(status = if_else(status == "Lei", "Aprovada", status))
    
    n_cruzado = proposicoes_tudo %>% pull(id_leggo) %>% n_distinct()
    flog.info(str_glue("{n_cruzado} proposições após cruzar input e leggo"))
    
    proposicoes_com_tram = cruza_destaques_tramitacao(proposicoes_tudo, arquivo_destaques_tramitacao)
    
    proposicoes_com_tram
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
    select(
      nome,
      id_entidade,
      partido,
      uf,
      casa,
      sigla_tipo,
      proposicao = nome_proposicao,
      classificacao_ambientalismo,
      autores,
      governismo
    ) %>%
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
      positivas = sum(classificacao_ambientalismo == "Positivo", na.rm = T),
      negativas = sum(classificacao_ambientalismo == "Negativo", na.rm = T),
      neutras = sum(classificacao_ambientalismo == "Neutro", na.rm = T),
      .groups = "drop"
    )
}

transform_atuacao <-
  function(atuacao_file = "data/raw/leggo_data/autorias.csv",
           parlamentares) {
    atuacao = read_atuacao_raw(atuacao_file)
    parlamentares = parlamentares %>%
      select(id_entidade_parlametria, partido, uf, governismo, peso_politico)
    
    atuacao %>%
      left_join(parlamentares,
                by = c("id_autor_parlametria" = "id_entidade_parlametria")) %>% 
      mutate(ano_apresentacao = lubridate::year(data)) %>% 
      filter(ano_apresentacao >= 2019, ano_apresentacao <= 2020) %>% 
      select(id_leggo, id_principal, casa, id_documento, sigla, descricao_tipo_documento, 
             data, id_autor_parlametria, partido, uf, nome_eleitoral, casa_autor, 
             tipo_documento, tipo_acao, peso_autor_documento, governismo, peso_politico)
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

#' Transforma critérios para destaques de tramitação. Principais colunas resultado
#' são urgencia, avanco e status. Juntos eles ajudam a dar uma visão do avanço das proposições.
#'
cruza_destaques_tramitacao <- function(props,
                                       destaques_file = "data/raw/leggo_data/proposicoes_destaques.csv") {
  destaques_raw = read_destaques_raw(destaques_file)
  criterios = destaques_raw %>%
    transmute(
      id_leggo = id_leggo,
      urgencia = case_when(
        criterio_req_urgencia_aprovado ~ "Urgência aprovada",
        criterio_req_urgencia_apresentado ~ "Urgência apresentada",
        TRUE ~ "Sem urgência"
      ),
      avanco = case_when(
        criterio_aprovada_em_uma_casa ~ "Avançou em uma casa",
        criterio_avancou_comissoes ~ "Avançou em comissões",
        TRUE ~ "Ainda no início"
      )
    )
  
  # Precisamos conferir quais já são leis
  destaques = criterios %>%
    right_join(props, by = "id_leggo")
  
  n_props = n_distinct(destaques$id_leggo)
  n_faltantes = destaques %>%
    mutate(falta = is.na(urgencia) |
             is.na(avanco)) %>%
    summarise(faltam = sum(falta)) %>%
    pull(faltam)
  
  flog.info(str_glue("{n_props} proposiçòes nos dados de destaques"))
  flog.info(str_glue("{n_faltantes} proposiçòes com destaques NA"))
  
  destaques
}

transform_votacoes <- function(acontecidas, rotuladas){
  
}
