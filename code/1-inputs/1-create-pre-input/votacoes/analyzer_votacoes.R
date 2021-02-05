library(tidyverse)
library(here)
source(here("code/utils/check_packages.R"))
source(here("code/1-inputs/1-create-pre-input/proposicoes/fetcher_proposicoes_camara.R"))
.check_and_load_perfilparlamentar_package()

#' @title Adiciona votações da planilha que não estão no dataframe de votações
#' @description Adiciona as votações faltantes no csv de votações, considerando a planilha
#' de votações relacionadas ao Meio Ambiente em 2019 e 2020.
#' @param votacoes_df Dataframe das votações retornadas pelo pacote perfilparlamentar
#' @param planilha_votacoes_path Caminho do csv contendo as votações preenchidas na planilha
#' @return Dataframe com o merge das votações
merge_votacoes_com_planilha_externa <- function(
  votacoes_df,
  planilha_votacoes_path = here::here("data/externo/planilha_votacoes/votacoes_meio_ambiente_camara.csv")) {
  
  votacoes_planilha <- read_csv(planilha_votacoes_path, col_types = cols(.default = "c")) %>% 
    anti_join(votacoes_df, by = "id_votacao")
  
  votacoes_planilha_info <- votacoes_planilha %>% 
    mutate(data = map(id_proposicoes,
                      perfilparlamentar::fetch_info_proposicao_camara)) %>%
    select(-id_proposicoes) %>% 
    unnest(data) %>% 
    select(
      nome_proposicao = nome,
      ementa_proposicao = ementa,
      obj_votacao = obj_possiveis,
      resumo = descricao_resultado,
      data = data_votacao,
      data_apresentacao_proposicao = data_apresentacao,
      autor,
      indexacao_proposicao = indexacao,
      tema,
      uri_tramitacao,
      id_proposicao = id,
      id_votacao,
      descricao_efeitos
    )
  
  votacoes <- votacoes_df %>% 
    bind_rows(votacoes_planilha_info)
  
  return(votacoes)
}

#' @title Votações nominais em plenário de Meio Ambiente e Agricultura
#' @description Processa informações de votações em plenário relacionadas a proposições dos temas de Meio Ambiente e Agricultura
#' @return Informações sobre as votações
#' @examples
#' votacoes <- processa_votacoes_camara()
processa_votacoes_camara <- function() {
  ## Marcando quais as proposições tiveram votações nominais em plenário em 2019 e 2020
  proposicoes_votadas <- fetch_proposicoes_votadas_plenario_camara()
  
  proposicoes <- proposicoes_votadas %>% 
    distinct(id)
  
  proposicoes_info <-
    purrr::pmap_dfr(list(proposicoes$id), ~ fetch_info_proposicao_camara(..1)) %>%
    select(
      id_proposicao = id,
      nome_proposicao = nome,
      data_apresentacao_proposicao = data_apresentacao,
      ementa_proposicao = ementa,
      autor,
      indexacao_proposicao = indexacao,
      tema,
      uri_tramitacao
    )
  
  proposicoes_ma <- proposicoes_info %>% 
    filter(str_detect(tolower(tema), "meio ambiente|agricultura|estrutura fundiária")) %>% 
    group_by(id_proposicao) %>% 
    mutate(tema = paste0(tema, collapse = ";"),
              autor = paste0(autor, collapse = ";")) %>% 
    ungroup() %>% 
    distinct() 
  
  votacoes_proposicoes <- tibble(id_proposicao = proposicoes_ma$id_proposicao) %>%
    mutate(data = map(id_proposicao,
                      perfilparlamentar::fetch_votacoes_por_proposicao_camara)) %>%
    select(-id_proposicao) %>% 
    unnest(data) %>% 
    mutate(id_proposicao = as.character(id_proposicao))
  
  votacoes <- votacoes_proposicoes %>%
    left_join(proposicoes_info, by = c("id_proposicao")) %>%
    select(
      nome_proposicao,
      ementa_proposicao,
      obj_votacao,
      resumo,
      data,
      data_apresentacao_proposicao,
      autor,
      indexacao_proposicao,
      tema,
      uri_tramitacao,
      id_proposicao,
      id_votacao
    )
  
  votacoes_alt <- votacoes %>% 
    merge_votacoes_com_planilha_externa()
  
  return(votacoes_alt)
}


#' @title Votações nominais em plenário de Meio Ambiente e Agricultura
#' @description Processa informações de votações em plenário relacionadas a proposições dos temas de Meio Ambiente e Agricultura
#' @return Informações sobre as votações
#' @examples
#' votacoes <- processa_votacoes_senado()
processa_votacoes_senado <- function() {
  
  votacoes_senado <- fetch_proposicoes_votadas_senado(initial_date = "01/02/2019",
                                                      end_date = format(Sys.Date(), "%d/%m/%Y"))
  
  proposicoes <-
    purrr::map_df(
      votacoes_senado %>% 
        distinct(id_proposicao) %>%
        pull(id_proposicao), ~ fetch_info_proposicao_senado(.x)
    )
  
  # Remove proposições do tipo MSF e OFS
  proposicoes_filtradas <- proposicoes %>% 
    filter(!str_detect(tolower(nome), "msf|ofs"))
  
  proposicoes_ma <- proposicoes_filtradas %>% 
    filter(
      str_detect(
        tema,
        "Nao especificado|Meio ambiente|Agricultura, pecuária e abastecimento|Recursos hídricos"
      )
    ) 
  
  votacoes_filtradas <- votacoes_senado %>% 
    inner_join(proposicoes_ma,
               by = c("id_proposicao" = "id")) %>% 
    mutate(cod_sessao = "",
           hora = "") %>% 
    select(id_proposicao,
           id_votacao,
           nome_proposicao = nome,
           ementa_proposicao = ementa,
           obj_votacao = objeto_votacao,
           votacao_secreta,
           resumo = link_votacao,
           tema,
           cod_sessao,
           hora,
           data = datetime,
           data_apresentacao_proposicao = data_apresentacao,
           autor,
           uri_tramitacao)
  
  return(votacoes_filtradas)
}