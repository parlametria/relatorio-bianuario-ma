library(tidyverse)
library(here)
source(here("code/utils/check_packages.R"))
source(here("code/proposicoes/fetcher_proposicoes_camara.R"))
.check_and_load_perfilparlamentar_package()

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
    filter(str_detect(tolower(tema), "meio ambiente|agricultura")) %>% 
    group_by(id_proposicao) %>% 
    mutate(tema = paste0(tema, collapse = ";"),
              autor = paste0(autor, collapse = ";")) %>% 
    ungroup() %>% 
    distinct() 
  
  votacoes_proposicoes <- tibble(id_proposicao = proposicoes_ma$id_proposicao) %>%
    mutate(data = map(id_proposicao,
                      fetch_votacoes_por_proposicao_camara)) %>%
    unnest(data) %>%
    mutate(id_votacao = paste0(cod_sessao, str_remove(hora, ":"))) %>%
    select(id_proposicao,
           id_votacao,
           obj_votacao,
           resumo,
           cod_sessao,
           hora,
           data)
  
  votacoes <- votacoes_proposicoes %>%
    left_join(proposicoes_info, by = c("id_proposicao")) %>%
    select(
      nome_proposicao,
      ementa_proposicao,
      obj_votacao,
      resumo,
      cod_sessao,
      hora,
      data,
      data_apresentacao_proposicao,
      autor,
      indexacao_proposicao,
      tema,
      uri_tramitacao,
      id_proposicao,
      id_votacao
    )
  
  return(votacoes)
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
  
  proposicoes_ma <- proposicoes %>% 
    filter(
      str_detect(
        tema,
        "Nao especificado|Meio ambiente|Agricultura, pecuária e abastecimento|Recursos hídricos"
      )
    ) 
  
  votacoes_filtradas <- votacoes_senado %>% 
    inner_join(proposicoes,
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