library(tidyverse)
library(here)
library(agoradigital)
source(here("code/utils/check_packages.R"))
source(here("code/proposicoes/fetcher_proposicoes_camara.R"))
source(here("code/proposicoes/fetcher_proposicoes_senado.R"))
.check_and_load_perfilparlamentar_package()

#' @title Processa dados de proposições apresentadas na Câmara em 2019 e 2020
#' @description Processa dados de proposições do tema de Meio Ambiente que foram apresentadas em
#' 2019 e 2020
#' @return Dataframe com informações das proposições
#' @examples
#' processa_proposicoes_camara()
processa_proposicoes_camara <- function() {
  .TIPOS_PROPOSICOES <- c("PDL", "PEC", "PL", "PLP", "PRS")
  
  proposicoes_ma_agric <- fetch_proposicoes_apresentadas_ma_camara()
  
  proposicoes_ma_agric_filtradas <- proposicoes_ma_agric %>%
    filter(sigla_tipo %in% .TIPOS_PROPOSICOES)
  
  ## Marcando quais as proposições tiveram votações nominais em plenário em 2019 e 2020
  proposicoes_votadas <- fetch_proposicoes_votadas_plenario_camara()
  
  proposicoes <- proposicoes_ma_agric_filtradas %>%
    left_join(proposicoes_votadas, by = "id") %>%
    mutate(votada_plenario = as.numeric(!is.na(votada_plenario)))
  
  proposicoes_autores <-
    purrr::pmap_dfr(list(proposicoes$id), ~ get_autores(..1)) %>% 
    select(-cod_tipo) %>% 
    group_by(id_documento) %>% 
    mutate(autor = paste(nome, collapse = "; ")) %>% 
    ungroup() %>% 
    select(id_documento, autor)
  
  ## Gerando CSV no formato final
  proposicoes_final <- proposicoes %>%
    inner_join(proposicoes_autores, by = c("id" = "id_documento")) %>%
    mutate(
      link = paste0(
        "https://www.camara.leg.br/proposicoesWeb/fichadetramitacao?idProposicao=",
        id
      ),
      casa = "camara"
    ) %>%
    select(sigla_tipo,
           numero,
           ano,
           ementa,
           votada_plenario,
           cod_tipo,
           id,
           casa,
           tema,
           autor,
           link) %>% 
    distinct()
  
  return(proposicoes_final)
}

#' @title Processa dados de proposições apresentadas no Senado em 2019 e 2020
#' @description Processa dados de proposições do tema de Meio Ambiente e Agricultura que foram
#' apresentadas em 20149 e 2020
#' @return Dataframe com informações das proposições
#' @examples
#' processa_proposicoes_senado()
processa_proposicoes_senado <- function() {

  .TIPOS_PROPOSICOES <- c("PDL", "PEC", "PL", "PLP", "PRS")
  proposicoes <- fetch_proposicoes_apresentadas_ma_senado()
  
  proposicoes_filtradas <- proposicoes %>%
    filter(sigla_tipo %in% .TIPOS_PROPOSICOES)
  
  proposicoes_info <-
    purrr::map2_df(proposicoes_filtradas$id, proposicoes_filtradas$rn,
                   function(x, y) {
                      print(y)
                      return(fetch_info_proposicao_senado(x))
                      }) %>% 
    group_by(id) %>%
    mutate(autor = paste(autor, collapse = "; ")) %>%
    ungroup()
  
  
  proposicoes_final <- proposicoes_filtradas %>% 
    inner_join(proposicoes_info, by = "id") %>%
    filter(
      str_detect(
        tema,
        "Nao especificado|Meio ambiente|Agricultura, pecuária e abastecimento|Recursos hídricos"
      )
    ) %>%
    mutate(casa = "senado") %>% 
    select(
      sigla_tipo,
      numero,
      ano,
      ementa,
      id,
      casa,
      tema,
      autor,
      link = uri_tramitacao
    )
  
  return(proposicoes_final)
}


#' @title Processa dados de proposições apresentadas na Câmara e no Senado em 2019 e 2020
#' @description Processa dados de proposições do tema de Meio Ambiente e Agricultura que foram
#' apresentadas em 20149 e 2020 no Congresso
#' @return Dataframe com informações das proposições
#' @examples
#' processa_proposicoes()
processa_proposicoes <- function() {
  proposicoes_camara <- processa_proposicoes_camara()
  proposicoes_senado <- processa_proposicoes_senado()
  
  proposicoes <- proposicoes_camara %>% 
    bind_rows(proposicoes_senado)
  
  return(proposicoes)
}