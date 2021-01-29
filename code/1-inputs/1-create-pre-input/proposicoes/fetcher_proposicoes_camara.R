library(tidyverse)
source(here::here("code/utils/check_packages.R"))
.check_and_load_perfilparlamentar_package()

#' @title Retorna autores de uma proposição
#' @description Recupera autores de uma proposição
#' @return Dataframe com informações dos autores das proposições
get_autores <- function(id_prop, casa = "camara") {
  print(id_prop)
  return(agoradigital::fetch_autores_documento(id_prop, casa))
}

#' @title Retorna dados de proposições apresentadas na Câmara em 2019 e 2020
#' @description Processa dados de proposições do tema de Meio Ambiente e Agricultura que foram
#' apresentadas em 20149 e 2020
#' @return Dataframe com informações das proposições
fetch_proposicoes_apresentadas_ma_camara <- function() {
  proposicoes_apresentadas_meio_ambiente <-
    fetch_proposicoes_apresentadas_camara(data_inicio = "2019-02-01",
                                          data_final = "2020-12-31",
                                          tema = 48)
  
  proposicoes_apresentadas_agricultura <-
    fetch_proposicoes_apresentadas_camara(data_inicio = "2019-01-01",
                                          data_final = "2020-12-31",
                                          tema = 64)
  
  proposicoes_ma_agric <- proposicoes_apresentadas_meio_ambiente %>% 
    rbind(proposicoes_apresentadas_agricultura) %>% 
    mutate(tema = if_else(tema == 48,
                          'Meio Ambiente e Desenvolvimento Sustentável',
                          'Agricultura, Pecuária, Pesca e Extrativismo')) %>%
    group_by(id) %>% 
    summarise(tema = paste0(tema, collapse = ";"),
              sigla_tipo = first(siglaTipo),
              cod_tipo = first(codTipo),
              numero = first(numero),
              ano = first(ano),
              ementa = first(ementa)) %>% 
    ungroup() %>% 
    mutate(id = as.character(id))
  
  return(proposicoes_ma_agric)
}

#' @title Retorna dados de proposições votadas em plenário na Câmara em 2019 e 2020
#' @description Processa dados de proposições que foram
#' votadas em plenário em 20149 e 2020
#' @return Dataframe com informações das proposições votadas
fetch_proposicoes_votadas_plenario_camara <- function() {
  proposicoes_votadas_2019 <-
    fetch_proposicoes_votadas_camara(ano = 2019)
  
  proposicoes_votadas_2020 <-
    fetch_proposicoes_votadas_camara(ano = 2020)
  
  proposicoes_votadas <- proposicoes_votadas_2019 %>%
    rbind(proposicoes_votadas_2020) %>%
    mutate(votada_plenario = 1) %>%
    distinct(id, votada_plenario)
  
  return(proposicoes_votadas)
}
