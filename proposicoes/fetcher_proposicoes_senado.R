library(tidyverse)
source(here::here("code/utils/check_packages.R"))
.check_and_load_perfilparlamentar_package()

#' @title Retorna dados de proposições apresentadas no Senado em 2019 e 2020
#' @description Processa dados de proposições do tema de Meio Ambiente e Agricultura que foram
#' apresentadas em 20149 e 2020
#' @return Dataframe com informações das proposições
fetch_proposicoes_apresentadas_ma_senado <- function() {
  datas_inicio <-
    seq(as.Date('2019-02-01'),
        length.out = 5,
        by = '6 month') %>%
    str_remove_all("-")
  
  datas_fim <-
    seq(as.Date('2019-08-01'),
        length.out = 5,
        by = '6 month') %>%
    str_remove_all("-")
  
  datas <-
    data.frame(inicio = datas_inicio,
               fim = datas_fim,
               stringsAsFactors = F)
  
  proposicoes <- purrr::pmap_df(
    list(datas$inicio,
         datas$fim),
    ~ fetcher_proposicoes_em_intervalo_senado(..1, ..2)
  )
  
  return(proposicoes)
}