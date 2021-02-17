source(here::here("code/utils/check_packages.R"))
.check_and_load_perfilparlamentar_package()

#' @title Recupera dados dos votos para um conjunto de votações.
#' @description Recebe um caminho para o dataframe de votações e retorna todos os
#' votos relacionados.
#' @param votacoes_datapath Caminho para o csv de votações do Senado
#' @return Datafrane com os votos.
fetch_votos_senado <- function(
  votacoes_datapath = here::here("data/inputs/1-create-pre-input/votacoes/votacoes_senado.csv")) {
  library(tidyverse)  
  
  votacoes <- read_csv(votacoes_datapath, col_types=cols(.default = "c")) %>% 
    select(id_votacao, id_proposicao)
  
  
  votos <- purrr::map2_df(
    votacoes$id_proposicao,
    votacoes$id_votacao, 
    ~ fetch_votos_por_proposicao_votacao_senado(.x, .y))
  
  return(votos)
}