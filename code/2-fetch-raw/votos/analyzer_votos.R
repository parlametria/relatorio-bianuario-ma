library(tidyverse)
source(here::here("code/read_raw.R"))
source(here::here("code/2-fetch-raw/votos/fetcher_votos_senado.R"))
source(here::here("code/utils/check_packages.R"))
.check_and_load_perfilparlamentar_package()

#' @title Recupera dados dos votos para um conjunto de votações.
#' @description Recebe um caminho para o dataframe de votações e retorna todos os
#' votos relacionados.
#' @param votacoes_datapath Caminho para o csv de votações do Senado
#' @param parlamentares_datapath Caminho para o csv de entidades
#' @return Datafrane com os votos.
process_votos_senado <- function(
  votacoes_datapath = here::here("data/inputs/1-create-pre-input/votacoes/votacoes_senado.csv"),
  parlamentares_datapath = here::here("data/raw/leggo_data/entidades.csv")) {
  
  votos <- fetch_votos_senado(votacoes_datapath) %>%
    select(id_votacao, nome_eleitoral = senador, voto)
  
  votos <- votos %>% 
    mutate(nome_eleitoral = str_remove(nome_eleitoral, "^[:space:]*|[:space:]$"))
  
  parlamentares <- read_parlamentares_raw(parlamentares_datapath) %>%
    ungroup() %>% 
    filter(casa == 'senado') %>%
    select(id = id_entidade,
           nome_eleitoral = nome)
  
  votos_alt <-
    perfilparlamentar::mapeia_nome_eleitoral_to_id_senado(parlamentares, votos) %>%
    select(id_votacao, id_parlamentar = id, voto) %>% 
    distinct()
  
  return(votos_alt)
}