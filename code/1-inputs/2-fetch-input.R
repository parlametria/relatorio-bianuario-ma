library(tidyverse)
library(googlesheets4)
sheets_deauth()
library(here)
source(here("code/1-inputs/constants.R"))

#' @title Recupera as planilhas rotuladas de proposições usadas como entrada
#' @description Recupera as planilhas rotuladas de proposições e salva como csv no repositório
#' @param planilha_camara_url Caminho para a planilha de entrada da câmara
#' @param planilha_senado_url Caminho para a planilha de entrada da câmara
#' @return Dataframe com proposições rotuladas
#' @examples
#' fetch_input_proposicoes(planilha_camara_url, planilha_senado_url)
fetch_input_proposicoes <- function(planilha_camara_url = NULL, planilha_senado_url = NULL) {
  if (is.null(planilha_camara_url)) {
    planilha_camara_url <- .URL_PLANILHA_PROPOSICAO_CAMARA
  }
  
  if (is.null(planilha_senado_url)) {
    planilha_senado_url <- .URL_PLANILHA_PROPOSICAO_SENADO
  }
  
  proposicoes_camara <- read_sheet(.URL_PLANILHA_PROPOSICAO_CAMARA, sheet = "Proposições")
  
  proposicoes_senado <- read_sheet(.URL_PLANILHA_PROPOSICAO_SENADO, sheet = "Proposições")
  
  proposicoes <- proposicoes_camara %>% 
    mutate(proposicao = paste0(sigla_tipo, " ", numero, "/", ano)) %>% 
    bind_rows(proposicoes_senado) %>% 
    select(relacionada_ma = `Relacionada com Meio Ambiente ou Clima?`, 
           classificacao_ambientalismo = `Pos/Neg/Neutro para o ambientalismo`, 
           id, casa, proposicao, sigla_tipo, numero, ano, ementa, tema, autor, link)
  
  # Salvar
  out_proposicoes = "data/inputs/2-fetch-input/proposicoes/proposicoes_preenchidas.csv"
  
  proposicoes %>%
    write_csv(here::here(out_proposicoes))
  message("Proposições processadas em ", out_proposicoes)
  
  return(proposicoes)
  
}

fetch_input_proposicoes()
