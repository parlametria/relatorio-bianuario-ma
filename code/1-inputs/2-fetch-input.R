library(tidyverse)
library(googlesheets4)
sheets_deauth()
library(here)
source(here("code/1-inputs/constants.R"))

fetch_aba <- function(url_recebida, url_default, aba) {
  if (is.null(url_recebida)) {
    planilha <- url_default
  }
  
  read_sheet(planilha, sheet = aba)
}


#' @title Recupera as planilhas rotuladas de proposições usadas como entrada
#' @description Recupera as planilhas rotuladas de proposições e salva como csv no repositório
#' @param planilha_camara_url Caminho para a planilha de entrada da câmara
#' @param planilha_senado_url Caminho para a planilha de entrada da câmara
#' @return Dataframe com proposições rotuladas
#' @examples
#' fetch_input_proposicoes(planilha_camara_url, planilha_senado_url)
fetch_input_proposicoes <-
  function(planilha_camara_url = NULL,
           planilha_senado_url = NULL) {
    proposicoes_camara <- fetch_aba(planilha_camara_url,
                                    .URL_PLANILHA_PROPOSICAO_CAMARA,
                                    aba = "Proposições")
    
    proposicoes_senado <- fetch_aba(planilha_senado_url,
                                    .URL_PLANILHA_PROPOSICAO_SENADO,
                                    aba = "Proposições")
    
    proposicoes <- proposicoes_camara %>%
      mutate(proposicao = paste0(sigla_tipo, " ", numero, "/", ano)) %>%
      bind_rows(proposicoes_senado) %>%
      select(
        relacionada_ma = `Relacionada com Meio Ambiente ou Clima?`,
        classificacao_ambientalismo = `Pos/Neg/Neutro para o ambientalismo`,
        id,
        casa,
        proposicao,
        sigla_tipo,
        numero,
        ano,
        ementa,
        tema,
        autor,
        link
      )
    
    # Salvar
    out_proposicoes = "data/inputs/2-fetch-input/proposicoes/proposicoes_preenchidas.csv"
    
    proposicoes %>%
      write_csv(here::here(out_proposicoes))
    message("Proposições anotadas em ", out_proposicoes)
    
    return(proposicoes)
    
  }

fetch_input_votacoes <-
  function(planilha_camara_url = NULL,
           planilha_senado_url = NULL) {
    votos_camara <- fetch_aba(planilha_camara_url,
                              .URL_PLANILHA_PROPOSICAO_CAMARA,
                              aba = "Votações")
    
    votos_senado <- fetch_aba(planilha_senado_url,
                              .URL_PLANILHA_PROPOSICAO_SENADO,
                              aba = "Votações")
    
    senado_mdc = votos_senado %>%
      select(-votacao_secreta,-cod_sessao,-hora, -data_apresentacao_proposicao) %>% 
      mutate(id_votacao = as.character(id_votacao))
    
    camara_mdc = votos_camara %>%
      select(-indexacao_proposicao,-descricao_efeitos, -data_apresentacao_proposicao) %>%
      mutate(data = lubridate::dmy_hms(data))
    
    votos <- camara_mdc %>% 
      bind_rows(senado_mdc) %>%
      rename(orientacao_ma = `Ambientalismo orienta SIM/NÃO/LIBERADO`)
    
    # Salvar
    out_votos = "data/inputs/2-fetch-input/votacoes/votacoes_preenchidas.csv"
    
    votos %>%
      write_csv(here::here(out_votos))
    message("Votações anotadas em ", out_votos)
    
    return(votos)
  }

fetch_input_proposicoes()
fetch_input_votacoes()