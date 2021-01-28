library(tidyverse)

#' @title Baixa os pesos políticos dos parlamentares em exercício
#' @description Baixa os dados dos parlamentares na Câmara e no Senado que estão
#' em exercício.
#' @return Dataframe contendo id do parlamentar no Parlametria e valor do peso_político.
fetch_peso_politico <- function() {
  .URL_PESO_POLITICO = "https://perfil.parlametria.org/api/perfil/lista"
  
  pesos_politicos <- RCurl::getURL(.URL_PESO_POLITICO) %>%
    jsonlite::fromJSON() %>%
    rename(id_parlamentar_parlametria = idParlamentarVoz,
           peso_politico = pesoPolitico)
  
  return(pesos_politicos)
}
