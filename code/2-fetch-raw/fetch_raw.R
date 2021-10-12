library(futile.logger)

#'@title Salva todos os dados referentes a cargos
#'@description Baixa e salva todos os dados de cargos de mesa, comissões e
#'lideranças para Câmara e Senado
fetch_cargos <- function() {
  flog.info("Baixando todos os dados de cargos...")
  
  source(here::here("code/2-fetch-raw/cargos/cargos_mesa/export_cargos_mesa.R"))
  source(here::here("code/2-fetch-raw/cargos/liderancas/export_liderancas.R"))
  source(here::here("code/2-fetch-raw/cargos/comissoes/export_comissoes.R"))
  
  flog.info("Concluído!")
}

#'@title Salva todos os dados referentes aos votos nominais
#'@description Baixa e salva todos os dados de votos das votações
#'nominais para Câmara e o Senado
fetch_votos <- function() {
  flog.info("Baixando dados de votos...")
  source(here::here("code/2-fetch-raw/votos/export_votos_camara.R"))
  source(here::here("code/2-fetch-raw/votos/export_votos_senado.R"))
}

#'@title Salva o governismo dos parlamentares
#'@description Processa e salva os dados de governismo para os
#'parlamentares da câmara e do senado
fetch_governismo <- function() {
  flog.info("Processando governismo...")
  source(here::here("code/2-fetch-raw/governismo/fetch_governismo.R"))
  fetch_governismo_ma()
}

fetch_parlamentares <- function() {
  flog.info("Processando dados de parlamentares...")
  source(here::here("code/2-fetch-raw/parlamentares/export_parlamentares.R"))
}

fetch_peso_politico <- function() {
  flog.info("Processando dados de peso político...")
  source(here::here("code/2-fetch-raw/peso-politico/export_peso_politico.R"))
}

fetch_parlamentares()
fetch_votos()
fetch_cargos()
fetch_governismo()
fetch_peso_politico()
