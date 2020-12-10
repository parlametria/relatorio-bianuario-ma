#' @title Baixa os dados das lideranças partidárias para ambas as casas.
#' @description Baixa todos os cargos de lideranças dos partidos na Câmara e no
#' Senado.
#' @return Dataframes com as informações de lideranças partidárias.
fetch_liderancas <- function() {
  source(here::here("code/utils/check_packages.R"))
  .check_and_load_perfilparlamentar_package()
  return(perfilparlamentar::processa_liderancas())
}