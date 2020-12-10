#' @title Baixa os dados de cargos na mesa para ambas as casas
#' @description Baixa todos os cargos nas Mesas Diretoras da Câmara e do Senado.
#' @return Dataframe com os dados de cargos na mesa para as duas casas.
fetch_cargos_mesa <- function() {
  source(here::here("code/utils/check_packages.R"))
  .check_and_load_perfilparlamentar_package()
  return(perfilparlamentar::processa_cargos_mesa())
}