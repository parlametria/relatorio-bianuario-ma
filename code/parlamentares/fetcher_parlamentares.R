#' @title Baixa os dados dos parlamentares para um conjunto de legislaturas
#' @description Baixa os dados dos parlamentares na Câmara e no Senado para
#' um conjunto de legislaturas.
#' @param legislaturas Lista com os IDs das legislaturas. Default: c(56)
#' @return Dataframe com os dados de parlamentares.
fetch_parlamentares <- function(legislaturas = c(56)) {
  source(here::here("code/utils/check_packages.R"))
  .check_and_load_perfilparlamentar_package()
  return(
    perfilparlamentar::processa_dados_parlamentares(legislaturas_list = legislaturas)
  )
}