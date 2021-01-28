#' @title Baixa os dados de comissões para ambas as casas
#' @description Baixa todas as comissões das duas casas com os membros.
#' @return Lista de dataframes com as informações das comissões e suas composições.
fetch_comissoes_e_membros <- function() {
  source(here::here("code/utils/check_packages.R"))
  .check_and_load_perfilparlamentar_package()
  return(perfilparlamentar::processa_comissoes_e_composicoes())
}