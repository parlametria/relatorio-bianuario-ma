#' @title Checa se o pacote perfilparlamentar está instalado e carrega-o.
#' @description Checa se o perfilparlamentar foi instalado. Caso contrário,
#' realiza a instalação e o carrega
.check_and_load_perfilparlamentar_package <- function() {
  if(!require(perfilparlamentar)){
    devtools::install_github("parlametria/perfil-parlamentarR@main")
  }
  suppressWarnings(suppressMessages(library(perfilparlamentar)))
}