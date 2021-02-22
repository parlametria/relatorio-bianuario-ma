library(tidyverse)
library(futile.logger)
library(pscl)
source(here::here("code/utils/check_packages.R"))
.check_and_load_perfilparlamentar_package()

#' @title Recupera o governismo a partir das votações de Meio Ambiente e salva em CSV
#' @description Recupera o governismo usando o pacote do perfilparlamentar e salva o resultado em CSV
#' @param arquivo_votos_camara Caminho para o csv de votos da Câmara
#' @param arquivo_votos_senado Caminho para o csv de votos do Senado
#' @return Dataframe de Governismo em MA (valor entre -10 e 10)
#' @examples
#' fetch_governismo_ma(arquivo_votos_camara, arquivo_votos_senado)
fetch_governismo_ma <- function(arquivo_votos_camara = "data/raw/votos/votos_camara.csv",
                                arquivo_votos_senado = "data/raw/votos/votos_senado.csv") {
  
  votos_camara <- read_votos_raw(arquivo_votos_camara) %>% 
    perfilparlamentar::enumera_voto()
  votos_senado <- read_votos_raw(arquivo_votos_senado) %>% 
    perfilparlamentar::enumera_voto()
  
  flog.info("Processando Governismo")
  governismo_camara_ma <- perfilparlamentar::processa_governismo(votos_camara) %>% 
    mutate(casa = "camara")
  governismo_senado_ma <- perfilparlamentar::processa_governismo(votos_senado) %>% 
    mutate(casa = "senado")
  
  governismo <- governismo_camara_ma %>% 
    bind_rows(governismo_senado_ma)
  
  # Salvar
  out_governismo = "data/raw/governismo/governismo_ma.csv"
  
  governismo %>%
    write_csv(here::here(out_governismo))
  flog.info(str_glue("Governismo processado em ", out_governismo))
  
  return(governismo)
}
