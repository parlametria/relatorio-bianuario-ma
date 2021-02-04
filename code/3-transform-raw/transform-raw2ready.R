library(tidyverse)
library(futile.logger)
source(here::here("code/3-transform-raw/transform-raw2ready-lib.R"))
flog.threshold(TRACE, "transform-raw")

#' Main do script para uso em CLI.
#' Para uso interativo, chame main() no console
#'
main <- function(argv = NULL) {
  out_props = "data/ready/proposicoes.csv"
  out_autorias_detalhes = "data/ready/autorias-detalhes.csv"
  out_autorias_resumo = "data/ready/autorias-resumo.csv"
  out_relatorias = "data/ready/relatorias.csv"
  out_atuacao = "data/ready/atuacao.csv"
  
  # PROPOSIÇÕES
  props = transform_proposicoes(
    "data/raw/leggo_data/proposicoes.csv",
    "data/inputs/3-transform-input/proposicoes/proposicoes_input.csv"
  )
  
  props %>% # temos uma linha por casa da proposição
    write_csv(here::here(out_props))
  flog.info(str_glue("Dados prontos das proposições em {out_props}"))
  
  # PARLAMENTARES
  parlamentares = parlamentares_data(
    parlamentares_file = "data/raw/leggo_data/entidades.csv",
    "data/externo/governismo/governismo-ideal-deputados.csv",
    "data/externo/governismo/governismo-ideal-senadores.csv",
    "data/raw/peso_politico/peso_politico.csv"
  )
  
  # AUTORIAS
  autorias = transform_autorias_detalhes(props,
                                         "data/raw/leggo_data/autores_leggo.csv",
                                         parlamentares)
  autorias %>%
    write_csv(here::here(out_autorias_detalhes))
  flog.info(str_glue("Detalhes de autorias em {out_autorias_detalhes}"))
  
  autorias_resumo = transform_autorias_resumo(props,
                                              "data/raw/leggo_data/autores_leggo.csv",
                                              parlamentares)
  autorias_resumo %>%
    write_csv(here::here(out_autorias_resumo))
  flog.info(str_glue("Resumo de autorias em {out_autorias_resumo}"))
  
  # RELATORIAS
  relatorias = transform_relatorias(props,
                                    "data/raw/leggo_data/relatores_leggo.csv",
                                    parlamentares)
  relatorias %>%
    write_csv(here::here(out_relatorias))
  flog.info(str_glue("Relatorias salvas em {out_relatorias}"))
  
  # ATUAÇÃO
  atuacao = transform_atuacao("data/raw/leggo_data/atuacao.csv",
                              parlamentares)
  atuacao %>%
    write_csv(here::here(out_atuacao))
  flog.info(str_glue("Atuação salva em {out_atuacao}"))
}

if (!interactive()) {
  argv <- commandArgs(TRUE)
  main(argv)
}
