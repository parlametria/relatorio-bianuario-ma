library(tidyverse)
library(here)
library(stringr)
source(here::here("code/1-inputs/1-create-pre-input/proposicoes/analyzer_proposicoes.R"))

if (!require(optparse)) {
  install.packages("optparse")
  suppressWarnings(suppressMessages(library(optparse)))
}

args = commandArgs(trailingOnly = TRUE)

message("LEIA O README deste diretório")
message("Use --help para mais informações\n")

option_list = list(
  make_option(
    c("-o", "--out"),
    type = "character",
    default = here::here("data/inputs/1-create-pre-input/proposicoes/proposicoes_camara.csv"),
    help = "nome do arquivo de saída [default= %default]",
    metavar = "character"
  )
)

opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

save_proposicoes <- function(saida) {
  message("Baixando as proposições de Meio Ambiente para a Câmara")
  props <- processa_proposicoes_camara()
  
  message(paste0("Salvando o resultado em ", saida))
  write_csv(props, saida)
  
  message("Concluído")
}

saida <- opt$out

if (!interactive()) {
  save_proposicoes(saida)
}