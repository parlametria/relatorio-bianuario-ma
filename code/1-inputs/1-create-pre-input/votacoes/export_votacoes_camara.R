library(tidyverse)
library(here)
library(stringr)
source(here::here("code/1-inputs/1-create-pre-input/votacoes/analyzer_votacoes.R"))

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
    default = here::here("data/inputs/1-create-pre-input/votacoes/votacoes_camara.csv"),
    help = "nome do arquivo de saída [default= %default]",
    metavar = "character"
  )
)

opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

save_votacoes <- function(saida) {
  message("Baixando as votações em Plenário das proposições de Meio Ambiente")
  votacoes <- processa_votacoes_camara()
  
  message(paste0("Salvando o resultado em ", saida))
  write_csv(votacoes, saida)
  
  message("Concluído")
}

saida <- opt$out

if (!interactive()) {
  save_votacoes(saida)
}