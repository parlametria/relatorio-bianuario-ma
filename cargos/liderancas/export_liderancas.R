library(tidyverse)
library(here)

source(here::here("code/cargos/liderancas/fetcher_liderancas.R"))

if(!require(optparse)){
  install.packages("optparse")
  suppressWarnings(suppressMessages(library(optparse)))
}

args = commandArgs(trailingOnly=TRUE)

message("LEIA O README deste diretório")
message("Use --help para mais informações\n")

option_list = list(
  make_option(c("-o", "--out"), type="character", default=here::here("data/liderancas/liderancas.csv"), 
              help="nome do arquivo de saída [default= %default]", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

saida <- opt$out

message("Baixando dados de lideranças partidárias...")
liderancas <- fetch_liderancas()

message(paste0("Salvando o resultado em ", saida))

write_csv(liderancas, saida)

message("Concluído!")
