library(tidyverse)
library(here)

source(here::here("code/cargos/cargos_mesa/fetcher_cargos_mesa.R"))

if(!require(optparse)){
  install.packages("optparse")
  suppressWarnings(suppressMessages(library(optparse)))
}

args = commandArgs(trailingOnly=TRUE)

message("LEIA O README deste diretório")
message("Use --help para mais informações\n")

option_list = list(
  make_option(c("-o", "--out"), type="character", default=here::here("data/cargos_mesa/cargos_mesa.csv"), 
              help="nome do arquivo de saída [default= %default]", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

saida <- opt$out

message("Baixando dados de cargos nas mesas diretoras...")
cargos_mesa <- fetch_cargos_mesa()

message(paste0("Salvando o resultado em ", saida))
write_csv(cargos_mesa, saida)

message("Concluído!")
