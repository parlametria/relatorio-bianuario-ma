library(tidyverse)
library(here)

source(here::here("code/2-fetch-raw/votos/fetcher_votos_camara.R"))

if(!require(optparse)){
  install.packages("optparse")
  suppressWarnings(suppressMessages(library(optparse)))
}

args = commandArgs(trailingOnly=TRUE)

message("LEIA O README deste diretório")
message("Use --help para mais informações\n")

option_list = list(
  make_option(c("-o", "--out"), type="character", default=here::here("data/raw/votos/votos_camara.csv"), 
              help="nome do arquivo de saída [default= %default]", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

saida <- opt$out

message("Baixando dados de votos na Câmara...")
votos <- fetch_votos_camara()

message(paste0("Salvando o resultado em ", saida))

write_csv(votos, saida)

message("Concluído!")
