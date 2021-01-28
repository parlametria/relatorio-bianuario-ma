library(tidyverse)
library(here)

source(here::here("code/cargos/comissoes/fetcher_comissoes.R"))

if(!require(optparse)){
  install.packages("optparse")
  suppressWarnings(suppressMessages(library(optparse)))
}

args = commandArgs(trailingOnly=TRUE)

message("LEIA O README deste diretório")
message("Use --help para mais informações\n")

option_list = list(
  make_option(c("-o", "--out"), type="character", default=here::here("data/raw/comissoes/"), 
              help="nome do arquivo de saída [default= %default]", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

saida <- opt$out

message("Baixando dados de comissões e seus membros...")
comissoes_membros <- fetch_comissoes_e_membros()

comissoes_info <- comissoes_membros[[1]]

comissoes_membros <- comissoes_membros[[2]]

message(paste0("Salvando o resultado no diretório ", saida))

write_csv(comissoes_info, paste0(saida, "comissoes.csv"))
write_csv(comissoes_membros, paste0(saida, "composicao_comissoes.csv"))

message("Concluído!")
