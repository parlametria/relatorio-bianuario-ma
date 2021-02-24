library(tidyverse)
library(here)
library(futile.logger)

transform_input_votos <-
  function(votos_input_file = "data/inputs/2-fetch-input/votacoes/votacoes_preenchidas.csv",
           votos_raw_file = "data/raw/votos/votos-referencia.csv") {
    votos = read_csv(
      here::here(votos_input_file),
      col_types = cols(.default = col_character(),
                       data = col_datetime(format = ""))
    )
    
    votos %>% 
      write_csv(votos_raw_file)
    
    flog.info(str_glue("Votos referência (orientação MA) de input para raw em {votos_raw_file}"))
  }