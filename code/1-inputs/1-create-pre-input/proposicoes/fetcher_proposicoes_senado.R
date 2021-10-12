library(tidyverse)
source(here::here("code/utils/check_packages.R"))
.check_and_load_perfilparlamentar_package()

#' @title Retorna dados de proposições apresentadas no Senado em 2019 e 2020
#' @description Processa dados de proposições do tema de Meio Ambiente e Agricultura que foram
#' apresentadas em 20149 e 2020
#' @return Dataframe com informações das proposições
fetch_proposicoes_apresentadas_ma_senado <- function() {
  datas_inicio <-
    seq(as.Date('2019-02-01'),
        length.out = 6,
        by = '6 month')
  
  datas_fim <-
    seq(as.Date('2019-08-01'),
        length.out = 6,
        by = '6 month')
  
  datas <-
    data.frame(inicio = datas_inicio,
               fim = datas_fim,
               stringsAsFactors = F)
  
  proposicoes <- purrr::pmap_df(
    list(datas$inicio,
         datas$fim),
    function(x, y) {
      print(str_glue("Baixando proposições entre {x} e {y}..."))
      df <- fetcher_proposicoes_em_intervalo_senado(x, y)
      return(df)
    }
  )
  
  return(proposicoes)
}

#' @title Retorna o ano de apresentação de uma proposição feita na Câmara
#' @description Recebe um id e "raspa" no site do senado de uma proposição se ela tem
#' uma proposição de origem. Se sim, retorna o ano de apresentação.
#' @param id_proposicao ID da proposição no Senado
#' @return Ano de apresentação da proposição de origem
.crawler_ano_apresentacao_origem_senado <- function(id_proposicao) {
  library(rvest)
  ano <- tryCatch({
    print(str_glue("Recuperando ano de origem da proposição {id_proposicao}..."))
    
    url <- 
      str_glue("https://www25.senado.leg.br/web/atividade/materias/-/materia/{id_proposicao}")
    
    html_raw <- read_html(url) %>% 
      html_nodes(".span8") %>% 
      html_nodes("p") %>%
      html_text() %>% 
      str_remove_all("\n|\t") %>% 
      str_extract("Nº na Câmara dos Deputados.*") %>% 
      str_remove("Norma Gerada:.*") %>% 
      str_remove("[:space:]*$")
    
    origem_proposicao <- html_raw[!is.na(html_raw)]
    
    if (!all(is.na(origem_proposicao))) {
      ano_apresentacao_origem <- str_extract(origem_proposicao, "\\d{4}$")
      return(ano_apresentacao_origem)
    }
    
    return(NA)
  }, error = function(e) {
    print(e)
    return(NA)
  })
  
  return(ano)
}
