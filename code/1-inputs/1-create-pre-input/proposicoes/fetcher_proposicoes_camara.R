library(tidyverse)
source(here::here("code/utils/check_packages.R"))
.check_and_load_perfilparlamentar_package()

#' @title Retorna autores de uma proposição
#' @description Recupera autores de uma proposição
#' @return Dataframe com informações dos autores das proposições
get_autores <- function(id_prop, casa = "camara") {
  print(id_prop)
  return(agoradigital::fetch_autores_documento(id_prop, casa))
}

#' @title Retorna dados de proposições apresentadas na Câmara em 2019 e 2020
#' @description Processa dados de proposições do tema de Meio Ambiente e Agricultura que foram
#' apresentadas em 20149 e 2020
#' @return Dataframe com informações das proposições
fetch_proposicoes_apresentadas_ma_camara <- function() {
  proposicoes_apresentadas_meio_ambiente <-
    fetch_proposicoes_apresentadas_camara(data_inicio = "2019-02-01",
                                          data_final = "2020-12-31",
                                          tema = 48) %>% 
    mutate(tema = 'Meio Ambiente e Desenvolvimento Sustentável')
  
  proposicoes_apresentadas_agricultura <-
    fetch_proposicoes_apresentadas_camara(data_inicio = "2019-01-01",
                                          data_final = "2020-12-31",
                                          tema = 64) %>% 
    mutate(tema = 'Agricultura, Pecuária, Pesca e Extrativismo')
  
  proposicoes_apresentadas_estrutura_fundiaria <-
    fetch_proposicoes_apresentadas_camara(data_inicio = "2019-01-01",
                                          data_final = "2020-12-31",
                                          tema = 51) %>% 
    mutate(tema = 'Estrutura Fundiária')
  
  proposicoes_ma_agric <- proposicoes_apresentadas_meio_ambiente %>% 
    rbind(proposicoes_apresentadas_agricultura) %>% 
    rbind(proposicoes_apresentadas_estrutura_fundiaria) %>% 
    group_by(id) %>% 
    summarise(tema = paste0(tema, collapse = ";"),
              sigla_tipo = first(siglaTipo),
              cod_tipo = first(codTipo),
              numero = first(numero),
              ano = first(ano),
              ementa = first(ementa)) %>% 
    ungroup() %>% 
    mutate(id = as.character(id))
  
  return(proposicoes_ma_agric)
}

#' @title Retorna dados de proposições votadas em plenário na Câmara em 2019 e 2020
#' @description Processa dados de proposições que foram
#' votadas em plenário em 20149 e 2020
#' @return Dataframe com informações das proposições votadas
fetch_proposicoes_votadas_plenario_camara <- function() {
  proposicoes_votadas_2019 <-
    fetch_proposicoes_votadas_camara(ano = 2019)
  
  proposicoes_votadas_2020 <-
    fetch_proposicoes_votadas_camara(ano = 2020)
  
  proposicoes_votadas <- proposicoes_votadas_2019 %>%
    rbind(proposicoes_votadas_2020) %>%
    mutate(votada_plenario = 1) %>%
    distinct(id, votada_plenario)
  
  return(proposicoes_votadas)
}

#' @title Retorna o ano de apresentação de uma proposição feita no Senado
#' @description Recebe um id e "raspa" no site da câmara de uma proposição se ela tem
#' uma proposição de origem. Se sim, retorna o ano de apresentação.
#' @param id_proposicao ID da proposição na Câmara
#' @return Ano de apresentação da proposição de origem
.crawler_ano_apresentacao_origem <- function(id_proposicao) {
  library(rvest)
  ano <- tryCatch({
    print(str_glue("Recuperando ano de origem da proposição {id_proposicao}..."))
    
    url <- 
      str_glue("https://www.camara.leg.br/proposicoesWeb/fichadetramitacao?idProposicao={id_proposicao}")
    
    html_raw <- read_html(url) %>% 
      html_nodes("#subSecaoSituacaoOrigemAcessoria") %>% 
      html_nodes("p") %>%
      html_text() %>% 
      str_extract("Origem.*")
    
    origem_proposicao <- html_raw[!is.na(html_raw)]
    
    if (!all(is.na(origem_proposicao))) {
      ano_apresentacao_origem <- str_extract(origem_proposicao, "\\d{4}")
      return(ano_apresentacao_origem)
    }
    
    return(NA)
  }, error = function(e) {
    print(e)
    return(NA)
  })
  
  return(ano)
}
