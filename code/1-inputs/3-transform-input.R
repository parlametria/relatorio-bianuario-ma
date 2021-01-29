library(tidyverse)
library(readxl)
library(urltools)
library(here)

#' @title Processa a planilha de entrada
#' @description Processa a planilha de entrada da lista de proposições para o formato de entrada do parlametria
#' @param planilha_path Caminho para a planilha de entrada
#' @return Dataframe com o formato do csv de entrada para o processamento do Parlametria
#' @examples
#' transform_input_proposicoes()
transform_input_proposicoes <-
  function(planilha_path = here::here("data/input/PDLs Ambientais 2019 e 2020 - Filtro Bruno Carazza.xlsx")) {
    proposicoes_selecao <- read_excel(planilha_path, 
                                      sheet = "Revisão Carol")
    urls <- proposicoes_selecao %>%
      filter(str_detect(`Dentro do escopo do relatório?`, "Sim")) %>%
      select(
        proposicao = `Proposições`,
        url = Link,
        tema = `Classificação Bruno`,
        situacao = `Situação`,
        norma_atacada = `Norma Atacada`
      ) %>%
      mutate(dominio = domain(url)) %>%
      mutate(
        origem = case_when(
          (
            dominio == "www.camara.leg.br" |
              dominio == "www.camara.gov.br"
          ) ~ "camara",
          dominio == "www.congressonacional.leg.br" ~ "congresso",
          dominio ==  "www25.senado.leg.br" ~ "senado",
          TRUE ~ NA_character_
        )
      ) %>%
      rowwise() %>%
      mutate(id_prop = .extract_id(url, origem)) %>%
      mutate(casa = if_else(origem == 'congresso', 'senado', origem))
    
    proposicoes_camara <- urls %>%
      filter(casa == "camara") %>%
      select(proposicao, tema, url, situacao, norma_atacada, id_camara = id_prop)
    
    proposicoes_senado <- urls %>%
      filter(casa == "senado") %>%
      select(proposicao, tema, url, situacao, norma_atacada, id_senado = id_prop)
    
    proposicoes <- proposicoes_camara %>%
      bind_rows(proposicoes_senado) %>%
      mutate(
        apelido = "",
        prioridade = "",
        advocacy_link = "",
        keywords = "",
        tipo_agenda = "",
        explicacao_projeto = ""
      ) %>%
      mutate_at(.funs = list(~ replace_na(., "")),
                .vars = vars(id_camara, id_senado)) %>%
      select(
        proposicao,
        id_camara,
        id_senado,
        apelido,
        tema,
        prioridade,
        advocacy_link,
        keywords,
        tipo_agenda,
        explicacao_projeto,
        situacao,
        norma_atacada
      )
    
    # Salvar
    out_proposicoes = "data/raw/proposicoes_input.csv"
    
    proposicoes %>%
      write_csv(here::here(out_proposicoes))
    message("Proposições processadas em ", out_proposicoes)
    
    return(proposicoes)
  }

#' @title Extrai ids das proposições através da URL
#' @description Extrai ids das proposições através da URL para a página da proposição na respectiva casa
#' @param url URL para a página da proposição
#' @param casa Casa de origem da proposição
#' @return Id da proposição
#' @examples
#' .extract_id("http://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao=2194820", "camara")
.extract_id <- function(url, casa) {
  id <- case_when(
    is.na(casa) ~ NA_character_,
    casa == "camara" ~ .extract_number_from_regex(url, "idProposicao=[0-9]+"),
    casa == "senado" ~ .extract_number_from_regex(url, "materia/[0-9]+"),
    casa == "congresso" ~ .extract_number_from_regex(url, "mpv/[0-9]+"),
    TRUE ~ NA_character_
  )
  return(id)
}

#' @title Extrai somente os números de um texto extraído de regex
#' @description A partir de uma expressão regular, extrai o texto desse regex e depois retorna apenas os números existentes
#' @param text Texto onde o regex será aplicado
#' @param text_regex Expressão regular onde o texto será extraído para depois serem retornados apenas os números
#' @return Números existentes em um texto extraído a partir de uma expressão regular
#' @examples
#' .extract_number_from_regex("http://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao=2194820", "idProposicao=[0-9]+")
.extract_number_from_regex <- function(text, text_regex) {
  return(stringr::str_extract(text, text_regex) %>%
           stringr::str_extract("[0-9]+"))
}


transform_input_proposicoes()
