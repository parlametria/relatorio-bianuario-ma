#' @title Processa a planilha de entrada
#' @description Processa a planilha de entrada da lista de proposições para o formato de entrada do parlametria
#' @param planilha_path Caminho para a planilha de entrada
#' @return Dataframe com o formato do csv de entrada para o processamento do Parlametria
#' @examples
#' transform_input_proposicoes()
transform_input_proposicoes <-
  function(planilha_path = here::here("data/inputs/2-fetch-input/proposicoes/proposicoes_preenchidas.csv")) {
    
    proposicoes_selecao <- read_csv(planilha_path, 
                                    col_types = cols(.default = col_character()))
    
    proposicoes_camara <- proposicoes_selecao %>%
      filter(casa == "camara") %>%
      mutate(explicacao_ambientalismo = classificacao_ambientalismo) %>% 
      select(proposicao, sigla_tipo, numero, ano, tema, id_camara = id, casa, 
             classificacao_ambientalismo, explicacao_ambientalismo)
    
    proposicoes_senado <- proposicoes_selecao %>%
      filter(casa == "senado",
             relacionada_ma == "Sim") %>%
      mutate(explicacao_ambientalismo = classificacao_ambientalismo) %>% 
      select(proposicao, sigla_tipo, numero, ano, tema, id_senado = id, casa, 
             classificacao_ambientalismo, explicacao_ambientalismo)
    
    proposicoes_merge <- proposicoes_camara %>%
      bind_rows(proposicoes_senado) %>% 
      filter(classificacao_ambientalismo != "Fora do tema") %>% 
      mutate(classificacao_ambientalismo = 
               case_when(
                 str_detect(tolower(classificacao_ambientalismo), "negativo") ~ "Negativo",
                 str_detect(tolower(classificacao_ambientalismo), "positiv.") ~ "Positivo",
                 str_detect(tolower(classificacao_ambientalismo), "neutr.") ~ "Neutro",
                 TRUE ~ NA_character_
               )
      ) %>% 
      group_by(proposicao) %>% 
      fill(id_camara, .direction = c("downup")) %>% 
      fill(id_senado, .direction = c("downup")) %>% 
      ungroup() %>% 
      distinct(id_camara, id_senado, .keep_all = TRUE) %>% 
      rowwise()
    
    flog.info("Buscando ids das proposições na câmara e senado")
    
    proposicoes_id <- purrr::pmap_dfr(
      list(
        proposicoes_merge$sigla_tipo,
        proposicoes_merge$numero,
        proposicoes_merge$ano,
        proposicoes_merge$id_camara,
        proposicoes_merge$id_senado
      ),
      ~ process_id_proposicao(..1, ..2, ..3, ..4, ..5)
    )
    
    proposicoes_input <- proposicoes_merge %>% 
      left_join(proposicoes_id %>% 
                  mutate(id_camara = as.character(id_camara),
                         id_senado = as.character(id_senado)),
                by = c("sigla_tipo"="tipo",
                       "numero", 
                       "ano")) %>% 
      mutate(id_camara = if_else(is.na(id_camara.x), id_camara.y, id_camara.x),
             id_senado = if_else(is.na(id_senado.x), id_senado.y, id_senado.x)) %>% 
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
        classificacao_ambientalismo,
        explicacao_ambientalismo
      )
    
    # Salvar
    out_proposicoes = "data/inputs/3-transform-input/proposicoes/proposicoes_input.csv"
    
    proposicoes_input %>%
      write_csv(here::here(out_proposicoes))
    flog.info(paste0("Proposições processadas em ", out_proposicoes))
    
    return(proposicoes_input)
  }

#' @title Recupera id na câmara ou no senado a partir do nome formal da proposição
#' @description Recupera id na câmara ou no senado a partir do nome formal da proposição usando funções do rcongresso
#' @param tipo Tipo da proposição
#' @param numero Número da proposição
#' @param ano Ano da proposição
#' @param casa Casa da proposição
#' @param id Id da proposição
#' @return id da proposição
#' @examples
#' process_id_proposicao(tipo = "PEC", numero = "6", ano = "2019", id_camara = NA, id_senado = NA)
process_id_proposicao <- function(tipo = "PEC", numero = "6", ano = "2019", id_camara = NA, id_senado = NA) {
  flog.trace(str_glue("Buscando ids para {tipo} {numero} / {ano}. Temos: câmara {id_camara}, senado {id_senado}"))
  
  if (is.na(id_camara)) {
    id_camara <- rcongresso::fetch_id_proposicao_camara(tipo, numero, ano)
  }
  
  if (is.na(id_senado)) {
    data_senado <- rcongresso::fetch_proposicao_senado_sigla(tipo, numero, ano) 
    
    if (nrow(data_senado) != 0) {
      id_senado <- data_senado %>% pull(codigo_materia)
    }
  }
  
  flog.info(str_glue("Resultado dos ids  {tipo} {numero} / {ano}: câmara {id_camara}, senado {id_senado}"))
  
  data <- tibble(
    tipo = tipo,
    numero = numero,
    ano = ano,
    id_camara = as.numeric(id_camara),
    id_senado = as.numeric(id_senado)
  )
  
  return(data)
}
