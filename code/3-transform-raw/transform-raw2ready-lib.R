library(tidyverse)
library(futile.logger)
source(here::here("code/read_raw.R"))

#' Transforma dados das proposições acompanhadas para ready.
#'
#' @param raw_data Caminho do dado raw
#' @return As proposições salvas como ready
#'
transform_proposicoes <-
  function(raw_data = "data/raw/leggo_data/proposicoes.csv",
           arquivo_props_input = "data/inputs/3-transform-input/proposicoes/proposicoes_input.csv",
           arquivo_destaques_tramitacao = "data/raw/leggo_data/proposicoes_destaques.csv") {
    proposicoes_leggo = read_proposicoes_raw(raw_data)
    n_leggo = proposicoes_leggo %>% pull(id_leggo) %>% n_distinct()
    flog.info(str_glue("{n_leggo} proposições nos dados do leggo"))
    
    proposicoes_input_tudo = read_proposicoes_input_raw(arquivo_props_input) 
    n_input = proposicoes_input_tudo %>% pull(proposicao) %>% n_distinct()
    flog.info(str_glue("{n_input} proposições na planilha de input"))
    proposicoes_input = proposicoes_input_tudo %>% 
      filter(classificacao_ambientalismo != "Fora do tema")
    flog.info(str_glue("{NROW(proposicoes_input)} proposições no tema segundo input"))
    
    pi_long = pivot_longer(
      proposicoes_input,
      c(id_camara, id_senado),
      names_to = "tipo_id",
      values_to = "id_ext"
    ) %>%
      filter(!is.na(id_ext)) %>%
      select(-tipo_id)
    
    proposicoes_tudo = proposicoes_leggo %>%
      inner_join(pi_long, by = c("id_ext")) %>%
      rename(nome_proposicao = proposicao) %>%
      filter(lubridate::year(data_apresentacao) >= 2019) %>%
      rename(efeito = classificacao_ambientalismo) %>% 
      mutate(status = if_else(status == "Lei", "Aprovada", status), 
             efeito = case_when(
               efeito == "Positivo" ~ "Convergente",
               efeito == "Negativo" ~ "Divergente",
               efeito == "Neutro" ~ "Neutro",
               TRUE ~ NA_character_
             )) 
    
    n_cruzado = proposicoes_tudo %>% pull(id_leggo) %>% n_distinct()
    flog.info(str_glue("{n_cruzado} proposições após cruzar input e leggo"))
    
    proposicoes_com_tram = cruza_destaques_tramitacao(proposicoes_tudo, arquivo_destaques_tramitacao)
    
    proposicoes_com_tram
  }

#' Código comum para transform_autorias_*
#'
le_cruza_autorias <- function(proposicoes,
                              autores_file,
                              parlamentares) {
  # Ler
  autores_leggo_raw = read_autorias_raw(autores_file)
  
  # Cruzar
  autores_leggo = autores_leggo_raw %>%
    inner_join(proposicoes, by = "id_leggo")
  
  autorias = autores_leggo %>%
    left_join(parlamentares,
              by = c("id_autor_parlametria" = "id_entidade_parlametria"))
  
  autorias
}

#' Transforma autorias detalhada das proposições acompanhadas para ready.
#'
#' @param proposicoes Lista das proposições consideradas
#'
transform_autorias_detalhes <- function(proposicoes,
                                        autores_file,
                                        parlamentares) {
  autorias = le_cruza_autorias(proposicoes,
                               autores_file,
                               parlamentares)
  autorias %>%
    detalha_autorias()
}

#' Transforma autorias resumida das proposições acompanhadas para ready.
#'
#' @param proposicoes Lista das proposições consideradas
#'
transform_autorias_resumo <- function(proposicoes,
                                      autores_file,
                                      parlamentares) {
  autorias = le_cruza_autorias(proposicoes,
                               autores_file,
                               parlamentares)
  
  resumo_autores = autorias %>%
    resume_autorias()
  
  resumo_todos = parlamentares %>%
    left_join(resumo_autores,
              by = c("casa", "nome", "partido", "uf", "governismo")) %>%
    mutate(across(assinadas:autorias_ponderadas, replace_na, 0))
  
  resumo_todos
}

parlamentares_data <-
  function(parlamentares_file,
           governismo_deps_file,
           governismo_sens_file,
           governismo_ma_file,
           peso_file) {
    parlamentares_raw = read_parlamentares_raw(parlamentares_file)
    governismo = read_governismo_raw(governismo_deps_file, governismo_sens_file)
    governismo_ma <- read_governismo_ma_raw(governismo_ma_file)
    peso = read_peso_raw(peso_file)
    
    parlamentares_raw %>%
      left_join(governismo,
                by = c("id_entidade" = "id_parlamentar")) %>%
      left_join(governismo_ma,
                by = c("id_entidade" = "id_parlamentar")) %>%
      left_join(peso,
                by = c("id_entidade_parlametria" = "id_parlamentar_parlametria"))
  }

detalha_autorias = function(data) {
  data %>%
    filter(!is.na(nome)) %>%
    mutate(proposicao = str_glue("{sigla_tipo} {numero}")) %>%
    group_by(id_leggo, proposicao) %>%
    mutate(autores = n()) %>%
    ungroup() %>%
    select(
      nome,
      id_entidade,
      partido,
      uf,
      casa,
      sigla_tipo,
      proposicao = nome_proposicao,
      efeito,
      autores,
      governismo,
      governismo_ma
    ) %>%
    mutate(
      assinadas = 1,
      autorias_ponderadas = 1 / autores,
      coautores = autores - 1
    )
}

resume_autorias = function(data) {
  data %>%
    detalha_autorias() %>%
    group_by(nome, partido, uf, casa, governismo) %>%
    summarise(
      assinadas = sum(assinadas),
      autorias_ponderadas = sum(autorias_ponderadas),
      positivas = sum(efeito == "Positivo", na.rm = T),
      negativas = sum(efeito == "Negativo", na.rm = T),
      neutras = sum(efeito == "Neutro", na.rm = T),
      .groups = "drop"
    )
}

transform_atuacao <-
  function(atuacao_file = "data/raw/leggo_data/autorias.csv",
           parlamentares) {
    atuacao = read_atuacao_raw(atuacao_file)
    parlamentares = parlamentares %>%
      select(id_entidade_parlametria,
             partido,
             uf,
             governismo,
             governismo_ma,
             peso_politico)
    
    atuacao %>%
      left_join(parlamentares,
                by = c("id_autor_parlametria" = "id_entidade_parlametria")) %>%
      mutate(ano_apresentacao = lubridate::year(data)) %>%
      filter(ano_apresentacao >= 2019, ano_apresentacao <= 2020) %>%
      select(
        id_leggo,
        id_principal,
        casa,
        id_documento,
        sigla,
        descricao_tipo_documento,
        data,
        id_autor_parlametria,
        partido,
        uf,
        nome_eleitoral,
        casa_autor,
        tipo_documento,
        tipo_acao,
        peso_autor_documento,
        governismo,
        governismo_ma,
        peso_politico
      )
  }

transform_relatorias <-
  function(props, relatorias_file, parlamentares) {
    relatorias = read_relatoria_raw(relatorias_file)
    
    t = relatorias %>%
      left_join(
        parlamentares,
        by = c(
          "relator_id_parlametria" = "id_entidade_parlametria",
          "casa",
          "relator_id" = "id_entidade"
        )
      )
    
    props %>%
      left_join(t, by = "id_leggo")
  }

#' Transforma critérios para destaques de tramitação. Principais colunas resultado
#' são urgencia, avanco e status. Juntos eles ajudam a dar uma visão do avanço das proposições.
#'
cruza_destaques_tramitacao <- function(props,
                                       destaques_file = "data/raw/leggo_data/proposicoes_destaques.csv") {
  destaques_raw = read_destaques_raw(destaques_file)
  criterios = destaques_raw %>%
    transmute(
      id_leggo = id_leggo,
      urgencia = case_when(
        criterio_req_urgencia_aprovado ~ "Urgência aprovada",
        criterio_req_urgencia_apresentado ~ "Urgência apresentada",
        TRUE ~ "Sem urgência"
      ),
      avanco = case_when(
        criterio_aprovada_em_uma_casa ~ "Avançou em uma casa",
        criterio_avancou_comissoes ~ "Avançou em comissões",
        TRUE ~ "Ainda no início"
      )
    )
  
  # Precisamos conferir quais já são leis
  destaques = criterios %>%
    right_join(props, by = "id_leggo")
  
  n_props = n_distinct(destaques$id_leggo)
  n_faltantes = destaques %>%
    mutate(falta = is.na(urgencia) |
             is.na(avanco)) %>%
    summarise(faltam = sum(falta)) %>%
    pull(faltam)
  
  flog.info(str_glue("{n_props} proposiçòes nos dados de destaques"))
  flog.info(str_glue("{n_faltantes} proposiçòes com destaques NA"))
  
  destaques
}

transform_votos_detalhes <-
  function(acontecidas_file = "data/raw/votos/votos_camara.csv",
           rotuladas_file = "data/raw/votos/votos-referencia.csv",
           parlamentares,
           casa_votacoes = "camara") {
    rotuladas_raw = read_csv(
      here::here(rotuladas_file),
      col_types = cols(.default = col_character(),
                       data = col_datetime(format = ""))
    )
    
    acontecidas = read_csv(here::here(acontecidas_file),
                           col_types = "ccc")
    
    rotuladas = rotuladas_raw %>%
      filter(orientacao_ma %in% c("SIM", "NÃO"))
    
    votos = rotuladas %>%
      filter(casa == casa_votacoes) %>%
      left_join(acontecidas, by = "id_votacao") %>%
      left_join(parlamentares,
                by = c("id_parlamentar" = "id_entidade", "casa" = "casa"))
    
    votos
  }

transform_votos_resumo <-
  function(acontecidas_file = "data/raw/votos/votos_camara.csv",
           rotuladas_file = "data/raw/votos/votos-referencia.csv",
           parlamentares,
           casa_votacoes = "camara") {
    detalhes = transform_votos_detalhes(acontecidas_file,
                                        rotuladas_file,
                                        parlamentares,
                                        casa_votacoes)
    detalhes %>%
      group_by(
        nome,
        id_entidade_parlametria,
        partido,
        uf,
        casa,
        governismo,
        governismo_ma,
        peso_politico
      ) %>%
      .resume_votos()
  }


#' Transforma votações rotuladas em arquivo sobre as votações (e não votos).
#'
#' @param rotuladas_file Votações rotuladas raw.
#'
#' @return Df ready com as votações
#'
transform_votacoes <-
  function(rotuladas_file = "data/raw/votos/votos-referencia.csv",
           acontecidas_camara = "data/raw/votos/votos_camara.csv",
           acontecidas_senado = "data/raw/votos/votos_senado.csv") {
    rotuladas_raw = read_csv(
      here::here(rotuladas_file),
      col_types = cols(.default = col_character(),
                       data = col_datetime(format = ""))
    )
    
    acontecidas = read_csv(here::here(acontecidas_camara),
                           col_types = "ccc") %>%
      bind_rows(read_csv(here::here(acontecidas_senado),
                         col_types = "ccc"))
    
    n_rotulos = rotuladas_raw %>%
      count(orientacao_ma) 
    
    flog.info(
      str_glue(
        "{NROW(rotuladas_raw)} votações das planilhas. Orientações:"
      )
    )
    flog.info(paste(n_rotulos$orientacao_ma, n_rotulos$n))
    
    votos = rotuladas_raw %>%
      left_join(acontecidas, by = "id_votacao")
    
    votacoes = votos %>%
      group_by(
        orientacao_ma,
        nome_proposicao,
        ementa_proposicao,
        obj_votacao,
        resumo,
        data,
        autor,
        tema,
        id_votacao,
        casa
      ) %>%
      .resume_votos() %>%
      mutate(consenso = abs(votos_sim - votos_nao) / (votos_sim_nao))
    
    votacoes
  }

.resume_votos <- function(votos) {
  votos %>%
    mutate(
      orientacao_ma = stringr::str_to_title(orientacao_ma),
      voto = stringr::str_to_title(voto),
      apoiou = if_else(
        voto %in% c("Sim", "Não"),
        if_else((orientacao_ma == voto), "apoio", "contra"),
        "indefinido"
      )
    )  %>%
    summarise(
      votos_capturados = sum(!is.na(id_parlamentar)),
      votos_favoraveis = sum(apoiou == "apoio"),
      votos_contra = sum(apoiou == "contra"),
      votos_indef = sum(apoiou == "indefinido"),
      apoio = votos_favoraveis / (votos_favoraveis + votos_contra),
      votos_sim = sum(voto == "Sim"),
      votos_nao = sum(voto == "Não"),
      votos_sim_nao = votos_sim + votos_nao,
      votos_outros = sum(!is.na(voto) &
                           voto != "Sim" & voto != "Não"),
      .groups = "drop"
    )
}

#' @title Ordena valores
#' @description Recebe dois valores e os retorna ordenados e
#' separados por um separador
#' @param x Primeiro valor a ser comparado
#' @param y Segundo valor a ser comparado
#' @param sep Character usado para ser o separador entre os valores
#' @return Valores ordenados separados por um separador.
.paste_cols <- function(x, y, sep = ":") {
  stopifnot(length(x) == length(y))
  return(lapply(1:length(x), function(i) {
    paste0(sort(c(x[i], y[i])), collapse = ":")
  }) %>%
    unlist())
}

#' @title Remove relação duplicadas entre autores
#' @description Recebe um dataframe e retira as linhas comutadas repetidas 
#' de coautorias. Ex: em uma linha x e y autorou em z;
#'  em outra linha y e x autorou em z.
#' @param df Dataframe de coautorias
#' @return Dataframe de coautorias sem relações repetidas para a mesma
#' proposição.
.remove_duplicated_edges <- function(df) {
  df %>%
    mutate(col_pairs =
             .paste_cols(partido.x,
                        partido.y,
                        sep = ":")) %>%
    group_by(col_pairs) %>%
    tidyr::separate(col = col_pairs,
                    c("partido.x",
                      "partido.y"),
                    sep = ":") %>%
    group_by(partido.x, partido.y) %>%
    distinct()
}

#' @title Transforma autorias detalhada das proposições 
#' em nós e arestas de coautoria.
#' @param autorias_detalhadas_df Dataframe de autorias detalhadas
transform_nos_e_arestas <- function(autorias_detalhadas_df) {

  autorias_df <- autorias_detalhadas_df %>%
    filter(!is.na(partido), sigla_tipo != "PEC") %>%
    mutate(partido = if_else(partido == "PODE", "PODEMOS", partido)) %>% # Padroniza
    group_by(proposicao) %>%
    mutate(partidos = n_distinct(partido)) %>% 
    distinct(proposicao, partido, partidos) %>% 
    filter(partidos > 1) %>% 
    mutate(peso_aresta = 1/partidos) %>% 
    ungroup() %>% 
    select(-partidos)
  
  coautorias_df <- autorias_df %>% 
    full_join(autorias_df, 
              by = c("proposicao", "peso_aresta")) %>% 
    filter(partido.x != partido.y) %>% 
    .remove_duplicated_edges() %>% 
    group_by(partido.x, partido.y) %>% 
    summarise(peso_total_arestas = sum(peso_aresta), 
              .groups = "drop") %>% 
    ungroup() %>% 
    filter(peso_total_arestas >= 0.1) # Filtra por peso mínimo
  
  nodes <- autorias_df %>% 
    distinct(partido) %>%
    rowid_to_column("index") %>% 
    mutate(index = index - 1) # Índice deve começar em 0
    
  edges <- coautorias_df %>% 
    left_join(nodes, by=c("partido.x" = "partido")) %>% 
    left_join(nodes, by = c("partido.y" = "partido")) %>% 
    select(source = index.x, target = index.y, peso_total_arestas) 

  nodes = nodes %>% 
    rename(Id = index, Label = partido) # para o GEPHI
    
  return(list(nos = nodes, arestas = edges))
}
