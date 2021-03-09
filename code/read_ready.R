library(readr)

read_proposicoes = function() {
  read_csv(
    here::here("data/ready/proposicoes.csv"),
    col_types = cols(
      .default = col_character(),
      data_apresentacao = col_datetime(format = "")
    )
  ) %>%
    mutate(efeito = factor(
      efeito,
      levels = c("Divergente", "Neutro", "Convergente"),
      ordered = T
    ))
}

read_autorias_res = function() {
  read_csv(
    here::here("data/ready/autorias-resumo.csv"),
    col_types = cols(
      .default = col_character(),
      governismo = col_double(),
      assinadas = col_double(),
      positivas = col_integer(),
      negativas = col_integer(),
      neutras = col_integer(),
      peso_politico = col_double(),
      autorias_ponderadas = col_double()
    )
  )
}

read_autorias_det = function() {
  read_csv(
    here::here("data/ready/autorias-detalhes.csv"),
    col_types = cols(
      .default = col_character(),
      governismo = col_double(),
      assinadas = col_integer(),
      autorias_ponderadas = col_double(),
      coautores = col_integer()
    )
  )
}

read_atuacao <- function() {
  read_csv(
    here::here("data/ready/atuacao.csv"),
    col_types = cols(
      .default = col_character(),
      data = col_datetime(),
      peso_autor_documento = col_double(),
      governismo = col_double(),
      governismo_ma = col_double(),
      peso_politico = col_double()
    )
  )
}

read_relatoria <- function() {
  read_csv(
    here::here("data/ready/relatorias.csv"),
    col_types = cols(
      .default = col_character(),
      governismo = col_double(),
      peso_politico = col_double()
    )
  )
}

read_votos_resumo <-
  function(arquivo = "data/ready/votos-camara-resumo.csv") {
    read_csv(
      here::here(arquivo),
      col_types = cols(
        .default = col_double(),
        nome = col_character(),
        casa = col_character(),
        id_entidade_parlametria = col_character(),
        partido = col_character(),
        uf = col_character()
      )
    )
  }

read_votos_detalhe <-
  function(arquivo = "data/ready/votos-camara-detalhes.csv") {
    read_csv(
      here::here(arquivo),
      col_types = cols(
        .default = col_character(),
        data = col_datetime(format = ""),
        governismo = col_double(),
        governismo_ma = col_double(),
        peso_politico = col_double()
      )
    )
  }

read_votacoes <- function(arquivo = "data/ready/votacoes.csv") {
  read_csv(
    here::here(arquivo),
    col_types = cols(
      .default = col_double(),
      orientacao_ma = col_character(),
      nome_proposicao = col_character(),
      ementa_proposicao = col_character(),
      obj_votacao = col_character(),
      resumo = col_character(),
      data = col_datetime(format = ""),
      autor = col_character(),
      tema = col_character(),
      id_votacao = col_character(),
      casa = col_character()
    )
  )
}

read_nos_partidos <-
  function(arquivo = "data/ready/nos-partidos.csv") {
    read_csv(here::here(arquivo),
             col_types = cols(index = col_integer(),
                              partido = col_character()))
  }

read_arestas_partidos <-
  function(arquivo = "data/ready/arestas-partidos.csv") {
    read_csv(
      here::here(arquivo),
      col_types = cols(
        source = col_integer(),
        target = col_integer(),
        peso_total_arestas = col_double()
      )
    )
  }
