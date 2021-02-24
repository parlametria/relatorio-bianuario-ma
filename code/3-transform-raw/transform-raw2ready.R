library(tidyverse)
library(futile.logger)
source(here::here("code/3-transform-raw/transform-raw2ready-lib.R"))
flog.threshold(TRACE, "transform-raw")

#' Main do script para uso em CLI.
#' Para uso interativo, chame main() no console
#'
main <- function(argv = NULL) {
  out_props = "data/ready/proposicoes.csv"
  out_autorias_detalhes = "data/ready/autorias-detalhes.csv"
  out_autorias_resumo = "data/ready/autorias-resumo.csv"
  out_relatorias = "data/ready/relatorias.csv"
  out_atuacao = "data/ready/atuacao.csv"
  out_votos_cam_detalhes = "data/ready/votos-camara-detalhes.csv"
  out_votos_sen_detalhes = "data/ready/votos-senado-detalhes.csv"
  out_votos_cam_resumo = "data/ready/votos-camara-resumo.csv"
  out_votos_sen_resumo = "data/ready/votos-senado-resumo.csv"
  out_votacoes = "data/ready/votacoes.csv"
  out_nos = "data/ready/nos-partidos.csv"
  out_arestas = "data/ready/arestas-partidos.csv"
  
  # PROPOSIÇÕES
  props = transform_proposicoes(
    "data/raw/leggo_data/proposicoes.csv",
    "data/inputs/3-transform-input/proposicoes/proposicoes_input.csv"
  )
  
  props %>% # temos uma linha por casa da proposição
    write_csv(here::here(out_props))
  flog.info(str_glue("Dados prontos das proposições em {out_props}"))
  
  # PARLAMENTARES
  parlamentares = parlamentares_data(
    parlamentares_file = "data/raw/leggo_data/entidades.csv",
    governismo_deps_file = "data/externo/governismo/governismo-ideal-deputados.csv",
    governismo_sens_file = "data/externo/governismo/governismo-ideal-senadores.csv",
    governismo_ma_file = "data/raw/governismo/governismo_ma.csv",
    peso_file = "data/raw/peso_politico/peso_politico.csv"
  )
  
  # AUTORIAS
  autorias = transform_autorias_detalhes(props,
                                         "data/raw/leggo_data/autores_leggo.csv",
                                         parlamentares)
  autorias %>%
    write_csv(here::here(out_autorias_detalhes))
  flog.info(str_glue("Detalhes de autorias em {out_autorias_detalhes}"))
  
  autorias_resumo = transform_autorias_resumo(props,
                                              "data/raw/leggo_data/autores_leggo.csv",
                                              parlamentares)
  autorias_resumo %>%
    write_csv(here::here(out_autorias_resumo))
  flog.info(str_glue("Resumo de autorias em {out_autorias_resumo}"))
  
  # RELATORIAS
  relatorias = transform_relatorias(props,
                                    "data/raw/leggo_data/relatores_leggo.csv",
                                    parlamentares)
  relatorias %>%
    write_csv(here::here(out_relatorias))
  flog.info(str_glue("Relatorias salvas em {out_relatorias}"))
  
  # ATUAÇÃO
  atuacao = transform_atuacao("data/raw/leggo_data/autorias.csv",
                              parlamentares)
  atuacao %>%
    write_csv(here::here(out_atuacao))
  flog.info(str_glue("Atuação salva em {out_atuacao}"))
  
  # VOTOS
  votos_cam_detalhes = transform_votos_detalhes(
    acontecidas_file = "data/raw/votos/votos_camara.csv",
    rotuladas_file = "data/raw/votos/votos-referencia.csv",
    parlamentares,
    casa_votacoes = "camara"
  )
  
  votos_sen_detalhes = transform_votos_detalhes(
    acontecidas_file = "data/raw/votos/votos_senado.csv",
    rotuladas_file = "data/raw/votos/votos-referencia.csv",
    parlamentares,
    casa_votacoes = "senado"
  )
  
  votos_cam_detalhes %>% write_csv(here::here(out_votos_cam_detalhes))
  votos_sen_detalhes %>% write_csv(here::here(out_votos_sen_detalhes))
  flog.info(
    str_glue(
      "Votos detalhados ready em {out_votos_cam_detalhes} e {out_votos_sen_detalhes}"
    )
  )
  
  votos_cam_resumo = transform_votos_resumo(
    acontecidas_file = "data/raw/votos/votos_camara.csv",
    rotuladas_file = "data/raw/votos/votos-referencia.csv",
    parlamentares,
    casa_votacoes = "camara"
  )
  
  votos_cam_resumo %>%
    write_csv(here::here(out_votos_cam_resumo))
  flog.info(str_glue("Votos resumidos ready em {out_votos_cam_resumo}"))
  
  flog.info(str_glue("SEM votos resumidos do senado por hora"))
  # Não temos votações suficientes do senado por enquanto
  # votos_s_resumo = transform_votacoes_resumo(
  #   acontecidas_file = "data/raw/votos/votos_senado.csv",
  #   rotuladas_file = "data/raw/votos/votos-referencia.csv",
  #   parlamentares,
  #   casa_votacoes = "senado"
  # )
  
  # VOTAÇÕES
  votacoes = transform_votacoes(
    rotuladas_file = "data/raw/votos/votos-referencia.csv",
    acontecidas_camara = "data/raw/votos/votos_camara.csv",
    acontecidas_senado = "data/raw/votos/votos_senado.csv"
  )
  
  votacoes %>% 
    write_csv(out_votacoes)
  
  flog.info(str_glue("Votações escritas em {out_votacoes}"))
  
  # NÓS E ARESTAS DE GRAFO DE PARTIDO
  nos_arestas <- transform_nos_e_arestas(autorias)
  
  write_csv(nos_arestas$nos, out_nos)
  flog.info(str_glue("Nós de coautorias dos partidos escritas em {out_nos}"))
  
  write_csv(nos_arestas$arestas, out_arestas)
  flog.info(str_glue("Arestas de coautorias dos partidos escritas em {out_arestas}"))
  
}

if (!interactive()) {
  argv <- commandArgs(TRUE)
  main(argv)
}
