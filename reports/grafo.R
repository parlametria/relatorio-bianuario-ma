#' @title Cria o grafo
#' @description Recebe dois dataframes correspondendo aos nós e arestas
#' e gera um grafo
#' @param nodes Nós do grafo
#' @param edges Arestas do grafo
#' @return Grafo com nós e arestas
generate_graph <- function(nodes, edges) {
  library(networkD3)
  
  fn <- forceNetwork(
    Links = edges, 
    Nodes = nodes,
    Source = "source", 
    Target = "target",
    Value = "peso_total_arestas", 
    NodeID = "Label",
    Group ="Label", 
    zoom = T,
    linkColour = "#bfbdbd",
    fontFamily = "roboto",
    fontSize = 6,
    opacity = 0.8)
  
  return(fn)
}