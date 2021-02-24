#' @title Ordena valores
#' @description Recebe dois valores e os retorna ordenados e
#' separados por um separador
#' @param x Primeiro valor a ser comparado
#' @param y Segundo valor a ser comparado
#' @param sep Character usado para ser o separador entre os valores
#' @return Valores ordenados separados por um separador.
paste_cols <- function(x, y, sep = ":") {
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
remove_duplicated_edges <- function(df) {
  df %>%
    mutate(col_pairs =
             paste_cols(partido.x,
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

