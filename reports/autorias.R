cria_paleta <- function(domain, from = "#ffe4cc", to = "#ff9500") {
  function(x) {
    if (is.na(x)) {
      return(rgb(221, 221, 221, maxColorValue = 255))
    }
    normalized = (x - min(domain)) / (max(domain) - min(domain))
    rgb(colorRamp(c(from, to))(normalized), maxColorValue = 255)
  }
}

tbl_autorias_resumida = function(data) {
  escala_assinadas = cria_paleta(data$assinadas, from = "#feebe2", to = "#f768a1")
  escala_ponderadas = cria_paleta(data$autorias_ponderadas, from = "#feebe2", to = "#f768a1")
  escala_governismo = cria_paleta(data %>% filter(!is.na(governismo)) %>% pull(governismo),
                                  from = "#ffffcc",
                                  to = "#41b6c4")
  escala_peso = cria_paleta(data %>% filter(!is.na(peso_politico)) %>% pull(peso_politico),
                            from = "#f7f7f7",
                            to = "#969696")
  escala_positivas = cria_paleta(data %>% filter(!is.na(positivas)) %>% pull(positivas),
                                 from = "#f1eef6",
                                 to = "#2b8cbe")
  escala_negativas = cria_paleta(data %>% filter(!is.na(negativas)) %>% pull(negativas), 
                                 from = "#fef0d9", 
                                 to = "#e34a33")
  
  data %>%
    mutate(
      casa = if_else(casa == "camara", "dep", "sen"),
      nome = str_glue("{nome} ({casa})")
    ) %>%
    select(-casa,-neutras) %>%
    reactable(
      defaultPageSize = 15,
      compact = TRUE,
      searchable = T,
      defaultSorted = "assinadas",
      columns = list(
        nome = colDef(name = "Parlamentar", minWidth = 150),
        partido = colDef(name = "Partido", minWidth = 75),
        uf = colDef(name = "UF", minWidth = 50),
        # casa = colDef(name = "Casa", minWidth = 50),
        assinadas = colDef(
          name = "Prop. assinadas",
          minWidth = 70,
          defaultSortOrder = "desc",
          style = function(value) {
            list(background = escala_assinadas(value))
          }
        ),
        positivas = colDef(
          name = "Prop. positivas",
          minWidth = 70,
          defaultSortOrder = "desc",
          style = function(value) {
            list(background = escala_positivas(value))
          }
        ),
        negativas = colDef(
          name = "Prop. negativas",
          minWidth = 70,
          defaultSortOrder = "desc",
          style = function(value) {
            list(background = escala_negativas(value))
          }
        ),
        autorias_ponderadas = colDef(
          name = "Autorias ponderadas",
          minWidth = 70,
          format = colFormat(digits = 1),
          style = function(value) {
            list(background = escala_ponderadas(value))
          }
        ),
        governismo = colDef(
          name = "Governismo (-10 a 10)",
          minWidth = 70,
          format = colFormat(digits = 1),
          style = function(value) {
            if (is.na(value)) {
              list()
            } else{
              list(background = escala_governismo(value))
            }
          }
        ),
        peso_politico = colDef(
          name = "Peso político",
          minWidth = 60,
          format = colFormat(digits = 1),
          style = function(value) {
            if (is.na(value)) {
              list()
            } else{
              list(background = escala_peso(value))
            }
          }
        )
      )
    )
}

tbl_detalhes_autorias = function(data) {
  data %>%
    mutate(nome = str_glue("{nome} - {partido}/{uf}")) %>% 
    select(-autores, -uf, -casa, -partido) %>%
    reactable(
      defaultPageSize = 10,
      compact = TRUE,
      searchable = T,
      striped = FALSE,
      groupBy = c("nome"),
      defaultSorted = "assinadas",
      columns = list(
        nome = colDef(name = "Parlamentar", minWidth = 150),
        classificacao_ambientalismo = colDef(
          name = "Efeito", 
          minWidth = 50
        ),
        governismo = colDef(
          name = "Governismo",
          minWidth = 50,
          aggregate = "mean",
          format = colFormat(digits = 1)
        ),
        proposicao = colDef(
          name = "Proposições",
          minWidth = 100,
          aggregate = "count"
        ),
        assinadas = colDef(
          name = "Prop. assinadas",
          minWidth = 70,
          defaultSortOrder = "desc",
          aggregate = "sum"
        ),
        autorias_ponderadas = colDef(
          name = "Autorias ponderadas",
          minWidth = 70,
          aggregate = "sum",
          format = colFormat(digits = 2)
        ),
        coautores = colDef(name = "Coautores")
      )
    )
}
