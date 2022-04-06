get_rubricas <- function(receitas, codes, name) {
  data <- receitas %>%
    filter(rubrica_code %in% codes) %>%
    group_by(periodo) %>%
    summarise(value = sum(valor))
  colnames(data) <- c("periodo", name)
  return(data)
}
