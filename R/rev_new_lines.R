rev_new_lines <- function(receitas, code, share_df, n) {
  receitas %>% filter(rubrica_code == code) %>%
    left_join(share_df) %>%
    mutate(rubrica_code = paste0(rubrica_code, "-", n),
      valor_historico  = valor_historico * ifelse(n == 1, share, 1 - share),
      valor_atualizado = valor_atualizado * ifelse(n == 1, share, 1 - share)) %>%
    select(-share)
}
