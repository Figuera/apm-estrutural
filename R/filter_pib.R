filter_pib <- function() {
  pib <- rbcb::get_series(22099)[, 2, drop = T]

  focus <- rbcb::get_market_expectations("quarterly", "PIB Total", start_date = Sys.Date() - 14)

  focus <- focus %>%
    filter(Data == max(Data), baseCalculo == 1) %>%
    mutate(DataReferencia = as.Date(paste0("1/", DataReferencia), "%d/%m/%Y")) %>%
    arrange(DataReferencia) %>%
    transmute(value = cumprod(1 + Mediana / 100))

  pib <- c(pib, pib[length(pib)] * focus$value)
  pib <- ts(pib, start = 1995, frequency = 4)

  filter <- mFilter::hpfilter(pib, freq = 1600)
  h_pib <- (pib - filter$trend) / filter$trend

  tibble::tibble(periodo = zoo::yearqtr(time(h_pib)), h_pib = as.numeric(h_pib), trend = filter$trend)
}
