filter_pib <- function(freq = 1600) {
  pib <- rbcb::get_series(22099)[, 2, drop = T]

  focus <- rbcb::get_market_expectations("quarterly", "PIB Total", start_date = Sys.Date() - 14)

  focus <- focus %>%
    filter(Data == max(Data), baseCalculo == 1) %>%
    mutate(DataReferencia = as.Date(paste0("1/", DataReferencia), "%d/%m/%Y")) %>%
    arrange(DataReferencia) %>%
    transmute(value = cumprod(1 + Mediana / 100))

  pib  <- c(pib, pib[length(pib)] * focus$value)
  pib  <- ts(pib, start = 1995, frequency = 4)
  piba <- zoo::rollapply(pib, 4, mean, align = "right", fill = 100)

  filter_pib <- mFilter::hpfilter(pib, freq = freq)
  h_pib <- (pib - pib_filter$trend) / pib_filter$trend

  filter_piba <- mFilter::hpfilter(piba, freq = freq)
  h_piba <- (piba - piba_filter$trend) / piba_filter$trend

  tibble::tibble(
    periodo    = zoo::yearqtr(time(h_pib)),
    pib        = pib,
    piba       = piba,
    h_pib      = as.numeric(h_pib),
    trend_pib  = filter_pib$trend,
    h_piba     = as.numeric(h_piba),
    trend_piba = filter_piba$trend
  )

}
