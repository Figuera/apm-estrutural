library(rtn)
library(tidyverse)
library(lubridate)

process_revenue <- function(value) {
  receitas  <- rtn::get_revenue_data() %>%
    mutate(month        = month(Data),
    periodo      = str_c(year(Data), "_", quarter(Data)),
    rubrica_code = str_extract(Rubrica, "[\\d,\\.]*"),
    rubrica_name = str_extract(Rubrica, "[A-Z,a-z].*$"),
    valor        = valor_historico)#{{value}})

  calc(receita)

  # Create Revenue Groups
  trt  <- get_rubricas(receitas, c("1.1.03.1", "1.1.03.3.1"), "trt")
  tfp  <- get_rubricas(receitas, c("1.4.6", "1.3"),           "tfp")
  trc  <- get_rubricas(receitas, c("1.1.03.2", "1.1.07"),     "trc")
  tgc  <- get_rubricas(receitas, c("1.1.03.3.2"),             "tgc")
  tran <- get_rubricas(receitas, c("2.1", "2.3", "2.4"),      "tran")
  ti   <- get_rubricas(receitas, c("1.1.05", "1.1.06"),       "ti")
  tm   <- get_rubricas(receitas, c("1.1.01"),                 "tm")

  # Get ICMS data on the correct format
  icms <- get_icms() %>%
    mutate(periodo = str_c(year(data), "_", quarter(data))) %>%
    group_by(periodo) %>%
    summarise(ICMS = sum(valor / 10^6))

  # Join everything in a comprehensive dataset
  receita <- full_join(trt, tfp) %>%
    full_join(trc)  %>%
    full_join(ti)   %>%
    full_join(tm)   %>%
    full_join(tgc)  %>%
    full_join(tran) %>%
    full_join(icms) %>%
    arrange(periodo)
  receita$periodo <- zoo::yearqtr(seq(from = 1997, length.out = nrow(receita), by = 0.25))

  return(receita)
}
