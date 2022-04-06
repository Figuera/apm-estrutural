library(rtn)
library(tidyverse)
library(dplyr)
library(lubridate)

process_revenue <- function(value) {
  receitas  <- rtn::get_revenue_data()
  trt_share <- readr::read_rds("data/trt_share.rds")
  tm_share  <- readr::read_rds("data/tm_share.rds")

  trt_share <- trt_share %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    summarise(share = mean(share))
  tm_share <- tm_share %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    summarise(pis_share = mean(pis), cofins_share = mean(cofins))

  receitas %<>% mutate(
    month        = month(Data),
    periodo      = str_c(year(Data), "_", quarter(Data)),
    rubrica_code = str_extract(Rubrica, "[\\d,\\.]*"),
    rubrica_name = str_extract(Rubrica, "[A-Z,a-z].*$"),
    valor = {{value}})


  # Split IRPF from work related income (trt) and Capital related income (TGP)
  trt_new_lines        <- rtn::rev_new_lines(receitas, "1.1.03.1", trt_share, 1)
  tgc_new_lines        <- rtn::rev_new_lines(receitas, "1.1.03.1", trt_share, 2)
  # Split COFINS from Importation-related or not
  tm_cofins_new_lines  <- rtn::rev_new_lines(receitas, "1.1.05", transmute(tm_share, month, share = cofins_share), 1)
  ti_cofins_new_lines  <- rtn::rev_new_lines(receitas, "1.1.05", transmute(tm_share, month, share = cofins_share), 2)
  # Split PIS from Importation-related or not
  tm_pis_new_lines     <- rtn::rev_new_lines(receitas, "1.1.06", transmute(tm_share, month, share = pis_share),    1)
  ti_pis_new_lines     <- rtn::rev_new_lines(receitas, "1.1.06", transmute(tm_share, month, share = pis_share),    2)
  # Add new lines to data.frame
  receitas <- rbind(receitas, trt_new_lines, tgc_new_lines, tm_cofins_new_lines,
    tm_pis_new_lines, ti_cofins_new_lines, ti_pis_new_lines)

  # Create Revenue Groups
  trt  <- rtn::get_rubricas(filter(receitas, Data >= as.Date("2000-1-1")), c("1.1.03.1-1", "1.1.03.3.1"), "trt")
  tfp  <- rtn::get_rubricas(receitas, c("1.4.6", "1.3"), "tfp")
  trc  <- rtn::get_rubricas(receitas, c("1.1.03.2", "1.1.07"), "trc")
  tgc  <- rtn::get_rubricas(receitas %>% filter(Data >= as.Date("2000-1-1")), c("1.1.03.1-2", "1.1.03.3.2"), "tgc")
  tran <- rtn::get_rubricas(receitas, c("2.1", "2.3", "2.4"), "tran")

  ti_1  <- rtn::get_rubricas(filter(receitas, Data < as.Date("2004-05-1")), c("1.1.05", "1.1.06"), "ti")
  ti_2  <- rtn::get_rubricas(filter(receitas, Data >= as.Date("2004-05-1")), c("1.1.05-2", "1.1.06-2"), "ti")
  ti    <- rbind(ti_1, ti_2) %>% group_by(periodo) %>% summarise(ti = sum(ti))
  tm_1  <- rtn::get_rubricas(filter(receitas, Data < as.Date("2004-05-1")), c("1.1.01"), "tm")
  tm_2  <- rtn::get_rubricas(filter(receitas, Data >= as.Date("2004-05-1")), c("1.1.01", "1.1.05-1", "1.1.06-1"), "tm")
  tm    <- rbind(tm_1, tm_2) %>% group_by(periodo) %>% summarise(tm = sum(tm))

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
