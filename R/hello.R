library(zoo)
setwd("/home/apoema/gitapps/EstruturalSPE/")
source("R/process_revenue.R")
source("R/rev_new_lines.R")
source("R/get_rubricas.R")
source("R/get_icms.R")
source("R/filter_pib.R")

rev  <- process_revenue(valor_atualizado)
h_pib <- filter_pib()
h_pib$H <- h_pib$trend_pib/h_pib$pib
h_pib$h_pib1 <- c(h_pib$h_pib[-1], NA)
h_pib$h_pib4 <- c(h_pib$h_pib[-1:-4], rep(NA, 4))

gathered <- gather(h_pib, "key", "value", -periodo)
ggplot(gathered %>% filter(key %in% c("pib", "piba", "trend_pib", "trend_piba")),
  aes(x = periodo, y = value, color = key)) + geom_line()

rev[, -1] <- log(rev[, -1])
dados <- left_join(rev, h_pib)
dados <- zoo(dados[,-1], order.by = dados$periodo)

library(KFAS)
source("../ElastH/R/build_ssm.R")
source("../ElastH/R/decompose.R")
source("../ElastH/R/update_fit.R")
source("../ElastH/R/get_variance.R")
source("../ElastH/R/search_interventions.R")
source("../ElastH/R/get_intervention_position.R")
source("../ElastH/R/autocreate_interventions.R")
source("../ElastH/R/test_intervs.R")
source("../ElastH/R/fitSSM2.R")
source("../ElastH/R/check_neighborhood.R")
source("../ElastH/R/summary_tests.R")
source("../ElastH/R/h_test.R")
source("../ElastH/R/normality_test.R")
source("../ElastH/R/R2.R")

library(tidyverse)

search_best_fit <- function(formula, dados, n=50, ...) {
  best <- list(logLik = -Inf)
  for (i in 1:n) {
    init <- c(level = -1, slope = -2, seas = -3, irregular = -0.5, h_piba4 = -8, h_pib = -8, trend = -8)
    if (i > 1)
      init <- c(level = runif(1, -17, 0), slope = runif(1, -17, 0), seas = runif(1, -17, 0),
        regres = runif(1, -17, 0), irregular = runif(1, -17, 0), h_piba4 = runif(1, -17, 0))
    x <- decompose(formula, data = dados, init = init, ...)
    if (x$logLik > best$logLik) {
      print(x$fit$par)
      best <- x
    }
  }
  return(best)
}

search_best_model <- function(receita, n = 25) {
  dados$receita <- dados[, receita, drop = T]

  h0 <- search_best_fit(receita ~ level() + slope() + seas() + h_pib(0), dados, n = n)
  h1 <- search_best_fit(receita ~ level() + slope() + seas() + h_pib1(0), dados, n = n)
  ha <- search_best_fit(receita ~ level() + slope() + seas() + h_piba(0), dados, n = n)

  aic1 <- summary_tests(h0)$aic
  aic2 <- summary_tests(h1)$aic
  aic3 <- summary_tests(ha)$aic

  list(h0, h1, ha)[[which.min(c(aic1, aic2, aic3))]]
}

trt <- search_best_model("trt", 25)
tfp <- search_best_model("tfp", 25)
trc <- search_best_model("trc", 25)
ti  <- search_best_model("ti", 25)
tm  <- search_best_model("tm", 25)
tran <- search_best_model("tran", 25)
icms <- search_best_model("ICMS", 25)


coefs <- list(
  trt  = coef(trt)[nrow(dados), 1],
  tfp  = coef(tfp)[nrow(dados), 1],
  trc  = coef(trc)[nrow(dados), 1],
  ti   = coef(ti)[nrow(dados), 1],
  tm   = coef(tm)[nrow(dados), 1],
  tran = coef(tran)[nrow(dados), 1],
  icms = coef(icms)[nrow(dados), 1]
)

hiatus_factor <- function(x) {
  h <- as_tibble(dados)[, names(x), drop = T]
  H <- 1 / (1 + h)

  (1 - H^x[1])
}

cycle <- (exp(dados[, c("trt", "tfp", "trc", "ti", "tm", "tran", "icms")]) * sapply(coefs, hiatus_factor)) %>% as.data.frame()
cycle$total <- rowSums(cycle)
cycle$periodo <- dados$periodo

nrow(cycle)
ggplot(cycle, aes(x = 1:101, y = total)) + geom_line()
