# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:

library(zoo)
setwd("/home/apoema/gitapps/EstruturalSPE/")
source("R/process_revenue.R")
source("R/rev_new_lines.R")
source("R/get_rubricas.R")
source("R/get_icms.R")
source("R/filter_pib.R")

rev  <- process_revenue(valor_atualizado)
Hpib <- filter_pib()
Hpib$Hpib1 <- c(Hpib$Hpib[-1], NA)
Hpib$Hpib2 <- c(Hpib$Hpib[-1:-2], rep(NA, 2))
Hpib$Hpib3 <- c(Hpib$Hpib[-1:-3], rep(NA, 3))
Hpib$Hpib4 <- c(Hpib$Hpib[-1:-4], rep(NA, 4))
Hpiba4 <- rep(0, 115)
for(i in 4:115)
  Hpiba4[i] <- mean(Hpib$Hpib[(i-3):i])
Hpib$Hpiba4 <- Hpiba4
Hpiba6 <- rep(0, 115)
for(i in 6:115)
  Hpiba6[i] <- mean(Hpib$Hpib[(i-5):i])
Hpib$Hpiba6 <- Hpiba6

ggplot(Hpib, aes(x = periodo, y = Hpiba4)) + geom_line() + geom_line(aes(y = Hpib), linetype="dashed")

rev[,-1] <- log(rev[,-1])
dados <- left_join(rev, Hpib)
dados <- zoo(dados[,-1], order.by=dados$periodo)

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
  best <- list(logLik=-Inf)
  for(i in 1:n) {
    init <- c(level = -1, slope = -2, seas = -3, irregular = -0.5, Hpiba4 = -8, Hpib = -8, trend = -8)
    if(i > 1)
      init <- c(level = runif(1, -17, 0), slope = runif(1, -17, 0), seas = runif(1, -17, 0), regres = runif(1, -17, 0), irregular = runif(1, -17, 0), Hpiba4 = runif(1, -17, 0))
    x <- decompose(formula, data = dados, init = init, ...)
    if(x$logLik > best$logLik){
      print(x$fit$par)
      best <- x
    }
  }
  return(best)
}

init <- c(level = -1, slope = -2, seas = -3, irregular = -0.5, Hpiba4 = -8, Hpib = -8, trend = -8)
debugonce(decompose)
TRT  <- decompose(TRT ~ level() + slope() + seas() + Hpiba4(0), window(dados, start = 2000), init=init)
TRT  <- search_best_fit(TRT ~ level() + slope(0) + seas() + Hpiba4(0), window(dados, start = 2000), n = 100)
TRT2  <- search_best_fit(TRT ~ level(0) + slope() + seas() + Hpiba4(0), window(dados, start = 2000), n = 1000)
ggplot(tibble(), aes(x= 1:1000, y = TRT2$full)) + geom_point() + ylim(c(0, 90))
coef(TRT)
TRT$fit
TRT2$fit

summary_tests(TRT)
summary_tests(TRT2$best)
summary_tests(iTRT)
iTRT$logLik
TRT2$best$logLik
debugonce(summary_tests)
iTRT <- TRT2$best %>% autocreate_interventions()
coef(iTRT)
TRT$mod$Q
TRT$mod$H
TRT$mod$a1
TRT$mod$P1
TRT$varpars
x <- as_tibble(coef(iTRT))
x$time <- time(coef(iTRT))
x$y    <- window(dados$TRT, start = 2000)
x$x    <- window(dados$Hpib1, start = 2000)
x$I    <- rep(0, length(x$x))
x$I[81] <- 1
ggplot(x, aes(x = time)) + geom_line(aes(y = y)) + geom_line(aes(y = level), linetype="dashed") + geom_line(aes(y = level + sea_trig1 + sea_trig2), linetype="dotted") + geom_line(aes(y = level + sea_trig1 + sea_trig2 + Hpiba4*x), color = "red") + geom_line(aes(y = level + sea_trig1 + sea_trig2 + Hpiba4*x + I.principal.84*I), color = "red", linetype = "dotted")
ggplot(x, aes(x = time)) + geom_line(aes(y = H))
TRT$mod$P1
TRT$mod$a1
coef(TFP)
TFP  <- search_best_fit(TFP ~ level() + slope() + seas() + Hpib(0), dados, a1 = list(a1 = NA, P1 = NA), n= 10)
TFP$fit
TFP
testar.ssm(TFP)
debugonce(autocreate_interventions)
TFP %<>% autocreate_interventions()
new_interventions
second_chance
ssm$a1
TRC  <- search_best_fit(TRC ~ level() + slope() + seas() + Hpib(0), dados)
TI   <- search_best_fit(TI ~ level() + slope() + seas() + Hpib(0), dados)
TM   <- search_best_fit(TM ~ level() + slope() + seas() + Hpib1(0), dados)
TGC  <- search_best_fit(TGC ~ level() + slope() + seas() + Hpib1(0), window(dados, start = 2000))
TRAN <- search_best_fit(TRAN ~ level() + slope() + seas() + Hpib(0), dados)
ICMS <- search_best_fit(ICMS ~ level() + slope() + seas() + Hpib(0), dados)

log(diag(TRT$model$Q[,,1]))
str(TRT$mod$a1)
TRT$logLik
TRT$model
str(TRT$fit)

str(TRT)

model <- TRT

x <- autocreate_interventions(TRT)
coef(x)
x$mod$Q
