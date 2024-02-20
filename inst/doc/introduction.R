## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(savvyr)
library(kableExtra)

## ----include=TRUE, echo=TRUE--------------------------------------------------
n <- 200

min_cens <- 0
max_cens <- 1000

set.seed(2020)
dat1 <- generate_data(
  n,
  cens = c(min_cens, max_cens),
  haz_ae = 0.00265,
  haz_death = 0.00151,
  haz_soft = 0.00227
)

tau <- max(dat1[, "time_to_event"])

## ----include=TRUE, echo=TRUE--------------------------------------------------
kable(head(dat1, 10), align = c("crcr"))

## ----include=TRUE, echo=TRUE--------------------------------------------------
ip <- inc_prop(dat1, tau)
id <- prop_trans_inc_dens(dat1, tau)
km <- one_minus_kaplan_meier(dat1, tau)

idce_2 <- prop_trans_inc_dens_ce(dat1, ce = 2, tau)
aj_2 <- aalen_johansen(dat1, ce = 2, tau)

idce_3 <- prop_trans_inc_dens_ce(dat1, ce = 3, tau)
aj_3 <- aalen_johansen(dat1, ce = 3, tau)

## ----include=TRUE, echo=TRUE--------------------------------------------------
tab <- rbind(ip, id, km, idce_2, aj_2[1:2], idce_3, aj_3[1:2])
colnames(tab) <- c(
  "estimated AE probability",
  "variance of estimation"
)
rownames(tab) <- c(
  "incidence proportion",
  "probability transform incidence density ignoring competing event",
  "1 - Kaplan-Meier", "probability transform incidence density (death only)",
  "Aalen-Johansen (death only), AE risk", "probability transform incidence density (all CEs)",
  "Aalen-Johansen (all CEs), AE risk"
)

kable(tab, digits = c(3, 5))

## ----include=TRUE, echo=TRUE--------------------------------------------------
tab <- rbind(aj_2[3:4], aj_3[3:4])
colnames(tab) <- c(
  "estimated probability",
  "variance of estimation"
)
rownames(tab) <- c(
  "Aalen-Johansen (death only), CE risk",
  "Aalen-Johansen (all CEs), CE risk"
)

kable(tab, digits = c(3, 5))

