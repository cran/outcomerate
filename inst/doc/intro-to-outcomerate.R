## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 4,
  collapse = TRUE,
  comment = "#>"
)
options(dplyr.summarise.inform = FALSE)

## ----message=FALSE------------------------------------------------------------
# load packages
library(outcomerate)
library(dplyr)
library(tidyr)
library(knitr)

## ----message=FALSE------------------------------------------------------------
# load dataset
data(middleearth)

# tabulate frequency table of outcomes
kable(count(middleearth, code, outcome))

## ----include = FALSE----------------------------------------------------------
disposition_counts <- table(middleearth$code)
I <- disposition_counts[["I"]]
P <- disposition_counts[["P"]]
R <- disposition_counts[["R"]]
NC <- disposition_counts[["NC"]]
O <- disposition_counts[["O"]]
UO <- disposition_counts[["UO"]]

## -----------------------------------------------------------------------------
disp_counts <- c(I = 760, P = 339, R = 59, NC = 288, O = 1,
                 UH = 40, UR = 60, UO = 73, NE = 71)

e <- eligibility_rate(disp_counts)
outcomerate(disp_counts, e = e)

## ----category-specific-e------------------------------------------------------
e_by_class <- c(UH = 0.35, UR = 0.70, UO = 0.50)
outcomerate(
  disp_counts,
  e = e_by_class,
  rate = c("RR3", "RR4", "REF2", "CON2")
)

## -----------------------------------------------------------------------------
outcomerate(disp_counts, rate = c("RR1", "RR2"))

## -----------------------------------------------------------------------------
# print the head of the dataset
head(middleearth)

# calculate rates using codes; should be same result as before
outcomerate(middleearth$code, e = e)

## -----------------------------------------------------------------------------
# create a small wrapper function
get_rates <- function(x, ...){
  rlist <- c("RR1", "RR2", "COOP1", "COOP2", "CON1", "REF1", "LOC1")
  as.data.frame(as.list(outcomerate(x, rate = rlist, e = e, ...)))
}

# calculate rates by group
middleearth %>%
  group_by(race) %>%
  summarise(n     = n(),
            Nhat  = sum(svywt),
            rates = list(get_rates(code))) %>%
  unnest(cols = c(rates)) %>%
  kable(digits = 2, caption = "Outcome Rates by Race")

## -----------------------------------------------------------------------------
# calculate weighted rates by group
middleearth %>%
  group_by(region) %>%
  summarise(n     = n(),
            Nhat  = sum(svywt),
            rates = list(get_rates(code, weight = svywt))) %>%
  unnest(cols = c(rates)) %>%
  kable(digits = 2, caption = "Weighted Outcome Rates by Region")

## ----echo=FALSE---------------------------------------------------------------
# calculate weighted rates by group
middleearth %>%
  group_by(region) %>%
  summarise(n     = n(),
            Nhat  = sum(svywt),
            rates = list(get_rates(code))) %>%
  unnest(cols = c(rates)) %>%
  kable(digits = 2, caption = "Unweighted Outcome Rates by Region")

## -----------------------------------------------------------------------------
library(ggplot2)
library(stringr)

# day-by-day quality monitoring
middleearth %>%
  group_by(day) %>%
  summarise(rates = list(get_rates(code))) %>%
  unnest(cols = c(rates)) %>%
  gather(rate, value, -day) %>%
  mutate(type = str_sub(rate, start = -9, end = -2)) %>%
  ggplot(aes(x = day, y = value, colour = rate)) +
  geom_line(size = 1) +
  facet_wrap(~type) +
  labs(title = "Outcome Rates Over Time")

## -----------------------------------------------------------------------------
# first, calculate the outcome rates
(res <- outcomerate(middleearth$code))

# estimate standard errors using the Normal approximation for proportions 
se <- vapply(
  res,
  function(p) sqrt((p * (1 - p)) / nrow(middleearth)),
  numeric(1)
)

## -----------------------------------------------------------------------------
# calculate 95% confidence intervals
rbind(res - (se * 1.96), res + (se * 1.96))

