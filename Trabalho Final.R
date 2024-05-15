# Trabalho Final

library(readxl)
library(rstanarm)
library(dplyr)
library(tidyverse)
library(patchwork)
library(ggplot2)

rm(list = ls(all.names = TRUE))

dados = read.csv("insurance.csv")

dados = dados %>% mutate_at(vars(sex,smoker,region), as.factor)

summary(dados$charges)

stan_glm(charges ~ age + sex + bmi + children + smoker + region, data = dados) %>%
  summary(digits = 3)

fit = stan_glm(charges ~ age + sex + bmi + children + smoker + region, data = dados)

summary(fit)

fit$coefficients

# posterior samples
post_rstan <- as.matrix(fit) %>%
  as.data.frame() %>% rename("Female_NSmoker_NE" = "(Intercept)")

post_rstan %>%
  head()

mu.Female_NSmoker_NE <- post_rstan$Female_NSmoker_NE
mu.Male_NSmoker_NE <- post_rstan$Female_NSmoker_NE + post_rstan$sexmale


rstan_results <- data.frame(mu.Female_NSmoker_NE, mu.Male_NSmoker_NE) %>%
  pivot_longer(cols = everything())

ggplot(rstan_results, aes(x = value, fill = as.factor(name))) +
  geom_histogram() + facet_wrap(~ name , ncol = 2) + theme(legend.position = "null")



