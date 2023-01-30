library(tidyverse)
library(broom)
source("https://raw.githubusercontent.com/kyledougherty/CougR/main/qic.R")

summarise_ssf <- function(model){
  
  QIC = tibble(Formula = paste(names(model$coefficients), collapse = "+"), 
               LogLik = as.numeric(logLik(model)), 
               DF = attributes(logLik(model))$df, 
               QIC = qic(model))
  
  model_summary = tidy(model) %>%
    select(-statistic) %>%
    left_join(as_tibble(summary(model)$conf.int, 
                        rownames = "term") %>%
                mutate(across(contains("95"), 
                              log)) %>%
                select(term, contains("95")), 
              by = "term")
  
  QIC %>%
    mutate(Summary = list(model_summary))
}