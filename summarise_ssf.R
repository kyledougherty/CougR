library(tidyverse)
library(broom)

summarize_ssf <- function(model){
  
  AIC = tibble(Formula = paste(names(model$coefficients), collapse = "+"), 
               LogLik = as.numeric(logLik(model)), 
               DF = attributes(logLik(model))$df, 
               AICc = AICc(model))
  
  model_summary = tidy(model) %>%
    select(-statistic) %>%
    left_join(as_tibble(summary(model)$conf.int, 
                        rownames = "term") %>%
                mutate(across(contains("95"), 
                              log)) %>%
                select(term, contains("95")), 
              by = "term")
  
  AIC %>%
    mutate(Summary = list(model_summary))
}