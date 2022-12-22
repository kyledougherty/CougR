summarise_rsf <- function(model, unrescaled_data, confint_type = "Wald", 
                          random_effects = c("ID", "Year")){
  terms = rownames(summary(model)$coefficients)[-1]
  
  mean_used_available = unrescaled_data %>% 
    group_by(Used) %>% 
    dplyr::select(all_of(terms)) %>% 
    summarise(across(everything(),
                     mean)) %>% 
    pivot_longer(cols = !Used, 
                 names_to = "term") %>% 
    pivot_wider(values_from = value, 
                names_from = Used) %>% 
    dplyr::select(term, Used = `1`, Available = `0`)
  
  percent_available = unrescaled_data %>% 
    filter(Used == 0) %>%
    unite("ID", random_effects) %>% 
    group_by(ID)%>% 
    dplyr::select(ID, all_of(terms)) %>% 
    mutate(across(contains("Dist"), 
                  ~if_else(.x == 0, 
                           1, 
                           0))) %>% 
    summarise(across(contains("Dist"), 
                     ~(sum(.x)/length(.x))*100))
  
  data_summary = unrescaled_data %>% 
    dplyr::select(all_of(terms)) %>% 
    summarise(across(everything(), 
                     .fns = list(Mean = mean, 
                                 SD = sd))) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "term") %>%
    mutate(Statistic = str_extract(term, "Mean|SD"), 
           term = str_remove(term, "_Mean|_SD")) %>%
    pivot_wider(values_from = value, 
                names_from = Statistic)
  
  model_summary = summary(model)$coefficients %>% 
    as_tibble(rownames = "term") %>% 
    filter(term != "(Intercept)") %>% 
    left_join(confint(model, method = confint_type) %>%
                as_tibble(rownames = "term")) %>% 
    left_join(data_summary)
  
  list(Model_Summary = model_summary,
       Mean_Used_Available = mean_used_available, 
       Percent_Available = percent_available)
  
}