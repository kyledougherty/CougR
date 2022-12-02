fit_hmm <- function(data, n_states = 3, state_names = c("Encamped", "Meandering", "Directed"),
                    distributions = list(step = "gamma", angle = "wrpcauchy"),
                    initial_step_values = list(15, "75%", "90%"),
                    initial_turn_values = list(0.05, 0.25, 0.75),
                    n_simulations, n_tries){
  
  tryCatch({
    # Prep imputed data and pull step lengths 
    steps <- prepData(data) %>% 
      as_tibble() %>% 
      filter(!is.na(step)) %>%
      pull(step)
    
    # Get quantiles and zero mass value, desigm matrices 
    # will be different depending on if the zero mass 
    # parameter is needed or not
    quantiles = quantile(steps, seq(0, 1, 0.01))
    zero_mass = length(steps[steps == 0])/length(steps)
    
    if(n_states == 3){
      
      if(zero_mass == 0){
        
        # Set design matrix such that state 3 must 
        # be longer than states 1 and 2, and state 
        # 2 must be longer than state 1
        stepDM = matrix(c(1,0,0,0,0,0,
                          1,1,0,0,0,0,
                          1,1,1,0,0,0,
                          0,0,0,1,0,0,
                          0,0,0,0,1,0,
                          0,0,0,0,0,1),
                        2*n_states,
                        2*n_states,
                        byrow=TRUE)
        
        # Set lower bound of state 3 step to 0, which forces it to be positive
        stepworkBounds = matrix(c(-Inf,-Inf,0, -Inf, -Inf, -Inf,
                                  Inf, Inf, Inf, Inf, Inf, Inf),
                                2*n_states,
                                2,
                                dimnames=list(NULL, c("lower","upper")))
        
        # Set step bounds that are interpretable, rather than working bounds
        # Here state 1 is bounded 0-100, whereas states 2/3 are unbounded
        stepBounds = matrix(c(0, 30, 
                              0, Inf, 
                              0, Inf, 
                              0, 30, 
                              0, Inf, 
                              0, Inf), 
                            2*n_states, 
                            2, 
                            byrow = TRUE)
        
        # Set state 3 to have higher concentration than states 1 and 2
        angleDM = matrix(c(1,0,0,
                           0,1,0,
                           1,1,1),
                         n_states, 
                         3,
                         byrow=TRUE)
        
        # Set lower bound of state 3 concentration, which force it to be positive
        angleworkBounds = matrix(c(-Inf,-Inf, 0,
                                   Inf, Inf, Inf),
                                 n_states,
                                 2,
                                 dimnames=list(colnames(angleDM),c("lower","upper")))
        
        angleBounds = matrix(c(0, 0, 0.5, 
                               0.5, 0.5, 0.99), 
                             n_states, 
                             2)
        
        # Get initial working parameters (these results are not as directly interpreable
        # as the values for step length/turning angle concentration that are supplied)
        # Set step parameters to desired quantiles such that they would reasonably correspond
        # to a stationary (short), meandering (intermediate), and directed state (long) 
        # and turning angle concentration such that the first two states have very little
        # directional persistence and state three has strong directional persistence
        initial_parameters <- getParDM(nbStates = n_states,
                                       dist = distributions,
                                       Par=list(step=c(initial_step_values[[1]], quantiles[[initial_step_values[[2]]]], quantiles[[initial_step_values[[3]]]], 
                                                       initial_step_values[[1]], quantiles[[initial_step_values[[2]]]], quantiles[[initial_step_values[[3]]]]),
                                                angle = c(initial_turn_values[[1]], initial_turn_values[[2]], initial_turn_values[[3]])),
                                       DM = list(step = stepDM, angle = angleDM),
                                       workBounds = list(step = stepworkBounds, angle = angleworkBounds), 
                                       userBounds = list(step = stepBounds, angle = angleBounds))
        
      } else{
        
        # The code below is virtually the same as that above, but includes the zero
        # inflation parameter is several spaces. 
        stepDM = matrix(c(1,0,0,0,0,0,0,0,0,
                          1,1,0,0,0,0,0,0,0,
                          1,1,1,0,0,0,0,0,0,
                          0,0,0,1,0,0,0,0,0,
                          0,0,0,0,1,0,0,0,0,
                          0,0,0,0,0,1,0,0,0,
                          0,0,0,0,0,0,1,0,0,
                          0,0,0,0,0,0,0,1,0,
                          0,0,0,0,0,0,0,0,1),
                        3*n_states,
                        3*n_states,
                        byrow=TRUE)
        
        stepworkBounds = matrix(c(-Inf,-Inf,0, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf,
                                  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf),
                                3*n_states,
                                2,
                                dimnames=list(NULL, c("lower","upper")))
        
        stepBounds = matrix(c(0, 30, 
                              0, Inf, 
                              0, Inf, 
                              0, 30, 
                              0, Inf, 
                              0, Inf, 
                              0, 1,
                              0, 1, 
                              0, 1), 
                            3*n_states, 
                            2, 
                            byrow = TRUE)
        
        angleDM = matrix(c(1,0,0,
                           1,1,0,
                           1,1,1),
                         n_states, 
                         3,
                         byrow=TRUE)
        
        # Set lower bound of state 3 concentration, which force it to be positive
        angleworkBounds = matrix(c(-Inf,-Inf, 0,
                                   Inf, Inf, Inf),
                                 n_states,
                                 2,
                                 dimnames=list(colnames(angleDM),c("lower","upper")))
        
        angleBounds = matrix(c(0, 0, 0.5, 
                               0.5, 0.5, 0.99), 
                             n_states, 
                             2)
        
        initial_parameters <- getParDM(nbStates = n_states,
                                       dist = distributions,
                                       Par=list(step=c(initial_step_values[[1]], quantiles[[initial_step_values[[2]]]], quantiles[[initial_step_values[[3]]]], 
                                                       initial_step_values[[1]], quantiles[[initial_step_values[[2]]]], quantiles[[initial_step_values[[3]]]],
                                                       rep(zero_mass, 3)),
                                                angle = c(initial_turn_values[[1]], initial_turn_values[[2]], initial_turn_values[[3]])),
                                       zeroInflation = list(step = TRUE),
                                       DM = list(step = stepDM, angle = angleDM),
                                       workBounds = list(step = stepworkBounds, angle = angleworkBounds), 
                                       userBounds = list(step = stepBounds, angle = angleBounds))
        
      }
    } else if(n_states == 2){
      
      if(zero_mass == 0){
        
        stepDM = matrix(c(1,0,0,0,
                          1,1,0,0,
                          0,0,1,0,
                          0,0,0,1),
                        2*n_states,
                        2*n_states,
                        byrow=TRUE)
        
        # Set lower bound of state 2 step to 0, which forces it to be positive
        stepworkBounds = matrix(c(-Inf,0, -Inf, -Inf,
                                  Inf, Inf, Inf, Inf),
                                2*n_states,
                                2,
                                dimnames=list(NULL, c("lower","upper")))
        
        
        stepBounds = matrix(c(0, 1000, 
                              0, Inf, 
                              0, Inf, 
                              0, Inf), 
                            2*n_states, 
                            2, 
                            byrow = TRUE)
        
        angleDM = matrix(c(1,0,
                           1,1),
                         n_states, 
                         n_states,
                         byrow=TRUE)
        
        # Set lower bound of state 3 concentration, which force it to be positive
        angleworkBounds = matrix(c(-Inf, 0,
                                   Inf, Inf),
                                 n_states,
                                 2)
        
        angleBounds = matrix(c(0, 0.5, 
                               0.5, 0.99), 
                             n_states, 
                             2)
        
        # Get initial working parameters (these results are not as directly interpreable
        # as the values for step length/turning angle concentration that are supplied)
        # Set step parameters to desired quantiles such that they would reasonably correspond
        # to a stationary (short), meandering (intermediate), and directed state (long) 
        # and turning angle concentration such that the first two states have very little
        # directional persistence and state three has strong directional persistence
        initial_parameters <- getParDM(nbStates = n_states,
                                       dist = distributions,
                                       Par=list(step=c(quantiles[[initial_step_values[[1]]]], quantiles[[initial_step_values[[2]]]],
                                                       quantiles[[initial_step_values[[1]]]], quantiles[[initial_step_values[[2]]]]),
                                                angle = c(initial_turn_values[[1]], initial_turn_values[[2]])),
                                       DM = list(step = stepDM, angle = angleDM),
                                       workBounds = list(step = stepworkBounds, angle = angleworkBounds), 
                                       userBounds = list(step = stepBounds, angle = angleBounds))
        
      } else{
        
        stepDM = matrix(c(1,0,0,0,0,0,
                          1,1,0,0,0,0,
                          0,0,1,0,0,0,
                          0,0,0,1,0,0,
                          0,0,0,0,1,0,
                          0,0,0,0,0,1),
                        3*n_states,
                        3*n_states,
                        byrow=TRUE)
        
        stepworkBounds = matrix(c(-Inf,0, -Inf, -Inf, -Inf, -Inf,
                                  Inf, Inf, Inf, Inf, Inf, Inf),
                                3*n_states,
                                2,
                                dimnames=list(NULL, c("lower","upper")))
        
        stepBounds = matrix(c(0, 1000, 
                              0, Inf, 
                              0, Inf, 
                              0, Inf,
                              0, 1, 
                              0, 1), 
                            3*n_states, 
                            2, 
                            byrow = TRUE)
        
        angleDM = matrix(c(1,0,
                           1,1),
                         n_states, 
                         2,
                         byrow=TRUE)
        
        angleworkBounds = matrix(c(-Inf, 0,
                                   Inf, Inf),
                                 n_states,
                                 2,
                                 dimnames=list(colnames(angleDM),c("lower","upper")))
        
        angleBounds = matrix(c(0, 0.5, 
                               0.5, 0.99), 
                             n_states, 
                             2)
        
        initial_parameters <- getParDM(nbStates = n_states,
                                       dist = distributions,
                                       Par=list(step=c(quantiles[[initial_step_values[[1]]]], quantiles[[initial_step_values[[2]]]],
                                                       quantiles[[initial_step_values[[1]]]], quantiles[[initial_step_values[[2]]]],
                                                       rep(zero_mass, 2)),
                                                angle = c(initial_turn_values[[1]], initial_turn_values[[2]])),
                                       zeroInflation = list(step = TRUE),
                                       DM = list(step = stepDM, angle = angleDM),
                                       workBounds = list(step = stepworkBounds, angle = angleworkBounds), 
                                       userBounds = list(step = stepBounds, angle = angleBounds))
        
      }
      
    }
    
    prior = function(par){sum(dnorm(par,0,10,log=TRUE))}
    
    hmm_result <- MIfitHMM(data, 
                           nSims = n_simulations,
                           ncores = 1, 
                           nbStates = n_states, 
                           retryFits = n_tries,
                           dist = distributions,
                           Par0 = list(step = initial_parameters$step, angle = initial_parameters$angle),
                           DM = list(step = stepDM, angle = angleDM), 
                           workBounds = list(step = stepworkBounds, angle = angleworkBounds),
                           userBounds = list(step = stepBounds, angle = angleBounds),
                           prior = prior,
                           stateNames = state_names)
    
    print(paste("Completed:", names(data$crwFits), "at", Sys.time()))
    
    return(hmm_result)
    
  }, error = function(e){
    print(paste(names(data$crwFits), e))
  })
  
}