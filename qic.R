qic <- function(model){
  # In a model fit with clogit(), 
  # the second element returned by 
  # loglik is the quasi-likelihood
  quasi_likelihood = model$loglik[2]
  
  # Solve gets the inverse of the naive variance/covariance matrix. 
  # Then taking the diagonal, do matrix multiplication with 
  # the variance/covariance matrix. Lastly, sum the result. 
  trace = sum(diag(solve(model$naive.var) %*% model$var))
  
  # Calculate QIC
  -2*quasi_likelihood + 2*trace
  
}