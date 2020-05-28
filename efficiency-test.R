efficiency.test <- function(home_odds,draw_odds,away_odds,Res,omit.messages = T){
  # Method developed by Nyberg (2014)
  
  # Compute the implicit probabilities for each match outcome
  p_home  <- 1/home_odds       # Probability of a Home victory.
  p_draw  <- 1/draw_odds       # Probability of a draw outcome.
  p_away  <- 1/away_odds       # Probabilidad of an Away victory
  
  # Calculate the average fee that bookmakers charge
  over_round = p_home + p_draw + p_away - 1
  
  # Normalize
  p_home  <- p_home*(1/(over_round+1))
  p_draw  <- p_draw*(1/(over_round+1))
  p_away  <- p_away*(1/(over_round+1))
  
  # Variables to use in the computation of the likelihood function
  y_away <- ifelse(Res=="A",1,0)
  y_draw <- ifelse(Res=="D",1,0)
  y_home <- ifelse(Res=="H",1,0)
  
  # Logit for home victory outcome
  pi_home <- function(alpha,beta) {
    return(alpha + beta*log(p_home/p_draw))
  }
  
  # Logit for away victory outcome
  pi_away <- function(alpha,beta){
    return(alpha + beta*log(p_away/p_draw))
  }
  
  # Probabilities using the multinomial model.
  
  # Probability of a home victory given the implicit probabilities
  prob_home <- function(theta) exp(pi_home(theta[3],theta[4])) / (exp(pi_away(theta[1],theta[2])) +
    exp(pi_home(theta[3],theta[4])) + 1)
  
  # Probability of a draw outcome given the implicit probabilities
  prob_draw <- function(theta) 1 / (exp(pi_away(theta[1],theta[2])) +
    exp(pi_home(theta[3],theta[4])) + 1)
    
  # Probability of an away victory outcome given the implicit probabilities:
  prob_away <- function(theta) exp(pi_away(theta[1],theta[2])) / (exp(pi_away(theta[1],theta[2])) +
    exp(pi_home(theta[3],theta[4])) + 1)
  
  # likelihood function
  l <- function(theta){
    -1*sum(y_home*log(prob_home(theta)) + y_draw*log(prob_draw(theta)) + y_away*log(prob_away(theta)))
  }
  
  # Maximum Likelihood Estimator
  theta_final <- optim(par = rep(0,4),fn = l,hessian=TRUE)
  
  # Likelihood under the null hypothesis (the market is efficient in the weak form)
  L0 <- -1*l(c(0,1,0,1))
  
  # Likelihood under the maximum likelihood estimator model
  L1 <- -1*l(theta_final$par)
  
  # Likelihood ratio
  LR <- 2*(L1-L0)
  
  # p value
  p = 1-pchisq(LR,4)
  
  theta <- theta_final$par
  
  # The Hessian matrix of the maximum likelihood estimator; Fisher's information matrix 
  hessian <- theta_final$hessian
  
  # The inverse of the Hessian matrix, which is an estimator of the covariance matrix of theta.
  covariance <- solve(hessian)
  
  # Standard errors 
  std.errors <- sqrt(diag(covariance))
  if (!omit.messages){
  print(paste("Likelihood: ", L1))
  print(paste("Likelihood (Under the Null Hypothesis):",L0))
  print(paste("Likelihood-Ratio",LR,"p-value: ", p))
  print("Estimated parameters: ")
  print(theta_final$par)
  }
  
  # fitted values
  fitted.values_home <- prob_home(theta)
  fitted.values_draw <- prob_draw(theta)
  fitted.values_away <- prob_away(theta)
  fitted.values <- cbind(fitted.values_home,fitted.values_draw,fitted.values_away)
  return(list(theta=theta_final$par,l = function(x) -l(x), p = p, fitted.values=fitted.values, LR = LR, L1 = L1, L0 = L0,std.errors = std.errors))
}
