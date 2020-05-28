efficiency.test <- function(home_odds,draw_odds,away_odds,Res,omit.messages = T){
  # Método basado en Nyberg (2014)
  
  # Calculamos las probabilidades implícitas para cada uno de los 3 posibles resultados de un partido.
  
  p_home  <- 1/home_odds         # Probabilidad de que gane la casa
  p_draw  <- 1/draw_odds       # Probabilidad de que ocurra un empate
  p_away  <- 1/away_odds       # Probabilidad de que gana el visitante
  
  # Calculamos el margen (promedio) que se llevan las casas de apuestas
  
  over_round = p_home + p_draw + p_away 
  
  # Normalizamos las probabilidades
  
  p_home  <- p_home*(1/over_round)
  p_draw  <- p_draw*(1/over_round)
  p_away  <- p_away*(1/over_round)
  
  # Variables usadas para el cálculo de la función de verosimilitud
  y_away <- ifelse(Res=="A",1,0)
  y_draw <- ifelse(Res=="D",1,0)
  y_home <- ifelse(Res=="H",1,0)
  
  # Logit para el resultado H (home)
  
  pi_home <- function(alpha,beta) {
    return(alpha + beta*log(p_home/p_draw))
  }
  
  # Logit para el resultado A (away)
  
  pi_away <- function(alpha,beta){
    return(alpha + beta*log(p_away/p_draw))
  }
  
  # Probabilidades usando el modelo multinomial
  
    # Probabilidad de que la casa gane dado que se observaron las probabilidades implícitas:
  
  prob_home <- function(theta) exp(pi_home(theta[3],theta[4])) / (exp(pi_away(theta[1],theta[2])) +
    exp(pi_home(theta[3],theta[4])) + 1)
  
    # Probabilidad de suceda un empate dado que se observaron las probabilidades implícitas:
  
  prob_draw <- function(theta) 1 / (exp(pi_away(theta[1],theta[2])) +
    exp(pi_home(theta[3],theta[4])) + 1)
    
  # Probabilidad de que el visitante gane dado que se observaron las probabilidades implícitas:
  
  prob_away <- function(theta) exp(pi_away(theta[1],theta[2])) / (exp(pi_away(theta[1],theta[2])) +
    exp(pi_home(theta[3],theta[4])) + 1)
  
  # Función de verosimilitud (likelihood) NOTA: usamos el negativo para minimizar, que es equivalente a maximizar la función original
  
  l <- function(theta){
    -1*sum(y_home*log(prob_home(theta)) + y_draw*log(prob_draw(theta)) + y_away*log(prob_away(theta)))
  }
  
  # estimador de máxima verosimilitud; aquel que maximiza (o minimiza el negativo de) la función de verosimilitud
  theta_final <- optim(par = rep(0,4),fn = l,hessian=TRUE)
  # verosimilitud del modelo restringido a la hipótesis nula H_0
  L0 <- -1*l(c(0,1,0,1))
  # versommilitud del modelo usando el estimador de máxima verosimilitud
  L1 <- -1*l(theta_final$par)
  # Estadístico del radio de verosimilitudes
  LR <- 2*(L1-L0)
  # Calculamos el valor p, usando una distribución chi cuadrada de 4 grados de libertad (# de restricciones del modelo)
  p = 1-pchisq(LR,4)
  theta <- theta_final$par
  # La matriz Hessiana evaluada en el estimador de máxima verosimilitud; esta es la matriz observada de información de FIsher 
  hessian <- theta_final$hessian
  # A continuación calculamos la inversa de la matriz Hessiana, la cual es un estimador de la matriz de covarianza de
  # nuestro parámetro
  covariance <- solve(hessian)
  # por último, los errores estándar 
  std.errors <- sqrt(diag(covariance))
  if (!omit.messages){
  print(paste("Likelihood: ", L1))
  print(paste("Likelihood (Under the Null Hypothesis):",L0))
  print(paste("Likelihood-Ratio",LR,"p-value: ", p))
  print("Estimated parameters: ")
  print(theta_final$par)
  }
  # valores esperados utilizando el modelo
  fitted.values_home <- prob_home(theta)
  fitted.values_draw <- prob_draw(theta)
  fitted.values_away <- prob_away(theta)
  fitted.values <- cbind(fitted.values_home,fitted.values_draw,fitted.values_away)
  return(list(theta=theta_final$par,l = function(x) -l(x), p = p, fitted.values=fitted.values, LR = LR, L1 = L1, L0 = L0,std.errors = std.errors))
}
df <- read.csv("data/MEX.csv")
eff <- efficiency.test(home_odds = df$AvgH,draw_odds = df$AvgD,away_odds = df$AvgA,Res = df$Res,omit.messages = T)

