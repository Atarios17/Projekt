EOP <- function(S0,r,K,Time,dt,sigma){
  
  u <- exp(sigma*sqrt(dt))
  d <- exp(-sigma*sqrt(dt))
  n <- Time/dt
  p <- (exp(r*dt)-d)/(u-d)
  
  Suma <- 0
  
  for(j in 0:n){
    Suma <- Suma + choose(n,j)*p^j*(1-p)^(n-j)*max(S0*u^j*d^(n-j)-K,0)      
  }
  
  return(exp(-r*Time)*Suma)
  }

EOP(20,0.12,21,24,1,0.3)
