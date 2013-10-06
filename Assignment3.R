library(TSA)
library(fGarch)
library(xtable)

ARMA11 <- function(n, phi, theta) {
  return( 
    data.frame(
      var_phi = ((1 - phi**2) / n) * (((1 - phi * theta) / (phi - theta))**2)
      var_theta = ((1 - theta**2) / n) * (((1 - phi * theta) / (phi - theta))**2)
      corr = sqrt((1 - phi**2) * (1 - theta**2)) / (1 - phi * theta)
    )
  )
}

ARMA.MC <- function(reps, phi = 0.5, theta = -0.45, mean = 3, err = list(mean = 0, sd = 1)) {
  # chained output initializers
  output1 <- output2 <- matrix(0, ncol = 3, nrow = reps)
    
  for (i in 1:reps) {
    
  }
}

