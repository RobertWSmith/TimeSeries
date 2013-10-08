# library(knitr)
# # compile Rmd -> md
# knit("Assignment3.Rmd", "A3.md")
# # compile md -> tex -> pdf
# pandoc("A3.md", format="latex")


library(TSA)
library(fGarch)
library(xtable)
library(ggplot2)
library(gridExtra)

ARMA11 <- function(n, phi, theta) {
  # modify theta for Cryer definition
  theta <- theta * -1 
  return(
    data.frame(
      var_phi = ((1 - phi**2) / n) * (((1 - phi * theta) / (phi - theta))**2),
      var_theta = ((1 - theta**2) / n) * (((1 - phi * theta) / (phi - theta))**2),
      corr = sqrt((1 - phi**2) * (1 - theta**2)) / (1 - phi * theta)
    )
  )
}


ARIMA.MC <- function(reps, sample.size, phi, theta, mean, err = list(mean = 0, sd = 1)) {
  # correcting to Cryer theta notation
  theta <- theta * -1
  # chained output initializers
  mle <- vars <- matrix(0, ncol = 3, nrow = reps)

  for (i in 1:reps) {

    sim <- arima.sim(
      n = sample.size, 
      list(ar = phi, ma = theta), 
      innov = rnorm(sample.size, err$mean, err$sd)
      ) + mean

    est <- arima(sim, order = c(1, 0, 1))
    mle[i,] <- est$coef
    vars[i,] <- diag(est$var.coef)
  }

  return(as.data.frame(cbind(mle,vars)))
}


Q1C.100 <- ARIMA.MC(reps = 1000, sample.size = 100, phi = 0.5, theta = -0.45, 
                    mean = 3, err = list(mean = 0, sd = 1) )

MLE <- as.data.frame(Q1C.100[,1])
VARS <- as.data.frame(Q1C.100[,4])
names(MLE) <- names(VARS) <- "data"

plot1 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) + 
  ggtitle("Maximum Likelihood Estimator")

plot2 <- ggplot(VARS, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(VARS$data)))/50 ) +    
  geom_density(color = "red", fill = NA) + 
  ggtitle("Variable Coefficients")

grid.arrange(plot1, plot2, ncol=2)


