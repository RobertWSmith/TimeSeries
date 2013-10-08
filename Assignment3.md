% Assignment #3
% Robert Smith
% \today





1. Consider an ARMA(1,1) model with $\phi=0.5$ and $\theta=-0.45$ with $\mu=3$ 
  and $\sim N(0,1)$ errors.


```r

library(TSA)
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
    ))
}

print(
  xtable(
    ARMA11(n = 100, phi = 0.5, theta = -0.45), 
    digits = 6, 
    caption = "Question 1-a"
  ), type = "latex", 
  caption.placement = "top")
```

\begin{table}[ht]
\centering
\caption{Question 1-a} 
\begin{tabular}{rrrr}
  \hline
 & var\_phi & var\_theta & corr \\ 
  \hline
1 & 1.801875 & 1.915994 & 0.997917 \\ 
   \hline
\end{tabular}
\end{table}

```r

print(
  xtable(
    ARMA11(n = 300, phi = 0.5, theta = -0.45), 
    digits = 6, 
    caption = "Question 1-b"
  ), type = "latex", 
  caption.placement = "top")
```

\begin{table}[ht]
\centering
\caption{Question 1-b} 
\begin{tabular}{rrrr}
  \hline
 & var\_phi & var\_theta & corr \\ 
  \hline
1 & 0.600625 & 0.638665 & 0.997917 \\ 
   \hline
\end{tabular}
\end{table}

```r

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
  ggtitle(paste(expression(phi), expression(mu)))

plot2 <- ggplot(VARS, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(VARS$data)))/50 ) +    
  geom_density(color = "red", fill = NA) + 
  ggtitle(paste(expression(phi), expression(sigma)))

MLE <- as.numeric(MLE[[1]]); VARS <- as.numeric(VARS[[1]])
print(
  xtable(
    data.frame(
      phi.mean = mean(MLE),
      phi.sd = sd(MLE),
      phi.err.mean = sqrt(mean(VARS))
      ), digits = 6
    ), type = "latex"
  )
```

\begin{table}[ht]
\centering
\begin{tabular}{rrrr}
  \hline
 & phi.mean & phi.sd & phi.err.mean \\ 
  \hline
1 & 0.468435 & 0.118857 & 0.116013 \\ 
   \hline
\end{tabular}
\end{table}

```r

MLE <- as.data.frame(Q1C.100[,2])
VARS <- as.data.frame(Q1C.100[,5])
names(MLE) <- names(VARS) <- "data"

plot3 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(theta), expression(mu)))

plot4 <- ggplot(VARS, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(VARS$data)))/50 ) +    
  geom_density(color = "red", fill = NA) + 
  ggtitle(paste(expression(theta), expression(sigma)))

MLE <- as.numeric(MLE[[1]]); VARS <- as.numeric(VARS[[1]])
print(
  xtable(
    data.frame(
      theta.mean = mean(MLE),
      theta.sd = sd(MLE),
      theta.err.mean = mean(VARS)
      ), digits = 6
    ), type = "latex"
  )
```

\begin{table}[ht]
\centering
\begin{tabular}{rrrr}
  \hline
 & theta.mean & theta.sd & theta.err.mean \\ 
  \hline
1 & 0.468351 & 0.126399 & 0.014071 \\ 
   \hline
\end{tabular}
\end{table}

```r

MLE <- as.data.frame(Q1C.100[,3])
VARS <- as.data.frame(Q1C.100[,6])
names(MLE) <- names(VARS) <- "data"

plot5 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(mu), expression(mu)))

plot6 <- ggplot(VARS, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(VARS$data)))/50 ) +    
  geom_density(color = "red", fill = NA) + 
  ggtitle(paste(expression(mu), expression(sigma)))

MLE <- as.numeric(MLE[[1]]); VARS <- as.numeric(VARS[[1]])
print(
  xtable(
    data.frame(
      mean.mean = mean(MLE),
      mean.sd = sd(MLE),
      mean.err.mean = mean(VARS)
      ), digits = 6
    ), type = "latex"
  )
```

\begin{table}[ht]
\centering
\begin{tabular}{rrrr}
  \hline
 & mean.mean & mean.sd & mean.err.mean \\ 
  \hline
1 & 2.998905 & 0.281142 & 0.079504 \\ 
   \hline
\end{tabular}
\end{table}

```r

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)
```

![plot of chunk Q1A](figure/Q1A.png) 


The monte carlo simuation indicates that $\phi \approx 0.5$  and $\theta \approx 0.5$, but $\sigma_{\phi} > \sigma_{\theta}$

The simulation indicates that as N approaches infinity that the MLE will converge towards the theoretical value.

2. Consider an ARMA(1,1) with $\phi = 0.5$ and $\theta = 0.45$ with $\mu=3$ and $\sim N(0,1)$ errors.


```r

print(
  xtable(
    ARMA11(n = 1000, phi = 0.5, theta = 0.45), 
    digits = 6, 
    caption = "Question 1-a"
  ), type = "latex", 
  caption.placement = "top")
```

\begin{table}[ht]
\centering
\caption{Question 1-a} 
\begin{tabular}{rrrr}
  \hline
 & var\_phi & var\_theta & corr \\ 
  \hline
1 & 0.001247 & 0.001326 & 0.631335 \\ 
   \hline
\end{tabular}
\end{table}

```r

print(
  xtable(
    ARMA11(n = 3000, phi = 0.5, theta = 0.45), 
    digits = 6, 
    caption = "Question 1-b"
  ), type = "latex", 
  caption.placement = "top")
```

\begin{table}[ht]
\centering
\caption{Question 1-b} 
\begin{tabular}{rrrr}
  \hline
 & var\_phi & var\_theta & corr \\ 
  \hline
1 & 0.000416 & 0.000442 & 0.631335 \\ 
   \hline
\end{tabular}
\end{table}

```r

# 1000 reps - Monte Carlo
Q2C.1000 <- ARIMA.MC(reps = 1000, sample.size = 100, phi = 0.5, theta = -0.45, 
                    mean = 3, err = list(mean = 0, sd = 1) )

MLE <- as.data.frame(Q2C.1000[,1])
VARS <- as.data.frame(Q2C.1000[,4])
names(MLE) <- names(VARS) <- "data"

plot1 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(phi), expression(mu)))

plot2 <- ggplot(VARS, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(VARS$data)))/50 ) +    
  geom_density(color = "red", fill = NA) + 
  ggtitle(paste(expression(phi), expression(sigma)))

MLE <- as.numeric(MLE[[1]]); VARS <- as.numeric(VARS[[1]])
print(
  xtable(
    data.frame(
      phi.mean = mean(MLE),
      phi.sd = sd(MLE),
      phi.err.mean = sqrt(mean(VARS))
      ), digits = 6
    ), type = "latex"
  )
```

\begin{table}[ht]
\centering
\begin{tabular}{rrrr}
  \hline
 & phi.mean & phi.sd & phi.err.mean \\ 
  \hline
1 & 0.472301 & 0.114298 & 0.115582 \\ 
   \hline
\end{tabular}
\end{table}

```r

MLE <- as.data.frame(Q2C.1000[,2])
VARS <- as.data.frame(Q2C.1000[,5])
names(MLE) <- names(VARS) <- "data"

plot3 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(theta), expression(mu)))

plot4 <- ggplot(VARS, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(VARS$data)))/50 ) +    
  geom_density(color = "red", fill = NA) + 
  ggtitle(paste(expression(theta), expression(sigma)))

MLE <- as.numeric(MLE[[1]]); VARS <- as.numeric(VARS[[1]])
print(
  xtable(
    data.frame(
      theta.mean = mean(MLE),
      theta.sd = sd(MLE),
      theta.err.mean = mean(VARS)
      ), digits = 6
    ), type = "latex"
  )
```

\begin{table}[ht]
\centering
\begin{tabular}{rrrr}
  \hline
 & theta.mean & theta.sd & theta.err.mean \\ 
  \hline
1 & 0.465877 & 0.122766 & 0.013990 \\ 
   \hline
\end{tabular}
\end{table}

```r

MLE <- as.data.frame(Q2C.1000[,3])
VARS <- as.data.frame(Q2C.1000[,6])
names(MLE) <- names(VARS) <- "data"

plot5 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(mu), expression(mu)))

plot6 <- ggplot(VARS, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(VARS$data)))/50 ) +    
  geom_density(color = "red", fill = NA) + 
  ggtitle(paste(expression(mu), expression(sigma)))

MLE <- as.numeric(MLE[[1]]); VARS <- as.numeric(VARS[[1]])
print(
  xtable(
    data.frame(
      mean.mean = mean(MLE),
      mean.sd = sd(MLE),
      mean.err.mean = mean(VARS)
      ), digits = 6
    ), type = "latex"
  )
```

\begin{table}[ht]
\centering
\begin{tabular}{rrrr}
  \hline
 & mean.mean & mean.sd & mean.err.mean \\ 
  \hline
1 & 2.991591 & 0.279044 & 0.080233 \\ 
   \hline
\end{tabular}
\end{table}

```r

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)
```

![plot of chunk Q2A](figure/Q2A1.png) 

```r

## 3000 reps - Monte Carlo
Q2C.3000 <- ARIMA.MC(reps = 3000, sample.size = 100, phi = 0.5, theta = -0.45, 
                    mean = 3, err = list(mean = 0, sd = 1) )

MLE <- as.data.frame(Q2C.3000[,1])
VARS <- as.data.frame(Q2C.3000[,4])
names(MLE) <- names(VARS) <- "data"

plot1 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(phi), expression(mu)))

plot2 <- ggplot(VARS, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(VARS$data)))/50 ) +    
  geom_density(color = "red", fill = NA) + 
  ggtitle(paste(expression(phi), expression(sigma)))

MLE <- as.numeric(MLE[[1]]); VARS <- as.numeric(VARS[[1]])
print(
  xtable(
    data.frame(
      phi.mean = mean(MLE),
      phi.sd = sd(MLE),
      phi.err.mean = sqrt(mean(VARS))
      ), digits = 6
    ), type = "latex"
  )
```

\begin{table}[ht]
\centering
\begin{tabular}{rrrr}
  \hline
 & phi.mean & phi.sd & phi.err.mean \\ 
  \hline
1 & 0.467435 & 0.118408 & 0.116611 \\ 
   \hline
\end{tabular}
\end{table}

```r

MLE <- as.data.frame(Q2C.3000[,2])
VARS <- as.data.frame(Q2C.3000[,5])
names(MLE) <- names(VARS) <- "data"

plot3 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(theta), expression(mu)))

plot4 <- ggplot(VARS, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(VARS$data)))/50 ) +    
  geom_density(color = "red", fill = NA) + 
  ggtitle(paste(expression(theta), expression(sigma)))

MLE <- as.numeric(MLE[[1]]); VARS <- as.numeric(VARS[[1]])
print(
  xtable(
    data.frame(
      theta.mean = mean(MLE),
      theta.sd = sd(MLE),
      theta.err.mean = mean(VARS)
      ), digits = 6
    ), type = "latex"
  )
```

\begin{table}[ht]
\centering
\begin{tabular}{rrrr}
  \hline
 & theta.mean & theta.sd & theta.err.mean \\ 
  \hline
1 & 0.468838 & 0.122106 & 0.014221 \\ 
   \hline
\end{tabular}
\end{table}

```r

MLE <- as.data.frame(Q2C.3000[,3])
VARS <- as.data.frame(Q2C.3000[,6])
names(MLE) <- names(VARS) <- "data"

plot5 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(mu), expression(mu)))

plot6 <- ggplot(VARS, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(VARS$data)))/50 ) +    
  geom_density(color = "red", fill = NA) + 
  ggtitle(paste(expression(mu), expression(sigma)))

MLE <- as.numeric(MLE[[1]]); VARS <- as.numeric(VARS[[1]])
print(
  xtable(
    data.frame(
      mean.mean = mean(MLE),
      mean.sd = sd(MLE),
      mean.err.mean = mean(VARS)
      ), digits = 6
    ), type = "latex"
  )
```

\begin{table}[ht]
\centering
\begin{tabular}{rrrr}
  \hline
 & mean.mean & mean.sd & mean.err.mean \\ 
  \hline
1 & 3.005862 & 0.292485 & 0.080175 \\ 
   \hline
\end{tabular}
\end{table}

```r

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)
```

![plot of chunk Q2A](figure/Q2A2.png) 


3. Plot histogram of $\hat{\phi}$ and $\hat{\theta}$ from Problem 1 and for $n = 1000$, $n = 3000$ from problem 2. 


```r

Q1C.100 <- ARIMA.MC(reps = 1000, sample.size = 100, phi = 0.5, theta = -0.45, 
                    mean = 3, err = list(mean = 0, sd = 1) )

MLE <- as.data.frame(Q1C.100[,1])

plot1 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(phi), expression(mu)))
```

```
Warning: no non-missing arguments to min; returning Inf
Warning: no non-missing arguments to max; returning -Inf
```

```r

MLE <- as.data.frame(Q1C.100[,2])

plot2 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(theta), expression(mu)))
```

```
Warning: no non-missing arguments to min; returning Inf
Warning: no non-missing arguments to max; returning -Inf
```

```r


## 3000 reps - Monte Carlo
Q2C.1000 <- ARIMA.MC(reps = 1000, sample.size = 100, phi = 0.5, theta = -0.45, 
                    mean = 3, err = list(mean = 0, sd = 1) )

MLE <- as.data.frame(Q2C.1000[,1])

plot3 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(phi), expression(mu)))
```

```
Warning: no non-missing arguments to min; returning Inf
Warning: no non-missing arguments to max; returning -Inf
```

```r

MLE <- as.data.frame(Q2C.1000[,2])

plot4 <- ggplot(MLE, aes(x = data, y = ..density..)) +    
  geom_histogram(binwidth = sum(abs(range(MLE$data)))/50 ) +    
  geom_density(color = "red", fill = NA) +
  ggtitle(paste(expression(theta), expression(mu)))
```

```
Warning: no non-missing arguments to min; returning Inf
Warning: no non-missing arguments to max; returning -Inf
```

```r

grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
```

```
Error: arguments imply differing number of rows: 0, 1000
```

```r

```
