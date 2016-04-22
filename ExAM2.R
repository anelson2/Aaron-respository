

log.growth.theta <- function(t, y, p) {
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N * (1 - (N / K)^theta)
    return(list(dN.dt))
  })
}

library(deSolve)

p <- c('r' = 0.2, 'K' = 1.05, 'theta' = 1.05)
y0 <- c('N' = runif(1, min = 0.01, max = 1.05))
        
        t <- 1:100
sim <- ode(y = y0, times = t, func = log.growth.theta, parms = p, method = 'lsoda')

sim<- ode(y = y0, times = t, func = log.growth.theta, parms = p, method = 'lsoda')
