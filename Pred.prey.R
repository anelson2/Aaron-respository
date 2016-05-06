library(deSolve)

pred.prey <- function(t, y, p) {
  H <- y[1]
  P <- y[2]
  with(as.list(p), {
    dH.dt <- r*H - b*H*P 
    dP.dt <- c*H*P - K*P
    
    return(list(c(dH.dt, dP.dt)))
  })
}

t <- 1:20
y0 <- c('H' = 1.1, 'P' = 1.1)
p <- c('r' = 1, 'b' = 1,
       'K' = 1, 'c' = 1)
       

sim <- ode(y = y0, times = t, func = pred.prey, parms = p,
method = 'lsoda')

sim <- as.data.frame(sim)

plot(H ~ time, type = 'l', col = 'blue', bty = 'l', data = sim)
points(P ~ time, type = 'l', lty = 2, data = sim)

