require(deSolve)
#SEIV with demography and vaccination 
seivd.model <- function(t, x, params) {
  S <- x[1]
  E <- x[2]
  I <- x[3]
  It <- x[4]
  V <- x[5]
  with(as.list(params), {
    N=S+E+I+V
    gamma=(b-mu)/K
    dS <- (1-v)*b*(S+E+V)-beta*S*I/K-mu*S-gamma*S*N+omega*V
    dE <- beta*S*I/K - sigma*E- mu*E-gamma*E*N
    dI <- sigma*E -alpha*I- mu*I -gamma*I*N
    dIt <- sigma*E
    dV <- v*b*(S+E+V)-mu*V-gamma*V*N-omega*V
    res <- c(dS, dE, dI, dIt, dV)
    list(res)
  })
}

central_point_campaign <- function (cov, camp_years, eq_row, pars) {
  pars["v"] <- 0
  camp_V <- (eq_row$S + eq_row$V)*cov
  camp_S <- (eq_row$S + eq_row$V)*(1-cov)
  camp_start <- c(S = camp_S, E = eq_row$E, I = eq_row$I, It = 0, V = camp_V)
  camp_time <- seq(0, 365, by = 5)
  camp_out <- as.data.frame(lsoda(camp_start, camp_time, seivd.model, pars))
  for (q in 2:camp_years) {
    camp_V <- (camp_out$S[nrow(camp_out)] + camp_out$V[nrow(camp_out)])*cov
    camp_S <- (camp_out$S[nrow(camp_out)] + camp_out$V[nrow(camp_out)])*(1-cov)
    camp_start <- c(S = camp_S, E = camp_out$E[nrow(camp_out)], I = camp_out$I[nrow(camp_out)], 
                    It = camp_out$It[nrow(camp_out)], V = camp_V)
    temp_out <- as.data.frame(lsoda(camp_start, camp_time, seivd.model, pars))
    camp_out <- rbind(camp_out[-nrow(camp_out),], temp_out)
  }
  camp_out
}
