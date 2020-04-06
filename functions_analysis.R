combo <- function (beta_trial, K_trial, init, tf, pars) {
  pars["beta"] <- beta_trial
  pars["K"] <- K_trial
  output <- as.data.frame(lsodes(init, tf, seivd.model, pars))
}

pop_size <- function (trial, output) {
  Ns <- output[[2,trial]]+output[[3,trial]]+output[[4,trial]]+output[[6,trial]]
  Ns[length(Ns)]
}

pop_MCMC <- function (output) {
  Ns <- output$S + output$E + output$I + output$V
  Ns[length(Ns)]
}

annual_pop <- function (output, time_vec, number_of_years) {
  year_points <- seq(length(time_vec) - number_of_years*365/diff(time_vec[1:2]), 
                     length(time_vec), by = 365/diff(time_vec[1:2]))
  Ns <- output$S[year_points] + output$E[year_points] + output$I[year_points] + output$V[year_points]
  Ns[-length(Ns)]
}

monthly_rabid <- function (output, time_vec) {
  jumper <- 30/diff(time_vec[1:2])
  month_points <- seq(length(time_vec) - jumper*12, length(time_vec), by = jumper)
  diff(output$It[month_points])
}

run_rabies_K<-function (K_in, init, tf, pars){
  pars["K"]<-K_in
  outputforK<-as.data.frame(lsoda(init, tf, seivd.model, pars))
}

run_rabies_Beta <- function (Beta_in, init, tf, pars) {
  pars["beta"] <- Beta_in
  outputforbeta <- as.data.frame(lsoda(init, tf, seivd.model, pars))
}

monthly_humans <- function (output, time_vec, km2, bite_rate) {
  model_inc <- mean(monthly_rabid(output, time_vec))*km2*bite_rate
  model_inc
}

annual_rabid <- function (output, time_vec, number_of_years) {
  year_points <- seq(length(time_vec) - number_of_years*365/diff(time_vec[1:2]), length(time_vec), by = 365/diff(time_vec[1:2]))
  diff(output$It[year_points])
}

annual_humans <- function (output, time_vec, number_of_years, bite_rate) {
  model_inc <- annual_rabid(output, time_vec, number_of_years)*bite_rate
  model_inc
}