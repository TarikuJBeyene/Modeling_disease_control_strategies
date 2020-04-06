## Created by Meagan Fitzpatrick, Center for Infectious Disease Modeling and Analysis
## Yale School of Public Health
## In collaboration with Tariku Beyene

source("Rabies_Eth_Fullbasemodel.R")
source("functions_analysis.R")
library(MASS)

# District size
Lemu_km <- 1184  # rural km2
Lemu_human_pop <-  187222 # rural human population, Central Statistics Authority 2013
Lemu_mean <- 35.4
Lemu_SE <- 0.290

Bishoftu_km <- 40  # urban km2
Bishoftu_human_pop <- 140039 # urban human population, Central Statistics Authority 2013
Bish_mean <- 340
Bish_SE <- 5.16

# Montly human rabies data
human_data <- read.csv("Monthlybitecases.csv", header = TRUE, row.names = 1)

# Rabies deaths
Lemu_death <- 8

#loglikelihood function

loglik_calc <- function (data_human_exp, model_human_exp,
                         data_human_death, model_human_death,
                         data_mean_pop, data_SE_pop, model_mean_pop) {
  ll_exp <- dpois(data_human_exp, model_human_exp, log = TRUE)
  ll_death <- dpois(data_human_death, model_human_death, log = TRUE)
  ll_pop <- dnorm(data_mean_pop, model_mean_pop, data_SE_pop, log = TRUE)
  ll_exp + ll_death + ll_pop
}

# Start up
draws <- 10000 # How many draws? In the final, 10K or higher. Short for testing.
ll_keep <- rep(NA, times = draws)  # set up vector to store log likelihood
momentum <- rep(0, times = draws)
beta_jump <- rep(NA, times = draws)
beta_keep <- rep(NA, times = draws)
beta_keep[1] <- 0.42
beta_jump[1] <- beta_keep[1]
human_vec <- rep(NA, times = draws)
human_keep <- human_vec
death_vec <- rep(NA, times = draws)
death_keep <- death_vec

K_jump <- rep(NA, times = draws)
K_jump[1] <- 52000
K_keep <- K_jump
pop_vec <- rep(NA, times = draws)
pop_keep <- pop_vec

alpha_vec <- 1/rgamma(draws, shape = 450, scale = 0.0069)
alpha_keep <- alpha_vec
sigma_vec <- 1/rnorm(draws, mean = 22.3, sd = 1.28)
sigma_keep <- sigma_vec
bite_vec <- rnorm(draws, 0.51, 0.03)
bite_keep <- bite_vec

Lemu_PEP_vec <- rbeta(draws, 95, 189-95) # From Tariku
Lemu_PEP_keep <- Lemu_PEP_vec

prob_of_woundsites <- c(0.0476,0.217,0.0529,0.6825) # head/neck,upper extremity,trunk,lower extremity; from Tariku
prob_of_rabies_wrt_woundsite<-c(0.55,0.22,0.09,0.12) # from Eunha Shim's 2009 paper
### turn this into probabilities, waiting for info from Katie


multinomial_sample <- rmultinom(draws,189,prob_of_woundsites)/189 # sampling from mulinomial
prob_of_dev_rabies <- t(multinomial_sample)%*%prob_of_rabies_wrt_woundsite
dev_rabies <- prob_of_dev_rabies[,1]
dev_rabies_keep <- dev_rabies

# rural demographic parameters (Lemu)
params <- c(beta = beta_keep[1], sigma = sigma_vec[1], alpha = alpha_vec[1], omega=0, 
            v=0, b = 0.397/365, mu = 0.35/365, K = K_jump[1])  #mu is adjusted
xstart <- c(S = 40000, E = 15, I = 2, It = 0, V = 0)
times <- seq(0, 1200000, by = 10) #in days
pb <- txtProgressBar(title = "progress bar", min = 0, max = draws, width = 100, style = 3)

# Run the model and get the first likelihood
rabies_out <- as.data.frame(lsoda(xstart, times, seivd.model, params))
human_vec[1] <- annual_humans(rabies_out, times, 1, bite_vec[1])
human_keep[1] <- human_vec[1]
death_vec[1] <- human_vec[1]*(1-Lemu_PEP_vec[1])*dev_rabies[1]
pop_vec[1] <- pop_MCMC(rabies_out)
pop_keep[1] <- pop_vec[1]
ll_keep[1] <- loglik_calc(sum(as.numeric(human_data[2,])), human_vec[1], Lemu_death,
                          death_vec[1], Lemu_mean, Lemu_SE,  pop_vec[1]/Lemu_km)

# MCMC
for (i in 2:draws) {
  beta_jump[i] <- beta_keep[i-1] + rnorm(1, 0, sd = 0.006) # 0.0075 gives ~10%
  K_jump[i] <- K_keep[i-1] + rnorm(1, 0, sd = 200) # 250 gives ~10%
  params["beta"] <- beta_jump[i]
  params["K"] <- K_jump[i]
  params["alpha"] <- alpha_vec[i]
  params["sigma"] <- sigma_vec[i]
  rabies_out <- as.data.frame(lsoda(xstart, times, seivd.model, params))
  human_vec[i] <- annual_humans(rabies_out, times, 1, bite_vec[i])
  death_vec[i] <- human_vec[i]*(1-Lemu_PEP_vec[i])*dev_rabies[i]
  pop_vec[i] <- pop_MCMC(rabies_out)
  ll_jump <- loglik_calc(sum(as.numeric(human_data[2,])), human_vec[i], Lemu_death,
                         death_vec[i], Lemu_mean, Lemu_SE,  pop_vec[i]/Lemu_km)
  
  ratio <- exp(ll_jump - ll_keep[i-1])
  # Roll the dice to accept or reject
  if (runif(1, 0, 1) < ratio) {
    # accept
    beta_keep[i] <- beta_jump[i]
    ll_keep[i] <- ll_jump
    human_keep[i] <- human_vec[i]
    death_keep[i] <- death_vec[i]
    pop_keep[i] <- pop_vec[i]
    K_keep[i] <- K_jump[i]
    momentum[i] <- 1
  } else {
    # reject
    beta_keep[i] <- beta_keep[i-1]
    ll_keep[i] <- ll_keep[i-1]
    human_keep[i] <- human_keep[i-1]
    death_keep[i] <- death_keep[i-1]
    pop_keep[i] <- pop_keep[i-1]
    K_keep[i] <- K_keep[i-1]
    alpha_keep[i] <- alpha_keep[i-1]
    sigma_keep[i] <- sigma_keep[i-1]
    bite_keep[i] <- bite_keep[i-1]
    Lemu_PEP_keep[i] <- Lemu_PEP_keep[i-1]
    dev_rabies_keep[i] <- dev_rabies_keep[i-1]
  }
  setTxtProgressBar(pb, i, title = paste(round(i/draws*100, 0), "% done"))
  save(list = ls(), file = paste0("MCMC_Lemu_chain4_", Sys.Date(), ".RData"))
  }

close(pb)
# burn_in <- 2005
# acceptability_ratio <- sum(momentum[(burn_in+1):draws])/length((burn_in+1):draws) # Needs to be between 0.15 and 0.4
# 
# post_subset <- seq(burn_in, draws, by = 5)
# post_K <- K_keep[post_subset]
# post_lik <- ll_keep[post_subset]
# quantile(post_K, probs = c(0.025, 0.975))



