rm(list = ls())

library(deSolve)
library(lattice)

## restore the saved values to the current environment

load("D:/R projects/Yale_Eth model reintroduction/Bishoftu for higher bite rate/Bish_posteriorintro.Rdata")
source("functions_analysisintro.R")
source("Rabies_Eth_Fullbasemodelintro.R")

#for K#
quantile(K_Bish, probs = c(0.025, 0.975))
mean(K_Bish)/40
(quantile(K_Bish, probs = c(0.025, 0.975)))/40

#for sigma#
quantile(sigma_Bish, probs = c(0.025, 0.975))
1/mean(sigma_Bish)
1/(quantile(sigma_Bish, probs = c(0.025, 0.975)))

#for alpha#
quantile(alpha_Bish, probs = c(0.025, 0.975))
1/mean(alpha_Bish)
1/(quantile(alpha_Bish, probs = c(0.025, 0.975)))

#for beta#
quantile(beta_Bish, probs = c(0.025, 0.975))
mean(beta_Bish)

#for bite rate
quantile(bite_Bish, probs = c(0.025, 0.975))
mean(bite_Bish)

#for probability of developing rabies
quantile(dev_rabies_Bish, probs = c(0.025, 0.975))
mean(dev_rabies_Bish)

##for PEP post
quantile(PEP_Bish, probs = c(0.025, 0.975))
mean(PEP_Bish)

sample_numbers <- seq(1, 110)

alpha_predict_Bish <- alpha_Bish[sample_numbers]
beta_predict_Bish <- beta_Bish[sample_numbers]
K_predict_Bish <- K_Bish[sample_numbers]
sigma_predict_Bish <- sigma_Bish[sample_numbers]
bite_predict_Bish<-bite_Bish[sample_numbers]
dev_rabies_predict_Bish<-dev_rabies_Bish[sample_numbers]
ll_predict_Bish<-ll_Bish[sample_numbers]
PEP_predict_Bish<-PEP_Bish[sample_numbers]
vacc_predict_Bish<-vacc_Bish[sample_numbers]


camp_years_Bish <- 10
cov_Bish <- c(0.18,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
rabid_dogs_Bish <- array(NA, dim = c(length(sample_numbers), length(cov_Bish), camp_years_Bish)) #annual rabid dogs
human_output_Bish <- array(NA, dim = c(length(sample_numbers), length(cov_Bish), camp_years_Bish)) #annual exposed humans
vacc_dogs_Bish <- array(NA, dim = c(length(sample_numbers), length(cov_Bish), camp_years_Bish))


# Step 1: run model to equilibrium (50 years? 100? look at output) with 0 vaccination

# new urban demographic parameters (Bishoftu)
xstart <- c(S = 14000, E = 15, I = 2, It = 0, V = 0.18) 
times <- seq(0, 100000, by = 10) #in days, longer time to get to equilibrium

##run prediction for v=
for  (i in 1 :length (sample_numbers)){{
  params_Bish <- c(beta = beta_predict_Bish[i] , sigma = sigma_predict_Bish[i], alpha = alpha_predict_Bish[i], omega=0, 
              v=vacc_predict_Bish[i], b = 0.533/365, mu = 0, K = K_predict_Bish[i]) #mu = 0.35/365
  rabies_predict_Bish <- as.data.frame(lsoda(xstart, times, seivd.model, params_Bish))
  human_output_Bish[i,1,] <- annual_humans(rabies_predict_Bish, times, camp_years_Bish, bite_predict_Bish[i])
  rabid_dogs_Bish[i,1,] <- annual_rabid(rabies_predict_Bish, times, camp_years_Bish)
  vacc_dogs_Bish[i,1,] <- annual_pop(rabies_predict_Bish, times, camp_years_Bish)*cov_Bish[1]
  
  ## loop cov
  newtimes <- seq(0, 380*camp_years_Bish, by = 10) ## run over 10 years
  equilibrium <- rabies_predict_Bish[nrow(rabies_predict_Bish),]
  for (j in 2:length(cov_Bish)) {
    rabies_camp_Bish <- central_point_campaign(cov_Bish[j], camp_years_Bish, 
                                          equilibrium, params_Bish)
    rabid_dogs_Bish[i,j,] <- annual_rabid(rabies_camp_Bish, newtimes, camp_years_Bish)
    human_output_Bish[i,j,] <- annual_humans(rabies_camp_Bish, newtimes, camp_years_Bish, bite_predict_Bish[i])
    vacc_dogs_Bish[i,j,] <- annual_pop(rabies_camp_Bish, newtimes, camp_years_Bish)*cov_Bish[j]
    }
  }
# save(list = ls(), file = paste0("predict_", Sys.Date(), ".RData"))
}

save(list = ls(), file = paste0("Trial_11_Bishoftu_new_99biterate", Sys.Date(), ".RData"))


