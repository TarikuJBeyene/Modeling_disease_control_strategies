rm(list = ls())
library(ggplot2)
library(reshape)
library(BCEA)
library(RColorBrewer)
library(mc2d)
library(msm)
library(triangle)
load("D:/R projects/Yale_Ethiopia model_lemu/lemu final by Meagan/Lemu new trial/Trial_11_2017-07-02.Rdata")


cov <- c(0, seq(0.1, 0.9, by = 0.1))

Birr_to_USD<-21.6   #Currency rate (Currency rate, january 2016; $US 1 = 21.6birr) 

esample <- length(sample_numbers)
bc <- which.max(ll_predict[sample_numbers]) #15 ### UPDATE TO MOST LIKELY TRIAL
discount <- 0.03
time_frame <- 10
GDP<-568 #new World bank Ethiopian GDP per capita

##
###CI calculation> 
mean<- 5
SD <- 2
nofsamples <- 20
error <- qnorm(0.975)*SD/sqrt(nofsamples)
LCI <- mean-error
UCL <- mean+error

#inputs 
#vaccine prive = triangular distribution distribtion with +/1 25%- same both for districts
NVI_vaccine_price<-12/Birr_to_USD #12 birr same place ''National veterinary institute''- Dr Hundera Sori
# Triangular distribution- 25% +/- around the mean
#same for both
#Pva_vaccine<-rtriangle(esample, 0.75*NVI_vaccine_price/Birr_to_USD, 1.25*NVI_vaccine_price/Birr_to_USD, NVI_vaccine_price/Birr_to_USD)
##
NVI_vaccine_price_bc<-12/Birr_to_USD
#Transportaion price # asked three transporters in the three districts, and 
#transport price depends on the number of doses, fewer doses are expensive while larger doses are relatively cheaper.
#this cost if from the manufacturer to the districts
#district to vaccination site is not cosnidered assuming the vaccinators will carry with them

#truncated normal distribution around the mean
#for lemu
Tva_transportationlist_LB<-c(1,2,3)  #price of transporation in lemuna bilbilo
Tva_transportation_LB<-rtnorm(esample, mean(Tva_transportationlist_LB/Birr_to_USD), sd(Tva_transportationlist_LB/Birr_to_USD),lower=0)
##
Tva_transportation_bc_LB<-mean(Tva_transportationlist_LB/Birr_to_USD)


#cost of consumables
#syringe-truncated normal distribitions around the mean
#for lemu
Psn_syringes_needles_disinfectantswablist_LB<- c(12.95,14.15,6) ##+certificate and collar=+10 birr on each
Psn_syringes_needles_disinfectantswab_LB<- rtnorm(esample, mean(Psn_syringes_needles_disinfectantswablist_LB/Birr_to_USD), sd(Psn_syringes_needles_disinfectantswablist_LB/Birr_to_USD),lower=0)
#Pds_desinfectantswabs- truncated normal distribution- 
#same both for districts 
#Pds_desinfectantswabslist<- c(1.95, 2.5, 3.05)
#Pds_desinfectantswabs<- rtnorm(esample, mean(Pds_desinfectantswabslist/Birr_to_USD), sd(Pds_desinfectantswabslist/Birr_to_USD), lower=0)
##
Psn_syringes_needles_disinfectantswab_bc_LB<-mean(Psn_syringes_needles_disinfectantswablist_LB/Birr_to_USD)


#Pib_icebars-truncated normal distribitions around the mean-
#same both for districts
Pib_icebarslist<- c(15,20,25)
Pib_icebar<- rtnorm(esample, mean(Pib_icebarslist/Birr_to_USD), sd(Pib_icebarslist/Birr_to_USD),lower=0)
##
Pib_icebar_bc<-mean(Pib_icebarslist/Birr_to_USD)
#Ncapv_vaccination_capacity- triangular distribution with +/- 25%
#for lemu
Ncapv_vaccination_capacitylist_LB<-75   #assuming that a vaccination team in LB will vaccinate only 50% of what a team in Bishoftu vaccinate
Ncapv_vaccination_capacity_LB<-rtriangle(esample,0.75*Ncapv_vaccination_capacitylist_LB,1.25*Ncapv_vaccination_capacitylist_LB, Ncapv_vaccination_capacitylist_LB)

##
Ncapv_vaccination_capacity_bc_LB<-75
#Nvs_tempvacc_supervisor-vaccinator groups per supervisor- 1 supervisor district irrespective of number of villages-assumption
#in other words number of teams that can be supervised at a time #max is 27-all the villages
#triangular distribution
#for lemu
Number_of_villages_LB<-27

##
Number_of_villages_bc_LB<-27

#Stv_salary_temp_vaccinator- salary for temporary vaccinators- 
#same for both districts
#truncated normal distibution 
Stv_salary_temp_vaccinatorlist<-c(95,150,200)
Stv_salary_temp_vaccinator<- rtnorm(esample, mean(Stv_salary_temp_vaccinatorlist/Birr_to_USD), sd(Stv_salary_temp_vaccinatorlist/Birr_to_USD), lower=0)
##
Stv_salary_temp_vaccinator_bc<-mean(Stv_salary_temp_vaccinatorlist/Birr_to_USD)
#Cps_Perduim_cost_supervisor- salary for supervisors- 
#same for both districts
#truncated distribution 
Cps_Perduim_cost_supervisorlist<-c(126,176,206)
Cps_Perduim_cost_supervisor<- rtnorm(esample, mean(Cps_Perduim_cost_supervisorlist/Birr_to_USD), 
                                       sd(Cps_Perduim_cost_supervisorlist/Birr_to_USD),lower=0)
##
Cps_Perduim_cost_supervisor_bc<-mean(Cps_Perduim_cost_supervisorlist/Birr_to_USD)

#Vgroup- persons in a group if vaccinators
#for lemu
Vgroup_LB<-4
###
Vgroup_bc_LB<-4

#ftv_Transport_fuelper_team_of_temp_vaccinators- Transportation for temprary vaccinators 
#truncated normal
#for lemu
ftv_Transport_fuelper_team_of_temp_vaccinatorslist_LB<-c(150,200,250)*2
ftv_Transport_fuelper_team_of_temp_vaccinators_LB<- rtnorm(esample, mean(ftv_Transport_fuelper_team_of_temp_vaccinatorslist_LB/Birr_to_USD), 
                                                          sd(ftv_Transport_fuelper_team_of_temp_vaccinatorslist_LB/Birr_to_USD), lower =0 )
#
ftv_Transport_fuelper_team_of_temp_vaccinators_bc_LB<-mean(ftv_Transport_fuelper_team_of_temp_vaccinatorslist_LB/Birr_to_USD)
#fcm_Transport_fuelper_team_of_temp_supervisor- Transportation for supervisors 
#truncated normal 
#for lemu
Fcm_Transport_fuelper_team_of_supervisorslist_LB<-c(300,400,500)*2
Fcm_Transport_fuelper_team_of_supervisors_LB<- rtnorm(esample, mean(Fcm_Transport_fuelper_team_of_supervisorslist_LB/Birr_to_USD), 
                                                          sd(Fcm_Transport_fuelper_team_of_supervisorslist_LB/Birr_to_USD),lower=0)
##
Fcm_Transport_fuelper_team_of_supervisors_bc_LB<-mean(Fcm_Transport_fuelper_team_of_supervisorslist_LB/Birr_to_USD)


#Cost_of_campaign- uniform dist+/-25%
#triangular
#for lemu
campagncost_LB<- 27000
Trainingcost_LB<- 27000
Cost_of_campaign_total_LB<- rtriangle(esample, 0.75*(campagncost_LB + Trainingcost_LB)/Birr_to_USD,1.25*(campagncost_LB + Trainingcost_LB)/Birr_to_USD,(campagncost_LB + Trainingcost_LB)/Birr_to_USD)

##
Cost_of_campaign_total_bc_LB<-(campagncost_LB + Trainingcost_LB)/Birr_to_USD
#Ncb_cool_bags- number of cool bags needed
#triangular
# for lemu
Ncb_cool_bags_LB<-27

##
Ncb_cool_bags_bc_LB<-27
#Price_ofcollbag
#truncated normal
#same
Pcb_coolbaglist<-c(450,500,550)
Pcb_coolbag<- rtnorm(esample, mean(Pcb_coolbaglist/Birr_to_USD), sd(Pcb_coolbaglist/Birr_to_USD), lower=0)

#
Pcb_coolbag_bc<-mean(Pcb_coolbaglist/Birr_to_USD)
#for lemu
#Nmc_motorcycleslist_LB<-27
#Nmc_motorcycles_LB<- rtriangle(1000,0.75*Nmc_motorcycleslist_LB,1.25*Nmc_motorcycleslist_LB,Nmc_motorcycleslist_LB)

#same
#Pmc_motorcycleslist<- c(30000, 50000, 70000)
#Pmc_motorcycles<- rtnorm(1000, mean(Pmc_motorcycleslist/Birr_to_USD), sd(Pmc_motorcycleslist/Birr_to_USD),lower=0)

#for lemu
Nrf_refrigerators_LB<-27

##
Nrf_refrigerators_bc_LB<-27
#same
Prf_refrigeratorslist<- c(10000,15000, 20000)
Prf_refrigerators<- rtnorm(esample, mean(Prf_refrigeratorslist/Birr_to_USD), sd(Prf_refrigeratorslist/Birr_to_USD), lower=0)

##
Prf_refrigerators_bc<-mean(Prf_refrigeratorslist/Birr_to_USD)
# for lemu
Nmz_muzzles_LB<- 27

##
Nmz_muzzles_bc_LB<- 27
#same
Pmz_muzzlelist<-c(200,500, 800)
Pmz_muzzle<- rtnorm(esample, mean(Pmz_muzzlelist/Birr_to_USD), sd(Pmz_muzzlelist/Birr_to_USD), lower=0)

##
Pmz_muzzle_bc<-mean(Pmz_muzzlelist/Birr_to_USD)
#same
Lcmr_life_year_capital_goods<-5  #uniform distribution with +/-25% around the mean

##
Lcmr_life_year_capital_goods_bc<-5
#same
LBy_days_in_1_year<-365
##
LBy_days_in_1_year_bc<-365
#############################################################cost descriptions###########################################

####vaccine cost###
#Estimates vaccine aLB transport price for range of cov
##for lemu
vaccine_plus_trans_LB<-NVI_vaccine_price+Tva_transportation_LB
vaccine_plus_trans_LB <- array(rep(vaccine_plus_trans_LB, 100), dim = c(esample, 10, 10))
Cost_of_vaccine_LB<- vaccine_plus_trans_LB*vacc_dogs

##
vaccine_plus_trans_bc_LB<-NVI_vaccine_price_bc+Tva_transportation_bc_LB
vaccine_plus_trans_bc_LB <- matrix(rep(vaccine_plus_trans_bc_LB, 10), 10, 10)
Cost_of_vaccine_bc_LB<- vaccine_plus_trans_bc_LB*vacc_dogs[bc,,]

#########################################
#Estimates consumables price for range of cov
#for lemu
consumables_LB<-Psn_syringes_needles_disinfectantswab_LB + (Pib_icebar/Ncapv_vaccination_capacity_LB)
consumables_LB<-array(rep(consumables_LB,100),dim=c(esample, 10, 10))
Cost_of_consumables_LB<-consumables_LB*vacc_dogs

##
consumables_bc_LB<-Psn_syringes_needles_disinfectantswab_bc_LB + (Pib_icebar_bc/Ncapv_vaccination_capacity_bc_LB)
consumables_bc_LB<-matrix(rep(consumables_bc_LB,10),10, 10)
Cost_of_consumables_bc_LB<-consumables_bc_LB*vacc_dogs[bc,,]

##############################################
#number of vaccination days required assuming only one team in a village 
#for lemu
search_days <- c(0,4,3,2,1,0,1,2,3,4)
dogsvaccinatedperday_LB<- Ncapv_vaccination_capacity_LB*Number_of_villages_LB
dogsvaccinatedperday_LB<-array(rep(dogsvaccinatedperday_LB,100),dim=c(esample,10,10))
search_matrix_LB <- array(rep(search_days,100),dim=c(esample,10,10))
NVdays_LB<-ceiling(vacc_dogs/dogsvaccinatedperday_LB)+search_matrix_LB

##
dogsvaccinatedperday_bc_LB<- Ncapv_vaccination_capacity_bc_LB*Number_of_villages_bc_LB
search_matrix_bc_LB <- matrix(rep(search_days,10),10,10)
dogsvaccinatedperday_bc_LB<-matrix(rep(dogsvaccinatedperday_bc_LB,10),10,10)
NVdays_bc_LB<-ceiling(vacc_dogs[bc,,]/dogsvaccinatedperday_bc_LB) + search_matrix_bc_LB



#############################################
#cost of temporary vaccinators 
#for lemu
Trans_temp_cor_factor<- c(0, 4, 4, 4, 1, 1, 1, 1, 1, 1)
  
cost_of_temporary_vaccinatorsperday_LB<-(Stv_salary_temp_vaccinator*Vgroup_LB)*Number_of_villages_LB
cost_of_temporary_vaccinatorsperday_LB<-array(rep(cost_of_temporary_vaccinatorsperday_LB,100),dim=c(esample,10,10))

cost_of_fuel_cor_LB <- ftv_Transport_fuelper_team_of_temp_vaccinators_LB %o% Trans_temp_cor_factor

Trans_temp_cor_factor_matrix_LB<-array(rep(cost_of_fuel_cor_LB,100),dim=c(esample,10,10))
cost_of_temporary_vaccinatorsperday_cor_LB<-cost_of_temporary_vaccinatorsperday_LB+Trans_temp_cor_factor_matrix_LB

##
cost_of_temporary_vaccinatorsperday_bc_LB<-(Stv_salary_temp_vaccinator_bc*Vgroup_bc_LB)*Number_of_villages_bc_LB
Trans_temp_cor_factor_matrix_bc_LB<-matrix(rep(Trans_temp_cor_factor,10),10,10)

cost_of_fuel_cor_bc_LB <- ftv_Transport_fuelper_team_of_temp_vaccinators_bc_LB %o% Trans_temp_cor_factor

Trans_temp_cor_factor_matrix_bc_LB<-matrix(rep(cost_of_fuel_cor_bc_LB,10),10,10)
cost_of_temporary_vaccinatorsperday_cor_bc_LB<-cost_of_temporary_vaccinatorsperday_bc_LB + Trans_temp_cor_factor_matrix_bc_LB
#############################################

#27 village vaccination sites
#for lemu
cost_of_temporaryvaccinators_LB<-NVdays_LB*cost_of_temporary_vaccinatorsperday_cor_LB

##
cost_of_temporaryvaccinators_bc_LB<-NVdays_bc_LB*cost_of_temporary_vaccinatorsperday_cor_bc_LB
################################################
#cost of supervisor 
#for lemu
Trans_sup_cor_factor<-c(0, 4, 4, 4, 1, 1, 1, 1, 1, 1)

Cost_of_supervisorperday_LB<-Cps_Perduim_cost_supervisor   
Cost_of_supervisorperday_LB<-array(rep(Cost_of_supervisorperday_LB,100),dim=c(esample,10,10))

cost_of_fuel_sup_cor_LB <- Fcm_Transport_fuelper_team_of_supervisors_LB %o% Trans_sup_cor_factor

Trans_sup_cor_factor_matrix_LB<-array(rep(cost_of_fuel_sup_cor_LB,100),dim=c(esample,10,10))
Cost_of_supervisors_cor_LB<- (NVdays_LB*Cost_of_supervisorperday_LB)*1.50 + Trans_sup_cor_factor_matrix_LB# multiplied by 50% to account for uncertainity on size of villages and number of days it takes

##
Cost_of_supervisorperday_bc_LB<-Cps_Perduim_cost_supervisor_bc
Cost_of_supervisorperday_bc_LB<-matrix(rep(Cost_of_supervisorperday_bc_LB,10),10,10)

cost_of_fuel_sup_cor_bc_LB <- Fcm_Transport_fuelper_team_of_supervisors_bc_LB %o% Trans_sup_cor_factor

Trans_sup_cor_factor_matrix_bc_LB<-matrix(rep(cost_of_fuel_sup_cor_bc_LB,10),10,10)
Cost_of_supervisors_cor_bc_LB<- (NVdays_bc_LB*Cost_of_supervisorperday_bc_LB)*1.50 + Trans_sup_cor_factor_matrix_bc_LB# multiplied by 50% to account for uncertainity on size of villages and number of days it takes

###############################################
##cost of vaccinators
#for lemu
Cost_of_vaccinators_LB<-cost_of_temporaryvaccinators_LB+Cost_of_supervisors_cor_LB

##
Cost_of_vaccinators_bc_LB<-cost_of_temporaryvaccinators_bc_LB+Cost_of_supervisors_cor_bc_LB
###############################################

##cost of campaign 
#for lemu
Cost_of_campaign_total_LB<-array(rep(Cost_of_campaign_total_LB,100),dim = c(esample,10,10))
Cost_of_campaign_totaladj_LB<-(vacc_dogs/vacc_dogs)*Cost_of_campaign_total_LB
Cost_of_campaign_totaladj_LB[is.nan(Cost_of_campaign_totaladj_LB)] <- 0


###
Cost_of_campaign_total_bc_LB<-matrix(rep(Cost_of_campaign_total_bc_LB,10),10,10)
Cost_of_campaign_totaladj_bc_LB<-(vacc_dogs[bc,,]/vacc_dogs[bc,,])*Cost_of_campaign_total_bc_LB
Cost_of_campaign_totaladj_bc_LB[is.nan(Cost_of_campaign_totaladj_bc_LB)] <- 0


################################################

##capital cost
#for lemu
capital_costperyear_LB<-(Ncb_cool_bags_LB*Pcb_coolbag+ Prf_refrigerators*Nrf_refrigerators_LB+
                 Nmz_muzzles_LB*Pmz_muzzle)/(Lcmr_life_year_capital_goods)

capital_costperyear_LB<-array(rep(capital_costperyear_LB,100),dim=c(esample,10,10))
capital_costperyearadjusted_LB<-(vacc_dogs/vacc_dogs)*capital_costperyear_LB
capital_costperyearadjusted_LB[is.nan(capital_costperyearadjusted_LB)]<-0


##
capital_costperyear_bc_LB<-(Ncb_cool_bags_bc_LB*Pcb_coolbag_bc+ Prf_refrigerators_bc*Nrf_refrigerators_bc_LB+
                         Nmz_muzzles_bc_LB*Pmz_muzzle_bc)/(Lcmr_life_year_capital_goods_bc)
capital_costperyear_bc_LB<-matrix(rep(capital_costperyear_bc_LB,10),10,10)
capital_costperyearadjusted_bc_LB<-(vacc_dogs[bc,,]/vacc_dogs[bc,,])*capital_costperyear_bc_LB
capital_costperyearadjusted_bc_LB[is.nan(capital_costperyearadjusted_bc_LB)]<-0

                                  
#################################################
#cost of campaign
#for lemu
Total_CMDV_LB <- Cost_of_vaccine_LB + Cost_of_consumables_LB + Cost_of_vaccinators_LB + capital_costperyearadjusted_LB + Cost_of_campaign_totaladj_LB

#
Total_CMDV_bc_LB <- Cost_of_vaccine_bc_LB + Cost_of_consumables_bc_LB + Cost_of_vaccinators_bc_LB+ capital_costperyearadjusted_bc_LB + Cost_of_campaign_totaladj_bc_LB


########

#CI
costsum_LB<-apply(Total_CMDV_LB, 1, rowSums)
LCI_costsum_LB<-apply(costsum_LB, MARGIN=1, quantile, probs=0.025)
UCI_costsum_LB<-apply(costsum_LB, MARGIN=1, quantile, probs=0.975)
### plot cost across coverages 

plot(cov*100, Total_CMDV_bc_LB[,1], type = "l", ylim = c(0, 100000),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Total vaccination cost")
lines(cov*100, LCI_costsum_LB/10,lty=2)
lines(cov*100, UCI_costsum_LB/10,lty=2)


##

##discounting
discounted_sum <- function(data_vec, discount) {
  tp <- 0:(length(data_vec)-1)
  sum(data_vec/((1+discount)^tp))
}

####### Discounted Sums
d_Total_CMDV_LB<-apply(Total_CMDV_LB, c(1,2), discounted_sum, discount)
d_Total_CMDV_bc_LB<-apply(Total_CMDV_bc_LB, 1, discounted_sum, discount)


#################################################
###PET costs
#for lemu
PET_list_LB<-c(44.86,30.59,20.57,49.1,87.84, 31.89,83.94,9.11,54.79,26.57,26.57,10.41,
          32.15,24.74,34.82,40.99,4.95, 17.86,36.05,4.17,4.79,41.52,34.75,6.51,
          39.80,31.63,37.62,29.54,24.74,29.54,42.71,20.04,29.54,45.35,45.79,43.83,
          45.89,2.34, 16.66,41.20,18.91,7.55, 35.99,30.79,3.39, 29.86,4.95,
          37.72,31.59,54.69,39.84,30.45,30.79,36.45,4.95,34.86,30.59,32.93,
          31.16,39.95,32.77,39.95,87.17,2.34, 2.34, 2.34, 2.34, 37.35, 83.94,
          35.79,43.75,36.09,29.12,34.27,52.59,21.86,29.67,24.74,2.34,
          39.80,4.95,39.84, 11.41,39.28,53.54,4.32, 32.35,50.63,29.54,2.34,
          32.15,29.95,2.34,49.48,15.87)


PET_listmedplus_LB<-sort(PET_list_LB)[9:95]-2.34

PET_med_only_LB<-rbeta(esample, 8, 95-8)
PET_medplus_LB<-rnorm(esample, mean(PET_listmedplus_LB), sd(PET_listmedplus_LB)/sqrt(87))

PET_mean_LB<-2.34*PET_med_only_LB+(1-PET_med_only_LB)*(2.34+PET_medplus_LB)
hist(PET_mean_LB)


PETcost_perrabieddog_LB<-bite_predict[i]*(PEP_predict[i])*PET_mean_LB 
PETcost_perrabieddog_LB<-array(rep(PETcost_perrabieddog_LB,100),dim=c(esample,10,10))
##
#bite_predict<-bite_vec[sample_numbers]

##
PET_bc_LB<-2.34*(8/95)+(1-(8/95))*(2.34+mean(PET_listmedplus_LB))
PETcost_perrabieddog_bc_LB<-bite_predict[bc]*(PEP_predict[bc])*PET_bc_LB ###replace 0.51 with "bite_predict[bc]
PETcost_perrabieddog_bc_LB<-matrix(rep(PETcost_perrabieddog_bc_LB,10),10,10)


##PET_list_LB<- c(75.0,70.8,67.8,79.1,88.3,72.8,86.5,64.9,77.9,72.3,72.3,66.5,73.1,70.5,71.0,73.8,65.1,69.9,73.9,63.1,
              #  63.2,74.0,72.0,63.9,73.5,71.1,78.5,70.5,70.5,70.5,74.37,68.8,70.5,73.8,74.3,73.4,75.3,62.5,68.2,78.4,79.0,64.0,72.39,70.86,
               # 62.8,70.5,63.43,72.9,71.63,81.93,75.8,70.7,70.8,72.5,63.26,72.06,70.8,71.49,70.97,73.56,71.44,73.56,87.44,62.5,62.5,62.5,62.5,
              #  78.31,86.5,72.3,74.67,72.42,72.06,73.90,80.44,68.24,72.259,70.5,62.5,79.58,65.10,79.58,70.38,78.29,77.55,63.08,71.32,80.01,70.5,
              #  62.5,71.26,72.36,62.5,76.36,66.47)

#sort(PET_list_LB)

#PET_listmedplus_LB<-sort(PET_list_LB)[10:95]-62.5

#PET_med_only_LB<-rbeta(esample, 9, 95-9)
#PET_medplus_LB<-rnorm(esample, mean(PET_listmedplus_LB), sd(PET_listmedplus_LB)/sqrt(85))

#PET_mean_LB<-62.5*PET_med_only_LB+(1-PET_med_only_LB)*(62.5+PET_medplus_LB)
#hist(PET_mean_LB)





#PETcost_perrabieddog_LB<-0.51*(120/189)*(95/120)*PET_mean_LB 
#PETcost_perrabieddog_LB<-array(rep(PETcost_perrabieddog_LB,100),dim=c(esample,10,10))
##
#bite_predict<-bite_vec[sample_numbers]

##
#PET_bc_LB<-62.5*(9/95)+(1-(9/95))*(62.5+mean(PET_listmedplus_LB))
#PETcost_perrabieddog_bc_LB<-0.51*(120/189)*(95/120)*PET_bc_LB ###replace 0.51 with "bite_predict[bc]
#PETcost_perrabieddog_bc_LB<-matrix(rep(PETcost_perrabieddog_bc_LB,10),10,10)

#0.51-number of humans bitter per rabid dog
#120/189- victims bitten by rabied dogs visiting health center
#95/120- victims recieving PEP out of health center visits

PET_cost_total_LB<-rabid_dogs*PETcost_perrabieddog_LB
#
PET_cost_total_bc_LB<-rabid_dogs[bc,,]*PETcost_perrabieddog_bc_LB

##
###plots
PETcostsum_LB<-apply(PET_cost_total_LB, 1, rowSums)
LCI_PETcostsum_LB<-apply(PETcostsum_LB, MARGIN=1, quantile, probs=0.025)
UCI_PETcostsum_LB<-apply(PETcostsum_LB, MARGIN=1, quantile, probs=0.975)
rowmeanPET_cost_total_bc_LB<-rowMeans(PET_cost_total_bc_LB)
### plot cost across coverages 
plot(cov*100, rowmeanPET_cost_total_bc_LB, type = "l", ylim = c(0, 3500),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Cost of PET")
lines(cov*100, LCI_PETcostsum_LB/10,lty=2)
lines(cov*100, UCI_PETcostsum_LB/10,lty=2)


##discounting
d_PET_cost_total_LB<-apply(PET_cost_total_LB, c(1,2), discounted_sum, discount)
d_PET_cost_total_bc_LB<-apply(PET_cost_total_bc_LB, 1, discounted_sum, discount)


##total cost of dog vaccination plus human PET
Total_CMDV_plus_PET_cost_total_LB<-Total_CMDV_LB+PET_cost_total_LB
Total_CMDV_plus_PET_cost_total_bc_LB<-Total_CMDV_bc_LB+PET_cost_total_bc_LB

###plots
sumTotal_CMDV_plus_PET_cost_total_LB<-apply(Total_CMDV_plus_PET_cost_total_LB, 1, rowSums)
LCI_sumTotal_CMDV_plus_PET_cost_total_LB<-apply(sumTotal_CMDV_plus_PET_cost_total_LB, MARGIN=1, quantile, probs=0.025)
UCI_sumTotal_CMDV_plus_PET_cost_total_LB<-apply(sumTotal_CMDV_plus_PET_cost_total_LB, MARGIN=1, quantile, probs=0.975)
rowmeanTotal_CMDV_plus_PET_cost_total_bc_LB<-rowMeans(Total_CMDV_plus_PET_cost_total_bc_LB)
### plot cost across coverages 
plot(cov*100, rowmeanTotal_CMDV_plus_PET_cost_total_bc_LB, type = "l", ylim = c(0, 80000),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Total cost of DMV and PET")
lines(cov*100, LCI_sumTotal_CMDV_plus_PET_cost_total_LB/10,lty=2)
lines(cov*100, UCI_sumTotal_CMDV_plus_PET_cost_total_LB/10,lty=2)


#discounting  
d_Total_CMDV_plus_PET_cost_total_LB<-apply(Total_CMDV_plus_PET_cost_total_LB, c(1,2), discounted_sum, discount)
d_Total_CMDV_plus_PET_cost_total_bc_LB<-apply(Total_CMDV_plus_PET_cost_total_bc_LB, 1, discounted_sum, discount)


#DALYlost
draws<-1000
ave_DALYlostperrabiedcase_LB<-c(64.7,66.7,64.1,59.85,55.35,50.95,46.65,42.4,38.25,34.2,
                                30.05,25.95,21.95,18.05,14.45,11.15,8.3,5.8,4.15)
death_sample<-c(0,0,0,0.25,0.125,0.25,0,0.375,0,0,0,0,0,0,0,0,0,0,0)
multinomial_deathsample <- rmultinom(draws,8,death_sample)/8 # sampling from mulinomial
prob_of_DALY_lostpercase <- t(multinomial_deathsample)%*%ave_DALYlostperrabiedcase_LB
ave_DALY_lostpercase <- prob_of_DALY_lostpercase[,]


DALYlost_perrabieddog_list<-bite_predict[i]*(1-(PEP_predict[i]))*dev_rabies_predict[i]*ave_DALY_lostpercase[i]
DALYlost_perrabieddog_LB<-array(rep(DALYlost_perrabieddog_list,100),dim=c(esample,10,10))
Total_DALY_lost_LB<-rabid_dogs*DALYlost_perrabieddog_LB

#bc
DALYlost_perrabieddog_bc_LB<-bite_predict[bc]*(1-(PEP_predict[bc]))*dev_rabies_predict[bc]*ave_DALY_lostpercase[bc]
DALYlost_perrabieddog_bc_LB<-matrix(rep(DALYlost_perrabieddog_bc_LB,10),10,10)
Total_DALY_lost_bc_LB<-rabid_dogs[bc,,]*DALYlost_perrabieddog_bc_LB



##plot
sumTotal_DALY_lost_LB<-apply(Total_DALY_lost_LB, 1, rowSums)
LCI_sumTotal_DALY_lost_LB<-apply(sumTotal_DALY_lost_LB, MARGIN=1, quantile, probs=0.025)
UCI_sumTotal_DALY_lost_LB<-apply(sumTotal_DALY_lost_LB, MARGIN=1, quantile, probs=0.975)
rowmeanTotal_DALY_lost_bc_LB<-rowMeans(Total_DALY_lost_bc_LB)
### plot cost across coverages 
plot(cov*100, rowmeanTotal_DALY_lost_bc_LB, type = "l", ylim = c(0, 300),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Total DALY lost")
lines(cov*100, LCI_sumTotal_DALY_lost_LB/10,lty=2)
lines(cov*100, UCI_sumTotal_DALY_lost_LB/10,lty=2)


# Discounted Sums
d_Total_DALY_lost_LB<-apply(Total_DALY_lost_LB, c(1,2), discounted_sum, discount)
d_Total_DALY_lost_bc_LB<-apply(Total_DALY_lost_bc_LB, 1, discounted_sum, discount)


# ICER
Lemu_df <- data.frame("Coverage" = cov, "DALY" = d_Total_DALY_lost_bc_LB, "Cost" = d_Total_CMDV_plus_PET_cost_total_bc_LB,
                             "ICER" = NA)
for (cv in 2:length(cov)) {
  Lemu_df$ICER[cv] <- (Lemu_df$Cost[cv] - Lemu_df$Cost[cv - 1])/(Lemu_df$DALY[cv-1] - Lemu_df$DALY[cv]) 
}


ICER <- function (cost_vec, DALY_vec) {
  ICER_out <- rep(NA, length(cost_vec))
  ICER_out[2:length(ICER_out)] <- diff(cost_vec)/-diff(DALY_vec)
  ICER_out
}

# use äpply to create intermediate matrix, with ICER vector for each trial
# then apply "quantile" across each coverage
ICERs_LB <- matrix(NA, nrow = esample, ncol = 10)
for (tr in 1:esample) {
  ICERs_LB[tr,] <- ICER(d_Total_CMDV_plus_PET_cost_total_LB[tr,], d_Total_DALY_lost_LB[tr,])
}

Lemu_df$LCI <- apply(ICERs_LB, 2, quantile, probs = 0.025, na.rm = TRUE)
Lemu_df$UCI <- apply(ICERs_LB, 2, quantile, probs = 0.975, na.rm = TRUE)


# Calculate Net Benefits
WTP <- c(1, seq(5, 5000, by = 5))

NetBenefits <- function(cost_mat, DALY_mat, WTP_vec, trials) {
  NB <- matrix(NA, trials, length(WTP_vec))
  for (wtp in 1:length(WTP_vec)) {
    nb <- (DALY_mat[,1] - DALY_mat) - cost_mat/WTP_vec[wtp]
    NB[,wtp] <- apply(nb, 1, which.max)
  }
  NB.probs <- apply(NB, 2, tabulate, nbins = length(cost_mat[1,]))/trials
}

nb_Lemu <- NetBenefits(d_Total_CMDV_plus_PET_cost_total_LB, d_Total_DALY_lost_LB, WTP, esample)

### CEAC
plot(WTP, nb_Lemu[1,], type = "n", col = 1, bty = "l", 
     ylim = c(0, 1),xlim=c(0,5000),
     xlab = "Cost-effectiveness threshold (USD per DALY)", 
     ylab = "Probability that strategy is optimal (Lemuna-bilbilo)")
for(pl in 2:nrow(nb_Lemu)) {
  lines(WTP, nb_Lemu[pl,], col = pl)
}
legend("topright", col = 1:10,
       legend = c("0% (Status quo)", "10%", "20%","30%","40%", "50%", "60%", "70%", "80%", "90%"),
                       lty = 1, bty = "n", title = "Coverage", cex = 0.75)

abline(v=3*GDP,lty=2)
abline(v=GDP,lty=3)




######livestock cost
calc_prop<-function(vec){
  vec/vec[1]
}

rabid_prop_LB<-apply(rabid_dogs,c(1,3), "calc_prop")
rabid_prop_bc_LB<-rabid_prop_LB[,bc,]


#input Jibat et al., 2016)
cattlepop<-240704
cattleperherd<-14.5
number_of_herds<-cattlepop/cattleperherd
herd_level_incidence<-1 #base case = 0.194
Loss_per_affected_herd<-147  #USD
annual_loss_lvsrabies<-herd_level_incidence*number_of_herds*Loss_per_affected_herd

annual_loss_lvsrabies_bc_LB<-rabid_prop_bc_LB*annual_loss_lvsrabies

annual_loss_lvsrabies_LB<-rabid_prop_LB*annual_loss_lvsrabies
sum_loss_lvsrabies_LB<-apply(annual_loss_lvsrabies_LB, 1, rowSums)

LCI_sum_loss_lvsrabies_LB<-apply(sum_loss_lvsrabies_LB, MARGIN=2, quantile, probs=0.025)
UCI_sum_loss_lvsrabies_LB<-apply(sum_loss_lvsrabies_LB, MARGIN=2, quantile, probs=0.975)
sum_loss_lvsrabies_bc_LB<-rowSums(annual_loss_lvsrabies_bc_LB)

### plot cost across coverages 
plot(cov*100, sum_loss_lvsrabies_bc_LB, type = "l", ylim = c(0, 5000000),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Annual livestock related loss")
lines(cov*100, LCI_sum_loss_lvsrabies_LB,lty=2)
lines(cov*100, UCI_sum_loss_lvsrabies_LB,lty=2)


##discountin the livestock
d_sum_loss_lvsrabies_LB<-apply(annual_loss_lvsrabies_LB, c(2,1), discounted_sum, discount)
d_sum_loss_lvsrabies_bc_LB<-apply(annual_loss_lvsrabies_bc_LB, 1, discounted_sum, discount)


# ICER
Lemu_df <- data.frame("Coverage" = cov, "DALY" = d_Total_DALY_lost_bc_LB, "Cost" = d_Total_CMDV_plus_PET_cost_total_bc_LB+d_sum_loss_lvsrabies_bc_LB,
                      "ICER" = NA)
for (cv in 2:length(cov)) {
  Lemu_df$ICER[cv] <- (Lemu_df$Cost[cv] - Lemu_df$Cost[cv - 1])/(Lemu_df$DALY[cv-1] - Lemu_df$DALY[cv]) 
}


ICER <- function (cost_vec, DALY_vec) {
  ICER_out <- rep(NA, length(cost_vec))
  ICER_out[2:length(ICER_out)] <- diff(cost_vec)/-diff(DALY_vec)
  ICER_out
}

# use äpply to create intermediate matrix, with ICER vector for each trial
# then apply "quantile" across each coverage
ICERs_LB <- matrix(NA, nrow = esample, ncol = 10)
for (tr in 1:esample) {
  ICERs_LB[tr,] <- ICER((d_Total_CMDV_plus_PET_cost_total_LB+d_sum_loss_lvsrabies_LB)[tr,], d_Total_DALY_lost_LB[tr,])
}

Lemu_df$LCI <- apply(ICERs_LB, 2, quantile, probs = 0.025, na.rm = TRUE)
Lemu_df$UCI <- apply(ICERs_LB, 2, quantile, probs = 0.975, na.rm = TRUE)


# Calculate Net Benefits
WTP <- c(1, seq(5, 5000, by = 5))

NetBenefits <- function(cost_mat, DALY_mat, WTP_vec, trials) {
  NB <- matrix(NA, trials, length(WTP_vec))
  for (wtp in 1:length(WTP_vec)) {
    nb <- (DALY_mat[,1] - DALY_mat) - cost_mat/WTP_vec[wtp]
    NB[,wtp] <- apply(nb, 1, which.max)
  }
  NB.probs <- apply(NB, 2, tabulate, nbins = length(cost_mat[1,]))/trials
}

nb_Lemu <- NetBenefits(d_Total_CMDV_plus_PET_cost_total_LB+d_sum_loss_lvsrabies_LB, d_Total_DALY_lost_LB, WTP, esample)

### CEAC

plot(WTP, nb_Lemu[1,], type = "n", col = 1, bty = "l", 
     ylim = c(0, 1),xlim=c(0,5000),
     xlab = "Willingness to pay threshold (USD per DALY)", 
     ylab = "Probability that strategy is optimal (Lemuna-bilbilo)")
for(pl in 2:nrow(nb_Lemu)) {
  lines(WTP, nb_Lemu[pl,], col = pl)
}
legend("topright", col = 1:pl,
       legend = c("0% (Status quo)", "10%", "20%","30%","40%", "50%", "60%", "70%", "80%", "90%"),
       lty = 1, bty = "n", title = "Coverage", cex = 0.75)
abline(v=3*GDP,lty=2)
abline(v=GDP,lty=3)





save(list = ls(), file = paste0("Epi_and_economic_model11_mod_lemu_new", Sys.Date(), ".RData"))

##cost components plotting
d_cost_of_vaccine_bc_LB<-apply(Cost_of_vaccine_bc_LB, 1, discounted_sum, discount)
d_Cost_of_consumables_bc_LB<-apply(Cost_of_consumables_bc_LB, 1, discounted_sum, discount)
d_Cost_of_vaccinators_bc_LB<-apply(Cost_of_vaccinators_bc_LB,1,discounted_sum,discount)
d_Cost_of_campaign_totaladj_bc_LB<-apply(Cost_of_campaign_totaladj_bc_LB,1,discounted_sum,discount)
d_capital_costperyearadjusted_bc_LB<-apply(capital_costperyearadjusted_bc_LB,1,discounted_sum,discount)
plot(cov*100,d_cost_of_vaccine_bc_LB)
lines(cov*100,d_Cost_of_consumables_bc_LB, col="red")
lines(cov*100,d_Cost_of_vaccinators_bc_LB, col="blue")
lines(cov*100,d_Cost_of_campaign_totaladj_bc_LB, col="purple" )
lines(cov*100,d_capital_costperyearadjusted_bc_LB, col="darkorange")




#plot CE
plot(Lemu_df$DALY[1] - Lemu_df$DALY, Lemu_df$Cost)

rainbow(10)
coverage_colors <- rainbow(10)
plot(Lemu_df$DALY[1] - Lemu_df$DALY, Lemu_df$Cost, col = coverage_colors, pch = 18, lwd = 4, cex = 3)
plot(Lemu_df$DALY[1] - Lemu_df$DALY, Lemu_df$Cost, col = coverage_colors, pch = 18, lwd = 4, cex = 3, xlim = c(1500, 2100), ylim = c(0, 1000000))


plot(Lemu_df$DALY[1] - Lemu_df$DALY, Lemu_df$Cost, col = 1:10, pch = 20, lwd = 4, cex = 2, ylim = c(0, 22000000),
     xlab = "Life years saved", 
     ylab = "Cost of rabies_Lemuna-bilblo")
#lines(c(500, 1800), c(50000, 75000))
legend("topright", col = 1:10,
       legend = c("0% (Status quo)", "10%", "20%","30%","40%", "50%", "60%", "70%", "80%", "90%"),
       lty = 1, bty = "n", title = "Coverage", cex = 0.75)

