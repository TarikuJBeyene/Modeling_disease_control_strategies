rm(list = ls())
library(ggplot2)
library(reshape)
library(BCEA)
library(RColorBrewer)
library(mc2d)
library(msm)
library(triangle)

load("D:/R projects/Yale_Ethiopia model_Bishoftu/Bishoftu regular/Trial_11_Bishoftu_new_2017-06-12.RData")


cov_Bish <- c(0.18, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
length(cov_Bish)
Birr_to_USD<-21.6   #Currency rate (Currency rate, january 2016; $US 1 = 21.6birr) 

esample <- length(sample_numbers)
bc_Bish <- which.max(ll_predict_Bish[sample_numbers]) #15 ### UPDATED TO MOST LIKELY TRIAL
discount <- 0.03
time_frame <- 10
GDP<-568 # World bank Ethiopian GDP per capita


mean<- 5
SD <- 2
nofsamples <- 20
error <- qnorm(0.975)*SD/sqrt(nofsamples)
LCI <- mean-error
UCL <- mean+error
##
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
#for Bishoftu
Tva_transportationlist_Bish<-c(0.5,0.75,1)
Tva_transportation_Bish<-rtnorm(esample, mean(Tva_transportationlist_Bish/Birr_to_USD), sd(Tva_transportationlist_Bish/Birr_to_USD), lower=0)

##
Tva_transportation_bc_Bish<-mean(Tva_transportationlist_Bish/Birr_to_USD)


#cost of consumables
#syringe-truncated normal distribitions around the mean
#for Bish
Psn_syringes_needles_disinfectantswablist_Bish<- c(11.75,12,12.25)##+certificate and collar=+10 birr on each
Psn_syringes_needles_disinfectantswab_Bish<- rtnorm(esample, mean(Psn_syringes_needles_disinfectantswablist_Bish/Birr_to_USD), sd(Psn_syringes_needles_disinfectantswablist_Bish/Birr_to_USD), lower=0)

#Pds_desinfectantswabs- truncated normal distribution- 
#same both for districts 
#Pds_desinfectantswabslist<- c(1.95, 2.5, 3.05)
#Pds_desinfectantswabs<- rtnorm(esample, mean(Pds_desinfectantswabslist/Birr_to_USD), sd(Pds_desinfectantswabslist/Birr_to_USD), lower=0)
##
Psn_syringes_needles_disinfectantswab_bc_Bish<-mean(Psn_syringes_needles_disinfectantswablist_Bish/Birr_to_USD)



#Pib_icebars-truncated normal distribitions around the mean-
#same both for districts
Pib_icebarslist_Bish<- c(15,20,25)
Pib_icebar_Bish<- rtnorm(esample, mean(Pib_icebarslist_Bish/Birr_to_USD), sd(Pib_icebarslist_Bish/Birr_to_USD),lower=0)
##
Pib_icebar_bc_Bish<-mean(Pib_icebarslist_Bish/Birr_to_USD)



#Ncapv_vaccination_capacity- triangular distribution with +/- 25%
#for Bishoftu
Ncapv_vaccination_capacitylist_Bish<-150   #Malawi, Gibson et al., 2016 +interview
Ncapv_vaccination_capacity_Bish<-rtriangle(esample,0.75*Ncapv_vaccination_capacitylist_Bish,1.25*Ncapv_vaccination_capacitylist_Bish,Ncapv_vaccination_capacitylist_Bish )


##
Ncapv_vaccination_capacity_bc_Bish<-150


#Nvs_tempvacc_supervisor-vaccinator groups per supervisor- 1 supervisor district irrespective of number of villages-assumption
#in other words number of teams that can be supervised at a time #max is 27-all the villages
#triangular distribution
#for lemu
Number_of_villages_Bish<-9

##
Number_of_villages_bc_Bish<-9

#Stv_salary_temp_vaccinator- salary for temporary vaccinators- 
#same for both districts
#truncated normal distibution 

Stv_salary_temp_vaccinatorlist_Bish<-c(95,150,200)
Stv_salary_temp_vaccinator_Bish<- rtnorm(esample, mean(Stv_salary_temp_vaccinatorlist_Bish/Birr_to_USD), sd(Stv_salary_temp_vaccinatorlist_Bish/Birr_to_USD), lower=0)
##
Stv_salary_temp_vaccinator_bc_Bish<-mean(Stv_salary_temp_vaccinatorlist_Bish/Birr_to_USD)


#Cps_Perduim_cost_supervisor- salary for supervisors- 
#same for both districts
#truncated distribution 
Cps_Perduim_cost_supervisorlist_Bish<-c(126,176,206)
Cps_Perduim_cost_supervisor_Bish<- rtnorm(esample, mean(Cps_Perduim_cost_supervisorlist_Bish/Birr_to_USD), 
                                          sd(Cps_Perduim_cost_supervisorlist_Bish/Birr_to_USD),lower=0)
##
Cps_Perduim_cost_supervisor_bc_Bish<-mean(Cps_Perduim_cost_supervisorlist_Bish/Birr_to_USD)




#Vgroup- persons in a group if vaccinators
#for Bishoftu
Vgroup_Bish<-4
###
Vgroup_bc_Bish<-4

#ftv_Transport_fuelper_team_of_temp_vaccinators- Transportation for temprary vaccinators 
#truncated normal
#for Bishoftu
ftv_Transport_fuelper_team_of_temp_vaccinatorslist_Bish<-c(100,100,100)*2
ftv_Transport_fuelper_team_of_temp_vaccinators_Bish<- rtnorm(esample, mean(ftv_Transport_fuelper_team_of_temp_vaccinatorslist_Bish/Birr_to_USD), 
                                                             sd(ftv_Transport_fuelper_team_of_temp_vaccinatorslist_Bish/Birr_to_USD), lower =0 )
#
ftv_Transport_fuelper_team_of_temp_vaccinators_bc_Bish<-mean(ftv_Transport_fuelper_team_of_temp_vaccinatorslist_Bish/Birr_to_USD)


#fcm_Transport_fuelper_team_of_temp_supervisor- Transportation for supervisors 
#truncated normal 
#for Bishoftu
Fcm_Transport_fuelper_team_of_supervisorslist_Bish<-c(150,200,300)*2
Fcm_Transport_fuelper_team_of_supervisors_Bish<- rtnorm(esample, mean(Fcm_Transport_fuelper_team_of_supervisorslist_Bish/Birr_to_USD), 
                                                        sd(Fcm_Transport_fuelper_team_of_supervisorslist_Bish/Birr_to_USD),lower=0)
##
Fcm_Transport_fuelper_team_of_supervisors_bc_Bish<-mean(Fcm_Transport_fuelper_team_of_supervisorslist_Bish/Birr_to_USD)

error <- qnorm(0.975)*sd(Fcm_Transport_fuelper_team_of_supervisorslist_Bish/Birr_to_USD)/sqrt(esample)
LCI <- mean(Fcm_Transport_fuelper_team_of_supervisorslist_Bish/Birr_to_USD)-error
UCL <- mean(Fcm_Transport_fuelper_team_of_supervisorslist_Bish/Birr_to_USD)+error

#Cost_of_campaign- uniform dist+/-25%
#triangular
#for Bishoftu
campagncost_Bish<- 9000
Trainingcost_Bish<- 9000
Cost_of_campaign_total_Bish<- rtriangle(esample, 0.75*(campagncost_Bish + Trainingcost_Bish)/Birr_to_USD,1.25*(campagncost_Bish + Trainingcost_Bish)/Birr_to_USD,(campagncost_Bish + Trainingcost_Bish)/Birr_to_USD)

##
Cost_of_campaign_total_bc_Bish<-(campagncost_Bish + Trainingcost_Bish)/Birr_to_USD
#Ncb_cool_bags- number of cool bags needed
#triangular
# for Bishoftu
Ncb_cool_bags_Bish<-9

##
Ncb_cool_bags_bc_Bish<-9
#Price_ofcollbag
#truncated normal
#same
Pcb_coolbaglist_Bish<-c(450,500,550)
Pcb_coolbag_Bish<- rtnorm(esample, mean(Pcb_coolbaglist_Bish/Birr_to_USD), sd(Pcb_coolbaglist_Bish/Birr_to_USD), lower=0)

#
Pcb_coolbag_bc_Bish<-mean(Pcb_coolbaglist_Bish/Birr_to_USD)
#for lemu
#Nmc_motorcycleslist_LB<-27
#Nmc_motorcycles_LB<- rtriangle(1000,0.75*Nmc_motorcycleslist_LB,1.25*Nmc_motorcycleslist_LB,Nmc_motorcycleslist_LB)

#same
#Pmc_motorcycleslist<- c(30000, 50000, 70000)
#Pmc_motorcycles<- rtnorm(1000, mean(Pmc_motorcycleslist/Birr_to_USD), sd(Pmc_motorcycleslist/Birr_to_USD),lower=0)

#for lemu
Nrf_refrigerators_Bish<-9

##
Nrf_refrigerators_bc_Bish<-9
#same
Prf_refrigeratorslist_Bish<- c(10000,15000, 20000)
Prf_refrigerators_Bish<- rtnorm(esample, mean(Prf_refrigeratorslist_Bish/Birr_to_USD), sd(Prf_refrigeratorslist_Bish/Birr_to_USD), lower=0)

##
Prf_refrigerators_bc_Bish<-mean(Prf_refrigeratorslist_Bish/Birr_to_USD)
# for lemu
Nmz_muzzles_Bish<- 9

##
Nmz_muzzles_bc_Bish<- 9
#same
Pmz_muzzlelist_Bish<-c(200,500, 800)
Pmz_muzzle_Bish<- rtnorm(esample, mean(Pmz_muzzlelist_Bish/Birr_to_USD), sd(Pmz_muzzlelist_Bish/Birr_to_USD), lower=0)

##
Pmz_muzzle_bc_Bish<-mean(Pmz_muzzlelist_Bish/Birr_to_USD)
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
vaccine_plus_trans_Bish<-NVI_vaccine_price+Tva_transportation_Bish
vaccine_plus_trans_Bish <- array(rep(vaccine_plus_trans_Bish, 90), dim = c(esample, 9, 10))
Cost_of_vaccine_Bish<- vaccine_plus_trans_Bish*vacc_dogs_Bish

##
vaccine_plus_trans_bc_Bish<-NVI_vaccine_price_bc+Tva_transportation_bc_Bish
vaccine_plus_trans_bc_Bish <- matrix(rep(vaccine_plus_trans_bc_Bish, 10), 9, 10)
Cost_of_vaccine_bc_Bish<- vaccine_plus_trans_bc_Bish*vacc_dogs_Bish[bc_Bish,,]

#########################################
#Estimates consumables price for range of cov
#for lemu
consumables_Bish<-Psn_syringes_needles_disinfectantswab_Bish + (Pib_icebar_Bish/Ncapv_vaccination_capacity_Bish)
consumables_Bish<-array(rep(consumables_Bish,90),dim=c(esample, 9, 10))
Cost_of_consumables_Bish<-consumables_Bish*vacc_dogs_Bish

##
consumables_bc_Bish<-Psn_syringes_needles_disinfectantswab_bc_Bish + (Pib_icebar_bc_Bish/Ncapv_vaccination_capacity_bc_Bish)
consumables_bc_Bish<-matrix(rep(consumables_bc_Bish,10),9, 10)
Cost_of_consumables_bc_Bish<-consumables_bc_Bish*vacc_dogs_Bish[bc_Bish,,]

##############################################
#number of vaccination days required assuming only one team in a village 
#for lemu
search_days_Bish <- c(0,0,0,0,0,1,2,3,4)
dogsvaccinatedperday_Bish<- Ncapv_vaccination_capacity_Bish*Number_of_villages_Bish
dogsvaccinatedperday_Bish<-array(rep(dogsvaccinatedperday_Bish,90),dim=c(esample,9,10))
search_matrix_Bish <- array(rep(search_days_Bish,90),dim=c(esample,9,10))
NVdays_Bish<-ceiling(vacc_dogs_Bish/dogsvaccinatedperday_Bish)+search_matrix_Bish

##
dogsvaccinatedperday_bc_Bish<- Ncapv_vaccination_capacity_bc_Bish*Number_of_villages_bc_Bish
search_matrix_bc_Bish <- matrix(rep(search_days_Bish,10),9,10)
dogsvaccinatedperday_bc_Bish<-matrix(rep(dogsvaccinatedperday_bc_Bish,10),9,10)
NVdays_bc_Bish<-ceiling(vacc_dogs_Bish[bc_Bish,,]/dogsvaccinatedperday_bc_Bish) + search_matrix_bc_Bish

#############################################
#cost of temporary vaccinators 
#for lemu
Trans_temp_cor_factor_Bish<- c(1, 1, 1, 1, 1, 1, 1, 1, 1)

cost_of_temporary_vaccinatorsperday_Bish<-(Stv_salary_temp_vaccinator_Bish*Vgroup_Bish)*Number_of_villages_Bish
cost_of_temporary_vaccinatorsperday_Bish<-array(rep(cost_of_temporary_vaccinatorsperday_Bish,90),dim=c(esample,9,10))

cost_of_fuel_cor_Bish <- ftv_Transport_fuelper_team_of_temp_vaccinators_Bish %o% Trans_temp_cor_factor_Bish

Trans_temp_cor_factor_matrix_Bish<-array(rep(cost_of_fuel_cor_Bish,90),dim=c(esample,9,10))
cost_of_temporary_vaccinatorsperday_cor_Bish<-cost_of_temporary_vaccinatorsperday_Bish+Trans_temp_cor_factor_matrix_Bish

##
cost_of_temporary_vaccinatorsperday_bc_Bish<-(Stv_salary_temp_vaccinator_bc_Bish*Vgroup_bc_Bish)*Number_of_villages_bc_Bish
Trans_temp_cor_factor_matrix_bc_Bish<-matrix(rep(Trans_temp_cor_factor_Bish,10),9,10)

cost_of_fuel_cor_bc_Bish <- ftv_Transport_fuelper_team_of_temp_vaccinators_bc_Bish%o% Trans_temp_cor_factor_Bish

Trans_temp_cor_factor_matrix_bc_Bish<-matrix(rep(cost_of_fuel_cor_bc_Bish,10),9,10)
cost_of_temporary_vaccinatorsperday_cor_bc_Bish<-cost_of_temporary_vaccinatorsperday_bc_Bish + Trans_temp_cor_factor_matrix_bc_Bish
#############################################

#27 village vaccination sites
#for lemu
cost_of_temporaryvaccinators_Bish<-NVdays_Bish*cost_of_temporary_vaccinatorsperday_cor_Bish

##
cost_of_temporaryvaccinators_bc_Bish<-NVdays_bc_Bish*cost_of_temporary_vaccinatorsperday_cor_bc_Bish
################################################
#cost of supervisor 
#for lemu
Trans_sup_cor_factor_Bish<-c(1, 1, 1, 1, 1, 1, 1, 1, 1)

Cost_of_supervisorperday_Bish<-Cps_Perduim_cost_supervisor_Bish   
Cost_of_supervisorperday_Bish<-array(rep(Cost_of_supervisorperday_Bish,90),dim=c(esample,9,10))

cost_of_fuel_sup_cor_Bish <- Fcm_Transport_fuelper_team_of_supervisors_Bish %o% Trans_sup_cor_factor_Bish

Trans_sup_cor_factor_matrix_Bish<-array(rep(cost_of_fuel_sup_cor_Bish,90),dim=c(esample,9,10))
Cost_of_supervisors_cor_Bish<- (NVdays_Bish*Cost_of_supervisorperday_Bish)*1.50 + Trans_sup_cor_factor_matrix_Bish# multiplied by 50% to account for uncertainity on size of villages and number of days it takes

##
Cost_of_supervisorperday_bc_Bish<-Cps_Perduim_cost_supervisor_bc_Bish
Cost_of_supervisorperday_bc_Bish<-matrix(rep(Cost_of_supervisorperday_bc_Bish,10),9,10)

cost_of_fuel_sup_cor_bc_Bish <- Fcm_Transport_fuelper_team_of_supervisors_bc_Bish %o% Trans_sup_cor_factor_Bish

Trans_sup_cor_factor_matrix_bc_Bish<-matrix(rep(cost_of_fuel_sup_cor_bc_Bish,10),9,10)
Cost_of_supervisors_cor_bc_Bish<- (NVdays_bc_Bish*Cost_of_supervisorperday_bc_Bish)*1.50 + Trans_sup_cor_factor_matrix_bc_Bish# multiplied by 50% to account for uncertainity on size of villages and number of days it takes

###############################################
##cost of vaccinators
#for lemu
Cost_of_vaccinators_Bish<-cost_of_temporaryvaccinators_Bish+Cost_of_supervisors_cor_Bish

##
Cost_of_vaccinators_bc_Bish<-cost_of_temporaryvaccinators_bc_Bish+Cost_of_supervisors_cor_bc_Bish
###############################################

##cost of campaign 
#for lemu
Cost_of_campaign_total_Bish<-array(rep(Cost_of_campaign_total_Bish,90),dim = c(esample,9,10))
Cost_of_campaign_totaladj_Bish<-(vacc_dogs_Bish/vacc_dogs_Bish)*Cost_of_campaign_total_Bish
Cost_of_campaign_totaladj_Bish[is.nan(Cost_of_campaign_totaladj_Bish)] <- 0
Cost_of_campaign_totaladj_Bish

###
Cost_of_campaign_total_bc_Bish<-matrix(rep(Cost_of_campaign_total_bc_Bish,10),9,10)
Cost_of_campaign_totaladj_bc_Bish<-(vacc_dogs_Bish[bc_Bish,,]/vacc_dogs_Bish[bc_Bish,,])*Cost_of_campaign_total_bc_Bish
Cost_of_campaign_totaladj_bc_Bish[is.nan(Cost_of_campaign_totaladj_bc_Bish)] <- 0


################################################

##capital cost
#for lemu
capital_costperyear_Bish<-(Ncb_cool_bags_Bish*Pcb_coolbag_Bish+ Prf_refrigerators_Bish*Nrf_refrigerators_Bish+
                 Nmz_muzzles_Bish*Pmz_muzzle_Bish)/(Lcmr_life_year_capital_goods)

capital_costperyear_Bish<-array(rep(capital_costperyear_Bish,90),dim=c(esample,9,10))
capital_costperyearadjusted_Bish<-(vacc_dogs_Bish/vacc_dogs_Bish)*capital_costperyear_Bish
capital_costperyearadjusted_Bish[is.nan(capital_costperyearadjusted_Bish)]<-0


##
capital_costperyear_bc_Bish<-(Ncb_cool_bags_bc_Bish*Pcb_coolbag_bc_Bish+ Prf_refrigerators_bc_Bish*Nrf_refrigerators_bc_Bish+
                         Nmz_muzzles_bc_Bish*Pmz_muzzle_bc_Bish)/(Lcmr_life_year_capital_goods_bc)
capital_costperyear_bc_Bish<-matrix(rep(capital_costperyear_bc_Bish,10),9,10)
capital_costperyearadjusted_bc_Bish<-(vacc_dogs_Bish[bc_Bish,,]/vacc_dogs_Bish[bc_Bish,,])*capital_costperyear_bc_Bish
capital_costperyearadjusted_bc_Bish[is.nan(capital_costperyearadjusted_bc_Bish)]<-0
capital_costperyearadjusted_bc_Bish
                                  
#################################################
#cost of campaign
#for lemu
Total_CMDV_Bish <- Cost_of_vaccine_Bish + Cost_of_consumables_Bish + Cost_of_vaccinators_Bish + capital_costperyearadjusted_Bish + Cost_of_campaign_totaladj_Bish
Total_CMDV_Bish[is.nan(Total_CMDV_Bish)]<-0
#
Total_CMDV_bc_Bish <- Cost_of_vaccine_bc_Bish + Cost_of_consumables_bc_Bish + Cost_of_vaccinators_bc_Bish+ capital_costperyearadjusted_bc_Bish + Cost_of_campaign_totaladj_bc_Bish
Total_CMDV_bc_Bish[is.nan(Total_CMDV_bc_Bish)]<-0

########
#CI
costsum_Bish<-apply(Total_CMDV_Bish, 1, rowSums)
costsum_Bish<-costsum_Bish[-1,]
LCI_costsum_Bish<-apply(costsum_Bish, MARGIN=1, quantile, probs=0.025)
UCI_costsum_Bish<-apply(costsum_Bish, MARGIN=1, quantile, probs=0.975)
### plot cost across coverages 

plot(cov_Bish*100, Total_CMDV_bc_Bish[,1], type = "l", ylim = c(0, 100000),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Total vaccination cost")
lines(cov_Bish*100, LCI_costsum_Bish/10,lty=2)
lines(cov_Bish*100, UCI_costsum_Bish/10,lty=2)


##

##discounting
discounted_sum <- function(data_vec, discount) {
  tp <- 0:(length(data_vec)-1)
  sum(data_vec/((1+discount)^tp))
}

####### Discounted Sums
d_Total_CMDV_Bish<-apply(Total_CMDV_Bish, c(1,2), discounted_sum, discount)
d_Total_CMDV_bc_Bish<-apply(Total_CMDV_bc_Bish, 1, discounted_sum, discount)


#################################################
###PET costs
#for lemu
PET_list_Bish<-c(27.6,19.5,31.5,31.8,19.5,26.7,23.0,27.9,27.6,32.7,30.3,15.3,24.0,17.7,15.8,17.7,25.6,10.4,19.5,
                 25.0,16.8,27.6,13.9,17.7,16.0,27.6,15.8,15.8,10.4,37.8,23.8,29.0,10.2,15.5,15.8,28.0,25.4,
                 12.0,31.1,23.0,26.7, 12.1,11.4,27.9,9.6,15.9,22.2,26.4,23.1,15.9,3.4,14.1,13.8, 14.0,19.5, 21.9,19.5, 4.1, 19.5, 
                 19.5,19.5,27.3,27.5,  22.2,  31.0, 19.5,  17.7, 14.0, 14.2, 27.6,26.6,31.4, 17.7, 
                 26.0,10.4, 29.9, 22.1,28.9, 13.9, 29.2, 25.4,28.6, 19.7,23.0,27.6,31.1,25.7, 31.9, 15.9, 
                 19.5,15.7,27.7,23.3,27.6,27.4,18.3, 15.6,12.2,17.7, 27.1,23.8,9.3, 38.1, 10.4,28.9,27.4,32.7, 
                 18.0, 9.6,24.4, 27.4,14.0, 51.2,14.0, 24.7, 38.6, 27.0, 18.3,25.3, 27.5, 16.4, 72.7,
                 22.4,21.4, 15.4,21.8,30.1,29.9, 27.7,15.7, 12.5, 24.5,30.1,  14.1,27.7, 31.5,41.9,
                 64.9, 30.5, 38.3, 27.0, 27.6,27.6, 27.5,2.3, 22.5, 58.8)

hist(PET_list_Bish)
PET_listmedplus_Bish<-sort(PET_list_Bish)

PET_med_only_Bish<-rbeta(esample, 0, 147)
PET_medplus_Bish<-rnorm(esample, mean(PET_listmedplus_Bish), sd(PET_listmedplus_Bish)/sqrt(147))

PET_mean_Bish<-0*PET_med_only_Bish+(1-PET_med_only_Bish)*(0+PET_medplus_Bish)
hist(PET_mean_Bish)


PETcost_perrabieddog_Bish<-bite_predict_Bish[i]*(PEP_predict_Bish[i])*PET_mean_Bish 
PETcost_perrabieddog_Bish<-array(rep(PETcost_perrabieddog_Bish,90),dim=c(esample,9,10))
##
#bite_predict<-bite_vec[sample_numbers]

##
PET_bc_Bish<-0*(0/147)+(1-(0/147))*(0+mean(PET_listmedplus_Bish))
PETcost_perrabieddog_bc_Bish<-bite_predict_Bish[bc_Bish]*(PEP_predict_Bish[bc_Bish])*PET_bc_Bish ###replace 0.51 with "bite_predict[bc]
PETcost_perrabieddog_bc_Bish<-matrix(rep(PETcost_perrabieddog_bc_Bish,10),9,10)

#for cell based vaccine
#PET_list_Bish_CBV<- c(74.29,67.54,75.46,76.82,67.54,75.32, 68.58,74.60,74.29,75.87, 75.36,70.32,72.61,67.02,70.47,
                     # 67.02, 81.91, 68.61, 70.70, 73.91,71.39,74.29, 70.39, 67.02, 70.61, 75.49,  70.47, 71.58,
                     # 68.61, 77.29, 72.45, 74.72,  68.45, 70.11,70.97,   74.67, 74.02,  69.82,75.41,  69.76, 74.52,
                     # 69.59,69.65, 74.60,65.92,66.50,68.34,69.56,68.61, 66.50, 4.69,70.41,70.23,70.31, 67.54,70.54,
                     # 67.54,63.02, 67.54,67.54,  67.54,74.29, 74.26,68.34,74.93, 67.54, 67.02, 70.42,70.46, 75.49,
                    #  73.35, 75.53, 71.49, 73.85, 68.51, 74.94, 72.70, 73.95, 70.26,  74.88,  74.14, 75.33, 72.12,
                     # 69.85,73.81, 75.41, 75.14,  75.87, 66.50,  67.54, 70.92, 74.45, 73.41, 74.31, 74.23, 67.19,
                    #  70.94, 70.14,67.02,74.17,72.45,68.30, 92.77, 68.61,73.95,74.23,75.87, 67.10,65.92,73.08,
                    #  74.23,70.29, 80.61,   70.29,  69.09,77.65, 73.87,  67.19, 73.25,
                  #    75.44, 71.00,  83.19, 71.05,72.62,70.37,73.11,75.15,74.94,74.41,72.21,69.86,73.13, 74.42, 70.45,74.34,
                   #   75.46,78.52,89.67, 74.78, 74.50,   75.38, 75.91, 75.49,74.15,62.50, 70.07,79.10)

#hist(PET_list_Bish)
#PET_listmedplus_Bish<-sort(PET_list_Bish)

#PET_med_only_Bish<-rbeta(esample, 0, 147)
#PET_medplus_Bish<-rnorm(esample, mean(PET_listmedplus_Bish), sd(PET_listmedplus_Bish)/sqrt(147))

#PET_mean_Bish<-0*PET_med_only_Bish+(1-PET_med_only_Bish)*(0+PET_medplus_Bish)
#hist(PET_mean_Bish)


#PETcost_perrabieddog_Bish<-bite_predict[i]*(PEP_predict[i])*PET_mean_Bish 
#PETcost_perrabieddog_Bish<-array(rep(PETcost_perrabieddog_Bish,100),dim=c(esample,10,10))
##
#bite_predict<-bite_vec[sample_numbers]

##
#PET_bc_Bish<-0*(0/147)+(1-(0/147))*(0+mean(PET_listmedplus_Bish))
#PETcost_perrabieddog_bc_Bish<-bite_predict[bc]*(PEP_predict[bc])*PET_bc_Bish ###replace 0.51 with "bite_predict[bc]
#PETcost_perrabieddog_bc_Bish<-matrix(rep(PETcost_perrabieddog_bc_Bish,10),10,10)


#0.51-number of humans bitter per rabid dog
#120/189- victims bitten by rabied dogs visiting health center
#95/120- victims recieving PEP out of health center visits

PET_cost_total_Bish<-rabid_dogs_Bish*PETcost_perrabieddog_Bish
#
PET_cost_total_bc_Bish<-rabid_dogs_Bish[bc_Bish,,]*PETcost_perrabieddog_bc_Bish


##
###plots
PETcostsum_Bish<-apply(PET_cost_total_Bish, 1, rowSums)
LCI_PETcostsum_Bish<-apply(PETcostsum_Bish, MARGIN=1, quantile, probs=0.025)
UCI_PETcostsum_Bish<-apply(PETcostsum_Bish, MARGIN=1, quantile, probs=0.975)
rowmeanPET_cost_total_bc_Bish<-rowMeans(PET_cost_total_bc_Bish)
### plot cost across coverages 
plot(cov_Bish*100, rowmeanPET_cost_total_bc_Bish, type = "l", ylim = c(0, 6000),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Cost of PET")
lines(cov_Bish*100, LCI_PETcostsum_Bish/10,lty=2)
lines(cov_Bish*100, UCI_PETcostsum_Bish/10,lty=2)


##discounting
d_PET_cost_total_Bish<-apply(PET_cost_total_Bish, c(1,2), discounted_sum, discount)
d_PET_cost_total_bc_Bish<-apply(PET_cost_total_bc_Bish, 1, discounted_sum, discount)


##total cost of dog vaccination plus human PET
Total_CMDV_plus_PET_cost_total_Bish<-Total_CMDV_Bish+PET_cost_total_Bish
Total_CMDV_plus_PET_cost_total_bc_Bish<-Total_CMDV_bc_Bish+PET_cost_total_bc_Bish

###plots
sumTotal_CMDV_plus_PET_cost_total_Bish<-apply(Total_CMDV_plus_PET_cost_total_Bish, 1, rowSums)
sumTotal_CMDV_plus_PET_cost_total_Bish<-sumTotal_CMDV_plus_PET_cost_total_Bish[-1,]


LCI_sumTotal_CMDV_plus_PET_cost_total_Bish<-apply(sumTotal_CMDV_plus_PET_cost_total_Bish, MARGIN=1, quantile, probs=0.025)
UCI_sumTotal_CMDV_plus_PET_cost_total_Bish<-apply(sumTotal_CMDV_plus_PET_cost_total_Bish, MARGIN=1, quantile, probs=0.975)
rowmeanTotal_CMDV_plus_PET_cost_total_bc_Bish<-rowMeans(Total_CMDV_plus_PET_cost_total_bc_Bish)
### plot cost across coverages 
plot(cov_Bish*100, rowmeanTotal_CMDV_plus_PET_cost_total_bc_Bish, type = "l", ylim = c(0, 80000),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Total cost of DMV and PET")
lines(cov_Bish*100, LCI_sumTotal_CMDV_plus_PET_cost_total_Bish/10,lty=2)
lines(cov_Bish*100, UCI_sumTotal_CMDV_plus_PET_cost_total_Bish/10,lty=2)


#discounting  
d_Total_CMDV_plus_PET_cost_total_Bish<-apply(Total_CMDV_plus_PET_cost_total_Bish, c(1,2), discounted_sum, discount)
d_Total_CMDV_plus_PET_cost_total_bc_Bish<-apply(Total_CMDV_plus_PET_cost_total_bc_Bish, 1, discounted_sum, discount)


#DALYlost
draws<-10000
ave_DALYlostperrabiedcase_Bish<-c(64.7,66.7,64.1,59.85,55.35,50.95,46.65,42.4,38.25,34.2,
                                30.05,25.95,21.95,18.05,14.45,11.15,8.3,5.8,4.15)
death_sample_Bish<-c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0, 0)
multinomial_deathsample_Bish <- rmultinom(draws,1,death_sample_Bish)/1 # sampling from mulinomial
prob_of_DALY_lostpercase_Bish <- t(multinomial_deathsample_Bish)%*%ave_DALYlostperrabiedcase_Bish
ave_DALY_lostpercase_Bish <- prob_of_DALY_lostpercase_Bish[,]

ave_DALY_lostpercase_Bish[bc_Bish]

DALYlost_perrabieddog_list_Bish<-bite_predict_Bish[i]*(1-(PEP_predict_Bish[i]))*dev_rabies_predict_Bish[i]*ave_DALY_lostpercase_Bish[i]
DALYlost_perrabieddog_Bish<-array(rep(DALYlost_perrabieddog_list_Bish,90),dim=c(esample,9,10))
Total_DALY_lost_Bish<-rabid_dogs_Bish*DALYlost_perrabieddog_Bish

#bc
DALYlost_perrabieddog_bc_Bish<-bite_predict_Bish[bc_Bish]*(1-(PEP_predict_Bish[bc_Bish]))*dev_rabies_predict_Bish[bc_Bish]*ave_DALY_lostpercase_Bish[bc_Bish]
DALYlost_perrabieddog_bc_Bish<-matrix(rep(DALYlost_perrabieddog_bc_Bish,10),9,10)
Total_DALY_lost_bc_Bish<-rabid_dogs_Bish[bc_Bish,,]*DALYlost_perrabieddog_bc_Bish



##plot
sumTotal_DALY_lost_Bish<-apply(Total_DALY_lost_Bish, 1, rowSums)
LCI_sumTotal_DALY_lost_Bish<-apply(sumTotal_DALY_lost_Bish, MARGIN=1, quantile, probs=0.025)
UCI_sumTotal_DALY_lost_Bish<-apply(sumTotal_DALY_lost_Bish, MARGIN=1, quantile, probs=0.975)
rowmeanTotal_DALY_lost_bc_Bish<-rowMeans(Total_DALY_lost_bc_Bish)
### plot cost across coverages 
plot(cov_Bish*100, rowmeanTotal_DALY_lost_bc_Bish, type = "l", ylim = c(0, 500),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Total DALY lost")
lines(cov_Bish*100, LCI_sumTotal_DALY_lost_Bish/10,lty=2)
lines(cov_Bish*100, UCI_sumTotal_DALY_lost_Bish/10,lty=2)


# Discounted Sums
d_Total_DALY_lost_Bish<-apply(Total_DALY_lost_Bish, c(1,2), discounted_sum, discount)
d_Total_DALY_lost_bc_Bish<-apply(Total_DALY_lost_bc_Bish, 1, discounted_sum, discount)


# ICER
Bish_df <- data.frame("Coverage" = cov_Bish, "DALY" = d_Total_DALY_lost_bc_Bish, "Cost" = d_Total_CMDV_plus_PET_cost_total_bc_Bish,
                             "ICER" = NA)
for (cv in 2:length(cov_Bish)) {
  Bish_df$ICER[cv] <- (Bish_df$Cost[cv] - Bish_df$Cost[cv - 1])/(Bish_df$DALY[cv-1] - Bish_df$DALY[cv]) 
}


ICER <- function (cost_vec, DALY_vec) {
  ICER_out <- rep(NA, length(cost_vec))
  ICER_out[2:length(ICER_out)] <- diff(cost_vec)/-diff(DALY_vec)
  ICER_out
}

# use äpply to create intermediate matrix, with ICER vector for each trial
# then apply "quantile" across each coverage
ICERs_Bish <- matrix(NA, nrow = esample, ncol = 9)
for (tr in 1:esample) {
  ICERs_Bish[tr,] <- ICER(d_Total_CMDV_plus_PET_cost_total_Bish[tr,], d_Total_DALY_lost_Bish[tr,])
}

Bish_df$LCI <- apply(ICERs_Bish, 2, quantile, probs = 0.025, na.rm = TRUE)
Bish_df$UCI <- apply(ICERs_Bish, 2, quantile, probs = 0.975, na.rm = TRUE)


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

nb_Bish <- NetBenefits(d_Total_CMDV_plus_PET_cost_total_Bish, d_Total_DALY_lost_Bish, WTP, esample)
par(bty = 'l')

### CEAC
bty = 'l'
col<-rainbow(10)
plot(WTP, nb_Bish[1,], type = "n", col = col[2], lwd=2,
     ylim = c(0, 1),xlim=c(0,5000),
     xlab = "Cost-effectiveness threshold (USD per DALY)", 
     ylab = "Probability that strategy is optimal (Bishoftu)")
for(pl in 2:nrow(nb_Bish)) {
  lines(WTP, nb_Bish[pl,], col = col[pl+1],lwd=2)
}
legend("topright", col = col[2:10],
       legend = c("18% (Status quo)", "20%","30%","40%", "50%", "60%", "70%", "80%", "90%"),
                       lwd=2,lty = 1, bty = "n", title = "Coverage", cex = 0.75)

abline(v=3*GDP,lty=2)
abline(v=GDP,lty=3)


rabid_dogs_Bish[bc_Bish,,]

save(list = ls(), file = paste0("Epi_and_economic_model11_mod_Bish_", Sys.Date(), ".RData"))

##cost components plotting
d_cost_of_vaccine_bc_Bish<-apply(Cost_of_vaccine_bc_Bish, 1, discounted_sum, discount)
d_Cost_of_consumables_bc_Bish<-apply(Cost_of_consumables_bc_Bish, 1, discounted_sum, discount)
d_Cost_of_vaccinators_bc_Bish<-apply(Cost_of_vaccinators_bc_Bish,1,discounted_sum,discount)
d_Cost_of_campaign_totaladj_bc_Bish<-apply(Cost_of_campaign_totaladj_bc_Bish,1,discounted_sum,discount)
d_capital_costperyearadjusted_bc_Bish<-apply(capital_costperyearadjusted_bc_Bish,1,discounted_sum,discount)
plot(cov_Bish*100,d_cost_of_vaccine_bc_Bish)
lines(cov_Bish*100,d_Cost_of_consumables_bc_Bish, col="red")
lines(cov_Bish*100,d_Cost_of_vaccinators_bc_Bish, col="blue")
lines(cov_Bish*100,d_Cost_of_campaign_totaladj_bc_Bish, col="purple" )
lines(cov_Bish*100,d_capital_costperyearadjusted_bc_Bish, col="darkorange")




#plot CE
col<-rainbow(10)

plot(Bish_df$DALY[1] - Bish_df$DALY, Bish_df$Cost)

plot(Bish_df$DALY[1] - Bish_df$DALY, Bish_df$Cost, col = col[2:10], pch = 18, lwd = 4, cex = 3)
plot(Bish_df$DALY[1] - Bish_df$DALY, Bish_df$Cost, col = col[2:10], pch = 18, lwd = 4, cex = 3, ylim = c(0, 200000))


plot(Bish_df$DALY[1] - Bish_df$DALY, Bish_df$Cost, col = col[2:10], pch = 20, lwd = 4, cex = 2, xlim=c(0,2000), ylim = c(0, 200000),
     xlab = "Life years saved", 
     ylab = "Cost of rabies (Bishoftu)")
#lines(c(500, 1800), c(50000, 75000))
legend("topleft", col = col[2:10],
       legend = c("18% (Status quo)", "20%","30%","40%", "50%", "60%", "70%", "80%", "90%"),
       lty = 1, bty = "n", title = "Coverage", pch=19, cex =0.9)



