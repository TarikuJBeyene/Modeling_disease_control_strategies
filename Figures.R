# Figures
rm(list = ls())
library(RColorBrewer)

load("D:/R projects/Yale_Ethiopia model_lemu/lemu final/combined/Epi_and_economic_model11_mod_Bish_2017-05-31.RData")

load("D:/R projects/Yale_Ethiopia model_lemu/lemu final/combined/Epi_and_economic_model11_mod_lemu2017-03-10.RData")



##cumulative number of exposed humans over 10 years
rowsumofexposedhumans_Bish<-rowSums(human_output_Bish[bc_Bish,,])
rowsumofexposedhumans_LB<-rowSums(human_output[bc,,])
plot(rowsumofexposedhumans_Bish, type="b",pch=16, xaxt="n",col=1,
     ylim = c(0, 2000), 
     xlim= c(0.18, length(cov_Bish)),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Cumulative number of exposed persons over 10 years (Bishoftu)")
axis(1, at=1:9, labels=c( "18", "20", "30", "40", "50","60","70","80","90")) 



plot(rowsumofexposedhumans_LB, type="b",pch=16, xaxt="n",col=1,
     ylim = c(0, 2000), 
     xlim= c(0.0, length(cov)),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Cumulative number of exposed persons over 10 years (Lemuna-bilbilo)")
axis(1, at=1:10, labels=c( "0","10", "20", "30", "40", "50","60","70","80","90")) 






###DALY
rowsumofDALylost<-rowSums(Total_DALY_lost_Bish[bc_Bish,,])
plot(rowsumofDALylost, type="b",pch=16, xaxt="n",col="black",
     ylim = c(0, 3500), 
     xlim= c(0.18, length(cov_Bish)),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Cumulative life years lost over 10 years (Bishoftu)")
axis(1, at=1:9, labels=c( "18", "20", "30", "40", "50","60","70","80","90")) 


rowsumofDALylost<-rowSums(Total_DALY_lost_LB[bc,,])
plot(rowsumofDALylost, type="b",pch=16, xaxt="n",col="black",
     ylim = c(0, 6000), 
     xlim= c(0, length(cov)),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Cumulative life years lost over 10 years (Lemuna-bilbilo)")
axis(1, at=1:10, labels=c( "0", "10", "20", "30", "40", "50","60","70","80","90")) 



##rabied dogs 

rowsumofbitingrabieddog_Bish<-rowSums(rabid_dogs_Bish[bc_Bish,,])
rowsumofbitingrabieddog_Bish
plot(rowsumofbitingrabieddog_Bish, type="b",pch=16, xaxt="n",
     ylim = c(0, 4500), 
     xlim= c(0.18, length(cov_Bish)),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Cumulative number of Rabid dogs over 10 years (Bishoftu)")
axis(1, at=1:9, labels=c( "18", "20", "30", "40", "50","60","70","80","90")) 



rowsumofbitingrabieddog<-rowSums(rabid_dogs[bc,,])
rowsumofbitingrabieddog
plot(rowsumofbitingrabieddog, type="b",pch=16, xaxt="n",
     ylim = c(0, 3500), 
     xlim= c(0, length(cov)),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Cumulative number of Rabid dogs over 10 years (Lemuna-bilbilo)")
axis(1, at=1:10, labels=c( "0", "10", "20", "30", "40", "50","60","70","80","90")) 


##rabied dog per km2

rowsumofbitingrabieddog_Bishperkm<-rowSums(rabid_dogs_Bish[bc_Bish,,]/40)
rowsumofbitingrabieddog_Bishperkm
plot(rowsumofbitingrabieddog_Bishperkm, type="b",pch=16, xaxt="n",
     ylim = c(0, 120), 
     xlim= c(0, length(cov_Bish)),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Cumulative number of Rabid dogs over 10 years per km2 (Bishoftu)")
axis(1, at=1:9, labels=c( "18", "20", "30", "40", "50","60","70","80","90")) 


rowsumofbitingrabieddog_lemuperkm<-rowSums(rabid_dogs[bc,,]/1184)
rowsumofbitingrabieddog_lemuperkm
plot(rowsumofbitingrabieddog_lemuperkm, type="b",pch=16, xaxt="n",
     ylim = c(0, 3), 
     xlim= c(0, length(cov)),
     xlab="Coverage of annual dog vaccination campaigns, % ", 
     ylab="Cumulative number of Rabid dogs over 10 years per km2 (Lemuna-bilbilo)")
axis(1, at=1:10, labels=c( "0", "10", "20", "30", "40", "50","60","70","80","90"))



