
###############################################################################
######################## Time series
##############################################################################
rm(list=ls())
Raw_data_r <- read.csv("C:/Users/Benny/OneDrive/Documents/R/oecd_rev1.csv", header = TRUE)
Raw_data_e <- read.csv("C:/Users/Benny/OneDrive/Documents/R/oecd_exp1.csv", header = TRUE)
attach(Raw_data_r)
attach(Raw_data_e)
names(Raw_data_r)

library(tseries)
library(moments)
library(plm)
library(mgcv)
library(splines)
library(Matrix)
library(lmtest)

library(MASS)
library(strucchange)

?strucchange


###############################  Detecting strutucral change


################   Expenditure

cz <- breakpoints(Raw_data_e$CzechRepublic ~ 1)
summary(cz)
plot(cz)

Es <- breakpoints(Raw_data_e$Estonia ~ 1)
summary(Es)
plot(Es)

Hun <- breakpoints(Raw_data_e$Hungary ~ 1)
summary(Hun)
plot(Hun)

Lat <- breakpoints(Raw_data_e$Latvia ~ 1)
summary(Lat)
plot(Lat)


Lith <- breakpoints(Raw_data_e$Lithuania ~ 1)
summary(Lith)
plot(Lith)

Pol <- breakpoints(Raw_data_e$Poland ~ 1)
summary(Pol)
plot(Pol)


sk <- breakpoints(Raw_data_e$SlovakRepublic ~ 1)
summary(sk)
plot(sk)


sl <- breakpoints(Raw_data_e$Slovenia ~ 1)
summary(sl)
plot(sl)


bg <- breakpoints(Raw_data_e$Bulgaria ~ 1)
summary(bg)
plot(bg)

rm<- breakpoints(Raw_data_e$Romania ~ 1)
summary(rm)
plot(rm)



############################## Revenue
Hun <- breakpoints(Raw_data_r$Hungary ~ 1)
summary(Hun)
plot(Hun)


Lat <- breakpoints(Raw_data_r$Latvia ~ 1)
summary(Lat)
plot(Lat)

Lith <- breakpoints(Raw_data_r$Lithuania ~ 1)
summary(Lith)
plot(Lith)

Pol <- breakpoints(Raw_data_r$Poland ~ 1)
summary(Pol)
plot(Pol)

sk <- breakpoints(Raw_data_r$SlovakRepublic ~ 1)
summary(sk)
plot(sk)

sl <- breakpoints(Raw_data_r$Slovenia ~ 1)
summary(sl)
plot(sl)

bg <- breakpoints(Raw_data_r$Bulgaria ~ 1)
summary(bg)
plot(bg)

rm<- breakpoints(Raw_data_r$Romania ~ 1)
summary(rm)
plot(rm)








rm(list=ls())
##### PLotting structural breaks

ab <- read.csv("C:/Users/Benny/OneDrive/Documents/R/oecd.csv", header = TRUE)

Czechia <- ts(ab[1:25,3:4], start="1995", frequency = 1)
Estonia<- ts(ab[26:50,3:4], start="1995", frequency = 1)
Hungary <- ts(ab[51:75,3:4], start="1995", frequency = 1)
Latvia <- ts(ab[76:100,3:4], start="1995", frequency = 1)
Lithuania <- ts(ab[101:125,3:4], start="1995", frequency = 1)
Poland <- ts(ab[126:150,3:4], start="1995", frequency = 1)
Slovakia <- ts(ab[151:175,3:4], start="1995", frequency = 1)
Slovenia <- ts(ab[176:200,3:4], start="1995", frequency = 1)
Bulgaria <- ts(ab[201:225,3:4], start="1995", frequency = 1)
Romania <- ts(ab[226:250,3:4], start="1995", frequency = 1)


par(mfrow = c(1,5))
plot(Czechia[,1], xlab="Time", ylab = "Expenditure", main ="Czech (1 break)")
abline(v=2003,col = "red")
plot(Estonia[,1], xlab="", ylab = "", main ="Estonia (3 breaks)")
abline(v=c(1999,2007,2010),col = "red")
plot(Hungary[,1], xlab="", ylab = "", main ="Hungary (3 break)")
abline(v=c(1998,2001,2015),col = "red")
plot(Latvia[,1], xlab="", ylab = "", main ="Latvia (4 breaks)")
abline(v=c(1997,2000,2008,2011),col = "red")
plot(Lithuania[,1], xlab="", ylab = "", main ="Lithuania (3 breaks)")
abline(v=c(2000,2008,2011),col = "red")



par(mfrow = c(1,5))
plot(Poland[,1], xlab="", ylab = "", main ="Poland (2 breaks)")
abline(v=c(1997, 2011),col = "red")
plot(Slovakia[,1], xlab="", ylab = "", main ="Slovakia (2 breaks)")
abline(v=c(2002,2008),col = "red")
plot(Slovenia[,1], xlab="", ylab = "", main ="Slovenia (2 breaks)")
abline(v=c(2008,2015),col = "red")
plot(Bulgaria[,1], xlab="", ylab = "", main ="Bulgaria (2 break)")
abline(v=c(1998,2002),col = "red")
plot(Romania[,1], xlab="", ylab = "", main ="Romania (4 break)")
abline(v=c(1998,2001,2006,2012),col = "red")
par(mfrow = c(1,1))


################################## Revenue


par(mfrow = c(1,5))
plot(Czechia[,2], xlab="", ylab = "", main ="Czech (2 break)")
abline(v= c(2002, 2010),col = "red")
plot(Estonia[,2], xlab="", ylab = "", main ="Estonia (3 breaks)")
abline(v=c(1998,2008,2011),col = "red")
plot(Hungary[,2], xlab="", ylab = "", main ="Hungary (4 break)")
abline(v=c(1997,2006,2011,2015),col = "red")
plot(Latvia[,2], xlab="", ylab = "", main ="Latvia (2 breaks)")
abline(v=c(1999,2009),col = "red")
plot(Lithuania[,2], xlab="", ylab = "", main ="Lithuania (1 break)")
abline(v=c(2000),col = "red")
par(mfrow = c(1,1))


par(mfrow = c(1,5))
plot(Poland[,2], xlab="", ylab = "", main ="Poland (3 breaks)")
abline(v=c(1997,2008,2015),col = "red")
plot(Slovakia[,2], xlab="", ylab = "", main ="Slovakia (4 breaks)")
abline(v=c(1997,2000,2003,2012),col = "red")
plot(Slovenia[,2], xlab="", ylab = "", main ="Slovenia (2 breaks)")
abline(v=c(2011,2015),col = "red")
plot(Bulgaria[,2], xlab="", ylab = "", main ="Bulgaria (2 break)")
abline(v=c(1997,2008,2012),col = "red")
plot(Romania[,2], xlab="", ylab = "", main ="Romania (1 break)")
abline(v=c(1997),col = "red")
par(mfrow = c(1,1))


######################## Hamilton filter
install.packages("zoo")
library(zoo)
library(xts)
library(neverhpfilter)


rm(list = ls())
y <- as.xts(ts(Czechia[,1], start="1995", frequency = 1))
dimnames(y) <- list(NULL, "Czech")
cyc_czech_exp <- yth_filter(y, p=2, h=2,output = c("x", "trend"))

y1 <- as.xts(ts(Estonia[,1], start="1995", frequency = 1))
dimnames(y1) <- list(NULL, " Estonia")
cyc_Estonia_exp <- yth_filter(y1, p=2, h=2,output = c("x", "trend"))

y2 <- as.xts(ts(Hungary[,1], start="1995", frequency = 1))
dimnames(y2) <- list(NULL, "HUngary")
cyc_Hungary_exp <- yth_filter(y2, p=2, h=2,output = c("x", "trend"))

y3 <- as.xts(ts(Latvia[,1], start="1995", frequency = 1))
dimnames(y3) <- list(NULL, "Latvia")
cyc_Latvia_exp <- yth_filter(y3, p=2, h=2,output = c("x", "trend"))

y4 <- as.xts(ts(Lithuania[,1], start="1995", frequency = 1))
dimnames(y4) <- list(NULL, "Lithuania")
cyc_Lithuania_exp <- yth_filter(y4, p=2, h=2,output = c("x", "trend"))

y5 <- as.xts(ts(Poland[,1], start="1995", frequency = 1))
dimnames(y5) <- list(NULL, "Poland")
cyc_Poland_exp <- yth_filter(y5, p=2, h=2,output = c("x", "trend"))

y6 <- as.xts(ts(Slovakia[,1], start="1995", frequency = 1))
dimnames(y6) <- list(NULL, "Slovakia")
cyc_Slovakia_exp <- yth_filter(y6, p=2, h=2,output = c("x", "trend"))

y7 <- as.xts(ts(Slovenia[,1], start="1995", frequency = 1))
dimnames(y7) <- list(NULL, "Slovenia")
cyc_Slovenia_exp <- yth_filter(y7, p=2, h=2,output = c("x", "trend"))

y8 <- as.xts(ts(Bulgaria[,1], start="1995", frequency = 1))
dimnames(y8) <- list(NULL, "Bulgaria")
cyc_Bulgaria_exp <- yth_filter(y8, p=2, h=2,output = c("x", "trend"))

y9 <- as.xts(ts(Romania[,1], start="1995", frequency = 1))
dimnames(y9) <- list(NULL, "Romania")
cyc_Romania_exp <- yth_filter(y9, p=2, h=2,output = c("x", "trend"))






par(mfrow = c(1,5))
plot(cyc_czech_exp, grid.col = "white", legend.loc = "topleft", main = "Czech")
plot(cyc_Estonia_exp , grid.col = "white", legend.loc = "topleft", main = "Estonia")
plot(cyc_Hungary_exp, grid.col = "white", legend.loc = "topleft", main = "Hungary")
plot(cyc_Latvia_exp , grid.col = "white", legend.loc = "topright", main = "Latvia")
plot(cyc_Lithuania_exp , grid.col = "white", legend.loc = "topleft", main = "Lithuania")
par(mfrow = c(1,1))


par(mfrow = c(1,5))
plot(cyc_Poland_exp, grid.col = "white", legend.loc = "topleft", main = "Poland")
plot(cyc_Slovakia_exp , grid.col = "white", legend.loc = "topleft", main = "Slovakia")
plot(cyc_Slovenia_exp, grid.col = "white", legend.loc = "topleft", main = "Slovania")
plot(cyc_Bulgaria_exp , grid.col = "white", legend.loc = "topright", main = "Bulgaria")
plot(cyc_Romania_exp , grid.col = "white", legend.loc = "topleft", main = "Romania")
par(mfrow = c(1,1))



########## Revenue Hamilton Filter
y <- as.xts(ts(Czechia[,2], start="1995", frequency = 1))
dimnames(y) <- list(NULL, "Czech")
cyc_czech_rev <- yth_filter(y, p=2, h=2,output = c("x", "trend"))

y1 <- as.xts(ts(Estonia[,2], start="1995", frequency = 1))
dimnames(y1) <- list(NULL, " Estonia")
cyc_Estonia_rev <- yth_filter(y1, p=2, h=2,output = c("x", "trend"))

y2 <- as.xts(ts(Hungary[,2], start="1995", frequency = 1))
dimnames(y2) <- list(NULL, "HUngary")
cyc_Hungary_rev <- yth_filter(y2, p=2, h=2,output = c("x", "trend"))

y3 <- as.xts(ts(Latvia[,2], start="1995", frequency = 1))
dimnames(y3) <- list(NULL, "Latvia")
cyc_Latvia_rev <- yth_filter(y3, p=2, h=2,output = c("x", "trend"))

y4 <- as.xts(ts(Lithuania[,2], start="1995", frequency = 1))
dimnames(y4) <- list(NULL, "Lithuania")
cyc_Lithuania_rev <- yth_filter(y4, p=2, h=2,output = c("x", "trend"))

y5 <- as.xts(ts(Poland[,2], start="1995", frequency = 1))
dimnames(y5) <- list(NULL, "Poland")
cyc_Poland_rev <- yth_filter(y5, p=2, h=2,output = c("x", "trend"))

y6 <- as.xts(ts(Slovakia[,2], start="1995", frequency = 1))
dimnames(y6) <- list(NULL, "Slovakia")
cyc_Slovakia_rev <- yth_filter(y6, p=2, h=2,output = c("x", "trend"))

y7 <- as.xts(ts(Slovenia[,2], start="1995", frequency = 1))
dimnames(y7) <- list(NULL, "Slovenia")
cyc_Slovenia_rev <- yth_filter(y7, p=2, h=2,output = c("x", "trend"))

y8 <- as.xts(ts(Bulgaria[,2], start="1995", frequency = 1))
dimnames(y8) <- list(NULL, "Bulgaria")
cyc_Bulgaria_rev <- yth_filter(y8, p=2, h=2,output = c("x", "trend"))

y9 <- as.xts(ts(Romania[,2], start="1995", frequency = 1))
dimnames(y9) <- list(NULL, "Romania")
cyc_Romania_rev <- yth_filter(y9, p=2, h=2,output = c("x", "trend"))


par(mfrow = c(1,5))
plot(cyc_czech_rev, grid.col = "white", legend.loc = "topleft", main = "Czech")
plot(cyc_Estonia_rev , grid.col = "white", legend.loc = "topleft", main = "Estonia")
plot(cyc_Hungary_rev, grid.col = "white", legend.loc = "topleft", main = "Hungary")
plot(cyc_Latvia_rev , grid.col = "white", legend.loc = "topright", main = "Latvia")
plot(cyc_Lithuania_rev , grid.col = "white", legend.loc = "topleft", main = "Lithuania")
par(mfrow = c(1,1))


par(mfrow = c(1,5))
plot(cyc_Poland_rev, grid.col = "white", legend.loc = "topleft", main = "Poland")
plot(cyc_Slovakia_rev , grid.col = "white", legend.loc = "topleft", main = "Slovakia")
plot(cyc_Slovenia_rev, grid.col = "white", legend.loc = "topleft", main = "Slovania")
plot(cyc_Bulgaria_rev , grid.col = "white", legend.loc = "topright", main = "Bulgaria")
plot(cyc_Romania_rev, grid.col = "white", legend.loc = "topleft", main = "Romania")
par(mfrow = c(1,1))








#############################################################################
####Panel Analysis
###########################################################################


rm(list = ls())
Raw_data_c <- read.csv("C:/Users/Benny/OneDrive/Documents/R/oecd1.csv", header = TRUE)
Raw_data_d <- read.csv("C:/Users/Benny/OneDrive/Documents/R/residuals.csv", header = TRUE)


###########################
# declare dataset to be panel
###########################
library(plm) 
library(stargazer)


rm(list = ls())
Raw_data_c <- read.csv("C:/Users/Benny/OneDrive/Documents/R/oecd1.csv", header = TRUE)
Raw_data_d <- read.csv("C:/Users/Benny/OneDrive/Documents/R/residuals.csv", header = TRUE)

Final_data_c<- pdata.frame(Raw_data_c,index=c("Country","Year"))
Final_data_d<- pdata.frame(Raw_data_d,index=c("Countries","Year"))
attach(Final_data_c)
attach(Final_data_d)



names(Final_data_c)


model1 <- plm(expenditure.ratio ~ revenue.ratio + debtratio, data = Final_data_c, model="within")
summary(model1)



ggplot(Raw_data_c, aes(x = revenue.ratio, y = expenditure.ratio)) + 
  geom_point() +
  geom_smooth() 


############## Test for poolability


model.pool <- Final_data_c$revenue.ratio ~ 1 + Final_data_c$expenditure.ratio
pooltest(model.pool, data=Final_data_c, effect = "individual", model = "within")  # different intercepts
pooltest(model.pool, data=Final_data_c, effect = "individual", model = "pooling")  ## identical intercepts


pooltest(model.pool, data=Final_data_c, effect = "time", model = "within")
pooltest(model.pool, data=Final_data_c, effect = "time", model = "pooling")




######################################################################
######## Panel heterogeneous models
#######################################################################

ccmg <- pmg(ham_rev ~  1+ ham_exp , trend = TRUE , data = Final_data_c, model = "cmg")  
summary(ccmg, diagnostics=TRUE)
### includes cross sectional averages of the dependent and independent variables ybar_t and xbar_t.
## Together these can account for the unobserved common factor f_t and given the group-specific estimation the heterogeneous impact (lambda_i) is also given. The coefficients b_i are again averaged across panel members, where different weights may be applied.
hetty <-ccmg$residuals
hetty

mg <- pmg(ham_rev ~ 1+  ham_exp , trend =TRUE, data = Final_data_c)
summary(mg, diagnostics=TRUE)

### Residual diagnostics



### Residual diagnostics plots
par(mfrow = c(1,2))
qqnorm(ccmg$residuals, main = " MG",col=Country)
qqline(ccmg$residuals)
qqnorm(mg$residuals, main = " CCEMG", col=Country)
qqline(mg$residuals)

shapiro.test(ccmg$residuals)
shapiro.test(mg$residuals)


pcdtest(pmg(ham_rev ~  1+ ham_exp , data = Final_data_c) , test = c("cd")) ### Peseran CD test
