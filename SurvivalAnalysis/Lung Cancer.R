library(survival)
library(tidyr)
library(survminer)
library(ggplot2)
library(ggpubr)
library(dplyr)

help (cancer)
#Loading data
Cancer <- read.csv("C:/Users/LENOVO/Music/cancer.csv",sep=";")
View(Cancer)

#Variabel yang digunakan
# Menggunakan fungsi subset
cancer <- subset(Cancer, select = c(time, status, sex, age, meal.cal, ph.ecog, wt.loss))

### PREPROCESSING
#Cek NA
sum(is.na(cancer))

# Function to impute missing values for numeric columns with mean
impute_mean <- function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
}

# Apply mean imputation to numeric columns
cancer <- cancer %>%
  mutate(across(where(is.numeric), impute_mean))

# Check for remaining NA values
sum(is.na(cancer))

# Cek Duplikasi Data
duplicates <- duplicated(cancer)
sum(duplicates)

### STATISTIKA DESKRIPTIF
summary(cancer$time)
summary(cancer$status)

#Kaplan Meir
y <- Surv(cancer$time, cancer$status==1) ;y
kmfit=survfit(y~1)
kmfit
plot(kmfit,col=c("#264653","#2a9d8f","#b7e4c7"), lwd=2, xlab ="Time", 
     ylab ="Probabilitas Survival", main="Kaplan-Meier Curves")

cancer$age_cat <- cut(cancer$age, breaks = c(39,64,82), labels = c("Age <= 64 yo","Age >64 yo"))
km_age <- survfit(y~cancer$age_cat)
plot(km_age, xlab ="Time", ylab = "Probabilitas Survival", col = c("black", "blue"), main = "Kurva Kaplan-Meier Umur Pasien")
legend("topright", c("Age <= 64 yo","Age > 64 yo"), 
       lty=c("solid"), col = c("black", "blue"))

cancer$ph.ecog <- factor(cancer$ph.ecog, levels = c(0, 1, 2), 
                         labels = c("asymptomatic", 
                                    "in bed <50% of the day", 
                                    "in bed > 50% of the day but not bedbound"))
km_ph.ecog <- survfit(y ~ cancer$ph.ecog, data = cancer)
plot(km_ph.ecog, xlab ="Time", ylab = "Probabilitas Survival", col = c("black","red", "brown"), 
     main = "Kurva Kaplan-Meier skor kinerja ECOG")
legend("topright", c("asymptomatic",
                     "in bed <50% of the day", "in bed > 50% of the day 
                     but not bedbound"), cex = 0.65,
       lty=c("solid"), col = c("black", "red", "brown"))

km_sex <- survfit(y~sex, data=cancer)
plot(km_sex, xlab ="Time", ylab = "Probabilitas Survival", col = c("navy", "red"), main = "Kurva Kaplan-Meier Jenis Kelamin Pasien")
legend("topright", c("Male","Female"), 
       lty=c("solid"), col = c("navy", "red"))

cancer$meal.calcat <- cut(cancer$meal.cal, breaks = c(96,975,2600), labels = c("Low","High"))
km_meal.cal <- survfit(y ~ cancer$meal.calcat)
plot(km_meal.cal, xlab ="Time", ylab = "Probabilitas Survival", col = c("Orange","Red"), 
     main = "Calories")
legend("topright", c("Low","High"), 
       lty=c("solid"), col = c("Orange","Red"))

cancer$wt.losscat <- cut(cancer$wt.loss, breaks = c(-24,7,68), labels = c("Low","High"))
km_wt.loss <- survfit(y ~ cancer$wt.losscat)
plot(km_wt.loss, xlab ="Time",  ylab = "Probabilitas Survival", col = c("Red","Blue"), 
     main = "Weight Loss")
legend("topright", c("Low","High"), 
       lty=c("solid"), col = c("Red","Blue"))

## UJI LOG-RANK ## 
lr1<-survdiff(y ~ age,data=cancer)
lr1 #Berbeda signifikan antara dua kurva

lr2<-survdiff(y ~ cancer$sex)
lr2 #Berbeda signifikan antara dua kurva

lr3<-survdiff(y ~ cancer$ph.ecog)
lr3 #Tidak ada perbedaan signifikan

lr4<-survdiff(y ~ cancer$meal.cal) 
lr4  #Berbeda signifikan antara dua kurva

lr5<-survdiff(y ~ cancer$wt.loss)
lr5 #Berbeda signifikan antara dua kurva

## MODEL COX PH ##
model1 <- coxph(y ~ age + sex  + ph.ecog + meal.cal + wt.loss,
                data = cancer)
model1

#Metode Grafik
## UJI ASUMSI PH ##
# METODE GRAFIK #
###LogLog Plot
minusloglog <- function(p){
  return(-log(-log(p)))
}
##AGE
min(cancer$age)
quantile(cancer$age, 0.5)
max(cancer$age)

cancer$age_cat <- cut(cancer$age, breaks = c(39,64,82), labels = c("Age <= 64 yo","Age >64 yo"))
kmfit_age <- survfit(y ~ cancer$age_cat)
plot(kmfit_age, fun = minusloglog, xlab ="Time", 
     ylab = "-log-log S", col = c("black", "blue"), main = "LogLog Plot Age")
legend("topright", c("Age <= 64 yo","Age > 64 yo"), 
       lty=c("solid"), col = c("black", "blue"))

##SEX
kmfit_sex <- survfit(y ~ cancer$sex)
plot(kmfit_sex, fun = minusloglog, xlab ="Time", 
     ylab = "-log-log S", col = c("navy", "pink"), main = "LogLog Plot Sex")
legend("topright", c("Male","Female"), 
       lty=c("solid"), col = c("navy", "pink"))

##ph.ecog
kmfit_sex <- survfit(y ~ cancer$ph.ecog)
plot(kmfit_sex, fun = minusloglog, xlab ="Time", 
     ylab = "-log-log S", col = c("black", "red", "green"), main = "LogLog Plot ph.ecog")
legend("topright", c("asymptomatic", "in bed <50% of the day", "in bed > 50% of the day but not bedbound"),
       lty=c("solid"), col = c("black", "red", "green"))

#meal.cal 
min(cancer$meal.cal)
quantile(cancer$meal.cal, 0.5)
max(cancer$meal.cal)

cancer$meal.calcat <- cut(cancer$meal.cal, breaks = c(96,975,2600), labels = c("Low","High"))
kmfit_meal.cal <- survfit(y ~ cancer$meal.calcat)
plot(kmfit_meal.cal, fun = minusloglog, xlab ="Time", 
     ylab = "-log-log S", col = c("Orange","Red"), 
     main = "LogLog Plot Calories Consumed")
legend("topright", c("Low","High"), 
       lty=c("solid"), col = c("Orange","Red"))

#Weight loss
min(cancer$wt.loss)
quantile(cancer$wt.loss, 0.5)
max(cancer$wt.loss)

cancer$wt.losscat <- cut(cancer$wt.loss, breaks = c(-24,7,68), labels = c("Low","High"))
kmfit_wt.loss <- survfit(y ~ cancer$wt.losscat)
plot(kmfit_wt.loss, fun = minusloglog, xlab ="Time", 
     ylab = "-log-log S", col = c("Red","Blue"), 
     main = "LogLog Plot Weight Loss")
legend("topright", c("Low","High"), 
       lty=c("solid"), col = c("Red","Blue"))

#Observed vs Expected
plot(km_age,
     main = "Observed vs Expected Survival Probability for Usia",
     xlab = "Time (day)", ylab = "Survival Probability",
     lty = 1, col = c("red", "blue")
)
kmfit_age <- coxph(formula = y ~ age, data = cancer)
age_new <- data.frame(age = 0:1)
lines(survfit(kmfit_age, age_new ), col = c("red", "blue"),lty = 2)
legend("topright", legend = c("0 (Obs)","0 (Eks)","1 (Obs)","1 (Eks)"), col = c("red","red","blue","blue"), lty =c(1,2,1,2), title = "Age")

#Sex
plot(km_sex,
     main = "Observed vs Expected Survival Probability for Sex",
     xlab = "Time (day)", ylab = "Survival Probability",
     lty = 1, col = c("red", "blue")
)
kmfit_sex <- coxph(formula = y ~ sex, data = cancer)
sex_new <- data.frame(sex = 0:1)
lines(survfit(kmfit_sex, sex_new ), col = c("red", "blue"),lty = 2)
legend("topright", legend = c("0 (Obs)","0 (Eks)","1 (Obs)","1 (Eks"), col = c("red","red","blue","blue"), lty =c(1,2,1,2), title = "Sex")

#Ph.ecog
plot(km_ph.ecog,
     main = "Observed vs Expected Survival Probability 
     for kinerja ECOG",
     xlab = "Time (day)", ylab = "Survival Probability",
     lty = 1, col = c("red", "black", "blue")
)
kmfit_ph.ecog <- coxph(formula = y ~ ph.ecog, data = cancer)
ph.ecog_new <- data.frame(ph.ecog = factor(0:2, levels = c(0, 1, 2), 
                                           labels = c("asymptomatic", 
                                                      "in bed <50% of the day", 
                                                      "in bed > 50% of the day but not bedbound")))
lines(survfit(kmfit_ph.ecog, ph.ecog_new ), col = c("red", "black", "blue"),lty = 2)
legend("topright", legend = c("0 (Obs)","0 (Eks)","1 (Obs)","1 (Eks)","2 (Obs)","2 (Eks"), col = c("red","red","black","black", "brown","brown"), lty =c(1,2,3,1,2,3), title = "ph.ecog",cex=0.7)

#meal.cal
plot(km_meal.cal,
     main = "Observed vs Expected Survival Probability for Calories",
     xlab = "Time (day)", ylab = "Survival Probability",
     lty = 1, col = c("red", "blue")
)
kmfit_meal.cal <- coxph(formula = y ~ meal.cal, data = cancer)
meal.cal_new <- data.frame(meal.cal = 0:1)
lines(survfit(kmfit_meal.cal, meal.cal_new ), col = c("red", "blue"),lty = 4)
legend("bottomleft", legend = c("0 (Obs)","0 (Eks)","1 (Obs)","1 (Eks)"), col = c("red","red","blue","blue"), lty =c(1,2,1,2), title = "ph.karno")

#ws.losscat
plot(km_wt.loss,
     main = "Observed vs Expected Survival Probability for Weight Loss",
     xlab = "Time (day)", ylab = "Survival Probability",
     lty = 1, col = c("red", "blue")
)
kmfit_wt.loss <- coxph(formula = y ~ wt.loss, data = cancer)
wt.loss_new <- data.frame(wt.loss = 0:1)
lines(survfit(kmfit_wt.loss, wt.loss_new ), col = c("red", "blue"),lty = 4)
legend("bottomleft", legend = c("0 (Obs)","0 (Eks)","1 (Obs)","1 (Eks)"), col = c("red","red","blue","blue"), lty =c(1,2,1,2), title = "ph.karno")

###GOF
gof=cox.zph(model1)
gof

#(Schoendeld Individual Test)
ggcoxzph(gof)

#Model Strata Tanpa Interaksi
#Model 2
## MODEL COX PH  ##
stra1 <- coxph(y ~ age + sex + ph.ecog + wt.loss + strata(meal.cal),
                data = cancer)
summary(stra1)
#Variabel wt.loss tidak signifikan

#Model 3
stra2 <- coxph(y ~ age + sex + ph.ecog + 
                strata(meal.cal),data = cancer)
summary(stra2)
#Age tidak signifikan

stra3 <- coxph(y ~ sex + ph.ecog + strata(meal.cal),
               data = cancer)
summary(stra3)
#Sudah SIgnifikan

#Model Strata Dengan Interaksi
stra4 <- coxph(y ~ sex + ph.ecog + sex:meal.cal+ ph.ecog:meal.cal + strata(meal.cal),
               data = cancer)
summary(stra4)

#likelihood Test
# HO : Model dengan interaksi tidak memiliki pengaruh signifikan terhadap survival time
# H1 : Model dengan interaksi memiliki pengaruh signifikan terhadap survival time
# STATISTIK UJI
G <- -2*(stra1$loglik[2]-stra6$loglik[2]); G
Zchisq_table <- qchisq(0.95,2); chisq_table
# KEPUTUSAN
if(G > chisq_table){
  print("Tolak H0")
  print("Model dengan interaksi memiliki pengaruh signifikan terhadap survival time")
}else{
  print("Gagal Tolak H0")
  print("Model dengan interaksi tidak memiliki pengaruh signifikan terhadap survival time")
}

