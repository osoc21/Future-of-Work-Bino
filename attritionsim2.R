library(readr)
library(survival)
library(survminer)

df <- read_csv("attritionsimdata.csv")[,-1]


survfit(Surv(YOS, SurvY) ~ 1, data = df)

plot(survfit(Surv(YOS,SurvY) ~ 1, data = df))

ggsurvplot(
  fit = survfit(Surv(YOS,SurvY) ~ 1, data = df), 
  xlab = "Years of Service", 
  ylab = "Overall probability of retention",
  risk.table = TRUE)


ggsurvplot(
  fit = survfit(Surv(YOS,SurvY) ~ Family, data = df), 
  xlab = "Years of Service", 
  ylab = "Overall probability of retention",
  risk.table = TRUE)
