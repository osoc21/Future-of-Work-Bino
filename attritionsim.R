library(readxl)
library(dplyr)
library(lubridate)

### attempt to simulate actual attrition data + very very quick POC of using KM estimated survival curves
## was abandoned due to probable lack of data + issues with explanations
# when using this in practice one would need actual survival models and automated checking of assumptions there
# (e.g. PH in Cox PH) would likely be too complex to include in an app + difficult to explain to end users

simdate <- function(x,
                  min = '1900-01-01',
                  max = '2000-01-01',
                  sort = FALSE) {
  
  dates <- sample(seq(as.Date(min), as.Date(max), by = "day"), x, replace = TRUE)
  if (sort == TRUE) {
    sort(dates)
  } else {
    dates
  }
  
}


Workforce <- read_excel("dummydata.xlsx", 
                        sheet = "Workforce")
jobtitles <- unique(Workforce$`Job title`)
countries <- unique(Workforce$`Country of Personnel Area`)

jtfam1 <- c("Global Regulatory CMC Scientist", "Clinical Project Manager", "Supplier & Vendor QA Lead")
jtfam2 <- c("Project Lead Programmer", "Safety Alliance Manager", "Local Regulatory Scientist")


attritiondata <- data.frame("JobTitle" = sample(jobtitles,replace=TRUE,800),
                            "Country" = sample(countries,replace=TRUE,800),
                            "DOB" = simdate(800, min = '1945-01-01', max = '1986-01-01'),
                            "Startdate" = simdate(800, min = '2000-01-01', max = '2016-12-12'))


attritiondata <- attritiondata %>% mutate(
                               Family = ifelse(JobTitle %in% jtfam1,'Science',
                                          ifelse(JobTitle %in% jtfam2, 'Regul', 'Support')))


attritiondata$YOS[attritiondata$Family =='Science'] = floor(rchisq(sum(attritiondata$Family == 'Science'),df=3))
attritiondata$YOS[attritiondata$Family =='Regul'] = floor(rchisq(sum(attritiondata$Family == 'Regul'),df=6))
attritiondata$YOS[attritiondata$Family =='Support'] = floor(rchisq(sum(attritiondata$Family == 'Support'),df=5))

attritiondata <- attritiondata %>% mutate(Enddate = ymd(Startdate + years(YOS)), 
                                          Attrage = floor(interval(DOB,Enddate)/years(1)),
                                          Outcome = ifelse(Attrage > 65, 3, 
                                                           ifelse(Enddate > "2019-01-01", 2,1)),
                                          SurvY = ifelse(Outcome==1,1,2))

#### outcome column 
  # 1 = attrition (dead)
  # 2 = still in service (right censored)
  # 3 = retired (right censored)

write.csv(attritiondata,"attritionsimdata.csv")



survfit(Surv(YOS, SurvY) ~ 1, data = attritiondata)

plot(survfit(Surv(YOS,SurvY) ~ 1, data = attritiondata))

ggsurvplot(
  fit = survfit(Surv(YOS,SurvY) ~ 1, data = attritiondata), 
  xlab = "Years of Service", 
  ylab = "Overall probability of retention",
  risk.table = TRUE)


ggsurvplot(
  fit = survfit(Surv(YOS,SurvY) ~ Family, data = attritiondata), 
  xlab = "Years of Service", 
  ylab = "Overall probability of retention",
  risk.table = TRUE)


summary(survfit(Surv(YOS,SurvY) ~ Family, data = attritiondata))
