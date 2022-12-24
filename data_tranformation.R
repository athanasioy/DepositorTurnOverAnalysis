# data tranformation

# age groups:
# 17-25
# 26-62
# 63-98
setwd("C:\\R Projects\\2nd Quarter\\Statistics 2\\project I")
getwd()
require(readxl)
dt <- read_excel("project I  2021-2022.xls", sheet = "bank-additional-full.csv")

head(dt)
str(dt)

dt$job <- as.factor(dt$job)
dt$default <- as.factor(dt$default)
dt$marital <- as.factor(dt$marital)
dt$education <- as.factor(dt$education)
dt$housing <- as.factor(dt$housing)
dt$loan <- as.factor(dt$loan) 
dt$contact <- as.factor(dt$contact)
dt$month <- as.factor(dt$month)
dt$poutcome <- as.factor(dt$poutcome)
dt$day_of_week <- as.factor(dt$day_of_week)
dt$SUBSCRIBED <- as.factor(dt$SUBSCRIBED)

dt$age_group <- cut(dt$age,
                    breaks = c(16,25,62,99),
                    labels = c("16-25","26-62","63+")) 

#dt$below_25 <- dt$age <=25

dt$pdays <- replace(dt$pdays, dt$pdays == 999,0)
#table(dt$pdays)

#dt$age_below25 <- dt$age * dt$below_25

