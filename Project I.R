#Project 1
library(ggplot2)
library(dplyr)
library(psych)
library(ggpubr)
library(R2HTML)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
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
COLORS <- c("#121619","#D64045","#69995D","#A4BFEB")
setwd("/home/antonisath/Documents/Aueb/2nd Quarter/Statistics 2/project I")

str(dt)
table(dt$SUBSCRIBED)
prop.table(table(dt$SUBSCRIBED))
table(dt$SUBSCRIBED,dt$marital)
prop.table(table(dt$SUBSCRIBED,dt$marital), margin = 2)


round(t(describe(dt[,index])),2)
summary(dt[,-index])
#
index <- sapply(dt, class) == "numeric"
dt[,index]
round(cor(dt[,index]),2)
corrplot::corrplot(cor(dt[,index]))

## PLOTTING
# subscribed vs age
g <- ggplot(data = dt, aes(x = age))

g + geom_histogram(aes(fill = SUBSCRIBED), position = 'identity', alpha = 0.8)

g + geom_histogram() + facet_grid(~SUBSCRIBED)


age_df <- dt %>% group_by(age) %>% summarise(percent_success = sum(SUBSCRIBED=='yes')/n(), n = n())

g <- ggplot(age_df, aes(x = age, y = percent_success))
age_line <-g + geom_line(aes(colour = log(n)),size =0.8) +
  theme_minimal()+ scale_color_gradient(low ="black",high = "red")+
  scale_x_continuous(breaks = seq(16,100,4))+
  labs(title = "Percentage of successful phone calls vs Age",
       y = "Percentage of Success",
       x = "Age",
       color = "Log of Count")+theme(title = element_text(size = 13),
                                     axis.title.x = element_text(size = 12),
                                     axis.title.y = element_text(size = 12),
                                     axis.text.x = element_text(size = 10,
                                                                color = "gray20"),
                                     axis.text.y = element_text(size = 10,
                                                                color = "gray20")
       )


age_line

dt$age_group <- cut(dt$age,
                    breaks = c(16,25,62,99),
                    labels = c("16-25","26-62","63+")) 


g<- ggplot(dt, aes(x = age_group, y = ..count.., fill = SUBSCRIBED))
age_bar <- g+ geom_bar(position = 'fill')+
  geom_text(aes(label = ..count..), stat = "count",
            position = position_fill(vjust = 0.5), color = "white", size = 4.5)
age_bar <-age_bar + theme_minimal()+scale_fill_manual(values = COLORS)+
  labs(title = "Percentage of successful phone calls vs Age Group",
      y = "Percentage of Success",
      x = "Age Group",
      fill = "Subscribed")+theme(title = element_text(size = 13),
                                 axis.title.x = element_text(size = 12),
                                 axis.title.y = element_text(size = 12),
                                 axis.text.x = element_text(size = 10,
                                                               color = "gray20"),
                                 axis.text.y = element_text(size = 10,
                                                               color = "gray20")
      )
age_bar
ggarrange(age_line,age_bar,ncol = 2)
# age groups:
# 17-25
# 26-62
# 63-98

# marital vs subscribed
g <- ggplot(dt, aes(marital, ..count.., fill = SUBSCRIBED))

g + geom_bar(position = 'dodge')

g + geom_bar(aes(fill = SUBSCRIBED),position = 'fill')+
  geom_text(aes(label = ..count..), stat = "count",
            position = position_fill(vjust = 0.5),
            color = "white",size = 4.5)+
  theme_minimal()+theme(title = element_text(size = 13),
                        axis.title.x = element_text(size = 12),
                        axis.title.y = element_text(size = 12),
                        axis.text.x = element_text(size = 10,
                                                   color = "gray20"),
                        axis.text.y = element_text(size = 10,
                                                   color = "gray20")
                        )+
  scale_fill_manual(values = COLORS)+
  labs(title = "Percentage of successful Phone Calls vs Marital Status",
       x = "Marital Status",
       y = "Percentage",
       fill = "Subscribed")

# education vs subscribed
g <- ggplot(dt, aes(education, ..count.., fill = SUBSCRIBED))

g + geom_bar(position = 'fill')+
  geom_text(aes(label = ..count..),stat ="count",
            position = position_fill(vjust =0.5),
            color = "white", size = 4.5)+
  theme_minimal()+scale_fill_manual(values = COLORS)+
  labs(title = "Percentage of successful Phone calls vs Education",
       x = "Education",
       y = "Percentage",
       fill = "Subscribed")+
  theme(axis.text.x = element_text(angle = 30,
                                   size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# job vs subscribed
job_df<-dt %>% group_by(job) %>% mutate(perc_succ = sum(SUBSCRIBED=="yes")/n())

g <- ggplot(job_df, aes(x = reorder(job,-perc_succ), ..count.., fill = SUBSCRIBED))

g + geom_bar(position = 'fill')+
  geom_text(aes(label = ..count..),stat ="count",
            position = position_fill(vjust =0.5),
            color = "white", size = 4.5)+
  theme_minimal()+scale_fill_manual(values = COLORS)+
  labs(title = "Percentage of successful Phone calls vs Job",
       x = "Job",
       y = "Percentage",
       fill = "Subscribed")+
  theme(axis.text.x = element_text(angle = 30,
                                   size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


g + geom_bar(aes(fill = SUBSCRIBED),position = 'dodge')

# housing vs subscribed

g <- ggplot(dt, aes(x = housing, y = ..count.., fill = SUBSCRIBED))

housing <- g + geom_bar(position = 'fill')+
  geom_text(aes(label = ..count..),stat = "count",
            size = 4.5, position = position_fill(vjust=0.5),
            color = "white")+
  theme_minimal()+scale_fill_manual(values = COLORS)+
  labs(title = "Percentage of Successful Phone calls vs Housing Loan",
       x = "Has housing loan",
       y = "Percentage",
       fill = "Subscribed")+
  theme(axis.text.x = element_text(size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
# loan vs Subscribed
g <- ggplot(dt, aes(x = loan, y = ..count.., fill = SUBSCRIBED))

loan <- g + geom_bar(position = 'fill')+
  geom_text(aes(label = ..count..),stat = "count",
            size = 4.5, position = position_fill(vjust=0.5),
            color = "white")+
  theme_minimal()+scale_fill_manual(values = COLORS)+
  labs(title = "Percentage of Successful Phone calls vs Loan",
       x = "Has loan",
       y = "Percentage",
       fill = "Subscribed")+
  theme(axis.text.x = element_text(size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

ggarrange(housing,loan, ncol = 2, common.legend = T)
prop.table(table(dt$loan,dt$SUBSCRIBED))
prop.table(table(dt$housing,dt$SUBSCRIBED))

#default vs SUBSCRIBED

g <- ggplot(dt, aes(default, ..count..,fill = SUBSCRIBED))

default <- g + geom_bar(position = 'fill')+
  geom_text(aes(label = ..count..),stat ="count",
            position = position_fill(vjust =0.5),
            color = "white",size = 4.5)+
  theme_minimal()+scale_fill_manual(values = COLORS)+
  labs(title = "Percentage of Successful Phone calls vs Default",
       x = "Has the client defaulted?",
       y = "Percentage",
       fill = "Subscribed")+
  theme(axis.text.x = element_text(size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


table(dt$default,dt$SUBSCRIBED)

table(dt$default)

# subscribed vs contact
table(dt$contact)

g <- ggplot(dt, aes(contact, ..count.., fill = SUBSCRIBED))

contact <-g + geom_bar(position = 'fill')+
  theme_minimal()+scale_fill_manual(values = COLORS)+
  geom_text(aes(label = ..count..), stat = "count",
            position = position_fill(vjust = 0.5),
            color = "White")+
  labs(title = "Contact Device vs Subscribed",
       x = "Contact Device",
       y = "Percentage",
       fill = "Subscribed")+
  theme(axis.text.x = element_text(size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# subscribed vs month

g <- ggplot(dt, aes(month,..count.., fill = SUBSCRIBED))
g + geom_bar(position = 'fill')+
  geom_text(aes(label =..count..),stat = "count",
            position = position_fill(vjust = 0.5),
            color = "white",size = 4.5)+
  theme_minimal()+scale_fill_manual(values = COLORS)+
  labs(title = "Percentage of Successful Phone calls per Month",
       x = "Month",
       y = "Percentage",
       fill = "Subscribed")+
  theme(axis.text.x = element_text(size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

df2 <- dt %>% group_by(month) %>% mutate(count_month = n())

g <- ggplot(df2, aes(x = reorder(month,count_month),y = ..count.., fill = SUBSCRIBED))

g + geom_bar(position = 'fill')+
  geom_text(aes(label =..count..),stat = "count",
            position = position_fill(vjust = 0.5),
            color = "white",size = 4.5)+
  theme_minimal()+scale_fill_manual(values = COLORS)+
  labs(title = "Percentage of Successful Phone calls per Month",
       subtitle = "Ordered by count",
       x = "Month",
       y = "Percentage",
       fill = "Subscribed")+
  theme(axis.text.x = element_text(size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


#subscribed vs day

g <- ggplot(dt, aes(day_of_week,fill = SUBSCRIBED))
g + geom_bar(stat="count") #RUBBISH


# subscribed vs duration

g <- ggplot(dt,aes(x = duration))
g + geom_histogram(aes(fill = SUBSCRIBED))

g <- ggplot(dt, aes(x = duration, y = SUBSCRIBED))
g + geom_boxplot() + coord_flip()+
  theme_minimal()+
  labs(title = "Boxplot of Duration and Subscribed",
       x = "Duration",
       y = "has subscribed?")+
  theme(axis.text.x = element_text(size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


# previous vs Subscribed
g <- ggplot(dt, aes(x = previous,fill = SUBSCRIBED))
previous <-g + geom_bar(position = 'fill')+ scale_y_continuous(labels = scales::percent)+
  geom_text(aes(label = ..count..), stat = "count",
            position = position_fill(vjust = 0.5),
            size = 4.5,color = "white")+
  theme_minimal()+scale_fill_manual(values = COLORS)+
  labs(title = "Number of Previous Contacts vs Subscribed",
       subtitle = "(before this campaign)",
       x = "Num of Previous Contacts",
       y = "Percentage",
       fill = "Subscribed")+
  theme(axis.text.x = element_text(size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

ggarrange(previous,contact,ncol = 2)
##
table(dt$campaign)
g <- ggplot(dt, aes(x = campaign, y = SUBSCRIBED))
g+ geom_boxplot()

line_df <- dt %>% group_by(campaign) %>% summarise(percent_success = sum(SUBSCRIBED=='yes')/n())
plot(line_df$campaign,line_df$percent_success, type = 'l')


g<- ggplot(line_df,aes(x = campaign, y = percent_success))
g + geom_line(size = 0.8) + theme_minimal()+
  scale_x_continuous(breaks = seq(from = 0,
                                  to = max(dt$campaign),
                                  by = 4))+
  labs(title = "Number of contacts for this campaign vs Percentage of successful Calls",
       x = "Number of contacts",
       y = "Percentage")+
  theme(axis.text.x = element_text(size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


# confidence vs subscribed

g<- ggplot(dt, aes(x = cons.conf.idx, y = SUBSCRIBED))

g + geom_boxplot() + coord_flip()+
  theme_minimal()


g<- ggplot(dt, aes(x = cons.conf.idx, fill = SUBSCRIBED))

g + geom_histogram()


# poutcome vs subscribed
g <- ggplot(dt, aes(x = poutcome,y = ..count..,fill = SUBSCRIBED))

g + geom_bar(position = 'fill')+
  geom_text(aes(label = ..count..),stat ="count",
            position = position_fill(vjust =0.5),
            size = 4.5, color = "white")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1))+theme_minimal()+
  scale_fill_manual(values = COLORS)+
  labs(title = "Previous outcome vs Subcribed",
       x = "Previous outcome",
       y = "Percentage",
       fill = "Subscribed")+
  theme(axis.text.x = element_text(size = 10,
                                   color = "gray20"),
        axis.text.y = element_text(size = 10,
                                   color = "gray20"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


#price.index vs subscribed
price_df <- dt %>% group_by(cons.price.idx) %>% summarise(percent_success = sum(SUBSCRIBED=='yes')/n(), n = n())


g<- ggplot(price_df, aes(x = cons.price.idx, y = percent_success))

g + geom_point(aes(color = n), size =4)+
  geom_text(aes(label = n), vjust = -0.5)

g<- ggplot(dt, aes(cons.conf.idx,SUBSCRIBED))
g + geom_boxplot() + coord_flip()

# euribor vs subscribed

euribor_df <- dt %>% group_by(euribor3m) %>% summarise(percent_success = sum(SUBSCRIBED == "yes")/n(), n = n())

g<- ggplot(euribor_df,aes(euribor3m, percent_success ))
g + geom_line(aes(color = n), size = 1)

g<- ggplot(dt, aes(euribor3m, SUBSCRIBED))
g + geom_boxplot(fill = COLORS[2], color ="black") + coord_flip()+
  theme_minimal()+
  labs(title = "Boxplot of Euribor3m vs Subscribed")

# Modeling


new_dt <- with(dt,
               data.frame(getmode(job),
                          getmode(marital),
                          getmode(education),
                          getmode(default),
                          getmode(loan),
                          median(duration),
                          median(age),
                          cons.conf.idx = seq(min(cons.conf.idx),
                                              max(cons.conf.idx), length.out = 100))
               )
colnames(new_dt)<- c("job",
                     "marital",
                     "education",
                     "default",
                     "loan",
                     "duration",
                     "age",
                     "cons.conf.idx")

preds <- predict(bModel, newdata = new_dt, type = "link")
preds<- plogis(preds)
plot(new_dt$cons.conf.idx, preds)



table(dt$SUBSCRIBED,preds)

plot(dt$duration,bModel$fitted.values)
plot(dt$age,bModel$fitted.values)

####
# MODEL BUILDING
#Model Building
library(glmnet)
library(car)
fullModel <- glm(SUBSCRIBED~.,data = dt, family = "binomial")
summary(fullModel)

stepModel<- step(fullModel,direction = 'both', k = log(nrow(dt)) )
summary(stepModel)
step_formula <- stepModel$call
dput(step_formula, file = "step_formula")
step_formula <- dget(file = "step_formula")

formula <- SUBSCRIBED ~ default + contact+month+duration+poutcome+cons.price.idx+
  cons.conf.idx+euribor3m+age_group+emp.var.rate

stepModel <- glm(step_formula, data =dt, family = "binomial")

logLik(stepModel)
logLik(glm(SUBSCRIBED~1,data =dt, family = "binomial"))
crit <-2*(logLik(stepModel)-logLik(glm(SUBSCRIBED~1,data =dt, family = "binomial")))
summary(stepModel)


df <- data.frame(stepModel$fitted.values,stepModel$y)
ggplot(df, aes(x = stepModel.fitted.values, y = stepModel.y))+
  geom_point(alpha = 0.01, color = COLORS[2])+theme_minimal()+
  labs(title = "Fitted vs Actual",
       x = "Fitted Probability",
       y = "Actual Outcome")
# predicting Plots

myModel <- glm(SUBSCRIBED~default+contact+month+duration+campaign+job+
                 age_group+euribor3m+poutcome+cons.conf.idx,
               data = dt,
               family = "binomial")

summary(myModel)
myModel$deviance;stepModel$deviance

logLik(myModel);logLik(stepModel)

myModel$rank;stepModel$rank
dCrit<- 2*(logLik(stepModel) -logLik(myModel))
dCrit
pchisq(dCrit,2,lower.tail = F) #My model is worse than StepModel

## LASSO

m_dt <- as.matrix(dt)
m_dt <- model.matrix(~.,dt[,-21])
dim(m_dt)
lambdas <- 10 ^ seq(4,-6,length=200)
start.time <- Sys.time()
cv.lasso <- cv.glmnet(m_dt, dt$SUBSCRIBED,
                      family = "binomial", type.measure = "class",
                      lambda = lambdas)
end.time <- Sys.time()
print(end.time-start.time)

plot(cv.lasso)

coef(cv.lasso, s = "lambda.1se") # bad model

lasso_model <- glm(SUBSCRIBED ~job+marital+education+month+contact+duration+campaign+emp.var.rate+age_group+
                     euribor3m+cons.conf.idx+default+poutcome+cons.price.idx,
                   data = dt,
                   family = "binomial")


summary(lasso_model)
summary(stepModel)

#
# Predictive Ploting
new_dt = with(dt,
              data.frame(
                getmode(default),
                getmode(contact),
                getmode(month),
                getmode(poutcome),
                mean(cons.price.idx),
                mean(cons.conf.idx),
                mean(euribor3m),
                getmode(age_group),
                seq(min(duration),
                    max(duration),
                    length.out = 250),
                getmode(emp.var.rate)
              ))



colnames(new_dt)<-c("default","contact",
                    "month","poutcome",
                    "cons.price.idx","cons.conf.idx",
                    "euribor3m","age_group","duration",
                    "emp.var.rate")


dur_change <- predict(stepModel, newdata = new_dt, type="response",se.fit = F)
df <- as.data.frame(cbind(dur_change,new_dt$duration))
d<-ggplot(df,aes(x = V2, y = dur_change))+
  geom_line(size =1 , color = COLORS[2])+theme_minimal()+
  labs(y = "Success Probability",
       x = "Duration (in seconds)",
       title = "Probabilty of subcription vs call duration")


## euribor3m
new_dt = with(dt,
              data.frame(
                getmode(default),
                getmode(contact),
                getmode(month),
                getmode(poutcome),
                mean(cons.price.idx),
                mean(cons.conf.idx),
                seq(min(euribor3m),
                    max(euribor3m),
                    length.out = 250),
                getmode(age_group),
                mean(duration),
                getmode(emp.var.rate)
              ))



colnames(new_dt)<-c("default","contact",
                    "month","poutcome",
                    "cons.price.idx","cons.conf.idx",
                    "euribor3m","age_group","duration",
                    "emp.var.rate")


euribor_change <- predict(stepModel, newdata = new_dt, type="response",se.fit = F)
df <- as.data.frame(cbind(euribor_change,new_dt$euribor3m))
e<-ggplot(df,aes(x = V2, y = euribor_change))+
  geom_line(size =1 , color = COLORS[2])+theme_minimal()+
  labs(y = "Success Probability",
       x = "euribor3m",
       title = "Probabilty of subcription vs euribor3m")


## confidence
new_dt = with(dt,
              data.frame(
                getmode(default),
                getmode(contact),
                getmode(month),
                getmode(poutcome),
                mean(cons.price.idx),
                seq(min(cons.conf.idx),
                    max(cons.conf.idx),
                    length.out = 250),
                mean(euribor3m),
                getmode(age_group),
                mean(duration),
                getmode(emp.var.rate)
              ))



colnames(new_dt)<-c("default","contact",
                    "month","poutcome",
                    "cons.price.idx","cons.conf.idx",
                    "euribor3m","age_group","duration",
                    "emp.var.rate")


conf_change <- predict(stepModel, newdata = new_dt, type="response",se.fit = F)
df <- as.data.frame(cbind(conf_change,new_dt$cons.conf.idx))
c<-ggplot(df,aes(x = V2, y = conf_change))+
  geom_line(size =1 , color = COLORS[2])+theme_minimal()+
  labs(y = "Success Probability",
       x = "Consumer Confidence Index",
       title = "Probabilty of subcription vs Consumer Confidence Index")

#price
new_dt = with(dt,
              data.frame(
                getmode(default),
                getmode(contact),
                getmode(month),
                getmode(poutcome),
                seq(min(cons.price.idx),
                    max(cons.price.idx),
                    length.out = 250),
                mean(cons.conf.idx),
                mean(euribor3m),
                getmode(age_group),
                mean(duration),
                getmode(emp.var.rate)
              ))



colnames(new_dt)<-c("default","contact",
                    "month","poutcome",
                    "cons.price.idx","cons.conf.idx",
                    "euribor3m","age_group","duration",
                    "emp.var.rate")


price_change <- predict(stepModel, newdata = new_dt, type="response",se.fit = F)
df <- as.data.frame(cbind(price_change,new_dt$cons.price.idx))
p<-ggplot(df,aes(x = V2, y = price_change))+
  geom_line(size =1 , color = COLORS[2])+theme_minimal()+
  labs(y = "Success Probability",
       x = "Price Index",
       title = "Probabilty of subcription vs Price Index")

p

ggarrange(d,e,c,p,ncol = 2,nrow = 2)


# TABLE OUTPUTS
# table outputs
options(scipen = 999)
data.frame(variable = names(dt),
           classe = sapply(dt, class),
           first_values = sapply(dt, function(x) paste0(head(x),  collapse = ", ")),
           row.names = NULL) %>% HTML(file = "tables.html")

round(t(describe(dt[,index])),2) %>% HTML(file = 'tables.html')

summary(dt[,!index])

summary(cbind(dt[,"job"],dt[,"marital"],dt[,"education"])) %>% HTML(file = 'tables.html')
summary(cbind(dt[,'default'],dt[,'housing'],dt[,'loan'])) %>% HTML(file = 'tables.html')

