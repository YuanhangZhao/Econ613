rm(list = ls())
library(tidyverse)
library(ggplot2)
library(scales)
library(sampleSelection)
library(AER)
library(plm)
library(panelr)
library(xtable)
setwd("D:/Yanis/Courses/ECON613/HW4")
dat1<-read.csv('dat_A4.csv')
############### excercise 1 ###############
#Q1
#add age and work_exp
dat1<-dat1%>%mutate(age=2019-KEY_BDATE_Y_1997,work_exp=rowSums(across(starts_with("CV_WKSWK_JOB"), function(x) x/52), na.rm = TRUE))
#Q2
#add additional education variables
#res_parent is the total parents' education year
dat1<-dat1%>%mutate(across(starts_with("CV_HGC_BIO"), function(x) ifelse(x == 95, NA, x)),
                    bio_edu = rowSums(across(starts_with("CV_HGC_BIO"))),
                    res_edu1 = rowSums(across(starts_with("CV_HGC_RES"))))
dat1<-dat1%>%mutate(res_edu=ifelse(bio_edu>res_edu1,bio_edu,res_edu1))
#edu is the total year of education of individuals
dat1<-dat1%>%mutate(edu=case_when(YSCH.3113_2019==1~0,
                                  YSCH.3113_2019==2~12,
                                  YSCH.3113_2019==3~12,
                                  YSCH.3113_2019==4~14,
                                  YSCH.3113_2019==5~16,
                                  YSCH.3113_2019==6~18,
                                  YSCH.3113_2019==7~23,                                 
                                  YSCH.3113_2019==0~NA_real_))
#Q3
#Visualize
#rename Gender, marital status, and children number
dat1<-dat1%>%mutate(gender = factor(KEY_SEX_1997, labels = c("male", "female")),
                    marital_status=factor(CV_MARSTAT_COLLAPSED_2019, labels = c("never", "married", "seperated", "divorced", "widowed")),
                    child_num = CV_BIO_CHILD_HH_U18_2019,
                    income = YINC_1700_2019)
#box chart to show the income distribution in different group
dat1 %>%
  ggplot(aes(x = age, y = income, group = age)) +
  geom_boxplot(position="dodge") +
  scale_y_continuous(labels = label_comma())
dat1 %>%
  ggplot(aes(x = gender, y = income, group = gender)) +
  geom_boxplot(position="dodge") +
  scale_y_continuous(labels = label_comma())
dat1 %>%
  ggplot(aes(x = child_num, y = income, group = child_num)) +
  geom_boxplot(position="dodge") +
  scale_y_continuous(labels = label_comma())
#calculate 0 share
dat1<-dat1%>%mutate(income_zero = income == 0 | is.na(income))
dat1%>%group_by(age)%>%summarise(across(c(income_zero),.fns =  mean, na.rm = TRUE))
dat1%>%group_by(gender)%>%summarise(across(c(income_zero),.fns =  mean, na.rm = TRUE))
dat1%>%group_by(marital_status,child_num)%>%summarise(across(c(income_zero),.fns =  mean, na.rm = TRUE))
dat1%>%group_by(marital_status)%>%summarise(across(c(income_zero),.fns =  mean, na.rm = TRUE))
dat1%>%group_by(child_num)%>%summarise(across(c(income_zero),.fns =  mean, na.rm = TRUE))

############### excercise 2 ###############
dat2 <- dat1%>%filter(income_zero==FALSE)%>%select(income, age, gender, marital_status, child_num,
                                                   edu, res_edu, work_exp)%>%drop_na()
dat2<-dat2%>%mutate(married = marital_status=="married")
#OLS model
lm1 <- lm(income~age+gender+married+child_num+edu+res_edu+work_exp,dat2)
summary(lm1)
dat3<-dat1%>%select(income, age, gender, marital_status, child_num,
                    edu, res_edu, work_exp,income_zero)%>%drop_na()
dat3<- dat3%>%mutate(income_positive = !income_zero)
dat3<-dat3%>%mutate(married = marital_status=="married")
#stage 1
stage1 <- glm(income_positive~age+gender+married+child_num+edu+res_edu+work_exp,binomial(link = "probit"),dat3)
summary(stage1)
#stage 2
dat3<-dat3%>%mutate(probit_pred=predict.glm(stage1,dat3),ratio = dnorm(probit_pred)/pnorm(probit_pred)) 
dat3<-dat3%>%mutate(income_new = ifelse(income==0,NA_real_,income))
stage2<-lm(income_new~age+gender+married+child_num+edu+res_edu+work_exp+ratio,dat3)
summary(stage2)

#calculate std
data <- data.matrix(dat3 %>% mutate("(Intercept)" = 1) %>% relocate("(Intercept)"))
positive <- data[,"income"] > 0
data[, "gender"] = data[, "gender"] - 1
errors <- stage2$residuals
probit_pred <- data[, "probit_pred"]
ratio <- data[, "ratio"]
ratio_coef <- stage2$coefficients["ratio"]
delta_i <- ratio * (ratio + probit_pred)
delta_i = delta_i[positive]
variance_hat <- ((errors %*% errors) / length(errors) +  (ratio_coef^2) * sum(delta_i, na.rm = TRUE) / length(errors))[1,1]
s1 <- data[positive, c("(Intercept)", "age","gender", "married", "child_num", "edu","res_edu", "work_exp", "ratio")]
s2 <- data[positive, c("(Intercept)", "age","gender", "married", "child_num", "edu","res_edu", "work_exp")]
q <- (ratio_coef / sqrt(variance_hat))
delta_matrix <- delta_i * diag(length(delta_i))
V <- vcov(stage1)
V <- V
Q_matrix <- q^2 * (t(s1) %*% delta_matrix %*% s2) %*% V %*% (t(s2) %*% delta_matrix %*% s1)
adjusted_var <-  variance_hat * solve(t(s1) %*% s1) %*% 
  (t(s1) %*% (diag(length(delta_i)) - q^2 * delta_matrix) %*% s1 + Q_matrix) %*% 
  solve(t(s1) %*% s1)
adjusted_var <- sqrt(diag(adjusted_var))
heckman_heckit = heckit(income_positive ~ age + gender + married + child_num + edu+ res_edu + work_exp,
                        income ~ age + gender + married + child_num + edu+ res_edu + work_exp,
                        data = dat3)
summary(heckman_heckit)

first_stage <- cbind(summary(heckman_heckit)$estimate[1:length(stage1$coefficients), 1:2], summary(stage1)$coefficients[, 1:2])
colnames(first_stage) = c("heckit : est", "heckit :se", "without package : est", "without package :se")
second_stage <- cbind(summary(heckman_heckit)$estimate[length(stage1$coefficients)+1:length(stage2$coefficients), 1:2], summary(stage2)$coefficients[, 1], adjusted_var)
colnames(second_stage) = c("heckit : est", "heckit :se", "without package : est", "without package :se")



############### excercise 3 ###############
#Q1
dat3 %>% 
  ggplot(aes(x=income)) + 
  geom_histogram(binwidth = 5000) +
  scale_x_continuous(labels = label_comma())
#censored value is 100,000

#Q2
#can be deal with a tobit model
#if income = 100,000, set income = 0, else, set income = income
dat4<-dat3%>%filter(income_positive==TRUE)
model3 <- tobit(income ~ age + gender + married + child_num + edu + res_edu + work_exp,
                     right = 100000, data = dat4, robust=TRUE)
aa1<-summary(model3)
aa1<-cbind(summary(model3)$coefficients)
xtable(aa1)
#Q3


############### excercise 4 ###############
raw_panel = read.csv("dat_A4_panel.csv") 
#convert wide panel to long panel
panel1 <- long_panel(raw_panel, prefix = "_", begin = 1997, end = 2019, label_location = "end")
panel1<-panel1%>%mutate(edu=0)
#find the corresponding education level
panel1$edu<- as.factor(ifelse(panel1$wave==1998,panel1$CV_HIGHEST_DEGREE_9899,
                              ifelse(panel1$wave==1999,panel1$CV_HIGHEST_DEGREE_9900,
                                     ifelse(panel1$wave==2000,panel1$CV_HIGHEST_DEGREE_0001,
                                            ifelse(panel1$wave==2001,panel1$CV_HIGHEST_DEGREE_0102,
                                                   ifelse(panel1$wave==2002,panel1$CV_HIGHEST_DEGREE_0203,
                                                          ifelse(panel1$wave==2003,panel1$CV_HIGHEST_DEGREE_0304,
                                                                 ifelse(panel1$wave==2004,panel1$CV_HIGHEST_DEGREE_0405,
                                                                        ifelse(panel1$wave==2005,panel1$CV_HIGHEST_DEGREE_0506,
                                                                               ifelse(panel1$wave==2006,panel1$CV_HIGHEST_DEGREE_0607,
                                                                                      ifelse(panel1$wave==2007,panel1$CV_HIGHEST_DEGREE_0708,
                                                                                             ifelse(panel1$wave==2008,panel1$CV_HIGHEST_DEGREE_0809,
                                                                                                    ifelse(panel1$wave==2009,panel1$CV_HIGHEST_DEGREE_0910,
                                                                                                           ifelse(panel1$wave==2010,panel1$CV_HIGHEST_DEGREE_1011,
                                                                                                                  ifelse(panel1$wave==2011,panel1$CV_HIGHEST_DEGREE_1112,
                                                                                                                                ifelse(panel1$wave==2013,panel1$CV_HIGHEST_DEGREE_1314,panel1$CV_HIGHEST_DEGREE_EVER_EDT))))))))))))))))

panel1<-panel1%>%mutate(eduyear=case_when(edu==1~12,
                                          edu==2~12,
                                          edu==3~14,
                                          edu==4~16,
                                          edu==5~18,
                                          edu==6~23,
                                          edu==7~18,
                                          edu==0~NA_real_))
panel1<-panel1%>%mutate(age = wave - KEY_BDATE_Y,
                        work_exp=rowSums(across(starts_with("CV_WKSWK_JOB"), function(x) x/52), na.rm = TRUE))
panel1 <- panel1 %>% rename(gender = KEY_SEX, marstat = CV_MARSTAT_COLLAPSED)

panel1<-panel1%>%mutate(gender=case_when(gender==1~'male',
                                          gender==2~"famale"))
panel1<-panel1%>%mutate(married=case_when(marstat==1~'married',
                                         marstat==0|2|3|4~"others"))
#final panel data
data <- panel1%>%select(YINC.1700,gender,age,married,eduyear,work_exp)%>%rename(edu=eduyear,year=wave,income=YINC.1700)%>%drop_na()

#Q2
#within
within<-plm(income~age+gender+married+work_exp+edu,data,model="within")
summary(within)
#between
between<-plm(income~age+gender+married+work_exp+edu,data,model="between")
summary(between)
#first difference
fd<-plm(income~age+gender+married+work_exp+edu,data,model="fd")
summary(fd)
