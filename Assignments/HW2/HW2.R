rm(list = ls())
library(tidyverse)
library(xtable)
library(ggplot2)
library(AER)
library(ggpubr)

setwd("D:/Yanis/Courses/ECON613/HW1/Data")
data <- read.csv("D:/Yanis/Courses/ECON613/HW1/Data/datind2009.csv", header = TRUE)
#replace all 0 to na
data[data == ""]<-NA
data_reg<-data.frame(data$age,data$wage)
#remove all na values
X_Y<-data_reg[complete.cases(data_reg),]
#generate age with intercept
intercept <- rep(1, nrow(X_Y))
##===========Exercise 1===========
#generate age with intercept
#Q1
y <- as.matrix(X_Y[, "data.wage"])
x <- as.matrix(X_Y[, "data.age"])
Y<-as.matrix(y)
X<-as.matrix(cbind(intercept,x))
colnames(Y)[1] <- 'wage'
colnames(X)[2] <- 'age'
#calculate the correlation
cor(X,Y)
#Q2
beta <- solve(t(X) %*% X) %*% (t(X) %*% Y)
xtable(beta)
#method with lm()
#beta_1 = lm(Y~X)

#Q3.1
# Calculate residual variance
residuals <- Y - X %*% beta
# degree of freedom calculation 
p <- ncol(X) - 1 
df <- nrow(X) - p - 1 
# Residual variance 
res_var <- sum(residuals^2) / df 
# Calculate covariance matrix of estimate 
beta_cov <- res_var * solve(t(X) %*% X)
# Square root of the diag 
beta_se <- sqrt(diag(beta_cov))
beta_se

#Q3.2
X_Y <-as.matrix(cbind(intercept,X_Y))
colnames(X_Y) = c("intercept","age","wage")
nind = nrow(X_Y);            # number of individuals
reg = lm(wage~age,data = data.frame(X_Y))
nvar = length(reg$coefficients)  # number of variables
BT<-function(dat,R,nind,nvar){
  outs = mat.or.vec(R,nvar)
  set.seed(123)
  data = data.frame(dat)
  for (i in 1:R)
  {
    samp     = sample(1:nind,nind,rep=TRUE)
    dat_samp = data[samp,]
    reg1     = lm(wage ~ age,data = dat_samp)
    outs[i,] = reg1$coefficients
  }
  
  mean_est = apply(outs,2,mean)
  sd_est   = apply(outs,2,sd)
  
  est = cbind(mean_est,
              sd_est)
  colnames(est) = c("BT: est","BT: sd")
  return(est)}
xtable(BT(X_Y,49,nind,nvar))
xtable(BT(X_Y,499,nind,nvar))
##===========Exercise 2===========
e2 <- read.csv("datind2005.csv", header = TRUE)
e3 <- read.csv("datind2006.csv", header = TRUE)
e4 <- read.csv("datind2007.csv", header = TRUE)
e5 <- read.csv("datind2008.csv", header = TRUE)
e6 <- read.csv("datind2009.csv", header = TRUE)
e7 <- read.csv("datind2010.csv", header = TRUE)
e8 <- read.csv("datind2011.csv", header = TRUE)
e9 <- read.csv("datind2012.csv", header = TRUE)
e10 <- read.csv("datind2013.csv", header = TRUE)
e11 <- read.csv("datind2014.csv", header = TRUE)
e12 <- read.csv("datind2015.csv", header = TRUE)
e13 <- read.csv("datind2016.csv", header = TRUE)
e14 <- read.csv("datind2017.csv", header = TRUE)
e15 <- read.csv("datind2018.csv", header = TRUE)
indlist<-list(e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15)
ind<-e2
for (j in indlist){
  ind<-rbind(ind,j)
}
ind1<-ind[complete.cases(ind$wage),]
#Q1
ind1$ag <- as.factor(ifelse(ind1$age<18, '0-18',ifelse(ind1$age<26, '18-25',
                                                     ifelse(ind1$age<31, '26-30',
                                                            ifelse(ind1$age<35, '31-35', 
                                                                   ifelse(ind1$age<41, '36-40',
                                                                          ifelse(ind1$age<46, '41-45',
                                                                                 ifelse(ind1$age<51, '46-50',
                                                                                        ifelse(ind1$age<56, '51-55',
                                                                                               ifelse(ind1$age<61, '56-60','60+'))))))))))
#Q2
indsub1<-subset(ind1,ag=="0-18")
indsub2<-subset(ind1,ag=="18-25")
indsub3<-subset(ind1,ag=="26-30")
indsub4<-subset(ind1,ag=="31-35")
indsub5<-subset(ind1,ag=="36-40")
indsub6<-subset(ind1,ag=="41-45")
indsub7<-subset(ind1,ag=="46-50")
indsub8<-subset(ind1,ag=="51-55")
indsub9<-subset(ind1,ag=="56-60")
indsub10<-subset(ind1,ag=="60+")
p1<-indsub1%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p2<-indsub2%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p3<-indsub3%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p4<-indsub4%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p5<-indsub5%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p6<-indsub6%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p7<-indsub7%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p8<-indsub8%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p9<-indsub9%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p10<-indsub10%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
ggexport(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, filename = "charts.png",
         nrow = 4, ncol = 3)
#Q3
ind$year2005 <- ifelse(ind$year == 2005, 1, 0)
ind$year2006 <- ifelse(ind$year == 2006, 1, 0)
ind$year2007 <- ifelse(ind$year == 2007, 1, 0)
ind$year2008 <- ifelse(ind$year == 2008, 1, 0)
ind$year2009 <- ifelse(ind$year == 2009, 1, 0)
ind$year2010 <- ifelse(ind$year == 2010, 1, 0)
ind$year2011 <- ifelse(ind$year == 2011, 1, 0)
ind$year2012 <- ifelse(ind$year == 2012, 1, 0)
ind$year2013 <- ifelse(ind$year == 2013, 1, 0)
ind$year2014 <- ifelse(ind$year == 2014, 1, 0)
ind$year2015 <- ifelse(ind$year == 2015, 1, 0)
ind$year2016 <- ifelse(ind$year == 2016, 1, 0)
ind$year2017 <- ifelse(ind$year == 2017, 1, 0)
ind$year2018 <- ifelse(ind$year == 2018, 1, 0)
y1 <- as.matrix(ind1[, "wage"])
x1 <- as.matrix(ind1[, "age"])
Y1<-as.matrix(y1)
intercept1<-rep(1, nrow(ind1))
X1<-as.matrix(cbind(intercept1,x1,ind1$year2006,ind1$year2007,ind1$year2008,ind1$year2009,ind1$year2010,ind1$year2011,ind1$year2012,ind1$year2013,ind1$year2014,ind1$year2015,ind1$year2016,ind1$year2017,ind1$year2018))
colnames(Y1)[1] <- 'wage'
colnames(X1)[2] <- 'age'
beta_1 = lm(Y1~X1)
beta1 <- solve(t(X1) %*% X1) %*% (t(X1) %*% Y1)
xtable(beta1)

##===========Exercise 3===========
data_1<-read.csv("datind2007.csv", header = TRUE)
#Q1
#remove all Inactive individuals
em<-subset(data_1,empstat != "Inactive")
#Q2
likelihood <- function(beta, y, X){
  y_1 <- X %*% beta
  prob_y <- pnorm(y_1)
  prob_y[prob_y > 0.9999] = 0.9999
  prob_y[prob_y < 0.0001] = 0.0001
  log_likelihood <- sum(y * log(prob_y) + (1-y) * log(1-prob_y))
  return(log_likelihood)
}

#Q3
em$emst <- as.factor(ifelse(em$empstat=="Employed", '1',"0"))
em.probit <- glm(emst~age, 
                   data = em,
                   family = binomial(link = "probit"))
xtable(summary(em.probit))
#Q4
#The wage cannot be the determinant of the labor market, because it will be omitted with the employment status
#Once individuals get employed, it will get wage. and wage is not a factor that have effects on employment status.

##===========Exercise 4===========
indlist2<-list(e3,e4,e5,e6,e7,e8,e9,e10,e11,e12)
ind2<-e2
for (j in indlist2){
  ind2<-rbind(ind2,j)
}
ind2$year2005 <- ifelse(ind2$year == 2005, 1, 0)
ind2$year2006 <- ifelse(ind2$year == 2006, 1, 0)
ind2$year2007 <- ifelse(ind2$year == 2007, 1, 0)
ind2$year2008 <- ifelse(ind2$year == 2008, 1, 0)
ind2$year2009 <- ifelse(ind2$year == 2009, 1, 0)
ind2$year2010 <- ifelse(ind2$year == 2010, 1, 0)
ind2$year2011 <- ifelse(ind2$year == 2011, 1, 0)
ind2$year2012 <- ifelse(ind2$year == 2012, 1, 0)
ind2$year2013 <- ifelse(ind2$year == 2013, 1, 0)
ind2$year2014 <- ifelse(ind2$year == 2014, 1, 0)
ind2$year2015 <- ifelse(ind2$year == 2015, 1, 0)
em2<-subset(ind2,empstat != "Inactive")
em2$emst <- ifelse(em2$empstat=="Employed", 1,0)

em2.probit <- glm(emst~age+year2006+year2007+year2008+year2009+year2010+year2011+year2012+year2013+year2014+year2015, 
                 data = em2,
                 family = binomial(link = "probit"))
xtable(summary(em2.probit))
em2.logit <- glm(emst~age+year2006+year2007+year2008+year2009+year2010+year2011+year2012+year2013+year2014+year2015, 
                  data = em2,
                  family = binomial(link = "logit"))
xtable(summary(em2.logit))
em2.linear<- lm(emst ~ age+year2006+year2007+year2008+year2009+year2010+year2011+year2012+year2013+year2014+year2015,data=em2)
xtable(summary(em2.linear))

##===========Exercise 5===========
#the marginal effects of probit model
margin_data0 = data.frame(age = mean(em2$age),year2006 = mean(em2$year2006),year2007 = mean(em2$year2007),year2008 = mean(em2$year2008),year2009 = mean(em2$year2009),year2010 = mean(em2$year2010),year2011 = mean(em2$year2011),year2012 = mean(em2$year2012),year2013 = mean(em2$year2013),year2014 = mean(em2$year2014),year2015 = mean(em2$year2015))

probit = predict(em2.probit, margin_data0, type="response", se=TRUE)
probit_fit = data.frame(Margin = probit$fit[1], se=probit$se.fit[1])

#the marginal effects of logit model
margin_data1 = data.frame(age = mean(em2$age),year2006 = mean(em2$year2006),year2007 = mean(em2$year2007),year2008 = mean(em2$year2008),year2009 = mean(em2$year2009),year2010 = mean(em2$year2010),year2011 = mean(em2$year2011),year2012 = mean(em2$year2012),year2013 = mean(em2$year2013),year2014 = mean(em2$year2014),year2015 = mean(em2$year2015))

logit = predict(em2.logit, margin_data1, type="response", se=TRUE)
logit_fit = data.frame(Margin = logit$fit[1], se=logit$se.fit[1])
sum = rbind(probit_fit,logit_fit)
xtable(sum)
