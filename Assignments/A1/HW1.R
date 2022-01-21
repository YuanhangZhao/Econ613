rm(list = ls())
library(tidyverse)
library(xtable)
library(ggplot2)
setwd("D:/Yanis/Courses/ECON613/HW1/Data")
##===========Excecise 1===========
d1 <- read.csv("D:/Yanis/Courses/ECON613/HW1/Data/dathh2007.csv", header = TRUE)
d2 <- read.csv("D:/Yanis/Courses/ECON613/HW1/Data/dathh2005.csv", header = TRUE)
d3 <- read.csv("D:/Yanis/Courses/ECON613/HW1/Data/datind2008.csv", header = TRUE)
d4 <- read.csv("D:/Yanis/Courses/ECON613/HW1/Data/datind2016.csv", header = TRUE)
d5 <- read.csv("D:/Yanis/Courses/ECON613/HW1/Data/datind2009.csv", header = TRUE)
d61<- read.csv("D:/Yanis/Courses/ECON613/HW1/Data/datind2005.csv", header = TRUE)
d62<- read.csv("D:/Yanis/Courses/ECON613/HW1/Data/datind2019.csv", header = TRUE)
d7 <- read.csv("D:/Yanis/Courses/ECON613/HW1/Data/datind2010.csv", header = TRUE)
d81 <- read.csv("D:/Yanis/Courses/ECON613/HW1/Data/dathh2011.csv", header = TRUE)
d82 <- read.csv("D:/Yanis/Courses/ECON613/HW1/Data/datind2011.csv", header = TRUE)

#1.1
d1 %>% nrow()
#1.2
d2 %>% filter(mstatus=="Couple, with Kids")%>% nrow()
#1.3
d3 %>% nrow()
#1.4
d4 %>% filter(age >= 25 & age <= 35) %>% nrow()
#1.5
Pro_Gen<-d5 %>%  group_by(profession, gender) %>%  summarize(n=n())
xtable(Pro_Gen)
#1.6
#distribution
d61 %>%  filter(! is.na(wage)) %>%   ggplot(aes(x = wage)) +  geom_density() 
d62 %>%  filter(! is.na(wage)) %>%   ggplot(aes(x = wage)) +  geom_density() 
#mean, std and gini
gini <- function(data){
  wage <- data$wage[!is.na(data$wage)]#clear NA
  wage <- sort(wage)
  n <- length(wage)
  a <- 2*sum((wage-sum(wage)/n) * 1:n)
  b <- n^2*mean(wage)
  gini <- a/b
  return(gini)}
t61<-d61 %>% summarise(MEAN = mean(wage, na.rm = TRUE), STD = sd(wage, na.rm = TRUE), IDR = quantile(wage, 9/10, na.rm = TRUE) - quantile(wage, 1/10, na.rm = TRUE)) %>%  mutate(GINI = gini(d61))
t62<-d62 %>% summarise(MEAN = mean(wage, na.rm = TRUE), STD = sd(wage, na.rm = TRUE), IDR = quantile(wage, 9/10, na.rm = TRUE) - quantile(wage, 1/10, na.rm = TRUE)) %>%  mutate(GINI = gini(d62))
xtable(t61)
xtable(t62)
#1.7
#in 2010, the number of Men in young age is greater than the women, and in old age, the number of women is greater than men.
d7 %>%  filter(! is.na(age)) %>%  ggplot(aes(x=age, fill=gender, group=gender)) + geom_density(alpha =0.7) + scale_x_continuous(limits = c(NA, 100)) +scale_y_continuous() 
#1.8
idmeninParis<-d81 %>%  filter(location == "Paris") %>% pull(idmen)
indinParis<-d82%>%filter(idmen%in%idmeninParis)
indinParis%>%nrow()

##===========Excecise 2===========
d1 <- read.csv("dathh2004.csv", header = TRUE)
d2 <- read.csv("dathh2005.csv", header = TRUE)
d3 <- read.csv("dathh2006.csv", header = TRUE)
d4 <- read.csv("dathh2007.csv", header = TRUE)
d5 <- read.csv("dathh2008.csv", header = TRUE)
d6 <- read.csv("dathh2009.csv", header = TRUE)
d7 <- read.csv("dathh2010.csv", header = TRUE)
d8 <- read.csv("dathh2011.csv", header = TRUE)
d9 <- read.csv("dathh2012.csv", header = TRUE)
d10 <- read.csv("dathh2013.csv", header = TRUE)
d11 <- read.csv("dathh2014.csv", header = TRUE)
d12 <- read.csv("dathh2015.csv", header = TRUE)
d13 <- read.csv("dathh2016.csv", header = TRUE)
d14 <- read.csv("dathh2017.csv", header = TRUE)
d15 <- read.csv("dathh2018.csv", header = TRUE)
d16 <- read.csv("dathh2019.csv", header = TRUE)

e1 <- read.csv("datind2004.csv", header = TRUE)
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
e16 <- read.csv("datind2019.csv", header = TRUE)

hhlist<-list(d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16)
indlist<-list(e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16)
#2.1.2
hh<-d1
for (i in hhlist){
  hh<-rbind(hh,i)
}
#2.1.1
ind<-e1
for (j in indlist){
  ind<-rbind(ind,j)
}
#2.1.3
tbl_vars(ind)[tbl_vars(ind) %in% tbl_vars(hh)]
#2.1.4
data<-left_join(ind,hh,by=c("idmen","year"))
data[data == ""]<-NA#replace all the blank to NA
#2.2.1
familyNUM<-data%>%count(idmen,year)
hhgeq4<-familyNUM%>%filter(n>4)
hhgeq4%>%nrow()
print(xtable(hhgeq4%>%count(year)), include.rownames=FALSE)
#2.2.2
hh_unem1<-data%>%filter(empstat == "Unemployed")%>%count(idmen,year)%>%filter(n>=1)
hh_unem1%>%nrow()
print(xtable(hh_unem1%>%count(year)), include.rownames=FALSE)
#2.2.3
hh_samepro<-data%>% filter(!is.na(profession))%>%count(idmen, year, profession)%>%filter(n>=2)
hh_samepro%>%nrow()
print(xtable(hh_samepro%>%count(year)), include.rownames=FALSE)
#2.2.4
ind_with_kids<-data%>%filter(mstatus=="Couple, with Kids")%>%distinct(idind, year)
ind_with_kids%>%nrow()
print(xtable(ind_with_kids%>%count(year)), include.rownames=FALSE)
#2.2.5
ind_from_Paris<-data%>%filter(location == "Paris")%>%distinct(idind, year)
ind_from_Paris%>%nrow()
print(xtable(ind_from_Paris%>%count(year)), include.rownames=FALSE)
#2.2.6
familyNUM[order(-familyNUM$n),]%>%slice_head() %>% pull(idmen)%>% as.character()
#2.2.7
hh_2010_2011<-data%>% filter(year == 2010 | year == 2011) %>% distinct(idmen, year) 
hh_2010_2011 %>% nrow()
print(xtable(hh_2010_2011 %>% count(year)), include.rownames=FALSE)

##===========Excecise 3===========
#3.1
hh_grouped<-hh%>%group_by(idmen)%>%mutate(time_passed=row_number())
hh_enter<-hh_grouped%>%
  filter(time_passed==1) %>%
  mutate(year_enter = year)%>%
  select(idmen, year_enter,time_passed) 
hh_exit<-hh_grouped%>%
  filter(row_number()==n()) %>%
  mutate(year_exit = year)%>%
  select(idmen, year_exit,time_passed) 
hh_enter_exit<-left_join(hh_enter,hh_exit,by="idmen")
hh_enter_exit_1<-hh_enter_exit%>%mutate(time_spent=year_exit-year_enter+1)%>%select(idmen,year_enter,year_exit,time_spent)
#distribution
hh_enter_exit_1 %>%  ggplot(aes(x = time_spent)) +  geom_histogram() 

#3.2
hh_datent<-data%>%mutate(year_is_datent = (datent == year))
print(xtable(hh_datent%>%head(10)), include.rownames=FALSE)
share<-hh_datent%>%group_by(year) %>%summarise(datent_portion = mean(year_is_datent, na.rm=TRUE)) 
share%>%ggplot(aes(x = year, y = datent_portion)) + geom_line() 

#3.3
hh_mig<-data%>%mutate(year_movein = ((myear == year) & !is.na(myear)) |((move == 2)& !is.na(move)))
print(xtable(hh_mig%>%head(10)), include.rownames=FALSE)
move_share<-hh_mig%>%group_by(year) %>%summarise(movein_portion = mean(year_movein, na.rm=TRUE)) 
move_share%>%ggplot(aes(x = year, y = movein_portion)) + geom_line() 

#3.4
datent_mig<-left_join(share,move_share)
datent_mig%>%ggplot(aes(x = year)) +   geom_line(aes(y = datent_portion), colour = "datent_portion") + geom_line(aes(y = movein_portion), color = "movein_portion") +labs(x = "Year", y = "(%)", color = "Legend") +  scale_color_manual("",breaks=c("datent_portion","movein_portion"),values = c("red","green"))
datent_mig%>%ggplot( aes(x = year)) +
  geom_line(aes(y = datent_portion, colour = "datent_portion")) +
  geom_line(aes(y = movein_portion, colour = "movein_portion")) +
  scale_colour_manual("", 
                      breaks = c("datent_portion", "movein_portion"),
                      values = c("red", "green")) +
  xlab(" ") +
  scale_y_continuous("(%)") + 
  labs(title="Datent and Movein Portion")

#3.5
hh_change_work<-hh_mig%>%filter(year_movein==1)%>%group_by(idind)%>%mutate(change_pro=n_distinct(profession, na.rm = TRUE) > 1)%>%mutate(change_em=n_distinct(empstat,na.rm = TRUE)>1)%>%mutate(change_work=change_em|change_pro)
hh_change_work%>%ungroup()%>%filter(change_work==1)%>%nrow()
#xtable(hh_change_work%>%ungroup()%>%filter(change_work==1)%>%count(year))

##===========Excecise 4===========
ind_grouped<-ind%>%group_by(idind)%>%arrange(year)
#every individual has a enter and exit year
#if exit year == survey year, means the individual exit that survey year, which is the attrition
#match the enter and exit year of individuals with the household idmen.
ind_enter_exit <- left_join(hh_enter_exit_1,ind_grouped,by="idmen")
ind_enter_exit<-ind_enter_exit%>%mutate(exit=(year_exit==year))
#true means attrition,
#note that the attrition rate in 2019 cannot be calculate
#since it is the last survey year
attrition_rate<-ind_enter_exit%>%group_by(year) %>%summarise(attrition_rate = mean(exit, na.rm=TRUE)) 
print(xtable(attrition_rate), include.rownames=FALSE)