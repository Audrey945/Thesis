##########################################################################################
#                           UNIVERSITY OF CAPE TOWN                                      #
#                           STUDENT NAME:   AUDREY MOYO                                  #
#                                REGRESSION ANALYSIS                                     #
#========================================================================================#
#THESIS TOPIC: Trends and determinants of contraceptive use and method choice among young# 
#              Zimbabwean women from 1988 to 2015                                        #               
##########################################################################################

#========================================================================================#
#                         BINARY LOGISTIC REGRESSSION
#Clear memory
rm(list=ls())

#----------------------#
#Set working directory
setwd("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes")

#Read data
DHS1988 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1988.csv")[,-1]
DHS1994 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1994.csv")[,-1]
DHS1999 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1999.csv")[,-1]
DHS2005 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2005.csv")[,-1]
DHS2010 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2010.csv")[,-1]
DHS2015 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2015.csv")[,-1]

#***********************************************************#
#CREATE AGE GROUP VARIABLE                                  #
#***********************************************************#

library(plyr) #load package

#1988 data
DHS1988$age_grp2 <- mapvalues(DHS1988$v012,c(15,16,17,18,19,20,21,22,23,24),c(1,1,1,2,2,3,3,0,0,0),
                              warn_missing = TRUE) #redefining values
table(DHS1988$age_grp2) #checking mapped values

#1994 data
DHS1994$age_grp2 <- mapvalues(DHS1994$v012,c(15,16,17,18,19,20,21,22,23,24),c(1,1,1,2,2,3,3,0,0,0),
                              warn_missing = TRUE) #redefining values
table(DHS1994$age_grp2) #checking mapped values

#1999 data
DHS1999$age_grp2 <- mapvalues(DHS1999$v012,c(15,16,17,18,19,20,21,22,23,24),c(1,1,1,2,2,3,3,0,0,0),
                              warn_missing = TRUE) #redefining values
table(DHS1999$age_grp2) #checking mapped values

#2005-06 data
DHS2005$age_grp2 <- mapvalues(DHS2005$v012,c(15,16,17,18,19,20,21,22,23,24),c(1,1,1,2,2,3,3,0,0,0),
                              warn_missing = TRUE) #redefining values
table(DHS2005$age_grp2) #checking mapped values

#2010-11 data 
DHS2010$age_grp2  <- mapvalues(DHS2010$v012,c(15,16,17,18,19,20,21,22,23,24),c(1,1,1,2,2,3,3,0,0,0),
                               warn_missing = TRUE) #redefining values
table(DHS2010$age_grp2) #checking mapped values

#2015 data
DHS2015$age_grp2 <- mapvalues(DHS2015$v012,c(15,16,17,18,19,20,21,22,23,24),c(1,1,1,2,2,3,3,0,0,0),
                              warn_missing = TRUE) #redefining values
table(DHS2015$age_grp2) #checking mapped values


#Define survey design
library(survey) #load package

#1988 data
DHS1988_Design <- svydesign(DHS1988$ccluster,strata = DHS1988$sstrata,
                            weights=DHS1988$weight,data=DHS1988,nest = TRUE )
#1994 data
DHS1994_Design <- svydesign(DHS1994$ccluster,strata = DHS1994$sstrata,
                            weights=DHS1994$weight,data=DHS1994,nest = TRUE )
#1999 data
DHS1999_Design <- svydesign(DHS1999$ccluster,strata = DHS1999$sstrata,
                            weights=DHS1999$weight,data=DHS1999,nest = TRUE )
#2005 data
DHS2005_Design <- svydesign(DHS2005$ccluster,strata = DHS2005$sstrata,
                            weights=DHS2005$weight,data=DHS2005,nest = TRUE )
#2010 data
DHS2010_Design <- svydesign(DHS2010$ccluster,strata = DHS2010$sstrata,
                            weights=DHS2010$weight,data=DHS2010,nest = TRUE )
#2015 data
DHS2015_Design <- svydesign(DHS2015$ccluster,strata = DHS2015$sstrata,
                            weights=DHS2015$weight,data=DHS2015,nest = TRUE )


#*********************************#
#CHECK VARIABLE CORRELATION       #
#*********************************#
#Install package
# install.packages("devtools")
# devtools::install_github("jacob-long/jtools")

library(jtools) #load package

#Correlation matrix
#1988
svycor(~fp_use+age_grp+region+residence+edu_level
       +v714+mrt_status+parity+fp_knowledge,DHS1988_Design)

#1994
svycor(~fp_use+age_grp+region+residence+edu_level
       +mrt_status+parity+fp_knowledge+kids_desire,DHS1994_Design)

#1999
svycor(~fp_use+age_grp+region+residence+edu_level
       +mrt_status+parity+fp_knowledge+kids_desire
       +v714,DHS1999_Design)

#2005
svycor(~fp_use+age_grp+region+residence+edu_level
       +mrt_status+parity+fp_knowledge,DHS2005_Design)

#2010
svycor(~fp_use+age_grp+region+residence+edu_level
       +mrt_status+parity+fp_knowledge+v714+kids_desire
       ,DHS2010_Design)
#2015
svycor(~fp_use+age_grp+region+residence+edu_level
       +mrt_status+parity+fp_knowledge+v714+kids_desire
       ,DHS2015_Design)


#*********************************#
# FIT SINGLE MODELS               #
#*********************************#

# #Age group
x1.1 <- svyglm(as.factor(fp_use)~ relevel(as.factor(age_grp),ref="3") ,DHS1988_Design, family="binomial") #age group
exp(x1.1$coefficients)
summary(x1.1)

x2.1 <- svyglm(as.factor(fp_use)~ relevel(as.factor(age_grp), ref="3") ,DHS1994_Design, family="binomial") #age group
exp(x2.1$coefficients)
summary(x2.1)

x3.1 <- svyglm(as.factor(fp_use)~ relevel(as.factor(age_grp),ref="3") ,DHS1999_Design, family="binomial") #age group
exp(x3.1$coefficients)
summary(x3.1)

x4.1 <- svyglm(as.factor(fp_use)~ relevel(as.factor(age_grp),ref="3") ,DHS2005_Design, family="binomial") #age group
exp(x4.1$coefficients)
summary(x4.1)

x5.1 <- svyglm(as.factor(fp_use)~ relevel(as.factor(age_grp),ref="3") ,DHS2010_Design, family="binomial") #age group
exp(x5.1$coefficients)
summary(x5.1)

x6.1 <- svyglm(as.factor(fp_use)~ relevel(as.factor(age_grp),ref="3") ,DHS2015_Design, family="binomial") #age group
exp(x6.1$coefficients)
summary(x6.1)

#Region
x1.3 <- svyglm(as.factor(fp_use)~as.factor(region) ,DHS1988_Design, family="binomial") #region
as.data.frame(exp(x1.3$coefficients))
summary(x1.3)

x2.3 <- svyglm(as.factor(fp_use)~as.factor(region) ,DHS1994_Design, family="binomial") #region
as.data.frame(exp(x2.3$coefficients))
summary(x2.3)

x3.3 <- svyglm(as.factor(fp_use)~as.factor(region) ,DHS1999_Design, family="binomial") #region
as.data.frame(exp(x3.3$coefficients))
summary(x3.3)

x4.3 <- svyglm(as.factor(fp_use)~as.factor(region) ,DHS2005_Design, family="binomial") #region
as.data.frame(exp(x4.3$coefficients))
summary(x4.3)

x5.3 <- svyglm(as.factor(fp_use)~as.factor(region) ,DHS2010_Design, family="binomial") #region
as.data.frame(exp(x5.3$coefficients))
summary(x5.3)

x6.3 <- svyglm(as.factor(fp_use)~as.factor(region) ,DHS2015_Design, family="binomial") #region
as.data.frame(exp(x6.3$coefficients))
summary(x6.3)

#Desire for more children
x1.7 <- svyglm(as.factor(fp_use)~as.factor(kids_desire) ,DHS1988_Design, family="binomial") #Desire for more children
exp(x1.7$coefficients)
summary(x1.7)

x2.7 <- svyglm(as.factor(fp_use)~as.factor(kids_desire) ,DHS1994_Design, family="binomial") #Desire for more children
exp(x2.7$coefficients)
summary(x2.7)

x3.7 <- svyglm(as.factor(fp_use)~as.factor(kids_desire) ,DHS1999_Design, family="binomial") #Desire for more children
exp(x3.7$coefficients)
summary(x3.7)

x4.7 <- svyglm(as.factor(fp_use)~as.factor(kids_desire) ,DHS2005_Design, family="binomial") #Desire for more children
exp(x4.7$coefficients)
summary(x4.7)

x5.7 <- svyglm(as.factor(fp_use)~as.factor(kids_desire) ,DHS2010_Design, family="binomial") #Desire for more children
exp(x5.7$coefficients)
summary(x5.7)

x6.7 <- svyglm(as.factor(fp_use)~as.factor(kids_desire) ,DHS2015_Design, family="binomial") #Desire for more children
exp(x6.7$coefficients)
summary(x6.7)

#Residence
x1.2 <- svyglm(as.factor(fp_use)~as.factor(residence) ,DHS1988_Design, family="binomial") #residence
exp(x1.2$coefficients)
summary(x1.2)

x2.2 <- svyglm(as.factor(fp_use)~as.factor(residence) ,DHS1994_Design, family="binomial") #residence
exp(x2.2$coefficients)
summary(x2.2)

x3.2 <- svyglm(as.factor(fp_use)~as.factor(residence) ,DHS1999_Design, family="binomial") #residence
exp(x3.2$coefficients)
summary(x3.2)

x4.2 <- svyglm(as.factor(fp_use)~as.factor(residence) ,DHS2005_Design, family="binomial") #residence
exp(x4.2$coefficients)
summary(x4.2)

x5.2 <- svyglm(as.factor(fp_use)~as.factor(residence) ,DHS2010_Design, family="binomial") #residence
exp(x5.2$coefficients)
summary(x5.2)

x6.2 <- svyglm(as.factor(fp_use)~as.factor(residence) ,DHS2015_Design, family="binomial") #residence
exp(x6.2$coefficients)
summary(x6.2)

#Education
x1.4 <- svyglm(as.factor(fp_use)~as.factor(edu_level) ,DHS1988_Design, family="binomial") #education
as.data.frame(exp(x1.4$coefficients))
summary(x1.4)

x2.4 <- svyglm(as.factor(fp_use)~as.factor(edu_level) ,DHS1994_Design, family="binomial") #education
exp(x2.4$coefficients)
summary(x2.4)

x3.4 <- svyglm(as.factor(fp_use)~as.factor(edu_level) ,DHS1999_Design, family="binomial") #education
exp(x3.4$coefficients)
summary(x3.4)

x4.4 <- svyglm(as.factor(fp_use)~as.factor(edu_level) ,DHS2005_Design, family="binomial") #education
exp(x4.4$coefficients)
summary(x4.4)

x5.4 <- svyglm(as.factor(fp_use)~as.factor(edu_level) ,DHS2010_Design, family="binomial") #education
exp(x5.4$coefficients)
summary(x5.4)

x6.4 <- svyglm(as.factor(fp_use)~as.factor(edu_level) ,DHS2015_Design, family="binomial") #education
exp(x6.4$coefficients)
summary(x6.4)

#Employment
x1.6 <- svyglm(as.factor(fp_use)~as.factor(v714) ,DHS1988_Design, family="binomial") #employment status
exp(x1.6$coefficients)
summary(x1.6)

x2.6 <- svyglm(as.factor(fp_use)~as.factor(v714) ,DHS1994_Design, family="binomial") #employment status
exp(x2.6$coefficients)
summary(x2.6)

x3.6 <- svyglm(as.factor(fp_use)~as.factor(v714) ,DHS1999_Design, family="binomial") #employment status
exp(x3.6$coefficients)
summary(x3.6)

x4.6 <- svyglm(as.factor(fp_use)~as.factor(v714) ,DHS2005_Design, family="binomial") #employment status
exp(x4.6$coefficients)
summary(x4.6)

x5.6 <- svyglm(as.factor(fp_use)~as.factor(v714) ,DHS2010_Design, family="binomial") #employment status
exp(x5.6$coefficients)
summary(x5.6)

x6.6 <- svyglm(as.factor(fp_use)~as.factor(v714) ,DHS2015_Design, family="binomial") #employment status
exp(x6.6$coefficients)
summary(x6.6)

#Marital status
x1.5 <- svyglm(as.factor(fp_use)~as.factor(mrt_status),DHS1988_Design, family="binomial") #education
exp(x1.5$coefficients)
summary(x1.5)

x2.5 <- svyglm(as.factor(fp_use)~as.factor(mrt_status) ,DHS1994_Design, family="binomial") #education
exp(x2.5$coefficients)
summary(x2.5)

x3.5 <- svyglm(as.factor(fp_use)~as.factor(mrt_status) ,DHS1999_Design, family="binomial") #education
exp(x3.5$coefficients)
summary(x3.5)

x4.5 <- svyglm(as.factor(fp_use)~as.factor(mrt_status) ,DHS2005_Design, family="binomial") #education
exp(x4.5$coefficients)
summary(x4.5)

x5.5 <- svyglm(as.factor(fp_use)~as.factor(mrt_status) ,DHS2010_Design, family="binomial") #education
exp(x5.5$coefficients)
summary(x5.5)

x6.5 <- svyglm(as.factor(fp_use)~as.factor(mrt_status) ,DHS2015_Design, family="binomial") #education
exp(x6.5$coefficients)
summary(x6.5)

#Parity
x1.9 <- svyglm(as.factor(fp_use)~as.factor(parity) ,DHS1988_Design, family="binomial") #FP knowledge
exp(x1.9$coefficients)  
summary(x1.9)

x2.9 <- svyglm(as.factor(fp_use)~as.factor(parity) ,DHS1994_Design, family="binomial") #FP knowledge
exp(x2.9$coefficients)  
summary(x2.9)

x3.10 <- svyglm(as.factor(fp_use)~as.factor(parity) ,DHS1999_Design, family="binomial") #partners age difference
exp(x3.10$coefficients)
summary(x3.10)

x4.10 <- svyglm(as.factor(fp_use)~as.factor(parity) ,DHS2005_Design, family="binomial") #partners age difference
exp(x4.10$coefficients)
summary(x4.10)

x5.10 <- svyglm(as.factor(fp_use)~as.factor(parity) ,DHS2010_Design, family="binomial") #partners age difference
exp(x5.10$coefficients)
summary(x5.10)

x6.10 <- svyglm(as.factor(fp_use)~as.factor(parity) ,DHS2015_Design, family="binomial") #partners age difference
exp(x6.10$coefficients)
summary(x6.10)

#Knowledge of contraceptive methods
x1.8 <- svyglm(as.factor(fp_use)~as.factor(fp_knowledge) ,DHS1988_Design, family="binomial") #FP knowledge
exp(x1.8$coefficients)  #Highly correlated to fp use
summary(x1.8)

x2.8 <- svyglm(as.factor(fp_use)~as.factor(fp_knowledge) ,DHS1994_Design, family="binomial") #FP knowledge
exp(x2.8$coefficients)  #Highly correlated to fp use
summary(x2.8)


x3.8 <- svyglm(as.factor(fp_use)~as.factor(fp_knowledge) ,DHS1999_Design, family="binomial") #FP knowledge
exp(x3.8$coefficients)  #Highly correlated to fp use
summary(x3.8)


x4.8 <- svyglm(as.factor(fp_use)~as.factor(fp_knowledge) ,DHS2005_Design, family="binomial") #FP knowledge
exp(x4.8$coefficients)  #Highly correlated to fp use
summary(x4.8)


x5.8 <- svyglm(as.factor(fp_use)~as.factor(fp_knowledge) ,DHS2010_Design, family="binomial") #FP knowledge
exp(x5.8$coefficients)  #Highly correlated to fp use
summary(x5.8)


x6.8 <- svyglm(as.factor(fp_use)~as.factor(fp_knowledge) ,DHS2015_Design, family="binomial") #FP knowledge
exp(x6.8$coefficients)  #Highly correlated to fp use
summary(x6.8)


#*********************************#
# FIT MODELS                      #
#*********************************#

#1988
glm1.1 <- svyglm(as.factor(fp_use)~as.factor(age_grp2)+as.factor(residence)
                 +as.factor(edu_level)+as.factor(v714)+as.factor(mrt_status)+
                   as.factor(parity)+as.factor(kids_desire),DHS1988_Design, family="binomial")
as.data.frame(exp(glm1.1$coefficients))
summary(glm1.1)
confint(glm1.1)
exp(confint(glm1.1))
#Error because once all missing values are removed, only married women remain

#1994
glm2.1 <- svyglm(as.factor(fp_use)~as.factor(age_grp2)+as.factor(residence)
                 +as.factor(edu_level)+as.factor(v714)+as.factor(mrt_status)+
                   as.factor(parity)+as.factor(kids_desire),DHS1994_Design, family="binomial")
as.data.frame(exp(glm2.1$coefficients))
summary(glm2.1)
confint(glm2.1)
exp(confint(glm2.1))

#1999
glm3.1 <- svyglm(as.factor(fp_use)~as.factor(age_grp2)+as.factor(residence)
                 +as.factor(edu_level)+as.factor(v714)+as.factor(mrt_status)+
                   as.factor(parity)+as.factor(kids_desire),DHS1999_Design, family="binomial")
as.data.frame(exp(glm3.1$coefficients))
summary(glm3.1)
confint(glm3.1)


#2005
glm4.1 <- svyglm(as.factor(fp_use)~as.factor(age_grp2)+as.factor(residence)
                 +as.factor(edu_level)+as.factor(v714)+as.factor(mrt_status)+
                   as.factor(parity)+as.factor(kids_desire),DHS2005_Design, family="binomial")
as.data.frame(exp(glm4.1$coefficients))
summary(glm4.1)
confint(glm4.1)

#2010
glm5.1 <- svyglm(as.factor(fp_use)~as.factor(age_grp2)+as.factor(residence)
                 +as.factor(edu_level)+as.factor(v714)+as.factor(mrt_status)+
                   as.factor(parity)+as.factor(kids_desire),DHS2010_Design, family="binomial")
as.data.frame(exp(glm5.1$coefficients))
summary(glm5.1)
confint(glm5.1)

#2015
glm6.1 <- svyglm(as.factor(fp_use)~as.factor(age_grp2)+as.factor(residence)
                 +as.factor(edu_level)+as.factor(v714)+as.factor(mrt_status)+
                   as.factor(parity)+as.factor(kids_desire),DHS2015_Design, family="binomial")
as.data.frame(exp(glm6.1$coefficients))
summary(glm6.1)
exp(confint(glm6.1))

AIC(glm1.1)
AIC(glm2.1)
AIC(glm3.1)
AIC(glm4.1)
AIC(glm5.1)
AIC(glm6.1)


#*************************************************#
#Check 2015 data                                  #
#*************************************************#

#2015
sum(svytable(~as.factor(edu_level),DHS2015_Design))#frequency 
svytable(~v012+as.factor(edu_level),DHS2015_Design)
prop.table(svytable(~as.factor(age_grp),DHS2015_Design))*100

#Distribution by age and education
svytable(~v012+as.factor(edu_level),DHS2015_Design)
prop.table(svytable(~v012+as.factor(edu_level),DHS2015_Design),1)*100

#Contraceptive use among those with none/primary education
svytable(~v012+as.factor(fp_use),subset(DHS2015_Design,edu_level==0))
prop.table(svytable(~v012+as.factor(fp_use),subset(DHS2015_Design,edu_level==0)),1)*100#proportions

#Contraceptive use among those with secondary/higher education
svytable(~v012+as.factor(fp_use),subset(DHS2015_Design,edu_level==1))
prop.table(svytable(~v012+as.factor(fp_use),subset(DHS2015_Design,edu_level==1)),1)*100#proportions

#Contraceptive use by education
svytable(~as.factor(edu_level),DHS2015_Design)
svytable(~as.factor(fp_use)+as.factor(edu_level),DHS2015_Design)
prop.table(svytable(~as.factor(fp_use)+as.factor(edu_level),DHS2015_Design),2)*100

#Age by education
svytable(~v012+as.factor(edu_level),DHS2015_Design)
svytable(~v012+as.factor(v106),DHS2015_Design)
svytable(~as.factor(fp_use)+as.factor(edu_level),DHS2015_Design)
prop.table(svytable(~as.factor(fp_use)+as.factor(edu_level),DHS2015_Design),2)*100

prop.table(svytable(~as.factor(v012)+as.factor(fp_use),subset(DHS2015_Design,region==0)))*100



#****************************************#
# AVERAGE MARGINAL EFFECTS               #
#****************************************#

library(margins) #load package 
summary(margins(glm2.1))  #1994
summary(margins(glm3.1))  #1999
summary(margins(glm4.1))  #2005
summary(margins(glm5.1))  #2010
summary(margins(glm6.1))  #2015

#Plot margins
library(ggplot2)

#-------------------------------#
#Age group
#Create data frame
AGE_CP <- rep(c("15-17","18-19","20-21"),4)
YEAR1 <- c(rep("1994",3),rep("1999",3),
           rep("2005-06",3),rep("2010-11",3))
Prop_agecp <- c(-0.0886,-0.0409,-0.0024,  -0.1100,-0.0417,-0.0404,
                -0.1088,-0.0311,-0.0218,  -0.1248,-0.1433,-0.0457)

data_agecp <- data.frame(YEAR1,AGE_CP,Prop_agecp)
data_agecp

#Plot 
ggplot(data_agecp, aes(fill=AGE_CP,x=YEAR1, y=Prop_agecp)) +
  
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 1.0)+
  labs(x = "Survey year",y = "Average Marginal Effects (AME)", title = "Age group",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))


#-------------------------------#
#Residence
#Create data frame
Residence <- rep("Rural",4)
YEAR2 <- c("1994","1999","2005-06","2010-11")
Prop_res <- c(-0.1479,-0.1560,-0.1057,-0.0183)

data_res <- data.frame(YEAR2,Residence,Prop_res)
data_res

#Plot 
ggplot(data_res, aes(fill=Residence,x=YEAR2, y=Prop_res)) +
  
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.3, end = 0.8)+
  labs(x = "Survey year",y = "Average Marginal Effects (AME)", title = "Residence",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))


#-------------------------------#
#Education
#Create data frame
Education <- rep("Secondary/Higher",4)
YEAR2 <- c("1994","1999","2005-06","2010-11")
Prop_edu <- c(0.0551,0.0554,0.0394,0.0336)

data_edu <- data.frame(YEAR2,Education,Prop_edu)
data_edu

#Plot 
ggplot(data_edu, aes(fill=Education,x=YEAR2, y=Prop_edu)) +
  scale_y_continuous(breaks = seq(0, 0.06, by = 0.02))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.5, end = 0.8)+
  labs(x = "Survey year",y = "Average Marginal Effects (AME)", title = "Education level",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))



#-------------------------------#
#Employment status
#Create data frame
Employment <- rep("Employed",4)
YEAR2 <- c("1994","1999","2005-06","2010-11")
Prop_emp <- c(0.1194,0.0146,0.0608,0.0686)

data_emp <- data.frame(YEAR2,Employment,Prop_emp)
data_emp

#Plot 
ggplot(data_emp, aes(fill=Employment,x=YEAR2, y=Prop_emp)) +
  
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.5, end = 0.8)+
  labs(x = "Survey year",y = "Average Marginal Effects (AME)", title = "Employment status",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))


#-------------------------------#
#Parity
#Create data frame
Parity <- rep("\u2265 1",4)
YEAR2 <- c("1994","1999","2005-06","2010-11")
Prop_prty <- c(0.2236,0.2424,0.3452,0.2993)

data_prty <- data.frame(YEAR2,Parity,Prop_prty)
data_prty

#Plot 
ggplot(data_prty, aes(fill=Parity,x=YEAR2, y=Prop_prty)) +
  
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.5, end = 0.8)+
  labs(x = "Survey year",y = "Average Marginal Effects (AME)", title = "Parity",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))



#-------------------------------#
#Marital status
#Create data frame
Mrtstatus <- rep(c("Married","Cohabitating","Widowed/\nDivorced/\nSeparated"),4)
YEAR1 <- c(rep("1994",3),rep("1999",3),
           rep("2005-06",3),rep("2010-11",3))
Prop_mrt <- c(0.2092,0,0.0472,  0.2725,0.1685,-0.0267,
              0.2648,0.2487,-0.0912,  0.2253,0.1930,-0.1371)

data_mrt <- data.frame(YEAR1,Mrtstatus,Prop_mrt)
data_mrt

#Plot 
ggplot(data_agecp, aes(fill=Mrtstatus,x=YEAR1, y=Prop_mrt)) +
  
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 1.0)+
  labs(x = "Survey year",y = "Average Marginal Effects (AME)", title = "Marital status",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))



#-------------------------------#
#Desire for more children
#Create data frame
Kids_desire <- rep(c("Within\n2 years","After\n2 years","Undecided/Unsure"),4)
YEAR1 <- c(rep("1994",3),rep("1999",3),
           rep("2005-06",3),rep("2010-11",3))
Prop_kdsdesire <- c(-0.2655,0.0192,-0.1245,  -0.2509,0.0624,-0.1043,
                    -0.1460,0.1069,-0.0299,   -0.2131,0.0799,-0.0532)

data_kdsdesire <- data.frame(YEAR1,Kids_desire,Prop_kdsdesire)
data_kdsdesire

#Plot 
ggplot(data_kdsdesire, aes(fill=Kids_desire,x=YEAR1, y=Prop_kdsdesire)) +
  
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 1.0)+
  labs(x = "Survey year",y = "Average Marginal Effects (AME)", title = "Desire for more children",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))


#========================================================================================#

#========================================================================================#
#                       MULTINOMIAL LOGISTIC REGRESSION

#Clear memory
rm(list=ls())

#----------------------#
#Set working directory
setwd("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes")

#Read data into R
DHS1988 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS1988_MC.csv")[,-1]
DHS1994 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS1994_MC.csv")[,-1]   
DHS1999 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS1999_MC.csv")[,-1]
DHS2005 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS2005_MC.csv")[,-1]
DHS2010 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS2010_MC.csv")[,-1]
DHS2015 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS2015_MC.csv")[,-1]

#----------------------------#
#Define survey designs
library(survey) #load package
DHS1988_Design <- svydesign(DHS1988$ccluster,strata = DHS1988$sstrata,
                               weights=DHS1988$weight,data=DHS1988,nest = TRUE )
DHS1994_Design <- svydesign(DHS1994$ccluster,strata = DHS1994$sstrata,
                               weights=DHS1994$weight,data=DHS1994,nest = TRUE )
DHS1999_Design <- svydesign(DHS1999$ccluster,strata = DHS1999$sstrata,
                               weights=DHS1999$weight,data=DHS1999,nest = TRUE )
DHS2005_Design <- svydesign(DHS2005$ccluster,strata = DHS2005$sstrata,
                               weights=DHS2005$weight,data=DHS2005,nest = TRUE )
DHS2010_Design <- svydesign(DHS2010$ccluster,strata = DHS2010$sstrata,
                               weights=DHS2010$weight,data=DHS2010,nest = TRUE )
DHS2015_Design <- svydesign(DHS2015$ccluster,strata = DHS2015$sstrata,
                               weights=DHS2015$weight,data=DHS2015,nest = TRUE )

#--------------------------------#
#Install packages
#install.packages("remotes")  #install remotes package
#remotes::install_github("carlganz/svrepmisc", force=TRUE)  #install svrepmisc package

#Note: If multinomial regression packages are not installing, install R tools


#--------------------------------#
#Define survey design for multinomial regression
library(svrepmisc) #load package
DHS1988_Rep <- as.svrepdesign(DHS1988_Design)  #1988 data
DHS1994_Rep <- as.svrepdesign(DHS1994_Design)  #1994 data
DHS1999_Rep <- as.svrepdesign(DHS1999_Design)  #1999 data
DHS2005_Rep <- as.svrepdesign(DHS2005_Design)  #2005 data
DHS2010_Rep <- as.svrepdesign(DHS2010_Design)  #2010 data
DHS2015_Rep <- as.svrepdesign(DHS2015_Design)  #2015 data

#Correlation matrix
library(jtools) #load package
#1988
svycor(~method_choice+age_grp+region+residence+edu_level
       +v714+mrt_status+parity+fp_knowledge,DHS1988_Design)

#1994
svycor(~method_choice+age_grp+region+residence+edu_level
       +mrt_status+parity+fp_knowledge+kids_desire,DHS1994_Design)

#1999
svycor(~method_choice+age_grp+region+residence+edu_level
       +mrt_status+parity+fp_knowledge+kids_desire
       +v714,DHS1999_Design)

#2005
svycor(~method_choice+age_grp+region+residence+edu_level
       +mrt_status+parity+fp_knowledge,DHS2005_Design)

#2010
svycor(~method_choice+age_grp+region+residence+edu_level
       +mrt_status+parity+fp_knowledge+v714+kids_desire
       ,DHS2010_Design)
#2015
svycor(~method_choice+age_grp+region+residence+edu_level
       +mrt_status+parity+fp_knowledge+v714+kids_desire
       ,DHS2015_Design)


#--------------------------------------------------#
#Fit single models

#1988
x1.1 <- svymultinom(method_choice~as.factor(age_grp),DHS1988_Rep)
exp(x1.1[c(1:16)])
x1.2 <- svymultinom(method_choice~as.factor(residence),DHS1988_Rep)
exp(x1.2[c(1:8)])
x1.3 <- svymultinom(method_choice~as.factor(region),DHS1988_Rep)
exp(x1.3[c(1:40)])
x1.4 <- svymultinom(method_choice~as.factor(edu_level),DHS1988_Rep)
exp(x1.4[c(1:12)])
x1.5 <- svymultinom(method_choice~as.factor(v714),DHS1988_Rep)
exp(x1.5[c(1:8)])
x1.6 <- svymultinom(method_choice~as.factor(kids_desire),DHS1988_Rep)
exp(x1.6[c(1:12)])

#1994
x2.1 <- svymultinom(method_choice~as.factor(age_grp),DHS1994_Rep)
exp(x2.1[c(1:16)])
x2.2 <- svymultinom(method_choice~as.factor(residence),DHS1994_Rep)
exp(x2.2[c(1:8)])
x2.3 <- svymultinom(method_choice~as.factor(region),DHS1994_Rep)
exp(x2.3[c(1:40)])
x2.4 <- svymultinom(method_choice~as.factor(edu_level),DHS1994_Rep)
exp(x2.4[c(1:12)])
x2.5 <- svymultinom(method_choice~as.factor(v714),DHS1994_Rep)
exp(x2.5[c(1:8)])
x2.6 <- svymultinom(method_choice~as.factor(kids_desire),DHS1994_Rep)
exp(x2.6[c(1:12)])

#1999
x3.1 <- svymultinom(method_choice~as.factor(age_grp),DHS1999_Rep)
exp(x3.1[c(1:16)])
x3.2 <- svymultinom(method_choice~as.factor(residence),DHS1999_Rep)
exp(x3.2[c(1:8)])
x3.3 <- svymultinom(method_choice~as.factor(region),DHS1999_Rep)
exp(x3.3[c(1:40)])
x3.4 <- svymultinom(method_choice~as.factor(edu_level),DHS1999_Rep)
exp(x3.4[c(1:12)])
x3.5 <- svymultinom(method_choice~as.factor(v714),DHS1999_Rep)
exp(x3.5[c(1:8)])
x3.6 <- svymultinom(method_choice~as.factor(kids_desire),DHS1999_Rep)
exp(x3.6[c(1:12)])


#2005
x4.1 <- svymultinom(method_choice~as.factor(age_grp),DHS2005_Rep)
exp(x4.1[c(1:16)])
x4.2 <- svymultinom(method_choice~as.factor(residence),DHS2005_Rep)
exp(x4.2[c(1:8)])
x4.3 <- svymultinom(method_choice~as.factor(region),DHS2005_Rep)
exp(x4.3[c(1:40)])
x4.4 <- svymultinom(method_choice~as.factor(edu_level),DHS2005_Rep)
exp(x4.4[c(1:12)])
x4.5 <- svymultinom(method_choice~as.factor(v714),DHS2005_Rep)
exp(x4.5[c(1:8)])
x4.6 <- svymultinom(method_choice~as.factor(kids_desire),DHS2005_Rep)
exp(x4.6[c(1:12)])


#2010
x5.1 <- svymultinom(method_choice~as.factor(age_grp),DHS2010_Rep)
exp(x5.1[c(1:16)])
x5.2 <- svymultinom(method_choice~as.factor(residence),DHS2010_Rep)
exp(x5.2[c(1:8)])
x5.3 <- svymultinom(method_choice~as.factor(region),DHS2010_Rep)
exp(x5.3[c(1:40)])
x5.4 <- svymultinom(method_choice~as.factor(edu_level),DHS2010_Rep)
exp(x5.4[c(1:12)])
x5.5 <- svymultinom(method_choice~as.factor(v714),DHS2010_Rep)
exp(x5.5[c(1:8)])
x5.6 <- svymultinom(method_choice~as.factor(kids_desire),DHS2010_Rep)
exp(x5.6[c(1:12)])


#2015
x6.1 <- svymultinom(method_choice~as.factor(age_grp),DHS2015_Rep)
exp(x6.1[c(1:16)])
x6.2 <- svymultinom(method_choice~as.factor(residence),DHS2015_Rep)
exp(x6.2[c(1:8)])
x6.3 <- svymultinom(method_choice~as.factor(region),DHS2015_Rep)
exp(x6.3[c(1:40)])
x6.4 <- svymultinom(method_choice~as.factor(edu_level),DHS2015_Rep)
exp(x6.4[c(1:12)])
x6.5 <- svymultinom(method_choice~as.factor(v714),DHS2015_Rep)
exp(x6.5[c(1:8)])
x6.6 <- svymultinom(method_choice~as.factor(kids_desire),DHS2015_Rep)
exp(x6.6[c(1:12)])


#Fit models
#1988
mlr2.1 <- svymultinom(method_choice~as.factor(residence)+as.factor(v714)
                      +as.factor(mrt_status),DHS1988_Rep)
as.data.frame(exp(mlr2.1[c(1:12)]))#odds
mlr2.1 

#1994
mlr2.2 <- svymultinom(method_choice~as.factor(residence)+as.factor(v714)
                      +as.factor(mrt_status),DHS1994_Rep)
as.data.frame(exp(mlr2.2[c(1:12)]))#odds
mlr2.2 

#1999
mlr2.3 <- svymultinom(method_choice~as.factor(residence)+as.factor(v714)
                      +as.factor(mrt_status),DHS1999_Rep)
as.data.frame(exp(mlr2.3[c(1:12)]))#odds
mlr2.3 

#2005 
mlr2.4 <- svymultinom(method_choice~as.factor(residence)+as.factor(v714)
                      +as.factor(mrt_status),DHS2005_Rep)
as.data.frame(exp(mlr2.4[c(1:16)]))#odds
mlr2.4 

#2010
mlr2.5 <- svymultinom(method_choice~as.factor(residence)+as.factor(v714)
                      +as.factor(mrt_status),DHS2010_Rep)
as.data.frame(exp(mlr2.5[c(1:16)]))#odds
mlr2.5 

#2015
mlr2.6 <- svymultinom(method_choice~as.factor(residence)+as.factor(v714)
                      +as.factor(mrt_status),DHS2015_Rep)
as.data.frame(exp(mlr2.6[c(1:16)]))#odds
mlr2.6 



#========================================================================================#


##########################################################################################
#                           END                                                          #
##########################################################################################
