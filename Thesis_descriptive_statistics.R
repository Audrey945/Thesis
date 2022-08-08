##########################################################################################
#                           UNIVERSITY OF CAPE TOWN                                      #
#                           STUDENT NAME:   AUDREY MOYO                                  #
#                                DESCRIPTIVE STATISTICS                                  #
#========================================================================================#
#THESIS TOPIC: Trends and determinants of contraceptive use and method choice among young# 
#              Zimbabwean women from 1988 to 2015                                        #               
##########################################################################################

#========================================================================================#
#                        DATA DESCRIPTION

#Clear memory
rm(list=ls())

#----------------------#
#Set working directory
setwd("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes")

#-----------------------#
#Read data into R

#Study population data
stdy_pop1988 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop1988.csv")[,-1] #1988 data
stdy_pop1994 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop1994.csv")[,-1] #1994 data
stdy_pop1999 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop1999.csv")[,-1] #1999 data
stdy_pop2005 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop2005.csv")[,-1] #2005 data
stdy_pop2010 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop2010.csv")[,-1] #2010 data
stdy_pop2015 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop2015.csv")[,-1] #2015 data

#sample data
DHS1988 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1988.csv")[,-1]
DHS1994 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1994.csv")[,-1]
DHS1999 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1999.csv")[,-1]
DHS2005 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2005.csv")[,-1]
DHS2010 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2010.csv")[,-1]
DHS2015 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2015.csv")[,-1]

# MICS vs DHS data
Stdy_MICs2009 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/Sty_MICs2009.csv")[,-1] #MICS 2009 - study pop
Stdy_MICs2014 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/Sty_MICs2014.csv")[,-1] #MICS 2014 - study pop
MICS2009 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/MICs2009.csv")  #MICS 2009 - sample
MICS2014 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/MICs2014.csv")  #MICS 2014 - sample
DHS2010_COMP <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/DHS2010_COMP.csv") #DHS 2010- sample
DHS2015_COMP <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/DHS2015_COMP.csv") #DHS 2015- sample

#---------------------------------#
#DEFINE SURVERY DESIGNS           
library(survey) #load package

#Study population
#1988 data
stdy_pop1988_Design <- svydesign(stdy_pop1988$ccluster,strata = stdy_pop1988$sstrata,
                                 weights=stdy_pop1988$weight,data=stdy_pop1988,nest = TRUE )
#1994 data
stdy_pop1994_Design <- svydesign(stdy_pop1994$ccluster,strata = stdy_pop1994$sstrata,
                                 weights=stdy_pop1994$weight,data=stdy_pop1994,nest = TRUE )
#1999 data
stdy_pop1999_Design <- svydesign(stdy_pop1999$ccluster,strata = stdy_pop1999$sstrata,
                                 weights=stdy_pop1999$weight,data=stdy_pop1999,nest = TRUE )
#2005 data
stdy_pop2005_Design <- svydesign(stdy_pop2005$ccluster,strata = stdy_pop2005$sstrata,
                                 weights=stdy_pop2005$weight,data=stdy_pop2005,nest = TRUE )
#2010 data
stdy_pop2010_Design <- svydesign(stdy_pop2010$ccluster,strata = stdy_pop2010$sstrata,
                                 weights=stdy_pop2010$weight,data=stdy_pop2010,nest = TRUE )
#2015 data
stdy_pop2015_Design <- svydesign(stdy_pop2015$ccluster,strata = stdy_pop2015$sstrata,
                                 weights=stdy_pop2015$weight,data=stdy_pop2015,nest = TRUE )



#Selected sample
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


#MICS AND DHS data

#study population
#2009 data
MICs2009_stdy_Design <- svydesign(id=Stdy_MICs2009$wm1,strata = Stdy_MICs2009$stratum,
                                  weights =Stdy_MICs2009$wmweight,data=Stdy_MICs2009 )
#2014 data
MICs2014_stdy_Design <- svydesign(id=Stdy_MICs2014$WM1,strata = Stdy_MICs2014$stratum,
                                  weights =Stdy_MICs2014$wmweight,data=Stdy_MICs2014 )
#sample
#2009 data
MICS2009_Design <- svydesign(id=MICS2009$wm1,strata = MICS2009$stratum,
                             weights =MICS2009$wmweight,data=MICS2009 )
#2014
MICS2014_Design <- svydesign(id=MICS2014$WM1,strata = MICS2014$stratum,
                             weights =MICS2014$wmweight,data=MICS2014 )
#2010 data
COMP_DHS2010_Design <- svydesign(DHS2010_COMP$ccluster,strata = DHS2010_COMP$sstrata,
                                 weights=DHS2010_COMP$weight,data=DHS2010_COMP,nest = TRUE )
#2015 data
COMP_DHS2015_Design <- svydesign(DHS2015_COMP$ccluster,strata = DHS2015_COMP$sstrata,
                                 weights=DHS2015_COMP$weight,data=DHS2015_COMP,nest = TRUE )


#--------------------------------------------------------------------------------------#
#DATA DISTRIBUTION BY AGE         #

#Study population 
svytable(~v012,stdy_pop1988_Design) #1988 data
svytable(~v012,stdy_pop1994_Design) #1994 data
svytable(~v012,stdy_pop1999_Design) #1999 data
svytable(~v012,stdy_pop2005_Design) #2005 data
svytable(~v012,stdy_pop2010_Design) #2010 data
svytable(~v012,stdy_pop2015_Design) #2015 data

#Sample population
svytable(~v012,DHS1988_Design)  #1988 data
svytable(~v012,DHS1994_Design) #1994 data
svytable(~v012,DHS1999_Design) #1999 data
svytable(~v012,DHS2005_Design) #2005 data
svytable(~v012,DHS2010_Design) #2010 data
svytable(~v012,DHS2015_Design) #2015 data

#-----------------#
#Plot graphs 

library(ggplot2) #load package

#1988 data
Nn_1988 <- c(229,195,180,215,202,195,142,194,135,174,
             18,33,52,70,94,116,91,152,107,141 ) #create number of women variable
Population <- c(rep("N",10),rep("n",10))#Create population variable
age <- rep(15:24,2) #Create age variable
data_Nn1988 <- data.frame(age,Nn_1988,Population) #Create data frame
data_Nn1988
#Plot line graph
m1<-ggplot(data_Nn1988, aes(x=age, y=Nn_1988,group=Population)) +
  scale_x_continuous(breaks = seq(15, 24, by = 1))+
  scale_y_continuous(breaks = seq(0, 250, by = 50))+
  geom_line(aes( linetype = Population),size = 0.75)+
  labs(x = "Age",y = "Number of women",title = "1988" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        legend.title = element_blank())
m1

#---------------------------------#
#1994 data
Nn_1994 <- c(310.4276,305.0822,310.0869,297.1096,248.9433,293.9886, 
             275.4526,263.7954,201.2950,234.8660,16.81770,46.68876,55.62485,
             111.26469,115.01651,172.62507,187.36186,192.48156,143.48460,189.98232)
data_Nn1994 <- data.frame(age,Nn_1994,Population) #Create data frame
data_Nn1994
#Plot line graph
m2<-ggplot(data_Nn1994, aes(x=age, y=Nn_1994,group=Population)) +
  scale_x_continuous(breaks = seq(15, 24, by = 1))+
  scale_y_continuous(breaks = seq(0, 350, by = 75))+
  geom_line(aes( linetype = Population),size = 0.75)+
  labs(x = "Age",y = "Number of women",title = "1994")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        legend.title = element_blank())
m2

#---------------------------------#
#1999 data
Nn_1999 <- c(261.0990,348.2150,306.4915,253.6360,277.2071,294.2063,265.8755,267.8986,
             248.1764,218.0405,26.68487,43.70480,79.77145,97.34243,143.46590,181.43492,
             200.45459,191.52581,187.77257,171.96541)
data_Nn1999 <- data.frame(age,Nn_1999,Population) #Create data frame
data_Nn1999
#Plot line graph
m3<-ggplot(data_Nn1999, aes(x=age, y=Nn_1999,group=Population)) +
  scale_x_continuous(breaks = seq(15, 24, by = 1))+
  scale_y_continuous(breaks = seq(0, 350, by = 75))+
  geom_line(aes( linetype = Population),size = 0.75)+
  labs(x = "Age",y = "Number of women",title = "1999" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        legend.title = element_blank())
m3

#-------------------------------#
#2005 data
Nn_2005 <- c(346.5344,501.7483,384.6581,472.0362,446.5336,365.8405,419.1461,426.9119, 
             433.7403,306.4789,22.47300,64.80984,85.90917,174.51800,192.40685,222.50182,
             300.03724,321.55789,337.73582,258.74462)
data_Nn2005 <- data.frame(age,Nn_2005,Population) #Create data frame
data_Nn2005
#Plot line graph
m4<-ggplot(data_Nn2005, aes(x=age, y=Nn_2005,group=Population)) +
  scale_x_continuous(breaks = seq(15, 24, by = 1))+
  scale_y_continuous(breaks = seq(0, 600, by = 100))+
  geom_line(aes( linetype = Population),size = 0.75)+
  labs(x = "Age",y = "Number of women",title = "2005-06" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        legend.title = element_blank())
m4

#--------------------------------------#
#2010 data
Nn_2010 <- c(380.5829,420.2610,360.4638,394.8971,388.6896,375.5589,399.5770,
             371.3962,338.2553,356.4570,33.58351,54.06267,93.78775,159.58045,
             195.59015,235.76416,258.74449,291.83379,275.42211,283.38248 )
data_Nn2010 <- data.frame(age,Nn_2010,Population)#Create data frame
data_Nn2010
#Plot line graph
m5<-ggplot(data_Nn2010, aes(x=age, y=Nn_2010,group=Population)) +
  scale_x_continuous(breaks = seq(15, 24, by = 1))+
  scale_y_continuous(breaks = seq(0, 600, by = 100))+
  geom_line(aes( linetype = Population),size = 0.75)+
  labs(x = "Age",y = "Number of women",title = "2010-11" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        legend.title = element_blank())
m5


#------------------------------------#
#2015 data
Nn_2015 <- c(487.2540,471.6955,434.6548,383.7090,421.3212,364.6218,335.9449,333.3781,
             353.9131,308.7193,30.97531,75.80657,124.33367,156.06263,221.41075,239.55749,
             225.12186,255.35780,286.71973,256.71315)
data_Nn2015 <- data.frame(age,Nn_2015,Population) #Create data frame
data_Nn2015
#Plot line graph
m6 <-ggplot(data_Nn2015, aes(x=age, y=Nn_2015,group=Population)) +
  scale_x_continuous(breaks = seq(15, 24, by = 1))+
  scale_y_continuous(breaks = seq(0, 600, by = 100))+
  geom_line(aes( linetype = Population),size = 0.75)+
  labs(x = "Age",y = "Number of women",title = "2015" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        legend.title = element_blank())
m6

#-----------------------------------------#
#Plot graphs side by side
library(cowplot)
plot_grid(m1,m2,m3,m4,m5,m6, labels = "",ncol = 3,align = "h")
#----------------------------------------------------------------------------------#


#----------------------------------------#
#DATA DISTRIBUTION BY RESIDENCE    

#Study population
svytable(~residence,stdy_pop1988_Design) #1988 data
svytable(~residence,stdy_pop1994_Design) #1994 data
svytable(~residence,stdy_pop1999_Design) #1999 data
svytable(~residence,stdy_pop2005_Design) #2005 data
svytable(~residence,stdy_pop2010_Design) #2010 data
svytable(~residence,stdy_pop2015_Design) #2015 data

#Plot 
Pop_prop <- c(1204,657,1832.6778,908.3694,1624.628,1116.218,
              2392.488,1711.141,2274.546,1511.593,2443.319,1451.892 )#Create number of women vector 
year <- c(rep("1988",2),rep("1994",2),rep("1999",2),
          rep("2005-06",2),rep("2010-11",2),rep("2015",2))#Create year vector
Residence <- rep(c("Rural","Urban"),6)#Create residence vector
data_res <- data.frame(year,Residence,Pop_prop)#Create data frame
data_res

#Plot
ggplot(data_res, aes(fill=Residence,x=year, y=Pop_prop)) +
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.1, end = 0.6)+
  labs(x = "Survey year",y = "Number of women",fill="",title = "Residence")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9))
#-------------------------------------------------------------------#


#----------------------------------#
#STUDY POPULATION BY EVER HAD SEX #

svytable(~ever_had_sex,stdy_pop1988_Design) #1988 data
svytable(~ever_had_sex,stdy_pop1994_Design) #1994 data
svytable(~ever_had_sex,stdy_pop1999_Design) #1999 data
svytable(~ever_had_sex,stdy_pop2005_Design) #2005-06 data
svytable(~ever_had_sex,stdy_pop2010_Design) #2010-11 data
svytable(~ever_had_sex,stdy_pop2015_Design) #2015 data

#Plot
year <- c(rep("1988",2),rep("1994",2),rep("1999",2),
          rep("2005-06",2),rep("2010-11",2),rep("2015",2))#Create year vector
#Create number of women never had sex vector
evhs_vec <- c(814,1040,1254.362,1485.629,1186.281,1554.565,1783.357,2316.225,
              1559.686,2226.453,1741.316,2153.896)
Ever_had_sex <- rep(c("Never","Yes"),6) #Create residence vector
data_SA<- data.frame(year,Ever_had_sex,evhs_vec)#Create data frame
#Plot
ggplot(data_SA, aes(fill=Ever_had_sex,x=year, y=evhs_vec)) +
  scale_y_continuous(breaks = seq(0, 2500, by = 500))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.1, end = 0.5)+
  labs(x = "Survey year",y = "Number of women",fill="",title = "Ever had sex" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9))
#----------------------------------------------------------------------------------------#

                        #*********************************#
                        #MICS AND DHS                     #
                        #*********************************#

#Study population
svytable(~wm9,MICs2009_stdy_Design) #MICS 2009
svytable(~WB2,MICs2014_stdy_Design)  #MICS 2014
svytable(~v012,stdy_pop2010_Design)  #DHS 2010
svytable(~v012,stdy_pop2015_Design)  #DHS 2015

#Sample
svytable(~wm9,MICS2009_Design)#MICS 2009
svytable(~WM9,MICS2014_Design)#MICS 2014
svytable(~v012,COMP_DHS2010_Design) #DHS 2010
svytable(~v012,COMP_DHS2015_Design) #DHS 2014

#Plot
#MICS 20019
STDY_MICS2009 <- c(517.4805,555.4035,517.4367,503.1079,522.7097,471.8123,465.4005,
                   511.2584,485.5150,478.0991, 502.8630,523.5372,474.2605,450.5691,
                   465.3019,413.7035,404.8872,455.4825,431.6290,414.8009  ) #create frequency vector
#MICS 2014
STDY_MICS2014 <- c(629.6476,632.6763,618.8482,600.2503,623.3142,525.5049,524.1500,491.8315,
                   509.5659,521.4064, 36.21148,100.45985,163.79185,231.00137,361.42456,
                   321.49356,375.80734,377.67191,396.71256,434.80568 ) #create frequency vector
#DHS 2010-11
STDY_DHS2010 <- c(380.5829,420.2610,360.4638,394.8971,388.6896,375.5589,399.5770,371.3962,338.2553,356.4570,
                  375.7819,406.1252,337.5475,358.1835,346.1920,329.6166,342.2352,333.1198,310.1259,308.1535 ) #create frequency vector
#DHS 2015
STDY_DHS2015 <- c(487.2540,471.6955,434.6548,383.7090,421.3212,364.6218,335.9449,333.3781,353.9131,308.7193,
                  32.18856,75.80657,124.33367,156.06263,221.41075,240.22010,226.31358,255.84729,288.27126,256.99867) #create frequency vector

#-------------------------------------#
#Plot population distribution by age
age <- as.character(rep(15:24,2)) #create age vector
Population <- c(rep("N",10),rep("n",10))
DATA_COMP <- data.frame(Population,age,STDY_MICS2009,STDY_DHS2010,STDY_MICS2014,STDY_DHS2015) #create data frame

#Plot 
#MICS2009
p1<-ggplot(DATA_COMP, aes(x=age, y=STDY_MICS2009,group=Population)) +
  scale_y_continuous(breaks = seq(400, 600, by = 50))+
  geom_line(aes( linetype = Population),size = 0.75)+
  labs(x = "Age",y = "Number of women",title = "MICS 2009" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        legend.title = element_blank())
p1

#DHS 2010
p2<-ggplot(DATA_COMP, aes(x=age, y=STDY_DHS2010,group=Population)) +
  scale_y_continuous(breaks = seq(200, 425, by = 25))+
  geom_line(aes( linetype = Population),size = 0.75)+
  labs(x = "Age",y = "Number of women",title = "DHS 2010-11" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        legend.title = element_blank())
p2

#MICS 2014
p3<-ggplot(DATA_COMP, aes(x=age, y=STDY_MICS2014,group=Population)) +
  scale_y_continuous(breaks = seq(0, 700, by = 100))+
  geom_line(aes( linetype = Population),size = 0.75)+
  labs(x = "Age",y = "Number of women",title = "MICS 2014" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        legend.title = element_blank())
p3

#DHS 2015
p4<-ggplot(DATA_COMP, aes(x=age, y=STDY_DHS2015,group=Population)) +
  scale_y_continuous(breaks = seq(0, 500, by = 100))+
  geom_line(aes( linetype = Population),size = 0.75)+
  labs(x = "Age",y = "Number of women",title = "DHS 2015" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        legend.title = element_blank())
p4

#Plot graphs side by side
library(cowplot)
plot_grid(p1,p2,p3,p4, labels = "",ncol = 2,align = "h")
#========================================================================================#


#========================================================================================#
#                     UNIVARIATE DATA ANALYSIS

#Clear memory
rm(list=ls())

#----------------------#
#Set working directory
setwd("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes")

#-----------------------#
#Read data into R
DHS1988 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1988.csv")[,-1]
DHS1994 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1994.csv")[,-1]   
DHS1999 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1999.csv")[,-1]
DHS2005 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2005.csv")[,-1]
DHS2010 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2010.csv")[,-1]
DHS2015 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2015.csv")[,-1]

#----------------------------#
#Define survey designs
library(survey)  #load packages
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


#-----------------------#
#MICS vs DHS

#Read data into R
MICS2009 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/MICs2009.csv")[,-1]
MICS2014 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/MICs2014.csv")[,-1]
DHS2010_comp <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/DHS2010_COMP.csv")[,-1]
DHS2015_comp <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/DHS2015_COMP.csv")[,-1]

#Define survey designs
MICS2009_Design <- svydesign(id=MICS2009$wm1,strata = MICS2009$stratum,
                             weights =MICS2009$wmweight,data=MICS2009 )
MICS2014_Design <- svydesign(id=MICS2014$WM1,strata = MICS2014$stratum,
                             weights =MICS2014$wmweight,data=MICS2014 )
DHS2010_Design_comp <- svydesign(DHS2010_comp$ccluster,strata = DHS2010_comp$sstrata,
                            weights=DHS2010_comp$weight,data=DHS2010_comp,nest = TRUE )
DHS2015_Design_comp <- svydesign(DHS2015_comp$ccluster,strata = DHS2015_comp$sstrata,
                            weights=DHS2015_comp$weight,data=DHS2015_comp,nest = TRUE )



#*********************************#
#BACKGROUND CHARACHERISTICS       #
#*********************************#

#-------------------------------------#
#Age group distribution
#1988
sum(svytable(~as.factor(age_grp),DHS1988_Design))#frequency 
prop.table(svytable(~as.factor(age_grp),DHS1988_Design))*100#proportions 

#1994
sum(svytable(~as.factor(age_grp),DHS1994_Design))#frequency 
prop.table(svytable(~as.factor(age_grp),DHS1994_Design))*100#proportions

#1999
sum(svytable(~as.factor(age_grp),DHS1999_Design))#frequency 
prop.table(svytable(~as.factor(age_grp),DHS1999_Design))*100#proportions

#2005
sum(svytable(~as.factor(age_grp),DHS2005_Design))#frequency 
prop.table(svytable(~as.factor(age_grp),DHS2005_Design))*100#proportions

#2010
sum(svytable(~as.factor(age_grp),DHS2010_Design))#frequency 
prop.table(svytable(~as.factor(age_grp),DHS2010_Design))*100#proportions

#2015
sum(svytable(~as.factor(age_grp),DHS2015_Design))#frequency 
prop.table(svytable(~as.factor(age_grp),DHS2015_Design))*100#proportions

#-------------------------------------#
#Region 
#1988
sum(svytable(~as.factor(region),DHS1988_Design))#frequency 
prop.table(svytable(~as.factor(region),DHS1988_Design))*100#proportions 

#1994
sum(svytable(~as.factor(region),DHS1994_Design))#frequency 
prop.table(svytable(~as.factor(region),DHS1994_Design))*100#proportions

#1999
sum(svytable(~as.factor(region),DHS1999_Design))#frequency 
prop.table(svytable(~as.factor(region),DHS1999_Design))*100#proportions

#2005
sum(svytable(~as.factor(region),DHS2005_Design))#frequency 
prop.table(svytable(~as.factor(region),DHS2005_Design))*100#proportions

#2010
sum(svytable(~as.factor(region),DHS2010_Design))#frequency 
prop.table(svytable(~as.factor(region),DHS2010_Design))*100#proportions

#2015
sum(svytable(~as.factor(region),DHS2015_Design))#frequency 
prop.table(svytable(~as.factor(region),DHS2015_Design))*100#proportions


#-------------------------------------#
#Residence distribution
#1988
sum(svytable(~as.factor(residence),DHS1988_Design))#frequency 
prop.table(svytable(~as.factor(residence),DHS1988_Design))*100#proportions 

#1994
sum(svytable(~as.factor(residence),DHS1994_Design))#frequency 
prop.table(svytable(~as.factor(residence),DHS1994_Design))*100#proportions

#1999
sum(svytable(~as.factor(residence),DHS1999_Design))#frequency 
prop.table(svytable(~as.factor(residence),DHS1999_Design))*100#proportions

#2005
sum(svytable(~as.factor(residence),DHS2005_Design))#frequency 
prop.table(svytable(~as.factor(residence),DHS2005_Design))*100#proportions

#2010
sum(svytable(~as.factor(residence),DHS2010_Design))#frequency 
prop.table(svytable(~as.factor(residence),DHS2010_Design))*100#proportions

#2015
sum(svytable(~as.factor(residence),DHS2015_Design))#frequency 
prop.table(svytable(~as.factor(residence),DHS2015_Design))*100#proportions

#-------------------------------------#
#Education level 
#1988
sum(svytable(~as.factor(edu_level),DHS1988_Design))#frequency 
prop.table(svytable(~as.factor(edu_level),DHS1988_Design))*100#proportions 

#1994
sum(svytable(~as.factor(edu_level),DHS1994_Design))#frequency 
prop.table(svytable(~as.factor(edu_level),DHS1994_Design))*100#proportions

#1999
sum(svytable(~as.factor(edu_level),DHS1999_Design))#frequency 
prop.table(svytable(~as.factor(edu_level),DHS1999_Design))*100#proportions

#2005
sum(svytable(~as.factor(edu_level),DHS2005_Design))#frequency 
prop.table(svytable(~as.factor(edu_level),DHS2005_Design))*100#proportions

#2010
sum(svytable(~as.factor(edu_level),DHS2010_Design))#frequency 
prop.table(svytable(~as.factor(edu_level),DHS2010_Design))*100#proportions

#2015
sum(svytable(~as.factor(edu_level),DHS2015_Design))#frequency 
prop.table(svytable(~as.factor(edu_level),DHS2015_Design))*100#proportions

#-------------------------------------#
#Marital status variable
#1988
sum(svytable(~as.factor(mrt_status),DHS1988_Design))#frequency 
prop.table(svytable(~as.factor(mrt_status),DHS1988_Design))*100#proportions 

#1994
sum(svytable(~as.factor(mrt_status),DHS1994_Design))#frequency 
prop.table(svytable(~as.factor(mrt_status),DHS1994_Design))*100#proportions

#1999
sum(svytable(~as.factor(mrt_status),DHS1999_Design))#frequency 
prop.table(svytable(~as.factor(mrt_status),DHS1999_Design))*100#proportions

#2005
sum(svytable(~as.factor(mrt_status),DHS2005_Design))#frequency 
prop.table(svytable(~as.factor(mrt_status),DHS2005_Design))*100#proportions

#2010
sum(svytable(~as.factor(mrt_status),DHS2010_Design))#frequency 
prop.table(svytable(~as.factor(mrt_status),DHS2010_Design))*100#proportions

#2015
sum(svytable(~as.factor(mrt_status),DHS2015_Design))#frequency 
prop.table(svytable(~as.factor(mrt_status),DHS2015_Design))*100#proportions

#-------------------------------------#
#Parity variable
#1988
sum(svytable(~as.factor(parity),DHS1988_Design))#frequency 
prop.table(svytable(~as.factor(parity),DHS1988_Design))*100#proportions 

#1994
sum(svytable(~as.factor(parity),DHS1994_Design))#frequency 
prop.table(svytable(~as.factor(parity),DHS1994_Design))*100#proportions

#1999
sum(svytable(~as.factor(parity),DHS1999_Design))#frequency 
prop.table(svytable(~as.factor(parity),DHS1999_Design))*100#proportions

#2005
sum(svytable(~as.factor(parity),DHS2005_Design))#frequency 
prop.table(svytable(~as.factor(parity),DHS2005_Design))*100#proportions

#2010
sum(svytable(~as.factor(parity),DHS2010_Design))#frequency 
prop.table(svytable(~as.factor(parity),DHS2010_Design))*100#proportions

#2015
sum(svytable(~as.factor(parity),DHS2015_Design))#frequency 
prop.table(svytable(~as.factor(parity),DHS2015_Design))*100#proportions

#-------------------------------------#
#Knowledge of contraceptive method variable
#1988
sum(svytable(~as.factor(fp_knowledge),DHS1988_Design))#frequency 
prop.table(svytable(~as.factor(fp_knowledge),DHS1988_Design))*100#proportions 

#1994
sum(svytable(~as.factor(fp_knowledge),DHS1994_Design))#frequency 
prop.table(svytable(~as.factor(fp_knowledge),DHS1994_Design))*100#proportions

#1999
sum(svytable(~as.factor(fp_knowledge),DHS1999_Design))#frequency 
prop.table(svytable(~as.factor(fp_knowledge),DHS1999_Design))*100#proportions

#2005
sum(svytable(~as.factor(fp_knowledge),DHS2005_Design))#frequency 
prop.table(svytable(~as.factor(fp_knowledge),DHS2005_Design))*100#proportions

#2010
sum(svytable(~as.factor(fp_knowledge),DHS2010_Design))#frequency 
prop.table(svytable(~as.factor(fp_knowledge),DHS2010_Design))*100#proportions

#2015
sum(svytable(~as.factor(fp_knowledge),DHS2015_Design))#frequency 
prop.table(svytable(~as.factor(fp_knowledge),DHS2015_Design))*100#proportions

#-------------------------------------#
#Employment status variable

#1988
svytable(~as.factor(v714),DHS1988_Design)
prop.table(svytable(~as.factor(v714),DHS1988_Design))*100#proportions

#1994
svytable(~as.factor(v714),DHS1994_Design)
prop.table(svytable(~as.factor(v714),DHS1994_Design))*100#proportions

#1999
svytable(~as.factor(v714),DHS1999_Design)
prop.table(svytable(~as.factor(v714),DHS1999_Design))*100#proportions

#2005
svytable(~as.factor(v714),DHS2005_Design)
prop.table(svytable(~as.factor(v714),DHS2005_Design))*100#proportions

#2010
svytable(~as.factor(v714),DHS2010_Design)
prop.table(svytable(~as.factor(v714),DHS2010_Design))*100#proportions

#2015
svytable(~as.factor(v714),DHS2015_Design)
prop.table(svytable(~as.factor(v714),DHS2015_Design))*100#proportions

#-------------------------------------#
#Desire for more children

#1988
svytable(~as.factor(kids_desire),DHS1988_Design)
prop.table(svytable(~as.factor(kids_desire), DHS1988_Design))*100#proportional distribution

#1994
svytable(~as.factor(kids_desire),DHS1994_Design)
prop.table(svytable(~as.factor(kids_desire), DHS1994_Design))*100#proportional distribution

#1999
svytable(~as.factor(kids_desire),DHS1999_Design)
prop.table(svytable(~as.factor(kids_desire), DHS1999_Design))*100#proportional distribution

#2005
svytable(~as.factor(kids_desire),DHS2005_Design)
prop.table(svytable(~as.factor(kids_desire), DHS2005_Design))*100#proportional distribution

#2010
svytable(~as.factor(kids_desire),DHS2010_Design)
prop.table(svytable(~as.factor(kids_desire), DHS2010_Design))*100#proportional distribution

#2015
svytable(~as.factor(kids_desire),DHS2015_Design)
prop.table(svytable(~as.factor(kids_desire), DHS2015_Design))*100#proportional distribution

#---------------------------------#
#Region by residence

#1988
prop.table(svytable(~ as.factor(region),subset(DHS1988_Design,residence == 1)))*100 #urban region 
prop.table(svytable(~ as.factor(region),subset(DHS1988_Design,residence == 2)))*100 #rural region 

#1994
prop.table(svytable(~ as.factor(region),subset(DHS1994_Design,residence == 1)))*100 #urban region 
prop.table(svytable(~ as.factor(region),subset(DHS1994_Design,residence == 2)))*100 #rural region 

#1999
prop.table(svytable(~ as.factor(region),subset(DHS1999_Design,residence == 1)))*100 #urban region 
prop.table(svytable(~ as.factor(region),subset(DHS1999_Design,residence == 2)))*100 #rural region 

#2005
prop.table(svytable(~ as.factor(region),subset(DHS2005_Design,residence == 1)))*100 #urban region 
prop.table(svytable(~ as.factor(region),subset(DHS2005_Design,residence == 2)))*100 #rural region 

#2010
prop.table(svytable(~ as.factor(region),subset(DHS2010_Design,residence == 1)))*100 #urban region 
prop.table(svytable(~ as.factor(region),subset(DHS2010_Design,residence == 2)))*100 #rural region 

#2015
prop.table(svytable(~ as.factor(region),subset(DHS2015_Design,residence == 1)))*100 #urban region 
prop.table(svytable(~ as.factor(region),subset(DHS2015_Design,residence == 2)))*100 #rural region 

#Plot
# Urban region 
year <- rep(c("1988","1994","1999","2005-06","2010-11","2015"),10) #create year variable
Region <- c(rep("1",6),rep("2",6),rep("3",6),rep("4",6),rep("5",6),
            rep("6",6),rep("7",6),rep("8",6),rep("9",6),rep("10",6)) 
Prop_reg <- c(3.8194444,7.2410542,5.0410039,7.2774035,7.2682736,6.5220394,
              6.5972222,0.8747244,2.3858580,1.4333060,1.3058807,1.7656311,
              6.5972222,0.8766060,1.6644510,2.9846412,2.8224344,3.5877554,
              9.3750000,8.1697377,7.2335544,7.6523205,7.4766376,8.6830403,
              0.0000000,2.1146188,3.1218363,2.6516795,2.8382924,2.0109727,
              0.0000000,0.5424352,2.2368018,3.2348014,2.7144199,3.6137189,
              14.2361111,8.7888776,8.8316672,8.6656549,9.4175412,11.0721358,
              2.0833333,2.0452765,1.3102748,2.5419159,2.0418418,2.9214184,
              24.3055556,50.5877405,49.6959659,45.1811793,46.5104570,39.5159834,
              32.9861111,18.7589291,18.4785866,18.3770979,17.6042214,20.3073045)

#Create data frame
data_reg <- data.frame(year,Prop_reg,Region)
data_reg

#Define colors for each region
library(tibble)
color_table <- tibble(
  Region = c("1","2","3","4","5","6","7","8","9","10"),
  Color = gray.colors(10,0.0,1.0)
)
#Setting each region as factor defined by each color
data_reg$Region <- factor(data_reg$Region, levels = color_table$Region)

#Plot 
ggplot(data_reg, aes(fill=Region,x=year, y=Prop_reg)) +
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 1.0)+
  labs(x = "Survey year",y = "Proportion (%)",fill="",title = "Urban region")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))


# Rural region 
year <- rep(c("1988","1994","1999","2005-06","2010-11","2015"),9) #create year variable
Region <- c(rep("1",6),rep("2",6),rep("3",6),rep("4",6),rep("5",6),
            rep("6",6),rep("7",6),rep("8",6),rep("9",6)) 
Prop_reg <- c(13.993174,15.921160,18.749086,14.127481,16.204223,16.006290,
              7.508532,12.458617,12.250550,15.467464,14.903619,12.843375,
              15.358362,11.146225,11.176488,10.544831,12.858649,12.287510,
              12.116041,14.287652,12.861577,10.471344,14.344149,11.598885,
              9.044369,11.632290,7.353955,9.692742,6.517738,7.401257,
              14.334471,8.387108,9.635683,6.061176,7.921840,7.918403,
              13.481229,13.820536,13.951707,14.435891,13.946155,16.753530,
              14.163823,12.346413,14.020954,19.199072,13.303627,13.614864,
              0.000000,0.000000,0.000000,0.000000,0.000000,1.575887)

#Create data frame
data_reg <- data.frame(year,Prop_reg,Region)
data_reg

#Define colors for each region
color_table <- tibble(
  Region = c("1","2","3","4","5","6","7","8","9"),
  Color = gray.colors(9,0.0,1.0)
)
#Setting each region as factor defined by each color
data_reg$Region <- factor(data_reg$Region, levels = color_table$Region)

#Plot 
ggplot(data_reg, aes(fill=Region,x=year, y=Prop_reg)) +
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 1.0)+
  labs(x = "Survey year",y = "Proportion (%)",fill="",title = "Rural region")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))


#****************************************#
#AGE AT FIRST SEX                        #
#****************************************#

#Read data into R
AFS1988 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS1988.csv")[,-1]
AFS1994 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS1994.csv")[,-1]
AFS1999 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS1999.csv")[,-1]
AFS2005 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS2005.csv")[,-1]
AFS2010 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS2010.csv")[,-1]
AFS2015 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS2015.csv")[,-1]

#Define survey design
AFS1988_Design <- svydesign(AFS1988$ccluster,strata = AFS1988$sstrata,
                            weights=AFS1988$weight,data=AFS1988,nest = TRUE ) #1988 data
AFS1994_Design <- svydesign(AFS1994$ccluster,strata = AFS1994$sstrata,
                            weights=AFS1994$weight,data=AFS1994,nest = TRUE ) #1994 data
AFS1999_Design <- svydesign(AFS1999$ccluster,strata = AFS1999$sstrata,
                            weights=AFS1999$weight,data=AFS1999,nest = TRUE ) #1999 data
AFS2005_Design <- svydesign(AFS2005$ccluster,strata = AFS2005$sstrata,
                            weights=AFS2005$weight,data=AFS2005,nest = TRUE ) #2005 data
AFS2010_Design <- svydesign(AFS2010$ccluster,strata = AFS2010$sstrata,
                            weights=AFS2010$weight,data=AFS2010,nest = TRUE ) #2010 data
AFS2015_Design <- svydesign(AFS2015$ccluster,strata = AFS2015$sstrata,
                            weights=AFS2015$weight,data=AFS2015,nest = TRUE ) #2015 data

#Sample size (n)
sum(svytable(~AFS_grp,AFS1988_Design))
sum(svytable(~AFS_grp,AFS1994_Design))
sum(svytable(~AFS_grp,AFS1999_Design))
sum(svytable(~AFS_grp,AFS2005_Design))
sum(svytable(~AFS_grp,AFS2010_Design))
sum(svytable(~AFS_grp,AFS2015_Design))

#Calculate mean age at first sex
svymean(~age_sex,AFS1988_Design) #1988 data
svymean(~age_sex,AFS1994_Design) #1994 data
svymean(~age_sex,AFS1999_Design) #1999 data
svymean(~age_sex,AFS2005_Design) #2005 data
svymean(~age_sex,AFS2010_Design) #2010 data
svymean(~age_sex,AFS2015_Design) #2015 data

#Calculate proportional distribution of  grouped age at first sex
prop.table(svytable(~AFS_grp,AFS1988_Design))*100  #1988 data 
prop.table(svytable(~AFS_grp,AFS1994_Design))*100  #1994 data
prop.table(svytable(~AFS_grp,AFS1999_Design))*100  #1999 data 
prop.table(svytable(~AFS_grp,AFS2005_Design))*100  #2005 data 
prop.table(svytable(~AFS_grp,AFS2010_Design))*100  #2010 data
prop.table(svytable(~AFS_grp,AFS2015_Design))*100  #2015 data

#Plot
#create Age at first sex <15 proportions vector
evhs_AFS0 <- c(14.1,9.6,6.0,8.8,6.1,8.2)

#create Age at first sex 15-17 proportions vector
evhs_AFS1 <- c(50.6,41.9,39.9,44.1,47.9,51.1)

#create Age at first sex 18-20 proportions vector
evhs_AFS2 <- c(29.6,34.0,34.8,35.2,37.6,33.5)

#create Age at first sex 21-22 proportions vector
evhs_AFS3 <- c(4.9,6.4,6.1,6.9,6.8,5.9)

#create Age at first sex 23-24 proportions vector
evhs_AFS4 <- c(0.8,1.2,0.9,1.1,0.9,0.9)

#create Age at first sex at first union proportions vector
evhs_AFS5 <- c(0.0,6.9,12.3,4.0,0.6,0.3)

#combine proportions for each age group
SA_AFS <- as.numeric(rbind(evhs_AFS0,evhs_AFS1,evhs_AFS2,evhs_AFS3,evhs_AFS4,
                           evhs_AFS5))   

#Create Age at first sex vector
AFS <- rep(c("<15","15-17","18-20","21-22","23-24","at first union"),6)

#Create year vector
year1 <-c(rep("1988",6),rep("1994",6),rep("1999",6),
          rep("2005-06",6),rep("2010-11",6),rep("2015",6))

#Create data frame
data_AFS <- data.frame(year1,AFS,SA_AFS)
data_AFS

#Plot
ggplot(data_AFS, aes(fill=AFS,x=year1, y=SA_AFS)) +
  scale_y_continuous(breaks = seq(0, 60, by = 10))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 1.0)+
  labs(x = "Survey year",y = "Proportion of women (%)",title ="",fill="" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9))


#****************************************#
#TRENDS IN CONTRACEPTIVE USE             #
#****************************************#
#1988
sum(svytable(~as.factor(fp_use),DHS1988_Design))
prop.table(svytable(~as.factor(fp_use),DHS1988_Design))*100#proportional distribution

#1994
sum(svytable(~as.factor(fp_use),DHS1994_Design))
prop.table(svytable(~as.factor(fp_use),DHS1994_Design))*100#proportional distribution

#1999
sum(svytable(~as.factor(fp_use),DHS1999_Design))
prop.table(svytable(~as.factor(fp_use),DHS1999_Design))*100#proportional distribution

#2005
sum(svytable(~as.factor(fp_use),DHS2005_Design))
prop.table(svytable(~as.factor(fp_use),DHS2005_Design))*100#proportional distribution

#2010
sum(svytable(~as.factor(fp_use),DHS2010_Design))
prop.table(svytable(~as.factor(fp_use),DHS2010_Design))*100#proportional distribution

#2015
sum(svytable(~as.factor(fp_use),DHS2015_Design))
prop.table(svytable(~as.factor(fp_use),DHS2015_Design))*100#proportional distribution

#Plot 
year <- c("1988\n(n=874)","1994\n(n=1 231)","1999\n(n=1 324)","2005-06\n(n=1 981) ","2010-11\n(n=1 882)","2015\n(n=1 872)") #create year vector
prop <- c(39.70252,46.31027,49.88653,52.80100,53.67190,58.23954) #create proportions vector
FP_data <- data.frame(year,prop)#Create data frame
#Plot
ggplot(FP_data, aes(x=year, y=prop)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 1.0)+
  labs(x = "Survey year",y = "Proportion (%)",title ="Women using modern contraceptives",fill="" )+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))

#-------------------------------#
#MICS vs DHS

#MICS 2009
sum(svytable(~as.factor(cp2),MICS2009_Design))#frequencies
prop.table(svytable(~as.factor(cp2),MICS2009_Design))*100#proportional distribution

#DHS 2010
sum(svytable(~as.factor(cp_use),DHS2010_Design_comp))
prop.table(svytable(~as.factor(cp_use),DHS2010_Design_comp))*100

#MICS 2014
sum(svytable(~as.factor(CP2),MICS2014_Design))#frequencies
prop.table(svytable(~as.factor(CP2),MICS2014_Design))*100#proportional distribution

#DHS 2015
sum(svytable(~as.factor(cp_use),DHS2015_Design_comp))
prop.table(svytable(~as.factor(cp_use),DHS2015_Design_comp))*100

#Plot
Year <- c("MICS 2009\n(n=4 537","DHS 2010-11\n(n=3 447",
          "MICS 2014\n(n=2 799","DHS 2015\n(n=1 877")
Proportion <- c(31.39926,29.80382,64.30954,58.59436)

#Create data frame
data_cpres <- data.frame(Year,Proportion)
data_cpres

#Plot
plot_grid(
  ggplot(data_cpres, aes(x=factor(Year,levels =c("MICS 2009\n(n=4 537","DHS 2010-11\n(n=3 447",
                                                 "MICS 2014\n(n=2 799","DHS 2015\n(n=1 877") ), y=Proportion)) +
    scale_y_continuous(breaks = seq(0, 100, by = 10))+
    geom_bar(stat="identity",position="dodge",color="black")+
    scale_fill_grey(start = 0.0, end = 0.5)+
    labs(x = "Survey year",y = "Proportion (%)", title = "Women using contraceptives",fill="")+
    theme_bw()+
    theme(legend.position="bottom",
          text = element_text(size=9),
          plot.title = element_text(hjust = 0.5)),
  labels = "")


#********************************************#
#TRENDS IN CONTRACEPTIVE UNMET NEED          #
#********************************************#

#Read data
CPNEED1994 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP Need data/CPNEED1994.csv")[,-1]
CPNEED1999 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP Need data/CPNEED1999.csv")[,-1]
CPNEED2005 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP Need data/CPNEED2005.csv")[,-1]
CPNEED2010 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP Need data/CPNEED2010.csv")[,-1]
CPNEED2015 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP Need data/CPNEED2015.csv")[,-1]


#Survey design
#1994 data
Need1994_Design <- svydesign(CPNEED1994$ccluster,strata = CPNEED1994$sstrata,
                             weights=CPNEED1994$weight,data=CPNEED1994,nest = TRUE )
#1999 data
Need1999_Design <- svydesign(CPNEED1999$ccluster,strata = CPNEED1999$sstrata,
                             weights=CPNEED1999$weight,data=CPNEED1999,nest = TRUE )
#2005 data
Need2005_Design <- svydesign(CPNEED2005$ccluster,strata = CPNEED2005$sstrata,
                             weights=CPNEED2005$weight,data=CPNEED2005,nest = TRUE )
#2010 data
Need2010_Design <- svydesign(CPNEED2010$ccluster,strata = CPNEED2010$sstrata,
                             weights=CPNEED2010$weight,data=CPNEED2010,nest = TRUE )
#2015 data
Need2015_Design <- svydesign(CPNEED2015$ccluster,strata = CPNEED2015$sstrata,
                             weights=CPNEED2015$weight,data=CPNEED2015,nest = TRUE )


#Contraceptive unmet need
#1994
sum(svytable(~as.factor(cp_needs),Need1994_Design))
prop.table(svytable(~as.factor(cp_needs),Need1994_Design))*100#proportional distribution

#1999
sum(svytable(~as.factor(cp_needs),Need1999_Design))
prop.table(svytable(~as.factor(cp_needs),Need1999_Design))*100#proportional distribution

#2005-06
sum(svytable(~as.factor(cp_needs),Need2005_Design))
prop.table(svytable(~as.factor(cp_needs),Need2005_Design))*100#proportional distribution

#2010-11
sum(svytable(~as.factor(cp_needs),Need2010_Design))
prop.table(svytable(~as.factor(cp_needs),Need2010_Design))*100#proportional distribution

#2015
sum(svytable(~as.factor(cp_needs),Need2015_Design))
prop.table(svytable(~as.factor(cp_needs),Need2015_Design))*100#proportional distribution

#Plot
Year <- c("1994\n(n= 1 090)","1999\n(n=1 135)","2005-06\n(n=1 702)",
          "2010-11\n(n=1 643)","2015\n(n=1 625)")
Proportion <- c(54.08509,48.72859,47.12385,45.90549,41.58298)
#Create data frame
data_unmet <- data.frame(Year,Proportion)
data_unmet

#Plot
plot_grid(
  ggplot(data_unmet, aes(x=Year, y=Proportion)) +
    scale_y_continuous(breaks = seq(0, 100, by = 10))+
    geom_bar(stat="identity",position="dodge",color="black")+
    scale_fill_grey(start = 0.0, end = 0.5)+
    labs(x = "Survey year",y = "Proportion (%)", title = "Women with unmet need for modern contraceptives",fill="")+
    theme_bw()+
    theme(legend.position="bottom",
          text = element_text(size=9),
          plot.title = element_text(hjust = 0.5)),
  labels = "")


#******************************************#
#TRENDS IN CONTRACEPTIVE METHOD CHOICE     #
#******************************************#

#Read data into R
DHS1988_MC <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS1988_MC.csv")[,-1]
DHS1994_MC <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS1994_MC.csv")[,-1]   
DHS1999_MC <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS1999_MC.csv")[,-1]
DHS2005_MC <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS2005_MC.csv")[,-1]
DHS2010_MC <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS2010_MC.csv")[,-1]
DHS2015_MC <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS2015_MC.csv")[,-1]

#----------------------------#
#Define survey designs
DHS1988_Design_MC <- svydesign(DHS1988_MC$ccluster,strata = DHS1988_MC$sstrata,
                            weights=DHS1988_MC$weight,data=DHS1988_MC,nest = TRUE )
DHS1994_Design_MC <- svydesign(DHS1994_MC$ccluster,strata = DHS1994_MC$sstrata,
                            weights=DHS1994_MC$weight,data=DHS1994_MC,nest = TRUE )
DHS1999_Design_MC <- svydesign(DHS1999_MC$ccluster,strata = DHS1999_MC$sstrata,
                            weights=DHS1999_MC$weight,data=DHS1999_MC,nest = TRUE )
DHS2005_Design_MC <- svydesign(DHS2005_MC$ccluster,strata = DHS2005_MC$sstrata,
                            weights=DHS2005_MC$weight,data=DHS2005_MC,nest = TRUE )
DHS2010_Design_MC <- svydesign(DHS2010_MC$ccluster,strata = DHS2010_MC$sstrata,
                            weights=DHS2010_MC$weight,data=DHS2010_MC,nest = TRUE )
DHS2015_Design_MC <- svydesign(DHS2015_MC$ccluster,strata = DHS2015_MC$sstrata,
                            weights=DHS2015_MC$weight,data=DHS2015_MC,nest = TRUE )

#--------------------------------------#
#Contraceptive method choice

#1988
sum(svytable(~as.factor(method_choice),DHS1988_Design_MC))
prop.table(svytable(~as.factor(method_choice),DHS1988_Design_MC))*100#proportional distribution

#1994
sum(svytable(~as.factor(method_choice),DHS1994_Design_MC))
prop.table(svytable(~as.factor(method_choice),DHS1994_Design_MC))*100#proportional distribution

#1999
sum(svytable(~as.factor(method_choice),DHS1999_Design_MC))
prop.table(svytable(~as.factor(method_choice),DHS1999_Design_MC))*100#proportional distribution

#2005
sum(svytable(~as.factor(method_choice),DHS2005_Design_MC))
prop.table(svytable(~as.factor(method_choice),DHS2005_Design_MC))*100#proportional distribution

#2010
sum(svytable(~as.factor(method_choice),DHS2010_Design_MC))
prop.table(svytable(~as.factor(method_choice),DHS2010_Design_MC))*100#proportional distribution

#2015
sum(svytable(~as.factor(method_choice),DHS2015_Design_MC))
prop.table(svytable(~as.factor(method_choice),DHS2015_Design_MC))*100#proportional distribution

#Plot
#Create data set
meth_choice <- rep(c("Pill","Injections","Male\ncondom",
                     "Norplant/\nImplant"),6)
Year <- c(rep("1988\n(n=831)",4),rep("1994\n(n=1 195)",4),rep("1999\n(n=1 284)",4),
          rep("2005-06\n(n=1 952)",4),rep("2010-11\n(n=1 853)",4),rep("2015\n(n=1 849)",4))
Proportion <- c(39.7111913,0.1203369,1.5643803,0,
                40.9291116,1.7287506,4.6887359,0.2908563,
                40.0003356,4.9990991,4.8442817,0.4270379,
                41.2265259,7.8622244,3.2657721,0.7152992,
                41.076230,7.357558,3.751487,1.704369,
                33.856027,9.651768,4.366821,10.378923)

data_cpres <- data.frame(Year,meth_choice,Proportion)
data_cpres

#Plot 
ggplot(data_cpres, aes(fill=meth_choice,x=Year, y=Proportion)) +
  scale_y_continuous(breaks = seq(0, 50, by = 5))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 1.0)+
  labs(x = "Survey year",y = "Proportion of women (%)",
       title = "Women's current method choice",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))
#========================================================================================#




#========================================================================================#
#                     BIVARIATE DATA ANALYSIS

#**************************************************#
#TRENDS IN CONTRACEPTIVE USE BY RESIDENCE          #
#**************************************************#

#1988
svytable(~ as.factor(residence),DHS1988_Design)#frequencies 
svytable(~as.factor(fp_use)+ as.factor(residence),DHS1988_Design)#frequencies 
prop.table(svytable(~as.factor(fp_use)+ as.factor(residence),DHS1988_Design),2)*100#proportional distribution

#1994
svytable(~ as.factor(residence),DHS1994_Design)#frequencies
svytable(~as.factor(fp_use)+ as.factor(residence),DHS1994_Design)#frequencies 
prop.table(svytable(~as.factor(fp_use)+ as.factor(residence),DHS1994_Design),2)*100#proportional distribution

#1999
svytable(~ as.factor(residence),DHS1999_Design)#frequencies
svytable(~as.factor(fp_use)+ as.factor(residence),DHS1999_Design)#frequencies 
prop.table(svytable(~as.factor(fp_use)+ as.factor(residence),DHS1999_Design),2)*100#proportional distribution

#2005
svytable(~ as.factor(residence),DHS2005_Design)#frequencies
svytable(~as.factor(fp_use)+ as.factor(residence),DHS2005_Design)#frequencies 
prop.table(svytable(~as.factor(fp_use)+ as.factor(residence),DHS2005_Design),2)*100#proportional distribution

#2010
svytable(~ as.factor(residence),DHS2010_Design)#frequencies
svytable(~as.factor(fp_use)+ as.factor(residence),DHS2010_Design)#frequencies 
prop.table(svytable(~as.factor(fp_use)+ as.factor(residence),DHS2010_Design),2)*100#proportional distribution

#2015
svytable(~ as.factor(residence),DHS2015_Design)#frequencies
svytable(~as.factor(fp_use)+ as.factor(residence),DHS2015_Design)#frequencies 
prop.table(svytable(~as.factor(fp_use)+ as.factor(residence),DHS2015_Design),2)*100#proportional distribution

#----------------------#
#Plot
#Create data set
Residence <- rep(c("Urban","Rural"),6)
Year <- c(rep("1988",2),rep("1994",2),rep("1999",2),
          rep("2005-06",2),rep("2010-11",2),rep("2015",2))
sample_size <- c("n=288","n=586","n=388","n=843","n=532","n=792",
                 "n=730","n=1 250","n=668","n=1 213","n=608","n=1 264")
Proportion <- c(47.91667,35.66553, 56.24181,41.73267, 58.83278,43.87202,
                55.89234,50.99517, 51.09597,55.09084, 56.13841,59.25048)

data_cpres <- data.frame(Year,Residence,Proportion,sample_size)
data_cpres


#Plot 
ggplot(data_cpres, aes(fill=Residence,x=Year, y=Proportion)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 0.5)+
  labs(x = "Survey year",y = "Proportion of women (%)",
       title = "Women using modern contraceptives by residence",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=sample_size), position=position_dodge(width=0.9),
            vjust=-0.25,size=3)


#-----------------------------------------#\
# MICS vs DHS

#MICS 2009
svytable(~as.factor(hh6),MICS2009_Design)#frequencies
svytable(~as.factor(cp2)+as.factor(hh6),MICS2009_Design)#frequencies
prop.table(svytable(~as.factor(cp2)+as.factor(hh6),MICS2009_Design),2)*100#proportional distribution

#DHS 2010
svytable(~as.factor(residence),DHS2010_Design_comp)
svytable(~as.factor(cp_use)+as.factor(residence),DHS2010_Design_comp)
prop.table(svytable(~as.factor(cp_use)+as.factor(residence),DHS2010_Design_comp),2)*100

#MICS 2014
svytable(~as.factor(HH6),MICS2014_Design)#frequencies
svytable(~as.factor(CP2)+as.factor(HH6),MICS2014_Design)#frequencies
prop.table(svytable(~as.factor(CP2)+as.factor(HH6),MICS2014_Design),2)*100#proportional distribution

#DHS 2015
svytable(~as.factor(residence),DHS2015_Design_comp)
svytable(~as.factor(cp_use)+as.factor(residence),DHS2015_Design_comp)
prop.table(svytable(~as.factor(cp_use)+as.factor(residence),DHS2015_Design_comp),2)*100

#Plot
#Create data frame
Residence <- rep(c("Urban","Rural"),4)
Year <- c(rep("MICS 2009",2),rep("DHS 2010-11",2),
          rep("MICS 2014",2),rep("DHS 2015",2))
sample_size <- c("n=1 811","n=2 726","n=1 405","n=2 042",
                 "n=842","n=1 957","n=610","n=1 268")
Proportion <- c(26.55965,34.61470, 24.60870,33.37998,
                60.98993,65.73808, 56.80042,59.45755)

#Create data frame
data_cpres <- data.frame(Year,Residence,Proportion,sample_size)
data_cpres

#Plot
ggplot(data_cpres, aes(fill=Residence,x=factor(Year,levels = c("MICS 2009","DHS 2010-11",
                                                               "MICS 2014","DHS 2015")), y=Proportion)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.1, end = 0.5)+
  labs(x = "Survey year",y = "Proportion of women (%)", title = "Women using contraceptives by residence",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=sample_size), position=position_dodge(width=0.9),
            vjust=-0.25,size=3)


#*************************************************#
# TRENDS IN CONTRACEPTIVE UNMET NEED BY RESIDENCE #
#*************************************************#

#1994
svytable(~as.factor(residence),Need1994_Design)#frequencies 
prop.table(svytable(~as.factor(cp_needs)+ as.factor(residence),Need1994_Design),2)*100#proportional distribution

#1999
svytable(~as.factor(residence),Need1999_Design)#frequencies 
prop.table(svytable(~as.factor(cp_needs)+ as.factor(residence),Need1999_Design),2)*100#proportional distribution

#2005
svytable(~as.factor(residence),Need2005_Design)#frequencies 
prop.table(svytable(~as.factor(cp_needs)+ as.factor(residence),Need2005_Design),2)*100#proportional distribution

#2010
svytable(~as.factor(residence),Need2010_Design)#frequencies 
prop.table(svytable(~as.factor(cp_needs)+ as.factor(residence),Need2010_Design),2)*100#proportional distribution

#2015
svytable(~as.factor(residence),Need2015_Design)#frequencies 
prop.table(svytable(~as.factor(cp_needs)+ as.factor(residence),Need2015_Design),2)*100#proportional distribution

#Plot
#Create data set
Residence <- rep(c("Urban","Rural"),5)
Year <- c(rep("1994",2),rep("1999",2),
          rep("2005-06",2),rep("2010-11",2),rep("2015",2))
sample_size <- c("n=337","n=767","n=471","n=682",
                 "n=589","n=1 153","n=555","n=1 110","n=521","n=1 132")
Proportion <- c(42.92568,59.02703, 41.58526,53.68432,  42.04362,49.72845,
                46.53332,45.59589, 44.2945,40.3409)

data_cpres <- data.frame(Year,Residence,Proportion,sample_size)
data_cpres


#Plot 
ggplot(data_cpres, aes(fill=Residence,x=Year, y=Proportion)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 0.5)+
  labs(x = "Survey year",y = "Proportion of women (%)",
       title = "Women with unmet need for modern contraceptives by residence",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=sample_size), position=position_dodge(width=0.9),
            vjust=-0.25,size=3)


#****************************************************#
# TRENDS IN CONTRACEPTIVE METHOD CHOICE BY RESIDENCE #
#****************************************************#

#1988
svytable(~as.factor(residence),DHS1988_Design_MC)#frequencies
svytable(~as.factor(method_choice)+ as.factor(residence),DHS1988_Design_MC)#frequencies 
prop.table(svytable(~as.factor(method_choice)+ as.factor(residence),DHS1988_Design_MC),2)*100#proportional distribution

#1994
svytable(~as.factor(residence),DHS1994_Design_MC)#frequencies 
svytable(~as.factor(method_choice)+ as.factor(residence),DHS1994_Design_MC)#frequencies 
prop.table(svytable(~as.factor(method_choice)+ as.factor(residence),DHS1994_Design_MC),2)*100#proportional distribution

#1999
svytable(~as.factor(residence),DHS1999_Design_MC)#frequencies 
svytable(~as.factor(method_choice)+ as.factor(residence),DHS1999_Design_MC)#frequencies 
prop.table(svytable(~as.factor(method_choice)+ as.factor(residence),DHS1999_Design_MC),2)*100#proportional distribution

#2005
svytable(~as.factor(residence),DHS2005_Design_MC)#frequencies 
svytable(~as.factor(method_choice)+ as.factor(residence),DHS2005_Design_MC)#frequencies 
prop.table(svytable(~as.factor(method_choice)+ as.factor(residence),DHS2005_Design_MC),2)*100#proportional distribution

#2010
svytable(~as.factor(residence),DHS2010_Design_MC)#frequencies 
svytable(~as.factor(method_choice)+ as.factor(residence),DHS2010_Design_MC)#frequencies 
prop.table(svytable(~as.factor(method_choice)+ as.factor(residence),DHS2010_Design_MC),2)*100#proportional distribution

#2015
svytable(~as.factor(residence),DHS2015_Design_MC)#frequencies 
svytable(~as.factor(method_choice)+ as.factor(residence),DHS2015_Design_MC)#frequencies 
prop.table(svytable(~as.factor(method_choice)+ as.factor(residence),DHS2015_Design_MC),2)*100#proportional distribution


#Plot
#------------------------------#
#Women using the pill 
#Create data set
Residence <- rep(c("Urban","Rural"),6)
Year <- c(rep("1988",2),rep("1994",2),rep("1999",2),
          rep("2005-06",2),rep("2010-11",2),rep("2015",2))
sample_size <- c("n=273","n=558","n=384","n=811","n=523","n=760",
                 "n=723","n=1 229","n=656","n=1 197","n=596","n=1 253")
Proportion <- c(46.8864469,36.2007168,  49.6007053,36.8203270,  46.5015894,35.5258833,
                38.7266356,42.6979788,  35.775203,43.983639,  28.472000,36.417908)

data_cpres <- data.frame(Year,Residence,Proportion,sample_size)
data_cpres

#Plot 
ggplot(data_cpres, aes(fill=Residence,x=Year, y=Proportion)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 0.5)+
  labs(x = "Survey year",y = "Proportion of women (%)",
       title = "Women using the pill by residence",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=sample_size), position=position_dodge(width=0.9),
            vjust=-0.25,size=3)

#------------------------------#
#Women using the Injections 
#Create data set
Residence <- rep(c("Urban","Rural"),6)
Year <- c(rep("1988",2),rep("1994",2),rep("1999",2),
          rep("2005-06",2),rep("2010-11",2),rep("2015",2))
sample_size <- c("n=273","n=558","n=384","n=811","n=523","n=760",
                 "n=723","n=1 229","n=656","n=1 197","n=596","n=1 253")
Proportion <- c(0.0000000,0.1792115, 2.2069222,1.5021828,  3.7899952,5.8312582,
                9.5897878,6.8453684,  6.881235,7.618804,  6.493298,11.154663)

data_cpres <- data.frame(Year,Residence,Proportion,sample_size)
data_cpres

#Plot 
ggplot(data_cpres, aes(fill=Residence,x=Year, y=Proportion)) +
  scale_y_continuous(breaks = seq(0, 14, by = 2))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 0.5)+
  labs(x = "Survey year",y = "Proportion of women (%)",
       title = "Women using injections by residence",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=sample_size), position=position_dodge(width=0.9),
            vjust=-0.25,size=3)

#------------------------------#
#Women using the Male condom 
#Create data set
Residence <- rep(c("Urban","Rural"),6)
Year <- c(rep("1988",2),rep("1994",2),rep("1999",2),
          rep("2005-06",2),rep("2010-11",2),rep("2015",2))
sample_size <- c("n=273","n=558","n=384","n=811","n=523","n=760",
                 "n=723","n=1 229","n=656","n=1 197","n=596","n=1 253")
Proportion <- c(2.5641026,1.0752688, 4.4534016,4.8002423,  7.4487708,3.0517564,
                6.3067202,1.4758489, 6.282264,2.363453,  8.054267,2.612223)

data_cpres <- data.frame(Year,Residence,Proportion,sample_size)
data_cpres

#Plot 
ggplot(data_cpres, aes(fill=Residence,x=Year, y=Proportion)) +
  scale_y_continuous(breaks = seq(0, 10, by = 2))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 0.5)+
  labs(x = "Survey year",y = "Proportion of women (%)",
       title = "Women using the male condom by residence",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=sample_size), position=position_dodge(width=0.9),
            vjust=-0.25,size=3)

#------------------------------#
#Women using the Norplant/Immplants 
#Create data set
Residence <- rep(c("Urban","Rural"),5)
Year <- c(rep("1994",2),rep("1999",2),
          rep("2005-06",2),rep("2010-11",2),rep("2015",2))
sample_size <- c("n=384","n=811","n=523","n=760",
                 "n=723","n=1 229","n=656","n=1 197","n=596","n=1 253")
Proportion <- c(0.6212836,0.1342929, 0.8538636,0.1332774,  1.5392400,0.2303219,
                1.918495,1.586929,  13.086466,9.090593)

data_cpres <- data.frame(Year,Residence,Proportion,sample_size)
data_cpres

#Plot 
ggplot(data_cpres, aes(fill=Residence,x=Year, y=Proportion)) +
  scale_y_continuous(breaks = seq(0, 14, by = 2))+
  geom_bar(stat="identity",position="dodge",color="black")+
  scale_fill_grey(start = 0.0, end = 0.5)+
  labs(x = "Survey year",y = "Proportion of women (%)",
       title = "Women using Norplant/Implant by residence",fill="")+
  theme_bw()+
  theme(legend.position="bottom",
        text = element_text(size=9),
        plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=sample_size), position=position_dodge(width=0.9),
            vjust=-0.25,size=3)


#*************************************************#
#CONTRACEPTIVE USE BY BACKGROUND CHARACTERISTICS  #
#*************************************************#

#---------------------------------------#
# Modern contraceptive use vs age group
#1988 data
svytable(~as.factor(age_grp),DHS1988_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(age_grp),DHS1988_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(age_grp),DHS1988_Design),2)*100#proportional distribution

#1994 data
svytable(~as.factor(age_grp),DHS1994_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(age_grp),DHS1994_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(age_grp),DHS1994_Design),2)*100#proportional distribution

#1999 data
svytable(~as.factor(age_grp),DHS1999_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(age_grp),DHS1999_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(age_grp),DHS1999_Design),2)*100#proportional distribution

#2005 data
svytable(~as.factor(age_grp),DHS2005_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(age_grp),DHS2005_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(age_grp),DHS2005_Design),2)*100#proportional distribution

#2010 data
svytable(~as.factor(age_grp),DHS2010_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(age_grp),DHS2010_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(age_grp),DHS2010_Design),2)*100#proportional distribution

#2015 data
svytable(~as.factor(age_grp),DHS2015_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(age_grp),DHS2015_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(age_grp),DHS2015_Design),2)*100#proportional distribution

#-------------------------------------#
#Modern contraceptive use by region
#1988
svytable(~as.factor(region),DHS1988_Design) #frequency
svytable(~ as.factor(fp_use)+as.factor(region),DHS1988_Design) #frequency
prop.table(svytable(~ as.factor(fp_use)+as.factor(region),DHS1988_Design),2)*100#proportional distribution

#1994
svytable(~ as.factor(region),DHS1994_Design) #frequency
svytable(~ as.factor(fp_use)+as.factor(region),DHS1994_Design) #frequency
prop.table(svytable(~ as.factor(fp_use)+as.factor(region),DHS1994_Design),2)*100#proportional distribution

#1999
svytable(~ as.factor(region),DHS1999_Design) #frequency
svytable(~ as.factor(fp_use)+as.factor(region),DHS1999_Design) #frequency
prop.table(svytable(~ as.factor(fp_use)+as.factor(region),DHS1999_Design),2)*100#proportional distribution

#2005
svytable(~ as.factor(region),DHS2005_Design) #frequency
svytable(~ as.factor(fp_use)+as.factor(region),DHS2005_Design) #frequency
prop.table(svytable(~ as.factor(fp_use)+as.factor(region),DHS2005_Design),2)*100#proportional distribution

#2010
svytable(~ as.factor(region),DHS2010_Design) #frequency
svytable(~ as.factor(fp_use)+as.factor(region),DHS2010_Design) #frequency
prop.table(svytable(~ as.factor(fp_use)+as.factor(region),DHS2010_Design),2)*100#proportional distribution

#2015
svytable(~ as.factor(region),DHS2015_Design) #frequency
svytable(~ as.factor(fp_use)+as.factor(region),DHS2015_Design) #frequency
prop.table(svytable(~ as.factor(fp_use)+as.factor(region),DHS2015_Design),2)*100#proportional distribution

#-------------------------------#
# Modern contraceptive use vs education
#1988 data
svytable(~as.factor(edu_level),DHS1988_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(edu_level),DHS1988_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(edu_level),DHS1988_Design),2)*100#proportional distribution

#1994 data
svytable(~as.factor(edu_level),DHS1994_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(edu_level),DHS1994_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(edu_level),DHS1994_Design),2)*100#proportional distribution

#1999 data
svytable(~as.factor(edu_level),DHS1999_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(edu_level),DHS1999_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(edu_level),DHS1999_Design),2)*100#proportional distribution

#2005 data
svytable(~as.factor(edu_level),DHS2005_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(edu_level),DHS2005_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(edu_level),DHS2005_Design),2)*100#proportional distribution

#2010 data
svytable(~as.factor(edu_level),DHS2010_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(edu_level),DHS2010_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(edu_level),DHS2010_Design),2)*100#proportional distribution

#2015 data
svytable(~as.factor(edu_level),DHS2015_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(edu_level),DHS2015_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(edu_level),DHS2015_Design),2)*100#proportional distribution

#-------------------------------------------#
# Modern contraceptive use vs marital status
#1988 data
svytable(~as.factor(mrt_status),DHS1988_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS1988_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS1988_Design),2)*100#proportional distribution

#1994 data
svytable(~as.factor(mrt_status),DHS1994_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS1994_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS1994_Design),2)*100#proportional distribution

#1999 data
svytable(~as.factor(mrt_status),DHS1999_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS1999_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS1999_Design),2)*100#proportional distribution

#2005 data
svytable(~as.factor(mrt_status),DHS2005_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS2005_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS2005_Design),2)*100#proportional distribution

#2010 data
svytable(~as.factor(mrt_status),DHS2010_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS2010_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS2010_Design),2)*100#proportional distribution

#2015 data
svytable(~as.factor(mrt_status),DHS2015_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS2015_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(mrt_status),DHS2015_Design),2)*100#proportional distribution

#-------------------------------#
# Modern contraceptive use vs parity
#1988 data
svytable(~as.factor(parity),DHS1988_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(parity),DHS1988_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(parity),DHS1988_Design),2)*100#proportional distribution

#1994 data
svytable(~as.factor(parity),DHS1994_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(parity),DHS1994_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(parity),DHS1994_Design),2)*100#proportional distribution

#1999 data
svytable(~as.factor(parity),DHS1999_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(parity),DHS1999_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(parity),DHS1999_Design),2)*100#proportional distribution

#2005 data
svytable(~as.factor(parity),DHS2005_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(parity),DHS2005_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(parity),DHS2005_Design),2)*100#proportional distribution

#2010 data
svytable(~as.factor(parity),DHS2010_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(parity),DHS2010_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(parity),DHS2010_Design),2)*100#proportional distribution

#2015 data
svytable(~as.factor(parity),DHS2015_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(parity),DHS2015_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(parity),DHS2015_Design),2)*100#proportional distribution

#----------------------------------------------------#
# Modern contraceptive use vs contraception knowledge
#1988 data
svytable(~as.factor(fp_knowledge),DHS1988_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(fp_knowledge),DHS1988_Design),2)*100#proportional distribution

#1994 data
svytable(~as.factor(fp_knowledge),DHS1994_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(fp_knowledge),DHS1994_Design),2)*100#proportional distribution

#1999 data
svytable(~as.factor(fp_knowledge),DHS1999_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(fp_knowledge),DHS1999_Design),2)*100#proportional distribution

#2005 data
svytable(~as.factor(fp_knowledge),DHS2005_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(fp_knowledge),DHS2005_Design),2)*100#proportional distribution

#2010 data
svytable(~as.factor(fp_knowledge),DHS2010_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(fp_knowledge),DHS2010_Design),2)*100#proportional distribution

#2015 data
svytable(~as.factor(fp_knowledge),DHS2015_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(fp_knowledge),DHS2015_Design),2)*100#proportional distribution

#-------------------------------#
# Modern contraceptive use vs employment
#1988 data
svytable(~as.factor(v714),DHS1988_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(v714),DHS1988_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(v714),DHS1988_Design),2)*100#proportional distribution

#1994 data
svytable(~as.factor(v714),DHS1994_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(v714),DHS1994_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(v714),DHS1994_Design),2)*100#proportional distribution

#1999 data
svytable(~as.factor(v714),DHS1999_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(v714),DHS1999_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(v714),DHS1999_Design),2)*100#proportional distribution

#2005 data
svytable(~as.factor(v714),DHS2005_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(v714),DHS2005_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(v714),DHS2005_Design),2)*100#proportional distribution

#2010 data
svytable(~as.factor(v714),DHS2010_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(v714),DHS2010_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(v714),DHS2010_Design),2)*100#proportional distribution

#2015 data
svytable(~as.factor(v714),DHS2015_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(v714),DHS2015_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(v714),DHS2015_Design),2)*100#proportional distribution

#-------------------------------#
# Modern contraceptive use vs desire for more children
#1988 data
svytable(~as.factor(kids_desire),DHS1988_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS1988_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS1988_Design),2)*100#proportional distribution

#1994 data
svytable(~as.factor(kids_desire),DHS1994_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS1994_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS1994_Design),2)*100#proportional distribution

#1999 data
svytable(~as.factor(kids_desire),DHS1999_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS1999_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS1999_Design),2)*100#proportional distribution

#2005 data
svytable(~as.factor(kids_desire),DHS2005_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS2005_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS2005_Design),2)*100#proportional distribution

#2010 data
svytable(~as.factor(kids_desire),DHS2010_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS2010_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS2010_Design),2)*100#proportional distribution

#2015 data
svytable(~as.factor(kids_desire),DHS2015_Design)#frequencies
svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS2015_Design)#frequencies
prop.table(svytable(~as.factor(fp_use)+as.factor(kids_desire),DHS2015_Design),2)*100#proportional distribution
#========================================================================================#

##########################################################################################
#                                END OF DESCRIPTIVE STATISTICS                           #
##########################################################################################
