##########################################################################################
#                           UNIVERSITY OF CAPE TOWN                                      #
#                           STUDENT NAME:   AUDREY MOYO                                  #
#                                DATA CLEANING                                           #
#========================================================================================#
#THESIS TOPIC: Trends and determinants of contraceptive use and method choice among young# 
#              Zimbabwean women from 1988 to 2015                                        #               
##########################################################################################


#=========================================================================================#
#                             DHS 1988 TO 2015 DATASETS                                   #
#Clear memory
rm(list=ls())

#----------------------#
#Set working directory
setwd("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes")

#---------------------------------------------#
#Read DHS data into R
#install.packages("haven")  #install package
library(haven) #load package
ZWDHS1988 <- read_dta("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/DHS data/ZW_1988_DHS_03012021_1233_148664/ZWIR01DT/ZWIR01FL.DTA") #1988 data
ZWDHS1994 <- read_dta("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/DHS data/ZW_1994_DHS_03012021_1234_148664/ZWIR31DT/ZWIR31FL.DTA") #1994 data
ZWDHS1999 <- read_dta("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/DHS data/ZW_1999_DHS_03012021_1234_148664/ZWIR42DT/ZWIR42FL.DTA") #1999 data
ZWDHS2005_06 <- read_dta("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/DHS data/ZW_2005-06_DHS_03012021_1235_148664/ZWIR52DT/ZWIR52FL.DTA")  #2005-6 data
ZWDHS2010_11 <- read_dta("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/DHS data/ZW_2010-11_DHS_03012021_1236_148664/ZWIR62DT/ZWIR62FL.DTA")  #2010-11 data
ZWDHS2015 <- read_dta("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/DHS data/ZW_2015_DHS_03012021_1231_148664/ZWIR72DT/ZWIR72FL.DTA")   #2015 data

#----------------------------------------#
#SELECT WOMEN AGED 15-24 
#install.packages("dplyr")  #install package 
library(dplyr) #load packages 

#1988 data
attributes(ZWDHS1988$v013)$labels  #variable labels
table(ZWDHS1988$v013)
mydata1988_filter <- dplyr::filter(ZWDHS1988, ZWDHS1988$v013<3)#selecting category 1 and 2
table(mydata1988_filter$v013) #checking if done correctly

#1994 data
attributes(ZWDHS1994$v013)$labels  #variable labels
table(ZWDHS1994$v013)
mydata1994_filter <- dplyr::filter(ZWDHS1994, ZWDHS1994$v013<3)#selecting category 1 and 2
table(mydata1994_filter$v013) #checking if done correctly

#1999 data
attributes(ZWDHS1999$v013)$labels  #variable labels
table(ZWDHS1999$v013)
mydata1999_filter <- dplyr::filter(ZWDHS1999, ZWDHS1999$v013<3)#selecting category 1 and 2
table(mydata1999_filter$v013) #checking if done correctly

#2005-06 data
attributes(ZWDHS2005_06$v013)$labels  #variable labels
table(ZWDHS2005_06$v013)
mydata2005_filter <- dplyr::filter(ZWDHS2005_06 , ZWDHS2005_06$v013<3)#selecting required category
table(mydata2005_filter$v013) #checking if done correctly

#2010-11 data
attributes(ZWDHS2010_11$v013)$labels  #variable labels
table(ZWDHS2010_11$v013)
mydata2010_filter <- dplyr::filter(ZWDHS2010_11, ZWDHS2010_11$v013<3)#selecting required category
table(mydata2010_filter$v013) #checking if done correctly

#2015 data
attributes(ZWDHS2015$v013)$labels  #variable labels
table(ZWDHS2015$v013)
mydata2015_filter <- dplyr::filter(ZWDHS2015, ZWDHS2015$v013<3)#selecting required category
table(mydata2015_filter$v013)

#----------------------------------------#
#RENAME SAMPLING UNIT VARIABLE                              
mydata1988_filter$ccluster <- mydata1988_filter$v001  #1988 data
mydata1994_filter$ccluster <- mydata1994_filter$v021  #1994 data
mydata1999_filter$ccluster <- mydata1999_filter$v021  #1999 data
mydata2005_filter$ccluster <- mydata2005_filter$v021  #2005 data
mydata2010_filter$ccluster <- mydata2010_filter$v021  #2010-11 data
mydata2015_filter$ccluster <- mydata2015_filter$v021  #2015 data

#-----------------------------------------------------------#
#RENAME STRATIFICATION VARIABLE                             
mydata1994_filter$sstrata <- mydata1994_filter$v023 #1994 data
mydata1999_filter$sstrata <- mydata1999_filter$v023 #1999 data
mydata2005_filter$sstrata <- mydata2005_filter$v023 #2005 data
mydata2010_filter$sstrata <- mydata2010_filter$v023 #2010-11 data
mydata2015_filter$sstrata <- mydata2015_filter$v023 #2015 data

#----------------------------------------#
#DEFINE SAMPLE WEIGHTS VARIABLE                             #
mydata1988_filter$weight <- mydata1988_filter$v005/1000000  #1988 data
mydata1994_filter$weight <- mydata1994_filter$v005/1000000  #1994 data
mydata1999_filter$weight <- mydata1999_filter$v005/1000000  #1999 data
mydata2005_filter$weight <- mydata2005_filter$v005/1000000  #2005 data
mydata2010_filter$weight <- mydata2010_filter$v005/1000000  #2010-11 data
mydata2015_filter$weight <- mydata2015_filter$v005/1000000  #2015 data

#----------------------------------------#
#SELECT REQUIRED VARIABLES  
library(dplyr) #load packages 
mydata1988 <- mydata1988_filter%>%select(caseid,ccluster,sstrata,weight,v012,v312,
                                         v313,v525,v101,v102,v106,v501,v218,v301,v605,
                                         v602,v714,v511,v213,v511,v130)
mydata1994 <- mydata1994_filter%>%select(caseid,ccluster,sstrata,weight,v012,v312,
                                         v313,v525,v024,v025,v106,v501,v218,v301,v605,
                                         v602,v714,v213,m10_1,v405,v511,v101,v511,v130)
mydata1999 <- mydata1999_filter%>%select(caseid,ccluster,sstrata,weight,v012,v312,
                                         v313,v525,v024,v025,v106,v501,v218,v301,v605,
                                         v602,v714,v730,v213,m10_1,v405,v511,v101,v511,v130)
mydata2005 <- mydata2005_filter%>%select(caseid,ccluster,sstrata,weight,v012,v312,
                                         v313,v525,v024,v025,v106,v501,v218,v301,v605,
                                         v602,v714,v730,v213,m10_1,v405,v511,v101,v511,v130)
mydata2010 <- mydata2010_filter%>%select(caseid,ccluster,sstrata,weight,v012,v312,
                                         v313,v525,v024,v025,v106,v501,v218,v301,v605,
                                         v602,v714,v730,v213,m10_1,v405,v511,v101,v511,v130)
mydata2015 <- mydata2015_filter%>%select(caseid,ccluster,sstrata,weight,v012,v312,
                                         v313,v525,v024,v025,v106,v501,v218,v301,v605,
                                         v602,v714,v730,v213,m10_1,v405,v511,v101,v511,v130)
                                          
#-------------------------------------------------------#
#CREATE CURRENT CONTRACEPTIVE USE VARIABLE        

#Note: 0 = Non-modern or none use and 1= modern contraceptive use

#install.packages("plyr")
library(plyr) #load package
#1988 data
attributes(mydata1988$v313)$labels  #variable labels
table(mydata1988$v313)  #checking categories in the variable
mydata1988$fp_use <- mapvalues(mydata1988$v313,c(0,1,2),c(0,0,1),
                               warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata1988$fp_use) #checking mapped values

#1994 data
attributes(mydata1994$v313)$labels
table(mydata1994$v313)
mydata1994$fp_use <- mapvalues(mydata1994$v313,c(0,1,2,3),c(0,0,0,1),
                               warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata1994$fp_use) #checking mapped values

#1999 data
attributes(mydata1999$v313)$labels
table(mydata1999$v313)
mydata1999$fp_use <- mapvalues(mydata1999$v313,c(0,1,2,3),c(0,0,0,1),
                               warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata1999$fp_use) #checking mapped values

#2005-06 data
attributes(mydata2005$v313)$labels
table(mydata2005$v313)
mydata2005$fp_use <- mapvalues(mydata2005$v313,c(0,1,2,3),c(0,0,0,1),
                               warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata2005$fp_use) #checking mapped values

#2010-11 data
attributes(mydata2010$v313)$labels
table(mydata2010$v313)
mydata2010$fp_use <- mapvalues(mydata2010$v313,c(0,2,3),c(0,0,1),
                               warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata2010$fp_use) #checking mapped values

#2015 data
attributes(mydata2015$v313)$labels
table(mydata2015$v313)
mydata2015$fp_use <- mapvalues(mydata2015$v313,c(0,2,3),c(0,0,1),
                               warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata2015$fp_use) #checking mapped values

#---------------------------------------------------------#
#CREATE CURRENT CONTRACEPTIVE METHOD CHOICE VARIABLE        

#Note:  0 = not using ;  1 = pill ; 2 = injections 
#       3 = male condom;   4 = Norplant/Implants

#1988
attributes(mydata1988$v312)$labels #variable labels
table(mydata1988$v312)  #checking categories in the variable
mydata1988$method_choice <- mapvalues(mydata1988$v312,c(0,1,2,3,5,8,9,10),c(0,1,5,2,3,5,5,5),
                                      warn_missing = TRUE) #re-coding values
table(mydata1988$method_choice)

#1994
attributes(mydata1994$v312)$labels
table(mydata1994$v312)
mydata1994$method_choice  <- mapvalues(mydata1994$v312,c(0,1,2,3,5,6,8,9,10,11),c(0,1,5,2,3,5,5,5,5,4),
                                       warn_missing = TRUE) #redefining values
table(mydata1994$method_choice)

#1999
attributes(mydata1999$v312)$labels
table(mydata1999$v312)
mydata1999$method_choice <- mapvalues(mydata1999$v312,c(0,1,3,5,8,9,10,11,13,14),c(0,1,2,3,5,5,5,4,5,5),
                                      warn_missing = TRUE) #redefining values
table(mydata1999$method_choice)

#2005
attributes(mydata2005$v312)$labels
table(mydata2005$v312)
mydata2005$method_choice <- mapvalues(mydata2005$v312,c(0,1,3,5,8,9,10,11,13,14),c(0,1,2,3,5,5,5,4,5,5),
                                      warn_missing = TRUE) #redefining values
table(mydata2005$method_choice)

#2010
attributes(mydata2010$v312)$labels
table(mydata2010$v312)
mydata2010$method_choice <- mapvalues(mydata2010$v312,c(0,1,2,3,5,9,11,13,14),c(0,1,5,2,3,5,4,5,5),
                                      warn_missing = TRUE) #redefining values
table(mydata2010$method_choice)

#2015
attributes(mydata2015$v312)$labels
table(mydata2015$v312)
mydata2015$method_choice  <- mapvalues(mydata2015$v312,c(0,1,2,3,5,8,9,11,13,14,16),c(0,1,5,2,3,5,5,4,5,5,5),
                                       warn_missing = TRUE) #redefining values
table(mydata2015$method_choice)

#----------------------------------------#
#CREATE EVER HAD SEX VARIABLE                              

#Note 0 = Never had sex, 1 = Ever had sex
#1988 data
attributes(mydata1988$v525)$labels #variable labels
table(mydata1988$v525)   #checking categories in variable
evsx_var <- mydata1988$v525  #saving values in variable v525 in a vector
evsx_var[evsx_var >=8 ] <- 1         #replacing values >=8 with 1
mydata1988$ever_had_sex <- evsx_var   #saving replaced values 
table(mydata1988$ever_had_sex)      #check if done correctly

#1994 data 
attributes(mydata1994$v525)$labels
table(mydata1994$v525)
evsx_var1 <- mydata1994$v525
evsx_var1[evsx_var1 >=9 ] <- 1         #replacing values >=9 with 1
mydata1994$ever_had_sex <- evsx_var1   #saving replaced values
table(mydata1994$ever_had_sex)       #check if done correctly

#1999 data
attributes(mydata1999$v525)$labels
table(mydata1999$v525)
evsx_var2 <- mydata1999$v525
evsx_var2[evsx_var2 >=10 ] <- 1         #replacing values >=10 with 1
mydata1999$ever_had_sex <- evsx_var2   #saving replaced values
table(mydata1999$ever_had_sex)      #check if done correctly

#2005-06 data
attributes(mydata2005$v525)$labels
table(mydata2005$v525)
evsx_var3 <- mydata2005$v525
evsx_var3[evsx_var3 >=8 ] <- 1         #replacing values >=8 with 1
mydata2005$ever_had_sex <- evsx_var3   #saving replaced values
table(mydata2005$ever_had_sex)      #check if done correctly

#2010-11 data
attributes(mydata2010$v525)$labels
table(mydata2010$v525)
evsx_var4 <- mydata2010$v525
evsx_var4[evsx_var4 >=9 ] <- 1         #replacing values >=9 with 1
mydata2010$ever_had_sex <- evsx_var4   #saving replaced values
table(mydata2010$ever_had_sex)      #check if done correctly

#2015 data
attributes(mydata2015$v525)$labels
table(mydata2015$v525)
evsx_var5 <- mydata2015$v525
evsx_var5[evsx_var5 >=8 ] <- 1         #replacing values >=8 with 1
mydata2015$ever_had_sex <- evsx_var5   #saving replaced values
table(mydata2015$ever_had_sex)      #check if done correctly

#----------------------------------------#
#RE-CODE REGION VARIABLE VALUES                                      

#1988 data
attributes(mydata1988$v101)$labels #variable labels
table(mydata1988$v101) #checking categories in the variable
mydata1988$region <- mapvalues(mydata1988$v101,c(0,1,2,3,4,5,6,7,8,9),c(1,2,3,4,5,6,7,8,0,10),
                               warn_missing = TRUE) #redefining values
table(mydata1988$region) #checking if values re-coded correctly

#1994 data
attributes(mydata1994$v024)$labels
table(mydata1994$v024)
bregion_var <- mydata1994$v024
bregion_var[bregion_var ==9] <-0    #re-code 9 as 0
mydata1994$region <- bregion_var 
table(mydata1994$region)

#1999 data
attributes(mydata1999$v024)$labels
table(mydata1999$v024)
cregion_var <- mydata1999$v024
cregion_var[cregion_var ==9] <-0    #re-code 9 as 0
mydata1999$region <- cregion_var 
table(mydata1999$region)

#2005-06 data
attributes(mydata2005$v024)$labels
table(mydata2005$v024)
dregion_var <- mydata2005$v024
dregion_var[dregion_var ==9] <-0    #re-code 9 as 0
mydata2005$region <- dregion_var 
table(mydata2005$region)

#2010-11 data
attributes(mydata2010$v024)$labels
table(mydata2010$v024)
eregion_var <- mydata2010$v024
eregion_var[eregion_var ==9] <- 0    #re-code 9 as 0
mydata2010$region <- eregion_var 
table(mydata2010$region)

#2015 data
attributes(mydata2015$v024)$labels
table(mydata2015$v024)
fregion_var <- mydata2015$v024
fregion_var[fregion_var ==9] <-0    #re-code 9 as 0
mydata2015$region <- fregion_var 
table(mydata2015$region)

#----------------------------------------#
#RENAME RESIDENCE VARIABLE                                  #
mydata1988$residence <- mydata1988$v102  #1988 data
mydata1994$residence <- mydata1994$v025 #1994 data
mydata1999$residence <- mydata1999$v025 #1999 data
mydata2005$residence <- mydata2005$v025 #2005-06 data
mydata2010$residence <- mydata2010$v025 #2010-11 data
mydata2015$residence <- mydata2015$v025 #2015 data

#----------------------------------------#
#RE-CODE EDUCATION VARIABLE                                 
#1988 data
attributes(mydata1988$v106)$labels  #variable labels
table(mydata1988$v106) #check categories in the variable
mydata1988$edu_level <- mapvalues(mydata1988$v106,c(0,1,2,3),c(0,0,1,1),
                                  warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata1988$edu_level) #checking mapped values

#1994 data
attributes(mydata1994$v106)$labels
table(mydata1994$v106)
mydata1994$edu_level <- mapvalues(mydata1994$v106,c(0,1,2,3),c(0,0,1,1),
                                  warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata1994$edu_level) #checking mapped values

#1999 data
attributes(mydata1999$v106)$labels
table(mydata1999$v106)
mydata1999$edu_level <- mapvalues(mydata1999$v106,c(0,1,2,3),c(0,0,1,1),
                                  warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata1999$edu_level) #checking mapped values

#2005-06 data
attributes(mydata2005$v106)$labels
table(mydata2005$v106)
mydata2005$edu_level <- mapvalues(mydata2005$v106,c(0,1,2,3),c(0,0,1,1),
                                  warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata2005$edu_level) #checking mapped values

#2010-11 data
attributes(mydata2010$v106)$labels
table(mydata2010$v106)
mydata2010$edu_level <- mapvalues(mydata2010$v106,c(0,1,2,3),c(0,0,1,1),
                                  warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata2010$edu_level) #checking mapped values

#2015 data
attributes(mydata2015$v106)$labels
table(mydata2015$v106)
mydata2015$edu_level <- mapvalues(mydata2015$v106,c(0,1,2,3),c(0,0,1,1),
                                  warn_missing = TRUE) #re-coding values and saving in a new column
table(mydata2015$edu_level) #checking mapped values

#----------------------------------------#
#RE-CODE MARITAL STATUS VARIABLE                            
#1988 data
attributes(mydata1988$v501)$labels #variable labels
table(mydata1988$v501)   #checking categories in the variable
mrt_var <- mydata1988$v501    #creating a vector
mrt_var[mrt_var ==4 ] <- 3         #replacing value 4 with 3
mydata1988$mrt_status <- mrt_var        #saving re-coded data
table(mydata1988$mrt_status)

#1994 data
attributes(mydata1994$v501)$labels#labels
table(mydata1994$v501)
mrt_var1 <- mydata1994$v501    #creating a vector
mrt_var1[mrt_var1 ==4 ] <- 3         #replacing value 4 with 3
mydata1994$mrt_status<- mrt_var1        #saving re-coded data
table(mydata1994$mrt_status)

#1999 data
attributes(mydata1999$v501)$labels #labels
table(mydata1999$v501)
mrt_var2 <- mydata1999$v501    #creating a vector
mrt_var2[mrt_var2 ==4 ] <- 3         #replacing value 4 with 3
mrt_var2[mrt_var2 ==5 ] <- 3         #replacing value 5 with 3
mydata1999$mrt_status <- mrt_var2        #saving re-coded data
table(mydata1999$mrt_status)

#2005-06 data
attributes(mydata2005$v501)$labels #labels
table(mydata2005$v501)
mrt_var3 <- mydata2005$v501    #creating a vector
mrt_var3[mrt_var3 ==4 ] <- 3         #replacing value 4 with 3
mrt_var3[mrt_var3 ==5 ] <- 3         #replacing value 5 with 3
mydata2005$mrt_status <- mrt_var3        #saving re-coded data
table(mydata2005$mrt_status)

#2010-11 data
attributes(mydata2010$v501)$labels #labels
table(mydata2010$v501)
mrt_var4 <- mydata2010$v501    #creating a vector
mrt_var4[mrt_var4 ==4 ] <- 3         #replacing value 4 with 3
mrt_var4[mrt_var4 ==5 ] <- 3         #replacing value 5 with 3
mydata2010$mrt_status <- mrt_var4        #saving re-coded data
table(mydata2010$mrt_status)

#2015 data
attributes(mydata2015$v501)$labels #labels
table(mydata2015$v501)
mrt_var5 <- mydata2015$v501    #creating a vector
mrt_var5[mrt_var5 ==4 ] <- 3         #replacing value 4 with 3
mrt_var5[mrt_var5 ==5 ] <- 3         #replacing value 5 with 3
mydata2015$mrt_status <- mrt_var5        #saving re-coded data
table(mydata2015$mrt_status)

#----------------------------------------#
#CREATING PARITY VARIABLE                                   
#1988 data
attributes(mydata1988$v218)$label #variable labels
table(mydata1988$v218) #checking categories in variable
mydata1988$parity <- mapvalues(mydata1988$v218,c(0,1,2,3,4,5,6),c(0,1,1,1,1,1,1),
                               warn_missing = TRUE) #redefining values
table(mydata1988$parity) #checking mapped values

#1994 data
attributes(mydata1994$v218)$label
table(mydata1994$v218)
mydata1994$parity  <- mapvalues(mydata1994$v218,c(0,1,2,3,4,5),c(0,1,1,1,1,1),
                                warn_missing = TRUE) #redefining values
table(mydata1994$parity) #checking mapped values

#1999 data
attributes(mydata1999$v218)$label
table(mydata1999$v218)
mydata1999$parity <- mapvalues(mydata1999$v218,c(0,1,2,3,4,6),c(0,1,1,1,1,1),
                               warn_missing = TRUE) #redefining values
table(mydata1999$parity) #checking mapped values

#2005-06 data
attributes(mydata2005$v218)$label
table(mydata2005$v218)
mydata2005$parity <- mapvalues(mydata2005$v218,c(0,1,2,3,4,5),c(0,1,1,1,1,1),
                               warn_missing = TRUE) #redefining values
table(mydata2005$parity) #checking mapped values

#2010-11 data
attributes(mydata2010$v218)$label
table(mydata2010$v218)
mydata2010$parity  <- mapvalues(mydata2010$v218,c(0,1,2,3,4),c(0,1,1,1,1),
                                warn_missing = TRUE) #redefining values
table(mydata2010$parity) #checking mapped values

#2015 data
attributes(mydata2015$v218)$label
table(mydata2015$v218)
mydata2015$parity <- mapvalues(mydata2015$v218,c(0,1,2,3,4),c(0,1,1,1,1),
                               warn_missing = TRUE) #redefining values
table(mydata2015$parity) #checking mapped values

#---------------------------------------------------#
#RE-CODE KNOWLEDGE OF CONTRACEPTIVES VARIABLE               
#1988 data
attributes(mydata1988$v301)$labels #variable labels
table(mydata1988$v301)  #checking categories in variable
mydata1988$fp_knowledge  <- mapvalues(mydata1988$v301,c(0,1,2),c(0,0,1),
                                      warn_missing = TRUE) #redefining values
table(mydata1988$fp_knowledge)  #checking if done correctly

#1994 data
attributes(mydata1994$v301)$labels
table(mydata1994$v301)
mydata1994$fp_knowledge <- mapvalues(mydata1994$v301,c(0,2,3),c(0,0,1),
                                     warn_missing = TRUE) #redefining values
table(mydata1994$fp_knowledge) #checking mapped values

#1999 data
attributes(mydata1999$v301)$labels
table(mydata1999$v301)
mydata1999$fp_knowledge <- mapvalues(mydata1999$v301,c(0,1,2,3),c(0,0,0,1),
                                     warn_missing = TRUE) #redefining values
table(mydata1999$fp_knowledge) #checking mapped values

#2005-06 data
attributes(mydata2005$v301)$labels
table(mydata2005$v301)
mydata2005$fp_knowledge <- mapvalues(mydata2005$v301,c(0,1,2,3),c(0,0,0,1),
                                     warn_missing = TRUE) #redefining values
table(mydata2005$fp_knowledge) #checking mapped values

#2010-11 data
attributes(mydata2010$v301)$labels
table(mydata2010$v301)
mydata2010$fp_knowledge <- mapvalues(mydata2010$v301,c(0,2,3),c(0,0,1),
                                     warn_missing = TRUE) #redefining values
table(mydata2010$fp_knowledge) #checking mapped values

#2015 data 
attributes(mydata2015$v301)$labels
table(mydata2015$v301)
z36 <- mapvalues(mydata2015$v301,c(0,2,3),c(0,0,1),
                 warn_missing = TRUE) #redefining values
mydata2015$fp_knowledge <- z36    #saving redefined values in the appropriate column
table(mydata2015$fp_knowledge) #checking mapped values


#----------------------------------------------------#
#RE-CODE DESIRE FOR MORE CHILDREN VARIABLE                  
#1988 data
attributes(mydata1988$v605)$labels  #variable labels
table(mydata1988$v605)  #checking categories in data
mydata1988$kids_desire <- mapvalues(mydata1988$v605,c(1,2,3,4,5,6),c(1,2,3,3,0,4),
                                    warn_missing = TRUE) #redefining values
table(mydata1988$kids_desire) #checking mapped values

#1994 data
attributes(mydata1994$v605)$labels
table(mydata1994$v605)
mydata1994$kids_desire <- mapvalues(mydata1994$v605,c(1,2,3,4,5,6,7,8),c(1,2,3,3,0,4,4,8),
                                    warn_missing = TRUE) #redefining values
table(mydata1994$kids_desire) #checking mapped values

#1999 data
attributes(mydata1999$v605)$labels
table(mydata1999$v605)
mydata1999$kids_desire <- mapvalues(mydata1999$v605,c(1,2,3,4,5),c(1,2,3,3,0),
                                    warn_missing = TRUE) #redefining values
table(mydata1999$kids_desire) #checking mapped values

#2005-06 data
attributes(mydata2005$v605)$labels
table(mydata2005$v605)
mydata2005$kids_desire  <- mapvalues(mydata2005$v605,c(1,2,3,4,5,7),c(1,2,3,3,0,4),
                                     warn_missing = TRUE) #redefining values
table(mydata2005$kids_desire) #checking mapped values

#2010-11 data
attributes(mydata2010$v605)$labels
table(mydata2010$v605)
mydata2010$kids_desire  <- mapvalues(mydata2010$v605,c(1,2,3,4,5,7),c(1,2,3,3,0,4),
                                     warn_missing = TRUE) #redefining values
table(mydata2010$kids_desire) #checking mapped values

#2015 data
attributes(mydata2015$v605)$labels
table(mydata2015$v605)
mydata2015$kids_desire <- mapvalues(mydata2015$v605,c(1,2,3,4,5,7),c(1,2,3,3,0,4),
                                    warn_missing = TRUE) #redefining values
table(mydata2015$kids_desire) #checking mapped values

#----------------------------------------------------#
#CREATE AGE GROUP VARIABLE                                  
#1988 data
mydata1988$age_grp <- mapvalues(mydata1988$v012,c(15,16,17,18,19,20,21,22,23,24),c(0,0,0,1,1,2,2,3,3,3),
                                warn_missing = TRUE) #redefining values
table(mydata1988$age_grp) #checking mapped values

#1994 data
mydata1994$age_grp <- mapvalues(mydata1994$v012,c(15,16,17,18,19,20,21,22,23,24),c(0,0,0,1,1,2,2,3,3,3),
                                warn_missing = TRUE) #redefining values
table(mydata1994$age_grp) #checking mapped values

#1999 data
mydata1999$age_grp <- mapvalues(mydata1999$v012,c(15,16,17,18,19,20,21,22,23,24),c(0,0,0,1,1,2,2,3,3,3),
                                warn_missing = TRUE) #redefining values
table(mydata1999$age_grp) #checking mapped values

#2005-06 data
mydata2005$age_grp <- mapvalues(mydata2005$v012,c(15,16,17,18,19,20,21,22,23,24),c(0,0,0,1,1,2,2,3,3,3),
                                warn_missing = TRUE) #redefining values
table(mydata2005$age_grp) #checking mapped values

#2010-11 data 
mydata2010$age_grp  <- mapvalues(mydata2010$v012,c(15,16,17,18,19,20,21,22,23,24),c(0,0,0,1,1,2,2,3,3,3),
                                 warn_missing = TRUE) #redefining values
table(mydata2010$age_grp) #checking mapped values

#2015 data
mydata2015$age_grp <- mapvalues(mydata2015$v012,c(15,16,17,18,19,20,21,22,23,24),c(0,0,0,1,1,2,2,3,3,3),
                                warn_missing = TRUE) #redefining values
table(mydata2015$age_grp) #checking mapped values

#----------------------------------------------------#
#SAVE STUDY POPULATION DATA AS EXCEL FILES                     
getwd()  #find the working directory path
path <- "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes" 
newfolder <- "Study pop data"
dir.create(file.path(dirname(path),newfolder)) #create folder to save datasets
write.csv(as.data.frame(mydata1988),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop1988.csv") #1988 data
write.csv(as.data.frame(mydata1994),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop1994.csv") #1994 data
write.csv(as.data.frame(mydata1999),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop1999.csv") #1999 data
write.csv(as.data.frame(mydata2005),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop2005.csv") #2005 data
write.csv(as.data.frame(mydata2010),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop2010.csv") #2010 data
write.csv(as.data.frame(mydata2015),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop2015.csv") #2015 data


#***********************************************************#
#SAMPLE SELECTION                                           #
#***********************************************************#

#-----------------------------------------#
#Remove women who never had sex

#1988 data
attributes(mydata1988$v525)$labels
table(mydata1988$v525)
mydata1988_filter1 <- dplyr::filter(mydata1988,mydata1988$v525>0)#select required values
table(mydata1988_filter1$v525) #checking if done correctly

#1994 data
attributes(mydata1994$v525)$labels
table(mydata1994$v525)
mydata1994_filter1 <- dplyr::filter(mydata1994,mydata1994$v525>0)#select required values
table(mydata1994_filter1$v525)

#1999 data
attributes(mydata1999$v525)$labels
table(mydata1999$v525)
mydata1999_filter1 <- dplyr::filter(mydata1999,mydata1999$v525>0)#select required values
table(mydata1999_filter1$v525)

#2005-06 data
attributes(mydata2005$v525)$labels
table(mydata2005$v525)
mydata2005_filter1 <- dplyr::filter(mydata2005,mydata2005$v525>0)#select required values
table(mydata2005_filter1$v525)

#2010-11 data
attributes(mydata2010$v525)$labels
table(mydata2010$v525)
mydata2010_filter1 <- dplyr::filter(mydata2010,mydata2010$v525>0)#select required values
table(mydata2010_filter1$v525)

#2015 data
attributes(mydata2015$v525)$labels
table(mydata2015$v525)
mydata2015_filter1 <- dplyr::filter(mydata2015,mydata2015$v525>0)#select required values
table(mydata2015_filter1$v525)

#------------------------------------------#
#Remove those currently pregnant

#1988 data
attributes(ZWDHS1988$v213)$labels #variable label
table(mydata1988_filter$v213)  
mydata1988_filter2 <- dplyr::filter(mydata1988_filter1,mydata1988_filter1$v213==0)#select required values
table(mydata1988_filter2$v213)

#1994 data
attributes(mydata1994$v213)$labels
table(mydata1994_filter$v213)
mydata1994_filter2 <- dplyr::filter(mydata1994_filter1,mydata1994_filter1$v213==0)#select required values
table(mydata1994_filter2$v213)

#1999 data
attributes(mydata1999$v213)$labels
table(mydata1999_filter$v213)
mydata1999_filter2 <- dplyr::filter(mydata1999_filter1,mydata1999_filter1$v213==0)#select required values
table(mydata1999_filter2$v213)

#2005-06 data
attributes(mydata2005$v213)$labels
table(mydata2005_filter1$v213)
mydata2005_filter2 <- dplyr::filter(mydata2005_filter1,mydata2005_filter1$v213==0)#select required values
table(mydata2005_filter2$v213)

#2010-11 data
attributes(mydata2010$v213)$labels
table(mydata2010_filter1$v213)
mydata2010_filter2 <- dplyr::filter(mydata2010_filter1,mydata2010_filter1$v213==0)#select required values
table(mydata2010_filter2$v213)

#2015 data
attributes(mydata2015$v213)$labels
table(mydata2015_filter1$v213)
mydata2015_filter2 <- dplyr::filter(mydata2015_filter1,mydata2015_filter1$v213==0)#select required values
table(mydata2015_filter2$v213)

#--------------------------------------------------#
#Remove women declared infecund and sterilized

#1988 data
attributes(mydata1988_filter2$v602)$labels
table(mydata1988_filter2$v602)
which(mydata1988_filter2$v602==5) #finding out the row numbers containing with value 5
mydata1988_filter3 <- mydata1988_filter2[-c(68, 172, 259, 283, 523, 861 ),] #remove rows
table(mydata1988_filter3$v602) #check if done correctly

#1994 data
attributes(mydata1994_filter1$v602)$labels
table(mydata1994_filter2$v602)
which(mydata1994_filter2$v602==5) #finding out the row numbers containing with value 5
which(mydata1994_filter2$v602==4) #finding out the row numbers containing with value 5
mydata1994_filter3 <- mydata1994_filter2[-c(225,593,757,1103,1224, 1237,716 ),] #remove rows
table(mydata1994_filter3$v602) #checking if done correctly

#1999 data
attributes(mydata1999_filter1$v602)$labels
table(mydata1999_filter2$v602)
which(mydata1999_filter2$v602==5) #finding out the row numbers containing value 5
which(mydata1999_filter2$v602==4) #finding out the row numbers containing value 4
mydata1999_filter3 <- mydata1999_filter2 #rename dataset 

#2005-06 data
attributes(mydata2005_filter1$v602)$labels
table(mydata2005_filter2$v602)
which(mydata2005_filter2$v602==5) #finding out the row numbers containing value 5
which(mydata2005_filter2$v602==4) #finding out the row numbers containing value 4
mydata2005_filter3 <- mydata2005_filter2[-c(21,423,745,1344,1585,1743,1890),] #remove rows
table(mydata2005_filter3$v602)

#2010-11 data
attributes(mydata2010_filter1$v602)$labels
table(mydata2010_filter2$v602)
which(mydata2010_filter2$v602==5) #finding out the row numbers containing value 5
which(mydata2010_filter2$v602==4) #finding out the row numbers containing value 4
mydata2010_filter3 <- mydata2010_filter2[-c(391,943,1080,1271,1275),] #remove rows
table(mydata2010_filter3$v602)

#2015 data
attributes(mydata2015_filter1$v602)$labels
table(mydata2015_filter2$v602)
which(mydata2015_filter2$v602==5) #finding out the row numbers containing value 5
which(mydata2015_filter2$v602==4) #finding out the row numbers containing value 4
mydata2015_filter3 <- mydata2015_filter2[-c(184,227,502,504,899,918,948),] #remove rows
table(mydata2015_filter3$v602)

#---------------------------------------------------#
#Save data in excel -Contraceptive use data
path <- "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes" 
newfolder <- "CP use data"
dir.create(file.path(dirname(path),newfolder)) #create folder to save datasets

write.csv(as.data.frame(mydata1988_filter3%>%select(caseid,ccluster,sstrata,weight,v012,
                                                    fp_use,age_grp,residence,region,v101,
                                                    edu_level,mrt_status,v218,parity,v106,
                                                    fp_knowledge,kids_desire,v714,v511)),
          "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1988.csv") #1988 data

write.csv(as.data.frame(mydata1994_filter3%>%select(caseid,ccluster,sstrata,weight,v012,
                                                    fp_use,age_grp,residence,region,v101,
                                                    edu_level,mrt_status,v218,parity,v106,
                                                    fp_knowledge,v602,kids_desire,v714,v511)),
          "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1994.csv") 

write.csv(as.data.frame(mydata1999_filter3%>%select(caseid,ccluster,sstrata,weight,v012,
                                                    fp_use,age_grp,residence,region,v101,
                                                    edu_level,mrt_status,v218,parity,v106,
                                                    fp_knowledge,v602,kids_desire,v714,cp_agediff,v511)),
          "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS1999.csv") 

write.csv(as.data.frame(mydata2005_filter3%>%select(caseid,ccluster,sstrata,weight,v012,
                                                    fp_use,age_grp,residence,region,v101,
                                                    edu_level,mrt_status,v218,parity,v106,
                                                    fp_knowledge,v602,kids_desire,v714,cp_agediff,v511)),
          "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2005.csv") 

write.csv(as.data.frame(mydata2010_filter3%>%select(caseid,ccluster,sstrata,weight,v012,
                                                    fp_use,age_grp,residence,region,v106,
                                                    edu_level,mrt_status,v218,parity,v101,
                                                    fp_knowledge,v602,kids_desire,v714,cp_agediff,v511)),
          "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2010.csv") 

write.csv(as.data.frame(mydata2015_filter3%>%select(caseid,ccluster,sstrata,weight,v012,
                                                    fp_use,age_grp,residence,region,v101,
                                                    edu_level,mrt_status,v218,parity,v106,
                                                    fp_knowledge,v602,kids_desire,v714,cp_agediff,v511)),
          "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP use data/DHS2015.csv") 


#**********************************************************#
#DESCRIPTION OF STUDY SAMPLE SELECTION                     #
#**********************************************************#

library(survey) #survey package

#-------------------------------#
#1988 
mydata1988_Design <- svydesign(mydata1988$ccluster,strata = mydata1988$sstrata,
                               weights=mydata1988$weight,data=mydata1988,nest = TRUE )
mydata1988filter1_Design <- svydesign(mydata1988_filter1$ccluster,strata = mydata1988_filter1$sstrata,
                                      weights=mydata1988_filter1$weight,data=mydata1988_filter1,nest = TRUE )
mydata1988filter2_Design <- svydesign(mydata1988_filter2$ccluster,strata = mydata1988_filter2$sstrata,
                                      weights=mydata1988_filter2$weight,data=mydata1988_filter2,nest = TRUE )
mydata1988filter3_Design <- svydesign(mydata1988_filter3$ccluster,strata = mydata1988_filter3$sstrata,
                                      weights=mydata1988_filter3$weight,data=mydata1988_filter3,nest = TRUE )


sum(svytable(~fp_use,mydata1988_Design)) #total women aged 15-24
svytable(~v525,mydata1988_Design)   #0 shows total number of women who never had sex
svytable(~v213,mydata1988filter1_Design)# 1 shows total number of women who are currently pregnant
svytable(~v602,mydata1988filter2_Design) #5 shows total number of women declared infecund
sum(svytable(~fp_use,mydata1988filter3_Design)) #total selected sample of sexually active women

#----------------------------#
#1994 
mydata1994_Design <- svydesign(mydata1994$ccluster,strata = mydata1994$sstrata,
                               weights=mydata1994$weight,data=mydata1994,nest = TRUE )
mydata1994filter1_Design <- svydesign(mydata1994_filter1$ccluster,strata = mydata1994_filter1$sstrata,
                                      weights=mydata1994_filter1$weight,data=mydata1994_filter1,nest = TRUE )
mydata1994filter2_Design <- svydesign(mydata1994_filter2$ccluster,strata = mydata1994_filter2$sstrata,
                                      weights=mydata1994_filter2$weight,data=mydata1994_filter2,nest = TRUE )
mydata1994filter3_Design <- svydesign(mydata1994_filter3$ccluster,strata = mydata1994_filter3$sstrata,
                                      weights=mydata1994_filter3$weight,data=mydata1994_filter3,nest = TRUE )


sum(svytable(~fp_use,mydata1994_Design)) #total women aged 15-24
svytable(~v525,mydata1994_Design) #0 shows total number of women who never had sex
svytable(~v213,mydata1994filter1_Design) # 1 shows total number of currently pregnant women 
svytable(~v602,mydata1994filter2_Design) # 4 and 5 shows total number of sterilized and women declared infecund respectively
sum(svytable(~fp_use,mydata1994filter3_Design)) # total number of sexually active women in selected sample

#1999 
mydata1999_Design <- svydesign(mydata1999$ccluster,strata = mydata1999$sstrata,
                               weights=mydata1999$weight,data=mydata1999,nest = TRUE )
mydata1999filter1_Design <- svydesign(mydata1999_filter1$ccluster,strata = mydata1999_filter1$sstrata,
                                      weights=mydata1999_filter1$weight,data=mydata1999_filter1,nest = TRUE )
mydata1999filter2_Design <- svydesign(mydata1999_filter2$ccluster,strata = mydata1999_filter2$sstrata,
                                      weights=mydata1999_filter2$weight,data=mydata1999_filter2,nest = TRUE )
mydata1999filter3_Design <- svydesign(mydata1999_filter3$ccluster,strata = mydata1999_filter3$sstrata,
                                      weights=mydata1999_filter3$weight,data=mydata1999_filter3,nest = TRUE )

sum(svytable(~fp_use,mydata1999_Design)) #total women age 15-24
svytable(~v525,mydata1999_Design) # 0 shows total number of women who never had sex
svytable(~v213,mydata1999filter1_Design) # 1 indicates total number of currently pregnant women
svytable(~v602,mydata1999filter2_Design) 
sum(svytable(~fp_use,mydata1999filter3_Design)) # total selecte sample


#2005 
mydata2005_Design <- svydesign(mydata2005$ccluster,strata = mydata2005$sstrata,
                               weights=mydata2005$weight,data=mydata2005,nest = TRUE )
mydata2005filter1_Design <- svydesign(mydata2005_filter1$ccluster,strata = mydata2005_filter1$sstrata,
                                      weights=mydata2005_filter1$weight,data=mydata2005_filter1,nest = TRUE )
mydata2005filter2_Design <- svydesign(mydata2005_filter2$ccluster,strata = mydata2005_filter2$sstrata,
                                      weights=mydata2005_filter2$weight,data=mydata2005_filter2,nest = TRUE )
mydata2005filter3_Design <- svydesign(mydata2005_filter3$ccluster,strata = mydata2005_filter3$sstrata,
                                      weights=mydata2005_filter3$weight,data=mydata2005_filter3,nest = TRUE )


sum(svytable(~fp_use,mydata2005_Design)) # total number of women aged 15-24
svytable(~v525,mydata2005_Design)        # 0 indicates number of women who never had sex
svytable(~v213,mydata2005filter1_Design) # 1 indicates number of currently pregnant women
svytable(~v602,mydata2005filter2_Design) # 5 indicates number of women declared infecund
sum(svytable(~fp_use,mydata2005filter3_Design)) # total selected sample of sexually active women


#2010 
mydata2010_Design <- svydesign(mydata2010$ccluster,strata = mydata2010$sstrata,
                               weights=mydata2010$weight,data=mydata2010,nest = TRUE )
mydata2010filter1_Design <- svydesign(mydata2010_filter1$ccluster,strata = mydata2010_filter1$sstrata,
                                      weights=mydata2010_filter1$weight,data=mydata2010_filter1,nest = TRUE )
mydata2010filter2_Design <- svydesign(mydata2010_filter2$ccluster,strata = mydata2010_filter2$sstrata,
                                      weights=mydata2010_filter2$weight,data=mydata2010_filter2,nest = TRUE )
mydata2010filter3_Design <- svydesign(mydata2010_filter3$ccluster,strata = mydata2010_filter3$sstrata,
                                      weights=mydata2010_filter3$weight,data=mydata2010_filter3,nest = TRUE )

sum(svytable(~fp_use,mydata2010_Design))
svytable(~v525,mydata2010_Design)
svytable(~v213,mydata2010filter1_Design)
svytable(~v602,mydata2010filter2_Design)
sum(svytable(~fp_use,mydata2010filter3_Design))

#2015 
mydata2015_Design <- svydesign(mydata2015$ccluster,strata = mydata2015$sstrata,
                               weights=mydata2015$weight,data=mydata2015,nest = TRUE )
mydata2015filter1_Design <- svydesign(mydata2015_filter1$ccluster,strata = mydata2015_filter1$sstrata,
                                      weights=mydata2015_filter1$weight,data=mydata2015_filter1,nest = TRUE )
mydata2015filter2_Design <- svydesign(mydata2015_filter2$ccluster,strata = mydata2015_filter2$sstrata,
                                      weights=mydata2015_filter2$weight,data=mydata2015_filter2,nest = TRUE )
mydata2015filter3_Design <- svydesign(mydata2015_filter3$ccluster,strata = mydata2015_filter3$sstrata,
                                      weights=mydata2015_filter3$weight,data=mydata2015_filter3,nest = TRUE )

sum(svytable(~fp_use,mydata2015_Design))
svytable(~v525,mydata2015_Design)
svytable(~v213,mydata2015filter1_Design)
svytable(~v602,mydata2015filter2_Design)
sum(svytable(~fp_use,mydata2015filter3_Design))


#***********************************************************#
#SAMPLE SELECTION-Contraceptive method choice dataset       #
#***********************************************************#
#-------------------------------------#
#Remove other methods

#19888
table(mydata1988_filter3$method_choice)
mydata1988_filter4 <- dplyr::filter(mydata1988_filter3, mydata1988_filter3$method_choice<5)

#1994
table(mydata1994_filter3$method_choice)
mydata1994_filter4 <- dplyr::filter(mydata1994_filter3, mydata1994_filter3$method_choice<5)

#1999
table(mydata1999_filter3$method_choice)
mydata1999_filter4 <- dplyr::filter(mydata1999_filter3, mydata1999_filter3$method_choice<5)

#2005
table(mydata2005_filter3$method_choice)
mydata2005_filter4 <- dplyr::filter(mydata2005_filter3, mydata2005_filter3$method_choice<5)

#2010
table(mydata2010_filter3$method_choice)
mydata2010_filter4 <- dplyr::filter(mydata2010_filter3, mydata2010_filter3$method_choice<5)

#2015
table(mydata2015_filter3$method_choice)
mydata2015_filter4 <- dplyr::filter(mydata2015_filter3, mydata2015_filter3$method_choice<5)

#Select required variables
DHS1988_MC <- mydata1988_filter4%>%select(caseid,ccluster,sstrata,weight,age_grp,
                                          residence,region,edu_level,mrt_status,parity,v714,
                                          fp_knowledge,kids_desire,method_choice)
DHS1994_MC <- mydata1994_filter4%>%select(caseid,ccluster,sstrata,weight,age_grp,
                                          residence,region,edu_level,mrt_status,parity,v714,
                                          fp_knowledge,kids_desire,method_choice)
DHS1999_MC <- mydata1999_filter4%>%select(caseid,ccluster,sstrata,weight,age_grp,
                                          residence,region,edu_level,mrt_status,parity,v714,
                                          fp_knowledge,kids_desire,cp_agediff,method_choice)
DHS2005_MC <- mydata2005_filter4%>%select(caseid,ccluster,sstrata,weight,age_grp,
                                          residence,region,edu_level,mrt_status,parity,v714,
                                          fp_knowledge,kids_desire,cp_agediff,method_choice)
DHS2010_MC <- mydata2010_filter4%>%select(caseid,ccluster,sstrata,weight,age_grp,
                                          residence,region,edu_level,mrt_status,parity,v714,v602,
                                          fp_knowledge,kids_desire,cp_agediff,method_choice)
DHS2015_MC <- mydata2015_filter4%>%select(caseid,ccluster,sstrata,weight,age_grp,
                                          residence,region,edu_level,mrt_status,parity,v714,
                                          fp_knowledge,kids_desire,cp_agediff,method_choice)

#---------------------------------------------------#
#Save data in excel
path <- "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes" 
newfolder <- "CP method choice data"
dir.create(file.path(dirname(path),newfolder)) #create folder to save datasets
write.csv(as.data.frame(DHS1988_MC),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS1988_MC.csv") 
write.csv(as.data.frame(DHS1994_MC),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS1994_MC.csv") 
write.csv(as.data.frame(DHS1999_MC),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS1999_MC.csv") 
write.csv(as.data.frame(DHS2005_MC),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS2005_MC.csv") 
write.csv(as.data.frame(DHS2010_MC),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS2010_MC.csv") 
write.csv(as.data.frame(DHS2015_MC),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP method choice data/DHS2015_MC.csv") 


#***********************************************************#
#CREATE CONTRACEPTIVE NEED DATA                             #
#***********************************************************#

#---------------------------------------------#
#1994 

#Select women who want to delay or stop childbearing, currently pregnant and amenorrheic women
mydata1994_cpneeds <- dplyr::filter(mydata1994_filter1, (v605==2 |v605==5 | v213==1 | v405==1))

#Extract women who want delay & stop childbearing, and create contraceptive need variable
mydata1994_ds <- dplyr::filter(mydata1994_cpneeds, v605==2 |v605==5) #extract
mydata1994_ds$cp_needs <- ifelse(mydata1994_ds$v605==2 & mydata1994_ds$fp_use==1, 1,
                                 ifelse(mydata1994_ds$v605==5 & mydata1994_ds$fp_use==1,1,0)) #contraceptive need variable
table(mydata1994_ds$cp_needs) #checking

#Extract remaining dataset
mydata1994_remaining <- dplyr::filter(mydata1994_cpneeds,(v605!=2 & v605!=5)) #Extract

#Extract currently pregnant women from remaining dataset, and create contraceptive need variable
mydata1994_pregnant <- dplyr::filter(mydata1994_remaining,v213==1 ) #Extract
mydata1994_pregnant$cp_needs <- ifelse(mydata1994_pregnant$m10_1==1,1,
                                       ifelse(mydata1994_pregnant$m10_1==2,0,
                                              ifelse(mydata1994_pregnant$m10_1==3,0,NA))) #contraceptive need variable
table(mydata1994_pregnant$cp_needs)

#Extract currently amenorrheic women from remaining dataset, and create contraceptive need variable
mydata1994_amn <- dplyr::filter(mydata1994_remaining,v405==1)
mydata1994_amn$cp_needs <- ifelse(mydata1994_amn$m10_1==1,1,
                                  ifelse(mydata1994_amn$m10_1==2,0,
                                         ifelse(mydata1994_amn$m10_1==3,0,NA)))
table(mydata1994_amn$cp_needs) #checking

#Merge data sets
Need_1994 <- rbind(mydata1994_pregnant,mydata1994_amn)
CPNEEDS1994 <- rbind(Need_1994,mydata1994_ds)

table(CPNEEDS1994$cp_needs) #checking


#---------------------------------------------#
#1999 

#Select women who want to delay or stop childbearing, currently pregnant and amenorrheic women
mydata1999_cpneeds <- dplyr::filter(mydata1999_filter1, (v605==2 |v605==5 | v213==1 | v405==1))

#Extract women who want delay & stop childbearing, and create contraceptive need variable
mydata1999_ds <- dplyr::filter(mydata1999_cpneeds, v605==2 |v605==5) #extract
mydata1999_ds$cp_needs <- ifelse(mydata1999_ds$v605==2 & mydata1999_ds$fp_use==1, 1,
                                 ifelse(mydata1999_ds$v605==5 & mydata1999_ds$fp_use==1,1,0)) #contraceptive need variable
table(mydata1999_ds$cp_needs) #checking

#Extract remaining dataset
mydata1999_remaining <- dplyr::filter(mydata1999_cpneeds,(v605!=2 & v605!=5)) #Extract

#Extract currently pregnant women from remaining dataset, and create contraceptive need variable
mydata1999_pregnant <- dplyr::filter(mydata1999_remaining,v213==1 ) #Extract
mydata1999_pregnant$cp_needs <- ifelse(mydata1999_pregnant$m10_1==1,1,
                                       ifelse(mydata1999_pregnant$m10_1==2,0,
                                              ifelse(mydata1999_pregnant$m10_1==3,0,NA))) #contraceptive need variable
table(mydata1999_pregnant$cp_needs)

#Extract currently amenorrheic women from remaining dataset, and create contraceptive need variable
mydata1999_amn <- dplyr::filter(mydata1999_remaining,v405==1)
mydata1999_amn$cp_needs <- ifelse(mydata1999_amn$m10_1==1,1,
                                  ifelse(mydata1999_amn$m10_1==2,0,
                                         ifelse(mydata1999_amn$m10_1==3,0,NA)))
table(mydata1999_amn$cp_needs) #checking

#Merge data sets
Need_1999 <- rbind(mydata1999_pregnant,mydata1999_amn)
CPNEEDS1999 <- rbind(Need_1999,mydata1999_ds)
table(CPNEEDS1999$cp_needs) #checking


#---------------------------------------------#
#2005 

#Select women who want to delay or stop childbearing, currently pregnant and amenorrheic women
mydata2005_cpneeds <- dplyr::filter(mydata2005_filter1, (v605==2 |v605==5 | v213==1 | v405==1))

#Extract women who want delay & stop childbearing, and create contraceptive need variable
mydata2005_ds <- dplyr::filter(mydata2005_cpneeds, v605==2 |v605==5) #extract
mydata2005_ds$cp_needs <- ifelse(mydata2005_ds$v605==2 & mydata2005_ds$fp_use==1, 1,
                                 ifelse(mydata2005_ds$v605==5 & mydata2005_ds$fp_use==1,1,0)) #contraceptive need variable
table(mydata2005_ds$cp_needs) #checking

#Extract remaining dataset
mydata2005_remaining <- dplyr::filter(mydata2005_cpneeds,(v605!=2 & v605!=5)) #Extract

#Extract currently pregnant women from remaining dataset, and create contraceptive need variable
mydata2005_pregnant <- dplyr::filter(mydata2005_remaining,v213==1 ) #Extract
mydata2005_pregnant$cp_needs <- ifelse(mydata2005_pregnant$m10_1==1,1,
                                       ifelse(mydata2005_pregnant$m10_1==2,0,
                                              ifelse(mydata2005_pregnant$m10_1==3,0,NA))) #contraceptive need variable
table(mydata2005_pregnant$cp_needs)

#Extract currently amenorrheic women from remaining dataset, and create contraceptive need variable
mydata2005_amn <- dplyr::filter(mydata2005_remaining,v405==1)
mydata2005_amn$cp_needs <- ifelse(mydata2005_amn$m10_1==1,1,
                                  ifelse(mydata2005_amn$m10_1==2,0,
                                         ifelse(mydata2005_amn$m10_1==3,0,NA)))
table(mydata2005_amn$cp_needs) #checking

#Merge data sets
Need_2005 <- rbind(mydata2005_pregnant,mydata2005_amn)
CPNEEDS2005 <- rbind(Need_2005,mydata2005_ds)
table(CPNEEDS2005$cp_needs) #checking


#---------------------------------------------#
#2010 

#Select women who want to delay or stop childbearing, currently pregnant and amenorrheic women
mydata2010_cpneeds <- dplyr::filter(mydata2010_filter1, (v605==2 |v605==5 | v213==1 | v405==1))

#Extract women who want delay & stop childbearing, and create contraceptive need variable
mydata2010_ds <- dplyr::filter(mydata2010_cpneeds, v605==2 |v605==5) #extract
mydata2010_ds$cp_needs <- ifelse(mydata2010_ds$v605==2 & mydata2010_ds$fp_use==1, 1,
                                 ifelse(mydata2010_ds$v605==5 & mydata2010_ds$fp_use==1,1,0)) #contraceptive need variable
table(mydata2010_ds$cp_needs) #checking

#Extract remaining dataset
mydata2010_remaining <- dplyr::filter(mydata2010_cpneeds,(v605!=2 & v605!=5)) #Extract

#Extract currently pregnant women from remaining dataset, and create contraceptive need variable
mydata2010_pregnant <- dplyr::filter(mydata2010_remaining,v213==1 ) #Extract
mydata2010_pregnant$cp_needs <- ifelse(mydata2010_pregnant$m10_1==1,1,
                                       ifelse(mydata2010_pregnant$m10_1==2,0,
                                              ifelse(mydata2010_pregnant$m10_1==3,0,NA))) #contraceptive need variable
table(mydata2010_pregnant$cp_needs)

#Extract currently amenorrheic women from remaining dataset, and create contraceptive need variable
mydata2010_amn <- dplyr::filter(mydata2010_remaining,v405==1)
mydata2010_amn$cp_needs <- ifelse(mydata2010_amn$m10_1==1,1,
                                  ifelse(mydata2010_amn$m10_1==2,0,
                                         ifelse(mydata2010_amn$m10_1==3,0,NA)))
table(mydata2010_amn$cp_needs) #checking

#Merge data sets
Need_2010 <- rbind(mydata2010_pregnant,mydata2010_amn)
CPNEEDS2010 <- rbind(Need_2010,mydata2010_ds)
table(CPNEEDS2010$cp_needs) #checking


#---------------------------------------------#
#2015 

#Select women who want to delay or stop childbearing, currently pregnant and amenorrheic women
mydata2015_cpneeds <- dplyr::filter(mydata2015_filter1, (v605==2 |v605==5 | v213==1 | v405==1))

#Extract women who want delay & stop childbearing, and create contraceptive need variable
mydata2015_ds <- dplyr::filter(mydata2015_cpneeds, v605==2 |v605==5) #extract
mydata2015_ds$cp_needs <- ifelse(mydata2015_ds$v605==2 & mydata2015_ds$fp_use==1, 1,
                                 ifelse(mydata2015_ds$v605==5 & mydata2015_ds$fp_use==1,1,0)) #contraceptive need variable
table(mydata2015_ds$cp_needs) #checking

#Extract remaining dataset
mydata2015_remaining <- dplyr::filter(mydata2015_cpneeds,(v605!=2 & v605!=5)) #Extract

#Extract currently pregnant women from remaining dataset, and create contraceptive need variable
mydata2015_pregnant <- dplyr::filter(mydata2015_remaining,v213==1 ) #Extract
mydata2015_pregnant$cp_needs <- ifelse(mydata2015_pregnant$m10_1==1,1,
                                       ifelse(mydata2015_pregnant$m10_1==2,0,
                                              ifelse(mydata2015_pregnant$m10_1==3,0,NA))) #contraceptive need variable
table(mydata2015_pregnant$cp_needs)

#Extract currently amenorrheic women from remaining dataset, and create contraceptive need variable
mydata2015_amn <- dplyr::filter(mydata2015_remaining,v405==1)
mydata2015_amn$cp_needs <- ifelse(mydata2015_amn$m10_1==1,1,
                                  ifelse(mydata2015_amn$m10_1==2,0,
                                         ifelse(mydata2015_amn$m10_1==3,0,NA)))
table(mydata2015_amn$cp_needs) #checking

#Merge data sets
Need_2015 <- rbind(mydata2015_pregnant,mydata2015_amn)
CPNEEDS2015 <- rbind(Need_2015,mydata2015_ds)
table(CPNEEDS2015$cp_needs) #checking


#---------------------------------------------------#
#Save data in excel
path <- "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes" 
newfolder <- "CP Need data"
dir.create(file.path(dirname(path),newfolder)) #create folder to save datasets
write.csv(as.data.frame(CPNEEDS1994),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP Need data/CPNEED1994.csv") 
write.csv(as.data.frame(CPNEEDS1999),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP Need data/CPNEED1999.csv") 
write.csv(as.data.frame(CPNEEDS2005),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP Need data/CPNEED2005.csv") 
write.csv(as.data.frame(CPNEEDS2010),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP Need data/CPNEED2010.csv") 
write.csv(as.data.frame(CPNEEDS2015),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/CP Need data/CPNEED2015.csv") 


#***********************************************************#
#CREATE AGE AT FIRST SEX DATA                               #
#***********************************************************#

#Create age at first sex variable


#--------------------------------------#
# 1) Replace age at first sex categorized as at first union with age at marriage

#1988 data
attributes(mydata1988_filter1$v525)$labels
table(mydata1988_filter1$v525)
mydata1988_filter1$age_sex <- ifelse(mydata1988_filter1$v525==96,mydata1988_filter1$v511,
                                     mydata1988_filter1$v525) #substitute values categorized as 96 
#with values from column v511
table(mydata1988_filter1$age_sex) #check if done correctly

#1994 data
attributes(mydata1994_filter1$v525)$labels
table(mydata1994_filter1$v525)
mydata1994_filter1$age_sex <- ifelse(mydata1994_filter1$v525==96,mydata1994_filter1$v511,
                                     mydata1994_filter1$v525) #substitute values categorized as 96 
#with values from column v511
table(mydata1994_filter1$age_sex) #check if done correctly

#1999 data
attributes(mydata1999_filter1$v525)$labels
table(mydata1999_filter1$v525)
mydata1999_filter1$age_sex <- ifelse(mydata1999_filter1$v525==96,mydata1999_filter1$v511,
                                     mydata1999_filter1$v525) #substitute values categorized as 96 
#with values from column v511
table(mydata1999_filter1$age_sex) #check if done correctly

#2005-06 data
attributes(mydata2005_filter1$v525)$labels
table(mydata2005_filter1$v525)
mydata2005_filter1$age_sex <- ifelse(mydata2005_filter1$v525==96,mydata2005_filter1$v511,
                                     mydata2005_filter1$v525) #substitute values categorized as 96 
#with values from column v511
table(mydata2005_filter1$age_sex) #check if done correctly

#2010-11 data
attributes(mydata2010_filter1$v525)$labels
table(mydata2010_filter1$v525)
mydata2010_filter1$age_sex <- ifelse(mydata2010_filter1$v525==96,mydata2010_filter1$v511,
                                     mydata2010_filter1$v525) #substitute values categorized as 96 
#with values from column v511
table(mydata2010_filter1$age_sex) #check if done correctly

#2015 data
attributes(mydata2015_filter1$v525)$labels
table(mydata2015_filter1$v525)
mydata2015_filter1$age_sex <- ifelse(mydata2015_filter1$v525==96,mydata2015_filter1$v511,
                                     mydata2015_filter1$v525) #substitute values categorized as 96 
#with values from column v511
table(mydata2015_filter1$age_sex) #check if done correctly

#---------------------------------------------------------#
# 2) Group age at first sex

#1988 data
table(mydata1988_filter1$v525)
mydata1988_filter1$AFS_grp <-  mapvalues(mydata1988_filter1$v525,c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                         c(0,0,0,0,0,0,0,1,1,1,2,2,2,3,3,4),warn_missing = TRUE) #redefining values
table(mydata1988_filter1$AFS_grp) #checking if done correctly

#1994 data
table(mydata1994_filter1$v525)
mydata1994_filter1$AFS_grp <-  mapvalues(mydata1994_filter1$v525,c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,96),
                                         c(0,0,0,0,0,0,1,1,1,2,2,2,3,3,4,4,5),warn_missing = TRUE) #redefining values
table(mydata1994_filter1$AFS_grp) #checking if done correctly

#1999 data
table(mydata1999_filter1$v525)
mydata1999_filter1$AFS_grp <-  mapvalues(mydata1999_filter1$v525,c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,96),
                                         c(0,0,0,0,0,1,1,1,2,2,2,3,3,4,4,5),warn_missing = TRUE) #redefining values
table(mydata1999_filter1$AFS_grp) #checking if done correctly

#2005-06 data
table(mydata2005_filter1$v525)
mydata2005_filter1$AFS_grp  <-  mapvalues(mydata2005_filter1$v525,c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,96),
                                          c(0,0,0,0,0,0,0,1,1,1,2,2,2,3,3,4,4,5),warn_missing = TRUE) #redefining values
table(mydata2005_filter1$AFS_grp) #checking if done correctly

#2010-11 data
table(mydata2010_filter1$v525)
mydata2010_filter1$AFS_grp <-  mapvalues(mydata2010_filter1$v525,c(9,11,12,13,14,15,16,17,18,19,20,21,22,23,96),
                                         c(0,0,0,0,0,1,1,1,2,2,2,3,3,4,5),warn_missing = TRUE) #redefining values
table(mydata2010_filter1$AFS_grp) #checking if done correctly

#2015 data
table(mydata2015_filter1$v525)
mydata2015_filter1$AFS_grp <-  mapvalues(mydata2015_filter1$v525,c(8,11,12,13,14,15,16,17,18,
                                                                   19,20,21,22,23,24,96),
                                         c(0,0,0,0,0,1,1,1,2,2,2,3,3,4,4,5),warn_missing = TRUE)#redefining values
table(mydata2015_filter1$AFS_grp) #checking if done correctly


#----------------------------------------------#
# 3) Remove women with category "97"=inconsistent and "98"=don't know

#1988 data
table(mydata1988_filter1$v525)
AFS_filter1988  <- dplyr::filter(mydata1988_filter1,mydata1988_filter1$v525<97) #select required values
table(AFS_filter1988$v525)

#1994 data
table(mydata1994_filter1$v525)
AFS_filter1994  <- dplyr::filter(mydata1994_filter1,mydata1994_filter1$v525<97) #select required values
table(AFS_filter1994$v525)

#1999 data
table(mydata1999_filter1$v525)
AFS_filter1999  <- dplyr::filter(mydata1999_filter1,mydata1999_filter1$v525<97) #select required values
table(AFS_filter1999$v525)

#2005-06 data
table(mydata2005_filter1$v525)
AFS_filter2005  <- dplyr::filter(mydata2005_filter1,mydata2005_filter1$v525<97) #select required values
table(AFS_filter2005$v525)

#2010-11 data
table(mydata2010_filter1$v525)
AFS_filter2010  <- dplyr::filter(mydata2010_filter1,mydata2010_filter1$v525<97) #select required values
table(AFS_filter2010$v525)
AFS_filter2010  <- dplyr::filter(AFS_filter2010,AFS_filter2010$v525!=26) #remove 26 
table(AFS_filter2010$v525)

#2015 data
table(mydata2015_filter1$v525)
AFS_filter2015  <- dplyr::filter(mydata2015_filter1,mydata2015_filter1$v525<97) #select required values
table(AFS_filter2015$v525)


#---------------------------------------------------#
#Save data in excel
path <- "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes" 
newfolder <- "AFS data"
dir.create(file.path(dirname(path),newfolder)) #create folder to save datasets
write.csv(as.data.frame(AFS_filter1988),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS1988.csv") #1988 data
write.csv(as.data.frame(AFS_filter1994),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS1994.csv") #1994 data
write.csv(as.data.frame(AFS_filter1999),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS1999.csv") #1999 data
write.csv(as.data.frame(AFS_filter2005),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS2005.csv") #2005 data
write.csv(as.data.frame(AFS_filter2010),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS2010.csv") #2010 data
write.csv(as.data.frame(AFS_filter2015),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/AFS data/AFS2015.csv") #2015 data
#=========================================================================================#





#=========================================================================================#
#                           DHS 2010-11, 2015 and MICS 2009,2014 DATASETS                 #


#Clear memory
rm(list=ls())

#Set working directory
setwd("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes")

#Load packages
library(haven)
library(dplyr) 
library(plyr)

#------------------------#
#Read data into R

#MICS data
MICs_2009 <- read_sav("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Unicef MICs data/Zimbabwe 2009 MICS_Datasets/Zimbabwe_Datasets/Zimbabwe MIMS 2009 SPSS Datasets/wm.sav")
View(MICs_2009)
MICs_2014 <- read_sav("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Unicef MICs data/Zimbabwe 2014 MICS_Datasets/Zimbabwe_MICS5_Datasets/Zimbabwe MICS 2014 SPSS Datasets/wm.sav")
View(MICs_2014)

#DHS data
DHS2010 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop2010.csv")[,-1] #2010 data
DHS2015 <- read.csv("C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Study pop data/stdy_pop2015.csv")[,-1] #2015 data


#----------------------------------#
#SELECT WOMEN AGED 15-24   

#MICS 2009 
attributes(MICs_2009$wm9)$label
table(MICs_2009$wm9)
MICs2009 <- dplyr::filter(MICs_2009, MICs_2009$wm9<25)#selecting required category
table(MICs2009$wm9)

#MICS 2014 
attributes(MICs_2014$WB2)$label
table(MICs_2014$WB2)
MICs2014 <- dplyr::filter(MICs_2014, MICs_2014$WB2<25)#selecting required category
table(MICs2014$WB2)

#------------------------------------------#
#CREATE GROUPED AGE VARIABLE 

#MICS 2009 
table(MICs2009$wm9)
MICs2009$age_grp <- mapvalues(MICs2009$wm9,c(15,16,17,18,19,20,21,22,23,24),
                              c(0,0,0,1,1,1,2,2,3,3),warn_missing = TRUE) #redefining values
table(MICs2009$age_grp)

#MICS 2014 
table(MICs2014$WB2)
MICs2014$age_grp <- mapvalues(MICs2014$WB2,c(15,16,17,18,19,20,21,22,23,24),
                              c(0,0,0,1,1,1,2,2,3,3),warn_missing = TRUE) #redefining values
table(MICs2014$age_grp)

#----------------------------------------#
#CREATE STRATUM VARIABLE 

#MICS 2009 
attributes(MICs2009$hh6)$label #Area
table(MICs2009$hh6)
attributes(MICs2009$hh7)$label #Province
table(MICs2009$hh7)

x <- MICs2009$hh6   #create area vector
y <- MICs2009$hh7   #create province vector
w <- ifelse(x==1 & y==0,0,   
            ifelse(x==2 & y==0,10,
                   ifelse(x==1 & y==1,1,
                          ifelse(x==2 & y==1,11,
                                 ifelse(x==1 & y==2,2,
                                        ifelse(x==2 & y==2,12,
                                               ifelse(x==1 & y==3,3,
                                                      ifelse(x==2 & y==3,13,
                                                             ifelse(x==1 & y==4,4,
                                                                    ifelse(x==2 & y==4,14,
                                                                           ifelse(x==1 & y==5,5,
                                                                                  ifelse(x==2 & y==5,15,
                                                                                         ifelse(x==1 & y==6,6,
                                                                                                ifelse(x==2 & y==6,16,
                                                                                                       ifelse(x==1 & y==7,7,
                                                                                                              ifelse(x==2 & y==7,17,
                                                                                                                     ifelse(x==1 & y==8,8,
                                                                                                                            ifelse(x==2 & y==8,18,
                                                                                                                                   ifelse(x==1 & y==9,9,
                                                                                                                                          ifelse(x==2 & y==9,19,99))))))))))))))))))) )
MICs2009$stratum <- w  #save variable in dataframe
table(MICs2009$stratum)

#MICS 2014 
attributes(MICs2014$HH6)$label #Area
table(MICs2014$HH6)
attributes(MICs2014$HH7)$label #Province
table(MICs2014$HH7)

x1 <- MICs2014$HH6   #create a vector
y1 <- MICs2014$HH7   #create a vector
w1 <- ifelse(x1==1 & y1==0,0,
             ifelse(x1==2 & y1==0,10,
                    ifelse(x1==1 & y1==1,1,
                           ifelse(x1==2 & y1==1,11,
                                  ifelse(x1==1 & y1==2,2,
                                         ifelse(x1==2 & y1==2,12,
                                                ifelse(x1==1 & y1==3,3,
                                                       ifelse(x1==2 & y1==3,13,
                                                              ifelse(x1==1 & y1==4,4,
                                                                     ifelse(x1==2 & y1==4,14,
                                                                            ifelse(x1==1 & y1==5,5,
                                                                                   ifelse(x1==2 & y1==5,15,
                                                                                          ifelse(x1==1 & y1==6,6,
                                                                                                 ifelse(x1==2 & y1==6,16,
                                                                                                        ifelse(x1==1 & y1==7,7,
                                                                                                               ifelse(x1==2 & y1==7,17,
                                                                                                                      ifelse(x1==1 & y1==8,8,
                                                                                                                             ifelse(x1==2 & y1==8,18,
                                                                                                                                    ifelse(x1==1 & y1==9,9,
                                                                                                                                           ifelse(x1==2 & y1==9,19,99))))))))))))))))))) )
MICs2014$stratum <- w1 #save variable in data frame
table(MICs2014$stratum)

#--------------------------------------# 
#Save MICS in excel
path <- "C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/Thesis_final codes" 
newfolder <- "MICS vs DHS data"
dir.create(file.path(dirname(path),newfolder)) #create folder to save datasets
write.csv(as.data.frame(MICs2009),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/Sty_MICs2009.csv") #2009 data
write.csv(as.data.frame(MICs2014),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/Sty_MICs2014.csv") #2014 data


#-----------------------------------------------#
#REMOVE MISSING VALUES FOR FP USE       

#MICS 2009
attributes(MICs2009$cp2)$labels
table(MICs2009$cp2)
MICs2009_filter <- dplyr::filter(MICs2009, MICs2009$cp2<9)#selecting required category
table(MICs2009_filter$cp2)

#MICS 2014 
attributes(MICs2014$CP2)$labels
table(MICs2014$CP2)
MICs2014_filter <- dplyr::filter(MICs2014, MICs2014$CP2<9)#selecting required category
table(MICs2014_filter$CP2)  

#-----------------------------------------------#
#REMOVE CURRENTLY PREGNANT WOMEN               

#-----------------------------#
#MICS data
attributes(MICs2009_filter$cp1)$labels
table(MICs2009_filter$cp1)
MICs2009_filter1 <- dplyr::filter(MICs2009_filter, MICs2009_filter$cp1==2) #Remove category 8
table(MICs2009_filter1$cp1)

#2014 data
attributes(MICs2014_filter$CP1)$labels
table(MICs2014_filter$CP1)
MICs2014_filter1 <- dplyr::filter(MICs2014_filter, MICs2014_filter$CP1==2)#remove category 8 and 9
table(MICs2014_filter1$CP1)


#DHS 2010 
table(DHS2010$v213)
DHS2010_filter1 <- dplyr::filter(DHS2010,DHS2010$v213==0)#select required values
table(DHS2010_filter1$v213)

#DHS 2015 
table(DHS2015$v213)
DHS2015_filter1 <- dplyr::filter(DHS2015,DHS2015$v213==0)#select required values
table(DHS2015_filter1$v213)

#----------------------------------------------#
#REMOVE WOMEN WHO NEVER HAD SEX                

#MICS 2014 
attributes(MICs2014_filter1$SB1)$labels
table(MICs2014_filter1$SB1)
MICs2014_filter2 <- dplyr::filter(MICs2014_filter1, MICs2014_filter1$SB1>0)#selecting required category
table(MICs2014_filter2$SB1)

#DHS 2015  
table(DHS2015_filter1$v525)
DHS2015_filter2 <- dplyr::filter(DHS2015_filter1,DHS2015_filter1$v525>0)#select required values
table(DHS2015_filter2$v525)

#----------------------------------------------#
#CREATE CONTRACEPTIVE USE VARIABLE --DHS       

#DHS 2010 
table(DHS2010_filter1$v313)
cp_var <- DHS2010_filter1$v313         #creating a vector
cp_var[cp_var == 2  ] <- 1       #replacing 2 with 1
cp_var[cp_var == 3  ] <- 1       #replacing 3 with 1
DHS2010_filter1$cp_use <- cp_var    #saving the replaced values 
table(DHS2010_filter1$cp_use)

#DHS 2015 
table(DHS2015_filter2$v313)
cp_var1 <- DHS2015_filter2$v313         #creating a vector
cp_var1[cp_var1 == 2 ] <- 1       #replacing 2 with 1
cp_var1[cp_var1 == 3 ] <- 1       #replacing 3 with 1
DHS2015_filter2$cp_use <- cp_var1    #saving the replaced values 
table(DHS2015_filter2$cp_use)


#------------------------------------#
#SAVE DATA IN EXCEL                            
write.csv(as.data.frame(MICs2009_filter1),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/MICs2009.csv") # MICS 2009 data
write.csv(as.data.frame(MICs2014_filter2),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/MICs2014.csv") # MICS 2014 data
write.csv(as.data.frame(DHS2010_filter1),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/DHS2010_COMP.csv") # DHS2010 data
write.csv(as.data.frame(DHS2015_filter2),"C:/Users/Audrey/OneDrive - University of Cape Town/Masters/Second year/Data analysis/MICS vs DHS data/DHS2015_COMP.csv") # DHS2015 data
#=========================================================================================#


##########################################################################################
#                           END OF DATA CLEANING                                         #
##########################################################################################
