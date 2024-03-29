# setting the working directory
setwd("D:/upgrad/capstone project")

# Loading necessary libraries
library("dplyr")
library("ggplot2")

# loading the credit bureu data
cb <- read.csv("Credit Bureau data.csv",header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)

# loading the demographics data
demo <- read.csv("Demographic data.csv",header = TRUE, na.strings = "NA",stringsAsFactors = FALSE)

################### exploring the demographics data ###########################
summary(demo)
# we see that from the data of all the applicants following are missing:
# gender for 2 applicants
# marital status of 6 applicants
# education for 119 of the applicants
# profession for 14 of the applicants
# type of residence for 8 of the applicants

# 1425 NA values are present in the performance tag

# features available in the data for analysis
colnames(demo)
#"Application.ID"                              "Age"                                        
#"Gender"                                      "Marital.Status..at.the.time.of.application."
#"No.of.dependents"                            "Income"                                     
#"Education"                                   "Profession"                                 
#"Type.of.residence"                           "No.of.months.in.current.residence"          
#"No.of.months.in.current.company"             "Performance.Tag"             

###################### Analyzing each of the columns independently #########################

################# application id column #####################
sum(is.na(demo$Application.ID)) 
# 0
sum(duplicated(demo$Application.ID))
# there are 3 duplicate application IDs present

demo[which(duplicated(demo$Application.ID)),1]
# 765011468 653287861 671989187

################# age column #####################
sum(is.na(demo$Age)) 
# 0
# plotting age distribution
ggplot(demo, aes(demo$Age))+geom_bar()

# checking the outliers
quantile(demo$Age,seq(0,1,0.01))

# checking the boxplot
boxplot(demo$Age)

summary(demo$Age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-3.00   37.00   45.00   44.94   53.00   65.00

# there is a negative value present in the age column

################# gender column #####################
sum(is.na(demo$Gender)) 
# 0
# and from summary we know that the gender is missing for two of the applicants

################# marital status column #####################
sum(is.na(demo$Marital.Status..at.the.time.of.application.)) 
# 0

# from summary we know that marital status for 6 of the applicants is missing

################# no. of dependents column #####################
sum(is.na(demo$No.of.dependents)) 
# 3

summary(demo$No.of.dependents)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.000   2.000   3.000   2.865   4.000   5.000       3 


################# income column #####################
sum(is.na(demo$Income)) 
# 0

summary(demo$Income)
#3Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-0.5    14.0    27.0    27.2    40.0    60.0 

# there is an entry of negative income value


################# education column #####################
sum(is.na(demo$Education)) 
# 0

summary(demo$Education)
# there are 119 records for which education is not defined


################# profession column #####################
sum(is.na(demo$Profession)) 
# 0

summary(demo$Profession)
# profession is not defined for 14 of the records


################# type of residence column #####################
sum(is.na(demo$Type.of.residence)) 
# 0

summary(demo$Type.of.residence)
# type of residence is missing for 8 of the records


################# no. of months in current residence column #####################
sum(is.na(demo$No.of.months.in.current.residence)) 
# 0

summary(demo$No.of.months.in.current.residence)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#6.00    6.00   11.00   34.56   60.00  126.00 
# all records seems fine


################# no. of months in current company column #####################
sum(is.na(demo$No.of.months.in.current.company)) 
# 0

summary(demo$No.of.months.in.current.company)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.00   16.00   34.00   33.96   51.00  133.00

# all records seems okay

################# performance tag column #####################
sum(is.na(demo$Performance.Tag)) 
# 1425

summary(demo$Performance.Tag)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0000  0.0000  0.0000  0.0422  0.0000  1.0000    1425 
########################################################################################

# following are the observations found in the demographics data
# 1. There are present duplicates in the application id column
# 2. There are present outliers in the age column
# 3. Missing values in the Gender column
# 4. Missing values in Marital status
# 5. NAs in the No. of dependent column
# 6. Outliers present in the Income column
# 7. Missing values in the education column
# 8. Missing values in profession column
# 9. Missing values in type of residence
# 10. NAs in the performance tag column

########################################################################################
# treating the data for the above abnormalities

# removing the rows with duplicate application id
demo <- demo[-which(duplicated(demo$Application.ID)),]

# removing all the rows containing negative values of age
demo <- demo[-which(demo$Age <= 0),]

# removing the rows that miss the gender
demo <- demo[-which(demo$Gender == ""),]
demo$Gender <- as.factor(demo$Gender)
summary(demo$Gender)

# removing the rows that miss marital status
demo <- demo[-which(demo$Marital.Status..at.the.time.of.application. == ""),]
demo$Marital.Status..at.the.time.of.application. <- as.factor(demo$Marital.Status..at.the.time.of.application.)
summary(demo$Marital.Status..at.the.time.of.application.)

# removing the rows containing NAs in No. Of Dependents column
demo <- demo[-which(is.na(demo$No.of.dependents)),]
demo$No.of.dependents <- as.factor(demo$No.of.dependents)
summary(demo$No.of.dependents)

# removing all the rows containing negative values of income
demo <- demo[-which(demo$Income <= 0),]
summary(demo$Income)

# removing the rows that miss education
demo <- demo[-which(demo$Education == ""),]
demo$Education <- as.factor(demo$Education)
summary(demo$Education)
# missing could fall under others as well, please check and do accordingly

# removing the rows that miss profession
demo <- demo[-which(demo$Profession == ""),]
demo$Profession <- as.factor(demo$Profession)
summary(demo$Profession)

# removing the rows that miss type of residence
demo <- demo[-which(demo$Type.of.residence == ""),]
demo$Type.of.residence <- as.factor(demo$Type.of.residence)
summary(demo$Type.of.residence)
# missing could fall under others as well, please check and do accordingly

# removing all the rows containing NAs in performance tag
demo <- demo[-which(is.na(demo$Performance.Tag)),]
demo$Performance.Tag <- as.factor(demo$Performance.Tag)
summary(demo$Performance.Tag)

############################# Data Preparation Ends Here #################################