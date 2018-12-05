#Cleaning Prescriber info Dataset for Analytics Project

#Descriptive statistics detailing original dataset
prescriber <- read.csv("https://raw.githubusercontent.com/adim7/Project620/master/prescriber-info%202.csv")
head(prescriber, n = 5)

#Sorting dataset by NPI
sort.prescriber <- prescriber[order(prescriber$NPI),]
head(sort.prescriber, n = 5)

agg.col <- colSums(sort.prescriber[,6:255])
head(agg.col, n= 5)

agg.row <- rowSums(sort.prescriber[6:255])
head(agg.row, n =5)

#Aggregating all drugs and categorizing in opioids and non opioids
drugs <- rowSums(sort.prescriber[,c(6:255)])

opioids <- rowSums(sort.prescriber[,c(7,40,60,88,106,108,150,161,162,187,188,189,236)])

n.opioids <- drugs - opioids

drugs.table <- data.frame(opioids, n.opioids, drugs)
head(drugs.table, n = 5)


#Condensing dataset with aggregated drug values
PrC1 <- data.frame(sort.prescriber[,-6:-255], drugs.table[ ,-3])
head(PrC1, n = 5)

#Export PrC to excel to filter and categorize 'States' variable by Regions & Specialty by Credentials
library("csv")
export.PrC <- write.csv(PrC, "/Users/AdimKris/Desktop/R/PrC")#You can insert your own working directory here in the ".../filename"
PrC2 <- read.csv("https://raw.githubusercontent.com/adim7/Project620/master/PrC_.csv")
head(PrC2, n =5)


#Converting categorical variables to dummy variables
#Gender
gender <- model.matrix(~Gender-1, data = PrC2)
#PrC2$FEMALE <- gender[ ,1]
PrC2$MALE <- gender[ ,2]
#Region 
region <- model.matrix(~Region-1, data = PrC2)
PrC2$MW <- region[ ,1]
PrC2$NE <- region[ ,2]
#PrC2$O <- region[ ,3]
PrC2$S <- region[ ,4]
PrC2$W <- region [ ,5]
#Specialty 
specialty <- model.matrix(~Specialty-1, data = PrC2)
PrC2$DDS <- specialty[ ,1]
#PrC2$DO <- specialty[ ,2]
PrC2$MD <- specialty[ ,3]
PrC2$NP <- specialty[ ,4]
PrC2$PA <- specialty[ ,5]

head(PrC2, n = 5)
dim(PrC2)

#Converting opioids and non opioid variables to dummy variables using mean prescriptions to measure
#over presription
#Calculating mean value
avg.opioids <- mean(PrC2$Opioids)
avg.n_opioids <- mean(PrC2$N.Opioids)

#Assigning binary classification, where >ave = 1 and <ave = 0
OO.Av <- ifelse(PrC2$Opioids > avg.opioids, 1, 0)
NO.Av <- ifelse(PrC2$N.Opioids > avg.n_opioids, 1, 0)

#Adding new classes to data frame
PrC3 <- data.frame(PrC2, OO.Av, NO.Av)

#Removed variables to view modified dataset (PrC.mod) for regression model
PrC4 <- PrC3[ ,-2:-7]
head(PrC4, n = 5)


PrC5 <- PrC4[,-1]
head(PrC5)

#Final number of variables for analysis
dim(PrC5)

#Export cleaned version (categorical variables) to csv for folder model building and analysis
write.csv(PrC5, "/Users/AdimKris/Desktop/R/PrC5")#You can insert your own working directory here in the ".../filename"

