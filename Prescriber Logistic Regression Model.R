#Import dataset from github
x <- read.csv("https://raw.githubusercontent.com/adim7/Zhjondon/master/prescript.csv")
head(x, n = 5)

#Convert Gender variable to dummy variable and store in function "gender"
gender <- model.matrix(~Gender-1, data = x)
head(gender, n = 5)

#Add dummy variables "gender" to data frame x
x$FEMALE <- gender[ ,1]
x$MALE <- gender[ ,2]
head(x, n = 5)

#Convert Region variable to dummy variable and store in function "region"
region <- model.matrix(~Region-1, data = x)
head(region, n = 5)

#Add dummy variables "region" to data frame x
x$MW <- region[ ,1]
x$NE <- region[ ,2]
x$S <- region[ ,4]
x$W <- region [ ,5]
head(x, n = 5)

#Convert Specialty variable to dummy variable and store in function "desig"
credSpec <- model.matrix(~Specialty-1, data = x)
head(credSpec, n = 5)

#Add dummy variables "desig" to data frame x
x$DDS <- credSpec[ ,1]
x$DO <- credSpec[ ,2]
x$MD <- credSpec[ ,3]
x$NP <- credSpec[ ,4]
x$PA <- credSpec[ ,5]
head(x, n = 5)

dim(x)
#Removed first 4 variables to view modified dataset (x.mod) for regression model
#x.mod <- x[-10][-16][-1:-5]
#head(x.mod, n = 5)


#just a test function
pc4 <- subset(x,select=c(1,6,7,8,10,11,12,13,14,15,16,17,18,19))
head(pc4, n = 5)


pc4 <- pc4[,-1]

set.seed(12345)

#Select random sample for Training data
training <- sample(1:nrow(pc4), 0.6*nrow(pc4))

#Apportion Training and Test Sets and isolate response variable(Opioids and/or Opioids.Prescriber)

##Opioids as response variable - Training 60% of the data
pc4.training <- pc4[training,-3]
pc4.training.results <- pc4[training,3]

#--------------------------------------------------------------------------------
##Opioid.Prescriber as response variable - Training 60% of the data
#pc4.training <- pc4[training,-8]
#pc4.training.results <- pc4[training,8]
#--------------------------------------------------------------------------------


##Opioids as response variable - Training 40% of the data
pc4.test = pc4[-training,-3]
pc4.test.results = pc4[-training,3]


#---------------------------------------------------------------------------------
##Opioid.Prescriber as response variable - Training 40% of the data
#pc4.test = pc4[-training,-8]
#pc4.test.results = pc4[-training,8]
#---------------------------------------------------------------------------------


#Logistic Regression Model using all the variables, modify as necessary

##Model with Opioids as response variable
pc4.lr <- glm(Opioid.P ~ ., family=binomial(link='logit'),data=pc4[training,])

#Model1 Output
summary(pc4.lr)
AIC(pc4.lr) #Akaike's Information Criteria. Penalizes the includion of additional models to the variables by increasing the error
BIC(pc4.lr) #Bayesian Informati0n Criteria. A variant of AIC that issue stronger penalties for introducing additional of variables
            #to the model


#-----------------------------------------------------------------------------------
##Model with Opioid.Prescriber as response variable
#pc4.lr <- glm(Opioid.P ~ ., family=binomial(link='logit'),data=pc4[training,])

#Model2 Output
#summary(pc4.lr)
AIC(pc4.lr) #Akaike's Information Criteria. Penalizes the includion of additional models to the variables by increasing the error
BIC(pc4.lr) #Bayesian Informati0n Criteria. A variant of AIC that issue stronger penalties for introducing additional of variables
            #to the model
#-----------------------------------------------------------------------------------

#Obtain the probabilities of each variable in the test set having the response variable
pc4.test.probabilities <- predict(pc4.lr, pc4.test,type = "response")


#Convert the probabilities to binary 0/1 classifications
pc4.lr.classifications <- round(pc4.test.probabilities, 0)

#Compute the proportion of classifications on the test set that were correct
sum(pc4.lr.classifications == pc4.test.results) / length(pc4.test.results)

#Show the classification confusion matrix for the test set
table(pc4.lr.classifications, pc4.test.results)

#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
##To classify new data points, we first need to import the new data
##Can we find 2015 data with similar variable to validate out models?
#Predict function to obtain the probabilities
#predict(pc4.lr, pc4new, type="response")
#Convert to binary classifications
#round(predict(pc4.lr, pc4new, type="response"),0)
