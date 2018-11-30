# Python Notebook - Project Predictive Models

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
#x$DO <- credSpec[ ,2]
x$MD <- credSpec[ ,3]
x$NP <- credSpec[ ,4]
x$PA <- credSpec[ ,5]
head(x, n = 5)

dim(x)
#Removed first 4 variables to view modified dataset (x.mod) for regression model
#x.mod <- x[-10][-16][-1:-5]
#head(x.mod, n = 5)


#just a test function
pc4 <- subset(x,select=c(1,6,7,8,10,11,12,13,14,15,16,17,18))
head(pc4, n = 5)



# - notes
## - Sub notes
### - function out for verification 



library("tree")

#pc4 <- read.csv() #Import final version of cleaned data set PrescribeClean4.0
#head(pc4, n = 5

#Confirm class_dimensions
#class(pc4)
#dim(pc4)
#summary(pc4)

#Remove NPI column
pc4 <-pc4[,-1]

#Set seed function
set.seed(12345)

#Select random sample for Training data
training <- sample(1:nrow(pc4), 0.6*nrow(pc4))
###training

#Apportion Training and Test Sets and isolate response variable(Opioids and/or Opioids.Prescriber)

##Opioids as response variable - Training 60% of the data
#pc4.training <- pc4[training,-3]
#pc4.training.results <- pc4[training,3]

##Opioid.Prescriber as response variable - Training 60% of the data
pc4.training <- pc4[training,-8]
pc4.training.results <- pc4[training,8]

##Opioids as response variable - Training 40% of the data
#pc4.test = pc4[-training,-3]
#pc4.test.results = pc4[-training,3]

##Opioid.Prescriber as response variable - Training 40% of the data
pc4.test = pc4[-training,-8]
pc4.test.results = pc4[-training,8]

#Classification Model using all the variables with arbitrary mindev parameter, modify as necessary

##Model with Opioids as response variable
#pc4.tree <- tree(Opioids ~ ., data=pc4[training,], mindev=0.05)

#Visualize model
#plot(pc4.tree)
#text(pc4.tree, cex=0.9)#Modify font size as needed


##Model with Opioid.Prescriber as response variable
pc4.tree <- tree(Opioid.P ~ ., data=pc4[training,], mindev=0.001)

#Visualize model
plot(pc4.tree)
text(pc4.tree, cex=0.5)#Modify font size as needed


#Obtain proportions of 1's from the training set in the tree endpoint for each data point in the test set
#("-training" tells R to use the rows not in the training set)
pc4.tree.proportions <- predict(pc4.tree, pc4[-training, -3])
###pc4.tree.proportions

#Round out each proportion to 0 or 1, obtaining the binary classifications
pc4.tree.classifications <- round(pc4.tree.proportions,0)

#Compute the proportion of classifications on the test set that were correct
accuracy <- sum(pc4.tree.classifications == pc4.test.results) / nrow(pc4[-training,])
accuracy

error.rate <- 1 - accuracy
error.rate

#Show the classification confusion matrix for the test set
table(pc4.tree.classifications, pc4.test.results)

#To classify new data points, we first need to import the new data
##Can we find 2015 data with similar variable to validate out models?
#Predict function to obtain the proportions
#predict(pc4.tree,pc4new)
#Convert them to binary classifications
#round(predict(DC.tree,DCnew),0)




