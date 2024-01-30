#Load the data and library ---------------------------------------------------
library(caTools)
mydata <-read.csv("dataset_admissions.csv")
#-----------------------------------------------------------------------------
#Inspect the structure of the data -------------------------------------------

str(mydata)
#----------------------------------------------------------------------------
#Split the data into train and test -----------------------------------------
sample <- sample.split(mydata, SplitRatio = 0.8)
train = subset(mydata, sample == "TRUE")
test = subset(mydata, sample == "FALSE")

#-------------------------------------------------------------------------------
#all categorical data change to factor ---------------------------------------

mydata$admit <- as.factor(mydata$admit)
#mydata$rank <- as.factor(mydata$rank)

str(test)
str(train)

#--------------------------------------------------------------------------------
#Train the model and inspect the model summary--------------------------------

mymodel <- glm(admit ~ gpa + rank, data = train, family = 'binomial')
summary(mymodel)

#Test the model for accuracy -------------------------------------------------

pred <- predict(mymodel,test,type = "response")

confmatr <- table(actual_value = test$admit, predicted_value = pred > 0.5 )
confmatr
(confmatr[[1,1]] + confmatr[[2,2]])/sum(confmatr)

#--------------------------------------------------------------------------