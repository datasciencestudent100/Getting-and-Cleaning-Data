
# Project for Getting and Cleaning Data
# Author: James Chen
# Date: 7/25/2014

# Requirement:
#   
# Create one R script called run_analysis.R that does the following. 
# 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


ProjectDirectory = getwd()
DataDirectory = "UCI HAR Dataset/"
dataFile = "dataset.RData"

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
                 "data.zip", quiet = TRUE, mode = "wb")  
unzip("data.zip")

setwd(DataDirectory)
  # Read labels
  temp = read.table("activity_labels.txt", sep = "") 

#  names(temp)
  activityLabels = as.character(temp$V2)

# Read features
  temp = read.table("features.txt", sep = "")
  attributeNames = temp$V2
  
  Xtrain = read.table("train/X_train.txt", sep = "")

# names(Xtrain)
  names(Xtrain) = attributeNames


  Ytrain = read.table("train/y_train.txt", sep = "")

  #head(Ytrain)
  names(Ytrain) = "Activity"

  # Factorization
  Ytrain$Activity = as.factor(Ytrain$Activity)

# Requirement: Uses descriptive activity names to name the activities in the data set
# Provides access to the levels attribute of a variable
  levels(Ytrain$Activity) = activityLabels

  trainSubjects = read.table("train/subject_train.txt", sep = "")
  names(trainSubjects) = "subject"

  trainSubjects$subject = as.factor(trainSubjects$subject)
  train = cbind(Xtrain, trainSubjects, Ytrain)


  #head(train)
  
###################################################################################
  # Read test data

  Xtest = read.table("test/X_test.txt", sep = "")
  names(Xtest) = attributeNames

  Ytest = read.table("test/y_test.txt", sep = "")
  names(Ytest) = "Activity"

# Requirement: Uses descriptive activity names to name the activities in the data set
# Provides access to the levels attribute of a variable
  Ytest$Activity = as.factor(Ytest$Activity)
  levels(Ytest$Activity) = activityLabels
  
  testSubjects = read.table("test/subject_test.txt", sep = "")
  names(testSubjects) = "subject"

  testSubjects$subject = as.factor(testSubjects$subject)
  test = cbind(Xtest, testSubjects, Ytest)
  
  #writes an external representation of R objects to the specified file. 
  save(train, test, file = dataFile)
  # remove objests
  rm(train, test, temp, Ytrain, Ytest, Xtrain, Xtest, trainSubjects, testSubjects, 
     activityLabels, attributeNames)

###################################################################################
load(dataFile)
setwd(ProjectDirectory)
#numPredictors = ncol(train) - 2
numColumns = ncol(train) - 2

# Data Summary
# 
# summary(train$subject)
# summary(test$subject)

#names(train)

train$Partition = "Train"
test$Partition = "Test"

# Requirement 1:  Merges the training and the test sets to create one data set.
all = rbind(train, test)  # combine sets for visualization
all$Partition = as.factor(all$Partition)

#nrow(test)
#nrow(train)

# nrow(all)
# 10299

#rm(all)  # recover memory
########################################################################################################
# Requirement: Extracts only the measurements on the mean and standard deviation for each measurement. 


library(plyr)
#install.packages("reshape");
library(reshape)

#head(all)
cindex <-grepl(pattern = "mean\\(|std\\(", names(all), perl=TRUE)

# cindex[562:564]
# length(cindex)

# INcluding "subject" "Activity" and "Partition"
cindex[ (length(cindex)-2):  (length(cindex)) ] <- TRUE
extract<-all[, cindex]

save(train, test, extract, file = dataFile)



#head(extract)
#dim(extract)
#[1] 10299    69
##########################################################################################################
# Requirement:  Creates a second, independent tidy data set with the average of each variable for each activity 
# and each subject. 
# This is the second table. It is created by summarizing the data in the first.

load(dataFile)
setwd(ProjectDirectory)
# getwd()

#head(extract)
#dim(extract)
#[1] 10299    69

#Melting data frames for creating a tidy file

tidydata <- melt(extract,id=67:69,measure.vars=1:66)

# dim(tidydata)
# head(tidydata)
# 
# names(tidydata)
# [1] "subject"   "Activity"  "Partition" "variable"  "value"    

newtidydata<-ddply(tidydata,.(subject,Activity,Partition,variable),summarize,value=mean(value))

# dim(newtidydata)
# [1] 11880     5
# 
# 
# head(newtidydata,100)

write.table(newtidydata, "mytidydata.txt", sep="\t") 


