##Course3, Peer-graded Assignment: Getting and Cleaning Data Course Project

#####
###Part 1 of question complete to "1. Merges the training and the test sets to create one data set."###
#####

##Defining Library for the program
library(curl)
library(RCurl)
library(dplyr)
library(lubridate)
library(tidyr)
library(plyr)

##clean up R memory before executing the code
rm(list=ls())

## Set Working Directory
setwd("/Users/arpanharit/Documents/Coursera/3/")

## Unzip File
zipfile <- "getdata_projectfiles_UCI HAR Dataset.zip"
unzip(zipfile, overwrite = TRUE)

##Sets the working directory to "directory"
setwd("/Users/arpanharit/Documents/Coursera/3/UCI HAR Dataset/")

## Read all file
activity_labels <-  read.table("./activity_labels.txt",header = FALSE, col.names = c("Id","Type"))
features <-  read.table("./features.txt",header = FALSE)

##Download train files and store in the destination
subject <- read.table("./train/subject_train.txt",header = FALSE, col.names = c("subject"))
x <- read.table("./train/X_train.txt",header = FALSE,col.names = features[,2])
y <- read.table("./train/y_train.txt",header = FALSE, col.names = c("Id"))

## column bind the data
output_train <- cbind(subject,x,y)

##adding column type to store training data
output_train[,"Experiment"] <- "Train"

##Download test files and store in the destination
subject <- read.table("./test/subject_test.txt",header = FALSE, col.names = c("subject"))
x <- read.table("./test/X_test.txt",header = FALSE,col.names = features[,2])
y <- read.table("./test/y_test.txt",header = FALSE, col.names = c("Id"))

## column bind the data
output_test <- cbind(subject,x,y)

##adding column type to store training data
output_test[,"Experiment"] <- "Test"

##binding training and test data
output <- rbind(output_train,output_test)

##remove all object to free up space
rm("zipfile","output_test","output_train","x","y","subject")

#####
#####Part 2 of question 2. Extracts only the measurements on the mean and standard deviation for each measurement.###
#####

## store all column names in all_column_names
all_column_names <- colnames(output)

## identify column names that covers mean & standard deviation
extract_mean_std <- output[,grepl("mean|std|subject|Id|Experiment",all_column_names,ignore.case = TRUE)]

##remove all object to free up space
rm("all_column_names")

#####
#####Part 3 of question 3. Uses descriptive activity names to name the activities in the data set###
#####

## Join the activity lables using ID
extract_mean_std <- join(extract_mean_std,activity_labels,by="Id",match="first")

## Remove the first column
extract_mean_std <- extract_mean_std[,-1]

#####
#####Part 4 of question 4. Appropriately labels the data set with descriptive variable names.
#####

##Remove () from the column names
names(extract_mean_std) <- gsub("\\(|\\)", "", names(extract_mean_std), perl  = TRUE)

##Remove . from the column names
names(extract_mean_std) <- gsub("\\.", "", names(extract_mean_std), perl  = TRUE)

##lable all data set with descriptive variables
names(extract_mean_std) <- gsub("^tBody", "Time Body ", names(extract_mean_std))
names(extract_mean_std) <- gsub("Acc", "Acceleration ", names(extract_mean_std))
names(extract_mean_std) <- gsub("^t", "Time ", names(extract_mean_std))
names(extract_mean_std) <- gsub("^f", "Frequency ", names(extract_mean_std))
names(extract_mean_std) <- gsub("BodyBody", "Body ", names(extract_mean_std))
names(extract_mean_std) <- gsub("mean", "Mean ", names(extract_mean_std))
names(extract_mean_std) <- gsub("std", "Standard Deviation ", names(extract_mean_std))
names(extract_mean_std) <- gsub("Freq", "Frequency ", names(extract_mean_std))
names(extract_mean_std) <- gsub("Mag", "Magnitude ", names(extract_mean_std))
names(extract_mean_std) <- gsub("Gravity*", "Gravity ", names(extract_mean_std))
names(extract_mean_std) <- gsub("Gyro*", "Gyro ", names(extract_mean_std))
names(extract_mean_std) <- gsub("Jerk*", "Jerk ", names(extract_mean_std))
names(extract_mean_std) <- gsub("Body*", "Body ", names(extract_mean_std))
names(extract_mean_std) <- gsub("Frequency uency*", "Frequency ", names(extract_mean_std))
names(extract_mean_std) <- gsub("angletBody", "Angle Time Body ", names(extract_mean_std))
names(extract_mean_std) <- gsub("Meangravity", "Mean Gravity ", names(extract_mean_std))
names(extract_mean_std) <- gsub("*gravityMean", " Gravity Mean ", names(extract_mean_std))
names(extract_mean_std) <- gsub("^angle", "Angle ", names(extract_mean_std))

#####
#####Part 5 of question 5. From the data set in step 4, creates a second, 
##   independent tidy data set with the average of each variable for each activity and each subject.
#####

## group by data set using subject & type and calculates the mean

tidy_extract_mean_std_data <- ddply(extract_mean_std,c("subject","Type"), numcolwise(mean))

## write the data to a txt file
write.table(tidy_extract_mean_std_data,file = "tidy_data_r_analysis.txt")

##library(knitr)
###knit2html("codebook.Rmd")
