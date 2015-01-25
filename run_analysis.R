# course Project - Getting and Cleaning Data
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected. 
# 
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following. 
# 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


library(dplyr)
library(tidyr)

# 1.  Merges the training and the test sets to create one data set.


xTrainData <- tbl_df(read.table(file.path(getwd(),"UCI HAR Dataset/train/X_train.txt"))) #read X_train data
dim(xTrainData) #[1] 7352    561

features <- tbl_df(read.table(file.path(getwd(),"UCI HAR Dataset/features.txt"))) #read features.txt which contains 
# names of 561 variables
dim(features) # 561 2

colnames(xTrainData) <- features$V2 # name the columns of x_train data from features.txt file

xTestData <- tbl_df(read.table(file.path(getwd(),"UCI HAR Dataset/test/X_test.txt"))) #read X_test data
dim(xTestData) #[1] 2947    561

colnames(xTestData) <- features$V2 # name the columns of x_test data from features.txt file

Dataset <- bind_rows(xTrainData, xTestData) #Merge train data and test data into a single dataset
dim(Dataset) # [1] 10299   477


# 2. Extract only the measurements on the mean and standard deviation for each measurement

stat_Dataset <- Dataset %>% select(contains("mean", ignore.case = TRUE), 
                                   contains("std", ignore.case = TRUE))

dim(stat_Dataset) #[1] 10299    86


# 3. Uses descriptive activity names to name the activities in the data set

# read y_train data ans y_test data and replace activity id into names from activity_lables.txt

yTrainData <- tbl_df(read.table(file.path(getwd(),"UCI HAR Dataset/train/y_train.txt"))) #read y_train data
dim(yTrainData) # [1] 7352    1

yTestData <- tbl_df(read.table(file.path(getwd(),"UCI HAR Dataset/test/y_test.txt"))) #read y_test data
dim(yTestData) #[1] 2947    1

y_Data <- bind_rows(yTrainData, yTestData) # combine y_Train and y_Test data into single y-Data

dim(y_Data) # [1] 10299     1

#replace activity id into names from activity_lables.txt

#read activity names
activities <- tbl_df(read.table(file.path(getwd(),"UCI HAR Dataset/activity_labels.txt"))) 

dim(activities) # 6 2

view(activities)

# 1 1 WALKING
# 2  2	WALKING_UPSTAIRS
# 3	3	WALKING_DOWNSTAIRS
# 4	4	SITTING
# 5	5	STANDING
# 6	6	LAYING

# Replace the numeric id with equivalent activity name

y_Data$activity[y_Data$V1 == 1] = "walking"
y_Data$activity[y_Data$V1 == 2] = "WALKING_UPSTAIRS"
y_Data$activity[y_Data$V1 == 3] = "WALKING_DOWNSTAIRS"
y_Data$activity[y_Data$V1 == 4] = "SITTING"
y_Data$activity[y_Data$V1 == 5] = "STANDING"
y_Data$activity[y_Data$V1 == 6] = "LAYING"

#combine y-Data into the stat_Dataset

stat_Dataset <- bind_cols(stat_Dataset, y_Data)

dim(stat_Dataset) #[1] 10299    88


# 4. Appropriately labels the data set with descriptive variable names. 
colnames(stat_Dataset)
# [1] "tBodyAcc-mean()-X"                    "tBodyAcc-mean()-Y"                   
# [3] "tBodyAcc-mean()-Z"                    "tGravityAcc-mean()-X"                
# [5] "tGravityAcc-mean()-Y"                 "tGravityAcc-mean()-Z"                
# [7] "tBodyAccJerk-mean()-X"                "tBodyAccJerk-mean()-Y"               
# [9] "tBodyAccJerk-mean()-Z"                "tBodyGyro-mean()-X"                  
# [11] "tBodyGyro-mean()-Y"                   "tBodyGyro-mean()-Z"                  
# [13] "tBodyGyroJerk-mean()-X"               "tBodyGyroJerk-mean()-Y"              
# [15] "tBodyGyroJerk-mean()-Z"               "tBodyAccMag-mean()"                  
# [17] "tGravityAccMag-mean()"                "tBodyAccJerkMag-mean()"              
# [19] "tBodyGyroMag-mean()"                  "tBodyGyroJerkMag-mean()"             
# [21] "fBodyAcc-mean()-X"                    "fBodyAcc-mean()-Y"                   
# [23] "fBodyAcc-mean()-Z"                    "fBodyAcc-meanFreq()-X"               
# [25] "fBodyAcc-meanFreq()-Y"                "fBodyAcc-meanFreq()-Z"               
# [27] "fBodyAccJerk-mean()-X"                "fBodyAccJerk-mean()-Y"               
# [29] "fBodyAccJerk-mean()-Z"                "fBodyAccJerk-meanFreq()-X"           
# [31] "fBodyAccJerk-meanFreq()-Y"            "fBodyAccJerk-meanFreq()-Z"           
# [33] "fBodyGyro-mean()-X"                   "fBodyGyro-mean()-Y"                  
# [35] "fBodyGyro-mean()-Z"                   "fBodyGyro-meanFreq()-X"              
# [37] "fBodyGyro-meanFreq()-Y"               "fBodyGyro-meanFreq()-Z"              
# [39] "fBodyAccMag-mean()"                   "fBodyAccMag-meanFreq()"              
# [41] "fBodyBodyAccJerkMag-mean()"           "fBodyBodyAccJerkMag-meanFreq()"      
# [43] "fBodyBodyGyroMag-mean()"              "fBodyBodyGyroMag-meanFreq()"         
# [45] "fBodyBodyGyroJerkMag-mean()"          "fBodyBodyGyroJerkMag-meanFreq()"     
# [47] "angle(tBodyAccMean,gravity)"          "angle(tBodyAccJerkMean),gravityMean)"
# [49] "angle(tBodyGyroMean,gravityMean)"     "angle(tBodyGyroJerkMean,gravityMean)"
# [51] "angle(X,gravityMean)"                 "angle(Y,gravityMean)"                
# [53] "angle(Z,gravityMean)"                 "tBodyAcc-std()-X"                    
# [55] "tBodyAcc-std()-Y"                     "tBodyAcc-std()-Z"                    
# [57] "tGravityAcc-std()-X"                  "tGravityAcc-std()-Y"                 
# [59] "tGravityAcc-std()-Z"                  "tBodyAccJerk-std()-X"                
# [61] "tBodyAccJerk-std()-Y"                 "tBodyAccJerk-std()-Z"                
# [63] "tBodyGyro-std()-X"                    "tBodyGyro-std()-Y"                   
# [65] "tBodyGyro-std()-Z"                    "tBodyGyroJerk-std()-X"               
# [67] "tBodyGyroJerk-std()-Y"                "tBodyGyroJerk-std()-Z"               
# [69] "tBodyAccMag-std()"                    "tGravityAccMag-std()"                
# [71] "tBodyAccJerkMag-std()"                "tBodyGyroMag-std()"                  
# [73] "tBodyGyroJerkMag-std()"               "fBodyAcc-std()-X"                    
# [75] "fBodyAcc-std()-Y"                     "fBodyAcc-std()-Z"                    
# [77] "fBodyAccJerk-std()-X"                 "fBodyAccJerk-std()-Y"                
# [79] "fBodyAccJerk-std()-Z"                 "fBodyGyro-std()-X"                   
# [81] "fBodyGyro-std()-Y"                    "fBodyGyro-std()-Z"                   
# [83] "fBodyAccMag-std()"                    "fBodyBodyAccJerkMag-std()"           
# [85] "fBodyBodyGyroMag-std()"               "fBodyBodyGyroJerkMag-std()"          
# [87] "V1"                                   "activity"  

#colnames(combinedData) <- sub("( )"," ",colnames, fixed = TRUE)

colnames(stat_Dataset) <- sub("()","", colnames(stat_Dataset), fixed =TRUE)

# remove V1 which is dulbicate column of activity
stat_Dataset <- select(stat_Dataset, -V1)

dim(stat_Dataset) #[1] 10299    87



# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

#read subject_train data and subject_test data and merge it as single subject

subjectTrainData <- tbl_df(read.table(file.path(getwd(),"UCI HAR Dataset/train/subject_train.txt"))) #read subject_train data
dim(subjectTrainData) # [1] 7352    1

subjectTestData <- tbl_df(read.table(file.path(getwd(),"UCI HAR Dataset/test/subject_test.txt"))) #read subject_test data
dim(subjectTestData) #[1] 2947    1

# merge subject_train data and subject_test data into a single file as subject

subject <- bind_rows(subjectTrainData, subjectTestData)

dim(subject) # [1] 10299     1

# merge subject data into stat_Dataset to form the complete data

newData <- bind_cols(stat_Dataset, subject)

dim(newData) # [1] 10299    88

# name the newly attached column as subject
colnames(newData)[colnames(newData)=="V1"] <- "subject"

# group newData by activity and subject  and store the result as by_subject
tidyData <- group_by(newData, activity, subject)

dim(tidyData) # [1] 10299    88

#average of each variable for each activity and each subject. 
tidyData <- tidyData %>% summarise_each(funs(mean))

dim(tidyData) # [1] 180  88

# create a .txt file with the final dataset.
write.table(tidyData, file = "finalData.txt", row.names = FALSE)











