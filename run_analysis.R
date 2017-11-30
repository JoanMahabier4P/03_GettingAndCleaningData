# Getting and Cleaning Data
# run_Analysis.R

#****************************** Preparations *************************************************************

# Clean up workspace
rm(list=ls())

library(data.table)
library(reshape2)

#set working directory to location where UCI HAR Dataset is unzipped
setwd("D:/01_DataScience_Coursera/03_Getting_and_Clean_Data/Assignment/data/UCI HAR Dataset")

#****************************** Collect the data *************************************************************
 
      # features.txt file: shows all sorts of calculationtypes (incl the mean and standard deviation).
      # activity_labels.txt: shows the used activity types(walking, standing, etc.)
      # subject_train.txt and subject_test.txt: train and test data
      # y_train.txt and y_test.txt: Training labels and Test labels (activity data); the numbers are explained in activity_labels.txt)
      # X_train.txt and X_test.txt: Training and Test dataset
      # For the purposes of this project, the files in the Inertial Signals folders are not used.

# Data about types of activities and calculations
tFeatures   <- read.table('./features.txt',header=FALSE) #read features.txt
tActivityLabels <- read.table('./activity_labels.txt',header=FALSE) #read activity_labels.txt

# Training Data
subjTrain <- read.table('./train/subject_train.txt',header=FALSE) #read training data from subject_train.txt
actTrain <- read.table('./train/y_train.txt',header=FALSE) #read activity data from y_train.txt
dataTrain <- read.table('./train/X_train.txt',header=FALSE) #read actual trainingdataset from X_train.txt

# Test Data
subjTest <- read.table('./test/subject_test.txt',header=FALSE) #read test data from subject_test.txt
actTest <- read.table('./test/y_test.txt',header=FALSE) #read activity data from y_test.txt
dataTest <- read.table('./test/X_test.txt',header=FALSE) #read actual testdataset from X_test.txt

#****************** 1. Merge training and test sets to create one data set *********************************************

              # Give column names
# Tables about types of activities and calculations)
setnames(tFeatures, names(tFeatures), c("featNumber", "featCalculation"))
setnames(tActivityLabels, names(tActivityLabels), c("activityNumber", "Activity"))

# Training Data
setnames(subjTrain, names(subjTrain), c("subjNumber"))
setnames(actTrain, names(actTrain), c("activityNumber"))
colnames(dataTrain) <- tFeatures[,2] 

# Test Data
setnames(subjTest, names(subjTest), c("subjNumber"))
setnames(actTest, names(actTest), c("activityNumber"))
colnames(dataTest) <- tFeatures[,2] 

              # Merge columns 
trainData <- cbind(subjTrain,actTrain,dataTrain) # training data
testData <- cbind(subjTest,actTest,dataTest) # test data

              # Concatenate training and test data to create one data set
oneDataSet <- rbind(trainData,testData)


#****************** 2. Extract only the measurements on the mean and standard deviation for each measurement. 

              # Retrieve the right column names  
Column <- colnames(oneDataSet) # Charactervector containing all columnnames from oneDataSet. 

# Search for the following matches in Column (returns TRUE or FALSE)
searchColumn <- (grepl("activity..",Column) | grepl("subj..",Column) | grepl("-mean..",Column) & !grepl("-meanFreq..",Column) & !grepl("mean..-",Column) | grepl("-std..",Column) & !grepl("-std()..-",Column))
SetMeanSD <- oneDataSet[searchColumn == TRUE] # Subset these matches


#****************** 3. Use descriptive activity names to name the activities in the data set ******************

# Join SetMeanSD with tActivityLabels 
completeData <- merge(SetMeanSD,tActivityLabels,by='activityNumber',all.x=TRUE)
Column <- colnames(completeData) # update Column since a new column (activity) is retrieved


#****************** 4. Appropriately label the data set with descriptive activity names ***********************

# Replace the names of the variables with descriptive names by using gsub
for (i in 1:length(Column)) {
            Column[i] <- gsub("-mean","Mean",Column[i])
            Column[i] <- gsub("-std","SD",Column[i])
            Column[i] <- gsub("\\()","",Column[i])
            Column[i] <- gsub("^(t)","Time",Column[i])
            Column[i] <- gsub("^(f)","Frequency",Column[i])
            Column[i] <- gsub("([Bb]ody|[Bb]ody[Bb]ody)","Body",Column[i])
            Column[i] <- gsub("([Gg]ravity)","Grav",Column[i])
            Column[i] <- gsub("[Gg]yro","Gyros",Column[i])
            Column[i] <- gsub("AccMag","AccelerMagni",Column[i])
            Column[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccelJerkMagni",Column[i])
            Column[i] <- gsub("JerkMag","JerkMagni",Column[i])
            Column[i] <- gsub("GyroMag","GyrosMagni",Column[i])
}

Column
colnames(completeData) <- Column # update the columns of completeData with these new names


#****************** 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject ***********************

# Aggregate only the calculation columns (3-20).
# Group by activityNumber and subjNumber (for 'each activity and each subject') and take the mean of each column.
aggData <- aggregate(completeData[,3:20],by=list(activityNumber <- completeData$activityNumber, subjNumber <- completeData$subjNumber),mean)
colnames(aggData)[1] <- "activityNumber" #reassign the appropriate names to the group by columns
colnames(aggData)[2] <- "subjNumber"
TidyDSet <- merge(aggData,tActivityLabels,by='activityNumber',all.x=TRUE) # Join aggData with tActivityLabels to get the names of the activities

# Export the tidyData set 
write.table(TidyDSet, './TidyDSet.txt',row.names=TRUE,sep='\t')



