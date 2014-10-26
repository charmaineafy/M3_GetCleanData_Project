## ---------------------------------------------------------------------------------------
## Author : Charmaine Ang
## Date   : 20-26 Oct 2014
## Script : run_analysis.R
## ---------------------------------------------------------------------------------------
## create one R script called run_analysis.R that does the following: 
## 1.Merges the training and the test sets to create one data set.
## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3.Uses descriptive activity names to name the activities in the data set
## 4.Appropriately labels the data set with descriptive variable names. 
## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## ---------------------------------------------------------------------------------------

install.packages("stringr") ## First time only.
library(stringr) ## Use in Step 4.

install.packages("reshape") ## First time only.
library(reshape) ## Use in Step 5.

## ---------------------------------------------------------------------------------------
## Step 1. Merges the training and the test sets to create one data set.
## ---------------------------------------------------------------------------------------

## ----------------
## 1a. Lookup Tables
## ----------------

## Read the list of activity labels.
activityLabel <- read.table("./data/UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE, col.names=c("actNum","actName"))

## Read the list of 561 features which mapped to the columns for the record in "X_train.txt" and "X_test.txt".
features <- read.table("./data/UCI HAR Dataset/features.txt", sep="", header=FALSE, col.names=c("fNum","fName"))

## ----------------
## 1b. Training Set
## ----------------

## Read the list of subjects which maps sequentially to the records in "X_train.txt".
trainSubj <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE, col.names="subject")

## Read Y (i.e. the list of numeric activity lables) which maps sequentially to the records in "X_train.txt".
trainY <- read.table("./data/UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE, col.names="activityNum")

## Read X: Load the list of training records where each record contains measurement for 561 features. 
## Note: read.table automatically converts the special characters (such as dash, open and close brackets) in col.names=features$fName to fullstop (".").
trainX <- read.table("./data/UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE, stringsAsFactors=FALSE, col.names=features$fName)

## Map subjects and activities to measurements. 
trainData <- cbind(trainSubj,trainY, trainX)

## ----------------
## 1c. Test Set
## ----------------

## Read the list of subjects which maps sequentially to the records in "X_test.txt".
testSubj <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE, col.names="subject")

## Read Y (i.e. the list of numeric activity lables) which maps sequentially to the records in "X_test.txt".
testY <- read.table("./data/UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE, col.names="activityNum")

## Read X: Load the list of test records where each record contains measurement for 561 features.
## Note: read.table automatically converts the special characters (such as dash, open and close brackets) in col.names=features$fName to fullstop ("."). 
testX <- read.table("./data/UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE, stringsAsFactors=FALSE, col.names=features$fName)

## Map subjects and activities to measurements. 
testData <- cbind(testSubj,testY, testX)

## ----------------
## 1d. Merge Training and test sets to one data set.
## ----------------
allData <- rbind(trainData,testData)

## ---------------------------------------------------------------------------------------
## Step 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## ---------------------------------------------------------------------------------------
## Assumption: 
## (i)  Measurement mean is taken from variables whose names contains "mean()".
## (ii) Measurement standard deviation is taken from variables whose name contains "std()".
## ---------------------------------------------------------------------------------------
selectData <- allData[,c(grep("subject|activity|mean|std",colnames(allData)))] 
selectData <-selectData[-grep("meanFreq", colnames(selectData))]  # To exclude meanFreq

## ---------------------------------------------------------------------------------------
## Step 3. Uses descriptive activity names to name the activities in the data set.
## ---------------------------------------------------------------------------------------
fnx <- function(actNum) {as.character(activityLabel[actNum,2])}
activityLabel <- sapply(selectData$activityNum,fnx)
transformData <- cbind(activityLabel,selectData)
transformData <-transformData[-grep("activityNum", colnames(transformData))]  # To exclude activityNum

## ---------------------------------------------------------------------------------------
## Step 4. Appropriately labels the data set with descriptive variable names. 
## ---------------------------------------------------------------------------------------

## This step will make the column names in descriptive format where the diff components in a column is separated by a single fullstop only.
## e.g. 
## In Step 1: label such as "tBodyAcc-mean()-X" is changed to "tBodyAcc.mean...X"; 
##            which contains multiple fullstops (sometimes continuously) in place of special characters.
## In Step 4: aim is to simplify "tBodyAcc.mean...X" to "tBodyAcc.mean.X"; with just ONE fullstop to separate diff components for readibility.

desCol <- colnames(transformData)
desCol <- str_replace(desCol,"\\.\\.\\.X",".X")
desCol <- str_replace(desCol,"\\.\\.\\.Y",".Y")
desCol <- str_replace(desCol,"\\.\\.\\.Z",".Z")
desCol <- str_replace(desCol,"mean\\.\\.","mean")
desCol <- str_replace(desCol,"std\\.\\.","std")

## Replace existing column names with the refined and descriptive names.
colnames(transformData) <- desCol

## ---------------------------------------------------------------------------------------
## 5.From the data set in step 4, 
## creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
## ---------------------------------------------------------------------------------------
## Ref: http://stackoverflow.com/questions/1407449/for-each-group-summarise-means-for-all-variables-in-dataframe-ddply-split

## ----------------
## 5a. Calculate average of each variable for each activity and each subject
## ----------------
melted <- melt(transformData, id.vars = c("subject", "activityLabel"))
result <- cast(subject + activityLabel ~ variable, data = melted, fun = mean) # independent tidy data set created.

## ----------------
## 5b. Output tidy data set to a text file. 
## ----------------
myfile <- file("./data/tidydata.txt", "wb")
write.table(result, file = myfile, sep = ",", row.name = FALSE)

## ---------------------------------------------------------------------------------------
## END OF SCRIPT
## ---------------------------------------------------------------------------------------
