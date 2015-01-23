#Merges the training and the test sets to create one data
setwd("~/Getting_and_cleaning_data_peer_assessment")
trainData <- read.table("./UCI_HAR_Dataset/train/X_train.txt")
dim(trainData)
head(trainData)
trainLabel <- read.table("./UCI_HAR_Dataset/train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./UCI_HAR_Dataset/train/subject_train.txt")
testData <- read.table("./UCI_HAR_Dataset/test/X_test.txt")
dim(testData)
testLabel <- read.table("./UCI_HAR_Dataset/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./UCI_HAR_Dataset/test/subject_test.txt")
joinData <- rbind(trainData, testData)
dim(joinData) 
joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel) 
joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject) 
#Extract the measurements on the mean and standard deviation for each measurement
features <- read.table("./UCI_HAR_Dataset/features.txt")
dim(features) 
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)
joinData <- joinData[, meanStdIndices]
dim(joinData) 
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 
# Use descriptive activity names to name the activities in the data set
activity <- read.table("./UCI_HAR_Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"
# Appropriately labels the data set with descriptive activity names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData)
write.table(cleanedData, "merged_data.txt")
# Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)
write.table(result, "data_with_means.txt") 
