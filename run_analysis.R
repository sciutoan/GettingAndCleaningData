##Peer Graded Assignment: Getting and Cleaning Data Course Project
##Student: Anthony
##Date Submitted: Jan 19, 2020

##Goals as described in the course instructions:
  #1. Merges the training and the test sets to create one data set.
  #2. Extracts only the measurements on the mean and standard deviation for each measurement.
  #3. Uses descriptive activity names to name the activities in the data set
  #4. Appropriately labels the data set with descriptive variable names.
  #5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)

#Goal 1: Merge the training and the test sets to create one data set.

#Read Labels and Features
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("n", "functions"))
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
#Read test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
#Read train data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
#Merge data
xdata <- rbind(X_train, X_test)
ydata <- rbind(y_train, y_test)
subjectdata <- rbind(subject_train, subject_test)
mergeddata <- cbind(subjectdata, ydata, xdata)


#2. Extracts only the measurements on the mean and standard deviation for each measurement.
Cleandata <- mergeddata %>% select(subject, code, contains("mean"), contains("std"))


#3. Uses descriptive activity names to name the activities in the data set
Cleandata$code <- activity_labels[Cleandata$code,2]


#4. Appropriately labels the data set with descriptive variable names.
names(Cleandata)[2] = "activity"
names(Cleandata) <- gsub("Acc", "Accelerometer", names(Cleandata))
names(Cleandata)<-gsub("Gyro", "Gyroscope", names(Cleandata))
names(Cleandata)<-gsub("BodyBody", "Body", names(Cleandata))
names(Cleandata)<-gsub("Mag", "Magnitude", names(Cleandata))
names(Cleandata)<-gsub("^t", "Time", names(Cleandata))
names(Cleandata)<-gsub("^f", "Frequency", names(Cleandata))
names(Cleandata)<-gsub("tBody", "TimeBody", names(Cleandata))
names(Cleandata)<-gsub("-mean()", "Mean", names(Cleandata), ignore.case = TRUE)
names(Cleandata)<-gsub("-std()", "STD", names(Cleandata), ignore.case = TRUE)
names(Cleandata)<-gsub("-freq()", "Frequency", names(Cleandata), ignore.case = TRUE)
names(Cleandata)<-gsub("angle", "Angle", names(Cleandata))
names(Cleandata)<-gsub("gravity", "Gravity", names(Cleandata))


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
averagedata <- Cleandata %>% group_by(subject, activity) %>% summarise_all(funs(mean))
write.table(averagedata, "averagedata.txt", row.name=FALSE)



