
# Programming Assignment for Getting and Cleaning Data

# Download zip file and unzip
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "ucihardataset.zip"
download.file(fileurl, zipfile)
unzip(zipfile)

# Read data into Data Frames
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("Number", "Function"))
activity <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("Label", "Activity"))

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "Subject")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "Label")
x_test <- read.table("UCI HAR Dataset/test/x_test.txt", col.names = features$Function)

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "Subject")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "Label")
x_train <- read.table("UCI HAR Dataset/train/x_train.txt", col.names = features$Function)

# Q1: Merge the training and the test sets to create one data set
testdata <- cbind(subject_test, y_test, x_test)
traindata <- cbind(subject_train, y_train, x_train)
combineddata <- rbind(traindata, testdata)

# Q2: Extract only the measurements on the mean and standard deviation for each measurement
relcols1 <- grep("mean", names(combineddata))
relcols2 <- grep("std", names(combineddata))
relcols <- c(1, 2, relcols1, relcols2)
sort(relcols)
reldata <- combineddata[, relcols]

# Q3: Use descriptive activity names to name the activities in the data set
names(reldata)[2] <- "Activity"
reldata$Activity <- as.character(reldata$Activity)
reldata$Activity <- gsub("1", "WALKING", reldata$Activity) 
reldata$Activity <- gsub("2", "WALKING_UPSTAIRS", reldata$Activity) 
reldata$Activity <- gsub("3", "WALKING_DOWNSTAIRS", reldata$Activity) 
reldata$Activity <- gsub("4", "SITTING", reldata$Activity) 
reldata$Activity <- gsub("5", "STANDING", reldata$Activity) 
reldata$Activity <- gsub("6", "LAYING", reldata$Activity) 

# Q4: Appropriately label the data set with descriptive variable names
names(reldata)<-gsub("Acc", "Accelerometer", names(reldata))
names(reldata)<-gsub("Gyro", "Gyroscope", names(reldata))
names(reldata)<-gsub("BodyBody", "Body", names(reldata))
names(reldata)<-gsub("Mag", "Magnitude", names(reldata))
names(reldata)<-gsub("^t", "Time", names(reldata))
names(reldata)<-gsub("^f", "Frequency", names(reldata))
names(reldata)<-gsub("tBody", "TimeBody", names(reldata))
names(reldata)<-gsub("mean()", "Mean", names(reldata), ignore.case = TRUE)
names(reldata)<-gsub("std()", "STD", names(reldata), ignore.case = TRUE)
names(reldata)<-gsub("-freq()", "Frequency", names(reldata), ignore.case = TRUE)
names(reldata)<-gsub("angle", "Angle", names(reldata))
names(reldata)<-gsub("gravity", "Gravity", names(reldata))

# Q5: From the data set in step 4, create a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

tidydata <- reldata %>% group_by(Activity, Subject) %>% summarize_all(mean)
write.table(tidydata, "GACDProgAssignment/tidydata.txt")

