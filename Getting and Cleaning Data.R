##Download a file
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile="./downloadfiles/dataset.zip", method="curl")
dateDownloaded <- date()

##Read data
train.x <- read.table("./downloadfiles/UCI HAR Dataset/train/x_train.txt")
test.x <- read.table("./downloadfiles/UCI HAR Dataset/test/x_test.txt")
train.y <- read.table("./downloadfiles/UCI HAR Dataset/train/y_train.txt")
test.y <- read.table("./downloadfiles/UCI HAR Dataset/test/y_test.txt")

subject_test <- read.table("./downloadfiles/UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./downloadfiles/UCI HAR Dataset/train/subject_train.txt")

features <- read.table("./downloadfiles/UCI HAR Dataset/features.txt")
activity <- read.table("./downloadfiles/UCI HAR Dataset/activity_labels.txt")

##Merges the training and the test sets to create one data set
train.all <- cbind(train.y,subject_train,train.x)
test.all <- cbind(test.y,subject_test,test.x)

combined.all <- rbind(train.all,test.all)

##Extracts only the measurements on the mean and standard deviation for each measurement
names(combined.all) <- c("V1","V2",as.character(features[,2]))
meanstd <- grep("V|mean()|std()",names(combined.all))

measurements <- combined.all[,c(meanstd)]

##Uses descriptive activity names to name the activities in the data set
colnames(measurements)[1:2] <- c("activity","subject")

##Appropriately labels the data set with descriptive variable names
class(activity$V2)
activity$V2 <- as.character(activity$V2)

for (i in 1:length(measurements[,1])) {
    measurements[i,1] <- activity[measurements[i,1],2]
}

##Creates a second, independent tidy data set with the average of each variable for each activity and each subject
colnames(measurements) [2] <- "subject"

attach(measurements)
tidy_means <- aggregate(measurements[,3] ~ subject + activity,data=measurements,FUN="mean")

for (i in 4:ncol(measurements)) {
    tidy_means[,i] <- aggregate(measurements[,i] ~ subject + activity,data=measurements,FUN="mean")[,3]
}

colnames(tidy_means) <- colnames(measurements)

write.table(tidy_means,file="./downloadfiles/TidyData.txt")
read.tidy <- read.table("./downloadfiles/TidyData.txt")


