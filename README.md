run_analysis.R
==============

-- Getting and Cleaning Data Course Project on Coursera

This file describes how the script works.

Script:
==============

The R script does the following.
 1. Download the data for the project
 2. Merges the training and the test sets to create one data set
 3. Extracts only the measurements on the mean and standard deviation for each measurement
 4. Uses descriptive activity names to name the activities in the data set
 5. Appropriately labels the data set with descriptive variable names
 6. Creates a second, independent tidy data set with the average of each variable for each activity and each subject

Data Source:
==============

Data for this project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Outcome:
==============

The R script creates the TidyData.txt that aggregates the combined training and test data set by activity and subject
