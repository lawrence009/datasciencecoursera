# R script to extract and merge HAR training and test data files into a data frame (df) for subsequent analysis

#library(data.table)

#read feature and activity labels
features.label <- read.table("./UCI HAR Dataset/features.txt", quote="\"")[,2]
activities.label <- read.table("./UCI HAR Dataset/activity_labels.txt", quote="\"")

#select mean and std
features.mean  <- grep('mean', features.label)
features.std <- grep('std', features.label)

#create dataset
#read training dataset
train <- read.table("./UCI HAR Dataset/X_train.txt", quote="\"")
colnames(train) <- features.label

#
activity <- read.table("./UCI HAR Dataset/y_train.txt", quote="\"")[,1]
#find a better way to do this
activity <- replace(activity, activity == 1, 'WALKING')
activity <- replace(activity, activity == 2, 'WALKING_UPSTAIRS')
activity <- replace(activity, activity == 3, 'WALKING_DOWNSTAIRS')
activity <- replace(activity, activity == 4, 'SITTING')
activity <- replace(activity, activity == 5, 'STANDING')
activity <- replace(activity, activity == 6, 'LAYING')
train <- cbind(activity, train)

#
subject.id <- read.table("./UCI HAR Dataset/subject_train.txt", quote="\"")[,1]
train <- cbind(subject.id, train)

#label dataset
train$dataset <- 'training'


#read test dataset
test <- read.table("./UCI HAR Dataset/X_test.txt", quote="\"")
colnames(test) <- features.label

#
activity <- read.table("./UCI HAR Dataset/y_test.txt", quote="\"")[,1]
#find a better way to do this
activity <- replace(activity, activity == 1, 'WALKING')
activity <- replace(activity, activity == 2, 'WALKING_UPSTAIRS')
activity <- replace(activity, activity == 3, 'WALKING_DOWNSTAIRS')
activity <- replace(activity, activity == 4, 'SITTING')
activity <- replace(activity, activity == 5, 'STANDING')
activity <- replace(activity, activity == 6, 'LAYING')
test <- cbind(activity, test)

#
subject.id <- read.table("./UCI HAR Dataset/subject_test.txt", quote="\"")[,1]
test <- cbind(subject.id, test)

#label dataset
test$dataset <- 'test'


#merge result 2 datasets into 1 tidy data
df <- rbind(train, test)


#clean up temp variables
rm(activity, activities.label, subject.id, train, test)


