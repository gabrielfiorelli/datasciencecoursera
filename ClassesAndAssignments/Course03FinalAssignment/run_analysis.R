rm(list=ls())
if("package:plyr" %in% search()) detach("package:plyr", unload=TRUE)

print("Loading data.table.")
library(data.table)
print("Loading dplyr")
library(dplyr)


path <- getwd()

fileUrl <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
fileName <- 'Dataset.zip'
if(!file.exists(fileName)) {
    download.file(fileUrl, fileName)
}
folder <- 'UCI HAR Dataset'
if(!file.exists(folder)) {
    unzip(fileName)
}

print("Setting directories.")
subjectTrainPath <- file.path(path, folder, 'train', 'subject_train.txt')
subjectTestPath <- file.path(path, folder, 'test', 'subject_test.txt')
activityTrainPath <- file.path(path, folder, 'train', 'y_train.txt')
activityTestPath <- file.path(path, folder, 'test', 'y_test.txt')
trainPath <- file.path(path, folder, 'train', 'X_train.txt')
testPath <- file.path(path, folder, 'test', 'X_test.txt')            
featuresPath <- file.path(path, folder, 'features.txt')
activityLabelsPath <- file.path(path, folder, 'activity_labels.txt')  
                               
print("Reading Subjects.")
subjectTrain <- data.table(read.table(subjectTrainPath))
subjectTest <- data.table(read.table(subjectTestPath))

subjectUnion <- bind_rows(subjectTrain, subjectTest)
subjectUnion <- rename(subjectUnion, Subject = V1)

remove(subjectTrain, subjectTest)


print("Reading Activities.")
activityTrain <- data.table(read.table(activityTrainPath))
activityTest <- data.table(read.table(activityTestPath))

activityUnion <- bind_rows(activityTrain, activityTest)
activityUnion <- rename(activityUnion, Activity = V1)

remove(activityTrain, activityTest)


print("Joining Datasets. Activity and Subject.")

dataset <- bind_cols(activityUnion, subjectUnion)
#dataset <- cbind(subjectUnion, activityUnion)
remove(activityUnion, subjectUnion)


print("Reading Activity Names.")
activityLabels <- data.table(read.table(activityLabelsPath))
activityLabels <- rename(activityLabels, Activity = V1, ActivityName = V2)
dataset <- merge(dataset, activityLabels, by.x = "Activity", by.y = "Activity", all = TRUE)

print("Reading Feature Data.")
train <- data.table(read.table(trainPath))
test <- data.table(read.table(testPath))
trainTest <- bind_rows(train, test)
remove(train, test)

print("Joining Datasets.")
dataset <- bind_cols(dataset, trainTest)
#dataset <- cbind(dataset, trainTest)
remove(trainTest)

print("Reading features")
features <- data.table(read.table(featuresPath))
features <- rename(features, FeatureIndex = V1, FeatureName = V2)
features <- select(filter(mutate(features, IsDeviationOrMean = ifelse(grepl(".*(std\\(\\)|mean\\(\\)).*", features$FeatureName), 1, 0)), IsDeviationOrMean == 1), FeatureIndex, FeatureName)
features$FeatureIndex <- paste("V", features$FeatureIndex, sep = "")

print("Selecting just STD and Mean Features.")
dataset <- select(dataset, Activity, ActivityName, Subject, features$FeatureIndex)

print("Renaming Columns.")
setnames(dataset, old = names(select(dataset, features$FeatureIndex)), new = as.character(features$FeatureName))


names(dataset) <- gsub('^t', 'Time', names(dataset))
names(dataset) <- gsub('^f', 'Frequency', names(dataset))
names(dataset) <- gsub('Acc', 'Accelerometer', names(dataset))
names(dataset) <- gsub('Gyro','Gyroscope', names(dataset))
names(dataset) <- gsub('mean[(][)]','Mean',names(dataset))
names(dataset) <- gsub('std[(][)]','Std',names(dataset))
names(dataset) <- gsub('-','',names(dataset))


print("Summarizing.")
dataSetSummarized <- group_by(dataset, ActivityName, Subject)
dataSetSummarized <- summarise_all(dataSetSummarized, funs(mean))

dataSetSummarized$Activity <- NULL

print("Saving files.")
path = getwd()
write.table(dataSetSummarized, file.path(path, 'dataset_summarized.txt'), row.names=FALSE)
write.table(dataset, file.path(path, 'dataset.txt'), row.names=FALSE)