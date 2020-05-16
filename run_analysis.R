features <- read.table("C:/Users/Davitz/Google Drive/DataScience Specialization/3. Getting and Cleaning Data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt",
                       header = FALSE)
activityLabel <- read.table("C:/Users/Davitz/Google Drive/DataScience Specialization/3. Getting and Cleaning Data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt",
                             header = FALSE)
subjectTrain <- read.table("C:/Users/Davitz/Google Drive/DataScience Specialization/3. Getting and Cleaning Data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", 
                            header=FALSE)
xTrain <- read.table("C:/Users/Davitz/Google Drive/DataScience Specialization/3. Getting and Cleaning Data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", 
                      header=FALSE)
yTrain <- read.table("C:/Users/Davitz/Google Drive/DataScience Specialization/3. Getting and Cleaning Data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", 
                      header=FALSE)



#Assign column names to the data above.

colnames(activityLabel)<-c("activityId","activityType")
colnames(subjectTrain) <- "subId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"


#Merging training Data...

trainData <- cbind(yTrain,subjectTrain,xTrain)

#Reading the test Data

subjectTest <- read.table("C:/Users/Davitz/Google Drive/DataScience Specialization/3. Getting and Cleaning Data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", 
                           header=FALSE)
xTest <- read.table("C:/Users/Davitz/Google Drive/DataScience Specialization/3. Getting and Cleaning Data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", 
                     header=FALSE)
yTest  <- read.table("C:/Users/Davitz/Google Drive/DataScience Specialization/3. Getting and Cleaning Data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", 
                      header=FALSE)

# Assign column names.. same as for training data..

colnames(subjectTest) <- "subId"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityId"

# merging test Data
testData <- cbind(yTest,subjectTest,xTest)


#final merged data

finalData <- rbind(trainData,testData)
head(finalData)

# creating a vector for column names to be used further

colNames <- colnames(finalData)
head(finalData)


# 2. Extract only the measurements on the mean and standard deviation for each measurement


data_mean_std <-finalData[,grepl("mean|std|activityId",colnames(finalData))]
head(data_mean_std, n=1)


#3. #Uses descriptive activity names to name the activities in the data set


library(plyr)

data_mean_std <- join(data_mean_std, activityLabel, by = "activityId", match = "first")

data_mean_std <-data_mean_std[,-1]


#4. Appropriately labels the data set with descriptive variable names.

#Remove parentheses

names(data_mean_std) <- gsub("\\(|\\)", "", names(data_mean_std), perl  = TRUE)

#correct syntax in names

names(data_mean_std) <- make.names(names(data_mean_std))

#add descriptive names

names(data_mean_std) <- gsub("Acc", "Acceleration", names(data_mean_std))
names(data_mean_std) <- gsub("^t", "Time", names(data_mean_std))
names(data_mean_std) <- gsub("^f", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("BodyBody", "Body", names(data_mean_std))
names(data_mean_std) <- gsub("mean", "Mean", names(data_mean_std))
names(data_mean_std) <- gsub("std", "Std", names(data_mean_std))
names(data_mean_std) <- gsub("Freq", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("Mag", "Magnitude", names(data_mean_std))

#creates a second, independent tidy data set with the average of each variable for each activity and each subject.


tidydata_average_sub<- ddply(data_mean_std, c("subject","activity"), numcolwise(mean))


write.table(tidydata_average_sub,file="tidydata.txt")
