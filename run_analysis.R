library(readr)
library(dplyr)

#Load feature and activity labels
features <- read_delim("./UCI HAR Dataset/features.txt", delim = " ", col_names = FALSE)
features <- unlist(features[,2])
activity_labels <- read_delim("./UCI HAR Dataset/activity_labels.txt", delim = " ", col_names = FALSE)
activity_labels <- unlist(activity_labels[,2])

#Load data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", sep = "", header = FALSE, col.names = features)
y_test <- read_delim("./UCI HAR Dataset/test/y_test.txt", delim = " ", col_names = "activity")
subject_test <- read_delim("./UCI HAR Dataset/test/subject_test.txt", delim = " ", col_names = "subject")
test_data <- cbind(subject_test,y_test,X_test) #Combine subject, activity and data

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", sep = "", header = FALSE, col.names = features)
y_train <- read_delim("./UCI HAR Dataset/train/y_train.txt", delim = " ", col_names = "activity")
subject_train <- read_delim("./UCI HAR Dataset/train/subject_train.txt", delim = " ", col_names = "subject")
train_data <- cbind(subject_train,y_train,X_train) #Combine subject, activity and data

full_data <- rbind(test_data,train_data) #Combine test and training

full_data <- arrange(full_data,subject,activity) #Arrange data according to subject and activity

relevant <- grep("mean()|std()",features) #Find means and standard deviations among features
irrelevant <- grep("meanFreq()",features) #Find meanFreq()
relevant <- setdiff(relevant,irrelevant) #Remove meanFreq() hits from search results
relevant_data <- full_data[,c(1,2,relevant+2)] #Select those columns + subject and activity

relevant_data$activity <- cut(relevant_data$activity, 6, labels = activity_labels) #Switch numbered factors to activity_labels

variables <- names(relevant_data) #Retrieve variable names
variables <- tolower(variables) #Remove upper-case letters
variables <- gsub("\\.","",variables) #Remove periods
variables <- gsub("^t","time",variables) #Switch leading "t" to "time"
variables <- gsub("^f","fourier",variables) #Switch leading "f" to "fourier"
variables <- gsub("mag","magnitude",variables) #Switch "mag" to "magnitude"

names(relevant_data) <- variables #Set as column names

tidy_data <- data.frame(relevant_data[1,]) #Initialize tidy_data with right number of columns

for (i in 1:30){
    for (j in 1:6) {
        index <- relevant_data$subject==i & relevant_data$activity==activity_labels[j]
        tidy_data[6*(i-1)+j,1:2] <- c(i,activity_labels[j])
        tidy_data[6*(i-1)+j,-(1:2)] <- colMeans(relevant_data[index,-(1:2)])
    }
}

tidy_data$subject <- factor(tidy_data$subject) #Make 'subject' a factor variable

write.table(tidy_data,file="tidy_data.txt",row.names = FALSE) #Write to file
