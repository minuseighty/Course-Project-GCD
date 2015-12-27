run_analysis <- function() {
    
    filename <- "dataset.zip"
    library(dplyr)
    
    ## Step 1: Download dataset:
    if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, filename, method="curl")
    }  
    if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
    }    
    
    ## Step 2: import data
    
    ## Features
    features <- read.table("UCI HAR Dataset/features.txt")
    features[,2] <- as.character(features[,2])
    ## Activities
    activity.lab <- read.table("UCI HAR Dataset/activity_labels.txt")
    activity.lab[,2] <- as.character(activity.lab[,2])
    ## Test Set
    test.dta <- read.table("UCI HAR Dataset/test/X_test.txt") ##test set
    test.lab <- read.table("UCI HAR Dataset/test/Y_test.txt") ##test label
    test.sub <- read.table("UCI HAR Dataset/test/subject_test.txt") ##test subject
    test <- cbind(test.sub, test.lab, test.dta) ## test data frame
    ## Training Set
    training.dta <- read.table("UCI HAR Dataset/train/X_train.txt") ##trainng set
    training.lab <- read.table("UCI HAR Dataset/train/Y_train.txt") ##training label
    training.sub <- read.table("UCI HAR Dataset/train/subject_train.txt") ##training subject
    training <- cbind(training.sub, training.lab, training.dta) ## training data frame
    
    ## Step 3: Merge training and test
    train.test <- rbind(training,test)
    
    ## clean up column names
    colnames(train.test) <- c("subject", "activity", features[ ,2])
    
    ## Step 4: Find and subset mean and std columns
    
    ## find mean and std columns
    sub.names <- features$V2[grep("mean\\(\\)|std\\(\\)",features$V2)] 
    
    ## add in activity and subject also convert sub.names to character vector
    sub.names2 <- c("subject", "activity", as.character(sub.names))
    
    ## subset data set
    train.test <- train.test[ ,sub.names2]
    
    ## Step 5: Use descriptive activity names to name the activities in the data set
    train.test$activity <- factor(train.test$activity, levels = activity.lab[,1], labels = activity.lab[,2])
    
    ## Step 6: Change the default column names to something more readable 
    names(train.test) <- gsub("^t", "time", names(train.test))
    names(train.test) <- gsub("^f", "frequency", names(train.test))
    names(train.test) <- gsub("Acc", "Accelerometer", names(train.test))
    names(train.test) <- gsub("Gyro", "Gyroscope", names(train.test))
    names(train.test) <- gsub("Mag", "Magnitude", names(train.test))
    names(train.test) <- gsub("BodyBody", "Body", names(train.test))
    
    ## Step 7: Create the txt file with averages
    tidy.data <- aggregate(. ~subject + activity, train.test, mean)
    
    ## order by subject
    tidy.data <- tidy.data[order(tidy.data$subject), ]
    
    ## output txt file to folder
    write.table(tidy.data, file = "tidydataset.txt",row.name=FALSE)
}