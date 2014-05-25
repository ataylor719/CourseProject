## This function takes one argument - the file name of the resulting analysis
## table - and returns the analysis to that location in a tab-delimited format.
## It checks to see if the necessary dataset is in the working directory,
## downloads and unzips the files if necessary, then creates a new table that
## has the average value of select variables for each subject and subject 
## activity.

run_analysis <- function(outputfile = "analysis.csv") {
        
        ## Check for packages necessary to run the function
        require(plyr)
        require(utils)
        
        ## Check for data and download if subfolder is not present in 
        ## the working directory
        path <- "./UCI HAR Dataset/"
        if(!file.exists(path)) {
                fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                filename <- "getdata-projectfiles-UCI HAR Dataset.zip"
                download.file(fileurl, destfile = filename, method = "curl")
                unzip(filename)
        }
        
        ## Read the table that holds the feature names
        features <- read.table(paste0(path, "features.txt"), as.is = TRUE)
        
        ## Identify the features related to mean and standard deviation
        featuresindex <- sort(c(grep("mean\\(", features[,2]), 
                                grep("std", features[,2])))
        featurenames <- features[,2][featuresindex]
        
        ## Make the feature names lower case, without parentheses, dashes, and
        ## with more descriptive prefixes than "t" or "f"
        step1 <- tolower(featurenames)
        step2 <- sub("\\(\\)", "", step1,)
        step3 <- sub("-std", "stddev", step2,)
        step4 <- gsub("-", "", step3,)
        step5 <- sub("^t", "time", step4,)
        step6 <- sub("^f", "freq", step5,)
        finfeaturenames <- gsub("bodybody", "body", step6,)
        
        ## Add "subject" and "activity" to featurenames for the final table
        tidycolnames <- c("subjectid", "activity", finfeaturenames)
        
        ## Read the table that identifies the activity labels with descriptive
        ## language
        activitylabelsraw <- read.table(paste0(path, "activity_labels.txt"), 
                                        as.is = TRUE)
        # Make the activity labels lower case, without underscores
        activitylabelslower <- tolower(activitylabelsraw[,2])
        activitylabelsnodash <- gsub("_", "", activitylabelslower,)
        activitylabels <- data.frame(activitylabelsraw[,1], activitylabelsnodash)
        colnames(activitylabels) <- c("V1", "V2")
        
        ## Create a data.frame from the test file.
        
        ## Read in the data on the test group.
        testframefull <- read.table(paste0(path, "test/X_test.txt"), 
                                    as.is = TRUE)
        ## Select only the columns related to mean and standard deviation
        testframesub <- testframefull[,featuresindex]
        
        ## Read in the data that has the test subject codes
        testsubjects <- read.table(paste0(path, "test/subject_test.txt"), 
                                   as.is = TRUE)
        
        ## Read in the data that has the activity codes. Convert them to
        ## descriptive names.
        testactivitycode <- read.table(paste0(path, "test/y_test.txt"), 
                                       as.is = TRUE)
        testactivity <- join(activitylabels, testactivitycode, by = "V1", 
                             type = "right", match = "all")
        
        # Combine the tables into one data frame and assign the column names.
        testframe <- data.frame(testsubjects, testactivity[,2], testframesub)
        colnames(testframe) <- tidycolnames
        
        ## Create a data.frame from the training file.
        
        ## Read in the data on the training group.
        trainframefull <- read.table(paste0(path, "train/X_train.txt"), 
                                     as.is = TRUE)
        ## Select only the columns related to mean and standard deviation.
        trainframesub <- trainframefull[,featuresindex]
        
        ## Read in the data that has the training subject codes
        trainsubjects <- read.table(paste0(path, "train/subject_train.txt"),
                                    as.is = TRUE)
        
        ## Read in the data that has the activity codes. Convert them to
        ## descriptive names.
        trainactivitycode <- read.table(paste0(path, "train/y_train.txt"), 
                                        as.is = TRUE)
        trainactivity <- join(activitylabels, trainactivitycode, by = "V1", 
                              type = "right", match = "all")
        
        # Combine the tables into one data frame and assign the column names.
        trainframe <- data.frame(trainsubjects, trainactivity[,2], 
                                 trainframesub)
        colnames(trainframe) <- tidycolnames
        
        ## Bind the test and training data.frames
        fullframe <- rbind(testframe, trainframe, deparse.level = 0)
        
        ## Create new data.frame with the average of each variable for each 
        ## subject and activity
        averaging <- function(x) mean(x)
        reshapeframe <- ddply(fullframe, .(subjectid, activity), 
                              colwise(averaging))
        
        ## Write the new table to a file in the working directory
        write.csv(reshapeframe, outputfile, row.names = FALSE)
}