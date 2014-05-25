### STEP 1: START WITH COMBINING DATA FIRST ###

# read test data into R
testSubject <- read.table("test/subject_test.txt", header=FALSE)
testData <- read.table("test/X_test.txt", header=FALSE)
testActivity <- read.table("test/y_test.txt", header=FALSE)

# read train data into R
trainSubject <- read.table("train/subject_train.txt", header=FALSE)
trainData <- read.table("train/X_train.txt", header=FALSE)
trainActivity <- read.table("train/y_train.txt", header=FALSE)

# combine individual datasets
testSet <-  cbind(testData, testSubject, testActivity)
trainSet <-  cbind(trainData, trainSubject, trainActivity)

# set names of individual dataframes together and combine all into a single dataset
names(trainSet) = names(testSet)
allData <- rbind(trainSet, testSet)

# add names to column by extracting names from feature.txt
colHeadPart1 <- read.table("features.txt", header=FALSE)

# append two rows for subject and activity code of data
allNames <- rbind(colHeadPart1, data.frame(V1 = c(562, 563),  V2 = c("Subject","Activity")))
names(allData) = allNames[,2] # set names equal to vector of names from features.txt

### STEP 2: Extract only the measurements on the mean/standard deviation ###

# return only columns with measurements for mean/std deviation + subject/activity
retainCols <-  grep("mean\\(\\)|std\\(\\)|subject|activity", allNames$V2, ignore.case=TRUE)
# How many columns expected
table(grepl("mean\\(\\)|std\\(\\)|subject|activity", allNames$V2, ignore.case=TRUE))

# perform subset
subsettedData <- subset(allData, select=retainCols)

### STEP 3: Clean up column names ###

# decision to not use lower case as this makes the variables much less readable.
# Having an upper case letter hints to user the potential meaning of a variable name
names(subsettedData) <- gsub("\\(\\)|-","",names(subsettedData))

# change mean and std to capital letters too to make them more readable
names(subsettedData) <- gsub("mean","Mean",names(subsettedData))
names(subsettedData) <- gsub("std","Std",names(subsettedData))

### STEP 4: Make labels for the activity descriptive with the activity name ###

# import activity names and change their header names
activitylabel <- read.table("activity_labels.txt", header=FALSE)
names(activitylabel) <- c("Activity","activity")

allData <- merge(subsettedData, activitylabel, by.x = "Activity", by.y="Activity")
allData <- allData[, !(names(allData) %in% "Activity")] # drop column with activity number
dim(allData) to confirm dimensions of new dataset

### STEP 5: Make a second tidy dataset with the average of each variable for each activity and each subject ###

library(plyr)

dim(aggregate(.~ Subject + activity, data=allData, FUN=mean))
tidyDataset <- aggregate(.~ Subject + activity, data=allData, FUN=mean)
write.table(tidyDataset, "tidyDataset.txt")
