# Create a tidy data set from the Samsung motion data sets                                         FEB-2015
#
# STEP A. Download the ZIP file & extract the data, perform this step only once, and then comment them out!
#
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Dataset.zip")
unzip("./Dataset.zip")
#
# The working directory for the Dateset will be ./UCI HAR Dataset
# 
# STEP 1 - FEATURES.TXT (list of 561 measured motion variables)
# Read the feature names into a table Feat_raw, all data
# Read the 2nd column of Feat_raw  into a vector Feat_nam  
# These will become the column names for the features in the dataset
Feat_raw <- read.table("./UCI HAR Dataset/features.txt")
Feat_nam <- Feat_raw$V2 

# STEP 2 Process the Train Data Set
#         - TRAIN/X_TRAIN.TXT       (7352 rows of data for the 561 motion variables, in order)
#         - TRAIN/Y_TRAIN.TXT       (7352 rows of activities, in order)
#         - TRAIN/SUBJECT_TRAIN.TXT (7352 rows of subject indexes for the 561 motion variables, in order)
#
# STEP 2a - Create the train data set and use the Features vector from STEP 1 to name the columns
train <- read.table("./UCI HAR Dataset/train/X_train.txt",col.names=Feat_nam)

# STEP 2b - create vectors for the train subjects (Sub_train) activities (Act_train) data
Sub_train <- read.table("./UCI HAR Dataset/train/subject_train.txt",col.names="subject")
Act_train <- read.table("./UCI HAR Dataset/train/y_train.txt",col.names="activity")

# STEP 2c - add subject and activity vectors as first two cols to the train data set from STEP 2a
train <- cbind(Sub_train, Act_train, train)

# STEP 3 Process the Test Data Set
#         - TRAIN/X_TEST.TXT       (2947 rows of data for the 561 motion variables, in order)
#         - TRAIN/Y_TEST.TXT       (2947 rows of activities, in order)
#         - TRAIN/SUBJECT_TEST.TXT (2947 rows of subject indexes for the 561 motion variables, in order)
#
# STEP 3a - Create the train data set and use the Features vector from STEP 1 to name the columns
test <- read.table("./UCI HAR Dataset/test/X_test.txt",col.names=Feat_nam)

# STEP 3b - create vectors for the test subjects (Sub_test) activities (Act_test) data
Sub_test <- read.table("./UCI HAR Dataset/test/subject_test.txt",col.names="subject")
Act_test <- read.table("./UCI HAR Dataset/test/y_test.txt",col.names="activity")

# STEP 3c - add subject and activity vectors as first two cols to the test data set from STEP 3a
test <- cbind(Sub_test,Act_test,test)

# STEP 4 Clean the combined TEST_TRAIN data set
#
# STEP 4a - combine the train and test data into one data frame using rbind()
train_test <- rbind(train,test)

# STEP 4b - create a vector of column numbers from the Features vector that have "mean" in their name
mean_feats <- grep("mean",Feat_nam,ignore.case=TRUE)

# STEP 4c - create a vector of column numbers from the Features vector that have "std" in their name
std_feats<-grep("std",Feat_nam,ignore.case=TRUE)

# STEP 4d - combine the list of mean and std col numbers and then sort in ascending order
combined_feats <- c(mean_feats, std_feats)
combined_feats <- sort.int(combined_feats)

# STEP 4e - add 2 to each col index as an offset to account for subject, and activity to get 88 cols
combined_feats <- combined_feats+2

# STEP 4f - select only the columns with "mean" and "std" in their name from the combined train_test
#           DF from STEP 4a, and add the subject and activity columns to get 88 cols in train_test data set
train_test <- train_test[ ,c(1,2,combined_feats)]

# STEP 4g - remove the "." characters from the column names
newNames <- gsub("\\.","",names(train_test),) 
colnames(train_test) <- newNames

# STEP 4h - change the activity level values from integer to the text lables found in file activity_labels.txt
train_test$activity <- factor(train_test$activity,level=1:6,labels=c('WALKING','WALKING_UPSTAIRS','WALKING_DOWNSTAIRS','SITTING','STANDING','LAYING'))

#STEP 5 Create the final tidy data set
#
#STEP 5a - create the aggregate DF for final data:  30 subs X 6 Activities = 180 rows   86 features + sub + act = 88 cols
mean_data <- aggregate(train_test[,3:88], by=list(train_test$subject, train_test$activity), FUN="mean")


# STEP 6 write the final data set, mean_data to a text file in the working directory
write.table(mean_data,file="./mean_data.txt",row.name=FALSE)
