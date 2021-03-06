# Tidy Data Project
Getting &amp; Cleaning Data Course Project
#1) List of Files
 This repository contains 3 files:  
  1) README.md      - This file, it explains how the R-Code works per the project requirements  
  2) Codebook.md    - Documents the variables in the Final tidy data set  
  3) run_analysis.R - The R code that acquires, cleans and outputs the tidy data set.  All R code is
 contained in this file  
 
#2) Project description and Data sources
 This data set holds data on motion experiments done on 30 subject users.  Each user performed 6 distinct
 actions WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
 wearing a smartphone (Samsung Galaxy S II) on the waist that recorded 3 dimensional (X,Y,Z)
  motion data.  The data was randomly partitioned into a test data set and a train data set.
 
The original paper: https://www.elen.ucl.ac.be/Proceedings/esann/esannpdf/es2013-84.pdf, which goes into more detail  
on the sampling protocols, filetring, etc. done to the data captured from the Android phone sensors. [#1]
  
  A full description of the data set is available at the site where the data was obtained: 
    http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here is the link to the data used for this project: 
    https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The R script run_analysis.R will download and unzip the data, if the script is to be run multiple times,
the first 10 lines of code should be commented out to reduce processing time.
  
The zipfile contains several files that need to be processed in order to create the final
tidy data set.  Among the important ones are:
 
 - activity_labels - Lists the 6 activities
 - features: List the 561 measured motion variables for the two data sets (train and test)
 - train/subject_train: Lists the subject number (in order) for each of the training data set tests
 - train/y_train: Lists the activity number (in order) for each of the training data set tests
 - train/X_train - Lists the motion data (in order) for each of the training data set tests, 561 motion parameters.
 
The previous 3 files can be combined to get a data frame with subject,activity,and measurements for each test in the training data set

 - test/subject_test: Lists the subject number (in order) for each of the test data set tests
 - test/y_test: Lists the activity number (in order) for each of the test data set tests
 - test/X_test: Lists the motion data (in order) for each of the test data set tests, 561 motion parameters.

The previous 3 files can be combined to get a data frame with subject,activity,measurements for each test in the test data set

#3) R Code --  run_analysis.R
STEP A - load the data from the web, run one time
Download the zip file and unzip it.  The data file is located at:
    URL: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
 
STEP 1 - load the TRAIN data from local files

Create the Training Data Frame (DF):
 - Set col names for the train DF to the data from the file features.txt
 - read train/X_train.txt into train DF
 - Set col 1 of the train DF to the data from the file train_subject.txt
 - Set col 2 of the train DF to the data from the file y_train.txt
 	     
STEP 2 - load the TEST data from the local files

Create the Test DF:
 - Set col names for the test DF to the data from the file features.txt
 - read train/X_test.txt into test DF
 - Set col 1 of the test DF to the data from the file test_subject.txt
 - Set col 2 of the test DF to the data from the file y_test.txt

STEP 3 - clean the data and merge TRAIN with TEST to create one data set

Combine (by rows) the Train and Test DFs into one DF  (RBIND)
Reduce this combined DF, keeping only columns with "mean" or "std" in the motion variable names
Strip "."s out of the motion variable names
Change activity numbers in col 2 of the DF to the proper enum values eg 1 should be changed to "WALKING"
 
STEP 4 - create final tidy data set

Create the mean_data DF with 180 rows and 88 cols to hold the final tidy data set.
 				 - 30 subjects X 6 activities = 180 rows
 				 - each record = a subject + an activity + 86 motion variables for a total of 88 cols  
Group by subject and by activity with the mean for each motion data element
Write the final tidy table "mean_data.txt" to a file
 
#4) Displaying output

To display the output in a readable format [#2]
 - run the following R command on the mean_data.txt file created by the R script

      data<-read.table("./mean_data.txt",header=TRUE)

#5) Footnotes and References

1.) Codebook suggestions and a good source for varible units provided by Jesus Castagnetto (Community TA) at the following discussion thread https://class.coursera.org/getdata-011/forum/thread?thread_id=249

2.) Code for this command to read the output file was taken from a Discussion Forum post by David Hood
 https://class.coursera.org/getdata-008/forum/thread?thread_id=24
