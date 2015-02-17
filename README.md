# Tidy Data Project
Getting &amp; Cleaning Data Course Project
#1) List of Files
 This repository contains 3 files:  
  1 README.md      - This file, it explains how the R-Code works per the project requirements  
  2 Codebook.md    - Documents the variables in the Final tidy data set  
  3 run_analysis.R - The R code that acquires, cleans and outputs the tidy data set.  All R code is
 contained in this file  
 
 #2) Some Background
 This data set holds data on motion experiments done on 30 subject users.  Each user performed 6 distinct
 actions WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
 wearing a smartphone (Samsung Galaxy S II) on the waist that recorded 3 dimensional (X,Y,Z)
  motion data.  The data was randomly partitioned into a test data set and a train data set.
  
  A full description is available at the site where the data was obtained: 
    http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

  Here are the data for the project: 
    https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
    
 The zipfile contains several files that need to be processed in order to create the final
 tidy data set.  Among the important ones are:  
 
 - activity_labels - Lists the 6 activities  
 
 - features: List the 561 measured motion variables for the two data sets (train and test)  
 
 - train/subject_train: Lists the subject number (in order) for each of the training data set tests  
 
 - train/y_train: Lists the activity number (in order) for each of the training data set tests  
 - train/X_train - Lists the motion data (in order) for each of the training data set tests.  There are 561
 motion parameters.  
 
 The previous 3 files can be combined to get a data frame with 
  subject,activity,measurements for each test in the training data set

 - test/subject_test: Lists the subject number (in order) for each of the test data set tests  
 - test/y_test: Lists the activity number (in order) for each of the test data set tests  
 - test/X_test: Lists the motion data (in order) for each of the test data set tests.  There are 561
 motion parameters.  
  
 The previous 3 files can be combined to get a data frame with 
  subject,activity,measurements for each test in the test data set


 #3) run_analysis.R script

  Download  gzip file and gunzip it.  The data file is located 
  at URL: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
 
  Create Training DF:
 - Set col names for train DF to data in file features.txt
 - read train/X_train.txt into train DF
 - Set col 1 of the train DF to the data in file train_subject.txt
 - Set col 2 of the train DF to the data in file y_train.txt
 	     
  Create Test DF:
 - Set col names for test DF to data in file features.txt
 - read train/X_test.txt into test DF
 - Set col 1 of the test DF to the data in file test_subject.txt
 - Set col 2 of the test DF to the data in file y_test.txt
 
  Combine (by rows) the Train and Test DFs into one DF
  
  Reduce this combined DF by only keeping columns with "mean" or "std" in the motion variable names
  
  Strip "."s out of the motion variable names
  
  Change activity numbers in col 2 of the DF to the proper enum values eg 1 changes to "WALKING"
 
 
 Create mean_data DF with 180 rows and 88 cols.
 				(30 subs X 6 activities = 180 rows)
 				 (subject + activity + 86 motion variables = 88 cols).  This will hold the final tidy data set.
 - Loop by subject and then within that loop by activity
  - create a tmp table with motion data for each subject/activity pair (eg sub1, act 1)
  - compute mean motion data for this tmp table
  - populate mean data from tmp table into proper row of final tidy table. 


 Write the final tidy table "mean_data.txt" to a file
 
 #4) Displaying output
 
  To display the output in a readable format
 - run the following R command on the mean_data.txt file created by the R script  

 data<-read.table("./mean_data.txt",header=TRUE)  #[1]

 1. Code for this command to read the output file was taken from a Discussion Forum post by David Hood
 https://class.coursera.org/getdata-008/forum/thread?thread_id=24
