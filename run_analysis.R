
#Reading data in R from working directory

X_train<-read.table("./UCI HAR Dataset/train/X_train.txt") 
      # Read training data set into R
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt") 
      # Read training data set subject information into R
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt") 
      # Read training data set activity information into R
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt") 
      # Read test data set into R
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt") 
      #Read test data set subject information into R
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt") 
      #Read test data set activity information into R
dataset_features<-read.table("./UCI HAR Dataset/features.txt") 
      #Read data set feature description into R
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")

#Merging data tables

merged_data<-rbind(X_train, X_test) 
      #merge training and test data sets
merged_subject<-rbind(subject_train, subject_test) 
      #merge training and test subject information
merged_activity<-rbind(y_train, y_test) 
      #merge training and test activity information

#Indexing of data tables for subsetting only mean and standard deviation data

mean_index<-grep("mean()", dataset_features[,2], fixed=TRUE)
std_index<-grep("std()", dataset_features[,2], fixed=TRUE)

library(gtools)

feature_index<-mixedsort(c(mean_index, std_index)) 
      #concatenates mean and std index vectors for subsetting data set 

#Subset mean and std columns using feature_index to generate mean_std_subset

mean_std_subset<-merged_data[,feature_index] 
      #extracts mean and std columns using feature_index as the index

#Add empty column for activity labels to merged_activity index

merged_activity$activity_label<-NA
      
#Adds appropriate activity label to merged_activity index table
merged_activity$activity_label[merged_activity$V1==1]<-"WALKING"
merged_activity$activity_label[merged_activity$V1==2]<-"WALKING_UPSTAIRS"
merged_activity$activity_label[merged_activity$V1==3]<-"WALKING_DOWNSTAIRS"
merged_activity$activity_label[merged_activity$V1==4]<-"SITTING"
merged_activity$activity_label[merged_activity$V1==5]<-"STANDING"
merged_activity$activity_label[merged_activity$V1==6]<-"LAYING"

#Merge merged_activity activity_label column with mean_std_subset data table
activities_mean_std_subset<-cbind(merged_activity[,2], mean_std_subset) 

#Convert column labels for variables to proper syntax
good_syntax<-colnames(activities_mean_std_subset)
make.names(good_syntax)
indexed_activity_label<-dataset_features[feature_index,] 
        #creates index of activity labels
activity_label_index_vector<-as.vector(indexed_activity_label[,2]) 
        #creates vector of descriptive activity labels
updated_activity_label_index_vector<-c("Activity", activity_label_index_vector) 
        #adds "activity" to beginning of vector to account for activity column
colnames(activities_mean_std_subset)<-updated_activity_label_index_vector 
        #renames columns with descriptive labels for activities and statistic

clean_data<-cbind(merged_subject, activities_mean_std_subset) #adds subject information to the datatable
colnames(clean_data)[1]<-"Subject" #Changes column "V1" label to "Subject"

#Generate new data table of variable means, output data table, and view on screen.
library(plyr)
mean_results_summary<-ddply(clean_data, c("Subject", "Activity"), numcolwise(mean)) #Means for each activity organized by subject.

#Write mean_results_summary to a a txt file (tidy_data.txt) in working directory and view output mean_results_summary in R.
write.table(mean_results_summary, file="tidy_data.txt", row.names=FALSE)

View(mean_results_summary)