#You should create one R script called run_analysis.R that does the following. 

#1.Merges the training and the test sets to create one data set.

#2.Extracts only the measurements on the mean 
#and standard deviation for each measurement. 

#3.Uses descriptive activity names to name the activities in the data set

#4.Appropriately labels the data set with descriptive variable names. 
#5.From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable 
#for each activity and each subject.

run_analysis <- function() {
        
        url<-"http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

        download.file(url,"dataset.zip",mode="wb")

        unzip("dataset.zip")

        features<-read.table("./UCI HAR Dataset/features.txt",header=FALSE,sep="")

        features$V2<-as.character(features$V2)
        
        #Creating a list of colomn's names
        list.col.names<-c("subject","label")

        for (i in seq(1,nrow(features))) {
                list.col.names<-c(list.col.names,features$V2[i])  
        }

        #Get test raw data
        t.labels<-read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE,sep="")
        t.set<-read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE,sep="")
        t.subject<-read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE,sep="")

        test.data<-cbind(t.subject,t.labels,t.set)
        names(test.data)<-make.names(list.col.names,unique=TRUE)

        #Get train raw data
        train.labels<-read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE,sep="")
        train.set<-read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE,sep="")
        train.subject<-read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE,sep="")

        train.data<-cbind(train.subject,train.labels,train.set)
        names(train.data)<-make.names(list.col.names,unique=TRUE)

        #Join test and train data
        general.data<-rbind(test.data,train.data)

        #Appropriately labels the data set with descriptive variable names
        general.data$label<-factor(general.data$label)
        levels(general.data$label)<-c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

        #Extracts only the measurements on the mean 
        #and standard deviation for each measurement
        mean.data<-select(general.data,contains(".mean."))
        std.data<-select(general.data,contains("std"))
        sl.data<-select(general.data,1:2)

        g.data<-cbind(sl.data,std.data,mean.data)
        g.data$subject<-factor(g.data$subject)

        #Create data set with the average of each variable 
        #for each activity and each subject
        group.data<-group_by(g.data,subject,label)

        tidy.data<-summarise_each(group.data,funs(mean))

        #Inpute tidy data in txt file
        write.table(tidy.data,file="./UCI HAR Dataset/tidy_data.txt",row.name=FALSE)
}
