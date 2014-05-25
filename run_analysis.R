run_analysis <- function(outcome) {

      if(!file.exists("./data")){dir.create("./data")}

      
      # read in label files
      activity.labels <- read.table("activity_labels.txt", sep = "")
      features <- read.table("features.txt", sep = "")
      
      # read in 'test' files & add column names for main data file
      subject.test <- read.table("subject_test.txt", sep = "")
      data.test <- read.table("X_test.txt", sep = "")
      activity.test <- read.table("y_test.txt", sep = "")
      colnames(data.test) <- features[,2]
      
      # setwd("./data")
      
      # create a new test data table which only contains columns with 'mean' or 'std' in the heading
      var.names <- c("mean", "std")
      patt <- sub(',\\s','|',(toString(var.names)))
      id.group <- grepl(patt,colnames(data.test))
      data.test <- data.test[,id.group]
      
      # read in 'train' files & add column names for main data file
      subject.train <- read.table("subject_train.txt", sep = "")
      data.train <- read.table("X_train.txt", sep = "")
      activity.train <- read.table("y_train.txt", sep = "")
      colnames(data.train) <- features[,2]
      
      # create a new train data table which only contains columns with 'mean' or 'std' in the heading
      data.train <- data.train[,id.group]
      
      # combine all 'test' data & add a column to identify the source ('test' or 'train')
      alltest.data <- cbind(subject.test, activity.test, data.test)
      alltest.data$type <- "test"
      
      # combine all 'train' data & add a column to identify the source ('test' or 'train')
      alltrain.data <- cbind(subject.train, activity.train, data.train)
      alltrain.data$type <- "train"
            
      # combine 'test' and 'train' data
      combined.data <- rbind(alltest.data, alltrain.data)
      
      # create column names for subject and activity in combined data 
      colnames(combined.data)[1] <- "subject"
      colnames(combined.data)[2] <- "activity"
      
      # replace activity numbers with activity labels by merging and then removing redundant columns 
      merged.data <- merge(activity.labels, combined.data, by.x="V1", by.y="activity")
      merged.data$V1 <- NULL
      colnames(merged.data)[1] <- "activity"
      
      # tidy up the column names to remove '-' and '()'
      old.colnames <- colnames(merged.data)
      new.colnames <- as.data.frame(old.colnames)
      new.colnames <- as.data.frame(sapply(new.colnames, gsub, pattern="\\(",replacement=""))
      new.colnames <- as.data.frame(sapply(new.colnames, gsub, pattern="\\)",replacement=""))
      new.colnames <- as.data.frame(sapply(new.colnames, gsub, pattern="-",replacement="."))
      
      colnames(merged.data) <- new.colnames[,1]
      
      # export cleaned data set
      write.table(merged.data, "./projectdata_1.txt", quote = FALSE, sep = " ")
      
      # create and export summarised data table
      # summary.data <- ftable(merged.data)
      # write.table(summary.data, "./projectdata_2.txt", quote = FALSE, sep = "")
      
} # end function