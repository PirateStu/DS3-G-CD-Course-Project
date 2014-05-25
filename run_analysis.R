run_analysis <- function() {

      # create directory for output files
      if(!file.exists("./data")){dir.create("./data")}
      
      # read in label files (column names ang categorical activity data)
      activity.labels <- read.table("activity_labels.txt", sep = "")
      features <- read.table("features.txt", sep = "")
      
      # read in 'test' files & add column names for main data file
      subject.test <- read.table("subject_test.txt", sep = "")
      data.test <- read.table("X_test.txt", sep = "")
      activity.test <- read.table("y_test.txt", sep = "")
      colnames(data.test) <- features[,2]
      
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
      colnames(combined.data)[1] <- "subject.id"
      colnames(combined.data)[2] <- "activity"
      
      # replace activity numbers with activity labels by merging and then removing redundant columns 
      merged.data <- merge(activity.labels, combined.data, by.x="V1", by.y="activity")
      colnames(merged.data)[1] <- "activity.id"
      colnames(merged.data)[2] <- "activity.label"
      
      # tidy up the column names to remove '-' and '()'
      old.colnames <- colnames(merged.data)
      new.colnames <- as.data.frame(old.colnames)
      new.colnames <- as.data.frame(sapply(new.colnames, gsub, pattern="\\(",replacement=""))
      new.colnames <- as.data.frame(sapply(new.colnames, gsub, pattern="\\)",replacement=""))
      new.colnames <- as.data.frame(sapply(new.colnames, gsub, pattern="-",replacement="."))
      
      colnames(merged.data) <- new.colnames[,1]
      
      # export cleaned data set
      write.table(merged.data, "./data/clean_data.txt", quote = FALSE, sep = " ")
      
      # create factors from numerics for id variables
      merged.data[,1] <- as.factor(merged.data[,1])
      merged.data[,3] <- as.factor(merged.data[,3])
      
      # split data and create variable names
      split.by <- split(merged.data, list(merged.data$subject.id,merged.data$activity.label))
      var.names <- c(colnames(merged.data))
      var.names <- var.names[-3:0]; var.names <- var.names[-80]
      
      # create tidy data set
      summary.data <- sapply(split.by, function(x) round(colMeans(x[, var.names], na.rm=TRUE), digits=6))

      # export summarised data table
      write.table(summary.data, "./data/tidy_data.txt", quote = FALSE, sep = " ")
      
} # end function
