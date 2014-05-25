Course Project: Getting & Cleaning Data
========================================================

## Introduction

This project forms part of the data Science Specialisation from John Hopkins University via Coursera.

The project uses 8 text files from the Human Activity Recognition Using Smartphones Data Set which carried out experiments with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities wearing a smartphone (Samsung Galaxy S II) on the waist.

* Walking
* Walking Upstairs
* Walking Downstairs
* Sitting
* Standing
* Laying

The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The data used here consists of 8 different files. The first two files provide categorical data:

* features.txt - which lists all the variables collected for each subject 
* activity_labels.txt - which lists the activities undertaken (see list above)

Then there are two sets of data each containing 3 files. The first set of data was the training set:
* subject_train.txt - contains the set subject identifiers (numerics from 1-30) for each line in the main data file
* y_train.txt - contains the activity identifiers (numerics from 1-6) for each activity as identified above
* X_train.txt - contains the data for all variables for each subject and activity combination

There is a matching set of files and data for 'test' data

## R Function

The R function (run_analysis.R) works through the following steps:

* Imports the 8 data files
* Adds the columns names from features.txt file to the two data files (test & train)
* Removes the unwamted columns from the data files (keeping only those columns with 'mean' or 'std' in the column name)
* Combines the data from the 2 data files into a single file
* Includes the text labels from activity_labels.txt
* Tidies up the column names by removing the unwanted characters ('-', '()')
* Exports a copy of the cleaned and tidied data file to a text file (clean_data.txt)
* Calculates the means for each variable by the combination of 'subject' and 'activity'
* Exports a copy of the 'tidy' data set to a text file (tidy_data.txt)

## Detail

Note that all text files are required to be in the working directory
First, read in the label information

```{r}
activity.labels <- read.table("activity_labels.txt", sep = "")
features <- read.table("features.txt", sep = "")
```

Read in the 'test' files using read.table with sep=""

```{r}
subject.test <- read.table("subject_test.txt", sep = "")
data.test <- read.table("X_test.txt", sep = "")
activity.test <- read.table("y_test.txt", sep = "")
```

Column names are then assign using the labels from the 'features' file

```{r}
colnames(data.test) <- features[,2]
```

Remove the columns which do not contain the text strings 'mean' or 'std' (using grepl)

```{r}
var.names <- c("mean", "std")
patt <- sub(',\\s','|',(toString(var.names)))
id.group <- grepl(patt,colnames(data.test))
data.test <- data.test[,id.group]
```

Repeat the process for the 'training' data

Combine all thet 'test' files into a single file (using cbind) and add a column which keeps the source (ie. 'test' or 'train')

```{r}
alltest.data <- cbind(subject.test, activity.test, data.test)
alltest.data$type <- "test"
```

Repeat for the 'training' data and then combine the the two files, 'test' and 'train' into a single file (using rbind)

```{r}
combined.data <- rbind(alltest.data, alltrain.data)
```

The column names for the variables will carry over from the data files when using cbind, but the first two columns of the new file will need names ('subject.id' and 'activity')

Merge the now combined data file with the activity labels files to get text labels for activity rather than just number identifiers

```{r}
merged.data <- merge(activity.labels, combined.data, by.x="V1", by.y="activity")
colnames(merged.data)[1] <- "activity.id"
colnames(merged.data)[2] <- "activity.label"
```

Copy the column names to a new table and remove the redundant or unwanted characters (such as '-' or '()'). Then copy the column names back to the main file (it's a little messy but it gest there in the end)

```{r}
old.colnames <- colnames(merged.data)
new.colnames <- as.data.frame(old.colnames)
new.colnames <- as.data.frame(sapply(new.colnames, gsub, pattern="\\(",replacement=""))
new.colnames <- as.data.frame(sapply(new.colnames, gsub, pattern="\\)",replacement=""))
new.colnames <- as.data.frame(sapply(new.colnames, gsub, pattern="-",replacement="."))
colnames(merged.data) <- new.colnames[,1]
```

Export the cleaned data file to a text file

```{r}
write.table(merged.data, "./data/clean_data.txt", quote = FALSE, sep = " ")
```

Split the cleaned data file by 'subject' and 'activity'

```{r}
split.by <- split(merged.data, list(merged.data$subject.id,merged.data$activity.label))
```

Create a list of variables names from the column names in the main data file, removing the names for 'subject', 'activity' and 'type'

```{r}
var.names <- c(colnames(merged.data))
var.names <- var.names[-3:0]; var.names <- var.names[-80]
```

Summarise the data by calculating the means for each column (split by 'subject' and 'activity'), and then export the tidty data set to a text file

```{r}
summary.data <- sapply(split.by, function(x) round(colMeans(x[, var.names], na.rm=TRUE), digits=6))
write.table(summary.data, "./data/tidy_data.txt", quote = FALSE, sep = " ")
```
