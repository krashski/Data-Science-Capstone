
#### 1. LOAD THE DATA

# # download the data set
# fileUrl <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/yelp_dataset_challenge_academic_dataset.zip'
# download.file(fileUrl, destfile = 'yelp_data.zip', method = 'curl')

# # unzip the data set
# unzip('yelp_data.zip')

# load the data
library(jsonlite)
file1 <- 'yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json'
file3 <- 'yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json'
file4 <- 'yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_tip.json'
file5 <- 'yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_user.json'

# turn each JSON file into a proper array by separating each object with a comma 
# and wrapping it in an array with square brackets
# flatten = TRUE removes the nested data frames

# Timer on
cat("\nStart processing:", format(Sys.time()),"\n")
ptm <- proc.time()

df1 <- fromJSON(sprintf('[%s]', paste(readLines(file1), collapse=',')), flatten = TRUE)
df2 <- fromJSON(sprintf('[%s]', paste(readLines(file2), collapse=',')), flatten = TRUE)
df3 <- fromJSON(sprintf('[%s]', paste(readLines(file3), collapse=',')), flatten = TRUE)
df4 <- fromJSON(sprintf('[%s]', paste(readLines(file4), collapse=',')), flatten = TRUE)
df5 <- fromJSON(sprintf('[%s]', paste(readLines(file5), collapse=',')), flatten = TRUE)

# Timer off
et <- proc.time() - ptm
cat("\nTotal elapsed in minutes: ", et[3]/60)
cat("\nEnd processing:", format(Sys.time()),"\n")

# final counts:
# df1: 61,184 businesses
# df2: 45,166 checkins
# df3: 1,569,264 reviews
# df4: 495,107 tips
# df5: 366,715 users

# cleanup and save the workspace
rm(list = ls(pattern = 'file'))
save.image(file = 'yelp.RData')

#### 2. CLEAN THE DATA

#load('yelp.RData')

### Checkin, Tips, and Users data not used in this project

### Business data

# convert list variables to strings
list.names <- names(df1)[c(4, 8, 28, 29, 30)]
for (i in list.names) {
    df1[[i]] <- sapply(df1[[i]], toString)
}

# recode logical variables to 1/0
# automatically converts to numeric
df1[df1 == 'TRUE'] <- 1
df1[df1 == 'FALSE'] <- 0

# record character variables to numeric
char.names <- names(df1)[28:30]
for (i in char.names) {
    df1[[i]] <- as.numeric(df1[[i]])
}

### Review data

# convert date to POSIX format, strip out year, month, weekday.
library(lubridate)
df3$date <- ymd(df3$date)
df3$review.year <- year(df3$date)
df3$review.month <- month(df3$date, label = TRUE)
df3$review.day <- wday(df3$date, label = TRUE)

# save workspace
rm(char.names, i, list.names)
save.image(file = 'yelp.RData')

