
#### FEATURE ENGINEERING

load('yelp.RData')

### 1. Businesses
# create a new data frame of businesses
rest <- grep('Restaurants', df1$categories)
dfbus <- df1[rest, ]

# remove duplicate 'Good for Kids' variable
dfbus <- dfbus[, -54]

# remove variables with > 50% missing values
library(prettyR)
x <- sapply(dfbus, valid.n)
keep <- names(dfbus[x > (nrow(dfbus) * 0.5)])
dfbus <- subset(dfbus, select = names(dfbus) %in% keep)

# remove other variables that will not be used (address, name, hours etc.)
dfbus <- dfbus[, -c(2, 8, 10, 13:27)]

# remove 'attributes' tag from variable names
names(dfbus) <- gsub('attributes\\.', '', names(dfbus))

# list the restaurant subcategories
types <- c('American', 'Irish', 'Chinese', 'Italian', 'Burgers', 'Seafood', 
           'Pizza', 'German', 'Greek', 'Indian', 'Mexican', 'Japanese', 
           'Tapas', 'Korean', 'Vietnamese', 'Middle Eastern', 'Asian Fusion', 
           'Steakhouses', 'Fast Food', 'Breakfast & Brunch', 'Cafes', 'Bars',
           'Sandwiches', 'Sports Bars', 'Mediterranean', 'Vegetarian', 'Pubs',
           'Delis', 'Bistros', 'Barbeque', 'Diners', 'Sushi Bars', 'Thai',
           'Southern', 'French', 'Caribbean', 'Fondue', 'Ethiopian', 'Cajun',
           'Hot Dogs', 'Chicken Wings', 'Hawaiian', 'Buffets', 'Fish & Chips',
           'Cuban', 'Filipino', 'Soul Food', 'Russian', 'Himalayan', 'British', 
           'Iranian', 'Mongolian', 'Tex-Mex', 'Vegan', 'Belgian', 'Peruvian', 
           'Brazilian', 'Taiwanese', 'Creperies', 'Canadian', 'African', 
           'Portuguese', 'Spanish', 'Polish', 'Afghan', 'Modern European', 
           'Turkish', 'Pakistani', 'Gastropubs', 'Bakeries', 'Scottish', 
           'Moroccan', 'Kebab', 'Cheesesteaks', 'Brasseries', 'Poutineries', 
           'Halal', 'Comfort Food', 'Kosher', 'Gluten-Free')

# create new variable based on subcategories
dfbus$cat <- 0
for (i in seq_along(types)) {
    x <- grepl(types[[i]], dfbus$categories)
    dfbus$cat <- ifelse(x == TRUE, i, dfbus$cat)
}

# recode 0 (Other) to 99
dfbus$cat[dfbus$cat == 0] <- 99

# Weight of Evidence (WOE) transformation for restaurant categories
dfbus$WOE.Category <- 0
for (i in seq_along(types)) {
    x <- log((table(dfbus$cat, dfbus$open)[i, 1] / table(dfbus$open)[1]) /
             (table(dfbus$cat, dfbus$open)[i, 2] / table(dfbus$open)[2]))
    dfbus$WOE.Category[dfbus$cat == i] <- x
}

# Add WOE for when restaurant category = Other
dfbus$WOE.Category[dfbus$WOE.Category == 0] <- log((table(dfbus$cat, dfbus$open)[81, 1] / table(dfbus$open)[1]) /
                                         (table(dfbus$cat, dfbus$open)[81, 2] / table(dfbus$open)[2]))

# Add WOE for when no restaurants in the category are closed
n <- as.numeric(which(table(dfbus$cat, dfbus$open)[, 1] == 0))
dfbus$WOE.Category[dfbus$cat == n] <- log((table(dfbus$open)[2] /
                                    ((table(dfbus$cat, dfbus$open)[n, 2] * 
                                      table(dfbus$open)[1]) + table(dfbus$open)[2])))

# create factor variable with labels for exploratory data analysis
dfbus$cat <- factor(dfbus$cat, levels = c(1:81), labels = c(types, 'Other'))

# create new city variable from latitude and longitude
dfbus$city <- NULL # remove old city variable 
library(ggmap)
cities <- c('Charlotte, NC', 'Edinburgh, UK', 'Karlsruhe, Germany', 
            'Las Vegas, NV', 'Madison, WI', 'Montreal, Canada', 
            'Phoenix, AZ', 'Pittsburgh, PA', 'Urbana-Champaign, IL',
            'Waterloo, Canada')
city.centres <- geocode(cities, source = 'google')
set.seed(42)
geo <- kmeans(dfbus[, c('longitude', 'latitude')], city.centres)
aggregate(dfbus, by = list(geo$cluster), FUN = mean)
dfbus <- data.frame(dfbus, geo$cluster)
names(dfbus)[46] <- 'city'

# Weight of Evidence transformation for cities
dfbus$WOE.City <- 0
for (i in seq_along(cities)) {
    x <- log((table(dfbus$city, dfbus$open)[[i, 1]] / table(dfbus$open)[1]) /
             (table(dfbus$city, dfbus$open)[[i, 2]] / table(dfbus$open)[2]))
    dfbus$WOE.City[dfbus$city == i] <- x    
}

# recode city variable to factor for exploratory data analysis
x <- strsplit(cities, ',') # remove state/country from city name
for (i in 1:10) { cities[i] <- x[[i]][1] }
dfbus$city <- factor(dfbus$city, levels = c(1:10), labels = cities)

# identify character variables to be recoded to dummy variables
char <- sapply(dfbus, is.character)
to_recode <- which(char == TRUE & !names(char) %in% c('business_id', 'categories', 'name', 'city'))
chardata <- dfbus[, c(2, to_recode)]

# recode character variables to dummy variables
library(caret)
dummies <- dummyVars(open ~ ., data = chardata, fullRank = TRUE)
data.dum <- data.frame(predict(dummies, newdata = chardata))
dfbus <- data.frame(dfbus, data.dum)
dfbus <- dfbus[, -to_recode] # remove original character variables

# clean up variable names
oldval <- c('X\\.', 'Alcohol', 'Attire', 'Noise.Level', 'Wi.Fi', 'review_count')
newval <- c('', 'Alcohol.', 'Attire.', 'NoiseLevel.', 'WiFi.', 'Review.Count')
for (i in seq_along(oldval)) {
    names(dfbus) <- gsub(oldval[[i]], newval[[i]], names(dfbus))
}

# number of words in the restaurant's name
library(stringi)
dfbus$Name.Length <- stri_count(dfbus$name, regex = "\\S+")
dfbus$Name.Length[dfbus$Name.Length > 6] <- 6 # 6 means 6 or more words

# create new target variable where a closed restaurant has a value of 1
# in order to predict closure rather than staying in business
dfbus$Target <- ifelse(dfbus$open == 0, 1, 0)

# save as RDS
saveRDS(dfbus, file = 'dfbus.RDS')

# load RDS
dfbus <- readRDS('dfbus.RDS')

### 2. Review data

# select reviews for businesses
library(sqldf)
dfreviews <- sqldf('select a.* from df3 as a, dfbus as b where a.business_id = b.business_id')

# download opinion lexicon
#http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar

pos.words <- scan('opinion-lexicon-English/positive-words.txt',
                  what = 'character', comment.char = ';')
neg.words <- scan('opinion-lexicon-English/negative-words.txt',
                  what = 'character', comment.char = ';')

# sentiment function
# hat tip: http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment
score.sentiment <- function(sentences, pos.words, neg.words, .progress = 'none') {
    
    require(plyr)
    require(stringr)
    
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array of scores back, so we use
    # "l" + "a" + "ply" = "laply":
    scores <- laply(sentences, function(sentence, pos.words, neg.words) {
        
        # clean up sentences with R's regex-driven global substitute, gsub():
        sentence <- gsub('[[:punct:]]', '', sentence)
        sentence <- gsub('[[:cntrl:]]', '', sentence)
        sentence <- gsub('\\d+', '', sentence)
        # and convert to lower case:
        sentence <- tolower(sentence)
        
        # split into words. str_split is in the stringr package
        word.list <- str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words <- unlist(word.list)
        
        # compare our words to the dictionaries of positive & negative terms
        pos.matches <- match(words, pos.words)
        neg.matches <- match(words, neg.words)
        
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches <- !is.na(pos.matches)
        neg.matches <- !is.na(neg.matches)
        
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score <- sum(pos.matches) - sum(neg.matches)

        return(score)}, pos.words, neg.words, .progress = .progress )
    
    scores.df <- data.frame(score = scores)
    return(scores.df)
}

# Timer on
cat("\nStart processing:", format(Sys.time()),"\n")
ptm <- proc.time()

# generate sentiment scores
# creates a data frame
dfreviews$sentiment <- score.sentiment(dfreviews$text, pos.words, neg.words, 
                                       .progress='text')

# Timer off
et <- proc.time() - ptm
cat("\nTotal elapsed in minutes: ", et[3]/60)
cat("\nEnd processing:", format(Sys.time()),"\n")

# save score, remove sentiment data frame
dfreviews$score <- dfreviews$sentiment$score
dfreviews <- dfreviews[, -14]

# compute string length so we can normalize sentiment score
library(stringi)
dfreviews$Review.Length <- stri_length(dfreviews$text)

# flag reviews with the words 'manager' or 'management'
dfreviews$manager <- ifelse(grepl('manager|management', 
                                  dfreviews$text) == TRUE, 1, 0)
# flag one-star reviews
dfreviews$onestar <- ifelse(dfreviews$stars == 1, 1, 0)

# save as RDS
saveRDS(dfreviews, file = 'dfreviews.RDS')

# load RDS
dfreviews <- readRDS('dfreviews.RDS')

# aggregate review file by customer id
library(reshape2)
df <- dfreviews[, c(3, 7, 14, 15)] # keep stars, business_id, score, review length
dftidy <- melt(df, id = 'business_id')
dftidy <- dcast(dftidy, business_id ~ variable, mean)

# create normalized sentiment score
dftidy$Score.Norm <- dftidy$score / dftidy$Review.Length

# create sum of manager mentions and one-star reviews
df2 <- dfreviews[, c(7, 16:17)] # keep business_id, manager flag, one-star flags
dftidy2 <- melt(df2, id = 'business_id')
dftidy2 <- dcast(dftidy2, business_id ~ variable, sum)

# merge aggregated variables into business data file
df.final <- merge(dfbus, dftidy, by = 'business_id')
df.final <- merge(df.final, dftidy2, by = 'business_id')

names(df.final)[55] <- 'Avg.Stars'

# recode NA values to 0
df.final[is.na(df.final)] <- 0

# compute percent of reviews that mention 'manager' or 'management'
df.final$Pct.Manager <- df.final$manager / df.final$Review.Count

# compute percent of one star reviews
df.final$Pct.Onestar <- df.final$onestar / df.final$Review.Count

# recode target variable into a factor for model building
df.final$Target <- factor(df.final$Target, levels = c('1', '0'),
                          labels = c('Yes', 'No'))

# save as RDS
saveRDS(df.final, file = 'dffinal.RDS')

# load RDS
df.final <- readRDS('dffinal.RDS')