# Francia F. Riesco
# Case I Ok Cupid EDA


######################################################
# Libraries that we will use on this process
######################################################
library(dplyr)
library(xtable)
library(stringr)
library(dplyr)
library(ggplot2)
library(utils)
library(tidyverse)
library(lattice)
require(mosaic)
library(maps)
library(mapdata)

######################################################
# set the local environment
######################################################

setwd("~/workstation/fr-p/Github/data_mining_ok_cupid") # local environment
dir() # list what we have

######################################################
# Load the dataset
######################################################
# Get the okcupid data as `profiles` load other csv files
profiles <- read.csv("OkCupidDataset/profiles.csv")
latlon <- read.csv("OkCupidDataset/LatLon.csv")
addr <- read.csv("OkCupidDataset/addr.csv")
sharedCensus2010 <- read.csv("OkCupidDataset/sharedCensus2010Vars.csv")

######################################################
# review the data structures for the files
######################################################

colnames(profiles)
colnames(latlon)
colnames(addr)
colnames(sharedCensus2010)

######################################################
# Check the get summary of the dataset
######################################################

summary(profiles)
summary(latlon)
summary(addr)
summary(sharedCensus2010)

######################################################
# Pick only complete rows this remove almost all the data we dismiss it
######################################################
profiles_complete <- profiles[complete.cases(profiles), ]


######################################################
# checking for the missing values  for numeric
######################################################
favstats(~age, data = profiles) # 0 MISSING
favstats(~height, data = profiles) # 3 MISSIG
favstats(~income, data = profiles) # 48442 MISSING

######################################################
# renaming sex variables
######################################################

profiles$sex[profiles$sex == "f"] <- "female"
profiles$sex[profiles$sex == "m"] <- "male"

unique(profiles$sex)

######################################################
# basic EDA and plotting of individual vars
# this is to get familiar with the dataset
######################################################

table(profiles$age, profiles$sex)
table(profiles$income, profiles$sex)
table(profiles$height, profiles$sex)
table(profiles$sex, profiles$orientation)

table(profiles$orientation)
hist(profiles$age, col = "steelblue")

table(profiles$age, profiles$orientation)
table(profiles$sex, profiles$orientation)

######################################################
# subset of male and female to start analysing
######################################################

male <- subset(profiles, sex == "male") # male subset
female <- subset(profiles, sex == "female") # female subset

######################################################
#  boxplot for age male
######################################################
five.num <- fivenum(male$age)
five.num
outlier.ranges <- c(five.num[2] - 1.5 * (five.num[4] - five.num[2]), five.num[4] + 1.5 * (five.num[4] - five.num[2]))
outlier.ranges # print the outlier ranges

boxplot(male$age,
        horizontal = TRUE, xaxt = "n", col = "lightblue"
)
axis(side = 1, at = five.num, labels = TRUE)
text(five.num, rep(1.2, 5), srt = 90, adj = 0, labels = c("Min", "Lower Hinge", "Median", "Upper Hinge", "Max"))


######################################################
#  boxplot for age female
######################################################
five.num <- fivenum(female$age)
five.num
outlier.ranges <- c(five.num[2] - 1.5 * (five.num[4] - five.num[2]), five.num[4] + 1.5 * (five.num[4] - five.num[2]))
outlier.ranges # print the outlier ranges
boxplot(female$age,
        horizontal = TRUE, xaxt = "n", col = "pink"
)
axis(side = 1, at = five.num, labels = TRUE)
text(five.num, rep(1.2, 5), srt = 90, adj = 0, labels = c("Min", "Lower Hinge", "Median", "Upper Hinge", "Max"))



######################################################
#  Fixing outlier of male age with the five.num
# with the median
######################################################
profiles$age[profiles$age > 51 & profiles$sex == "male"] <- 30
profiles$age[profiles$age < 11 & profiles$sex == "male"] <- 30
######################################################
#  Fixing outlier of male age with the five.num
######################################################
profiles$age[profiles$age > 80 & profiles$sex == "female"] <- 54
profiles$age[profiles$age < 9 & profiles$sex == "female"] <- 54


######################################################
# Histogram of age by sex
######################################################
histogram(~ age | sex, width = 1, layout = c(1, 2), xlab = "age", data = profiles)


######################################################
# fresh male and female subset
######################################################

male <- subset(profiles, sex == "male") # male subset
female <- subset(profiles, sex == "female") # female subset

######################################################
#  boxplot for height male
######################################################

five.num <- fivenum(male$height, na.rm = TRUE)
five.num
outlier.ranges <- c(five.num[2] - 1.5 * (five.num[4] - five.num[2]), five.num[4] + 1.5 * (five.num[4] - five.num[2]))
outlier.ranges # print the outlier ranges
boxplot(male$height,
        horizontal = TRUE, xaxt = "n", col = "lightblue"
)
axis(side = 1, at = five.num, labels = TRUE)
text(five.num, rep(1.2, 5), srt = 90, adj = 0, labels = c("Min", "Lower Hinge", "Median", "Upper Hinge", "Max"))


######################################################
#  boxplot for height female
######################################################

five.num <- fivenum(female$height, na.rm = TRUE)
five.num
outlier.ranges <- c(five.num[2] - 1.5 * (five.num[4] - five.num[2]), five.num[4] + 1.5 * (five.num[4] - five.num[2]))
outlier.ranges # print the outlier ranges
boxplot(female$height,
        horizontal = TRUE, xaxt = "n", col = "pink"
)
axis(side = 1, at = five.num, labels = TRUE)
text(five.num, rep(1.2, 5), srt = 90, adj = 0, labels = c("Min", "Lower Hinge", "Median", "Upper Hinge", "Max"))



######################################################
# Histogram of height by sex
######################################################
histogram(~ height | sex, width = 1, layout = c(2, 1), xlab = "height", data = profiles)

######################################################
# Complete the missing heights with the mean
######################################################
profiles$height <- as.double(profiles$height)
profiles <- profiles %>%
        group_by(profiles$sex) %>%
        mutate(height = replace_na(height, mean(height, na.rm = TRUE)))

######################################################
#  Fixing outlier of male height with the five.num
######################################################
profiles$height[profiles$height > 78 & profiles$sex == "male"] <- 70
profiles$height[profiles$height < 62 & profiles$sex == "male"] <- 70

######################################################
#  Fixing outlier of female height with the five.num
######################################################
profiles$height[profiles$height > 73 & profiles$sex == "female"] <- 65
profiles$height[profiles$height < 55 & profiles$sex == "female"] <- 65


######################################################
#  boxplot for income male
######################################################

five.num <- fivenum(male$income, na.rm = TRUE)
five.num
outlier.ranges <- c(five.num[2] - 1.5 * (five.num[4] - five.num[2]), five.num[4] + 1.5 * (five.num[4] - five.num[2]))
outlier.ranges # print the outlier ranges
boxplot(male$income,
        horizontal = TRUE, xaxt = "n", col = "lightblue"
)
axis(side = 1, at = five.num, labels = TRUE)
text(five.num, rep(1.2, 5), srt = 90, adj = 0, labels = c("Min", "Lower Hinge", "Median", "Upper Hinge", "Max"))

######################################################
#  boxplot for income female
######################################################
five.num <- fivenum(female$income, na.rm = TRUE)
five.num
outlier.ranges <- c(five.num[2] - 1.5 * (five.num[4] - five.num[2]), five.num[4] + 1.5 * (five.num[4] - five.num[2]))
outlier.ranges # print the outlier ranges
boxplot(female$income,
        horizontal = TRUE, xaxt = "n", col = "pink"
)
axis(side = 1, at = five.num, labels = TRUE)
text(five.num, rep(1.2, 5), srt = 90, adj = 0, labels = c("Min", "Lower Hinge", "Median", "Upper Hinge", "Max"))

######################################################
# Histogram of income by sex
######################################################
histogram(~ income | sex, width = 1, layout = c(2, 1), xlab = "income", data = profiles)

######################################################
#  Fixing outlier of male height with the five.num
######################################################
profiles$income[profiles$income > 205000 & profiles$sex == "male"] <- 60000
profiles$income[profiles$income < 1000 & profiles$sex == "male"] <- 60000

######################################################
#  Fixing outlier of female income with the five.num
######################################################
profiles$income[profiles$income > 150000 & profiles$sex == "female"] <- 40000
profiles$income[profiles$income < 1000 & profiles$sex == "female"] <- 40000

######################################################
# fix mixing income with me median and doing separated by sex
######################################################
profiles$income <- as.double(profiles$income)
profiles <- profiles %>%
        group_by(profiles$sex) %>%
        mutate(income = replace_na(income, median(income, na.rm = TRUE)))


######################################################
# fresh male and female subset
######################################################

male <- subset(profiles, sex == "male") # male subset
female <- subset(profiles, sex == "female") # female subset


######################################################
# Histogram of income by sex
######################################################
histogram(~ income | sex, width = 1, layout = c(2, 1), xlab = "income", data = profiles)



######################################################
# Three numeric variables  income is the one
# has more missing variables
######################################################

sum(is.na(profiles$income))
table(is.na(profiles$income))
hist(profiles$income)


table(is.na(profiles$age))
table(is.na(profiles$ethnicity))
table(is.na(profiles$height))

######################################################
# checking common responses
######################################################

unique(profiles$ethnicity)
unique(profiles$sex)
unique(profiles$sign)


######################################################
# # completing data with unknown values
######################################################

profiles$body_type[is.na(profiles$body_type)] <- "unknown"
profiles$diet[is.na(profiles$diet)] <- "unknown"
profiles$drinks[is.na(profiles$drinks)] <- "unknown"
profiles$drugs[is.na(profiles$drugs)] <- "unknown"
profiles$education[is.na(profiles$education)] <- "unknown"
profiles$ethnicity[is.na(profiles$ethnicity)] <- "unknown"
profiles$job[is.na(profiles$job)] <- "unknown"
profiles$offspring[is.na(profiles$offspring)] <- "unknown"
profiles$orientation[is.na(profiles$orientation)] <- "unknown"
profiles$pets[is.na(profiles$pets)] <- "unknown"
profiles$religion[is.na(profiles$religion)] <- "unknown"
profiles$sign[is.na(profiles$sign)] <- "unknown"
profiles$smokes[is.na(profiles$smokes)] <- "unknown"



######################################################
# checking the tables again
######################################################

table(is.na(profiles$diet))
table(is.na(profiles$body_type))
table(is.na(profiles$drinks))


######################################################
# creating a new religion field
######################################################

profiles <- profiles %>% mutate(religious_affil = gsub(" [A-z ]*", "", religion))
table(profiles$religious_affil)

######################################################
# create a new field about zodiac
######################################################
profiles <- profiles %>% mutate(zodiac = gsub(" [A-z]*", "", sign))
profiles$zodiac <- gsub("'t", "", as.character(profiles$zodiac))
profiles$zodiac <- gsub("'s", "", as.character(profiles$zodiac))
table(profiles$zodiac)



profiles <- profiles %>% mutate(language = gsub(" [A-z ],*", "", speaks))
profiles <- profiles %>% mutate(language = gsub(" [A-z ]*", "", language))
unique(profiles$language)
##### Enrich with one of the new data sets, you may want to do this with the other csv
moreData <- left_join(profiles, latlon, by = "location")
head(moreData)
######################################################
#### You can use complete.cases() to identify records without NA if that is the route you want to explore.
## Of course you can use a function covered in class to visualize the variables with the hightest % of NA
## so you could drop those instead of all rows with an NA.
######################################################


#### merged all data  in one final profile dataset
# this data set was dismissed
######################################################
# Merged the location data set that help us for the map plotting
######################################################
moreData2 <- left_join(moreData, addr, by = "location")
head(moreData2)
profiles_final <- left_join(moreData2, sharedCensus2010, by = "location")
head(profiles_final)

completeMoreData <- profiles_final[complete.cases(profiles_final), ]

######################################################
# Data Exploration samples
######################################################

# this plot help me to build a story to tell

ggplot(completeMoreData, aes(x = income, y = age, fill = sex)) +
        geom_boxplot()

five.num <- fivenum(completeMoreDatal$age)
five.num
outlier.ranges <- c(five.num[2] - 1.5 * (five.num[4] - five.num[2]), five.num[4] + 1.5 * (five.num[4] - five.num[2]))
outlier.ranges # print the outlier ranges
boxplot(completeMoreData$age,
        horizontal = TRUE, xaxt = "n", col = "blue"
)
axis(side = 1, at = five.num, labels = TRUE)
text(five.num, rep(1.2, 5), srt = 90, adj = 0, labels = c("Min", "Lower Hinge", "Median", "Upper Hinge", "Max"))




five.num <- fivenum(completeMoreData$income)
five.num
outlier.ranges <- c(five.num[2] - 1.5 * (five.num[4] - five.num[2]), five.num[4] + 1.5 * (five.num[4] - five.num[2]))
outlier.ranges # print the outlier ranges
boxplot(completeMoreData$income,
        horizontal = TRUE, xaxt = "n", col = "blue"
)
axis(side = 1, at = five.num, labels = TRUE)
text(five.num, rep(1.2, 5), srt = 90, adj = 0, labels = c("Min", "Lower Hinge", "Median", "Upper Hinge", "Max"))


boxplot(completeMoreData$age ~ completeMoreData$sex, outline = FALSE)
boxplot(completeMoreData$income ~ completeMoreData$sex, outline = FALSE)
boxplot(completeMoreData$height ~ completeMoreData$sex, outline = FALSE)