# Francia F. Riesco 
# Case I Ok Cupid EDA

######################################################
# Data Exploration
# EDA K CUPID DATA
######################################################

######################################################
# pie review sex male vs female
######################################################
table.sex<- table(completeMoreData$sex)
slice.labels <- names(table.sex)
slice.percents <- round(table.sex/sum(table.sex)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")

pie(table.sex, main="Male vs Female", labels = slice.labels, 
    col=c("pink","lightblue"))

######################################################
# mosaic orientation
######################################################
mosaic.table <- table(completeMoreData$sex, completeMoreData$orientation)
mosaic.table
mosaicplot(mosaic.table, main = "Sex vs Orientation",  
           border = "chocolate",color=c('pink', 'lightblue', 'lightgreen')
)

######################################################
#barplot orientation
######################################################
multi.table<-  table(completeMoreData$orientation,completeMoreData$sex)
multi.table
barplot(multi.table,  main="sex vs orintation", legend.text = TRUE,
        ylab="Orientation", xlab="sex", col=c("lightgreen","pink","lightblue"))



######################################################
# histograms
######################################################
histogram(~height | sex, width=1, layout=c(2,1), xlab="Height in inches", data=completeMoreData)
histogram(~age | sex, width=1, layout=c(2,1), xlab="Age in years", data=completeMoreData)
histogram(~income | sex, width=1, layout=c(2,1), xlab="Age in years", data=completeMoreData)
# create male female subsets


######################################################
# subset male and female from final data
######################################################
male <-subset(completeMoreData, sex == "male") # male subset
female <-subset(completeMoreData, sex == "female") # female subset


######################################################
# zodiac vs sex male and female 
######################################################
table.zodiac<- table(subset(profiles, zodiac != "unknown" & sex =='female')$zodiac)
slice.labels <- names(table.zodiac)
slice.percents <- round(table.zodiac/sum(table.zodiac)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")

pie(table.zodiac, main="Female Zodiac", labels = slice.labels)


table.zodiac<- table(subset(completeMoreData, zodiac != "unknown" & sex =='male')$zodiac)
slice.labels <- names(table.zodiac)
slice.percents <- round(table.zodiac/sum(table.zodiac)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")

pie(table.zodiac, main="Male Zodiac", labels = slice.labels)


######################################################
# female Histogram for Income
######################################################
summary(female)
fivenum(female$income)
five.num <- fivenum(female$income)
five.num
outlier.ranges <-c(five.num[2]-1.5*(five.num[4]-five.num[2]),five.num[4]+1.5*(five.num[4]-five.num[2]))
outlier.ranges # print the outlier ranges
ggplot(data=female, aes(female$income)) + 
  geom_histogram(aes(y =..density..), 
                 col="red", 
                 fill="lightgreen", 
                 alpha=.2) + 
  geom_density(col=2) + 
  
  scale_x_continuous(limits = c(0.0, 100000)) +
  labs(title="Histogram female income", x="income", y="Count")


######################################################
# female Histogram for Income
######################################################
summary(male)
fivenum(male$income)
five.num <- fivenum(male$income)
five.num
outlier.ranges <-c(five.num[2]-1.5*(five.num[4]-five.num[2]),five.num[4]+1.5*(five.num[4]-five.num[2]))
outlier.ranges # print the outlier ranges

ggplot(data=male, aes(male$income)) + 
  geom_histogram(aes(y =..density..), 
                 col="red", 
                 fill="lightgreen", 
                 alpha=.2) + 
  geom_density(col=2) + 
  
  scale_x_continuous(limits = c(0.0, 250000)) +
  labs(title="Histogram male income", x="income", y="Count")

table.body <- table(female$body_type)
table.body

######################################################
# ##calculate income density male vs female
######################################################

male_income <- data.frame(income = male$income) #create dataframe only with income variable
female_income <- data.frame( income= female$income)

male_income$variable <- 'male' # variable name
female_income$variable <- 'female'
income.info <- rbind(male_income, female_income)

ggplot(income.info, aes(income, fill = variable)) +
  scale_x_continuous(limits = c(0.0, 100000)) +
  geom_density(alpha = 0.2)

ggplot(income.info, aes(income, fill = variable)) +
  scale_x_continuous(limits = c(0.0, 100000)) +
  geom_density(alpha = 0.2) +
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')


######################################################
# ##calculate age density male vs female
######################################################

male_age<- data.frame(age = male$age) #create dataframe only with age variable
female_age <- data.frame(age= female$age)

male_age$variable <- 'male' # variable name
female_age$variable <- 'female'
age.info <- rbind(male_age, female_age)

ggplot(age.info, aes(age, fill = variable)) +
  scale_x_continuous(limits = c(0.0, 100)) +
  geom_density(alpha = 0.2)

ggplot(age.info, aes(age, fill = variable)) +
  scale_x_continuous(limits = c(0.0, 100)) +
  geom_density(alpha = 0.2) +
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')






######################################################
# plot categorical variables  drugs vs orientation
######################################################
ggplot(subset(completeMoreData, drugs != "unknown")) +
  geom_bar(aes(x=drugs, fill=sex),
           position = "dodge") +
  facet_wrap(~orientation)

######################################################
# plot categorical variables  religious_affil vs orientation
######################################################
ggplot(subset(completeMoreData, religious_affil != "unknown")) +
  geom_bar(aes(x=orientation, fill=sex),
           position = "dodge") +
  facet_wrap(~religious_affil)
######################################################
# plot categorical variables  drugs vs marital status 
######################################################

ggplot(subset(completeMoreData, drugs != "unknown" & status != "unknown")) +
  geom_bar(aes(x=drugs, fill=sex),
           position = "dodge") +
  facet_wrap(~status)

######################################################
# plot categorical variables  drinks vs marital status 
######################################################

ggplot(subset(completeMoreData, drinks != "unknown" & status != "unknown")) +
  geom_bar(aes(x=drinks, fill=sex),
           position = "dodge") +
  facet_wrap(~status)

######################################################
# plot categorical variables  smokes vs marital status 
######################################################

ggplot(completeMoreData) +
  geom_bar(aes(x=orientation, fill=sex),
           position = "dodge") +
  facet_wrap(~smokes)

table(completeMoreData$sex, completeMoreData$state.x)

######################################################
# # MAP location 
######################################################


usa <- map_data('state')
World_map <- map_data("world")

######################################################
# fitting the US map 
######################################################
female_usa <- female %>% 
  filter(!lon<= min(usa$long) & !lon >= max(usa$long), !is.na(lon))

male_usa <- male %>% 
  filter(!lon<= min(usa$long) & !lon >= max(usa$long), !is.na(lon))

# Visualising our data
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, colour = "black") + 
  coord_fixed(1.3) +
  geom_point(data = female_usa, aes(x=lon, y = lat, colour = "pink"), alpha=.8) +
  geom_point(data = male_usa, aes(x=lon, y = lat, colour = "blue"), alpha=.8) +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "The Locations of OkCupid User in the United States", x = "Longitude", y = "Latitude")

table(completeMoreData$state.x,completeMoreData$sex )


######################################################
# fitting the west coast
######################################################
west_coast <- subset(usa, region %in% c("california", "oregon", "washington"))
female_west <- female %>% 
  filter(state.x =='California' | state.x =='Oregon' | state.x =='washington')
male_west<- male %>% 
  filter(state.x =='California' | state.x =='Oregon' | state.x =='washington')

ggplot() + geom_polygon(data = west_coast, aes(x=long, y = lat, group = group), fill = NA, colour = "black") + 
  coord_fixed(1.3) +
  geom_point(data = female_west, aes(x=lon, y = lat, colour = "pink"), alpha=.8) +
  geom_point(data = male_west, aes(x=lon, y = lat, colour = "blue"), alpha=.8) +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "The Locations of OkCupid Users in the West Coast", x = "Longitude", y = "Latitude")


######################################################
# fitting the east coast
######################################################
east_coast <- subset(usa, region %in% c("massachusetts", "new york", "new jersey", "connecticut"))
female_east<- female %>% 
  filter(state.x =='Massachusetts' | state.x =='New York' | state.x =='New Jersey' | state.x =='Connecticut')
male_east<- male %>% 
  filter(state.x =='Massachusetts' | state.x =='New York' | state.x =='New Jersey' | state.x =='Connecticut')

ggplot() + geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill = NA, colour = "black") + 
  coord_fixed(1.3) +
  geom_point(data = female_east, aes(x=lon, y = lat, colour = "pink"), alpha=.8) +
  geom_point(data = male_east, aes(x=lon, y = lat, colour = "blue"), alpha=.8) +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "The Locations of OkCupid Users in the East Coast", x = "Longitude", y = "Latitude")


