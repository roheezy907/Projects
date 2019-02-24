# Assignment 3

# Setting up -------------------
craigslist = readRDS("cl_apartments.rds")
library(ggplot2)
library(ggrepel)
library(ggridges)
library(gridExtra)



# 1 and 5: Raw Data Features/analysis ----------------

# Update the data
craigslist$price[craigslist$price == 34083742] = 3408
craigslist$price[craigslist$price == 9951095 ] = 995

dim(craigslist_raw) 
craigslist_raw = readRDS("cl_apartments.rds")
craigslist_raw$price[craigslist$price == 34083742] = 3408
craigslist_raw$price[craigslist$price == 9951095 ] = 995


#  Missing values
# Features with no missing values
craigslist_isna_raw = sapply(craigslist_raw, is.na)
total_isna_raw = apply(craigslist_isna_raw,2,sum)
no_na_raw = craigslist_raw[total_isna_raw == 0]
names(no_na_raw)

# Looking at the features that do have missing features
missing_features_raw = craigslist_raw[total_isna_raw == nrow(craigslist_raw)]
names(missing_features_raw)
sort(total_isna_raw)

# Exploring patterns 
no_na_bathrooms = subset(craigslist_raw, !is.na(bedrooms) & !is.na(bathrooms))
nrow(craigslist_raw) - nrow(no_na_bathrooms) # Confirms theory that for all the posts that had na for bedrooms also had na for bathrooms

no_na_lat = subset(craigslist_raw, !is.na(latitude) & !is.na(longitude))
nrow(craigslist_raw) - nrow(no_na_lat) # Confirms theory that for all posts that had na for longitude also had na for latitude

no_na_state = subset(craigslist_raw, !is.na(state) & !is.na(county)) 
nrow(craigslist_raw) - nrow(no_na_state) # Confirms theory that for all posts that had na for state had it for county 



# Outliers (Numerical Data)

# Bathrooms
table(craigslist_raw$bathrooms) # Does not make sense for it to have 0 bathrooms

# Bedrooms
table(craigslist_raw$bedrooms)  # There are 6 and 7 bedrooms, looking at those posts it was seen they were houses 
alot_beds = subset(craigslist_raw, bedrooms == 6 |bedrooms == 7 ) 
alot_beds$title # Looking at the 6/7 bedrooms they are either services or houses, but even for 4/5 bedrooms i noticed some posts are houses not apartments
alot_beds$text

# Sq.ft
table(craigslist_raw$sqft) # Anything under 80 square feet is false/wrong data  as one cannot live under 80 sqft
sort(craigslist_raw$sqft, decreasing = TRUE) # 200000 Seems to lare and is an outlier
craigslist_raw[craigslist_raw$sqft > 9000,] # Looking at the post it is an obvious error

# Price 
table(craigslist$price) # Many prices that do not make sense for apartments 0-150


low.price = subset(craigslist, price < 200)
low.price$title
# Looking at posts with price less than 200, I noticed that all of them are either posts for services(such as moving trucks)
# Or have a rate of paying weekly and not monthly, Or have a short term rent.



# Errors in the data set (Categorical values)

# States
table(craigslist$state) # Not all states are in California

# Titles: Some texts are not apartments

grep("house", craigslist_raw$title, ignore.case = TRUE, value = TRUE ) # Alot say townhouse/open house, but some values are actual houses


# Subsetting data to get most accurate data ---------------

# Final Subsetting
craigslist = subset(craigslist, price >= 200) # Accounts for price outliers/ textss that are not apartment related
craigslist = subset(craigslist, state == "CA") # Gives us data thats only in CA
craigslist = subset(craigslist, sqft >= 80 & sqft < 199999) # Gets rid of outlier sqft.
craigslist = subset(craigslist, bathrooms > 0) # Gets rid of apartments that did not mention bathroom
craigslist = subset(craigslist, bedrooms < 6)
# Removes duplicate texts and titles
craigslist = craigslist[!duplicated(craigslist$title),]
craigslist = craigslist[!duplicated(craigslist$text),]

# Overview of the updated dataset -----------

# Rows and Columns
dim(craigslist) # 12,664 observations and 20 variables 
names(craigslist)

# Missing values
craigslist_isna = sapply(craigslist, is.na)
total_isna = apply(craigslist_isna,2,sum)

# Span of dates
head(sort(craigslist$date_posted)) # From September 9th
tail(sort(craigslist$date_posted)) # To October 15th

head(sort(craigslist$date_updated)) # From October 8th
tail(sort(craigslist$date_updated)) # To October 15th

# Span of Longitude/latitude
max(craigslist$latitude)
min(craigslist$latitude)

max(craigslist$longitude)
min(craigslist$longitude)




--------------------------
#---------------------
# 2A --------------------------


# Major cities: defined by the cities with the most apartments
major_names = tail(sort(table(craigslist$city)), 5) # Major cities: Sac, LA, SJ, SD, SF

# REFRENCE TO PATRICK ON SHOWING THIS METHOD OF SUBSET

# Subset data with major cities
major_cities = craigslist[craigslist$city %in% names(major_names),]

# Subset data with Suburban cities
suburban_cities = craigslist[!(craigslist$city %in% names(major_names)),]

# Comparison between Suburban and Major cities, which is more family-friendly?

# Room comparison: 

major_bed = ggplot(major_cities, aes(y = bedrooms)) + geom_boxplot() + labs(title = "Boxplot of bedrooms in major cities") 
                                                                                                                        
sub_bed = ggplot(suburban_cities, aes(y = bedrooms)) + geom_boxplot() + labs(title = "Boxplot of bedrooms in suburban cities")

grid.arrange(major_bed, sub_bed, ncol = 2)

mean(major_cities$bedrooms, na.rm = TRUE)
mean(suburban_cities$bedrooms, na.rm = TRUE)
# Overall not a big difference among the two

# Pets Comparison:

pets_major = ggplot(major_cities, aes(x = pets, y = stat(prop), group = 1)) + geom_bar() + labs(title = "Proportion of pet tolerance major cities", y = "proportion") 
pets_sub = ggplot(suburban_cities, aes(x = pets, y = stat(prop), group = 1)) + geom_bar() + labs(title = "Proportion of pet tolerance suburban cities", y = "proportion") 

grid.arrange(pets_major, pets_sub, ncol = 2)

# Parking Comparison:
parking_major = ggplot(major_cities, aes(x = parking, y = stat(prop), group = 1)) + geom_bar() + labs(title = "Proportion of parking options major cities", y = "proportion") 
parking_sub = ggplot(suburban_cities, aes(x = parking, y = stat(prop), group = 1)) + geom_bar() + labs(title = "Proportion of parking suburban cities", y = "proportion") 

grid.arrange(parking_major, parking_sub, ncol = 2)

# 2B: Which affects price more bedrooms or bathrooms

# To do the best analysis we must look at the price and only use data with no outliers
summary(craigslist$price)

ggplot(craigslist, aes(y = price)) + geom_boxplot() + ylim(0, 6000) + 
  labs(title = "Box plot of the prices of the craiglist posts")

# Calculating the outlier
outlier = 2915 + (1.5 * (2915 - 1720))
price_fixed = subset(craigslist, price > 4707.5)
sort(price_fixed$price, decreasing = TRUE)

# Now that we have the prices with no outliers in the data set we can now examine our problem
#
# Bedrooms:
ggplot(price_fixed, aes(x = bedrooms, y = mean(price))) + geom_bar(stat = "identity")






# 2B ----------------------

# Major city bathrooms vs. bedroom effect on price

# Subset bathrooms and bedroom so they have the same domains
major_city_bathroom = subset(major_cities, bathrooms == 1 | bathrooms == 2 | bathrooms == 3 | bathrooms == 4)
major_city_bedroom = subset(major_cities,  bedrooms == 1 | bedrooms == 2 | bedrooms == 3 | bedrooms == 4 )

# Plot bedrooms and bathrooms vs. price 
p = ggplot(major_city_bathroom, aes(x = bathrooms, y = price)) + stat_summary(fun.y = median, geom = "bar") + ylab("median price") 
q = ggplot(major_city_bedroom, aes(x = bedrooms, y = price)) + stat_summary(fun.y = median, geom = "bar") + ylab("median price") + ylim(0,8000) 

grid.arrange(p,q,ncol = 2, top = "Prices of bathrooms and bedrooms for major cities")


# Suburban city bathrooms vs. bedroom effect on price

# Subset bathrooms and bedroom so they have the same domains
sub_city_bathroom = subset(suburban_cities, bathrooms == 1 | bathrooms == 2 | bathrooms == 3 | bathrooms == 4)
sub_city_bedroom = subset(suburban_cities,  bedrooms == 1 | bedrooms == 2 | bedrooms == 3 | bedrooms == 4 )

# Plot bedrooms and bathrooms vs. price 
pp = ggplot(sub_city_bathroom, aes(x = bathrooms, y = price)) + stat_summary(fun.y = median, geom = "bar") + ylab("median price") 
qq = ggplot(sub_city_bedroom, aes(x = bedrooms, y = price)) + stat_summary(fun.y = median, geom = "bar")  + ylab("median price") + ylim(0,6000) 

grid.arrange(pp,qq,ncol = 2, top = "Prices of bathrooms and bedrooms for suburban cities")



# 2C ----------------------------

# Chose the sacramento area
my_cities = subset(craigslist, city == "Roseville" | city == "Sacramento" | city == "Elk Grove")


overall_price = ggplot(my_cities, aes(x = city, y = price)) + stat_summary(fun.y = median, geom = "bar")  + labs(title = "Overall price") 
bed_vs_price = ggplot(my_cities, aes(x = bedrooms, y = price)) + stat_summary(fun.y = median, geom = "bar") + facet_grid(~ city) + labs(title = "Bedrooms vs. price")
bath_vs_price = ggplot(my_cities, aes(x = bathrooms, y = price)) + stat_summary(fun.y = median, geom = "bar") + facet_grid(~ city) + labs(title = "Bathrooms vs. price")
sqft_apt = ggplot(my_cities, aes(x = city, y = sqft)) + stat_summary(fun.y = mean, geom = "bar")  + labs(title = "Avaerage Sqft for apartments") 

# Would be too much if i included this too. 
pet_comparison = ggplot(my_cities, aes(x = pets, y = stat(prop), group = 1)) + geom_bar() + facet_grid(~ city) + labs(title = "Pets")
parking_comparison = ggplot(my_cities, aes(x = parking, y = stat(prop), group = 1)) + geom_bar() + facet_grid(~ city) + labs(title = "Parking")
laundry_comparison = ggplot(my_cities, aes(x = parking, y = stat(prop), group = 1)) + geom_bar() + facet_grid(~ city) + labs(title = "Laundry")









































------------------------

# --------------------
#4A -----------------------

# For people, looking for apartments, which day is the best to check craigslist listings?
# Hypothesis: I would think it would be monday because its the start of the week?

# Get Plot of frequencies of posted and updated by days of the week

# REFERENCE PIAZZA POST 165 ON WEEKDDAYS()
updated = ggplot(craigslist, aes(x = weekdays(date_updated))) + geom_bar() + ylab("Count for date updated") + xlab("Days of the week")
posted = ggplot(craigslist, aes(x = weekdays(date_posted))) + geom_bar() + ylab("Count for date posted") + xlab("Days of the week") 

grid.arrange(posted, updated, ncol = 2, top = "Frequency of craigslist posts posted and updated by days of the week")

#4B ----------------------

# Do qualitative features such as pets, laundry, parking have an affect on price?

# Get median price for each category of each variable
pets_price = aggregate(price ~ pets, craigslist, median)
laundry_price = aggregate(price ~ laundry, craigslist, median)
parking_price = aggregate(price ~ parking, craigslist, median)

p1 = ggplot(pets_price, aes(x = pets, y = price)) + geom_bar(stat = "identity") + xlab("Pet policy") + ylab("Median price")
p2 = ggplot(laundry_price, aes(x = laundry, y = price)) + geom_bar(stat = "identity") + xlab("Laundry options") + ylab("Median price")
p3 = ggplot(parking_price, aes(x = parking, y = price)) + geom_bar(stat = "identity") + xlab("Parking options") + ylab("Median price")
grid.arrange(p1, p2, p3, ncol = 3, top = "Effects of pets, laundry, and parking on price")


#4C ---------------------------

# Question: Is it more expensive to rent an apartment in Nor-cal or Southern California
# Define Nor cal as everything above Fresno and so cal everything below it, Fresno is that middle place
# Google search showed fresno has a 36.7 degree la

# Subset cities by Northern California and Southern California
Nor_cal = subset(craigslist, latitude > 36.7)
So_cal = subset(craigslist, latitude < 36.7)

Nor_cal_prices = ggplot(Nor_cal, aes(y = price)) + geom_boxplot() + ylab("Nor-Cal prices") + ylim(0,15000)
So_cal_prices = ggplot(So_cal, aes(y = price)) + geom_boxplot() + ylab("so-Cal prices") + ylim(0,15000)

grid.arrange(Nor_cal_prices, So_cal_prices, ncol = 2, top = "Prices in Nor-cal vs. So-cal")



#4D ---------------------------

# Question: For UC Davis students who are able to commute, which city has the most affordable  apartments?
# Easy/Neigboring places for commuting: Sacramento,Davis, Woodland, West Sacramento and vacaville
# Looked at one bedroom apartments as most students have a roomates

# Subset cities close to davis, plot median price

neighbor_cities = subset(craigslist, city == "Sacramento" | city == "West Sacramento" | city == "Davis" | city == "Woodland" | city == "Vacaville")
one_bed = subset(neigbor_cities, bedrooms == 1)
one_bed_gg = ggplot(one_bed, aes(x = city, y = price)) + stat_summary(fun.y = median, geom = "bar")  + labs(title = " Prices for one bedroom apartments") + ylab("Median price") + 
  labs(label = "2 bedroom apartments for each city vs. price")


#4E-------------
# Question: For students renting apartments in davis, does the amount of rooms in an apartment affect the price they will pay for their room only?
# Will look at 1-3 rooms as there is no information for davis apartments with 4 bedrooms 

# Subset data by bedroom 
davis_postings_1 = subset(craigslist, city == "Davis" & bedrooms == 1)
davis_postings_2 = subset(craigslist, city == "Davis" & bedrooms == 2)
davis_postings_3 = subset(craigslist, city == "Davis" & bedrooms == 3)


d1 = ggplot(davis_postings_1, aes(y = price)) + geom_boxplot() + ylab("Prices per room for one bedroom apartments")
d2 = ggplot(davis_postings_1, aes(y = price/2)) + geom_boxplot() + ylim(0, 1500) + ylab("Prices per room for two bedroom apartments")
d3 = ggplot(davis_postings_1, aes(y = price/3)) + geom_boxplot() + ylim(0, 1500) + ylab("Prices per room for three bedroom apartments")



grid.arrange(d1,d2,d3,ncol = 3, top = "Price per room for apartments with 1,2,3 rooms")


# Used piazza post on grep for some guidance
# Worked with Jonathan Fernandez, Kelly Chan, Jiemin Huang, Fenglan Jiang 