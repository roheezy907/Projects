# Assignment 4
# Loading up the libraries/color palettes ----------------
library(ggplot2)
library(stringr)
install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
citation("ggmap")
library(gridExtra)
cbPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Craigslist data--------------------
craigslist = readRDS("cl_apartments.rds") # Craigslist data
# Clean the data (same code from previous assignment)
craigslist$price[craigslist$price == 34083742] = 3408
craigslist$price[craigslist$price == 9951095 ] = 995
craigslist = subset(craigslist, price >= 200) # Accounts for price outliers/ textss that are not apartment related
craigslist = subset(craigslist, state == "CA") # Gives us data thats only in CA
craigslist = subset(craigslist, sqft >= 80 & sqft < 199999) # Gets rid of outlier sqft.
craigslist = subset(craigslist, bathrooms > 0) # Gets rid of apartments that did not mention bathroom
craigslist = subset(craigslist, bedrooms < 6)
# Removes duplicate texts and titles
craigslist = craigslist[!duplicated(craigslist$title),]
craigslist = craigslist[!duplicated(craigslist$text),]

# Census data -----------------------
census = read.csv("DEC_10_SF1_SF1DP1_with_ann.csv", stringsAsFactors = FALSE)
head(census)
names(census)

# 1: Setting up map/data ------------------

# Get map
map = get_stamenmap()
bbox = c(-121.800081,38.524282,-121.693036,38.57378)
map = get_stamenmap(bbox, zoom = 15)

# Get data
davis = subset(craigslist, city == "Davis" | place == "Davis")

# 1A: Is there a relation between apartments in certain areas of Davis and their bedrooms?---------------

davis$bedroom = cut(davis$bedrooms, breaks = c(0,1.5,2,3), labels = c("1 bedroom", "2 bedroom", "3 bedroom")) # Factor the bedrooms

ggmap(map) + geom_point(aes(longitude, latitude, color = bedroom),size = 3, davis) +  
  labs(title = "Stamen map of Davis apartments sorted by the number of bedrooms", y= "Latitude", x = "Longitude")  + 
  scale_color_manual(name = "Number of bedrooms", values = cbPalette) 

# 1B: Is there a relationship among areas in Davis and price? ---------------------------------

summary(davis$price) # Get bounds of price
davis$pricebound = cut(davis$price, breaks = c(783,1408,1840,Inf), labels = c("Below Average", "Average", "Above Average"))

ggmap(map) + geom_point(aes(longitude, latitude, color = pricebound),size = 5, davis) +
  labs(title = "Stamen map of Davis apartments sorted by price", y = "Latitude", x = "Longitude")  + 
  scale_color_manual(name = "Price range", values = cbPalette)



# 1C: Is there a relationship among apartments in certain areas of Davis and the laundry options they offer?-------------
no_na = subset(davis, !is.na(laundry))
ggmap(map) + geom_point(aes(longitude, latitude, shape = laundry, color = laundry), size = 5 , no_na) +
  labs(title = "Stamen map of Davis apartments sorted by laundry options", y = "Latitude", x = "Longitude") + 
  scale_fill_manual(label = "Laundry options", values = cbPalette)


# 2: Setting up map/data----------------------------------------------

# Get Map
map2 = get_stamenmap()
sf_bound = c(-122.7,37.1137,-121.5685,38.0521)
map2 = get_stamenmap(sf_bound, zoom = 10, maptype = "toner-lite")

# Subset data
sf_data = subset(craigslist, county == "San Francisco" | county == "San Mateo" | county == "Santa Clara" | county == "Alameda" | county == "Contra Costa")

# 2A: Is there a relation between apartments in certain areas of Southern San Francisco Bay Area and their bedrooms? --------------------

sf_bed_data = subset(sf_data, bedrooms != 5)

ggmap(map2) + geom_density2d(aes(longitude, latitude, colour = county),size = 1, bins = 3, sf_bed_data) +
  labs(title = "Stamen map of Southern San Francisco Bay Area sorted by the number of bedrooms", y= "Latitude", x = "Longitude") + 
  scale_color_manual(name = "Counties", values = cbPalette) + facet_wrap(~ laundry)
# 2B: Is there a relationship among areas in Southern SF Bay and price? ----------------------

summary(sf_data$price)
sf_data$pricebound = cut(sf_data$price, breaks = c(199,2299,3494,Inf), labels = c("Below Average", "Average", "Above Average"))

ggmap(map2) + geom_density2d(aes(longitude, latitude, colour = county, alpha = 0.5),size = 1, bins = 6, sf_data) + 
  labs(title = "Stamen map of Southern SF Bay apartments sorted by price", y = "Latitude", x = "Longitude")  + 
  scale_color_manual(name = "Counties", values = cbPalette) +
  facet_wrap(~pricebound)

# 2C: Is there a relationship among apartments in certain areas of Southern SF Bay and the laundry options they offer?--------------------
no_na_sf = subset(sf_data, !is.na(laundry))

ggmap(map2) + geom_density2d(aes(longitude, latitude, colour = county),size = 1, bins = 3, no_na_sf) +
  labs(title = "Stamen map of Southern San Francisco Bay Area sorted by laundry options", y= "Latitude", x = "Longitude") + 
  scale_color_manual(name = "Counties", values = cbPalette) + facet_wrap(~ laundry)

# 3: Elderly Population in Southern SF Bay --------------------

# look at census data 
head(census)
view(census)
dim(census)
census$GEO.display.label 

# In order to merge the places to match plaxes in craigslist data we must remove all observations with the city ,CDP, and California 
census$GEO.display.label = str_remove(census$GEO.display.label," CDP, California")
census$GEO.display.label = str_remove(census$GEO.display.label," city, California")
census$GEO.display.label = str_remove(census$GEO.display.label,", California")
census$GEO.display.label = str_remove(census$GEO.display.label,"CDP ")

census$GEO.display.label

# Move data one row up so we have the full colnames
names(census) = lapply(census[1, ], as.character)
census = census[-1, ]

# Merging the datasets 
merged_data = merge(craigslist, census, by.x = "city", by.y = "Geography")
dim(merged_data)
merged_isna_raw = sapply(merged_data, is.na)
total_isna_raw = apply(merged_isna_raw,2,sum)
sort(total_isna_raw)


# Question: Which places in the southern San Francisco Bay Area have the oldest populations? How does this relate to the rental market, if at all?

# Defining Southern San Francisco Bay Area
sfb = subset(merged_data, county == "San Francisco" | county == "San Mateo" | county == "Santa Clara" | county == "Alameda" | county == "Contra Costa")
nrow(sfb)

# Cutting the percentages of 65+ into categories
min(as.numeric(sfb$`Percent; SEX AND AGE - Total population - 65 years and over`)) # Minimum percentage is 7.3
max(as.numeric(sfb$`Percent; SEX AND AGE - Total population - 65 years and over`)) # Maximum percentage is 26.6
sfb$percent_factors = cut(as.numeric(sfb$`Percent; SEX AND AGE - Total population - 65 years and over`), breaks = c(0,5, 10, 15, 20, 25, 30))
                    
# Cut price up 
summary(sfb$price)
sfb$price_breaks = cut(sfb$price, breaks = c(199,2349,3500,17701), labels = c("Below average", "Average", "Above Average"))

# Plotting data on the map 

just_prop = ggmap(map2) + geom_point(data = sfb, aes(longitude, latitude, color = percent_factors, alpha = 0.75)) + 
  labs(title = "Proportion of the elderly throughout Southern San Francisco Bay Area", x = "Longitude", y = "Latitude", color = "Elderly population percentages") +
  scale_color_manual(values = cbPalette) + facet_wrap(~county)

prop_vs_price = ggmap(map2) + geom_point(data = sfb, aes(longitude, latitude, color = percent_factors)) + facet_grid(price_breaks ~ county) + 
  labs(title = "Proportion of the elderly throughout Southern San Francisco Bay Area vs. price", x = "Longitude", y = "Latitude", color = "Elderly population percentages") +
  scale_color_manual(values = cbPalette)
# References ----------------
#  Stack overflow post on changing stuff
