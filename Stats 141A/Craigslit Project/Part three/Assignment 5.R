
library(stringr)
library(ggplot2)
install.packages("lubridate")
library(lubridate)

# 1 ------------------------

# Function will read lines of text file and collapse it into one whole line and string
read_post = function (txt){
  post = readLines(txt,encoding = "UTF-8")
  post_string = str_c(post, collapse = "\n")
}

# 2 ----------------------

# Function will read and extract basic features from every text file from a directory
read_all_posts = function(directory) {
  files = list.files(directory,full.names = TRUE)
  posts = sapply(files, read_post)
  cl = data.frame(text = posts, region = basename(directory))
  divide_post = str_split_fixed(cl$text, "\n", 2)
  cl$post_title = divide_post[,1]
  cl$post_description = divide_post[,2]
  cl = cl[,-1]
  # Get Date
  cl$date_posted = str_extract(cl$post_description, pattern = regex("date posted: [A-z]+ [0-9]+, [0-9]+ at [0-9]+:[0-9]+", ignore_case = TRUE))
  cl$date_posted = str_extract(cl$date_posted, pattern = regex("[A-z]+ [0-9]+, [0-9]+ at [0-9]+:[0-9]+", ignore_case = TRUE))
  cl$date_posted = as.character(cl$date_posted)
  cl$date_posted = strptime(cl$date_posted, "%B %d, %Y at %H:%M")
  # Get price
  cl$price = str_extract(cl$post_description, "Price: \\$[0-9,.]+")
  cl$price = str_remove_all(cl$price, ",") # Remove all commas so can make numeric
  cl$price = as.numeric(str_extract(cl$price, "[0-9,.]+"))
  # Latitude 
  cl$latitude = str_extract(cl$post_description, pattern = "Latitude: -?[0-9,.]+")
  cl$latitude = str_extract(cl$latitude, pattern = "-?[0-9,.]+")
  # Longitude
  cl$longitude = str_extract(cl$post_description, pattern = "Longitude: -?[0-9,.]+")
  cl$longitude = str_extract(cl$longitude, pattern = "-?[0-9,.]+")
  # Bedrooms
  cl$bedrooms = str_extract(cl$post_description, pattern = "Bedrooms: [0-9,.]+")
  cl$bedrooms = str_extract(cl$bedrooms, pattern = "[0-9,.]+")
  # Bathrooms
  cl$bathrooms = str_extract(cl$post_description, pattern = "Bathrooms: [0-9,.]+")
  cl$bathrooms = str_extract(cl$bathrooms, pattern = "[0-9,.]+")
  # Sqft
  cl$sqft = str_extract(cl$post_description, pattern = "Sqft: [0-9,.]+")
  cl$sqft = str_remove_all(cl$sqft, ",") # Remove all commas so can make numeric
  cl$sqft = as.numeric(str_extract(cl$sqft, "[0-9,.]+"))
}

# Setting up 4-8 ----------------------------

# This will get every post from every directory/folder, and put them into a data frame
my_dir = "~/Desktop/Sta 141a Assignment 5/messy"
dirs = list.files(my_dir, full.names = TRUE)
directory_files = function (directory){
  files = list.files(directory, full.names = TRUE)
  posts = sapply(files,read_post)
  data.frame(text = posts, region = basename(directory))
}
every_post = lapply(dirs, directory_files)
df = do.call(rbind,every_post) # total of 45,845 posts 

# Now divide it to get title and description
divide_post = str_split_fixed(df$text, "\n", 2)
df$post_title = divide_post[,1]
df$post_description = divide_post[,2]

# 4 ---------------

# Get the title price/user price and make it a numerical feature
df$title_price = str_extract(df$post_title,"\\$[0-9,.]+")
df$title_price = str_remove_all(df$title_price, ",") # Remove all commas so can make numeric
df$title_price = as.numeric(str_extract(df$title_price,"[0-9,.]+"))

user_pattern = regex("price: \\$[0-9,.]+", ignore_case = TRUE)
df$user_price = str_extract(df$post_description, pattern = user_pattern)
df$user_price = str_remove_all(df$user_price, ",") # Remove all commas so can make numeric
df$user_price = as.numeric(str_extract(df$user_price, "[0-9,.]+"))

# Compare the title price and user price 
table(abs(df$title_price - df$user_price)) # 43,672 posts have the same title and user price 
sum(is.na(df$title_price)) #  176 N/A values for title price
sum(is.na(df$user_price)) # 180 N/A values for user price 

# 5 ----------------------------- 

# find words that occur before deposit
other_deposits = str_extract(df$post_description, pattern = regex("[A-z]+ deposit:?", ignore_case = TRUE))
sort(table(other_deposits)) # Get frequencies of all the words before deposit

# Using The code above i defined a regular expression to extract security deposits and some other common deposits
deposit_pattern = regex("security deposit (of|is)? \\$[0-9,.]+|(pets?|cats?|holding|maximumpet|monthpet|allowed|security)? deposit:? ?\\$[0-9,.]+", ignore_case = TRUE) 

# First, i extract all security deposits and also the other most frequent deposits
# then i set all of those other deposits to NA so im only left with security deposits
df$security_deposit = str_extract(df$post_description, pattern = deposit_pattern)
df$security_deposit = str_replace_all(df$security_deposit, pattern = regex("(pets?|cats?|holding|maximumpet|monthpet|allowed) deposit:? ?\\$[0-9,.]+", ignore_case = TRUE), "NA")
df$security_deposit[df$security_deposit == "NA"] = NA
df$security_deposit = str_remove_all(df$security_deposit, ",")
df$security_deposit = as.numeric(str_extract(df$security_deposit,"[0-9,.]+"))
sum(is.na(df$security_deposit))

# Relationship among title price and security deposit price, subset it all to get the prices 
title_vs_sec = subset(df, !is.na(df$security_deposit))
title_vs_sec = subset(title_vs_sec,title_vs_sec$title_price >= 200 & title_vs_sec$security_deposit > 0)
fivenum(title_vs_sec$security_deposit)
fivenum(title_vs_sec$title_price)

# Make ggplot when finished, see if there is a linear relationship
ggplot(title_vs_sec, aes(x = title_price, y = security_deposit)) + geom_point() + ylim(0,10000) +
  labs(title = "Apartment prices versus security deposit prices", x = "Apartment price ($)", y = "Security deposit price ($)")

cor(title_vs_sec$title_price,title_vs_sec$security_deposit) #0.5398

# 6 ----------------------------

# Get patterns for both,dogs only,cats only, and none 
both_pattern = regex("pet-(friendly|max)|(cats|dogs) and (cats|dogs) (ok|allowed)|pets? (friendly|under|welcomed?|max)|(cats?|dogs?) \\& (cats?|dogs?) (allowed|ok)| love pets?", ignore_case = TRUE)
dog_only_pattern = regex("dogs? (only|ok|allowed|friendly)", ignore_case = TRUE)
cat_only_pattern = regex("cats? (only|ok|allowed|friendly)", ignore_case = TRUE)
no_pets_pattern = regex("no pet[s]", ignore_case = TRUE)
all_patterns = regex("pet-(friendly|max)|(cats|dogs) and (cats|dogs) (ok|allowed)|pets? (friendly|under|welcomed?|max)|(cats?|dogs?) \\& (dogs?|cats?) (allowed|ok)| love pets?|dogs? (only|ok|allowed|friendly)|cats? (only|ok|allowed|friendly)|no pet[s]", ignore_case = TRUE)

# Extract all descriptions that include any of the patterns above, and them categorize them
df$pet_policy = str_extract(df$post_description, pattern = all_patterns)
df$pet_policy = str_replace_all(df$pet_policy, pattern = both_pattern, "both")
df$pet_policy = str_replace_all(df$pet_policy, pattern = no_pets_pattern, "none")
df$pet_policy = str_replace_all(df$pet_policy, pattern = dog_only_pattern, "dogs")
df$pet_policy = str_replace_all(df$pet_policy, pattern = cat_only_pattern, "cats")
table(df$pet_policy)       

# Any other kind of pets, subset the data by the pattern
other_pets = subset(df, str_detect(df$post_description, "[Ss]mall [Cc]aged [Aa]nimals?") == TRUE)
head(other_pets$post_description)
# Some apartments allows small caged animals like birds, hamsters, gerbils, rabbits, guinea pigs, 
# chinchillas and aquarium/terrarium animals including fish, hermit crabs, turtles, frogs, and small lizards
# No exotic ones allowed

# Extracting pet deposits
# Get posts that include pet deposits to identify pet deposit patterns
pd_pattern = regex("pets? deposit", ignore_case = TRUE)
pets_df = subset(df, stTr_detect(df$post_description, pattern = pd_pattern) == TRUE ) # 2,131 have info on pet deposits

# Get the pet deposits
final_pattern = regex("\\$[0-9,.]+ per (cats?|dogs?)|pets? deposit:? \\$[0-9,.]+|pets? deposit : \\$[0-9,.]+|\\$[0-9,.]+ pets? deposits?|\\$[0-9,.]+ deposit per pets?", ignore_case = TRUE)
df$pet_deposit = str_extract(df$post_description, pattern = final_pattern)
df$pet_deposit = str_remove_all(df$pet_deposit, ",") # Remove all commas so can make numeric
df$pet_deposit = as.numeric(str_extract(df$pet_deposit, "[0-9,.]+"))

ggplot(df, aes(x = df$pet_deposit)) + geom_bar() + labs(title = "Distribution of pet deposits", x = "Pet deposit prices") # As seen mainly pet deposits cost $500

# 7 -------------------------------------

# Get heat and fireplace patterns
heat_patterns = regex("heaters?|heating|central (heatings?|heat|heaters?)", ignore_case = TRUE)
fire_place_pattern = regex("fireplaces?", ignore_case = TRUE)

# determine if post is heater or fireplace
df$heating = str_extract(df$post_description, pattern = heat_patterns)
df$fireplace = str_extract(df$post_description, pattern =  fire_place_pattern)

df$heating = str_replace(df$heating,heat_patterns, "true")
df$fireplace = str_replace(df$fireplace,fire_place_pattern, "true")

# Merge both fireplace and heater to categorize by: none,both,heating,fireplace
df$heating_options = paste(df$heating, df$fireplace)
df$heating_options = str_replace(df$heating_options, "NA NA", "none")
df$heating_options = str_replace(df$heating_options, "true NA", "heater")
df$heating_options = str_replace(df$heating_options, "NA true", "fireplace")
df$heating_options = str_replace(df$heating_options, "true true", "both")

ggplot(df, aes(x = heating_options)) + geom_bar() + labs(title = "distribution of heating options", x = "Heating options")

# Look at apartments with AC/cooling options
air_patterns = regex("A\\/C unit|[Aa]ir (condition|conditioning|conditioner)|AC|[Cc]entral ([Aa]ir|AC)", ignore_case = FALSE)
df$cooling_options = str_extract(df$post_description, pattern = air_patterns)
df$cooling_options = str_replace(df$cooling_options, pattern = air_patterns, "AC")
df$cooling_options = df[df$cooling_options == "<NA>"] = "No AC"


# Compare heating options and cooling options
table(df$heating_options) # 15,560 apartments have a heating option
table(df$cooling_options) # 10,521 apartments have a cooling option 

# heating options in apertments with and without AC
ggplot(df, aes(x = as.factor(cooling_options),fill = heating_options)) + geom_bar(position = "fill") + 
  scale_x_discrete(label = c("Apartments with AC", "Apartments without AC")) + labs(title = "Distribution of heating options within apartments with and without AC options", x = "Cooling options", y = "proportion of heating options") +
  scale_fill_viridis_d(name = "Heating options") 

# Cooling options among apartments with/without heating
ggplot(df, aes(x = as.factor(heating_options),fill = cooling_options)) + geom_bar(position = "fill") + scale_x_discrete(label = c("Both", "Fireplace", "Heater", "None")) + 
  labs(title = "Distribution of cooling options within apartments with and without heating options options", x = "Heating options", y = "proportion of cooling options") + 
  scale_fill_viridis_d(na.value = "#E69F00", name = "Cooling options")

# 8 ------------------------------

# See how many posts have phone numbers and emails
df$emails = str_extract(df$post_description, pattern = "[A-z0-9-]+\\@[A-z0-9-]+\\.[A-z0-9-]{2,3}") 
df$phone = str_extract(df$post_description, pattern = regex("\\(?\\d{3}\\)?( |-)+\\d{3}( |-)(\\d|o){4}|show contact info", ignore_case = TRUE)) 


# References: 
#    Piazza post 396 (getting phone number)
#    Paste function: https://stackoverflow.com/questions/18115550/how-to-combine-two-or-more-columns-in-a-dataframe-into-a-new-column-with-a-new-n/40994869
#    Worked with Jonathan Fernandez, Kelly Chan, Jiemin Huang, Fenglan Jiang 












