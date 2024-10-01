library(tidyverse) #helps wrangle data
# Use the conflicted package to manage conflicts
library(conflicted)
# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
#=====================
# STEP 1: COLLECT DATA
#=====================
# # Upload Divvy datasets (csv files) here
df <- read.csv("12_months_city_bike.csv")

# Inspect the new table that has been created
colnames(df) #List of column names
nrow(df) #How many rows are in data frame?
dim(df) #Dimensions of the data frame?
head(df) #See the first 6 rows of data frame. Also tail(df)
str(df) #See list of columns and data types (numeric, character, etc)
summary(df) #Statistical summary of data. Mainly for numeric

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and
#"Subscriber")and two names for casual riders ("Customer" and "casual"). We will need to
# that from four to two labels.                                                                Already Done!!!

# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to
#add some additional columns of data -- such as day, month, year -- that provide additional
#opportunities to aggregate the data.

> # Assuming 'started_at' is in character format
df$started_at <- ymd_hms(df$started_at)# Separate year, month, day, and time and add them to df
df$year <- year(df$started_at)
df$month <- month(df$started_at)
df$day <- day(df$started_at)
df$time <- format(df$started_at, "%H:%M:%S")
head(df) #View the updated dataframe


# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have
# the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.

# (4) There are some rides where tripduration shows up as negative, including several hundred     
df <- df %>%
  filter(ride_length >= 0)

# rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to
# delete these 


# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with
# "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make
# our dataframe consistent with their current nomenclature

# N.B.: "Level" is a special property of a column that is retained even if a subset does not
# contain any values from a specific level
# Begin by seeing how many observations fall under each usertype
table(df$member_casual)
# Reassign to the desired values (we will go with the current 2020 labels)
df <- df %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(df$member_casual)
# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing
# these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
df$date <- as.Date(df$started_at) #The default format is yyyy-mm-dd
df$month <- format(as.Date(df$date), "%m")
df$day <- format(as.Date(df$date), "%d")
df$year <- format(as.Date(df$date), "%Y")
df$day_of_week <- format(as.Date(df$date), "%A")
# Add a "ride_length" calculation to df (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
df$ride_length <- difftime(df$ended_at,df$started_at)
# Inspect the structure of the columns
str(df)
# Convert "ride_length" from Factor to numeric so we can run calculations on the data
# is.factor(df$ride_length)
df$ride_length <- as.numeric(as.character(df$ride_length))
is.numeric(df$ride_length)
# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and
checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
df_v2 <- df[!(df$start_station_name == "HQ QR" | df$ride_length<0),]
#=====================================

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(df_v2$ride_length,na.rm=TRUE) #straight average (total ride length / rides)
median(df_v2$ride_length,na.rm=TRUE) #midpoint number in the ascending array of ride lengths
max(df_v2$ride_length,na.rm=TRUE) #longest ride
min(df_v2$ride_length,na.rm=TRUE) #shortest ride
# You can condense the four lines above to one line using summary() on the specific attribute
summary(df_v2$ride_length)
# Compare members and casual users
aggregate(df_v2$ride_length ~ df_v2$member_casual, FUN = mean)
aggregate(df_v2$ride_length ~ df_v2$member_casual, FUN = median)
aggregate(df_v2$ride_length ~ df_v2$member_casual, FUN = max)
aggregate(df_v2$ride_length ~ df_v2$member_casual, FUN = min)
# See the average ride time by each day for members vs casual users
aggregate(df_v2$ride_length ~ df_v2$member_casual + df_v2$day_of_week,
          FUN = mean)
# Notice that the days of the week are out of order. Let's fix that.
df_v2$day_of_week <- ordered(df_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# Now, let's run the average ride time by each day for members vs casual users
aggregate(df_v2$ride_length ~ df_v2$member_casual + df_v2$week_day,
          FUN = mean)
# analyze ridership data by type and weekday
df_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using
  wday()
group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates
            the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average
  duration
arrange(member_casual, weekday) # sorts
# Let's visualize the number of rides by rider type
df_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
# Let's create a visualization for average duration
df_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location
accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can
read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
  counts <- aggregate(df_v2$ride_length ~ df_v2$member_casual +
                        df_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')