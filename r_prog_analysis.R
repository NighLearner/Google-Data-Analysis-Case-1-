library(tidyverse) #helps wrangle data
# Use the conflicted package to manage conflicts
library(conflicted)
library(dplyr)
library(ggplot2)

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
# Assuming 'started_at' is in character format
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
# checked for quality by Divvy or ride_length was negative
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
agg_data <-aggregate(df_v2$ride_length ~ df_v2$member_casual + df_v2$day_of_week,
          FUN = mean)
colnames(agg_data) <- c("member_casual", "week_day", "mean_ride_length")

# Define the correct order of the days (Monday to Sunday)
day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Convert 'week_day' to a factor with the correct order
agg_data$week_day <- factor(agg_data$week_day, levels = day_order)

# Order the data by 'week_day'
agg_data <- agg_data %>%
  arrange(week_day)


write.csv(agg_data, "aggregated_ride_length.csv", row.names = FALSE)
head(agg_data)
# Notice that the days of the week are out of order. Let's fix that.
df_v2$day_of_week <- ordered(df_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# Now, let's run the average ride time by each day for members vs casual users
aggregate(df_v2$ride_length ~ df_v2$member_casual + df_v2$week_of_day,
          FUN = mean)
# analyze ridership data by type and weekday
# Group data by rider type (member_casual) and day of the week (week_day)
ridership_analysis <- df_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    total_rides = n(),  # Total number of rides
    average_ride_length = mean(ride_length, na.rm = TRUE),  # Average ride length
    .groups = 'drop'
  )

head(ridership_analysis)


ridership_analysis$day_of_week <- factor(ridership_analysis$day_of_week, levels = day_order)
ridership_analysis <- ridership_analysis %>%
  arrange(member_casual, day_of_week)
print(ridership_analysis)
write.csv(ridership_analysis, "agg_count__ride_length.csv", row.names = FALSE)

ggplot(ridership_analysis, aes(x = day_of_week, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Rides by Rider Type and Day of the Week",
       x = "Day of the Week", y = "Total Rides") +
  theme_minimal()

ggplot(ridership_analysis, aes(x = day_of_week, y = average_ride_length, color = member_casual, group = member_casual)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Average Ride Length by Rider Type and Day of the Week",
       x = "Day of the Week", y = "Average Ride Length (seconds)") +
  theme_minimal()


# Create a new dataframe that groups by month and member_casual, then counts the number of rides
monthly_riders_df <- df_v2 %>%
  group_by(month, member_casual) %>%
  summarise(
    number_of_riders = n(),  # Count the number of rides
    .groups = 'drop'
  )

head(monthly_riders_df)

write.csv(ridership_analysis, "monthly_rides.csv", row.names = FALSE)

# Ensure month is numeric
monthly_riders_df$month <- as.numeric(monthly_riders_df$month)

str(monthly_riders_df)

# Plot the chart
ggplot(monthly_riders_df, aes(x = month, y = number_of_riders, color = member_casual, group = member_casual)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Number of Riders by Month and Rider Type",
       x = "Month", y = "Number of Riders",
       color = "Rider Type") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12, labels = month.name)


# Count the number of rides per start station
station_rider_counts <- df_v2 %>%
  group_by(start_station_name) %>%
  summarise(rider_count = n(), .groups = 'drop')

# Get the top 5 and bottom 5 stations based on rider count
top_5_stations <- station_rider_counts %>% 
  arrange(desc(rider_count)) %>% 
  slice(1:10)

bottom_5_stations <- station_rider_counts %>%
  arrange(rider_count) %>%
  slice(1:10)

# Bar plot for top 10 stations
ggplot(top_5_stations, aes(x = reorder(start_station_name, -rider_count), y = rider_count, fill = start_station_name)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for easier reading
  labs(title = "Top 5 Start Stations by Rider Count",
       x = "Start Station Name", y = "Rider Count") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend

# Bar plot for bottom 10 stations
ggplot(bottom_5_stations, aes(x = reorder(start_station_name, rider_count), y = rider_count, fill = start_station_name)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for easier reading
  labs(title = "Bottom 5 Start Stations by Rider Count",
       x = "Start Station Name", y = "Rider Count") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend


#Ride duration analysis
# Calculate average ride length by rider type
ride_duration_analysis <- df_v2 %>%
  group_by(member_casual) %>%
  summarise(
    avg_ride_length = mean(ride_length, na.rm = TRUE),
    median_ride_length = median(ride_length, na.rm = TRUE),
    .groups = 'drop'
  )

print(ride_duration_analysis)

ggplot(df_v2, aes(x = member_casual, y = ride_length, fill = member_casual)) +
  geom_boxplot() +
  labs(title = "Ride Length Distribution by Rider Type",
       x = "Rider Type", y = "Ride Length (seconds)") +
  theme_minimal()

#Hourly ride pattern
# Extract hour from the started_at column
df_v2 <- df_v2 %>%
  mutate(hour = format(started_at, "%H"))

# Calculate number of rides by hour and member_casual
hourly_rides <- df_v2 %>%
  group_by(hour, member_casual) %>%
  summarise(rides = n(), .groups = 'drop')

# Plot hourly ride patterns
ggplot(hourly_rides, aes(x = hour, y = rides, color = member_casual, group = member_casual)) +
  geom_line(size = 1) +
  labs(title = "Hourly Ride Patterns by Rider Type",
       x = "Hour of Day", y = "Number of Rides") +
  theme_minimal()

# weekend and weekday usage

# Create a new column indicating whether it's a weekend or weekday
df_v2 <- df_v2 %>%
  mutate(day_type = ifelse(day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

# Summarise rides by day type and member_casual
day_type_analysis <- df_v2 %>%
  group_by(day_type, member_casual) %>%
  summarise(total_rides = n(), .groups = 'drop')

print(day_type_analysis)

# Visualize
ggplot(day_type_analysis, aes(x = day_type, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Rides by Day Type and Rider Type",
       x = "Day Type", y = "Total Rides") +
  theme_minimal()


ggplot(monthly_riders_df, aes(x = month, y = number_of_riders, color = member_casual, group = member_casual)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Seasonal Trends: Number of Riders by Month",
       x = "Month", y = "Number of Riders",
       color = "Rider Type") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12, labels = month.name)

# Bike type preferences
# Count number of rides by bike type and member_casual
bike_type_analysis <- df_v2 %>%
  group_by(rideable_type, member_casual) %>%
  summarise(total_rides = n(), .groups = 'drop')

print(bike_type_analysis)

# Visualize
ggplot(bike_type_analysis, aes(x = rideable_type, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bike Type Usage by Rider Type",
       x = "Bike Type", y = "Total Rides") +
  theme_minimal()

# Saving all inferences
# Save Ride Duration Analysis
write.csv(ride_duration_analysis, "ride_duration_analysis.csv", row.names = FALSE)

# Save Hourly Ride Patterns
write.csv(hourly_rides, "hourly_rides.csv", row.names = FALSE)

# Save Weekend vs. Weekday Usage
write.csv(day_type_analysis, "day_type_analysis.csv", row.names = FALSE)

# Save Bike Type Preferences
write.csv(bike_type_analysis, "bike_type_analysis.csv", row.names = FALSE)

# Save Monthly Riders Data
write.csv(monthly_riders_df, "monthly_riders_df.csv", row.names = FALSE)

# Optional: Save the main dataset
write.csv(df_v2, "full_dataset.csv", row.names = FALSE)





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