setwd("E:/US SIUE/Intro to BA/Project/Final")
library(ggplot2)
library(dplyr)
library(caret)
library(caTools)
library(class)
library(rpart)
library(rpart.plot)
library(car)
#dev.off()

#-------------------------------------------------------------------------------
#load data
nyc_datasets <- read.csv(file="AB_NYC_2019.csv",header = T)
head(nyc_datasets)

#-------------------------------------------------------------------------------
#checking null values
which(is.na(nyc_datasets))
print(sum(is.na(nyc_datasets)))
print(colSums(is.na(nyc_datasets)))

#-------------------------------------------------------------------------------
#replacing all Nan values in 'reviews_per_month' with 0
nyc_datasets$reviews_per_month[which(is.na(nyc_datasets$reviews_per_month))] <- 0
print(colSums(is.na(nyc_datasets)))
print(sum(is.na(nyc_datasets)))

#-------------------------------------------------------------------------------
#dropping columns that are not significant
nyc_datasets<- subset(nyc_datasets, select = -c(id))
print (nyc_datasets)

#-------------------------------------------------------------------------------
#Finding data inconsistency 
summary(nyc_datasets)

#-------------------------------------------------------------------------------
#Identifying duplicate values
duplicates <- nyc_datasets[duplicated(nyc_datasets), ]
print(duplicates)

#-------------------------------------------------------------------------------
#Counting how many duplicate rows exist 
duplicate_count <- sum(duplicated(nyc_datasets))
print(duplicate_count)

#-------------------------------------------------------------------------------
#Handling outliers and noisy data in minimum nights
#Create box plot to identify outliers
boxplot(nyc_datasets$minimum_nights,main="Boxplot of Minimum Nights", xlab="Minimum Nights", ylab="Count", col = "blue",  border = "black" ,boxwex = 0.6)

#Calculate IQR and bounds for minimum nights 
Q1_minimumnights <- quantile(nyc_datasets$minimum_nights,0.25)
Q3_minimumnights <- quantile(nyc_datasets$minimum_nights,0.75)
IQR_minimumnights<- Q3_minimumnights-Q1_minimumnights
lower_bound_minnights <- Q1_minimumnights - 1.5 * IQR_minimumnights
print(lower_bound_minnights)
upper_bound_minnights <- Q3_minimumnights + 1.5 * IQR_minimumnights
print(upper_bound_minnights)

#Identify outliers for minimum nights
outliers_minnights <- nyc_datasets$minimum_nights[nyc_datasets$minimum_nights < lower_bound_minnights | nyc_datasets$minimum_nights > upper_bound_minnights]
print(outliers_minnights)

#Removing outliers 
nyc_datasets <- nyc_datasets [
  nyc_datasets$minimum_nights > (Q1_minimumnights - 1.5 * IQR_minimumnights) & 
    nyc_datasets$minimum_nights < (Q3_minimumnights + 1.5 * IQR_minimumnights),]

#After removing outliers and noisy data
boxplot(nyc_datasets$minimum_nights,main="Boxplot of Minimum Nights", xlab="Minimum Nights", ylab="Count", col = "blue",  border = "black" ,boxwex = 0.5)

#Summarizing data
summary(nyc_datasets$minimum_nights)

#-------------------------------------------------------------------------------
#Handling outliers and noisy data in Price
#Create box plot to identify data that does not make sense

boxplot(nyc_datasets$price,main="Boxplot of Price", xlab="Price (in dollars)", ylab="Frequency", col = "#4F81BD",  border = "#2E75B6" ,boxwex = 0.5)

#Removing price below $80 & above $5000
nyc_datasets <- nyc_datasets [
  nyc_datasets$price > 80 &
    nyc_datasets$price < 5000,]

print(nyc_datasets$price)

#Calculating mean,median and mode of the price to understand pricing data
price = mean(nyc_datasets$price)
print(price)

price_median = median(nyc_datasets$price)
print(price_median)

common_mode <- table(nyc_datasets$price)
common_mode <- head(sort(common_mode, decreasing = TRUE), 10)
print(common_mode)

price_min = min(nyc_datasets$price)
print(price_min)

price_max = max(nyc_datasets$price)
print(price_max)

price_standard = sd(nyc_datasets$price)
print(price_standard)

#-------------------------------------------------------------------------------
#Finding the most common room type among the guests
#Understanding the room types
Room_Type <- unique(nyc_datasets$room_type)
print(Room_Type)

#which room type is the most popular among guests 
top_room_type <- table(nyc_datasets$room_type)
top_room_type <- head(sort(top_room_type, decreasing = TRUE))
print(top_room_type)

# Summarize data to get counts by room type
top_room_type <- nyc_datasets %>%
  group_by(room_type) %>% # Group by room type
  summarise(Count = n()) %>% # Count the number of listings per type
  arrange(desc(Count)) # Optional: Sort by descending count

#Visualizing number of listings by room type
ggplot(top_room_type, aes(x = reorder(room_type, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkblue", color = "black", width = 0.3) +
  coord_flip() + # Convert to horizontal bar plot
  labs(
    title = ~bold("Number of Listings by Room Type"),
    x = "Room Type",
    y = "Number of Listings"
  ) +
  theme_minimal() # Optional: Use a clean theme

#-------------------------------------------------------------------------------
#top hosts in NYC
top_host <- table(nyc_datasets$host_id)
top_host <- head(sort(top_host, decreasing = TRUE), 10)
print(top_host)

# Summarize data to get counts by hostID's
popular_hosts <- nyc_datasets %>%
  group_by(host_name) %>% # Group by room type
  summarise(Count = n()) %>% # Count the number of listings per type
  arrange(desc(Count)) %>%
  slice_max(Count, n = 8) # Optional: Sort by descending count

print(popular_hosts)

#Visualizing number of listings by host name
ggplot(popular_hosts, aes(x = reorder(host_name,Count), y = Count)) +
  geom_bar(stat = "identity", fill = "royalblue", color = "black", width = 0.6) +
  coord_flip() +
  labs(
    title = ~bold("Number of Listings by Host Name"),
    x = "Host Name",
    y = "Number of Listings"
  ) +
  theme_minimal() # Optional: Use a clean theme


#-------------------------------------------------------------------------------
#Finding distribution of prices across different room types
# Aggregate price by room_type
aggregated_data <- nyc_datasets %>%
  group_by(room_type) %>% # Group data by room_type
  summarise(
    Avg_Price = mean(price),      # Average price
    Median_Price = median(price),# Median price
    Min_Price = min(price),      # Minimum price
    Max_Price = max(price),      # Maximum price
    Total_Listings = n()         # Number of listings
  )

# View the aggregated data
print(aggregated_data)

# Visualization
# Bar chart of average price by room type
ggplot(aggregated_data, aes(x = room_type, y = Avg_Price, fill=room_type, width = 0.5)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = ~bold("Average Price by Room Type"),
    x = "Room Type",
    y = "Average Price ($)"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------
#Finding distribution of price in each neighborhood_group
#Unique neighborhood groups
unique(nyc_datasets$neighbourhood_group)
length(unique(nyc_datasets$neighbourhood_group))

aggregated_price_neighbourhood <- nyc_datasets %>%
  group_by(neighbourhood_group) %>% # Group data by neighbourhood_group
  summarise(
    Avg_Price = mean(price),      # Average price
    Median_Price = median(price),# Median price
    Min_Price = min(price),      # Minimum price
    Max_Price = max(price),      # Maximum price
    Total_Listings = n()                       # Number of listings
  )

# View the aggregated data
print(aggregated_price_neighbourhood)

# Visualization
# Bar chart of average price by room type
ggplot(aggregated_price_neighbourhood, aes(x = reorder(neighbourhood_group,Total_Listings), y = Avg_Price, fill=neighbourhood_group, width = 0.5)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = ~bold("Average Price by Neighborhood Group"),
    x = "Neighborhood Group",
    y = "Average Price ($)"
  ) +
  theme_minimal()

#-----------------------------------------------
#Explanation:
#A Single-Host is a host with only one listing (calculated_host_listings_count == 1).
#A Multi-Host is a host with more than one listing (calculated_host_listings_count > 1).
#Visualization of Single and Multi-Listings
nyc_datasets<- nyc_datasets %>%
  mutate(listing_type = ifelse(calculated_host_listings_count > 1, "Multi-Listing", "Single-Listing"))

# Count Single and Multi-Listings
listing_distribution <- nyc_datasets %>%
  group_by(listing_type) %>%
  summarise(count = n())

# Classify hosts as Single or Multi
host_summary <- nyc_datasets %>%
  mutate(host_type = ifelse(calculated_host_listings_count > 1, "Multi-Host", "Single-Host")) %>%
  group_by(host_type) %>%
  summarise(count = n())

print(host_summary)

# Visualizing Single vs Multi-Hosts
ggplot(host_summary, aes(x = host_type, y = count, fill = host_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Single vs Multi-Hosts",
       x = "Host Type",
       y = "Count of Hosts") +
  theme_minimal()

#------------------------------------------
#Revenue Trends by Neighborhood Group
# Create a new column for estimated revenue
nyc_datasets$revenue <- nyc_datasets$price * nyc_datasets$minimum_nights
# Summarize revenue by neighborhood groups
revenue_trends <- nyc_datasets %>%
  group_by(neighbourhood_group) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE))

print(revenue_trends)

# Adjusting the bar chart to use a log scale for better visibility
ggplot(revenue_trends, aes(x = reorder(neighbourhood_group, -total_revenue), y = total_revenue, fill = neighbourhood_group)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(trans = 'log10') +  # Apply log scale
  labs(title = "Revenue Trends by Neighborhood Group",
       x = "Neighborhood Group",
       y = "Total Revenue (Log Scale)") +
  theme_minimal() +
  geom_text(aes(label = round(total_revenue, 0)), vjust = -0.5, size = 3)  # Add revenue values as labels

#-------------------------------------------------------------------------------
#Pricing Trends (Peak Seasons & Holiday Seasons)
if(!require('tidyverse')) {install.packages('tidyverse'); library(tidyverse)}
if(!require('lubridate')) {install.packages('lubridate'); library(lubridate)}

# Convert 'last_review' to Date format
nyc_datasets$last_review <- as.Date(nyc_datasets$last_review, format = "%m/%d/%Y")
print(nyc_datasets$last_review)

sum(is.na(nyc_datasets$last_review))  # Count of NA values in the column

# Extract month for seasonal analysis
nyc_datasets$month <- lubridate::month(nyc_datasets$last_review, label = TRUE)
print(nyc_datasets$month)

# Monthly Pricing Trends
monthly_pricing <- nyc_datasets %>%
  filter(!is.na(last_review)) %>%
  group_by(month) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))
print(monthly_pricing)

# Plot Monthly Pricing Trends
ggplot(monthly_pricing, aes(x = month, y = avg_price)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Pricing Trends for Airbnb in NYC",
       x = "Month",
       y = "Average Price") +
  theme_minimal()

#-------------------------------------------------------------------------------
#Revenue generated for airbnb in NYC
nyc_datasets <- nyc_datasets %>%
  mutate(revenue_potential = price * minimum_nights)
head(nyc_datasets)
print(nyc_datasets$revenue)

#Visualization Scatter Plot to show whether higher prices lead to higher revenue
ggplot(nyc_datasets %>% filter(price < 1000, revenue_potential < 10000), 
       aes(x = price, y = revenue_potential)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  labs(title = "Revenue Potential vs. Price (Zoomed In)",
       x = "Price ($)",
       y = "Revenue Potential ($)") +
  theme_minimal()


#Objective: To create linear regression model; use price as a dependent variable and neighborhood groups and neighborhood as independent variable)
#----------------------------------
#Using room type, minimum nights and availability as factors
cat("Performing linear regression: price...\n")
# Ensure price is numeric
nyc_datasets$price <- as.numeric(nyc_datasets$price)
is.numeric(resid(lm_model))

# Ensure room_type is a factor (if not already)
nyc_datasets$room_type <- as.factor(nyc_datasets$room_type)

# Build the model
lm_model <- lm(price ~ room_type + minimum_nights +  availability_365 + neighbourhood_group + number_of_reviews + listing_type , data = nyc_datasets)

# Print the summary of the model
cat("Linear Regression Model Summary:\n")
print(summary(lm_model))

#Using room type, minimum nights and availability as factors
cat("Performing linear regression: price...\n")
# Ensure price is numeric
nyc_datasets$price <- as.numeric(as.character(nyc_datasets$price))

# Build the model
lm_model1 <- lm(price ~   availability_365 + neighbourhood_group + room_type, data = nyc_datasets)
is.numeric(resid(lm_model1))
# Print the summary of the model
cat("Linear Regression Model Summary:\n")
print(summary(lm_model1))

