##############################################
# Using the R Script
# Instead of Rmd
# Will transfer the code blocks to Rmd
##############################################

## Importing Necessary Libraries
##---------
FUNCTIONS_PATH = "D:/UIS/Studies/Semester4/CSC532/Assignments/ML_Final_Project/ml_final_project/code_base/custom_functions.R"
source(FUNCTIONS_PATH)
##---------
library(dplyr)
library(readr)
library(caret)
library(mltools)
library(data.table)
##---------
# library(tm)
# library(class)
# library(gmodels)
# library(wordcloud)
# library(SnowballC)
# library(e1071)
# library(qdap)
# library(C50)
# library(abind)
# library(tfruns)
# library(keras)

## File Paths
input_folder_path = "D:/UIS/Studies/Semester4/CSC532/Assignments/ML_Final_Project/ml_final_project/input_datasets/"
input_file_name = "hotel_reservations_dataset/Hotel Reservations.csv"
input_file_path = paste(input_folder_path, input_file_name, sep = "")


## Reading dataset
hotel_reservation_data <- read_csv(input_file_path) # For a change we are using 'read_csv' function

## Primitive Information
class(hotel_reservation_data)
# Let us check the shape of the data
data_dim <- dim(hotel_reservation_data)
print(paste("Number of Records : ", data_dim[1]))
print(paste("Number of Features : ", data_dim[2]))
# Let us check the feature name snad the structure
str(hotel_reservation_data)
# Check the summary
summary(hotel_reservation_data)
# Check if there are missing values
# And let us do a thorough check
sum(is.na(hotel_reservation_data))
colSums(is.na(hotel_reservation_data))
# using a similar logic, I used in Assignment 2
na_strings_check <- c("NA", "N/A", "NULL", "", "."," ")
contains_strings <- apply(hotel_reservation_data, 2, function(x) any(x %in% na_strings_check))
contains_strings
# Hence we can conclude that we don't have any missing values

# Let us check number of uniqe elemnets in each of the feature
# Based on the number of unique features let us further analyse
# CHAT_GPT: short code to check the number of unique elements in the features in R dataframe
unique_counts_features <- sapply(hotel_reservation_data, function(x) length(unique(x)))
print(unique_counts_features)
print(sort(unique_counts_features, decreasing = TRUE))


# From the numbers, we can understand that the number of booking_ids are same as the number of records
# Which means that each record in the dataframe is a unique observation

# Booking_ID                                            36275
# no_of_adults                                              5
# no_of_children                                            6
# no_of_weekend_nights                                      8
# no_of_week_nights                                        18
# type_of_meal_plan                                         4
# required_car_parking_space                                2
# room_type_reserved                                        7
# lead_time                                               352
# arrival_year                                              2
# arrival_month                                            12
# arrival_date                                             31
# market_segment_type                                       5
# repeated_guest                                            2
# no_of_previous_cancellations                              9
# no_of_previous_bookings_not_canceled                     59
# avg_price_per_room                                     3930
# no_of_special_requests                                    6
# booking_status                                            2
