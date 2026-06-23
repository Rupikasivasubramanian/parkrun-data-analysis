# load required libraries
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(hms)

# Loads all the files into one object 
files <- list.files(path = "data", full.names = TRUE)  

# map_dfr - applies a function to each element of a list or vector
# ~ - anonymous function where .x is current element
# read_csv(.x) - read file where .x is the filepath of each iteration
# filename = basename(.x) - Adds a new column filename to the data
# containing just the base name of the file (not the full path) for each row
all_data <- files %>%
  map_dfr(~ read_csv(.x) %>% 
            mutate(
              filename = basename(.x)
            ))

# convert tibble element to dataframe
raw_df <- as.data.frame(all_data)
head(raw_df)

# split filename into location and ID of parkrun
raw_df <- separate(
  raw_df,
  col = filename,
  into = c("parkrun_loc" , "not_required" , "parkrun_id"),
  sep = "_")

# drop Not Required variable from dataset
raw_df <- select(
  raw_df,
  -c(not_required)
)

# split parkrunner variable
raw_df <- separate(
  raw_df,
  col = parkrunner,
  into = c("col1" , "col2" , "col3" , "col4" , "col5" , "col6" , "col7" , "col8" , "col9"),
  sep = "\n"
)

# replace Unknown with NA in all character columns
raw_df <- raw_df %>% 
  mutate(across(where(is.character), ~na_if(.x, "Unknown")))

# extract parkrunner name and number of parkruns from col1
raw_df <- raw_df %>% 
  mutate(
    Parkrunner = str_match(col1, "^(.*?)(?=\\d|\\bparkrun\\b|$)")[,2] %>% str_squish(),
    Number_of_Parkruns = str_extract(col1 , "\\d+")
  )

# change number of parkruns and parkrun_id columns to numeric
raw_df$Number_of_Parkruns <- as.numeric(raw_df$Number_of_Parkruns)
raw_df$parkrun_id <- as.numeric(raw_df$parkrun_id)

# remove extra columns
raw_df <- raw_df %>% 
  select(-c("col1" , "col2" , "col3" , "col4" , "col5" , "col6" , "col7" , "col8" , "col9"))

# extract gender from Gender
raw_df <- raw_df %>% 
  mutate(
    Gender1 = str_extract(Gender , "^[A-Za-z]+"),
  )

# extract age group and age grade from column Age Group
raw_df <- raw_df %>%
  extract(
    `Age Group`,
    into = c("Age_Group", "Age_Grading"),
    regex = "^([A-Za-z]{2}\\d{2}(?:-\\d{2})?)(\\d+(?:\\.\\d+)?)%\\s+age grade$"
  ) %>%
  mutate(Age_Grading = as.numeric(Age_Grading))

# extract time and personal best time from column Time
raw_df <-  raw_df %>%
  extract(
    Time,
    into = c("Time1" , "Time2"),
    regex = "^([^A-Za-z]*)([A-Za-z].*)?$",
    remove = FALSE
  )

# getting Personal Best Time
raw_df <- raw_df %>%
  mutate(Personal_Best_Time = str_extract(Time2, "(?:\\d+:)?\\d{1,2}:[0-5]\\d"))

# copying time to PB time if it is empty
raw_df <- raw_df %>% 
  mutate(
    Personal_Best_Time = if_else(is.na(Personal_Best_Time) | str_trim(Personal_Best_Time) == "" ,
                                 Time1,
                                 Personal_Best_Time)
  )

# Assigning a milestone club
# Milestone club is assigned when individual reach 10 (for under 18s)
# , 25, 50, 100, 250, 500 and 1,000 Saturday 5k parkruns
raw_df <- raw_df %>% 
  mutate(
    Milestone_Club = case_when(
      Number_of_Parkruns < 25 & substring(raw_df$Age_Group , 1 , 1) != "J" ~ "Not Applicable",
      Number_of_Parkruns < 25 & substring(raw_df$Age_Group , 1 , 1) == "J" ~ "Member of the 10 Club",
      Number_of_Parkruns >= 25 & Number_of_Parkruns < 50 ~ "Member of the 25 Club",
      Number_of_Parkruns >= 50 & Number_of_Parkruns < 100 ~ "Member of the 50 Club",
      Number_of_Parkruns >= 100 & Number_of_Parkruns < 250 ~ "Member of the 100 Club",
      Number_of_Parkruns >= 250 & Number_of_Parkruns < 500 ~ "Member of the 250 Club",
      Number_of_Parkruns >= 500 ~ "Member of the 500 Club",
      .default = "Not Applicable"
    )
  )

# remove extract columns
raw_df <- raw_df %>% 
  select(-c("Gender" , "Time" , "Time2"))

# renaming columns
raw_df <- raw_df %>% 
  rename(
    position = Position,
    age_group = Age_Group,
    age_grading_pct = Age_Grading,
    club = Club,
    finish_time = Time1,
    parkrun_location = parkrun_loc,
    parkrunner = Parkrunner,
    total_parkruns = Number_of_Parkruns,
    gender = Gender1,
    personal_best_time = Personal_Best_Time,
    milestone_club = Milestone_Club
  )

# converting parkrunners name into proper format
raw_df <- raw_df %>% 
  mutate(
    parkrunner = parkrunner %>% 
      str_squish() %>% 
      str_to_title()
  )

# trim white spaces from columns
raw_df <- raw_df %>%
  mutate(across(where(is.character), ~ str_trim(.x, side = "right")))

# convert time to time format
raw_df <- raw_df %>% 
  mutate(
    finish_time = as_hms(parse_date_time(finish_time , orders = c("HMS" , "MS") , tz = "UTC")),
    personal_best_time = as_hms(parse_date_time(personal_best_time , orders = c("HMS" , "MS") , tz = "UTC"))
  )

# check for NA 's across each variable
colSums(is.na(raw_df))

# Since 1325 records don't have a parkrunner so it is best to remove those records
raw_df <- raw_df %>% 
  filter(!is.na(parkrunner))

# Assign "Not Available" instead of NA's in column Age Group, Age Grading, Gender
# and club
raw_df <- raw_df %>%
  mutate(
    across(c(age_group, gender, club), ~ replace_na(.x, "Not Available")),
    age_grading_pct = replace_na(age_grading_pct, 0)  # numeric default
  )

# check for NA 's across each variable
colSums(is.na(raw_df))

# final dataframe
head(raw_df)

#check for count of parkrunners more than 2
raw_df %>% 
  count(raw_df$parkrunner, sort = TRUE) %>%
  filter (n>=2)

#Analysis for unique Parkrunner with age group
raw_df %>%
  filter(parkrunner == "Mark Paterson") %>%
  select(parkrunner, age_group, parkrun_location, parkrun_id)

#new df for unique parkrunner and age group combination
new_combined_df <- raw_df %>%
  select(parkrunner, age_group) %>%
  mutate(parkrunner_age = paste(parkrunner, age_group, sep = "_")) %>%
  distinct(parkrunner_age) %>%
  arrange(parkrunner_age) %>%
  mutate(parkrunner_id = 10000 + row_number())

#Verifying Unique parkrunners
new_combined_df %>% 
  count(parkrunner_age, sort = TRUE) %>%
  filter (n>1)

#combining it to original raw_df
raw_df <- raw_df %>%
  mutate(parkrunner_age = paste(parkrunner, age_group, sep = "_")) %>%
  left_join(new_combined_df, by = "parkrunner_age")

#Extracting age from original df
raw_df <- raw_df %>%
  mutate(
    age = case_when(
      age_group == "Not Available" ~ "Not Available",
      str_detect(age_group, "\\d+-\\d+") ~ str_extract(age_group, "\\d+-\\d+"),  
      str_detect(age_group, "\\d+$") ~ str_extract(age_group, "\\d+$"),          
      TRUE ~ "Not Available" 
    )
  )

# adding an event ID variable which shows the week number
raw_df <- raw_df %>% 
  group_by(parkrun_location) %>%
  mutate(event_id = dense_rank(parkrun_id))

# final df
final_df <- raw_df %>%
  select(parkrunner_id, event_id, parkrun_location, gender, age, club, finish_time) %>%
  rename(
    id = parkrunner_id,
    week = event_id,
    parkrun = parkrun_location,
    sex = gender,
    time = finish_time
  )

# writing final data frame into gizzed csv file
write.csv(x = final_df, file = gzfile("data/out/final_df.csv.gz"), row.names = FALSE)