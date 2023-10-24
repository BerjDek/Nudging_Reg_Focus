

raw_user_data <- read.csv(file="reports_by_uuid.csv", header = TRUE)


#since the beginning of the project, there have been 348,592 registration/downloads
raw_user_data <- raw_user_data %>% 
  rename( User_ID = user_UUID,
          Registered_Participation_Date = registration_time,
          Registered_Total_Reports = n) %>% 
  mutate(Registered_Participation_Date = as.POSIXct(Registered_Participation_Date, format = "%Y-%m-%d %H:%M:%S"),
         Registered_Total_Reports = as.integer(Registered_Total_Reports)) %>%
  replace_na(list(Registered_Total_Reports = 0)) 



# Count of unique users that have submitted at least 1 report
nrow(raw_user_data %>%
    filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1))


 # 59,325 of the registered users have  filled a report

nrow(raw_user_data %>%
    filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1 & 
             Registered_Participation_Date >= as.POSIXct("2020-10-02")))
 
# 44,532 registered users since the cutoff date of 2020-10-02. The different numbers between the unique users here and ones from the 
# reports data set, 45,135 vs 44,532 is due use of different versions.****UPDATE**** found out the difference is due to the user data is 
#older than the reports data and doesn't have users registered in October 2023
#this wont matter since in any case if a user re installs the app they will have a new user Id so its not possible to detect re installations.
#the data set is going to be connected with the reports one to have a grand table based on UUID


#checking the number of users registered after the update.
nrow(raw_user_data %>%
       filter(Registered_Participation_Date >= as.POSIXct("2020-10-02")))
#   264,684 users in total, this number is going to be used as the pool to attach to the main data set. Careful that results are going to be
# changed as the number of users who signed up and not filled a report are going to be left out, while ones who filled a report are going to 
# be found in the reports data set.

user_data <- raw_user_data %>%
  filter(Registered_Participation_Date >= as.POSIXct("2020-10-02"))
