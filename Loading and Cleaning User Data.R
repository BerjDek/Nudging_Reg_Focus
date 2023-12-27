

raw_user_data <- read.csv(file="reports_by_uuid.csv", header = TRUE)


#since the beginning of the project, there have been 354,712 registration/downloads
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


 # 60,375 of the registered users have  filled a report

nrow(raw_user_data %>%
    filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1 & 
             Registered_Participation_Date >= as.POSIXct("2020-10-02")))
 
# 45,582 registered users since the cutoff date of 2020-10-02. There is a difference in the numbers between the unique users here and ones from the 
# reports data set, 45,670 vs 44,582, even though the data was extracted/requested on the same day.

#the number from reports csv is going to be used



#checking the number of users registered after the update.
nrow(raw_user_data %>%
       filter(Registered_Participation_Date >= as.POSIXct("2020-10-02")))
#   270,804 users in total, this number is going to be used as the pool to attach to the main data set. 

user_data <- raw_user_data %>%
  filter(Registered_Participation_Date >= as.POSIXct("2020-10-02"))

write.csv(user_data, "CleanUserData.csv", row.names = FALSE)
