library(tidyverse)
raw_reports_data <- read.csv(file="full_reports_table.csv", header = TRUE) %>% 
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d"))

min(raw_reports_data$creation_time, na.rm = TRUE)

#Since the initiation of the Citizen Science project until 19/10/2023 177703 Reports have been filled


#To create a reports data that can be assessed and merged with a main data set 
#The raw data is cleaned to limit it to the final Update on 2nd of October 2020, after which all User Id's have been reset


reports_data <- raw_reports_data %>%
  select(user_id, location_choice, creation_time, type) %>% 
  rename(User_ID = user_id,
         Rprt_Loc_Choice = location_choice,
         Rprt_Date = creation_time,
         Rprt_Type = type) %>% 
  filter(User_ID != "" & Rprt_Date >= as.POSIXct("2020-10-02"))

#Since 12/10/2020 till the date of the analysis 144,832 reports have been filled.


#Reports data is summarized by user id to merge it with a larger data set, additional fields are added to show number and type of reports by Users
# The data set shows that  45,135 unique users have contributed to fill the  144,832 reports Since 12/10/2020
reports_data_summary <- reports_data %>%
  group_by(User_ID) %>%
  summarize(total_reports = n(),
            bite_reports = sum(Rprt_Type == "bite", na.rm = TRUE),
            adult_reports = sum(Rprt_Type == "adult", na.rm = TRUE),
            site_reports = sum(Rprt_Type == "site", na.rm = TRUE)) %>% 
  ungroup()




#Checking the seasonality of reports filled. 

reports_data %>%
  ggplot(aes(x = Rprt_Date)) +
  geom_bar(stat = "count", fill = "blue") +
  labs(title = "Seasonality of Reports",
       x = "Date",
       y = "Number of Reports") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Seems the season usually runs from the first of June til Mid October



  