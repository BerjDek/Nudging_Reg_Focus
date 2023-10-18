library(tidyverse)
raw_reports_data <- read.csv(file="all_reports.csv", header = TRUE) %>% 
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d"))


#creation of reports data to stick to  main data set from Survey
#Since 12/09/2020 till the date of the analysis 11,799 reports have been filled.

reports_data <- raw_reports_data %>%
  select(user_id, location_choice, creation_time, type) %>% 
  rename(User_ID = user_id,
         Rprt_Loc_Choice = location_choice,
         Rprt_Date = creation_time,
         Rprt_Type = type) %>% 
  filter(User_ID != "")




#Checking the seasonality of reports filled. #Seems the season usually runs from the first of June til Mid October

reports_data %>%
  filter(year(Rprt_Date) == 2022) %>%
  ggplot(aes(x = Rprt_Date)) +
  geom_bar(stat = "count", fill = "blue") +
  labs(title = "Seasonality of Reports",
       x = "Date",
       y = "Number of Reports") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




reports_data_summary <- reports_data %>%
  group_by(User_ID) %>%
  summarize(total_reports = n(),
            bite_reports = sum(Rprt_Type == "bite", na.rm = TRUE),
            adult_reports = sum(Rprt_Type == "adult", na.rm = TRUE),
            site_reports = sum(Rprt_Type == "site", na.rm = TRUE)) %>% 
  ungroup()
  