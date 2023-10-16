library(tidyverse)
raw_reports_data <- read.csv(file="all_reports.csv", header = TRUE) %>% 
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d"))


#creation of reports data to stick to  main data set from Survey

reports_data <- raw_reports_data %>%
  select(user_id, location_choice, creation_time, type) %>% 
  rename(User_ID = user_id,
         Rprt_Loc_Choice = location_choice,
         Rprt_Date = creation_time,
         Rprt_Type = type) %>% 
  filter(User_ID != "")




#Checking the seasonality of reports filled.

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

#adding columns based on earlier graph it seems the season runs from early june til mid of october

reports_data <- reports_data %>%
  group_by(User_ID) %>%
  mutate(
    Total_Rprts_Filled = n(),
    Season_Rprts_Filled = sum(Rprt_Date >= "2023-06-01" & Rprt_Date <= "2023-10-15"),
    Rprts_Filled_2023 = sum(Rprt_Date >= "2023-01-01"),
    Total_Bite_Rprts_Filled = sum(Rprt_Type == "bite"),
    Total_Adult_Rprts_Filled = sum(Rprt_Type == "adult"),
    Total_Site_Rprts_Filled = sum(Rprt_Type == "site")
  ) %>%
  ungroup()
  