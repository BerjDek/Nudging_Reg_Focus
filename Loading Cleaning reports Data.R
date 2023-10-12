library(tidyverse)
raw_reports_data <- read.csv(file="all_reports.csv", header = TRUE) %>% 
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d"))


#creation of reports data to stick to  main data set from Survey

reports_data <- raw_reports_data %>%
  select(user_id, location_choice, creation_time, type) %>% 
  rename(User_ID = user_id,
         Rprt_Loc_Choice = location_choice,
         Rprt_Date = creation_time,
         Rprt_Type = type)



reports_data <- reports_data %>%
  group_by(User_ID) %>%
  mutate(
    Total_Rprts_Filled = n(),
    Season_Rprts_Filled = sum(Rprt_Date >= "2023-06-01" & Rprt_Date <= "2023-09-30"),
    Total_Bite_Rprts_Filled = sum(Rprt_Type == "bite"),
    Total_Adult_Rprts_Filled = sum(Rprt_Type == "adult"),
    Total_Site_Rprts_Filled = sum(Rprt_Type == "site")
  ) %>%
  ungroup()
