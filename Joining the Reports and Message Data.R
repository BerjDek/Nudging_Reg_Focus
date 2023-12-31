

#checking if all user_id's mentioned in  survey_data are mentioned in message_data.
all(survey_data$User_ID %in% message_data$User_ID)


#Joining survey data and message data based on user id
data_tall <- full_join(survey_data, message_data, by = "User_ID")

#checking if all user_id's mentioned in  message_data are mentioned in reports_data. Yes all are there
all(message_data$User_ID %in% reports_data$User_ID)


#Joining data by reports on user id
data_tall <- full_join(data_tall, reports_data, by = "User_ID")




#creating necessary columns
data_tall <- data_tall %>%
  group_by(User_ID) %>%
  mutate(Got_Msgs = !is.na(First_Msg_Date),
         Total_Rprts_Filled = n(),
         Season_Rprts_Filled_2023 = sum(Rprt_Date >= "2023-05-01" & Rprt_Date <= "2023-10-30"),
         Season_Rprts_Filled_2022 = sum(Rprt_Date >= "2022-05-01" & Rprt_Date <= "2022-10-30"),
         Season_Rprts_Filled_2021 = sum(Rprt_Date >= "2021-05-01" & Rprt_Date <= "2021-10-30"),
         Rprts_Filled_2023 = sum(Rprt_Date >= "2023-01-01"),
         Rprts_Filled_2022 = sum(Rprt_Date >= "2022-01-01" & Rprt_Date <= "2022-12-31"),
         Rprts_Filled_2021 = sum(Rprt_Date >= "2021-01-01" & Rprt_Date <= "2021-12-31"),
         Total_Rprts_Segment = cut(Total_Rprts_Filled, breaks = c(-1, 0, 1, 10, 50, Inf),
                                   labels = c("0 reports", "1 report", "2-10 reports", "11-50 reports", "50+ reports"),right = TRUE),
         Total_Bite_Rprts_Filled = sum(Rprt_Type == "bite"),
         Total_Adult_Rprts_Filled = sum(Rprt_Type == "adult"),
         Total_Site_Rprts_Filled = sum(Rprt_Type == "site"),
         Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date, na.rm = TRUE),
         Rprts_Before_Msging = sum(Rprt_Date >= (First_Msg_Date - days(Msg_Duration_Days)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
         Rprts_After_Msging = sum(Rprt_Date > Last_Msg_Date & Rprt_Date <= (Last_Msg_Date + days(Msg_Duration_Days)), na.rm = TRUE)) %>% 
  ungroup()




data_tall <- data_tall %>% 
  dplyr::select(User_ID, Rprt_Date, Rprt_Type, Got_Msgs, Complt_Survey, Total_Rprts_Filled, Age, Age_Group, 
         Gender, Country, Participation_Date, Msg_Type, Reg_Orientation,Reg_Orientation_Cat, Network, Other_Citi_Sci,  Openness_To_Change,
         Self_Enhancement, Continuity, Self_Transcendence, Security, Teaching, Message_Group, First_Msg_Date, Last_Msg_Date, Nmbr_Msgs_Sent, 
         Nmbr_Msgs_Seen, Rprts_Filled_2021,Rprts_Filled_2022, Rprts_Filled_2023, Total_Rprts_Segment,Season_Rprts_Filled_2023, 
         Season_Rprts_Filled_2022,Season_Rprts_Filled_2021, Total_Bite_Rprts_Filled, Total_Adult_Rprts_Filled, Total_Site_Rprts_Filled,
         Rprts_During_Msging, Rprts_Before_Msging, Rprts_After_Msging)
         

str(data_tall)

#summarizing  bu user ID to combine with user id data set of all registered users



data <- data_tall %>%
  group_by(User_ID) %>%
  arrange(User_ID, Rprt_Date) %>% # Arrange by date to ensure first occurrence is taken
  dplyr::select(-Rprt_Date, -Rprt_Type) %>% # Remove unwanted columns
  slice(1L) %>% # Take the first occurrence for each user
  ungroup() 

 
#joining data with user data to have a full count of users that report or don't
data <- full_join(data, user_data, by = "User_ID")


# Replaced NAs with either FALSE, Non or 0 in specific columns, and then  reordered data set for clarity


data <- data %>%
  mutate(across(c(Got_Msgs, Complt_Survey), ~replace_na(.x, FALSE))) %>%
  mutate(across(c(Total_Rprts_Filled, Nmbr_Msgs_Sent, Nmbr_Msgs_Seen,Rprts_Filled_2021, Rprts_Filled_2022, Rprts_Filled_2023, 
                  Season_Rprts_Filled_2023, Season_Rprts_Filled_2022, Season_Rprts_Filled_2021,
                  Total_Bite_Rprts_Filled, Total_Adult_Rprts_Filled, Total_Site_Rprts_Filled,
                  Rprts_During_Msging, Rprts_Before_Msging, Rprts_After_Msging), ~replace_na(.x, 0))) %>%
  mutate(Msg_Type = forcats::fct_expand(Msg_Type, "None")) %>% 
  mutate(Msg_Type = tidyr::replace_na(Msg_Type, "None"))  
 
  
data <- data %>% 
  dplyr::select(User_ID, Got_Msgs, Complt_Survey, Total_Rprts_Filled, Registered_Total_Reports, 
         Age , Age_Group, Gender, Country, Participation_Date, Registered_Participation_Date, everything())  
  
str(data)       
write.csv(data, "loaddata.csv", row.names = FALSE)


survey_completed <- data %>% filter(Complt_Survey == TRUE)
write.csv(survey_completed, "sureycompdata.csv", row.names = FALSE)

received_msgs <- data %>% filter(Got_Msgs == TRUE)
write.csv(received_msgs, "recmsgdata.csv", row.names = FALSE)


str(survey_completed)

#IN THE FINAL ITIRATION THESE NEW VARIABLES WERE NOT CREATED


