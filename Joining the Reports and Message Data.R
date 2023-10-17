rm(differences)
str(reports_data)
str(test)
str(message_data)
str(UUID_Consent)
str(survey_data)
options(tibble.width = Inf)
options(width = 1000)
View(test)
#checking if all the users that received a message have filled at least one report
count(anti_join(message_data, reports_data, by = c("User_ID" = "User_ID")))
#the count is 0 meaning that all entries who have received a message have filled at least 1 report

#checking if all the users that received a message have filled at least one report
count(anti_join(survey_data, reports_data, by = c("User_ID" = "User_ID")))
#the count is 3 because of three surveys filled without user ID's as this does not help with further report analysis, the three
#entries should be excluded from final analysis in any case.




test <- left_join(reports_data, message_data,  by = "User_ID") 
test <- left_join(test, survey_data,  by = "User_ID") 

#joined the messages data to the reports data set, for now maintained reports from all years, 
#will try to create new variables based on messaging periods
test <- test %>%
  group_by(User_ID) %>%
           mutate(Got_Msgs = !is.na(First_Msg_Date) ) %>%
  ungroup()
                  
                  
                  Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date),
                  Total_Rprts_Filled = n(),
                  Season_Rprts_Filled = sum(Rprt_Date >= "2023-06-01" & Rprt_Date <= "2023-10-15"),
                  Rprts_Filled_2023 = sum(Rprt_Date >= "2023-01-01"),
                  Total_Bite_Rprts_Filled = sum(Rprt_Type == "bite"),
                  Total_Adult_Rprts_Filled = sum(Rprt_Type == "adult"),
                  Total_Site_Rprts_Filled = sum(Rprt_Type == "site")
                    ) %>%
                    ungroup()
                  
                  
                   ) %>%
  ungroup()
                  

test3 <-  test2 %>%
  group_by(User_ID) %>%
  summarize(
    Rprt_Loc_Choice = first(Rprt_Loc_Choice),
    Rprt_Type = first(Rprt_Type),
    Total_Rprts_Filled = first(Total_Rprts_Filled),
    Season_Rprts_Filled = first(Season_Rprts_Filled),
    Rprts_Filled_2023 = first(Rprts_Filled_2023),
    Total_Bite_Rprts_Filled = first(Total_Bite_Rprts_Filled),
    Total_Adult_Rprts_Filled = first(Total_Adult_Rprts_Filled),
    Total_Site_Rprts_Filled = first(Total_Site_Rprts_Filled),
    Msg_Type = first(Msg_Type),
    Repeat_User = first(Repeat_User),
    Msg_Lang = first(Msg_Lang),
    First_Msg_Date = first(First_Msg_Date),
    Last_Msg_Date = first(Last_Msg_Date),
    Nmbr_Msgs_Sent = first(Nmbr_Msgs_Sent),
    Nmbr_Msgs_Seen = first(Nmbr_Msgs_Seen),
    Repeated_Msg_Nmbr = first(Repeated_Msg_Nmbr),
    Got_Msgs = first(Got_Msgs),
    Rprts_During_Msging  = first( Rprts_During_Msging)
  )

ss <- as.data.frame(table(test3$Got_Msgs, test3$Total_Rprts_Filled))


Rprts_Before_Msging = sum(Rprt_Date >= (First_Msg_Date - 45) & Rprt_Date < First_Msg_Date),
                  Rprts_After_Msging = sum(Rprt_Date > Last_Msg_Date & Rprt_Date <= Last_Msg_Date + 45)
                  ) %>%
           ungroup()
         


test2<- test %>% 
  group_by(User_ID) %>%
         mutate(Rprts_Before_Msging = if_else(!is.na(First_Msg_Date), 
                                               sum(Rprt_Date >= (First_Msg_Date - 45) & Rprt_Date < First_Msg_Date, na.rm = TRUE), 0))%>%
  ungroup()
  
  
  
  
  
   
  group_by(User_ID) %>%
  summarize(
    Total_Rprts_Filled = first(Total_Rprts_Filled),
    Season_Rprts_Filled = first(Season_Rprts_Filled),
    Rprts_Filled_2023 = first(Rprts_Filled_2023),
    Total_Bite_Rprts_Filled = first(Total_Bite_Rprts_Filled),
    Total_Adult_Rprts_Filled = first(Total_Adult_Rprts_Filled),
    Total_Site_Rprts_Filled = first(Total_Site_Rprts_Filled),
    Msg_Type = first(type),
    Repeat_User = first(Repeat_User),
    Msg_Lang = first(Msg_Lang),
    First_Msg_Date = as.POSIXct(first(First_Msg_Date)),
    Last_Msg_Date = first(Last_Msg_Date),
    Nmbr_Msgs_Sent = first(Nmbr_Msgs_Sent),
    Nmbr_Msgs_Seen = first(Nmbr_Msgs_Seen),
    Repeated_Msg_Nmbr = first(Repeated_Msg_Nmbr)

    


            