rm(daily_reports_2021, reports_2021)
str(reports_data)
str(test)
str(message_data)

test <- left_join(reports_data, message_data,  by = "User_ID") 
rm(test)

#joined the messages data to the reports data set, for now maintained reports from all years, 
#will try to create new variables based on messaging periods
test2 <- test %>%
  group_by(User_ID) %>%
           mutate(Got_Msgs = !is.na(First_Msg_Date),
                  Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date),
                  Rprts_Before_Msging = sum(Rprt_Date >= (First_Msg_Date - 45) & Rprt_Date < First_Msg_Date),
                  Rprts_After_Msging = sum(Rprt_Date > Last_Msg_Date & Rprt_Date <= Last_Msg_Date + 45)
                  ) %>%
           ungroup()
         
          )
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

    
    as.POSIXct()

test2 <- test %>%
  mutate(Got_Msgs = !is.na(First_Msg_Date))
   

test2 <- test2 %>%  
  Rprts_During_Msging = sapply(First_Msg_Date, function(start_date) {
    sum(Rprt_Date >= start_date & Rprt_Date <= Last_Msg_Date)
            