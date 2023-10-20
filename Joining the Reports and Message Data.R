rm(Data_Tall, data_Tall, model,  test,test2,test1)
str(reports_data)
str(data)
str(message_data)
str(UUID_Consent)
str(survey_data)
options(tibble.width = Inf)
options(width = 1000)
View(test)

colnames(survey_data)
#checking if all the users that received a message have filled at least one report
count(anti_join(message_data, reports_data, by = c("User_ID" = "User_ID")))
#the count is 0 meaning that all entries who have received a message have filled at least 1 report

#checking if all the users that received a message have filled at least one report
count(anti_join(survey_data, reports_data, by = c("User_ID" = "User_ID")))
#the count is 3 because of three surveys filled without user ID's as this does not help with further report analysis, the three
#entries should be excluded from final analysis in any case.



#check if all the users in   reports_data are in  data_tall
all(reports_data$User_ID %in% data_tall$User_ID)
#####IMPORTANT, you discovered that data tall did not take all the users from reports data.
#on Monday change the way you merge the data sets, and check if all the reports are included in data set.

#DECIDE if you are going to keep all reports, or clean by version level till one after the update. if you decide to cut by version, check date of first report.
#merge user to report, to message to survey.



#Selecting relevant columns from Message and survey reports to join with report data set to create one large data set to analyse

message_data_join <- message_data %>% 
  select(User_ID, Msg_Type, Repeat_User, First_Msg_Date, 
         Last_Msg_Date,Nmbr_Msgs_Sent, Nmbr_Msgs_Seen)

survey_data_join <- survey_data %>% 
  select(User_ID, Complt_Survey,Age, Age_Group, Gender, Country, Participation_Date,
         Network, Other_Citi_Sci, Reg_Orientation, Openness_To_Change,
         Self_Enhancement, Continuity, Self_Transcendence, Security,
         Teaching, Self_Direction, Stimulation, Hedonism, Achievement,
         Face, Conformity, Benevolence, Universalism_Social, Universalism_Nature,
         Routine, Social_Expansion, Power, Help_Science, Dislike, Env_Change)


data_tall <- left_join(reports_data, message_data_join,  by = "User_ID") 
data_tall <- left_join(data_tall, survey_data_join,  by = "User_ID") 



rm(message_data_join, survey_data_join)





#joined the messages data to the reports data set, for now maintained reports from all years, 
#will try to create new variables based on messaging periods

data_tall <- data_tall %>%
  group_by(User_ID) %>%
           mutate(Got_Msgs = !is.na(First_Msg_Date),
                  Total_Rprts_Filled = n(),
                  Season_Rprts_Filled = sum(Rprt_Date >= "2023-06-01" & Rprt_Date <= "2023-10-15"),
                  Rprts_Filled_2023 = sum(Rprt_Date >= "2023-01-01"),
                  Total_Bite_Rprts_Filled = sum(Rprt_Type == "bite"),
                  Total_Adult_Rprts_Filled = sum(Rprt_Type == "adult"),
                  Total_Site_Rprts_Filled = sum(Rprt_Type == "site"),
                  Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date, na.rm = TRUE),
                  Rprts_Before_Msging = sum(Rprt_Date >= (First_Msg_Date - days(45)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
                  Rprts_After_Msging = sum(Rprt_Date > Last_Msg_Date & Rprt_Date <= (Last_Msg_Date + days(45)), na.rm = TRUE)) %>%
           mutate(Msg_Type = ifelse(is.na(Msg_Type), "Non", Msg_Type),
                  Repeat_User  = ifelse(is.na(Repeat_User), FALSE, Repeat_User),
                  Nmbr_Msgs_Sent = ifelse(is.na(Nmbr_Msgs_Sent), 0, Nmbr_Msgs_Sent),
                  Nmbr_Msgs_Seen = ifelse(is.na(Nmbr_Msgs_Seen), 0, Nmbr_Msgs_Seen)) %>% 
           ungroup()






data <- data_tall %>%
  group_by(User_ID) %>%
    mutate(Rprt_Loc_Choice = names(which.max(table(Rprt_Loc_Choice)))) %>% # Find the most frequent Rprt_Loc_Choice
      arrange(User_ID, Rprt_Date) %>% # Arrange by date to ensure first occurrence is taken
        select(-Rprt_Date, -Rprt_Type) %>% # Remove unwanted columns
          slice(1L) %>% # Take the first occurrence for each user
  ungroup() %>%
  mutate(Rprt_Loc_Choice = as.factor(Rprt_Loc_Choice))



#minor adjustments of columns  

data$Got_Msgs <- as.factor(data$Got_Msgs)
data_tall$                





model <- lm(Season_Rprts_Filled ~ Got_Msgs + Msg_Type + Nmbr_Msgs_Seen, data = test2)                 
summary(model)                 
                  
                  
   
    


            