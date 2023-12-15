reports_2023 <- reports_data %>%
  filter(year(as.Date(Rprt_Date)) == 2023) %>%
  mutate(Week_Number = week(as.Date(Rprt_Date)))

rep_msg_2023 <- reports_2023 %>% 
  filter(User_ID %in% recieved_msgs$User_ID) %>% 
  select(-Week_Number,-Rprt_Loc_Choice) %>% 
  mutate(Rprt_Type = as.character(Rprt_Type)) %>% 
  rename(Date = Rprt_Date)
str(rep_msg_2023)
write.csv(rep_msg_2023, "time_rprt.csv", row.names = FALSE)

msg_rep_2023<- raw_message_data %>%
  filter(year(Msg_Date) == 2023) %>% 
  select(-Msg_Lang,-msg_nmbr,-Repeat_User,-id) %>% 
  mutate(read_notification= as.integer(read_notification == "t"), Msg_Date = as.Date(Msg_Date)) %>% 
  select(User_ID,Msg_Date, type, read_notification, Msg_Nmbr) %>% 
  rename(Msg_Type = type, Msg_Seen = read_notification, Date = Msg_Date)
str(msg_rep_2023)
write.csv(msg_rep_2023, "time_msg.csv", row.names = FALSE)



dates_2023 <- seq.Date(as.Date("2023-01-01"), as.Date("2023-11-25"), by = "day")

expanded_reports <- expand.grid(User_ID = unique(rep_msg_2023$User_ID), Date = dates_2023)
expanded_messages <- expand.grid(User_ID = unique(msg_rep_2023$User_ID), Date = dates_2023)

full_reports <- merge(expanded_reports, rep_msg_2023, by = c("User_ID", "Date"), all = TRUE)
full_messages <- merge(expanded_messages, msg_rep_2023, by = c("User_ID", "Date"), all = TRUE)


full_reports$Rprt_Type[is.na(full_reports$Rprt_Type)] <- "None"
full_messages$Msg_Seen[is.na(full_messages$Msg_Seen)] <- 0
full_messages$Msg_Type[is.na(full_messages$Msg_Type)] <- "None"
full_messages$Msg_Nmbr[is.na(full_messages$Msg_Nmbr)] <- 0

report_msg_long <- left_join(full_reports, full_messages, by = c("User_ID", "Date"))

rm(full_messages,full_reports,dates_2023)


report_msg_long <- report_msg_long %>%
  mutate(Rprt_Filled = ifelse(Rprt_Type != "None", 1, 0), Msg_Received = ifelse(Msg_Type != "None", 1, 0) )

report_msg_long$Rprt_Type <- as.factor(report_msg_long$Rprt_Type)
report_msg_long$Msg_Type <- as.factor(report_msg_long$Msg_Type)

write.csv(report_msg_long, "time_series_data.csv", row.names = FALSE)



#doing a Chi test to check if users are more likely to fill a report on the day of receiving a message


contingency_table <- table(report_msg_long$Msg_Received, report_msg_long$Rprt_Filled)

chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)

rm(contingency_table,chi_test_result)
#measuring how much the observed frequencies (the number of days with/without messages and reports) deviate from the 
#expected frequencies under the null hypothesis (no association between receiving messages and filling reports).  The p-value is < 2.2e-16
# clearly showing a statistically significant association between a user receiving a message and the likelihood of a user filling a report.



#checking it for the season


dates_2023 <- seq.Date(as.Date("2023-05-01"), as.Date("2023-10-30"), by = "day")

rep_msg_2023 <- rep_msg_2023 %>% 
  filter(Date >= "2023-05-01"& Date <= "2023-10-30")

msg_rep_2023<- msg_rep_2023 %>% 
  filter(Date >= "2023-05-01"& Date <= "2023-10-30")
  

expanded_reports <- expand.grid(User_ID = unique(rep_msg_2023$User_ID), Date = dates_2023)
expanded_messages <- expand.grid(User_ID = unique(msg_rep_2023$User_ID), Date = dates_2023)

full_reports <- merge(expanded_reports, rep_msg_2023, by = c("User_ID", "Date"), all = TRUE)
full_messages <- merge(expanded_messages, msg_rep_2023, by = c("User_ID", "Date"), all = TRUE)


full_reports$Rprt_Type[is.na(full_reports$Rprt_Type)] <- "None"
full_messages$Msg_Seen[is.na(full_messages$Msg_Seen)] <- 0
full_messages$Msg_Type[is.na(full_messages$Msg_Type)] <- "None"
full_messages$Msg_Nmbr[is.na(full_messages$Msg_Nmbr)] <- 0

report_msg_long <- left_join(full_reports, full_messages, by = c("User_ID", "Date"))

rm(full_messages,full_reports,dates_2023)

report_msg_long <- report_msg_long %>%
  mutate(Rprt_Filled = ifelse(Rprt_Type != "None", 1, 0), Msg_Received = ifelse(Msg_Type != "None", 1, 0) )

report_msg_long$Rprt_Type <- as.factor(report_msg_long$Rprt_Type)
report_msg_long$Msg_Type <- as.factor(report_msg_long$Msg_Type)


contingency_table <- table(report_msg_long$Msg_Received,report_msg_long$Rprt_Filled)

chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)

# when checking the season we get  a p value = 0.001599

rm(contingency_table,chi_test_result)



#doing a regression analysis


#do one for long data here


#one for shorter data where each user has 1 date

report_msg_wide <- report_msg_long %>%
  mutate(report_indicator = as.integer(1)) %>%
  group_by(User_ID, Date, Rprt_Type) %>%
  summarize(report_indicator = sum(report_indicator, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Rprt_Type, 
    values_from = report_indicator, 
    values_fill = list(report_indicator = 0)  
  ) %>% 
  mutate(
    total_reports = adult + bite + site,
    Report = as.numeric(!None)) 

%>%
  filter(!User_ID %in% c("24ee7efd-a288-4f39-b02d-41e09e4c9ce9" "79918bb8-0fd5-430b-9de9-7553f180097e" "86c14172-8723-48a5-8af3-580607998c64"
                         "92d7a185-99e8-44ae-8841-9e993bab9c32" "f96a9713-4ffc-442d-b4b4-e402217f12b5")) 

,"fa099b57-da20-4e98-ab79-0c2615cac14e","4437b614-ec38-401b-9a0e-26c937933788"

msg_subset <- report_msg_long %>%
  group_by(User_ID, Date) %>%
  slice(1) %>%
  dplyr::select(User_ID, Date, Msg_Received, Msg_Type, Msg_Seen) %>%
  ungroup()

report_msg_wide <- report_msg_wide %>%
  left_join(msg_subset, by = c("User_ID", "Date"))
rm(msg_subset)

report_msg_wide <- report_msg_wide %>% 
  left_join(survey_data %>% dplyr::select(User_ID, Gender, Age_Group, Reg_Orientation_Cat), by = "User_ID")


report_msg_wide <- report_msg_wide %>%  
  filter(!User_ID %in% c("0eaa6fd0-99cd-4f57-aafb-f28b6d640ac8", "24ee7efd-a288-4f39-b02d-41e09e4c9ce9", "375404ad-a343-4f6d-8833-2dc665fc2714",
                         "76535311-fd93-4804-aff4-4cd0cc3122e9", "79918bb8-0fd5-430b-9de9-7553f180097e", "92d7a185-99e8-44ae-8841-9e993bab9c32",
                         "bf0a0b12-592a-43b5-8a8f-dbabafe628df", "c9faebc9-c286-4aef-ab99-65b2000a9ee1", "d44c8f5a-c314-44ac-a149-65d7fc9c3f0a",
                         "dbbf371f-ae92-4855-a690-f4c87190ab8a", "f96a9713-4ffc-442d-b4b4-e402217f12b5")) 

rm(msg_subset)


head(report_msg_wide)

#chi square with wide data
contingency_table <- table(report_msg_wide$Msg_Received,report_msg_wide$Report)
contingency_table
chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)

#all sec:   significant p-value = 0.004599
#outliers   significant p-value = 0.01134
#outliers 

#significant result p-value = 0.004599, with outlines taken out it shrinks to 0.01236. two extreme users taken out 0.01134 
#for the whole year it is much larger p-value < 2.2e-16



# Generalized Linear Model for the relationship between a report being filled and message being received by a user on a given day

model <- glmer(Report ~ Msg_Received + (1 | User_ID), 
               data = report_msg_wide, 
               family = binomial)
summary(model)


model <- glmer(Report ~ Msg_Received + (1 + Date|User_ID),
               data = report_msg_wide,
               family = binomial)
summary(model) #even more significant than before


#all sec:  significant p = 0.00418
#outliers  significant p = 0.0129 
#outliers 


# for  the season there is a significant relationship of messages to filling reports at p = 0.00415, shrinks after removing outliers to 0.0111,
#after moving additional 2 extreme users 0.00854
# for whole year way higher <2e-16 ***

# Logistic Regression for the impact of message type on report filing

report_msg_wide$Msg_Type <- relevel(report_msg_wide$Msg_Type, ref = "None")

model <- glm(Report ~ Msg_Type, family = binomial(link = "logit"), data = report_msg_wide)
summary(model)


model <- glmer(Report ~ Msg_Type + (1 | User_ID), 
               data = report_msg_wide, 
               family = binomial(link = "logit"))
summary(model)



model <- glmer(Report ~ Msg_Type + (1 + Date|User_ID), 
               data = report_msg_wide, 
               family = binomial(link = "logit"))
summary(model)# both prevention and promotion better off than neutral


#all sec:  promotion and prevention significant, p value: prev 0.01989  pro 0.00336  neutral negative not significant
#outliers: only prevention significant,p value: prev  9.79e-05 
#outliers 

#when checking the type of messages promotion and prevention messages have been deemed significantly effective with promotion having better p.
# neutral messages caused negative effect but not significant. p value: prev 0.01989  pro 0.00336
#taking out outliers, neutral is positive again, promotion not significant, prevention very significant at 0.000189
#after taking out additionally two extreme users 9.79e-05

#when checking all year everything looks positive, the highest impact being that of prevention



#checking user regulatory orientation
model <- glm(Report ~ Reg_Orientation_Cat, family = binomial(link = "logit"), data = report_msg_wide)
summary(model)

model <- glmer(Report ~ Reg_Orientation_Cat + (1 | User_ID), 
               data = report_msg_wide, 
               family = binomial(link = "logit"))
summary(model)



model <- glmer(Report ~ Msg_Type*Reg_Orientation_Cat + (1 + Date|User_ID), 
               data = report_msg_wide, 
               family = binomial(link = "logit"))
summary(model) #not Significant



#all sec:  prevention and promotion significant, prev higher p prev:2.67e-05 pro 0.00748
#outliers; prev oriented significant 0.0358
#outliers +2


#checking regulatory focus and type of message interaction

model <- glmer(Report ~ Reg_Orientation_Cat + (1 + Date|User_ID), 
               data = report_msg_wide, 
               family = binomial(link = "logit"))
summary(model)



# checking the effect of messages on the total number of reports filed. reporting intensity

model <- glm(total_reports ~ Msg_Received, family = poisson(link = "log"), data = report_msg_wide)
summary(model)


model <- glmer(total_reports ~ Msg_Received + (1 | User_ID), 
               data = report_msg_wide, 
               family = poisson)
summary(model)


model <- glmer(total_reports ~ Msg_Received + (1 + Date|User_ID), 
               data = report_msg_wide, 
               family = poisson) 
summary(model)  #significant


#all sec:  significant value p =0.00176
#outliers  highly significant p = 4.42e-05
#outliers + 2

#for the season a significant value p =0.00176, when outliers removed  more significant at  0.000188
#after taking out additionally two extreme users 4.42e-05
#for the whole year very high p value



library(MASS)
model <- glm.nb(total_reports ~ Msg_Received, data = report_msg_wide)
summary(model)


install.packages("glmmTMB")
library(glmmTMB)
model <- glmmTMB(total_reports ~ Msg_Received + (1 | User_ID),  ## did not work
                 data = report_msg_wide, 
                 family = nbinom2)
summary(model)
#all sec:   not significant
#outliers  significant  p = 0.0253 
#outliers 

#for the season, binomial modal isn't significant p = 0.121, outliers removed it becomes significant p =  0.0412 
#after taking out additionally two extreme users 0.0253  
#for whole year even negative binomial is significant