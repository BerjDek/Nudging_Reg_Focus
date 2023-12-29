#Time Series Final

rep_msg_2023 <- reports_data %>% 
  filter(User_ID %in% received_msgs$User_ID) %>% 
  filter(year(as.Date(Rprt_Date)) == 2023) %>%
  mutate(Rprt_Type = as.character(Rprt_Type)) %>% 
  rename(Date = Rprt_Date) %>% 
  filter(Date >= "2023-05-01"& Date <= "2023-10-31")


msg_rep_2023<- raw_message_data %>%
  filter(year(Msg_Date) == 2023) %>% 
  dplyr::select(-Msg_Lang,-msg_nmbr,-Repeat_User,-id) %>% 
  mutate(read_notification= as.integer(read_notification == "t"), Msg_Date = as.Date(Msg_Date)) %>% 
  dplyr::select(User_ID, Msg_Date, type, read_notification, Msg_Nmbr) %>% 
  rename(Msg_Type = type, Msg_Seen = read_notification, Date = Msg_Date) %>% 
filter(Date >= "2023-05-01"& Date <= "2023-10-31")



dates_2023  <- seq.Date(as.Date("2023-05-01"), as.Date("2023-10-31"), by = "day")

expanded_reports <- expand.grid(User_ID = unique(rep_msg_2023$User_ID), Date = dates_2023)
expanded_messages <- expand.grid(User_ID = unique(msg_rep_2023$User_ID), Date = dates_2023)

full_reports <- merge(expanded_reports, rep_msg_2023, by = c("User_ID", "Date"), all = TRUE)
full_messages <- merge(expanded_messages, msg_rep_2023, by = c("User_ID", "Date"), all = TRUE)

full_reports$Rprt_Type[is.na(full_reports$Rprt_Type)] <- "None"
full_messages$Msg_Seen[is.na(full_messages$Msg_Seen)] <- 0
full_messages$Msg_Type[is.na(full_messages$Msg_Type)] <- "None"
full_messages$Msg_Nmbr[is.na(full_messages$Msg_Nmbr)] <- 0

report_msg_long <- left_join(full_reports, full_messages, by = c("User_ID", "Date"))

report_msg_long <- report_msg_long %>%
  mutate(Rprt_Filled = ifelse(Rprt_Type != "None", 1, 0), Msg_Received = ifelse(Msg_Type != "None", 1, 0))

report_msg_long$Rprt_Type <- as.factor(report_msg_long$Rprt_Type)
report_msg_long$Msg_Type <- as.factor(report_msg_long$Msg_Type)


write.csv(report_msg_long, "reportmsglong.csv", row.names = FALSE)

rm(full_messages, full_reports, expanded_messages,expanded_reports, dates_2023, msg_rep_2023, rep_msg_2023)

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
    Report = as.numeric(!None)) %>% 
  dplyr::select(-None)


msg_subset <- report_msg_long %>%
  group_by(User_ID, Date) %>%
  slice(1) %>%
  dplyr::select(User_ID, Date, Msg_Received, Msg_Type, Msg_Seen) %>%
  ungroup()

report_msg_wide <- report_msg_wide %>%
  left_join(msg_subset, by = c("User_ID", "Date"))

report_msg_wide <- report_msg_wide %>% 
  left_join(survey_data %>% dplyr::select(User_ID, Gender, Age_Group, Reg_Orientation_Cat), by = "User_ID") %>% 
  filter(!is.na(Msg_Type))

rm(msg_subset)

report_msg_wide <- report_msg_wide %>%
  mutate(Orientation_Msg_Agreement = case_when(
    Msg_Type == "Neutral" & Reg_Orientation_Cat == "Neutral" ~ 1,
    Msg_Type == "Promotion" & Reg_Orientation_Cat == "Promotion" ~ 1,
    Msg_Type == "Prevention" & Reg_Orientation_Cat == "Prevention" ~ 1,
    TRUE ~ 0
  ))

str(report_msg_wide)

write.csv(report_msg_wide, "reportmsgwide.csv", row.names = FALSE)

#chi square with wide data
contingency_table <- table(report_msg_wide$Msg_Received,report_msg_wide$Report)
contingency_table
chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)


# Generalized Linear Model for the relationship between a report being filled and message being received by a user on a given day


model <- glmer(Report ~ Msg_Received + (1 + Date|User_ID),
               data = report_msg_wide,
               family = binomial)
summary(model)


# checking the effect of messages on the total number of reports filed. reporting intensity

model <- glmer(total_reports ~ Msg_Received + (1 + Date|User_ID), 
               data = report_msg_wide, 
               family = poisson) 
summary(model)



# Logistic Regression for the impact of message type on report filing
report_msg_wide$Msg_Type <- relevel(report_msg_wide$Msg_Type, ref = "None")


model <- glmer(Report ~ Msg_Type + (1 + Date|User_ID), 
               data = report_msg_wide, 
               family = binomial(link = "logit"))
summary(model)
#both prevention and promotion are significant


#checking intensity
model <- glmer(total_reports ~ Msg_Type + (1 + Date|User_ID), 
               data = report_msg_wide, 
               family = poisson) 
summary(model)



#checking the importance of message orientation agreement

contingency_table <- table(report_msg_wide[report_msg_wide$Msg_Received == 1, ]$Orientation_Msg_Agreement,report_msg_wide[report_msg_wide$Msg_Received == 1, ]$Report)
contingency_table
chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)

table(report_msg_wide[report_msg_wide$Msg_Received == 1, ]$Orientation_Msg_Agreement,report_msg_wide[report_msg_wide$Msg_Received == 1, ]$Report)


#checking agreement of message for each type of orientation


model <- glmer(Report ~ Orientation_Msg_Agreement + (1 + Date|User_ID), 
               data = report_msg_wide[report_msg_wide$Reg_Orientation_Cat == "Prevention", ], 
               family = binomial(link = "logit"))
summary(model)


model <- glmer(Report ~ Orientation_Msg_Agreement + (1 + Date|User_ID), 
               data = report_msg_wide[report_msg_wide$Reg_Orientation_Cat == "Promotion", ], 
               family = binomial(link = "logit"))
summary(model)

#it matters more for prevention oriented to receive prevention oriented messages




#The whole thing with reporters over 90 percentile removed.



#Testing while removing to 10 percent of outliers
report_msg_wide_reduced <- report_msg_wide %>% 
  filter(!User_ID %in% c("24ee7efd-a288-4f39-b02d-41e09e4c9ce9", "92d7a185-99e8-44ae-8841-9e993bab9c32", "d44c8f5a-c314-44ac-a149-65d7fc9c3f0a",
         "f96a9713-4ffc-442d-b4b4-e402217f12b5"))




#chi square with wide data
contingency_table <- table(report_msg_wide_reduced$Msg_Received,report_msg_wide_reduced$Report)
contingency_table
chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)


# Generalized Linear Model for the relationship between a report being filled and message being received by a user on a given day


model <- glmer(Report ~ Msg_Received + (1 + Date|User_ID),
               data = report_msg_wide_reduced,
               family = binomial)
summary(model) #significant but less so


# checking the effect of messages on the total number of reports filed. reporting intensity


model <- glmer(total_reports ~ Msg_Received + (1 + Date|User_ID), 
               data = report_msg_wide_reduced, 
               family = poisson) 
summary(model)# very significant

#negative Binomial Model because of over dispersion (variance in the count data is larger than the mean)
#model <- glm.nb(total_reports ~ Msg_Received, data = report_msg_wide_reduced)
#summary(model)
#SIGNIFICANT

#checking the msg type

report_msg_wide_reduced$Msg_Type <- relevel(report_msg_wide_reduced$Msg_Type, ref = "None")


model <- glmer(Report ~ Msg_Type + (1 + Date|User_ID), 
               data = report_msg_wide_reduced, 
               family = binomial(link = "logit"))
summary(model)
#both prevention and promotion are significant




#checking agreement of message for each type of orientation


model <- glmer(Report ~ Orientation_Msg_Agreement + (1 + Date|User_ID), 
               data = report_msg_wide_reduced[report_msg_wide_reduced$Reg_Orientation_Cat == "Prevention", ], 
               family = binomial(link = "logit"))
summary(model) #significant agreement for prevention messages


model <- glmer(Report ~ Orientation_Msg_Agreement + (1 + Date|User_ID), 
               data = report_msg_wide_reduced[report_msg_wide_reduced$Reg_Orientation_Cat == "Promotion", ], 
               family = binomial(link = "logit"))
summary(model)


#checking impact of  user regulatory orientation

model <- glmer(Report ~ Reg_Orientation_Cat + (1 | User_ID), 
               data = report_msg_wide_reduced, 
               family = binomial(link = "logit"))
summary(model)#no significant effect for promotion







