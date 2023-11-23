mean(recieved_msgs$Rprts_Filled_2022)
mean(recieved_msgs$Rprts_Filled_2023)


recieved_msgs %>% 
  filter(Registered_Participation_Date <= "2022-12-31") 

recieved_msgs %>%
  filter(Registered_Participation_Date <= "2022-12-31") %>%
  summarize(Average_Reports_Filled = mean(Rprts_Filled_2022, na.rm = TRUE)) %>%
  pull(Average_Reports_Filled)

recieved_msgs %>%
  filter(Registered_Participation_Date <= "2022-12-31") %>%
  summarize(Average_Reports_Filled = mean(Season_Rprts_Filled_2022, na.rm = TRUE)) %>%
  pull(Average_Reports_Filled)


recieved_msgs %>%
  filter(Registered_Participation_Date <= "2022-12-31") %>%
  summarize(Average_Reports_Filled = mean(Season_Rprts_Filled, na.rm = TRUE)) %>%
  pull(Average_Reports_Filled)


reports_data$Rprt_Date <- as.Date(reports_data$Rprt_Date)
ggplot(reports_data %>% 
         filter(year(Rprt_Date) == 2023) %>% 
         group_by(Week = format(Rprt_Date, "%Y-%U")) %>% 
         summarize(Reports = n()), aes(x = Week, y = Reports)) +
  geom_line() + # Line plot
  labs(title = "Number of Reports per Week in 2023",
       x = "Week of the Year",
       y = "Number of Reports") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



x <- reports_data %>% 
  filter(year(Rprt_Date) == 2023) %>% 
  group_by(Week = format(Rprt_Date, "%Y-%U")) %>% 
  summarize(Reports = n())

write.csv(reports_data,"Clean_Reports_Data.csv")


ggplot(data = reports_data %>%
         mutate(Rprt_Date = as.Date(Rprt_Date)) %>%
         filter(year(Rprt_Date) >= 1900 & year(Rprt_Date) <= 2262) %>%
         count(Month = floor_date(Rprt_Date, "month"))) +
  geom_bar(aes(x = Month, y = n), stat = "identity", fill = "skyblue") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "Monthly Report Counts",
       x = "Month",
       y = "Number of Reports") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))














x <- full_join(survey_data, message_data, by = "User_ID")
x <- full_join(x, reports_data, by = "User_ID")
x <- x %>%
  group_by(User_ID) %>%
mutate(Got_Msgs = !is.na(First_Msg_Date),Total_Rprts_Filled = n(),
Season_Rprts_Filled = sum(Rprt_Date >= "2023-05-01" & Rprt_Date <= "2023-10-30"),
Season_Rprts_Filled_2022 = sum(Rprt_Date >= "2022-05-01" & Rprt_Date <= "2022-10-30"),
Season_Rprts_Filled_2021 = sum(Rprt_Date >= "2021-05-01" & Rprt_Date <= "2021-10-30")) %>% 
  ungroup()

xx <- x %>% 
  select(User_ID, Rprt_Date, Got_Msgs,Complt_Survey,  Season_Rprts_Filled, Season_Rprts_Filled_2022, Season_Rprts_Filled_2021,)

xxx <- xx %>%
  group_by(User_ID) %>%
  arrange(User_ID, Rprt_Date) %>% # Arrange by date to ensure first occurrence is taken
  select(-Rprt_Date) %>% # Remove unwanted columns
  slice(1L) %>% # Take the first occurrence for each user
  ungroup() 

xxx <- full_join(xxx, user_data, by = "User_ID")


r <- xxx %>%
  filter(Got_Msgs == "TRUE" &  Registered_Participation_Date < as.Date('2023-01-01'))

paired_t_test_result <- with(r, t.test(Season_Rprts_Filled, Season_Rprts_Filled_2022,  paired = TRUE))
print(paired_t_test_result)

t <- xxx %>%
  filter( Got_Msgs == "TRUE" &  Registered_Participation_Date < as.Date('2022-01-01'))

paired_t_test_result <- with(t, t.test(Season_Rprts_Filled_2022, Season_Rprts_Filled_2021,  paired = TRUE))
print(paired_t_test_result)



reports_2023 <- reports_data %>%
  filter(year(as.Date(Rprt_Date)) == 2023) %>%
  mutate(Week_Number = week(as.Date(Rprt_Date)))

# Calculate the total number of reports for each week
weekly_counts <- reports_2023 %>%
  group_by(Week_Number) %>%
  summarise(Count = n())

# Calculate the average number of reports per week
average_reports_per_week <- mean(weekly_counts$Count)
average_reports_per_week
# Calculate the weights for each week
# The weight is the average divided by the count for that week
# Weeks with fewer reports get a higher weight
weekly_counts <- weekly_counts %>%
  mutate(Weight = average_reports_per_week / Count)

# If you want the weights to sum up to the number of weeks (to normalize them),
# you can scale them as follows:
total_weeks <- n_distinct(reports_2023$Week_Number)
weekly_counts <- weekly_counts %>%
  mutate(Weight = Weight / sum(Weight) * total_weeks)

# Now, join these weights back to the original reports
weighted_reports <- reports_2023 %>%
  left_join(weekly_counts, by = "Week_Number")


y <- full_join(weighted_reports, message_data, by = "User_ID")
z<- full_join(reports_data,message_data, by= "User_ID")

y <- y %>%
  group_by(User_ID) %>%
  summarize(
    Rprts_During_Msging_W = sum(Weight[Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date], na.rm = TRUE),
    Rprts_Before_Msging_W = sum(Weight[Rprt_Date >= (First_Msg_Date - days(37)) & Rprt_Date < First_Msg_Date], na.rm = TRUE),
    Rprts_After_Msging_W = sum(Weight[Rprt_Date > Last_Msg_Date & Rprt_Date <= (Last_Msg_Date + days(37))], na.rm = TRUE),
    Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date, na.rm = TRUE),
    Rprts_Before_Msging = sum(Rprt_Date >= (First_Msg_Date - days(45)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
    Rprts_After_Msging = sum(Rprt_Date > Last_Msg_Date & Rprt_Date <= (Last_Msg_Date + days(45)), na.rm = TRUE)
  ) %>%
  ungroup()

z2 = z[z$User_ID %in% recieved_msgs$User_ID, ]
z2 <- z2 %>%
  group_by(User_ID) %>%
  summarize(Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date, na.rm = TRUE),
            Rprts_Before_Msging = sum(Rprt_Date >= (First_Msg_Date - days(37)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
            Rprts_After_Msging = sum(Rprt_Date > Last_Msg_Date & Rprt_Date <= (Last_Msg_Date + days(37)), na.rm = TRUE)
  ) %>%
  ungroup()


z2 <- z2 %>%
  filter(!User_ID %in% c("fa099b57-da20-4e98-ab79-0c2615cac14e",
                         "c07e724e-f9bc-4dbd-9193-8b5888437e8b"))
z2 <- z2 %>% mutate(dif = Rprts_Before_Msging - Rprts_During_Msging)

summary(z2)


y2 = y[y$User_ID %in% recieved_msgs$User_ID, ]

summary(y2)
summary(z2)


paired_t_test_result <- with(y2, t.test(Rprts_After_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)
paired_t_test_result <- with(y2, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)
paired_t_test_result <- with(y2, t.test(Rprts_After_Msging_W, Rprts_Before_Msging_W,  paired = TRUE))
print(paired_t_test_result)
paired_t_test_result <- with(y2, t.test(Rprts_During_Msging_W, Rprts_Before_Msging_W,  paired = TRUE))
print(paired_t_test_result)

paired_t_test_result <- with(z2, t.test(Rprts_After_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)
paired_t_test_result <- with(z2, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)


#For the normal not weighed data it takes taking out two entries to make the difference significant.
#most of the people on top are ones that have many entries as soon as they sign up and then do nothing causing the skewed data.







y2 <- y2 %>% mutate(dif = Rprts_Before_Msging - Rprts_During_Msging)

y3 <- y2 %>%
  filter(!User_ID %in% c("fa099b57-da20-4e98-ab79-0c2615cac14e",
                         "0eaa6fd0-99cd-4f57-aafb-f28b6d640ac8",
                         "d4e820b3-e502-40c3-aff8-eeb9ef3955d0",
                         "52279491-6446-42b5-96f5-9c9f25b17c9d",
                         "c9faebc9-c286-4aef-ab99-65b2000a9ee1",
                         "fd786e71-17af-44ed-81cd-59fba94d55b2",
                         "f96a9713-4ffc-442d-b4b4-e402217f12b5",
                         "be5e80f1-5494-4e32-a8f6-2cb5b8f8fc96",
                         "4437b614-ec38-401b-9a0e-26c937933788"))


paired_t_test_result <- with(y3, t.test(Rprts_After_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)
summary(y3)

y4 <- recieved_msgs %>%
  filter(User_ID %in% c("fa099b57-da20-4e98-ab79-0c2615cac14e",
                        "0eaa6fd0-99cd-4f57-aafb-f28b6d640ac8",
                        "d4e820b3-e502-40c3-aff8-eeb9ef3955d0",
                        "52279491-6446-42b5-96f5-9c9f25b17c9d",
                        "c9faebc9-c286-4aef-ab99-65b2000a9ee1",
                        "fd786e71-17af-44ed-81cd-59fba94d55b2",
                        "f96a9713-4ffc-442d-b4b4-e402217f12b5",
                        "be5e80f1-5494-4e32-a8f6-2cb5b8f8fc96",
                        "4437b614-ec38-401b-9a0e-26c937933788"))

y5 <-  recieved_msgs %>%
  filter(User_ID %in% c("76535311-fd93-4804-aff4-4cd0cc3122e9",
                        "24ee7efd-a288-4f39-b02d-41e09e4c9ce9",
                        "375404ad-a343-4f6d-8833-2dc665fc2714",
                        "92d7a185-99e8-44ae-8841-9e993bab9c32",
                        "b52b1323-1c82-4d2b-a7ee-55c16750faa4"))

paired_t_test_result <- with(y3, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)
summary(y3)



y6 <- recieved_msgs %>%
  filter(!User_ID %in% c("fa099b57-da20-4e98-ab79-0c2615cac14e",
                         "0eaa6fd0-99cd-4f57-aafb-f28b6d640ac8",
                         "d4e820b3-e502-40c3-aff8-eeb9ef3955d0",
                         "52279491-6446-42b5-96f5-9c9f25b17c9d",
                         "c9faebc9-c286-4aef-ab99-65b2000a9ee1",
                         "fd786e71-17af-44ed-81cd-59fba94d55b2",
                         "f96a9713-4ffc-442d-b4b4-e402217f12b5",
                         "be5e80f1-5494-4e32-a8f6-2cb5b8f8fc96",
                         "4437b614-ec38-401b-9a0e-26c937933788"))

paired_t_test_result <- with(y6, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)

