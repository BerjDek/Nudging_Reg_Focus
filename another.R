
z2 <- z2 %>%
  filter(!User_ID %in% c("52279491-6446-42b5-96f5-9c9f25b17c9d",  
                         "0a9adcf3-aaac-46a4-8606-7dbc25f1cd67",  
                         "d3f13ba7-8d3d-4954-87a0-587c15e13ad1",  
                         "d5b6838d-3fa1-4c80-9592-f7d26b34af9c", 
                         "1a5af128-4c9f-43e1-a0f5-5e336218e8be")) 



#draft

z3 = z[z$User_ID %in% recieved_msgs$User_ID, ]
head(z3)

z2$days <- days(sum(z2$Rprt_Date >= z2$First_Msg_Date & z2$Rprt_Date <= z2$Last_Msg_Date, na.rm = TRUE))

head(z2)

paired_t_test_result <- with(z2, t.test(Rprts_During_Msging, Rprts_Before_Msging_2,  paired = TRUE))
print(paired_t_test_result)

head(message_data_2)

z3 = z[z$User_ID %in% recieved_msgs$User_ID, ]
z3 <- z3 %>%
  group_by(User_ID) %>%
  summarize(
    Msg_Type = first(Msg_Type),
    Nmbr_Msgs_Seen = first(Nmbr_Msgs_Seen),
    Msg_Duration_Days = first(Msg_Duration_Days),
    Msg_Duration_Days_cor = first(Msg_Duration_Days_cor),
    Duration_First_Sent_Last_Seen = first(Duration_First_Sent_Last_Seen),
    Duration_First_Last_Seen = first(Duration_First_Last_Seen),
    Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date, na.rm = TRUE),
    Rprts_During_Msging_cor = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date_cor, na.rm = TRUE),
    Rprts_Before_Msging_cor = sum(Rprt_Date >= (First_Msg_Date - days(Msg_Duration_Days_cor)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
    Rprts_After_Msging_cor = sum(Rprt_Date > Last_Msg_Date_cor & Rprt_Date <= (Last_Msg_Date_cor + days(Msg_Duration_Days_cor)), na.rm = TRUE),
    Rprts_Before_Msging_31 = sum(Rprt_Date >= (First_Msg_Date - days(31)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
    Rprts_After_Msging_31 = sum(Rprt_Date > Last_Msg_Date & Rprt_Date <= (Last_Msg_Date + days(31)), na.rm = TRUE),
    Rprts_After_Msging_31_cor = sum(Rprt_Date > Last_Msg_Date_cor & Rprt_Date <= (Last_Msg_Date_cor + days(31)), na.rm = TRUE),
    Rprts_During_Msging_Day_Seen = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Seen_Date , na.rm = TRUE),
    Rprts_Before_Msging_Day_Seen = sum(Rprt_Date >= (First_Msg_Date - days(Duration_First_Sent_Last_Seen)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
    Rprts_After_Msging_Day_Seen = sum(Rprt_Date > Last_Msg_Seen_Date & Rprt_Date <= (Last_Msg_Seen_Date + days(Duration_First_Sent_Last_Seen)), na.rm = TRUE),
    Rprts_During_Msging_Seen = sum(Rprt_Date >= First_Msg_Seen_Date & Rprt_Date <= Last_Msg_Seen_Date , na.rm = TRUE),
    Rprts_Before_Msging_Seen = sum(Rprt_Date >= (First_Msg_Seen_Date - days(Duration_First_Last_Seen)) & Rprt_Date < First_Msg_Seen_Date, na.rm = TRUE),
    Rprts_After_Msging_Seen = sum(Rprt_Date > Last_Msg_Seen_Date & Rprt_Date <= (Last_Msg_Seen_Date + days(Duration_First_Last_Seen)), na.rm = TRUE)
  ) %>%
  ungroup()



reports_data_delete <- reports_data %>% 
  filter(year(Rprt_Date) == 2023) 

reports_data_delete= reports_data_delete[reports_data_delete$User_ID %in% recieved_msgs$User_ID, ]
summary(reports_data_delete)

####

message_data_2<- raw_message_data %>%
  filter(year(Msg_Date) == 2023) %>% 
  group_by(User_ID) %>%
  summarize(
    Msg_Type = first(type),
    Repeat_User = first(Repeat_User),
    Nmbr_Msgs_Seen = sum(read_notification == "t"),
    Msg_Lang = first(Msg_Lang),
    First_Msg_Date = as.POSIXct(format(min(Msg_Date[msg_nmbr == 1]), "%Y-%m-%d")),
    Last_Msg_Date = as.POSIXct(format(max(Msg_Date[msg_nmbr == max(msg_nmbr)]), "%Y-%m-%d")),
    Last_Msg_Date_cor = as.POSIXct(format(max(Msg_Date), "%Y-%m-%d")),
    Nmbr_Msgs_Sent = n(),
    Repeated_Msg_Nmbr = if (anyDuplicated(msg_nmbr) > 0) msg_nmbr[duplicated(msg_nmbr)] else NA,
    First_Msg_Seen_Date = format(min(Msg_Date[read_notification == "t"], na.rm = TRUE), "%Y-%m-%d"),
    Last_Msg_Seen_Date= format(max(Msg_Date[read_notification == "t"], na.rm = TRUE), "%Y-%m-%d"),
    Msg_Duration_Days = as.integer(Last_Msg_Date - First_Msg_Date, units = "days"),
    Msg_Duration_Days_cor = as.integer(Last_Msg_Date_cor - First_Msg_Date, units = "days")
  ) %>%
  ungroup() %>%
  mutate(Msg_Type = as.factor(Msg_Type))


message_data_2<- message_data_2 %>% 
  group_by(User_ID) %>%
  mutate(First_Msg_Seen_Date = ifelse(First_Msg_Seen_Date == Inf | First_Msg_Seen_Date == -Inf, NA, First_Msg_Seen_Date),
         Last_Msg_Seen_Date = ifelse(Last_Msg_Seen_Date == Inf | Last_Msg_Seen_Date == -Inf, NA, Last_Msg_Seen_Date))

message_data_2<- message_data_2%>%
  mutate(
    First_Msg_Seen_Date = ifelse(is.na(First_Msg_Seen_Date), as.character(First_Msg_Date), First_Msg_Seen_Date),
    Last_Msg_Seen_Date = ifelse(is.na(Last_Msg_Seen_Date), as.character(First_Msg_Date), Last_Msg_Seen_Date)
  ) %>%
  mutate(
    First_Msg_Seen_Date = as.POSIXct(First_Msg_Seen_Date, format = "%Y-%m-%d"),
    Last_Msg_Seen_Date = as.POSIXct(Last_Msg_Seen_Date, format = "%Y-%m-%d")
  )

         

message_data_2<- message_data_2 %>% 
  mutate(Duration_First_Sent_Last_Seen = as.integer(Last_Msg_Seen_Date - First_Msg_Date, units = "days"),
         Duration_First_Last_Seen = as.integer(Last_Msg_Seen_Date - First_Msg_Seen_Date, units = "days"))


summary(message_data_2)


z<- full_join(reports_data,message_data_2, by= "User_ID")


z2 = z[z$User_ID %in% recieved_msgs$User_ID, ]


z2 <- z2%>%
  group_by(User_ID) %>%
  summarize(
    Total_Rprts_Filled = n(),
    Rprts_Filled_2023 = sum(Rprt_Date >= "2023-01-01"),
    Msg_Type = first(Msg_Type),
    Nmbr_Msgs_Seen = first(Nmbr_Msgs_Seen),
    Msg_Duration_Days = first(Msg_Duration_Days_cor),
    Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date_cor, na.rm = TRUE),
    Rprts_Before_Msging = sum(Rprt_Date >= (First_Msg_Date - days(Msg_Duration_Days_cor)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
    Rprts_After_Msging = sum(Rprt_Date > Last_Msg_Date_cor & Rprt_Date <= (Last_Msg_Date_cor + days(Msg_Duration_Days_cor)), na.rm = TRUE)
    ) %>%
  ungroup()

z2 <- z2 %>%
  left_join(survey_data %>% select(User_ID, Reg_Orientation_Cat), by = "User_ID")
    
z2 <- z2 %>% mutate(dif = Rprts_During_Msging - Rprts_Before_Msging)

summary(z2)


#checking for outliers in z2

Q1_total <- quantile(z2$Total_Rprts_Filled, 0.10)
Q3_total <- quantile(z2$Total_Rprts_Filled, 0.90)
IQR_total <- Q3_total - Q1_total

lower_bound_total <- Q1_total - 1.5 * IQR_total
upper_bound_total <- Q3_total + 1.5 * IQR_total

outliers_total_2 <- z2[z2$Total_Rprts_Filled < lower_bound_total | z2$Total_Rprts_Filled > upper_bound_total, ]


Q1_2023 <- quantile(z2$Rprts_Filled_2023, 0.10)
Q3_2023 <- quantile(z2$Rprts_Filled_2023, 0.90)
IQR_2023 <- Q3_2023 - Q1_2023

lower_bound_2023 <- Q1_2023 - 1.5 * IQR_2023
upper_bound_2023 <- Q3_2023 + 1.5 * IQR_2023

outliers_2023_2 <- z2[z2$Rprts_Filled_2023 < lower_bound_2023 | z2$Rprts_Filled_2023 > upper_bound_2023, ]

print(outliers_2023$User_ID)



#filtered out 90 percentaile outliers of reporters in 2023
z2 <- z2 %>%
  filter(!User_ID %in% c("24ee7efd-a288-4f39-b02d-41e09e4c9ce9", "92d7a185-99e8-44ae-8841-9e993bab9c32", "d44c8f5a-c314-44ac-a149-65d7fc9c3f0a",
                         "f96a9713-4ffc-442d-b4b4-e402217f12b5","fa099b57-da20-4e98-ab79-0c2615cac14e","4437b614-ec38-401b-9a0e-26c937933788")) 







ancova_result <- aov(dif ~ Msg_Type, data = z2) #there is a difference in the number of reports based on messages
summary(ancova_result)



lm_model <- lm(Rprts_During_Msging ~ Msg_Type + Reg_Orientation_Cat, data = z2)
summary(lm_model)

lm_model <- lm(dif ~ Msg_Type + Reg_Orientation_Cat, data = z2)
summary(lm_model)

lm_model <- lm(dif ~ Reg_Orientation_Cat, data = z2)
summary(lm_model)


result <- aov(dif ~ Msg_Type, data = z2)
summary(result)
TukeyHSD(result)


z2_promotion <- subset(z2, Msg_Type == "promotion")
z2_prevention <- subset(z2, Msg_Type == "prevention")

summary(z2_promotion)
summary(z2_prevention)


paired_t_test_result <- with(z2, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)

paired_t_test_result <- with(z2_promotion, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)

paired_t_test_result <- with(z2_prevention, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)


z3 %>% days(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date, na.rm = TRUE)

z2 <- z2 %>% mutate(dif = Rprts_Before_Msging - Rprts_During_Msging)
z2 <- z2 %>% mutate(dif3 = Rprts_Before_Msging_2 - Rprts_During_Msging)
z2 <- z2 %>% mutate(dif2 = Rprts_Before_Msging - Rprts_After_Msging)
z2 <- z2 %>% mutate(dif4 = Rprts_Before_Msging_2 - Rprts_After_Msging_2)



z2 <- z2 %>%
  group_by(User_ID) %>%
  summarize(Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date, na.rm = TRUE),
            Rprts_Before_Msging = sum(Rprt_Date >= (First_Msg_Date - days(31)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
            Rprts_After_Msging = sum(Rprt_Date > Last_Msg_Date & Rprt_Date <= (Last_Msg_Date + days(31)), na.rm = TRUE),
            Rprts_During_Seen = sum(Rprt_Date >= First_Msg_Seen_Date & Rprt_Date <= Last_Msg_Seen_Date, na.rm = TRUE),
            Rprts_Before_Seen = sum(Rprt_Date >= (First_Msg_Seen_Date - days(sum(Rprt_Date >= First_Msg_Seen_Date & Rprt_Date <= Last_Seen_Date, na.rm = TRUE))) & Rprt_Date < First_Msg_Seen_Date, na.rm = TRUE),
            Rprts_After_Seen = sum(Rprt_Date >= (Last_Msg_Seen_Date - days(sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date, na.rm = TRUE))) & Rprt_Date < Last_Msg_Seen_Date, na.rm = TRUE)
            ) %>%
  ungroup()
z2 <- z2 %>% mutate(dif = Rprts_During_Msging - Rprts_Before_Msging)
z2 <- z2 %>% mutate(dif2 = Rprts_Before_Msging - Rprts_After_Msging)


summary(z2)
summary(recieved_msgs$Rprts_During_Msging)
summary(recieved_msgs$Rprts_Before_Msging)
summary(recieved_msgs$Rprts_After_Msging)




paired_t_test_result <- with(z2, t.test(Rprts_After_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)
paired_t_test_result <- with(z2, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)

wilcox.test <- wilcox.test(y2$Rprts_During_Msging, y2$Rprts_Before_Msging, paired = TRUE)
print(wilcox.test)
#For the normal not weighed data it takes taking out two entries to make the difference significant.
#don't use the weighed data anymore
#most of the people on top are ones that have many entries as soon as they sign up and then do nothing causing the skewed data.


#do recount the second candidate taken out wasnt important
# 37days: alldata P0.0902 t1.70 dif 1.390;  -1  P0.0594 t1.894 dif 1.531; -2   P0.04069 t = 2.0581 dif 1.65; -3   P0.0279 t = 2.2 dif 1.7
# 35days: alldata P0.048  t1.98 dif 1.617;  -3  P0.016  t = 2.41 1.93

y4 <- recieved_msgs %>%
  filter(User_ID %in% c("c07e724e-f9bc-4dbd-9193-8b5888437e8b",  #1 1 1
                        "4437b614-ec38-401b-9a0e-26c937933788",  #3 2 3
                        "fa099b57-da20-4e98-ab79-0c2615cac14e",  #2 8
                        "fd786e71-17af-44ed-81cd-59fba94d55b2",  #5 4 4
                        "be5e80f1-5494-4e32-a8f6-2cb5b8f8fc96",  #6 5 2 
                        "52279491-6446-42b5-96f5-9c9f25b17c9d",  #4 3
                        "d4e820b3-e502-40c3-aff8-eeb9ef3955d0")) #- - 5
& Rprt_Date <= "2022-12-31"





#checking if a specific date of registration would matter

y5 <- left_join(recieved_msgs,z2, by= "User_ID")

y6<-y5 %>% select(User_ID,Registered_Participation_Date,dif,dif2)

y6 <- y6 %>%  filter(Registered_Participation_Date >= "2023-05-16"& Registered_Participation_Date <= "2023-05-18")
summary(y6$dif)  
  









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








                
  


