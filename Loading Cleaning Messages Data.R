library(lubridate)


#load message Data
message_data_raw <- read.csv(file="all_messages.csv", header = TRUE)
#seperate notifacation column to type language and msg_nmbr
message_data_raw <- message_data_raw %>% separate(notification_label, c('type','language','msg_nmbr'))
#change the message date to simple date
message_data_raw <- message_data_raw %>% 
    rename(User_ID = user_uuid, Msg_Date = date_sent, Msg_Lang = language) %>% 
    mutate(
    msg_nmbr = as.integer(msg_nmbr),
    Msg_Date = as.POSIXct(Msg_Date, format = "%Y-%m-%d %H:%M:%S")

#first the data for messages were loaded into a data set called message_data_raw, the notification label was split in order to have 
#sepearate columns for type, language and message number, column names changed to fit the style chosen for the survey data, and then 
#The types of message number column and message date column fixed to integer and date format



message_data <- message_data_raw %>%
  mutate(
    msg_nmbr = as.integer(msg_nmbr),
    Msg_Date = as.POSIXct(Msg_Date, format = "%Y-%m-%d %H:%M:%S")
  ) %>%
  group_by(User_ID) %>%
  mutate(Repeat_User = n_distinct(year(Msg_Date)) > 1) %>%
  filter(year(Msg_Date) == 2023) %>%
  summarize(
    type = first(type),
    Repeat_User = Repeat_User,
    language = first(Msg_Lang),
    date_first_msg = format(min(Msg_Date[msg_nmbr == 1]), "%d/%m/%Y"),
    date_last_msg = format(max(Msg_Date[msg_nmbr == max(msg_nmbr)]), "%d/%m/%Y"),
    sent_messages = n(),
    seen_messages = sum(read_notification == "t"),
    repeated_message_number = if (anyDuplicated(msg_nmbr) > 0) msg_nmbr[duplicated(msg_nmbr)] else NA
  ) %>%
  ungroup()


count(message_data%>%distinct(User_ID))
