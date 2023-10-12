library(lubridate)


#load message Data
message_data <- read.csv(file="all_messages.csv", header = TRUE)
#seperate notifacation column to type language and msg_nmbr
message_data <- message_data %>% separate(notification_label, c('type','language','msg_nmbr'))
#change the message date to simple date
message_data <- message_data %>% 
  mutate(date_comment = as.Date(date_comment)) %>% 
  rename(user_id = sent_to_user_id, msg_date = date_comment, msg_lang = language) 



count(message_data%>%distinct(user_uuid))
