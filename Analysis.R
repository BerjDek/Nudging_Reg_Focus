
library(car)
str(survey_completed)
#Analysis of survey data
data <- read.csv("loaddata.csv", header = TRUE)
# Filtering data for users who have completed the survey
survey_completed <- data %>% filter(Complt_Survey == TRUE)


# Descriptive statistics for demographic variables

summary(survey_completed %>% select(Age, Age_Group, Gender, Country, Network, Other_Citi_Sci, Participation_Date))

#Mean age of participants is 48.6, there is higher rate of male participation with 122 to 89, Majority of reports are from 3 countries
#Spain, Italy, and Netherlands with 103,77,and 22 participants respectively, vast majority have no acquaintances, and not part of other projects
#majority of participants are from 2023 (72), and then get less as the years go back to 54,40,21,10.....

# Histogram for Age Distribution

ggplot(survey_completed, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
  geom_vline(aes(xintercept = mean(Age, na.rm = TRUE)), color = "black", linetype = "dashed", size = 1) +
  labs(title = "Age Distribution of Users", x = "Age", y = "Count") +
  theme_minimal() +
  annotate("text", x = mean(survey_completed$Age, na.rm = TRUE), y = max(table(survey_completed$Age)), 
           label = sprintf("Avg Age: %.1f", mean(survey_completed$Age, na.rm = TRUE)), vjust = -20, color = "black")

# Bar Plot for Gender Distribution
ggplot(survey_completed, aes(x = reorder(Gender, -table(survey_completed$Gender)[Gender])))+
  geom_bar(fill = "skyblue", color = "white") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  labs(title = "Gender Distribution of Users", x = "Gender", y = "Count") +
  theme_minimal()


# Descriptive statistics for Regulatory Orientation and Motivators

summary(survey_completed %>% select(Reg_Orientation, Reg_Orientation_Cat, Openness_To_Change, Self_Enhancement, 
                                    Continuity, Self_Transcendence, Security, Teaching))

#The average Regulatory orientation of survey takers is -1.381, and by far the majority of users are of prevention-orientation with 121 to 70
#Regarding the motivators, the two highest means were for Self Transcendence(4.23) and Security (4.23) with security having a higher median (5)
# followed by Openness to Change (3.24), Teaching (2.26), and very low scores for Continuity (1.8) and Self Enhancement (1.49)







#Reg focus Distribution
survey_completed %>%
  filter(!is.na(Reg_Orientation_Cat)) %>%
  ggplot(aes(x = fct_infreq(Reg_Orientation_Cat))) +
  geom_bar(fill = "skyblue", color = "white") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Regulatory Orientation Categories", 
       x = "Regulatory Orientation Category", 
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#distribution by of Orientation between male and female
survey_completed %>%
  filter(!is.na(Reg_Orientation_Cat), !is.na(Gender), Gender %in% c("Male", "Female")) %>%
  group_by(Gender) %>%
  mutate(total = n()) %>%
  group_by(Reg_Orientation_Cat, Gender, total) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percentage = (count / total) * 100) %>%
  ggplot(aes(x = fct_infreq(Reg_Orientation_Cat))) +
  geom_bar(aes(y = percentage, fill = Gender), stat = "identity", position = "dodge", color = "white") +
  geom_text(aes(y = percentage, label = scales::percent(percentage/100, accuracy = 1), group = Gender),
            vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Regulatory Orientation Categories by Gender", 
       x = "Regulatory Orientation Category", 
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#the distribution of regulatory orientation is similar between both sexes


#distribution by of Orientation by age

survey_completed %>%
  filter(!is.na(Reg_Orientation_Cat), !is.na(Age_Group)) %>%
  group_by(Age_Group) %>%
  mutate(total_age_group = n()) %>%
  group_by(Reg_Orientation_Cat, Age_Group, total_age_group) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percentage = (count / total_age_group) * 100) %>%
  ggplot(aes(x = fct_infreq(Reg_Orientation_Cat))) +
  geom_bar(aes(y = percentage, fill = Age_Group), stat = "identity", position = "dodge", color = "white") +
  geom_text(aes(y = percentage, label = scales::percent(percentage/100, accuracy = 1), group = Age_Group),
            vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(title = "Distribution of Regulatory Orientation Categories by Age Group", 
       x = "Regulatory Orientation Category", 
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# checking orientation based on year of participation, to check if advertising over years attracted different people

percentage_data <- survey_completed %>%
  filter(!is.na(Reg_Orientation_Cat), !is.na(Participation_Date)) %>%
  filter(Participation_Date %in% c('2023', '2022', '2021', '2020', '2019')) %>%
  group_by(Participation_Date, Reg_Orientation_Cat) %>%
  summarise(Count = n()) %>%
  group_by(Participation_Date) %>%
  mutate(Total = sum(Count)) %>%
  mutate(Percentage = Count / Total * 100) %>%
  ungroup() %>%
  select(Participation_Date, Reg_Orientation_Cat, Count, Total, Percentage)


ggplot(percentage_data, aes(x = factor(Participation_Date, levels = c("2023", "2022", "2021", "2020", "2019")), 
                            y = Percentage, fill = Reg_Orientation_Cat)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage), y = Percentage + 2), position = position_dodge(width=0.9), size=3) +
  labs(title = "Distribution of Regulatory Orientation Categories by Participation Year",
       x = "Participation Year",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 100))

rm(percentage_data)

# It seems regardless of the major campaign this year the percentage of attracting Prevention regulatory orientation was higher
# 2019 is a too small of a sample and the only year where we attract equal percentages is 2021



#Distribution of Reg focus by country.

percentage_country_data <- survey_completed %>%
  group_by(Country, Reg_Orientation_Cat) %>%
  summarise(Count = n()) %>%
  left_join(survey_completed %>%
              group_by(Country) %>%
              summarise(Total = n()), by = "Country") %>%
  mutate(Percentage = (Count / Total) * 100) 


ggplot(
  percentage_country_data %>%
    filter(Country %in% c("Spain", "Italy", "Netherlands", "Hungary")), 
  aes(x = factor(Country, levels = c("Spain", "Italy", "Netherlands", "Hungary")), 
      y = Percentage, 
      fill = Reg_Orientation_Cat)
) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Regulatory Orientation Categories by Country",
       y = "Percentage",
       x = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rm(percentage_country_data)

#It seems that for some reason in Spain which is of course the biggest pool of participants we are attracting more prevention orientation


#looking into motivators

average_data <- survey_completed %>%
  summarise(
    Openness_To_Change = mean(Openness_To_Change, na.rm = TRUE),
    Self_Enhancement = mean(Self_Enhancement, na.rm = TRUE),
    Continuity = mean(Continuity, na.rm = TRUE),
    Self_Transcendence = mean(Self_Transcendence, na.rm = TRUE),
    Security = mean(Security, na.rm = TRUE),
    Teaching = mean(Teaching, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(cols = everything(), 
                      names_to = "Motivator", 
                      values_to = "Average")


ggplot(average_data, aes(x = Motivator, y = Average)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Average)), vjust = -0.5, size = 4) +
  labs(title = "Distribution of Average Motivator Ratings",
       y = "Rating over 5",
       x = "Motivator") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ylim(0, 5)
rm(average_data)


# Melting the data for motivators


survey_completed %>%
  select(Openness_To_Change:Teaching) %>%
  pivot_longer(cols = everything(), names_to = "Motivator", values_to = "Score") %>%
  ggplot(aes(x = Motivator, y = Score)) +
  geom_boxplot() +
  labs(title = "Distribution of Scores for Different Motivators",
       x = "Motivator", y = "Score") +
  theme_minimal()

#distribution of motivators by gender, seems both genders rate motivators equally.

average_data_gender <- survey_completed %>%
  group_by(Gender) %>%
  summarise(
    Openness_To_Change = mean(Openness_To_Change, na.rm = TRUE),
    Self_Enhancement = mean(Self_Enhancement, na.rm = TRUE),
    Continuity = mean(Continuity, na.rm = TRUE),
    Self_Transcendence = mean(Self_Transcendence, na.rm = TRUE),
    Security = mean(Security, na.rm = TRUE),
    Teaching = mean(Teaching, na.rm = TRUE)
  ) %>%
  filter(Gender %in% c("Male", "Female")) %>% 
  tidyr::pivot_longer(cols = -Gender, 
                      names_to = "Motivator", 
                      values_to = "Average")

ggplot(average_data_gender, aes(x = Motivator, y = Average, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Average), group=Gender),
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4) +
  labs(title = "Distribution of Average Motivator Ratings by Gender",
       y = "Average Rating",
       x = "Motivator") +
  scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "top") +
  ylim(0, 5)

rm(average_data_gender)


#motivators by age, same for age they seem equally split, where average ratings are mostly similar for age groups, except for OfC which is 
#notionally higher for 18-25 segement

average_data_age <- survey_completed %>%
  group_by(Age_Group) %>%
  summarise(
    Openness_To_Change = mean(Openness_To_Change, na.rm = TRUE),
    Self_Enhancement = mean(Self_Enhancement, na.rm = TRUE),
    Continuity = mean(Continuity, na.rm = TRUE),
    Self_Transcendence = mean(Self_Transcendence, na.rm = TRUE),
    Security = mean(Security, na.rm = TRUE),
    Teaching = mean(Teaching, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(cols = -Age_Group, 
                      names_to = "Motivator", 
                      values_to = "Average")


ggplot(average_data_age, aes(x = Motivator, y = Average, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Average), group=Age_Group),
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 2) +
  labs(title = "Distribution of Average Motivator Ratings by Age Group",
       y = "Average Rating",
       x = "Motivator") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "top") +
  ylim(0, 5)

rm(average_data_age)



#motivation rating by Regulatory focus orientations categories

average_data_reg <- survey_completed %>%
  group_by(Reg_Orientation_Cat) %>%
  summarise(
    Openness_To_Change = mean(Openness_To_Change, na.rm = TRUE),
    Self_Enhancement = mean(Self_Enhancement, na.rm = TRUE),
    Continuity = mean(Continuity, na.rm = TRUE),
    Self_Transcendence = mean(Self_Transcendence, na.rm = TRUE),
    Security = mean(Security, na.rm = TRUE),
    Teaching = mean(Teaching, na.rm = TRUE)
  ) %>%
  filter(Reg_Orientation_Cat != "NA") %>% 
  tidyr::pivot_longer(cols = -Reg_Orientation_Cat, 
                      names_to = "Motivator", 
                      values_to = "Average")

ggplot(average_data_reg, aes(x = Motivator, y = Average, fill = Reg_Orientation_Cat)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Average), group=Reg_Orientation_Cat),
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 2) +
  labs(title = "Distribution of Average Motivator Ratings by Regulatory Orientation Category",
       y = "Average Rating",
       x = "Motivator") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "top") +
  ylim(0, 5)



average_data_reg_cat <- survey_completed %>%
  group_by(Reg_Orientation_Cat) %>%
  summarise(
    Openness_To_Change = mean(Openness_To_Change, na.rm = TRUE),
    Self_Enhancement = mean(Self_Enhancement, na.rm = TRUE),
    Continuity = mean(Continuity, na.rm = TRUE),
    Self_Transcendence = mean(Self_Transcendence, na.rm = TRUE),
    Security = mean(Security, na.rm = TRUE),
    Teaching = mean(Teaching, na.rm = TRUE) 
  ) %>%
  filter(Reg_Orientation_Cat != "NA") %>% 
  tidyr::pivot_longer(cols = -Reg_Orientation_Cat, 
                      names_to = "Motivator", 
                      values_to = "Average")

ggplot(average_data_reg_cat, aes(x = Reg_Orientation_Cat, y = Average, fill = Motivator)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Average), group=Motivator),
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3) +
  labs(title = "Distribution of Average Motivator Ratings by Regulatory Orientation Category",
       y = "Average Rating",
       x = "Regulatory Orientation Category") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "top") +
  ylim(0, 5)


rm(average_data_reg_cat, average_data_reg)

# Unlike what was expected, various orientations value motivators similarly, the difference in security rating be higher for prev is mild

# to check if there was any significant correlation we perform a regression analysis

motivators <- c("Openness_To_Change", "Self_Enhancement", "Continuity", 
                "Self_Transcendence", "Security", "Teaching")


get_pvalue <- function(motivator) {
  model <- lm(data = survey_completed, formula = as.formula(paste(motivator, "~ Reg_Orientation")))
  p_value <- summary(model)$coefficients[2,4]  # Extract p-value for Reg_Orientation
  return(p_value)
}


results <- data.frame(
  Motivator = motivators,
  P_value = sapply(motivators, get_pvalue)
)

print(results)

rm( motivators, results, get_pvalue)

#as expected perform a linear regression For each motivator where Regulatory Orientation score is the predictor and the motivator 
#rating is the dependent variable, showed no significant relationship, where the p value for each was much higher than 0.05.



##Messages

sum(data$Got_Msgs)

sum(data$Complt_Survey)

#as stated before 237 individuals received messages, out of which 217 completed the survey.

filtered_data <- data %>% 
  filter(Got_Msgs == TRUE)

ggplot(filtered_data, aes(x = Msg_Type)) +
  geom_bar(fill = "skyblue", show.legend = FALSE) + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  labs(title = "Distribution of Message Type for Those Who Received Messages",
       y = "Count",
       x = "Message Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

filtered_data <- data[data$Got_Msgs & data$Complt_Survey, ]
ggplot(filtered_data, aes(x = Msg_Type)) +
  geom_bar(fill = "skyblue") + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  labs(title = "Distribution of Message Type for Those Who Received Messages",
       y = "Count",
       x = "Message Type") +
  theme_minimal()


rm(filtered_data)

#The distribution of consents seems equal for 79 for each group, but for the ones that completed the survey it seems prevention by random got 
#less, note that this might impact the results as one that did not fill the survey might be less motivated in general.



survey_completed %>%
  filter(!is.na(Msg_Type), !is.na(Gender), Gender %in% c("Male", "Female")) %>%
  group_by(Gender) %>%
  mutate(total = n()) %>%
  group_by(Msg_Type, Gender, total) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percentage = (count / total) * 100) %>%
  ggplot(aes(x = fct_infreq(Msg_Type))) +
  geom_bar(aes(y = percentage, fill = Gender), stat = "identity", position = "dodge", color = "white") +
  geom_text(aes(y = percentage, label = scales::percent(percentage/100, accuracy = 1), group = Gender),
            vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(title = "Distribution Gender to different Message Types", 
       x = "Msg_Type", 
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

survey_completed %>%
  filter(!is.na(Msg_Type), !is.na(Gender), Gender %in% c("Male", "Female")) %>%
  group_by(Msg_Type, Gender) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  ggplot(aes(x = fct_infreq(Msg_Type))) +
  geom_bar(aes(y = count, fill = Gender), stat = "identity", position = "dodge", color = "white") +
  geom_text(aes(y = count, label = count, group = Gender),
            vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(title = "Number of Males and Females Receiveing each Msg Type", 
       x = "Msg_Type", 
       y = "Count") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



survey_completed %>%
  filter(Gender %in% c("Male", "Female")) %>%
  group_by(Msg_Type, Gender) %>%
  summarise(count = n()) %>%
  group_by(Msg_Type) %>%
  mutate(proportion = count / sum(count)) %>% 
  ggplot(aes(x = Msg_Type, y = proportion, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)), 
            position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
  labs(title = "Distribution of Age Groups in Each Message Type", y = "Percentage", x = "Message Type", fill = "Age Group") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 10)) +
  scale_fill_brewer(palette = "Set1")

# the distribution of types of message seems similar between the sexes


#distribution by age group

survey_completed %>%
  group_by(Msg_Type, Age_Group) %>%
  summarise(count = n()) %>%
  group_by(Msg_Type) %>%
  mutate(proportion = count / sum(count)) %>% 
  ggplot(aes(x = Msg_Type, y = proportion, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)), 
            position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
  labs(title = "Distribution of Age Groups in Each Message Type", y = "Percentage", x = "Message Type", fill = "Age Group") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 10)) +
  scale_fill_brewer(palette = "Set1")
#by chance a the youngest group made of 13 individuals were assigned mostly to neutral messaging, the proportion seem mostly similar except for 
#large chunk of neutral being younger than 25




#Distribution of messages  based on regulatory focus orientation
survey_completed %>%
  filter(!is.na(Reg_Orientation_Cat)) %>%
  group_by(Msg_Type, Reg_Orientation_Cat) %>%
  summarise(count = n()) %>%
  group_by(Msg_Type) %>%
  mutate(proportion = count / sum(count)) %>% 
  ggplot(aes(x = Msg_Type, y = proportion, fill = Reg_Orientation_Cat)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)), 
            position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
  labs(title = "Distribution of Reg_Orientation_Cat in Each Message Type", y = "Percentage", x = "Message Type", fill = "Reg_Orientation_Cat") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 10)) +
  scale_fill_brewer(palette = "Set1")


#Distribution of messages  based on participation year

survey_completed %>%
  filter(!is.na(Msg_Type), !is.na(Participation_Date)) %>%
  filter(Participation_Date %in% c('2023', '2022', '2021', '2020', '2019')) %>%
  group_by(Msg_Type, Participation_Date) %>%
  summarise(count = n()) %>%
  group_by(Msg_Type) %>%
  mutate(proportion = count / sum(count)) %>% 
  ggplot(aes(x = Msg_Type, y = proportion, fill = Participation_Date)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)), 
            position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
  labs(title = "Distribution of Age Groups in Each Message Type", y = "Percentage", x = "Message Type", fill = "Age Group") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 10)) +
  scale_fill_brewer(palette = "Set1")
#also by year of participation the distribution seems mostly similar between message types. 


#Distribution of messages  based on messaging group
survey_completed %>%
  filter(!is.na(Msg_Type), !is.na(Message_Group)) %>%
  group_by(Msg_Type, Message_Group) %>%
  summarise(count = n()) %>%
  group_by(Msg_Type) %>%
  mutate(proportion = count / sum(count)) %>% 
  ggplot(aes(x = Msg_Type, y = proportion, fill = Message_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)), 
            position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
  labs(title = "Distribution of Age Groups in Each Message Type", y = "Percentage", x = "Message Type", fill = "Age Group") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 10)) +
  scale_fill_brewer(palette = "Set1")






#checking to see if the number of messages seen by users is different in the case of each message type, 
#for those that Got Messages (for those with only surevey added to markdown)

#average number of messages seen

data %>%
  filter(Got_Msgs) %>%
  ggplot(aes(x = Msg_Type, y = Nmbr_Msgs_Seen, fill = Msg_Type)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  theme_minimal() +
  labs(title = "Average Message Reading Rate by Message Type (All Msg Recievers)",
       x = "Message Type",
       y = "Average Messages Seen") +
  scale_fill_brewer(palette = "Set1") + 
  geom_text(stat = "summary", fun = mean, aes(label = sprintf("%.2f", ..y..)), position = position_dodge(width = 0.9), vjust = -0.5) 


anova_result <- aov(Nmbr_Msgs_Seen ~ Msg_Type, data = data %>% filter(Got_Msgs))
summary(anova_result)
posthoc <- TukeyHSD(anova_result)
print(posthoc)
rm(anova_result,posthoc)


#The average message reading rate  when compared to the number of messages sent

data %>%
  filter(Got_Msgs) %>%
  ggplot(aes(x = Msg_Type, y = (Nmbr_Msgs_Seen / Nmbr_Msgs_Sent) *100, fill = Msg_Type)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  theme_minimal() +
  labs(title = "Average Message Reading Rate by Message Type",
       x = "Message Type",
       y = "Average Messages Seen") +
  scale_fill_brewer(palette = "Set1") + 
  geom_text(stat = "summary", fun = mean, aes(label = sprintf("%.2f", ..y..)), position = position_dodge(width = 0.9), vjust = -0.5) # Added to display the mean value on top of each bar

#checking if there is a significant difference
anova_result <- aov((Nmbr_Msgs_Seen / Nmbr_Msgs_Sent) * 100 ~ Msg_Type, data = data)
summary(anova_result)

posthoc <- TukeyHSD(anova_result)
print(posthoc)

rm(anova_result, posthoc)
#no significant difference in checking messages



#number of messages seen as categories for each message type

data %>%
  filter(Got_Msgs) %>%
  mutate(seen_category = cut(Nmbr_Msgs_Seen,
                             breaks = c(-1, 0, 4, 8, Inf), 
                             labels = c("0 messages", "1-4 messages", "5-8 messages", "9+ messages"),
                             right = TRUE)) %>%
  ggplot(aes(x = Msg_Type, fill = seen_category)) + 
  geom_bar(position = "dodge") +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1), vjust = -0.5) +
  theme_minimal() +
  labs(x = "Message Group",
       y = "Number of Users") +
  scale_fill_brewer(palette = "Set1", name = "Messages Seen") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 




### Difference in Messages Seen by Regulatory Focus orientation
data %>%
  filter(Got_Msgs, !is.na(Reg_Orientation_Cat)) %>%
  group_by(Reg_Orientation_Cat, Msg_Type) %>%
  summarise(Avg_Msgs_Seen = mean(Nmbr_Msgs_Seen, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = Reg_Orientation_Cat, y = Avg_Msgs_Seen, fill = Msg_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Average Messages Seen by Regulatory Focus and Message Type",
       x = "Regulatory Focus Category",
       y = "Average Number of Messages Seen") +
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label = sprintf("%.2f", Avg_Msgs_Seen)), position = position_dodge(width = 0.9), vjust = -0.5)


### Anova

test <- data %>%
  filter(!is.na(Reg_Orientation_Cat)) %>%
  group_by(Reg_Orientation_Cat, Msg_Type) %>%
  summarise(Avg_Msgs_Seen = mean(Nmbr_Msgs_Seen, na.rm = TRUE), .groups = 'drop') %>%
  aov(Avg_Msgs_Seen ~ Reg_Orientation_Cat + Msg_Type + Reg_Orientation_Cat:Msg_Type, data = .)

summary(test)
rm(test)


## message seen by message group



data %>% subset(Got_Msgs == TRUE) %>% 
  ggplot(aes(x = Message_Group)) +
  geom_bar(fill = "skyblue") + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  labs(title = "Distribution of Date of First message received",
       y = "Count",
       x = "Message Type") +
  theme_minimal()


# Average Message Reading Rate by Message Group

data %>%
  filter(Got_Msgs) %>%
  ggplot(aes(x = Message_Group, y = Nmbr_Msgs_Seen, fill = Message_Group)) +
  geom_bar(stat = "summary", fun = "mean") +
  theme_minimal() +
  labs(title = "Average Message Reading Rate by Message Group",
       x = "Message Group",
       y = "Average Messages Seen") +
  scale_fill_brewer(palette = "Set1", guide = "none") + 
  geom_text(stat = "summary", fun = mean, aes(label = sprintf("%.2f", ..y..)), position = position_dodge(width = 0.9), vjust = -0.5)




###here
#proportion of users that saw the messages based on group

data %>%
  filter(Got_Msgs) %>%
  mutate(seen_category = cut(Nmbr_Msgs_Seen,
                             breaks = c(-1, 0, 4, 8, Inf), 
                             labels = c("0 messages", "1-4 messages", "5-8 messages", "9+ messages"),
                             right = TRUE)) %>%
  group_by(Message_Group, seen_category) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = Message_Group, y = percentage, fill = seen_category)) + 
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(percentage/100, accuracy = 1)), 
            position = position_stack(vjust = 0.5), color = "Black", size = 2.5) +
  theme_minimal() +
  labs(title = "Percentage of Messages Seen by Users for Each Message Group",
       x = "Message Group",
       y = "Percentage of Users") +
  scale_fill_brewer(palette = "Set3", name = "Messages Seen") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









# check if seen averages are significant, check if time of messaging is significant.









#The distribution seems similar, ironically less neutral oriented users out of the 24 seem to have been randomly chosen to get neutral messages















#check if the message and the personality alligning meant any ifference for the reporting










# Number of Reports Filled by Age Group # for this code  give a written summery, and make the graph so that the distribution is clearer, maybe limit space above 400 and mention occurrences
ggplot(survey_completed, aes(x = Age_Group, y = Total_Rprts_Filled)) +
  geom_boxplot() +
  labs(title = "Number of Reports Filled by Age Group",
       x = "Age Group", y = "Total Reports Filled") +
  theme_minimal()


# Type of Reports Filled, this isn't necessary, maybe get something that shows what each gender and age group fill.
type_reports <- survey_completed %>%
  select(Total_Bite_Rprts_Filled, Total_Adult_Rprts_Filled, Total_Site_Rprts_Filled) %>%
  gather(key = "Report_Type", value = "Count")

ggplot(type_reports, aes(x = Report_Type, y = Count)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Type of Reports Filled",
       x = "Report Type", y = "Total Count") +
  theme_minimal()

rm(motivators_melted, type_reports)