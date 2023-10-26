
library(car)

#Analysis of survey data

# Filtering data for users who have completed the survey
survey_completed <- data %>% filter(Complt_Survey == TRUE)

survey_completed <- survey_completed %>%  mutate(Participation_Date = as.factor(Participation_Date)) ## remove later 

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
            vjust = -0.5, size = 3) +
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

# Create the bar plot
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
rm( motivators, results, get_pvalue)

#as expected perform a linear regression For each motivator where Regulatory Orientation score is the predictor and the motivator 
#rating is the dependent variable, showed no significant relationship, where the p value for each was much higher than 0.05.



# Melting the data for motivators
motivators_melted <- survey_completed %>%
  select(Openness_To_Change:Teaching) %>%
  gather(key = "Motivator", value = "Score")

# Boxplot for Motivators # give a written summery, 
ggplot(motivators_melted, aes(x = Motivator, y = Score)) +
  geom_boxplot() +
  labs(title = "Distribution of Scores for Different Motivators",
       x = "Motivator", y = "Score") +
  theme_minimal()
rm(motivators_melted)


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