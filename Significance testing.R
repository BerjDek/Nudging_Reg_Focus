#checking if survey is representative

# Values
population_size <- 270804  # Total population that downloaded the app
sample_size <- 218         # Sample size who completed the survey
confidence_level_constant <- 1.96  # Z-value for 95% confidence level

# Assuming maximum variability since we don't know the population characteristics (p=0.5 for maximum variability)
p <- 0.5

# Calculate the margin of error using the formula with finite population correction
margin_of_error <- (confidence_level_constant * sqrt((p * (1 - p)) / sample_size)) / 
  (1 + ((confidence_level_constant^2 * p * (1 - p)) / 
          (sample_size * population_size)))

# Convert to percentage
margin_of_error_percentage <- margin_of_error * 100

# Calculate response rate and completion rate
invitations_seen <- 460
attempts_made <- 238
response_rate <- (attempts_made / invitations_seen) * 100
completion_rate <- (sample_size / attempts_made) * 100

list(margin_of_error_percentage = margin_of_error_percentage, 
     response_rate = response_rate, 
     completion_rate = completion_rate)
rm(population_size,sample_size, confidence_level_constant, p,margin_of_error, margin_of_error_percentage, margin_of_error_percentage,
   invitations_seen, attempts_made, response_rate, completion_rate)

#While the response rate of 48.22% among those who viewed the invitation is robust, the overall completion rate from the 
#total population of app downloaders is 0.0818%, which raises concerns about the representativeness of the survey results. Keeping in mind that 
#We have no idea who is still active or even has the app on their phone to receive the invite.
#The margin of error at 6.65% is somewhat higher than the typically desired 5%, suggesting that the survey findings should be interpreted with 
#caution. Due to the lack of visibility into active users versus those who have uninstalled the app, the survey results may not accurately reflect
#the current user base. As such, the survey findings are most representative of the subset of users who are engaged with the app to the extent 
#that they have seen and responded to the in-app invite. The results, therefore, should be generalized to the broader population of all app 
#downloaders only with substantial caveats. 

s3 <- survey_completed %>%
  filter(!User_ID %in% c("fa099b57-da20-4e98-ab79-0c2615cac14e",
                         "0eaa6fd0-99cd-4f57-aafb-f28b6d640ac8",
                         "d4e820b3-e502-40c3-aff8-eeb9ef3955d0",
                         "52279491-6446-42b5-96f5-9c9f25b17c9d",
                         "c9faebc9-c286-4aef-ab99-65b2000a9ee1",
                         "fd786e71-17af-44ed-81cd-59fba94d55b2",
                         "f96a9713-4ffc-442d-b4b4-e402217f12b5",
                         "be5e80f1-5494-4e32-a8f6-2cb5b8f8fc96",
                         "4437b614-ec38-401b-9a0e-26c937933788"))


#checking if the Regulatory focus Average is significant

# to test it we conduct a one-sample t-test to compare the mean of the sample to a known population mean.

mean_reg_orientation <- mean(survey_completed$Reg_Orientation, na.rm = TRUE)

t_test_result <- t.test(survey_completed$Reg_Orientation, mu = 0, na.rm = TRUE)

# Output the results
mean_reg_orientation
t_test_result
rm(mean_reg_orientation, t_test_result)

#The t-test yielded a statistic of approximately -4.23, with a p-value of about 0.0000346. well below the common significance threshold of 0.05, 
#indicating that the mean regulatory orientation for users who completed the survey is significantly different from 0 and more Prevention oriented


ggplot(survey_completed, aes(x = Reg_Orientation)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "#b4464b") +
  geom_vline(xintercept = -1.402778, color = "#82b446", linetype = "dashed") +
  geom_vline(xintercept = -2.0448547, color = "#b47846", linetype = "dashed") +
  geom_vline(xintercept = -0.7607009, color = "#b47846", linetype = "dashed") +
  annotate("text", x = -1.402778, y = Inf, label = "Mean (-1.40)", vjust = 0.8, color = "#82b446", size = 3) +
  annotate("text", x = -2.0448547, y = Inf, label = "Lower CI (-2.04)", vjust = 2,hjust = 1.2, color = "#b47846", size = 3) +
  annotate("text", x = -0.7607009, y = Inf, label = "Upper CI (-0.76)", vjust = 2, hjust = -0.3, color = "#b47846", size = 3) +
  labs(title = "One Sample t-test Result Visualization",
       x = "Reg Orientation Value",
       y = "Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 7))


#Checking if the Regulatory focus Average is significant

orientation_counts <- table(survey_completed$Reg_Orientation_Cat)
orientation_counts <- orientation_counts[names(orientation_counts) != "NA"]
orientation_counts
chi_squared_test <- chisq.test(orientation_counts)
print(chi_squared_test)

oriented_data <- subset(survey_completed, Reg_Orientation_Cat %in% c('Prevention-oriented', 'Promotion-oriented'))
oriented_counts <- table(oriented_data$Reg_Orientation_Cat)
oriented_counts <- oriented_counts[names(oriented_counts) != "Neutral"]
oriented_counts
# Since we expect the counts to be equal, the expected frequency for each is half of the total count
total_expected_count <- sum(oriented_counts)
expected_counts <- rep(total_expected_count / 2, 2)

chi_squared_test <- chisq.test(oriented_counts, p = rep(1/2, 2))
print(chi_squared_test)

rm(orientation_counts, chi_squared_test, oriented_data, oriented_counts, total_expected_count, expected_counts)


# The Chi-squared test is used to determine  whether the distribution of users across three regulatory orientation categories—
#'prevention-oriented,' 'promotion-oriented,' and 'neutral'—deviates significantly from a uniform distribution.
#The test yielded a a p-value of approximately 5.407e-15 suggesting  that the distribution of users' regulatory orientation categories 
#is significantly different from a uniform one

# To further assess if the number of promotion-oriented users is significantly different from the number of prevention-oriented users we performed 
# a Chi-squared test with the assumption that both categories should have equal numbers of participants, yielded a significant  P-value of 0.0002

#visualization of this chi square NOT IMPORTANT
ggplot(data = data.frame(Category = rep(names(oriented_counts), 2),
                         Count = c(oriented_counts, rep(total_expected_count / 2, 2)),
                         Type = rep(c("Observed", "Expected"), each = length(oriented_counts)))) +
  geom_bar(aes(x = Category, y = Count, fill = Type), stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("steelblue", "orange")) +
  labs(title = "Chi-Squared Test: Comparison of Observed and Expected Frequencies",
       subtitle = paste("Chi-Squared = 14.083, df = 1, p-value = 0.0001749"),
       x = "Orientation Category",
       y = "Frequency Count",
       caption = "Data Source: survey_completed dataset") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.title = element_blank())



#checking if some motivators are significantly more important to users than others

motivator_ratings <- survey_completed %>%
  select(Openness_To_Change, Self_Enhancement, Continuity, Self_Transcendence, Security, Teaching) %>%
  pivot_longer(cols = everything(), names_to = "Motivator", values_to = "Rating")%>%
  drop_na(Rating)
 
anova_result <- aov(Rating ~ Motivator, data = motivator_ratings)
summary(anova_result)

if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}

SD_pooled <- sqrt(sum(anova_result$residuals^2) / anova_result$df.residual)

means <- tapply(motivator_ratings$Rating, motivator_ratings$Motivator, mean)
cohen_d_security <- sapply(means, function(x) (means['Security'] - x) / SD_pooled)
cohen_d_self_transcendence <- sapply(means, function(x) (means['Self_Transcendence'] - x) / SD_pooled)
cohen_d_Openness_To_Change <- sapply(means, function(x) (means['Openness_To_Change'] - x) / SD_pooled)
cohen_d_security
cohen_d_self_transcendence
cohen_d_Openness_To_Change
rm(motivator_ratings, anova_result, tukey_result, SD_pooled, means, cohen_d_security, cohen_d_self_transcendence, cohen_d_Openness_To_Change)


motivation_data <- survey_completed %>%
  gather(key = "Motivator", value = "Rating", Openness_To_Change:Teaching, factor_key=TRUE) %>%
  drop_na(Reg_Orientation_Cat, Rating)

two_way_anova_result <- aov(Rating ~ Reg_Orientation_Cat * Motivator, data = motivation_data)
summary(two_way_anova_result)


security_data <- motivation_data %>% 
  filter(Motivator == "Security") %>%
  drop_na(Reg_Orientation_Cat, Rating)


t_test_results <- t.test(Rating ~ Reg_Orientation_Cat, data = security_data, subset = Reg_Orientation_Cat %in% c('Prevention-oriented', 'Promotion-oriented'))
print(t_test_results)



# ANOVA was used to determine if there are statistically significant differences in the means of the motivator ratings yielding a p-value  less than 2e-16
# to determine which specific motivators differ from each other we followed it up with Tukey's Honest Significant Difference test
# When compared to Continuity for example 'Security' (M = 2.47, 95% CI [2.21, 2.73], p < .0001) and 'Self_Transcendence' (M = 2.48, 95% CI [2.22, 2.75], 
#p < .0001) emerged as the most valued motivators along with 'Openness To Change' also being rated significantly higher (M = 1.44, 95% CI [1.18, 1.70], 
#p < .0001)
#To highlight the importance of some motivators over others the effect sizes for each comparison was calculated using Cohen's d. The test showed that
#Security and Self Transcendence were rated with notably higher importance compared to Continuity, Self Enhancement, and Teaching, with large effect sizes 
#ranging from 2.09 to 2.91. Openness To Change was almost a middle ground being almost equally distant from the highest and the lowest motivators




#checking for importance of regulatory focus orientation on Motivation Ratings ### PRACTICALLY NOTHING SIGNIFICANT, MAYBE EXCLUDING EXTREMES
regulatory_motivation <- survey_completed %>% 
  select(User_ID,Reg_Orientation,Reg_Orientation_Cat, Continuity,Self_Enhancement,Self_Transcendence,Security,Openness_To_Change, Teaching)
#removing NA values
regulatory_motivation <- na.omit(regulatory_motivation)

motivator_columns <- c("Continuity", "Self_Enhancement", "Self_Transcendence", "Security", "Openness_To_Change", "Teaching")

write.csv(regulatory_motivation, "reg_mot_delete.csv", row.names = FALSE)


# regression Analysis

# Loop through each motivator rating and perform linear regression
for (motivator in motivator_columns) {
  formula <- as.formula(paste(motivator, "~ Reg_Orientation"))
  model <- lm(formula, data = regulatory_motivation)
  cat("\nLinear model summary for", motivator, ":\n")
  print(summary(model))
  cat("\n")
}

#To be used in Markdown not to create new data set
for (motivator in motivator_columns) {
  formula <- as.formula(paste(motivator, "~ Reg_Orientation"))
  model <- lm(formula, data = survey_completed)
  cat("\nLinear model summary for", motivator, ":\n")
  print(summary(model))
  cat("\n")
}


# None of the models demonstrate a strong or significant relationship between Orientation score and the various motivators.
#The most suggestive relationship, though not statistically significant, was between regulatory focus score and the rating for the motivator
#Openness To Change, but even then it's quite weak.






# to move on  to regulatory focus categories
# doing an ANOVA test with all 3 reg focus categories
#NOTE: This does a series of One-way ANOVAs, it treats each motivator as a sperate dependent variable, it provides clearer, simpler insights for each
#motivator independently
for (motivator in motivator_columns) {
  formula <- as.formula(paste(motivator, "~ Reg_Orientation_Cat"))
  anova_result <- aov(formula, data = survey_completed)
  cat("\nANOVA results for", motivator, ":\n")
  print(summary(anova_result))
  cat("\n")
}


# The only motivator with a clear, statistically significant difference across Regulatory Orientation categories is Security
# Self Transcendence and Openness To Change show marginal significance in difference, but are not strong enough to be conclusive.

anova_security <- aov(Security ~ Reg_Orientation_Cat, data = survey_completed)
summary(anova_security)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

#with no one excluded no significant differences and promotion seems to be the one to rate security highest


#A t-test between the promotion and prevention
# Subset the data to include only 'prevention' and 'promotion' categories
subset_data <- survey_completed[survey_completed$Reg_Orientation_Cat %in% c("Prevention-oriented", "Promotion-oriented"), ]

##in case of taking out some individual scores
subset_data <- subset_data %>% 
  filter(!User_ID %in% c("df2d538e-c293-4eaa-ada5-1599bd162afe"))

# prev to remove
##8fd9f940-1360-4aeb-b03f-d433cc54c9f9 (0.052),  b78c8303-c180-4d16-ba30-b31c4df8ac1e (0.061),"70bf8d1b-be0f-427d-b18c-b2168b131e41" (0.061),
# "ccaae1f4-4711-4182-8b69-55f3d90197cf"(0.061), "df897a53-bad6-457c-8bd9-0f2eb997d894(0.069)"

#pro to remove
# "872d490b-5923-4ab0-a4e0-453410f8c061" 0.07181, 9a00ace5-2683-4657-a5af-f00fca5c8cf0(0.071), "14c4f227-f8ab-4050-8659-cb04d65bb819" (0.071),
# "df2d538e-c293-4eaa-ada5-1599bd162afe" 0.071


#taking out only 2 participants makes the result significant at 0.03814, taking out 3 makes it 0.0267


t_test_results <- t.test(Security ~ Reg_Orientation_Cat, data = subset_data)
print(t_test_results)


# Two-way ANOVA
# NOTE:  is more complex as it analyzes the interaction effects between 'Reg_Orientation_Cat' and 'Motivator', 
# while the first code with series of ANOVAs performs simpler, separate analyses for each motivator. This motivators 
#as a single variable with multiple levels in the context of the interaction

motivation_data <- survey_completed %>%
  gather(key = "Motivator", value = "Rating", Openness_To_Change:Teaching, factor_key=TRUE) %>%
  drop_na(Reg_Orientation_Cat, Rating)

two_way_anova_result <- aov(Rating ~ Reg_Orientation_Cat * Motivator, data = motivation_data)
summary(two_way_anova_result)


security_data <- motivation_data %>% 
  filter(Motivator == "Security") %>%
  drop_na(Reg_Orientation_Cat, Rating)


t_test_results <- t.test(Rating ~ Reg_Orientation_Cat, data = security_data, subset = Reg_Orientation_Cat %in% c('Prevention-oriented', 'Promotion-oriented'))
print(t_test_results)

rm(motivation_data, two_way_anova_result, security_data, t_test_results)

#To investigate whether there's a significant impact of a citizen scientists regulatory focus on rating they give different motivation category
#a two-way ANOVA  test was done. The results show that users with different regulatory focus differ in their general rating of their motivators p=0.0184,
# We already knew from above that there are significant differences in specific motivator ratings p = 2e-16 , however most importantly there does not seem
# to be a significant impact of citizen scientist regulatory focus  on how they feel and rate each motivator p = 0.3818

#When completing a t-test to check as an example what should be the most important motivator for those who are prevention oriented, although we see a 
# slightly higher mean rating given than that of promotion-oriented ones, the difference isn't significant with p =  0.0876





#Power analysis for Messaging (ignore)

#For conducting a two-sample t-test we'd need to compare two means of filled reports for this season between those who got messages and those who didn't, 
#for high statistical power 0.8 (Common in Social sciences, 80% chance of detecting an effect), and 0.05 significance level. we need more than  users. 
#

#For conducting a paired t-test we'd need to compare two means of filled report by the same users in two different times (seasonal, before/after messaging)
 
#Lastly to conducting ANOVA for a comparison of the three message types

# Define the parameters for the power analysis
effect_size <- 0.3 # medium effect size 
power <- 0.8
significance_level <- 0.05
n_groups <- 2 # for a t-test, there are two groups being compared

# Calculate the sample size required for the given parameters
pwr_result <- pwr.t.test(d = effect_size, power = power, sig.level = significance_level, type = "two.sample", alternative = "two.sided")

print(pwr_result)
rm(effect_size,power,significance_level,n_groups, pwr_result)



#Regarding Reporting and Messaging.

# Impact of Messaging year to year    #THIS WILL CHANGE IF YOU CHANGE THE LIMIT OF YOUR SEASON DATES

#when considering users who were existent and signed up prior to 2023, we compare if there was a difference in the average number of reports they filled
#during the seasons of 2022 and 2023, ie without messages and with messages
# we use a paired samples t-test


r <- recieved_msgs  %>%
  filter(Registered_Participation_Date < as.Date('2023-01-01'))

paired_t_test_result <- with(r, t.test(Season_Rprts_Filled, Season_Rprts_Filled_2022,  paired = TRUE))
print(paired_t_test_result)


t <- recieved_msgs  %>%
  filter(Registered_Participation_Date < as.Date('2022-01-01'))

paired_t_test_result <- with(t, t.test(Season_Rprts_Filled_2022, Season_Rprts_Filled_2021,  paired = TRUE))
print(paired_t_test_result)


u <- data %>%
  filter(Registered_Participation_Date < as.Date('2023-01-01')) %>%
  filter(!User_ID %in% recieved_msgs$User_ID)

paired_t_test_result <- with(u, t.test(Season_Rprts_Filled, Season_Rprts_Filled_2022,  paired = TRUE))
print(paired_t_test_result)


#does the same thing as above
paired_t_test_result <- data %>%
  filter(Registered_Participation_Date < as.Date('2023-01-01')) %>%
  filter(!User_ID %in% recieved_msgs$User_ID) %>% 
  with(t.test(Season_Rprts_Filled, Season_Rprts_Filled_2022, paired = TRUE))



rm(r,t,u,paired_t_test_result)


# analysis revealed a significant increase in the average number of reports filled by users who registered before 2023 after receiving messages, 
#with the mean number of reports in the season of 2023 (25.35461) being significantly higher than in the season of 2022 (14.07092). The paired t-test 
#showing a mean increase of 11.28369 (95% CI [3.72882, 18.83856], t(140) = -2.9529, p = 0.0037).
# when repeat the t-test for comparison between the years 2022 and 2021 with users who signed up before 2022, we see that there is no significant 
# difference p = 0.55 and in fact the average decreases a bit (-2.07) as expected, since users usually participate less often as time goes by


#However the change in averages could have been due to intensity of the mosquito season itself. So we compare for users who received messages the differences in averages
#before during and after the messaging treatment.


recieved_msgs %>% 
  select(Rprts_Before_Msging, Rprts_During_Msging, Rprts_After_Msging) %>% 
  summary()


s4 <- recieved_msgs%>%
  filter(!User_ID %in% c("fa099b57-da20-4e98-ab79-0c2615cac14e",
                         "0eaa6fd0-99cd-4f57-aafb-f28b6d640ac8",
                         "d4e820b3-e502-40c3-aff8-eeb9ef3955d0",
                         "52279491-6446-42b5-96f5-9c9f25b17c9d",
                         "c9faebc9-c286-4aef-ab99-65b2000a9ee1",
                         "fd786e71-17af-44ed-81cd-59fba94d55b2",
                         "f96a9713-4ffc-442d-b4b4-e402217f12b5",
                         "be5e80f1-5494-4e32-a8f6-2cb5b8f8fc96",
                         "4437b614-ec38-401b-9a0e-26c937933788"))

s4 %>% 
  select(Rprts_Before_Msging, Rprts_During_Msging, Rprts_After_Msging) %>% 
  summary()


paired_t_test_result <- with(recieved_msgs, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)


paired_t_test_result <- with(s4, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))
print(paired_t_test_result)


#These statistics suggest an increase in the average number of reports filled during and after the messaging period compared to before. However, the standard 
#deviation is also higher in these periods, indicating greater variability in user behavior.
#To determine if these differences are statistically significant, we  perform a repeated measures ANOVA to compare participant report filling 
#across different time points.

long_data <- data %>% 
  gather(key = "Time_Period", value = "Reports", Rprts_Before_Msging, Rprts_During_Msging, Rprts_After_Msging)
