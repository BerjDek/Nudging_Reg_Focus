#checkin if survey is representative

# Values
population_size <- 265227  # Total population that downloaded the app
sample_size <- 217         # Sample size who completed the survey
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
invitations_seen <- 450
attempts_made <- 237
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




#Checking if the Regulatory focus Average is significant

orientation_counts <- table(survey_completed$Reg_Orientation_Cat)
orientation_counts <- orientation_counts[names(orientation_counts) != "NA"]

chi_squared_test <- chisq.test(orientation_counts)
print(chi_squared_test)


oriented_data <- subset(survey_completed, Reg_Orientation_Cat %in% c('Prevention-oriented', 'Promotion-oriented'))
oriented_counts <- table(oriented_data$Reg_Orientation_Cat)
oriented_counts <- oriented_counts[names(oriented_counts) != "Neutral"]

# Since we expect the counts to be equal, the expected frequency for each is half of the total count
total_expected_count <- sum(oriented_counts)
expected_counts <- rep(total_expected_count / 2, 2)

chi_squared_test <- chisq.test(oriented_counts, p = rep(1/2, 2))
print(chi_squared_test)

rm(orientation_counts, chi_squared_test, oriented_data, total_expected_count, expected_counts)


# The Chi-squared test is used to determine  whether the distribution of users across three regulatory orientation categories—
#'prevention-oriented,' 'promotion-oriented,' and 'neutral'—deviates significantly from a uniform distribution.
#The test yielded a a p-value of approximately 5.407e-15 suggesting  that the distribution of users' regulatory orientation categories 
#is significantly different from a uniform one

# To further assess if the number of promotion-oriented users is significantly different from the number of prevention-oriented users we performed 
# a Chi-squared test with the assumption that both categories should have equal numbers of participants, yeilded a significant  P-value of 0.0002




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

# ANOVA was used to determine if there are statistically significant differences in the means of the motivator ratings yeilding ap-value is less than 2e-16
# to determine which specific motivators differ from each other we followed it up with Tukey's Honest Significant Difference test
# When compared to Continuity for example 'Security' (M = 2.47, 95% CI [2.21, 2.73], p < .0001) and 'Self_Transcendence' (M = 2.48, 95% CI [2.22, 2.75], 
#p < .0001) emerged as the most valued motivators along with 'Openness To Change' also being rated significantly higher (M = 1.44, 95% CI [1.18, 1.70], 
#p < .0001)
#To highlight the importance of some motivators over others the effect sizes for each comparison was calculated using Cohen's d. The test showed that
#Security and Self Transcendence were rated with notably higher importance compared to Continuity, Self Enhancement, and Teaching, with large effect sizes 
#ranging from 2.09 to 2.91. Openness To Change was almost a midlle ground being almost equally distant from the highest and the lowest motviators






#test whether there's a significant impact of regulatory focus on motivation category rating

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

#To investigate whether there's a significant impact of a citizen scientists regulatory focus on rating they give different motivation categorys
#a two-way ANOVA  test was done. The results show that users with different regulatory focus differ in their general rating of their motivators p=0.0184,
# We already knew from above that there are significant differences in specific motivator ratings p = 2e-16 , however most importantly there does not seem
# to be a significant impact of citizen scientist regulatory focus  on how they feel and rate each motivator p = 0.3818

#When completing a t-test to check as an example supposedly the most important motivator for those who are prevention oriented, although we see a 
# slightly higher mean rating given than that of promotion-oriented ones, the difference isn't significant with p =  0.0876