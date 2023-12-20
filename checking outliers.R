
#Checking outliers


#total reports filled, lower upper 10 percent
Q1_total <- quantile(received_msgs$Total_Rprts_Filled, 0.1)
Q3_total <- quantile(received_msgs$Total_Rprts_Filled, 0.9)
IQR_total <- Q3_total - Q1_total

lower_bound_total <- Q1_total - 1.5 * IQR_total
upper_bound_total <- Q3_total + 1.5 * IQR_total

outliers_total <- received_msgs[received_msgs$Total_Rprts_Filled < lower_bound_total | received_msgs$Total_Rprts_Filled > upper_bound_total, ]
print(outliers_total$User_ID)

rm(Q1_total,Q3_total, IQR_total,lower_bound_total,upper_bound_total,outliers_total)


#reports filed in 2023, lower upper 10 percent
Q1_2023 <- quantile(received_msgs$Rprts_Filled_2023, 0.1)
Q3_2023 <- quantile(received_msgs$Rprts_Filled_2023, 0.9)
IQR_2023 <- Q3_2023 - Q1_2023

lower_bound_2023 <- Q1_2023 - 1.5 * IQR_2023
upper_bound_2023 <- Q3_2023 + 1.5 * IQR_2023

outliers_2023 <- received_msgs[received_msgs$Rprts_Filled_2023 < lower_bound_2023 | received_msgs$Rprts_Filled_2023 > upper_bound_2023, ]
print(outliers_2023$User_ID)

rm(Q1_2023,Q3_2023, IQR_2023,lower_bound_2023,upper_bound_2023,outliers_2023)


#if necessary to take out outliers, it makes more sense to remove the ones above 90th percentile in 2023 since that is the time frame of the analysis
#when it comes to GLMER analysis, 2023 season might also be considered.

## a ggplot to visualize the quantile
ggplot(received_msgs, aes(x = Rprts_Filled_2023)) +
geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of number of reports filled in 2023",
       x = "Number of Reports Filled",
       y = "Number of Participants") +
  theme_minimal() +
  xlim(0, 100) +
  geom_vline(xintercept = quantile(received_msgs$Rprts_Filled_2023, 0.1), linetype = "dashed", color = "red") +
  geom_vline(xintercept = quantile(received_msgs$Rprts_Filled_2023, 0.25), linetype = "dashed", color = "green") +
  geom_vline(xintercept = quantile(received_msgs$Rprts_Filled_2023, 0.5), linetype = "dashed", color = "orange") +
  geom_vline(xintercept = quantile(received_msgs$Rprts_Filled_2023, 0.75), linetype = "dashed", color = "purple") +
  geom_vline(xintercept = quantile(received_msgs$Rprts_Filled_2023, 0.9), linetype = "dashed", color = "blue")



