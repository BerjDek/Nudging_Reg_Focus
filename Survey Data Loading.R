# Loading Library

library(tidyverse)


#loading Data

raw_survey_data <- read.csv("Results.csv")

colnames(raw_survey_data)

raw_survey_data <- raw_survey_data %>% rename( Language = Start.language,
                                               Consent = I.GIVE.MY.CONSENT.to.participate.in.this.study.and.allow.the.use.of.data.generated.in.Mosquito.Alert.on.my.device.to.be.re.used.in.this.research.project..,
                                               User_ID = user_UUID,
                                               Age = How.old.are.you.,
                                               Gender = What.is.your.gender.,
                                               Country = What.is.the.country.you.currently.reside.in.,
                                               Participation_Date = In.what.year.did.you.first.participate.in.Mosquito.Alert.,
                                               Network = How.many.people.do.you.personally.know..acquaintances..friends..family.members.etc...who.are.participating.in.Mosquito.Alert..not.including.yourself..,
                                               Other_Citi_Sci = Are.you.currently.engaged.in.other.citizen.science.projects.,
                                               Self_Direction = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.am.interested.in.the.topic.of.this.project.,
                                               Stimulation = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.challenge.myself.and.do.something.new.,
                                               Hedonism = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.s.a.fun.activity.,
                                               Achievement = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.s.an.opportunity.to.perform.better.than.others.,
                                               Face = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.enhance.my.reputation.,
                                               Security = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.live.in.safer.surroundings.,
                                               Conformity = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....Other.people.I.know.are.participating..,
                                               Benevolence = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.contribute.to.my.community.,
                                               Universalism_Social = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.make.the.world.a.better.place.,
                                               Universalism_Nature = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.protect.the.environment.,
                                               Routine = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....It.is.part.of.my.routine.,
                                               Social_Expansion = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.be.part.of.this.volunteers..community.,
                                               Power =On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.gain.recognition.,
                                               Help_Science = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.want.to.contribute.to.science.,
                                               Teaching = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.use.it.to.teach.others.about.the.topic.,
                                               Dislike = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.dislike.mosquitos..,
                                               Env_Change = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.noticed.an.increase.or.change.in.mosquitos.in.my.surroundings.,
                                               Prom_1 = I.prefer.to.work.without.instructions.from.others.,
                                               Prev_1 = Rules.and.regulations.are.helpful.and.necessary.for.me,
                                               Prev_2 = For.me..it.is.very.important.to.carry.out.the.obligations.placed.on.me.,
                                               Prom_2 = I.generally.solve.problems.creatively.,
                                               Prev_3 = I.m.not.bothered.about.reviewing.or.checking.things.really.closely.,
                                               Prom_3 = I.like.to.do.things.in.a.new.way.,
                                               Prev_4 = I.always.try.to.make.my.work.as.accurate.and.error.free.as.possible.,
                                               Prom_4 = I.like.trying.out.lots.of.different.things..and.am.often.successful.in.doing.so.,
                                               Prom_5 = It.is.important.to.me.that.my.achievements.are.recognized.and.valued.by.other.people.,
                                               Prev_5 = I.often.think.about.what.other.people.expect.of.me.)





raw_survey_data <- raw_survey_data %>% mutate(Country = if_else(Country == "", "I prefer not to say", Country))

raw_survey_data$User_ID <- sub(" target=", "", raw_survey_data$User_ID) #removed target= which was appearing at the end of some uuid's

#turning the Reg Focus Responses to Numeral
raw_survey_data <- raw_survey_data %>% mutate(Prom_1 = as.numeric(str_extract(Prom_1, "\\d+")),
                                              Prom_2 = as.numeric(str_extract(Prom_2, "\\d+")),
                                              Prom_3 = as.numeric(str_extract(Prom_3, "\\d+")),
                                              Prom_4 = as.numeric(str_extract(Prom_4, "\\d+")),
                                              Prom_5 = as.numeric(str_extract(Prom_5, "\\d+")),
                                              Prev_1 = as.numeric(str_extract(Prev_1, "\\d+")),
                                              Prev_2 = as.numeric(str_extract(Prev_2, "\\d+")),
                                              Prev_3 = as.numeric(str_extract(Prev_3, "\\d+")),
                                              Prev_4 = as.numeric(str_extract(Prev_4, "\\d+")),
                                              Prev_5 = as.numeric(str_extract(Prev_5, "\\d+")))


raw_survey_data <- raw_survey_data %>%
  mutate_at(vars(starts_with("Prom_"), starts_with("Prev_")), list(~as.numeric(str_extract(., "\\d+"))))

raw_survey_data <- raw_survey_data %>% mutate(Network = as.numeric(Network))

raw_survey_data <- raw_survey_data %>%  mutate(Participation_Date = as.factor(Participation_Date))

# Converting Other_Citi_Sci to a binary variable
raw_survey_data <- raw_survey_data %>%
  mutate(Other_Citi_Sci = ifelse(Other_Citi_Sci == "Yes", 1, 0))


#creating age groups
raw_survey_data <- raw_survey_data %>%
  mutate(Age_Group = case_when(
    Age >= 18 & Age <= 25 ~ '18-25',
    Age >= 26 & Age <= 35 ~ '26-35',
    Age >= 36 & Age <= 48 ~ '36-48',
    Age >= 49 & Age <= 62 ~ '49-62',
    Age >= 63 ~ '63 and older',
    TRUE ~ 'Unknown'  
  ))

raw_survey_data <- raw_survey_data %>% mutate(Age_Group = as.factor(Age_Group))

#creating an average individual Reg Focus
raw_survey_data <- raw_survey_data %>% mutate(Reg_Orientation = (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5 - Prev_1 - Prev_2- Prev_3- Prev_4- Prev_5))


# Adding categorical version of Reg_Orientation
raw_survey_data <- raw_survey_data %>%
  mutate(Reg_Orientation_Cat = case_when(
    Reg_Orientation < 0 ~ "Prevention-oriented",
    Reg_Orientation == 0 ~ "Neutral",
    Reg_Orientation > 0 ~ "Promotion-oriented",
    TRUE ~ as.character(NA)  # For NAs
  ))
raw_survey_data <- raw_survey_data %>% mutate(Reg_Orientation_Cat = as.factor(Reg_Orientation_Cat))


#Creating 4 higher level orders from CSMS of Levontin et.al

raw_survey_data <- raw_survey_data %>%
  mutate(Openness_To_Change = round((Self_Direction + Stimulation + Social_Expansion+Hedonism)/4,2)) %>%
  mutate(Self_Enhancement = round((Achievement + Power + Face)/3,2)) %>%
  mutate(Continuity = round((Routine + Conformity)/2,2)) %>%
  mutate(Self_Transcendence = round((Universalism_Social + Universalism_Nature + Benevolence+ Help_Science)/4,2))  

#rearranging 
raw_survey_data <- raw_survey_data %>% 
  select(Response.ID, Last.page, Language, Consent, User_ID, Age, 
         Age_Group, Gender, Country, Participation_Date, Network, 
         Other_Citi_Sci, Reg_Orientation,Reg_Orientation_Cat, 
         Openness_To_Change, Self_Enhancement, Continuity, Self_Transcendence, 
         Security, Teaching, Self_Direction, Stimulation, Hedonism, 
         Achievement, Face, Conformity, Benevolence, Universalism_Social, 
         Universalism_Nature, Routine, Social_Expansion, Power, 
         Help_Science,Dislike, Env_Change, Prom_1, Prom_2, Prom_3, 
         Prom_4, Prom_5,  Prev_1, Prev_2, Prev_3, Prev_4, Prev_5) %>% 
  mutate( Age_Group = as.factor(Age_Group),
          Gender = as.factor(Gender),
          Country = as.factor(Country))
                                        
#data cleaning and Exploration

#filtering to those who consented and completed the survey, and their UUID has been registered.

survey_data <- raw_survey_data %>%
  filter(Last.page  == 5 & Consent == "Yes" & nzchar(User_ID) > 0)

#removed the entries by users that have filled the survey twice, maintaining the results of their first attempt and deleting repeats.
survey_data <- survey_data %>%
  group_by(User_ID) %>%
  filter(row_number() == 1) %>%
  mutate(Complt_Survey = TRUE) %>% 
  ungroup()



nrow(raw_survey_data %>% filter(Consent == "Yes" & nzchar(User_ID))%>%
  group_by(User_ID) %>%
  filter(row_number() == 1) %>%
  ungroup())
str(survey_data)

## There are 237 consents that that have provided User Id's, after removing 7 that have done the survey twice. 

#from the original 450 Observers there are 217 that have completed the entire survey, and provided their 
#User IDs and whose entries will be analyzed. The total number of users that have provided consent and ID, 
#making them eligible for the messaging experiment (without necessary knowledge of their regulatory focus) is 237, 
#confirming the validity of the Messaging Data





#Exploring the age of Participants

mean(survey_data$Age, na.rm = TRUE) # average age is 48.72687


ggplot(survey_data, aes(x = Age_Group)) +
  geom_bar(fill = 'blue') +
  labs(title = 'Frequency of Age Groups', x = 'Age Group', y = 'Frequency') +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

# Gender Distribution
gender_dist <- survey_data %>%
  group_by(Gender) %>%
  summarize(Frequency = n()) %>%
  arrange(desc(Frequency))  #128 males to 93 females 


ggplot(gender_dist, aes(x = reorder(Gender, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Frequency), vjust = -0.5, size = 4) +  # Add frequency labels on top of bars
  labs(title = "Gender Distribution", x = "Gender", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# country distribution
Country_dist <- survey_data %>%
  group_by(Country) %>%
  summarize(Frequency = n()) %>%
  arrange(desc(Frequency))

ggplot(Country_dist, aes(x = reorder(Country, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Frequency), vjust = -0.5, size = 4) +  # Add frequency labels on top of bars
  labs(title = "Country Distribution", x = "Country", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
 #majority of participates are from Spain(107) and Italy (81) with Netherlanders coming in 3rd (22)



# start date for participation
Start_dist <- survey_data %>%
  group_by(Participation_Date) %>%
  summarize(Frequency = n()) %>%
  arrange(desc(Frequency))

ggplot(Start_dist, aes(x = reorder(Participation_Date, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Frequency), vjust = -0.5, size = 4) +  # Add frequency labels on top of bars
  labs(title = "Participant Start Date Distribution", x = "Start Year", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

#the users weren't as skewed to 2023 with majority at 75 but not that far off from 2022 (57) and 2021(41)


# Network & other citisci

Network_dist <- survey_data %>%
  group_by(Network) %>%
  summarize(Frequency = n()) %>%
  arrange(desc(Network))

ggplot(Network_dist, aes(x = Network, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Frequency), vjust = -0.5, size = 4) +  # Add frequency labels on top of bars
  labs(title = "Participant Network Distribution", x = "Network of Other Participants", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


#Most of the users don't know anyone else that is using the app.

#checking the average Regulatory Focus
mean(raw_survey_data$Reg_Orientation, na.rm = TRUE)



#before creating higher level orders lets see the various levels of each motivator.


STM_columns <- raw_survey_data %>% select(Security:Env_Change)

STM_averages <- STM_columns %>%
  summarise_all(~ mean(., na.rm = TRUE)) %>%
  gather(key = "Variable", value = "Average") %>%
  arrange(desc(Average))

ggplot(STM_averages, aes(x = reorder(Variable, Average), y = Average)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +  
  theme_minimal() +
  labs(x = "Motivator", y = "Average") +
  geom_text(aes(label = round(Average, 2)), hjust = -0.2, size = 3) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









