# Loading Library

library(tidyverse)


#loading Data

Raw_Data <- read.csv("Results.csv")

colnames(Raw_Data)

Raw_Data <- Raw_Data %>% rename( Language = Start.language,
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
                                               Resentment = On.a.scale.of.1.to.5..please.rate.the.importance.of.the.following.reasons.for.your.participation.in.Mosquito.Alert..with.1.being.the.lowest.score.and.5.being.the.highest.score....I.dislike.mosquitos..,
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


str(Raw_Data)

#modifying Column type ···might be walready a date
Raw_Data <- Raw_Data %>% mutate(Participation_Date = as.factor(Participation_Date))
Raw_Data <- Raw_Data %>%  mutate(Participation_Date = ymd(substr(Participation_Date, 1, 10)))
Raw_Data <- Raw_Data %>% mutate(Country = if_else(Country == "", "I prefer not to say", Country))



#turning the Reg Focus Responses to Numeral
Raw_Data <- Raw_Data %>% mutate(Prom_1 = as.numeric(str_extract(Prom_1, "\\d+")),
                                              Prom_2 = as.numeric(str_extract(Prom_2, "\\d+")),
                                              Prom_3 = as.numeric(str_extract(Prom_3, "\\d+")),
                                              Prom_4 = as.numeric(str_extract(Prom_4, "\\d+")),
                                              Prom_5 = as.numeric(str_extract(Prom_5, "\\d+")),
                                              Prev_1 = as.numeric(str_extract(Prev_1, "\\d+")),
                                              Prev_2 = as.numeric(str_extract(Prev_2, "\\d+")),
                                              Prev_3 = as.numeric(str_extract(Prev_3, "\\d+")),
                                              Prev_4 = as.numeric(str_extract(Prev_4, "\\d+")),
                                              Prev_5 = as.numeric(str_extract(Prev_5, "\\d+")))

Raw_Data <- Raw_Data %>%
  mutate_at(vars(starts_with("Prom_"), starts_with("Prev_")), list(~as.numeric(str_extract(., "\\d+"))))



#creating age groups
Raw_Data <- Raw_Data %>%
  mutate(Age_Group = case_when(
    Age >= 18 & Age <= 25 ~ '18-25',
    Age >= 26 & Age <= 35 ~ '26-35',
    Age >= 36 & Age <= 45 ~ '36-45',
    Age >= 46 & Age <= 55 ~ '46-55',
    Age >= 56 & Age <= 65 ~ '56-65',
    Age >= 66 ~ '66 and older',
    TRUE ~ 'Unknown'  
  ))



#creating an average individual Reg Focus
Raw_Data <- Raw_Data %>% mutate(Reg_Orientation = (Prom_1+Prom_2+Prom_3+Prom_4+Prom_5 - Prev_1 - Prev_2- Prev_3- Prev_4- Prev_5))

str(Data)

#data cleaning and Exploration

#filtering to those who consented and completed the survey.

Data <- Raw_Data %>%
  filter(Last.page  == 5 & Consent == "Yes")

#Exploring the age of Participants

mean(Data$Age, na.rm = TRUE) # average age is 48.72687


ggplot(Data, aes(x = Age_Group)) +
  geom_bar(fill = 'blue') +
  labs(title = 'Frequency of Age Groups', x = 'Age Group', y = 'Frequency') +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

# Gender Distribution
gender_dist <- Data %>%
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
Country_dist <- Data %>%
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
Start_dist <- Data %>%
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

#checking the average Regulatory Focus
mean(Raw_Data$Reg_Orientation, na.rm = TRUE)



#before creating higher level orders lets see the various levels of each motivator.


STM_columns <- Raw_Data %>% select(Self_Direction:Env_Change)

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








#creating 4 high-order values of Schwartz

#Raw_Data <- Raw_Data %>%  mutate(Open_To_Change = (Self_Direction + Stimulation + Hedonism + Social_Expansion)/4) %>% mutate(Self_Enhancement = (Achievment + Power + Face)/3) %>% mutate(Cotinuity = (Security + Conformity + Routine)/3) %>% mutate(Self_Transcendence = (Universalism_Social + Universalism_Nature + Benevolence + Help_Science)/4 )
                                 
  

Raw_Data <- Raw_Data %>%
  mutate(Open_To_Change = (Achievement + Power + Hedonism)/3) %>%
  mutate(Self_Enhancement = (Self_Direction + Stimulation + face) %>%
  mutate(Continuity = (Security + Conformity)/3) %>%
  mutate(Self_Transcendence = (Universalism_Social + Universalism_Nature + Benevolenc+ Help_Science)/4)                                 

mean(Raw_Data$Reg_Orientation, na.rm = TRUE)
Raw_Data <- Raw_Data %>% mean(

 Routine, help with research/ contriute to science, social expansion, motivation to teach

Data <- Raw_Data %>% filter(Last.page == 5)


summary(Data)
