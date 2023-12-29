# Loading Library
install.packages("pwr")
install.packages("multcomp")

library(tidyverse)
library(car)
library(scales)
library(broom)
library(pwr)
library(lme4)
library(MASS)
library(multcomp)
#loading Data

raw_survey_data <- read.csv("resultsss.csv")

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



raw_survey_data <- raw_survey_data %>%
  mutate_at(vars(starts_with("Prom_"), starts_with("Prev_")), list(~as.numeric(str_extract(., "\\d+")))) #turning the Reg Focus Responses to Numeral


raw_survey_data <- raw_survey_data %>% mutate(Network = as.numeric(Network))

raw_survey_data <- raw_survey_data %>%  mutate(Participation_Date = as.factor(Participation_Date))


raw_survey_data <- raw_survey_data %>%
  mutate(Other_Citi_Sci = ifelse(Other_Citi_Sci == "Yes", 1, 0)) # Converting Other_Citi_Sci to a binary variable


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
    Reg_Orientation < 0 ~ "Prevention",
    Reg_Orientation == 0 ~ "Neutral",
    Reg_Orientation > 0 ~ "Promotion",
    TRUE ~ as.character(NA)  # For NAs
  ))
raw_survey_data <- raw_survey_data %>% mutate(Reg_Orientation_Cat = as.factor(Reg_Orientation_Cat))


#Creating 4 higher level orders from CSMS of Levontin et.al

raw_survey_data <- raw_survey_data %>%
  mutate(Openness_To_Change = round((Self_Direction + Stimulation + Social_Expansion+Hedonism)/4,2)) %>%
  mutate(Self_Enhancement = round((Achievement + Power + Face)/3,2)) %>%
  mutate(Continuity = round((Routine + Conformity)/2,2)) %>%
  mutate(Self_Transcendence = round((Universalism_Social + Universalism_Nature + Benevolence+ Help_Science)/4,2))  

colnames(raw_survey_data)

#rearranging 
raw_survey_data <- raw_survey_data %>% 
  dplyr::select(Response.ID, Last.page, Language, Consent, User_ID, Age, 
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

## There are 238 consents that that have provided User Id's, after removing 8 that have done the survey twice. 

#from the original 460 Observers there are 217 that have completed the entire survey, and provided their 
#User IDs and whose entries will be analyzed. The total number of users that have provided consent and ID, 
#making them eligible for the messaging experiment (without necessary knowledge of their regulatory focus) is 238, 
#confirming the validity of the Messaging Data

write.csv(survey_data, "CleanSurveydData.csv", row.names = FALSE)










