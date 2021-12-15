# Load libraries
library(tidyverse)
library(here)

# Change dplyr settings so I can view all columns 
options(dplyr.width = Inf)

# Import cleaned data
choiceData <- read_csv(here("data","choiceData.csv"))

# Keep only the respondent information
respData <- choiceData %>% 
    select(videotime:platform) %>% 
    # remove the duplicates created when we did pivot_longer
    distinct()
nrow(respData)

# respData now has all the respondents included in the model

# with manual checking, there are 3 NAs for race
#       of those three, one has NA for raceOther
# 2 NAs for state
# 25 NAs for adults
# 158 NAs for minors
# 8 NAs for platform

# VIDEOTIME
plot_video <- respData %>% 
    select(videotime) %>% 
    mutate(watched = ifelse(videotime > 120, " Watched", "Skipped")) %>% 
    ggplot(aes(x = watched))+
    geom_bar()+
    labs(x = "Educational Video", y = "Respondents")

# YEARDOB
plot_dob <- respData %>% 
    select(yearDOB) %>% 
    filter(!is.na(yearDOB), yearDOB != "prefer_n_s") %>% 
    mutate(yearDOB = as.double(yearDOB)) %>% 
    ggplot(aes(x = yearDOB))+
    geom_histogram(binwidth = 5)+
    labs(x = "Birth Years", y = "Respondents")

# GENDER
labels_gender <- c("NR","Man","Woman","Other")
plot_gender <- respDaata %>% 
    select(gender) %>% 
    left_join(data.frame(
        gender = c("man","woman","other","prefer_n_s"),
        genderOrder = c("bman","cwoman","other","aNR")),
        by = "gender") %>% 
    ggplot(aes(x = genderOrder, fill = genderOrder))+
    geom_bar()+
    labs(x = "Gender", y = "Respondents")+
    scale_x_discrete(labels = labels_gender)+
    theme(legend.position = "none")

# RACE & RACEOTHER
raceData <- respData %>% 
    mutate(race = ifelse(!is.na(race),race,raceOther),
           race = ifelse(is.na(race),"prefer_n_s",race)) %>% 
    select(race) %>% 
    mutate(asian = ifelse(str_detect(race, "asian"),1,0),
           black = ifelse(str_detect(race, "black"),1,0),
           hispanic = ifelse(str_detect(race,"hispanic"),1,0),
           mideast = ifelse(str_detect(race, "Middle Easter"),1,0),
           native = ifelse(str_detect(race, "native"),1,0),
           pacific = ifelse(str_detect(race, "pacific"),1,0),
           white = ifelse(str_detect(race, "white"),1,0),
           nr = ifelse(str_detect(race, "n_s"),1,0)
           )
raceTable <- data.frame(
    "Race"=c("Asian","Black","Hispanic","Native",
             "Middle Eastern","Pacific Islander",
             "White","Not Reported"),
    "Respondents" =c(
        sum(raceData$asian),    sum(raceData$black),
        sum(raceData$hispanic), sum(raceData$mideast),
        sum(raceData$native),   sum(raceData$pacific),
        sum(raceData$white),    sum(raceData$nr))
    ) %>% 
    mutate(Percent = scales::label_percent(accuracy = 0.1)
                     (Respondents/nrow(respData))
           )

# STATE
respData %>% 
    select(state) %>% 
    ggplot(aes(x = state,fill = state))+
    geom_bar()
# generally reflective of population trends

# EDUCATION
labels_edu <- c("No H.S.","H.S.","some College","Vocational",
                "Associate Deg.","Bachelor's","Graduate")
plot_education <- respData %>% 
    select(education) %>% 
    left_join(data.frame(
        education = c("no_hs","hs","college_some","vocational",
                      "degree_associate","degree_bs","degree_grad","prefer_n_s"),
        eduOrder = c("a_no_hs","b_hs","college_some","d_vocational",
                     "e_associate","f_bs","graduate","NR")),
        by = "education") %>% 
    ggplot(aes(x = eduOrder, fill = eduOrder))+
    geom_bar()+
    labs(x = "Education Level", y = "Respondents")+
    scale_x_discrete(labels = labels_edu)+
    theme(legend.position = "none")

# INCOME
labels_inc <- c("< 10","10 - 15","15 - 25","25 - 35","35 - 50",
                "50 - 75","75 - 100","100 - 150","150 - 200",
                "> 200","NR")
plot_income <- respData %>% 
    select(income) %>% 
    ggplot(aes(x = income, fill = income))+
    geom_bar()+
    labs(x = "Income ($1,000)", y = "Respondents")+
    scale_x_discrete(labels = labels_inc)+
    theme(legend.position = "none")

# ADULTS & MINORS
plot_adults <- respData %>% 
    select(adults) %>% 
    filter(!is.na(adults)) %>% 
    mutate(adults = ifelse(adults>=5,"5+",adults)) %>% 
    ggplot(aes(x = adults))+
    geom_bar()+
    labs(x = "Adults in Respondent's Household", y = "Respondents")
#  25 NAs in adults
# 158 NAs in minors

# FREQUENCYPRIOR & FREQUENCYSINCE

# PLATFORM