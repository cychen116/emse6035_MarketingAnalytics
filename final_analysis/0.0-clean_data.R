# Load libraries
library(fastDummies)
library(here)
library(lubridate)
library(tidyverse)

# Change dplyr settings so I can view all columns 
options(dplyr.width = Inf)

# Import raw data
p1 <- read_csv(here("data", "Final_Survey_Part1.csv"))
p2 <- read_csv(here("data", "Final_Survey_Part2.csv"))
p3 <- read_csv(here("data", "Final_Survey_Part3.csv"))

# Screening and Consent section
p1 <- p1 %>% 
    # Compute time value for this section, replace NAs with 0
    mutate(
        created = ymd_hms(created, tz = "EST"),
        ended = ymd_hms(ended, tz = "EST"),
        time_sec_p1 = as.numeric(ended - created, units = "secs"),
        time_sec_p1 = ifelse(is.na(time_sec_p1), 0, time_sec_p1)
    ) %>% 
    # Filter out responses that are before the survey was fielded
    filter(created > ymd("2021-11-23")) %>% 
    # Filter out NA session values
    filter(!is.na(session)) %>% 
    # drop unnecessary variables
    select(session, time_sec_p1, screenoutLoc,
           screenoutUser, consentAge, consentUnderstand)

# Choice Questions section
p2 <- p2 %>% 
    # immediately drop the humongous variables
    select(-df_json, -df) %>% 
    # Compute time value for this section, replace NAs with 0
    mutate(
        created = ymd_hms(created, tz = "EST"),
        ended = ymd_hms(ended, tz = "EST"),
        time_sec_p2 = as.numeric(ended - created, units = "secs"),
        time_sec_p2 = ifelse(is.na(time_sec_p2), 0, time_sec_p2)
    ) %>% 
    # Calculate the time spent on each question
    mutate(
        videotime = as.numeric(time4 - time2, units = "secs"),
        cbc1time = as.numeric(time8 - time7, units = "secs"),
        cbc2time = as.numeric(time9 - time8, units = "secs"),
        cbc3time = as.numeric(time10 - time9, units = "secs"),
        cbc4time = as.numeric(time11 - time10, units = "secs"),
        cbc5time = as.numeric(time12 - time11, units = "secs"),
        cbc6time = as.numeric(time13 - time12, units = "secs"),
        cbc7time = as.numeric(time14 - time13, units = "secs"),
        cbc8time = as.numeric(ended - ymd_hms(time14, tz = "EST"), units = "secs")
    ) %>% 
    # Filter out NA session values
    filter(!is.na(session)) %>% 
    # drop unnecessary variables
    select(session, time_sec_p2, respondentID, videotime, starts_with("cbc"))

# Respondent Information section
p3 <- p3 %>% 
    # Compute time value for this section, replace NAs with 0
    mutate(
        created = ymd_hms(created, tz = "EST"),
        ended = ymd_hms(ended, tz = "EST"),
        time_sec_p3 = as.numeric(ended - created, units = "secs"),
        time_sec_p3 = ifelse(is.na(time_sec_p3), 0, time_sec_p3)
    ) %>% 
    # Filter out NA session values
    filter(!is.na(session)) %>% 
    # drop unnecessary variables
    select(session, time_sec_p3, yearDOB:platform)

# Join all parts together using the session variable
data <- p1 %>% 
    left_join(p2, by = "session") %>% 
    left_join(p3, by = "session") %>% 
    # No longer need session variable
    select(-session)
nresp <- nrow(data)
head(data)
view(data)

# Filter out bad responses ------------------

# Drop people who got screened out
data <- data %>% 
    filter(screenoutLoc == 1) %>% 
    filter(!is.na(screenoutUser), screenoutUser != 1)
nresp_screened <- nrow(data)

# Drop people who did not complete the consent
data <- data %>% 
    filter(consentAge == 1, consentUnderstand == 1)
nresp_consented <- nrow(data)

# Drop anyone who didn't complete all choice questions
data <- data %>% 
    filter(!is.na(cbc1)) %>% 
    filter(!is.na(cbc2)) %>% 
    filter(!is.na(cbc3)) %>% 
    filter(!is.na(cbc4)) %>% 
    filter(!is.na(cbc5)) %>% 
    filter(!is.na(cbc6)) %>% 
    filter(!is.na(cbc7)) %>% 
    filter(!is.na(cbc8))
nresp_completed <- nrow(data)

# Drop respondents that always chose the same option
data <- data %>% 
    filter(cbcAllSame == 0)
nresp_realansws <- nrow(data)

# Drop respondents who went too fast
data <- data %>% 
    mutate(
        # Now compute the total time
        time_min_total = (time_sec_p1 + time_sec_p2 + time_sec_p3) / 60
    )
# Look at summary of completion times
summary(data$time_min_total)
# Drop anyone who finished in under the 5th percentile of completion times
time_0.05 <- quantile(data$time_min_total, 0.05)
data <- data %>% 
    filter(time_min_total >= time_0.05)
nresp_enoughtime <- nrow(data)

# Drop respondents that spent less than 3 seconds on any choice question
data <- data %>% 
    filter(cbc1time >= 3, cbc2time >= 3, cbc3time >= 3,
           cbc4time >= 3, cbc5time >= 3, cbc6time >= 3,
           cbc7time >= 3, cbc8time >= 3)
nresp_thoughtful <- nrow(data)



# Create choice data ------------------

# First gather the data
choiceData <- data %>% 
    rename(
        cbc1choice = cbc1, cbc2choice = cbc2, cbc3choice = cbc3,
        cbc4choice = cbc4, cbc5choice = cbc5, cbc6choice = cbc6,
        cbc7choice = cbc7, cbc8choice = cbc8
    ) %>% 
    pivot_longer(
        cols = c(cbc1time:cbc8time,cbc1choice:cbc8choice),
        names_to = c("qID",".value"),
        names_pattern = "cbc(.)(.*)") %>% 
    # Convert the qID variable to a number
    mutate(qID = parse_number(qID))

# Read in choice questions and join it to the choiceData
survey <- read_csv("https://raw.githubusercontent.com/omaoanalytics/document/main/final_choice_questions.csv")
choiceData <- choiceData %>% 
    rename(respID = respondentID) %>% 
    left_join(survey, by = c("respID", "qID"))

# Convert choice column to 1 or 0 based on if the alternative was chosen 
choiceData <- choiceData %>% 
    mutate(choice = ifelse(choice == altID, 1, 0))

# Drop unused variables
choiceData <- choiceData %>% 
    select(-image, -cbcPractice, -cbcAllSame, -setOptions2,
           -contains("screenout"), -contains("consent")) %>% 
    select(contains("time_sec"),time_min_total,videotime,
           yearDOB:platform,time,qID,respID,choice:setOptions)

# Set new values for respID and obsID
nRespndnts <- nrow(data)
nAlts <- max(survey$altID)
nQtions <- max(survey$qID)
choiceData$respID <- rep(seq(nRespndnts), each = nAlts*nQtions)
choiceData$obsID  <- rep(seq(nRespndnts*nQtions), each = nAlts)

# Save cleaned data for modeling
write_csv(choiceData, here("data", "choiceData.csv"))

load(
    here("models", "mnl_setprice.RData")
)