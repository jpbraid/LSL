library(tidyverse)
library(lubridate)

##############################################
master_file <- "Infrastructure_proj.csv"
retirement_file <- "Retirementrate_PSSCSS_2017.csv"
valuation_date <- dmy("01/06/2019")
eligible_year <- 10 # number of years of service required before eligible for LSL
start_rate <- 0.09
reduce_rate <- 0.01
end_rate <- 0.06
LSL_DATA = "S:/Projection/"
LSL_VAL = "S:/Projection/LSL Valuation/"
##############################################

# read in the data
agency_data <- read_csv(paste0(LSL_DATA, master_file)) ## corresponds to &filename._temp
retirement_data <- read_csv(paste0(LSL_VAL, retirement_file)) ## corresponds to LSLVAL.&Retfile data set and RETIREMENTRATE table

# do some wrangling of the original data
## this dataframe corresponds to the &filename.val data set
agency_val <- agency_data %>% 
  select(-c(Duration, Exit_Rate))
filter(!is.na(Birthdate), !is.na(APS_Start), str_sub(Super, 1L, 1L) %in% c("P", "C")) %>%
  mutate(Birthdate = dmy(Birthdate), 
         APS_Start = dmy(APS_Start),
         Jage = as.numeric((valuation_date - Birthdate)/365.25),
         Jservice = as.numeric((valuation_date - APS_Start)/365.25),
         age = as.integer(Jage),
         service = as.integer(Jservice)) %>%
  filter(age >= 16, service <= 55)

# extract some variables (used in downstream analysis)
R_min_age <- min(retirement_data$Age)
R_max_age <- max(retirement_data$Age)
#min_duration <- min(agency_data$Duration, na.rm = TRUE)
#max_duration <- max(agency_data$Duration, na.rm = TRUE)
#N_datarec <- nrow(agency_data)
#C_minage <- min(agency_val$age)
#laps <- R_maxage - C_minage

# separate out the exit rate data from the rest of the input data
# also define the takeup rates for each year of eligibility
exit_rates <- agency_data %>% filter(complete.cases(agency_data)) %>% select(Exit_Rate) %>% unlist() 
takeup_rates <- ifelse(start_rate - (0:100)*reduce_rate >= end_rate, start_rate - (0:100)*reduce_rate, end_rate)

# for now let's just simplify things by assuming one set of retirement rates:
retirement_rates <- retirement_data$Retirement_PSS %>% unlist()

## LOOK AT A GIVEN OBSERVATION
current_record <- agency_val[1, ]

# set up params for analysis
start_age <- current_record$age
start_service <- current_record$service
LSL <- current_record$LSL
analysis_length <- R_max_age - start_age + 1
sum_taken <- 0
age <- start_age
service <- start_service
vector(length = analysis_length) -> qx -> px # -> taken_this_year # etc
# i think we need qx/px to be vectors because px is built up recursively out of qx's
# dunno about the others
px[1] <- 1
# retirement_rates <- ifelse(current_record$Super == "PSS", retirement_data$PSS, retirement_data$CSS)

while (age <= R_max_age) {
  index <- age - start_age + 1
  at_retirement_age <- age >= R_min_age
  eligible <- at_retirement_age || service >= eligible_years
  year_of_eligibility <- max(age - R_min_age + 1, service - eligible_year + 1)
  if (at_retirement_age) qx[index] <- retirement_rates[age - R_min_age + 1] else qx[index] <- exit_rates[service + 1]
  if (eligible) takeup_rate <- takeup_rates[year_of_eligibility] else takeup_rate <- 0
  taken_this_year <- LSL*takeup_rate
  LSL <- LSL - taken_this_year
  # multiply by px to condition on survival: taken_this_year*px
  # multiply LSL by px*qx for exit at this year: LSL*px*qx
  # then i guess CF is just the sum of these
  age <- age + 1
  service <- service + 1
  px[index + 1] <- px[index]*(1 - qx[index])
}
