library(tidyverse)
library(lubridate)

##############################################
master_file <- "NDIA_proj.csv"
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
agency_val <- agency_data %>% 
  select(-c(Duration, Exit_Rate)) %>%
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

# define exit rates and takeup rates [retirement rates depend on super scheme]
exit_rates <- agency_data %>% filter(complete.cases(agency_data)) %>% select(Exit_Rate) %>% unlist() 
takeup_rates <- ifelse(start_rate - (0:100)*reduce_rate >= end_rate, start_rate - (0:100)*reduce_rate, end_rate)

# define function for projecting cashflows
# TODO: remove dependence on global variables (R_max_age, R_min_age, eligible_year, exit_rates, takeup_rates)
project_cashflows <- function(record) {
  # set up params for analysis
  start_age <- record$age
  start_service <- record$service
  analysis_length <- R_max_age - start_age + 1 #***
  PSS <- str_sub(record$Super, 1L, 1L) == "P"
  retirement_rates <- PSS*retirement_data$Retirement_PSS + (1 - PSS)*retirement_data$Retirement_CSS #***
  
  # initialise stuff
  LSL <- record$LSL
  qx <- px <- vector(length = analysis_length)
  px[1] <- 1
  age <- start_age
  service <- start_service
  cash_flow_projections <- data.frame(observation = record$AGS, age = NA, service = NA, analysis_year = NA, kpx = NA, qx = NA, taken_in_service = NA,
                                      taken_in_service_weighted = NA, taken_after_exit = NA, taken_after_exit_weighted = NA, 
                                      cash_flow = NA)
  
  # now project cash flows for each year
  while (age <= R_max_age) {
    index <- age - start_age + 1
    at_retirement_age <- age >= R_min_age
    eligible_in_service <- service >= eligible_year
    eligible_on_exit <- at_retirement_age || eligible_in_service
    year_of_eligibility <- max(0, service - eligible_year + 1)
    if (at_retirement_age) qx[index] <- retirement_rates[age - R_min_age + 1] else qx[index] <- exit_rates[service + 1]
    if (eligible_in_service) takeup_rate <- takeup_rates[year_of_eligibility] else takeup_rate <- 0
    taken_this_year <- LSL*takeup_rate
    taken_this_year_weighted <- taken_this_year*px[index]
    LSL <- LSL - taken_this_year
    if (eligible_on_exit) {
      after_exit <- LSL
      after_exit_weighted <- after_exit*px[index]*qx[index]
    } else {
      after_exit <- after_exit_weighted <- 0
    }
    cash_flow <- taken_this_year_weighted + after_exit_weighted
    cash_flow_projections <- rbind(cash_flow_projections, 
                                   data.frame(observation = record$AGS, age = age, service = service, 
                                              analysis_year = index, kpx = px[index], 
                                              qx = qx[index], taken_in_service = taken_this_year, 
                                              taken_in_service_weighted = taken_this_year_weighted, 
                                              taken_after_exit = after_exit,
                                              taken_after_exit_weighted = after_exit_weighted, 
                                              cash_flow = cash_flow))
    age <- age + 1
    service <- service + 1
    px[index + 1] <- px[index]*(1 - qx[index])
  }
  
  cash_flow_projections %>% filter(complete.cases(cash_flow_projections))
}

# do some unit tests
# age 75, 74, 54, 55... different amounts of service
# then compare w/ limin's program
