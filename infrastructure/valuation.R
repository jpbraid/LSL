library(tidyverse)
library(lubridate)

master_file <- "Infrastructure_proj.csv"
retirement_file <- "Retirementrate_PSSCSS_2017.csv"
valuation_date = dmy("01/06/2019")
eligible_year <- 10 # number of years of service required before eligible for LSL
start_rate <- 0.09
reduce_rate <- 0.01
end_rate <- 0.06
LSL_DATA = "S:/Projection/"
LSL_VAL = "S:/Projection/LSL Valuation/"

# read in the data
agency_data <- read_csv(paste0(LSL_DATA, master_file))
retirement_data <- read_csv(paste0(LSL_VAL, retirement_file))

# extract some variables (used in downstream analysis)
R_minage <- min(retirement_data$Age)
R_maxage <- max(retirement_data$Age)
min_duration <- min(agency_data$Duration, na.rm = TRUE)
max_duration <- max(agency_data$Duration, na.rm = TRUE)

agency_data <- agency_data %>% 
  select(-c(Duration, Exit_Rate)) %>% 
  filter(LSL != "-", !is.na(Salary), Super %in% c("CSS", "PSS")) %>%
  mutate(Birthdate = dmy(Birthdate), APS_Start = dmy(APS_Start))

N_datarec <- nrow(agency_data)

# do some wrangling of the original data (don't know if it's really necessary to give this a new name)
#agency_val <- agency_data %>% mutate(blah)

