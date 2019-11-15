library(tidyverse)
library(lubridate)

## the analysis below assumes the master_file is a csv with the following column headings (case-sensitive):
## AGS <dbl>, Gender <chr>, Birthdate <chr>, APS_Start <chr>, Salary <dbl>, LSL <dbl>, 
## Super <chr>, Duration <int>, Exit_Rate <dbl>
## note the underscores also (if the columns aren't named exactly as above, none of the below will work)

##############################################
########### setup initial params #############
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
##############################################
##############################################

# read in the data
agency_data <- read_csv(paste0(LSL_DATA, master_file)) ## corresponds to &filename._temp
retirement_data <- read_csv(paste0(LSL_VAL, retirement_file)) ## corresponds to LSLVAL.&Retfile data set and RETIREMENTRATE table

# extract some variables (used in downstream analysis)
R_minage <- min(retirement_data$Age)
R_maxage <- max(retirement_data$Age)
min_duration <- min(agency_data$Duration, na.rm = TRUE)
max_duration <- max(agency_data$Duration, na.rm = TRUE)
N_datarec <- nrow(agency_data)

# separate out the exit rate data from the rest of the input data
exit_rates <- agency_data %>% 
  filter(complete.cases(agency_data)) %>% 
  select(c(Duration, Exit_Rate)) ## corresponds to exitrate table
agency_data <- agency_data %>% select(-c(Duration, Exit_Rate))

# do some wrangling of the original data
## this dataframe corresponds to the &filename.val data set
agency_val <- agency_data %>% 
  filter(!is.na(Birthdate), !is.na(APS_Start), str_sub(Super, 1L, 1L) %in% c("P", "C")) %>%
  mutate(Birthdate = dmy(Birthdate), 
         APS_Start = dmy(APS_Start),
         Jage = as.numeric((valuation_date - Birthdate)/365.25),
         Jservice = as.numeric((valuation_date - APS_Start)/365.25),
         age = as.integer(Jage),
         service = as.integer(Jservice)) %>%
  filter(age >= 16, service <= 55)

# create dodgydata and stat1 tables (not really necessary)

# define a couple more global variables
C_minage <- min(agency_val$age)
laps <- R_maxage - C_minage

# create a takeuprate dataframe for some reason
durations <- 0:min(20, laps)
rates <- start_rate - (durations - eligible_year)*reduce_rate
takeup_rate <- (durations >= eligible_year)*ifelse(rates > end_rate, rates, end_rate)
takeup_rate_table <- data.frame(duration = durations, takeup_rate = takeup_rate)


# now we do the actual work
# first setup variables to be used in calculating the projection
valuation_year <- year(valuation_date) + (month(valuation_date) > 6)
qM_CSSrx <- qF_CSSrx <- retirement_data$Retirement_CSS
qM_PSSrx <- qF_PSSrx <- retirement_data$Retirement_PSS
last_exit_rate <- exit_rates$Exit_Rate[max_duration + 1]
qM_CSSwt <- qF_CSSwt <- qM_PSSwt <- qF_CSSwt <- c(exit_rates$Exit_Rate, 
                                                  rep(last_exit_rate, laps - (max_duration - min_duration)))
CSS <- ifelse(str_sub(agency_val$Super, 1L, 1L) == "C", 1, 0)
px <- rep(NA, laps + 1)
qx <- rep(NA, laps + 1)
takeuprate <- inservice <- afterret <- inserviceVal <- afterretVal <- cashflow <- rep(NA, laps + 1)
q_rx <- vector(mode = "list", length = nrow(agency_val))
# q_rx[[1]] <- qM_CSSrx*CSS[[1]] + qM_PSSrx*(1 - CSS[[1]]) etc
q_wt <- vector(mode = "list", length = nrow(agency_val))
# q_wt[[1]] <- qM_CSSwt*CSS + qM_PSSwt*(1 - CSS)


### LOOK AT THE FIRST OBSERVATION ###
first_obs <- agency_val[1, ]
px[1] <- 1
suminservice <- 0
jage <- first_obs$age
jservice <- first_obs$service

for (k in 1:22) {
  qx[k] <- qM_CSSrx[jage - R_minage + 1] # obs is aged 54 so get the first possible retirement rate (prob exit in kth year)
  takeuprate[k] <- (jage < R_maxage)*end_rate # they've been in service 25 yrs so takeuprate is just end_rate
  
  if (jage < R_maxage) {
    inservice[k] = (first_obs$LSL - suminservice)*takeuprate[k] # how much are they taking in this year?
    suminservice = suminservice + inservice[k]
    inserviceVal[k] = inservice[k]*px[k]
    afterret[k] = (first_obs$LSL - suminservice)
    afterretVal[k] = afterret[k]*px[k]*qx[k]
  }
  
  px[k + 1] <- px[k]*(1 - qx[k]) # prob of staying in service until year after analysis year
  print(jage)
  jservice <- jservice + 1
  jage <- jage + 1
  
}
