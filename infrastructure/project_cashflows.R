## set up some global parameters
R_min_age <- 54 # min(retirement_data$age)
R_max_age <- 75 # max(retirement_data$age)
eligible_years <- 10

exit_rates <- c(0.15, 0.18, 0.12, 0.11, 0.10, 0.09, 0.09, 0.085, 0.05, 0.05, 0.03, rep(0.03, 100))
names(exit_rates) <- 0:110
retirement_rates <- c(0.2, 0.08, 0.07, 0.06, 0.065, 0.07, 0.071, 0.09, 0.15, 0.2, 0.25, rep(0.3, 5), 0.45, 0.5, 0.53, 0.56, 0.75, 1)
names(retirement_rates) <- R_min_age:R_max_age
takeup_rates <- c(0.09, 0.08, 0.06, 0.06) # etc

## LOOK AT A GIVEN OBSERVATION
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

while (age <= R_max_age) {
  index <- age - start_age + 1
  at_retirement_age <- age >= R_min_age
  eligible <- at_retirement_age || service >= eligible_years
  if (at_retirement_age) qx[index] <- retirement_rates[age - R_min_age + 1] else qx[index] <- exit_rates[service + 1]
  if (eligible) takeup_rate <- takeup_rates[year_of_eligibility] else takeup_rate <- 0
  taken_this_year <- LSL*takeup_rate
  LSL <- LSL - taken_this_year
  # multiply by px to condition on survival
  # multiply LSL by px*qx for exit at this year
  age <- age + 1
  service <- service + 1
}

# do some unit tests here: ge 75, age 74... age 54, age 53 etc
