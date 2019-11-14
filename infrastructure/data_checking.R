# read in raw LSL data
infrastructure_current <- read_csv("//FS2/Indiv$/ERB/Documents/raw_data_infrastructure.csv")

# set up params
valuation_date <- dmy("31/01/2019")

# convert dates, create new columns, etc
infrastructure_current <- infrastructure_current %>% mutate(ongoing = ifelse(str_detect(`Departmental/Administered`, "^Ongoing"), 
                                                             "Y", "N"),
                                                            DOB = dmy(`Date of Birth`), APS_hire_date = dmy(`APS Hire Date`),
                                                            age_at_start = (APS_hire_date - DOB)/365.25[[1]],
                                                            exact_service = (valuation_date - APS_hire_date)/365.25[[1]],
                                                            integer_service = as.integer(exact_service),
                                                            capped_10_service = ifelse(integer_service < 10, integer_service, 10),
                                                            exact_age = (valuation_date - DOB)/365.25[[1]],
                                                            age_for_graph = round(exact_age/2, 0)*2) %>% 
select(-c(`Date of Birth`, `APS Hire Date`))

# calculate the number of active employees with n years of service for n = 0, 1, ..., 9, 10+
# note that we only count ongoing employees under age 55
number_active <- vector(length = 11)
for (i in 0:10) {
  number_active[i + 1] <- infrastructure %>% 
    filter(exact_age < 55, ongoing == "Y", integer_service == i) %>% 
    nrow()
}
names(number_active) <- c(0:9, "10+")

# read in the previous version of the LSL data
infrastructure_previous <- read_csv("blah...")

# do some kind of high-level reconciliation
# total LSL liability this time = sum(liability); last time = .
# last time + accrual + - + - .... = this time?

# now we count exits: first, find those entries in previous that aren't in current
nominal_exits <- left_join(infrastructure_current, infrastructure_previous, by = "AGS")


