# Load libraries
library(dplyr)
library(tidyr)

# # Read in original data
# voter <- read.csv('VOTER_Survey_December16_Release1.csv', header = TRUE)
# 
# # Extract subset of data that we are interested in
# voter_varsub <- voter[,c("presvote16post_2016", "faminc_2016", "pid3_2016", 
#                          "employ_2016", "child18_2016", "marstat_2016", "educ_2016")]
# 
# 
# # Save to CSV
# write.csv(voter_varsub, file = "voter_subset.csv", row.names = FALSE)

# Read in data
data <- read.csv("voter_subset.csv", header = TRUE)


##### presvote16post_2016 variable #####
# Look at presvote16post_2016 variable
table(data$presvote16post_2016)

# Created voted_clinton variable with 1 if they voted clinton and 0 otherwise
data$voted_clinton <- 0
data$voted_clinton[data$presvote16post_2016 == "Hillary Clinton"] <- 1

# Remove the presvote16post_2016 variable
data <- data[,-c(1)]


##### faminc_2016 variable #####
# Look at faminc_2016 variable
table(data$faminc_2016)

# Create new variable for faminc with low, med, high, and didnotsay
data$faminc <- "faminc_low"
data$faminc[data$faminc_2016 %in% 
                   c("$50,000 - $59,999", 
                     "$60,000 - $69,999", 
                     "$70,000 - $79,999",
                     "$80,000 - $99,999",
                     "$100,000 - $119,999",
                     "$120,000 - $149,999")] <- "faminc_med"

data$faminc[data$faminc_2016 %in% 
                   c("$150,000 - $199,999",
                     "$200,000 - $249,999",
                     "$250,000 - $349,999",
                     "$350,000 - $499,999",
                     "$500,000 or more",
                     "$150,000 or more")] <- "faminc_high"

data$faminc[data$faminc_2016 == "Prefer not to say"] <- "faminc_didnotsay"

# Remove the faminc_2016 variable
data <- data[,-c(1)]

# Convert faminc variable to a set of 4 binary 0-1 variables 
binary <- data %>%
  select(faminc) %>%
  mutate(yesno = 1) %>%
  mutate(id = 1:8000) %>%
  distinct %>%
  spread(faminc, yesno, fill = 0)

data <- cbind(data[,c(1:6)], binary[,c(2:5)])


##### pid3_2016 variable #####
table(data$pid3_2016)

# Create new column pid that is the same as pid3_2016 for group names
# Note: grouping other and not sure together into "other"
data$pid <- NA

data$pid[data$pid3_2016 == "Democrat"] <- "pid_dem"
data$pid[data$pid3_2016 == "Republican"] <- "pid_rep"
data$pid[data$pid3_2016 == "Independent"] <- "pid_ind"
data$pid[data$pid3_2016 == "Other"] <- "pid_other"
data$pid[data$pid3_2016 == "Not sure"] <- "pid_other"

# Convert pid variable to a set of 4 binary 0-1 variables 
binary <- data %>%
  select(pid) %>%
  mutate(yesno = 1) %>%
  mutate(id = 1:8000) %>%
  distinct %>%
  spread(pid, yesno, fill = 0)

data <- cbind(data[,c(2:10)], binary[,c(2:5)])


##### employ_2016 variable #####
table(data$employ_2016)

# Group into full-time, part-time, retired, and other
data$employment <- "employment_other"
data$employment[data$employ_2016 == "Full-time"] <- "employment_fulltime"
data$employment[data$employ_2016 == "Part-time"] <- "employment_parttime"
data$employment[data$employ_2016 == "Retired"] <- "employment_retired"

# Convert employment variable to a set of 4 binary 0-1 variables 
binary <- data %>%
  select(employment) %>%
  mutate(yesno = 1) %>%
  mutate(id = 1:8000) %>%
  distinct %>%
  spread(employment, yesno, fill = 0)

data <- cbind(data[,c(2:13)], binary[,c(2:5)])


##### child18_2016 variable #####
table(data$child18_2016)

# Create binary has_child variable
data$has_child <- 0
data$has_child[data$child18_2016 == "Yes"] <- 1

# Remove child18_2016 variable
data <- data[,-c(1)]


##### marstat_2016 variable #####
table(data$marstat_2016)

# Group into single, coupled, and other
data$marstat <- "marstat_other"
data$marstat[data$marstat_2016 %in% 
                    c("Single")] <- "marstat_single"
data$marstat[data$marstat_2016 %in% 
                    c("Married", "Domestic partnership")] <- "marstat_couple"

# Convert marstat variable to a set of 3 binary 0-1 variables 
binary <- data %>%
  select(marstat) %>%
  mutate(yesno = 1) %>%
  mutate(id = 1:8000) %>%
  distinct %>%
  spread(marstat, yesno, fill = 0)

data <- cbind(data[,c(2:16)], binary[,c(2:4)])


##### educ_2016 variable #####
table(data$educ_2016)

# Group into college grad, some college, and no college
data$education <- "educ_collegegrad"
data$education[data$educ_2016 %in% 
                      c("2-year", "Some college")] <- "educ_somecollege"
data$education[data$educ_2016 %in% 
                      c("High school graduate", "No HS")] <- "educ_nocollege"


# Convert education variable to a set of 3 binary 0-1 variables 
binary <- data %>%
  select(education) %>%
  mutate(yesno = 1) %>%
  mutate(id = 1:8000) %>%
  distinct %>%
  spread(education, yesno, fill = 0)

data <- cbind(data[,c(2:18)], binary[,c(2:4)])


# Save processed data to csv
write.csv(data, file = "processed_voter_data.csv", row.names = FALSE)

