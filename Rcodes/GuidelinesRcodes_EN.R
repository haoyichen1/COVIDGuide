rm(list = ls())

# Loading libraries -------------------------------------------------------

library(pacman)
p_load(
  tidyverse,
  readr,
  vcd,
  DescTools,
  magrittr,
  PracTools,
  sampling,
  TeachingSampling,
  psych
)

# Reading data ------------------------------------------------------------

# HFPS = High Frequency Phone Survey - Malawi
# IHPS = Integrated Household Panel Survey - Malawi

# read household roster data from file hh_mod_b_19.csv and assign it to variable HFPS_hh_roster
HFPS_hh_roster <- read.csv("Data/hh_mod_b_19.csv")

# read household education data from file hh_mod_c_19.csv and assign it to variable HFPS_hh_edu
HFPS_hh_edu <- read.csv("Data/hh_mod_c_19.csv")

# read household economic data from file hh_mod_e_19.csv and assign it to variable HFPS_hh_eco
HFPS_hh_eco <- read.csv("Data/hh_mod_e_19.csv")

# read household filter data from file hh_mod_a_filt_19.csv and assign it to variable IHPS_hh_filter
IHPS_hh_filter <- read.csv("Data/hh_mod_a_filt_19.csv")


# Data cleaning -----------------------------------------------------------

# Joining and selecting relevant variables
HFPS <- HFPS_hh_roster %>%
  inner_join(HFPS_hh_edu) %>%
  inner_join(HFPS_hh_eco) %>%
  inner_join(IHPS_hh_filter) %>%
  select(
    y4_hhid,
    # household identifier
    id_code,
    # individual identifier
    hh_b04,
    # whether individual is head of household
    hh_b06_4,
    # whether household has working phone
    hh_c05_1,
    # individual literacy status
    hh_e06_8a,
    # individual employment status
    hh_wgt,
    # household weight
    panelweight_2019,
    # individual weight
    region,
    # region of the household
    reside,
    # residence type (urban or rural)
    hhsize           # household size
  ) %>%
  drop_na() %>%      # Removing rows with missing values
  filter(hh_e06_8a != 4)      # Excluding Unpaid Apprenticeship

# Household head data -----------------------------------------------------

# Identifying if the household has a working phone
hh_phone <- HFPS %>%
  group_by(y4_hhid) %>%
  summarise(hh_phone = min(hh_b06_4, na.rm = TRUE),
            # Minimum value of hh_b06_4 by group
            tt_wgt = sum(hh_wgt))                  # Total household weight

# Identifying household heads
head_hh <- HFPS %>%
  filter(hh_b04 == 1)        # Filtering for household heads

# Creating additional variables
hh_temp <- head_hh %>%
  inner_join(hh_phone) %>%     # Joining with hh_phone dataset
  drop_na() %>%                # Removing rows with missing values
  mutate(
    Phone =                      # Creating Phone variable based on hh_b06_4
      case_when(hh_b06_4 == 1 ~ "Yes",
                TRUE ~ "No"),
    Literacy =                   # Creating Literacy variable based on hh_c05_1
      case_when(hh_c05_1 == 1 ~ "Literate",
                TRUE ~ "Illiterate")
  )


# Chi-sqrd and Cramer's-V tests -------------------------------------------

# Phone ownership is correlated with literacy
# From the phone subset a sample is selected in a further 2nd phase
# The phone data conducted by phone

# Create a contingency table of phone ownership and literacy status
crosstable1 <- table(hh_temp$Phone, hh_temp$Literacy)
crosstable1

# Perform chi-squared test to check for association between phone ownership and literacy
chisq.test(crosstable1)

# Calculate Cramer's V statistic to measure the strength of association
CramerV(crosstable1,
        conf.level = 0.95)

# Calculate other association statistics based on the contingency table
assocstats(crosstable1)

# Create a contingency table of phone ownership and unpaid apprenticeship status
crosstable2 <- table(hh_temp$Phone, hh_temp$hh_e06_8a)

# Calculate proportion of phone owners who have unpaid apprenticeship status
prop.table(crosstable2, margin = 2)[2,]


# Propensity score model --------------------------------------------------

# logistic regression model
ps_fit1 <- glm(
  formula = factor(Phone) ~ 1 + factor(hh_c05_1) + factor(hh_e06_8a),
  family = binomial(link = "logit"),
  data = hh_temp
)

# print summary of the model
summary(ps_fit1)

# add the predicted PS as a new column to the dataset
hh_temp$ps <- predict(ps_fit1, type = "response")

# calculate the sum of PS
sum(hh_temp$ps)

# create a table of Phone ownership
table(hh_temp$Phone)

# create a histogram of PS
hist(hh_temp$ps)

# print summary statistics of PS
summary(hh_temp$ps)

# set up a 3x1 plot
par(mfrow = c(3, 1))

# create a histogram of PS for all units
hist(hh_temp$ps,
     main = "All units",
     xlab = "Estimated PS",
     xlim = c(0, 1))

# create a histogram of PS for respondents
hist(hh_temp$ps[hh_temp$Phone == "Yes"],
     main = "Respondents",
     xlab = "Estimated PS",
     xlim = c(0, 1))

# create a histogram of PS for non-respondents
hist(hh_temp$ps[hh_temp$Phone == "No"],
     main = "Nonrespondents",
     xlab = "Estimated PS",
     xlim = c(0, 1))

# close the plot
dev.off()

# set up a 2x2 plot
par(mfrow = c(2, 2))

# create a boxplot of PS by literacy for respondents
with(
  hh_temp[hh_temp$Phone == "Yes", ],
  boxplot(
    ps ~ factor(hh_c05_1),
    main = "Respondents",
    ylab = "Estimated PS",
    xlab = "Literacy"
  )
)

# create a boxplot of PS by literacy for non-respondents
with(
  hh_temp[hh_temp$Phone == "No", ],
  boxplot(
    ps ~ factor(hh_c05_1),
    main = "Nonrespondents",
    ylab = "Estimated PS",
    xlab = "Literacy"
  )
)

# create a boxplot of PS by employment status for respondents
with(
  hh_temp[hh_temp$Phone == "Yes", ],
  boxplot(
    ps ~ factor(hh_e06_8a),
    main = "Respondents",
    ylab = "Estimated PS",
    xlab = "Employment Status"
  )
)

# create a boxplot of PS by employment status for non-respondents
with(
  hh_temp[hh_temp$Phone == "No", ],
  boxplot(
    ps ~ factor(hh_e06_8a),
    main = "Nonrespondents",
    ylab = "Estimated PS",
    xlab = "Employment Status"
  )
)

# close the plot
dev.off()

# Representativity Indicators ---------------------------------------------

## At the national - level

# Summarize the hh_temp dataset at the national level
summary_ps_national <- hh_temp %>%
  summarise(
    n = n(),
    # count the number of observations
    urp = mean(ps),
    # calculate the unweighted mean of ps
    rho.bar = weighted.mean(ps, hh_wgt),
    # calculate the weighted mean of ps using hh_wgt as weights
    urr = sum(Phone == "Yes") / n(),
    # calculate the unweighted proportion of Phone == "Yes"
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    # calculate the weighted proportion of Phone == "Yes" using hh_wgt as weights
    sdp = sd(ps),
    # calculate the standard deviation of ps
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt)) # calculate the R-squared hat value
  )

# Print the summary_ps_national
summary_ps_national

# Calculate the standard deviation of ps in hh_temp dataset
sd(hh_temp$ps)

# Calculate the estimated number of households in the population
N.hat <- sum(hh_temp$hh_wgt)

# Calculate the weighted mean of ps using hh_wgt as weights
rho.bar <- sum(hh_temp$ps * hh_temp$hh_wgt) / N.hat

# Calculate the R-squared hat value
R.hat <- 1 - 2 *
  sqrt((1 / (N.hat - 1)) * sum(hh_temp$hh_wgt * (hh_temp$ps - rho.bar) ^ 2))

# Print the summary statistics of ps in hh_temp dataset
summary(hh_temp$ps)

## At the dissaggregated - level

# Literacy
# Create a boxplot of ps by Literacy level
with(hh_temp, boxplot(ps ~ Literacy))

# Create three histograms of ps, one for each level of Literacy
par(mfrow = c(1, 3))
hist(hh_temp$ps)
hist(hh_temp$ps[hh_temp$Literacy == "Literate"])
hist(hh_temp$ps[hh_temp$Literacy == "Illiterate"])
dev.off()

# Summarize the hh_temp dataset by Literacy level
summary_ps_Literacy <- hh_temp %>%
  group_by(Literacy) %>%
  summarise(
    n = n(),
    # count the number of observations
    urp = mean(ps),
    # calculate the unweighted mean of ps
    rho.bar = weighted.mean(ps, hh_wgt),
    # calculate the weighted mean of ps using hh_wgt as weights
    urr = sum(Phone == "Yes") / n(),
    # calculate the unweighted proportion of Phone == "Yes"
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    # calculate the weighted proportion of Phone == "Yes" using hh_wgt as weights
    sdp = sd(ps),
    # calculate the standard deviation of ps
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt)) # calculate the R-squared hat value
  ) %>% as.data.frame()

# Print the summary_ps_Literacy
summary_ps_Literacy

# Activity

# Create a boxplot of 'ps' variable grouped by 'hh_e06_8a'
with(hh_temp, boxplot(ps ~ hh_e06_8a))

# Create a histogram of 'ps' variable
hist(hh_temp$ps)

# Create a 2x2 grid of histograms of 'ps' variable grouped by 'hh_e06_8a' values
par(mfrow = c(2, 2))
hist(hh_temp$ps[hh_temp$hh_e06_8a == 1])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 2])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 3])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 5])

# Turn off the grid of histograms
dev.off()

# Create a summary table of 'ps' variable statistics grouped by 'hh_e06_8a' values
summary_ps_Activity <- hh_temp %>%
  group_by(hh_e06_8a) %>%
  summarise(
    n = n(),
    urp = mean(ps),
    rho.bar = weighted.mean(ps, hh_wgt),
    urr = sum(Phone == "Yes") / n(),
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    sdp = sd(ps),
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt))
  ) %>% as.data.frame()

# Print the summary table
summary_ps_Activity

# Activity and Literacy

# This code groups hh_temp by Literacy and hh_e06_8a, and calculates summary statistics for each group.
# The resulting data frame is saved as summary_ps_LiteracyActivity.

summary_ps_LiteracyActivity <- hh_temp %>%
  group_by(Literacy, hh_e06_8a) %>%
  summarise(
    n = n(),
    urp = mean(ps),
    rho.bar = weighted.mean(ps, hh_wgt),
    urr = sum(Phone == "Yes") / n(),
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    sdp = sd(ps),
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt))
  ) %>% as.data.frame()

# Print the resulting data frame
summary_ps_LiteracyActivity


# Class variable weighting ------------------------------------------------

# Group hh_temp data by three variables and calculate count and total weight for each group
hh_temp_agg <- hh_temp %>%
  group_by (hh_c05_1, hh_e06_8a, hh_b06_4) %>%
  summarise(n = n(),
            thh_wgt = sum(panelweight_2019))

# Filter hh_temp_agg data into two data frames based on the value of hh_b06_4 variable
hh_temp_agg_resp <- hh_temp_agg %>% filter(hh_b06_4 == 1)
hh_temp_agg_nonr <- hh_temp_agg  %>% filter(hh_b06_4 == 2)

# Combine hh_temp_agg_nonr data with hh_temp_agg_resp data, with hh_c05_1, hh_e06_8a, and hh_b06_4 variables removed from hh_temp_agg_resp data
Table11 <- data.frame(
  hh_temp_agg_nonr,
  hh_temp_agg_resp %>% ungroup %>%
    select(-hh_c05_1, -hh_e06_8a, -hh_b06_4)
)

# Calculate total weight for hh_temp_agg_resp and hh_temp_agg_nonr data
sum(hh_temp_agg_resp$thh_wgt)
sum(hh_temp_agg_nonr$thh_wgt)
sum(hh_temp$panelweight_2019)

# Add a new column 'ac' to hh_temp_agg_resp data, which calculates the ratio of total weight of both hh_temp_agg_resp and hh_temp_agg_nonr to the total weight of hh_temp_agg_resp
hh_temp_agg_resp$ac <-
  (hh_temp_agg_resp$thh_wgt + hh_temp_agg_nonr$thh_wgt) / hh_temp_agg_resp$thh_wgt

# Join hh_temp data with hh_temp_agg_resp data based on three variables and calculate weighted average of panelweight_2019 variable for each household in hh_temp_resp data
hh_temp_resp <-  hh_temp %>%
  inner_join(hh_temp_agg_resp %>%
               select(-n, -thh_wgt))

hh_temp_resp$wac <-
  with(hh_temp_resp, panelweight_2019 * ac * hhsize)
hh_temp_resp$w0 <-
  hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize

# Calculate total weight for w0 and wac variables in hh_temp_resp data
sum(hh_temp_resp$w0)
sum(hh_temp_resp$wac)

# Plot wac against w0 variables in hh_temp_resp data and add a line with slope 1 and intercept 0
plot(
  hh_temp_resp$w0,
  hh_temp_resp$wac,
  xlab = "w0",
  ylab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)
abline(a = 0, b = 1, col = 2)

# Create histograms of panelweight_2019 and wac variables in hh_temp_resp data
hist(hh_temp_resp$panelweight_2019)
hist(hh_temp_resp$wac)

# Classes for PS ----------------------------------------------------------

# Define a propensity score model using the pclass function
psclass = pclass(
  factor(Phone) ~ 1 + factor(hh_c05_1) +  # Independent variables
    factor(hh_e06_8a),
  # Independent variable
  type = "unwtd",
  # Type of analysis to perform (unweighted)
  data = hh_temp  # Dataset to use
)

# Create a table of the propensity score classes
table(psclass$p.class, useNA = "always")

# Summarize the propensities
summary(psclass$propensities)

# Predict the propensity score for each observation in the dataset
hh_temp$ps <- predict(ps_fit1, type = "response")

# Classify each observation into a propensity score class
hh_temp$class <- psclass$p.class

# Create a summary table of the dataset with five propensity score classes
summary_ps_5classes <- hh_temp %>%
  group_by(class) %>%
  summarise(
    n = n(),
    urp = mean(ps),
    # Unweighted mean propensity score
    wrp = weighted.mean(ps, hh_wgt),
    # Weighted mean propensity score
    urr = sum(Phone == "Yes") / n(),
    # Unweighted response rate
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    # Weighted response rate
    mps = median(ps) # Median propensity score
  )

# Remove the n variable from the summary table
ps_classified <- summary_ps_5classes %>% select(-n)

# Join the classified propensity score with the original dataset
hh_temp_resp <- hh_temp %>%
  inner_join(ps_classified) %>%
  inner_join(hh_temp_resp)

# Create variables for weighted propensity score calculations
hh_temp_resp$w1ps <-
  with(hh_temp_resp, hhsize * panelweight_2019 / urp)
hh_temp_resp$w2ps <-
  with(hh_temp_resp, hhsize * panelweight_2019 / wrp)
hh_temp_resp$w3ps <-
  with(hh_temp_resp, hhsize * panelweight_2019 / urr)
hh_temp_resp$w4ps <-
  with(hh_temp_resp, hhsize * panelweight_2019 / wrr)

# Calculate weighted sums of the different variables
sum(hh_temp_resp$w0)
sum(hh_temp_resp$w1ps)
sum(hh_temp_resp$w2ps)
sum(hh_temp_resp$w3ps)
sum(hh_temp_resp$w4ps)

# Set the plot layout to 2 x 2
par(mfrow = c(2, 2))

# Plot w1ps against wac and set the axis labels and limits
plot(
  hh_temp_resp$wac,
  hh_temp_resp$w1ps,
  ylab = "w1ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)

# Add a diagonal line with intercept 0 and slope 1 in red
abline(a = 0, b = 1, col = 2)

# Plot w2ps against wac and set the axis labels and limits
plot(
  hh_temp_resp$wac,
  hh_temp_resp$w2ps,
  ylab = "w2ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)

# Add a diagonal line with intercept 0 and slope 1 in red
abline(a = 0, b = 1, col = 2)

# Plot w3ps against wac and set the axis labels and limits
plot(
  hh_temp_resp$wac,
  hh_temp_resp$w3ps,
  ylab = "w3ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)

# Add a diagonal line with intercept 0 and slope 1 in red
abline(a = 0, b = 1, col = 2)

# Plot w4ps against wac and set the axis labels and limits
plot(
  hh_temp_resp$wac,
  hh_temp_resp$w4ps,
  ylab = "w4ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)

# Add a diagonal line with intercept 0 and slope 1 in red
abline(a = 0, b = 1, col = 2)

# Turn off the graphics device
dev.off()


# Calibration -------------------------------------------------------------

# Reside and Region
# Counts the number of observations in HFPS$reside
table(HFPS$reside)

# Counts the number of observations in hh_temp$reside
table(hh_temp$reside)

# Counts the number of observations in HFPS$region
table(HFPS$region)

# Counts the number of observations in hh_temp$region
table(hh_temp$region)

# Creates a contingency table with HFPS$reside and HFPS$region
table(paste(HFPS$reside, HFPS$region))

# Calculates the sum of HFPS$hh_wgt
sum(HFPS$hh_wgt)

# Groups HFPS by region and calculates the number of households and total weight
HFPS %>%
  group_by(region) %>%
  summarise(n = n(),
            N = sum(hh_wgt))

# Groups HFPS by reside and calculates the number of households and total weight
HFPS %>%
  group_by(reside) %>%
  summarise(n = n(),
            N = sum(hh_wgt))

# Groups HFPS by reside and region and calculates the number of households and total weight
totals <- HFPS %>%
  group_by(reside, region) %>%
  summarise(n = n(),
            N = sum(hh_wgt))

# Create a matrix of domains using the reside and region columns from hh_temp_resp
x0s <-
  as.matrix(Domains(paste(
    hh_temp_resp$reside, hh_temp_resp$region
  )))

# Create a matrix of total population values
tx0 <- as.matrix(totals$N)

# Sum the values in tx0
sum(tx0)

# Perform calibration using the linear method on the x0s and tx0 matrices
g0k <-
  calib(
    x0s,
    d = hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize,
    total = tx0,
    method = "linear"
  )

# Calculate calibrated weights and store them in hh_temp_resp$wcal
hh_temp_resp$wcal <-
  hh_temp_resp$panelweight_2019 * g0k * hh_temp_resp$hhsize

# Sum the calibrated weights in hh_temp_resp$wcal
sum(hh_temp_resp$wcal)

# Print tx0
tx0

# Print the column sums of x0s multiplied by panelweight_2019 and hhsize
colSums(x0s * hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize)

# Calculate the ratio of tx0 to the column sums of x0s multiplied by panelweight_2019 and hhsize
tx0 / colSums(x0s * hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize)

# Create a table of the calibrated weights
table(g0k)

# Sum the weights in hh_temp_resp$wac
sum(hh_temp_resp$wac)

# Sum the calibrated weights in hh_temp_resp$wcal
sum(hh_temp_resp$wcal)

# Create a scatter plot of wac vs. wcal
plot(hh_temp_resp$wac,
     hh_temp_resp$wcal,
     xlab = "wac",
     ylab = "wcal")

# Add a diagonal line to the plot
abline(a = 0, b = 1, col = 2)


# PS + Calibration --------------------------------------------------------

# Plotting w1ps against wcal
plot(hh_temp_resp$w1ps, hh_temp_resp$wcal)

# Adding a diagonal line to the plot
abline(a = 0, b = 1, col = 2)

# Creating a histogram of w1ps
hist(hh_temp_resp$w1ps)

# Creating a histogram of wcal
hist(hh_temp_resp$wcal)

# Applying a calibration function to w1ps
g1k <- calib(x0s,
             d = hh_temp_resp$w1ps,
             total = tx0,
             method = "linear")

# Scaling w1ps by the calibration factor
hh_temp_resp$wpscal <- hh_temp_resp$w1ps * g1k

# Summing the values of w0
sum(hh_temp_resp$w0)

# Summing the values of w1ps
sum(hh_temp_resp$w1ps)

# Summing the values of wcal
sum(hh_temp_resp$wcal)

# Summing the values of wpscal
sum(hh_temp_resp$wpscal)

# Plotting w1ps against wpscal
plot(hh_temp_resp$w1ps,
     hh_temp_resp$wpscal,
     ylab = "wpscal",
     xlab = "wps")

# Adding a diagonal line to the plot
abline(a = 0, b = 1, col = 2)

# Creating a histogram of w1ps
hist(hh_temp_resp$w1ps)

# Creating a histogram of wpscal
hist(hh_temp_resp$wpscal)

# Displaying a summary of the values of w1ps
summary(hh_temp_resp$w1ps)



# Final plots -------------------------------------------------------------

# Here is the code with comments added:

# Select the columns of interest from hh_temp_resp dataframe
dataweights <- hh_temp_resp %>%
  select(w0, wac, w1ps, wcal, wpscal)

# Calculate the correlation matrix of the selected columns
cor(dataweights)

# Create a scatterplot matrix for the selected columns
pairs.panels(dataweights)
