##############################################################################
#
# Cherwell Analysis Project
# Bill James (BCDL) / jamesw@csps.com
#
# Files:  https://github.com/wjamesTMC/ds-education-capstone_project.git
#
##############################################################################

#
# Library setups
#

# Import libraries
library(tidyverse)
library(tidyr)
library(dslabs)
library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(RColorBrewer)
library(extrafont)
library(scales)
library(lubridate)
library(reshape2)

#
# Open files and downloads
#

# Import and Open the Files - first the data file
data_filename   <- "Cherwell Working Copy Data File.csv"

# Set the duration limit for the entire year
dur_time_limit  <- 367 # Change this value for shorter duration times

# Establish the data set based on the time limit
full_dat        <- read.csv(data_filename, stringsAsFactors = FALSE)
dat             <- full_dat %>% filter(full_dat$Dur_Time < dur_time_limit)
num_rows        <- nrow(dat)   

# Files of the taxonomy, validations, staff names, and staff rankings
validations     <- "Validations File.csv"
Level_codes     <- read.csv(validations, stringsAsFactors = FALSE)
staff_rankings  <- read.csv("Staff_Ranking.csv")
staff_names     <- read.csv("SD_Staff_List.csv", stringsAsFactors = FALSE)

num_staff       <- nrow(staff_names)

# Create separate datasets for incidents and service requests
dat_inc         <- dat %>% filter(dat$IRT == 1)
num_rows_inc    <- nrow(dat_inc)
dat_svr         <- dat %>% filter(dat$IRT == 2)
num_rows_svr    <- nrow(dat_svr)

# Establish basic variables from the taxonomy
num_L1             <- max(Level_codes$L1C)
num_L2             <- max(Level_codes$L2C)
num_L3             <- max(Level_codes$L3C)
total_combinations <- num_L1 * num_L2 * num_L3


#*****************************************************************************
#
# Descriptive Statistics
#
#*****************************************************************************

#
# Plot of entire year events and durations
#

vdt <- dat$Dur_Time
df <- as.data.frame(table(vdt))
ggplot(df, aes(vdt, Freq)) +
     geom_point() + 
     theme_economist() +
     labs(title = "All Events - Duration Time and Frequency", 
          x = "Duration", 
          y = "Number of Event Occurrences")

#
# Resetting to create the 30-day view and start the analysis
#

# Re-import and Open the Files - first the data file
data_filename   <- "Cherwell Working Copy Data File.csv"

# Reset the duration limit
dur_time_limit  <- 30 # Change this value for shorter duration times

# Establish the data set based on the new time limit
full_dat        <- read.csv(data_filename, stringsAsFactors = FALSE)
dat             <- full_dat %>% filter(full_dat$Dur_Time < dur_time_limit + 1)
num_rows        <- nrow(dat)   

# Reset the incident and service request subsets
dat_inc         <- dat %>% filter(dat$IRT == 1)
num_rows_inc    <- nrow(dat_inc)

dat_svr         <- dat %>% filter(dat$IRT == 2)
num_rows_svr    <- nrow(dat_svr)

#
# Plot to show distribution of events and durations (30-day limit)
#

vdt <- dat$Dur_Time
df <- as.data.frame(table(vdt))
ggplot(df, aes(vdt, Freq)) +
     geom_point() + 
     theme_economist() +
     labs(title = "Events Duration Time and Frequency - 30 Day Cutoff", 
          x = "Duration", 
          y = "Number of Event Occurrences")


# Set up a dataframe to plot duration vs. number of events for a duration time
mat <- data.frame("L1"       = 1:total_combinations, 
                  "L2"       = 1:total_combinations,
                  "L3"       = 1:total_combinations,
                  "Events"   = 1:total_combinations,
                  "MaxDur"   = 1:total_combinations,
                  "MinDur"   = 1:total_combinations,
                  "AvgDur"   = 1:total_combinations,
                  "MMSpread" = 1:total_combinations,
                  "L3Name"   = 1:total_combinations)

# Loop through the data file looking for combinations that actually exist
counter <- 1
for(i in 1:num_L1) {
     for(j in 1:num_L2) {
          for(k in 1:num_L3) {
               x <- dat %>% filter(L1C == i & L2C == j & L3C == k)
               if(nrow(x) != 0) {
                    mat[counter, 1] <- i
                    mat[counter, 2] <- j
                    mat[counter, 3] <- k
                    mat[counter, 4] <- nrow(x)
                    mat[counter, 5] <- max(x$Dur_Time)
                    mat[counter, 6] <- min(x$Dur_Time)
                    mat[counter, 7] <- round(mean(x$Dur_Time), digits=1)
                    mat[counter, 8] <- max(x$Dur_Time) - min(x$Dur_Time)
                    mat[counter, 9] <- Level_codes[k,5]
                    counter <- counter + 1
               }
               else {
                    mat[counter, 1] <- i
                    mat[counter, 2] <- j
                    mat[counter, 3] <- k
                    mat[counter, 4] <- 0
                    mat[counter, 5] <- 0
                    mat[counter, 6] <- 0
                    mat[counter, 7] <- 0
                    mat[counter, 8] <- 0
                    mat[counter, 9] <- Level_codes[k,5]
                    counter <- counter + 1
                    return
               }
          }
     }
}

# Take the newly populated dataframe and eliminate unneeded rows 
newmat <- mat %>% filter(mat$Events != 0)
events <- sum(newmat$Events) 

# Plot of number of events by duration time
newmat %>%
     ggplot(aes(AvgDur, Events)) +
     geom_point() + 
     theme_economist() +
     labs(title = "Average Duration vs. Number of Events", x = "Average Duration", y = "Number of Events")

# Plot all events against the day of the year 
dat %>%
     ggplot(aes(DOY, Dur_Time, color = Priority)) +
     geom_point() + 
     theme_economist() +
     labs(title = "Event Resolutions - Durations over the Year", x = "Day of the Year", y = "Duration (Days)")

#*****************************************************************************
#
# Overview of Incidents
#
#*****************************************************************************

# Descriptive statistics for incidents:
inc_max_dur <- max(dat_inc$Dur_Time)
inc_min_dur <- min(dat_inc$Dur_Time)
inc_avg_dur <- mean(dat_inc$Dur_Time)
inc_med_dur <- median(dat_inc$Dur_Time)
inc_abv_med <- nrow(dat_inc %>% filter(Dur_Time >  inc_med_dur))
inc_eqt_med <- nrow(dat_inc %>% filter(Dur_Time == inc_med_dur))
inc_bel_med <- nrow(dat_inc %>% filter(Dur_Time <  inc_med_dur))
inc_rng_dur <- inc_max_dur - inc_min_dur

# Establish the number of incidents by priority 
inc_count_p1 <- length(which(dat_inc$IRT == 1 & dat_inc$Priority == 1))
inc_count_p2 <- length(which(dat_inc$IRT == 1 & dat_inc$Priority == 2))
inc_count_p3 <- length(which(dat_inc$IRT == 1 & dat_inc$Priority == 3))
inc_count_p4 <- length(which(dat_inc$IRT == 1 & dat_inc$Priority == 4))
inc_count_p5 <- length(which(dat_inc$IRT == 1 & dat_inc$Priority == 5))

# Establish the incidents by percentage
inc_tot_percent  <- num_rows_inc / num_rows
inc_percent_1    <- inc_count_p1 / num_rows_inc
inc_percent_2    <- inc_count_p2 / num_rows_inc
inc_percent_3    <- inc_count_p3 / num_rows_inc
inc_percent_4    <- inc_count_p4 / num_rows_inc
inc_percent_5    <- inc_count_p5 / num_rows_inc

# Print out summary of incidents by priority and percentage of each
cat("Total incidents         :", num_rows_inc)

cat("Maximum duration        :", round(inc_max_dur, digits = 1))
cat("Minimum duration        :", round(inc_min_dur, digits = 1))
cat("Average duration        :", round(inc_avg_dur, digits = 1))
cat("Median duration         :", round(inc_med_dur, digits = 1))
cat("     Number above median:", round(inc_abv_med, digits = 1))
cat("     Number at median   :", round(inc_eqt_med, digits = 1))
cat("     Number below median:", round(inc_bel_med, digits = 1))
cat("Range of duration       :", round(inc_rng_dur, digits = 1))

#*****************************************************************************
#
# Overview of service requests
#
#*****************************************************************************

# Basic statistics
srs_max_dur <- max(dat_svr$Dur_Time)
srs_min_dur <- min(dat_svr$Dur_Time)
srs_avg_dur <- mean(dat_svr$Dur_Time)
srs_med_dur <- median(dat_svr$Dur_Time)
srs_abv_med <- nrow(dat_svr %>% filter(Dur_Time >  srs_med_dur))
srs_eqt_med <- nrow(dat_svr %>% filter(Dur_Time == srs_med_dur))
srs_bel_med <- nrow(dat_svr %>% filter(Dur_Time <  srs_med_dur))
srs_rng_dur <- srs_max_dur - srs_min_dur

# Establish the number of Service Requests by priority 
srs_count_p1 <- length(which(dat_svr$IRT == 2 & dat_svr$Priority == 1))
srs_count_p2 <- length(which(dat_svr$IRT == 2 & dat_svr$Priority == 2))
srs_count_p3 <- length(which(dat_svr$IRT == 2 & dat_svr$Priority == 3))
srs_count_p4 <- length(which(dat_svr$IRT == 2 & dat_svr$Priority == 4))
srs_count_p5 <- length(which(dat_svr$IRT == 2 & dat_svr$Priority == 5))

# Establish the Service Requests by percentage
srs_tot_percent  <- num_rows_svr / num_rows
srs_percent_1    <- srs_count_p1 / num_rows_svr
srs_percent_2    <- srs_count_p2 / num_rows_svr
srs_percent_3    <- srs_count_p3 / num_rows_svr
srs_percent_4    <- srs_count_p4 / num_rows_svr
srs_percent_5    <- srs_count_p5 / num_rows_svr

# Print out summary of Service Requests by priority and percentage of each
cat("Total Service Requests  :",num_rows_svr)

cat("Maximum duration        :", round(srs_max_dur, digits = 1))
cat("Minimum duration        :", round(srs_min_dur, digits = 1))
cat("Average duration        :", round(srs_avg_dur, digits = 1))
cat("Median duration         :", round(srs_med_dur, digits = 1))
cat("     Number above median:", round(srs_abv_med, digits = 1))
cat("     Number at median   :", round(srs_eqt_med, digits = 1))
cat("     Number below median:", round(srs_bel_med, digits = 1))
cat("Range of duration       :", round(srs_rng_dur, digits = 1))

# Plot output
dat_svr %>%
     ggplot(aes(DOY, Dur_Time, color = Priority)) +
     geom_point() + scale_y_log10() +
     theme_economist() + 
     labs(title = "Service Request Resolutions - Date vs. Duration (log10 y axis)",
          x = "Day of the Year", 
          y = "Duration (Days)")

#*****************************************************************************
#
# Subcategories
#
#*****************************************************************************

# Display the taxonomy
Level_codes

# Isolate on the events causing 80% of the problems - first a vector of events
events <- sum(newmat$Events) 

# Sort the dataframe in descending order / largest # of events at the top
newmat <- newmat[order(newmat$Events, decreasing = TRUE),]

# Loop to identify the events that = the top 80% of all events; 
# the variable epi captures the index / row of when we have 80%
ept    <- events * .8
epi    <- 0
for(i in 1:nrow(newmat)) {
     ifelse(ept <= 0, return, {
          epi <- i
          ept <- ept - newmat[i,4]
     })
}

# Create the final version of the dataframe and total the events
finmat <- newmat[c(1:epi), c(1:9)]

# Display the results
cat("A total of",epi,"combinations account for 80% of the events")
finmat

# Plot all events - number for each category
newmat %>%
     ggplot(aes(L3, Events)) +
     geom_point() + 
     theme_economist() +
     labs(title = "Number of Events by Event Type", x = "Event Category", y = "Number of Events")

# Bar chart of same (a simpler view)
nrs <- nrow(dat %>% filter(L1N == "Software"))
nri <- nrow(dat %>% filter(L1N == "Infrastructure"))
nro <- nrow(dat %>% filter(L1N == "Other"))
pns <- c("1 - Software", "2 - Infrastructure", "3 - Other")
pnn <- c(nrs, nri, nro)
dfn <- data.frame(pns, pnn)

ggplot(dfn, aes(x = pns, y = pnn)) +
     geom_bar(fill = "steelblue", stat="identity") +
     geom_text(aes(pns, pnn, label = pnn), 
               vjust = 1.5, color = "white", size = 4) +theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     labs(title="Events by Major Category (SW, Inf, Other)",
          x="Major Category", y = "Events")

#*****************************************************************************
#
# Incidents breakdown via the 89 subcategories
#
#*****************************************************************************

# Define a new dataframe to hold the output
mat_inc <- data.frame("L1"   = 1:num_rows_inc, 
                      "L2"       = 1:num_rows_inc,
                      "L3"       = 1:num_rows_inc,
                      "Events"   = 1:num_rows_inc,
                      "MaxDur"   = 1:num_rows_inc,
                      "MinDur"   = 1:num_rows_inc,
                      "AvgDur"   = 1:num_rows_inc,
                      "MMSpread" = 1:num_rows_inc,
                      "L3Name"   = 1:num_rows_inc)

# Loop through the data file looking for combinations that actually exist
counter <- 1
for(i in 1:num_L1) {
     for(j in 1:num_L2) {
          for(k in 1:num_L3) {
               x <- dat_inc %>% filter(L1C == i & L2C == j & L3C == k)
               if(nrow(x) != 0) {
                    mat_inc[counter, 1] <- i
                    mat_inc[counter, 2] <- j
                    mat_inc[counter, 3] <- k
                    mat_inc[counter, 4] <- nrow(x)
                    mat_inc[counter, 5] <- max(x$Dur_Time)
                    mat_inc[counter, 6] <- min(x$Dur_Time)
                    mat_inc[counter, 7] <- round(mean(x$Dur_Time), digits=1)
                    mat_inc[counter, 8] <- max(x$Dur_Time) - min(x$Dur_Time)
                    mat_inc[counter, 9] <- Level_codes[k,5]
                    counter <- counter + 1
               }
               else {
                    mat_inc[counter, 1] <- i
                    mat_inc[counter, 2] <- j
                    mat_inc[counter, 3] <- k
                    mat_inc[counter, 4] <- 0
                    mat_inc[counter, 5] <- 0
                    mat_inc[counter, 6] <- 0
                    mat_inc[counter, 7] <- 0
                    mat_inc[counter, 8] <- 0
                    mat_inc[counter, 9] <- Level_codes[k,5]
                    counter <- counter + 1
                    return
               }
          }
     }
}
# Take the newly populated dataframe and eliminate unneeded rows 
newmat_inc <- mat_inc %>% filter(mat_inc$Events != 0 & mat_inc$L3 <= num_L3)

# Sort the dataframe in descending order / largest # of events at the top
newmat_inc <- newmat_inc[order(newmat_inc$Events, decreasing=TRUE),]

# Verify that you have accounted for all 1334 events
events_inc <- sum(newmat_inc$Events) 

# Second loop to identify the events that = the top 80% of all events
# The variable epi captures the index / row of when we have 80%
ept    <- events_inc * .8
epi    <- 0
for(i in 1:nrow(newmat_inc)) {
     ifelse(ept <= 0, return, {
          epi <- i
          ept <- ept - newmat_inc[i,4]
     })
}

# Create the final version of the dataframe and total the events. If the data
finmat_inc <- newmat_inc[c(1:epi), c(1:9)]

# Display the results
cat("A total of",epi,"combinations out of",nrow(newmat_inc),"account for 80% of the incidents")

finmat_inc

# We can look at the top 5 incident categories in more detail.
# Top Incident Number 1

L3_inc_1 <- finmat_inc[1,3]
L3_inc_2 <- finmat_inc[2,3]
L3_inc_3 <- finmat_inc[3,3]
L3_inc_4 <- finmat_inc[4,3]
L3_inc_5 <- finmat_inc[5,3]

L3_inc_1_name <- finmat_inc[1,9]
L3_inc_2_name <- finmat_inc[2,9]
L3_inc_3_name <- finmat_inc[3,9]
L3_inc_4_name <- finmat_inc[4,9]
L3_inc_5_name <- finmat_inc[5,9]

#
# Incident 1 
#

L3_inc_1_name

# define a dataframe to hold the output
df_inc_priorities <- data.frame("Priority" = 1:5,
                                "Events"   = 1:5,
                                "MaxDur"   = 1:5,
                                "MinDur"   = 1:5,
                                "AvgDur"   = 1:5,
                                "MMSpread" = 1:5)

# Loop through the data file
counter <- 1
for(i in 1:5) {
     x <- dat_inc %>% filter(L3C == L3_inc_1 & Priority == i)
     if(nrow(x) != 0) {
          df_inc_priorities[counter, 1] <- i
          df_inc_priorities[counter, 2] <- nrow(x)
          df_inc_priorities[counter, 3] <- max(x$Dur_Time)
          df_inc_priorities[counter, 4] <- min(x$Dur_Time)
          df_inc_priorities[counter, 5] <- round(mean(x$Dur_Time), digits=1)
          df_inc_priorities[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          counter <- counter + 1
     }
     else {
          df_inc_priorities[counter, 1] <- i
          df_inc_priorities[counter, 2] <- 0
          df_inc_priorities[counter, 3] <- 0
          df_inc_priorities[counter, 4] <- 0
          df_inc_priorities[counter, 5] <- 0
          df_inc_priorities[counter, 6] <- 0
          counter <- counter + 1
          return
     }
}

# Display breakout fo the priorities
cat("Priority Breakdown for incident category 1:",L3_inc_1_name)
df_inc_priorities

# Correlations
df_inc_priorities.cor <- cor(df_inc_priorities[-6])
cat("Correlations summary for Incident category 1:",L3_inc_1_name)
df_inc_priorities.cor

#
# Incident 2
#

L3_inc_2_name

# define a dataframe to hold the output
df_inc_priorities <- data.frame("Priority" = 1:5,
                                "Events"   = 1:5,
                                "MaxDur"   = 1:5,
                                "MinDur"   = 1:5,
                                "AvgDur"   = 1:5,
                                "MMSpread" = 1:5)

# Loop through the data file
counter <- 1
for(i in 1:5) {
     x <- dat_inc %>% filter(L3C == L3_inc_2 & Priority == i)
     if(nrow(x) != 0) {
          df_inc_priorities[counter, 1] <- i
          df_inc_priorities[counter, 2] <- nrow(x)
          df_inc_priorities[counter, 3] <- max(x$Dur_Time)
          df_inc_priorities[counter, 4] <- min(x$Dur_Time)
          df_inc_priorities[counter, 5] <- round(mean(x$Dur_Time), digits=1)
          df_inc_priorities[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          counter <- counter + 1
     }
     else {
          df_inc_priorities[counter, 1] <- i
          df_inc_priorities[counter, 2] <- 0
          df_inc_priorities[counter, 3] <- 0
          df_inc_priorities[counter, 4] <- 0
          df_inc_priorities[counter, 5] <- 0
          df_inc_priorities[counter, 6] <- 0
          counter <- counter + 1
          return
     }
}

# Display the results
cat("Priority Breakdown for incident category 2:",L3_inc_2_name)
df_inc_priorities

# Correlations 
df_inc_priorities.cor <- cor(df_inc_priorities[-6])
cat("Correlations summary for Incident category 2:",L3_inc_2_name)
df_inc_priorities.cor

#
# Incident 3
#

L3_inc_3_name

# define a dataframe to hold the output
df_inc_priorities <- data.frame("Priority" = 1:5,
                                "Events"   = 1:5,
                                "MaxDur"   = 1:5,
                                "MinDur"   = 1:5,
                                "AvgDur"   = 1:5,
                                "MMSpread" = 1:5)

# Loop through the data file
counter <- 1
for(i in 1:5) {
     x <- dat_inc %>% filter(L3C == L3_inc_3 & Priority == i)
     if(nrow(x) != 0) {
          df_inc_priorities[counter, 1] <- i
          df_inc_priorities[counter, 2] <- nrow(x)
          df_inc_priorities[counter, 3] <- max(x$Dur_Time)
          df_inc_priorities[counter, 4] <- min(x$Dur_Time)
          df_inc_priorities[counter, 5] <- round(mean(x$Dur_Time), digits=1)
          df_inc_priorities[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          counter <- counter + 1
     }
     else {
          df_inc_priorities[counter, 1] <- i
          df_inc_priorities[counter, 2] <- 0
          df_inc_priorities[counter, 3] <- 0
          df_inc_priorities[counter, 4] <- 0
          df_inc_priorities[counter, 5] <- 0
          df_inc_priorities[counter, 6] <- 0
          counter <- counter + 1
          return
     }
}

# Display the results
cat("Priority Breakdown for incident category 3:",L3_inc_3_name)
df_inc_priorities

# Correlations 
df_inc_priorities.cor <- cor(df_inc_priorities[-6])
cat("Correlations summary for incident category 3:",L3_inc_3_name)
df_inc_priorities.cor

#
# Incident 4
#

L3_inc_4_name

# define a dataframe to hold the output
df_inc_priorities <- data.frame("Priority" = 1:5,
                                "Events"   = 1:5,
                                "MaxDur"   = 1:5,
                                "MinDur"   = 1:5,
                                "AvgDur"   = 1:5,
                                "MMSpread" = 1:5)

# Loop through the data file
counter <- 1
for(i in 1:5) {
     x <- dat_inc %>% filter(L3C == L3_inc_4 & Priority == i)
     if(nrow(x) != 0) {
          df_inc_priorities[counter, 1] <- i
          df_inc_priorities[counter, 2] <- nrow(x)
          df_inc_priorities[counter, 3] <- max(x$Dur_Time)
          df_inc_priorities[counter, 4] <- min(x$Dur_Time)
          df_inc_priorities[counter, 5] <- round(mean(x$Dur_Time), digits=1)
          df_inc_priorities[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          counter <- counter + 1
     }
     else {
          df_inc_priorities[counter, 1] <- i
          df_inc_priorities[counter, 2] <- 0
          df_inc_priorities[counter, 3] <- 0
          df_inc_priorities[counter, 4] <- 0
          df_inc_priorities[counter, 5] <- 0
          df_inc_priorities[counter, 6] <- 0
          counter <- counter + 1
          return
     }
}

# Display the results
cat("Priority Breakdown for incident category 4:",L3_inc_4_name)
df_inc_priorities

# Plot
ggplot(df_inc_priorities, aes(x = Priority, y = Events)) +
     geom_bar(fill = "steelblue", stat="identity") +
     geom_text(aes(Priority, Events, label = Events), 
               vjust = 1.5, color = "white", size = 4) + theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     labs(title="Events by Priority", x="Priority", y = "Events")

# Correlations 
df_inc_priorities.cor <- cor(df_inc_priorities[-6])
cat("Correlations summary for incident category 4:",L3_inc_4_name)
df_inc_priorities.cor

#
# Incident 5
#

L3_inc_5_name

# define a dataframe to hold the output
df_inc_priorities <- data.frame("Priority" = 1:5,
                                "Events"   = 1:5,
                                "MaxDur"   = 1:5,
                                "MinDur"   = 1:5,
                                "AvgDur"   = 1:5,
                                "MMSpread" = 1:5)

# Loop through the data file
counter <- 1
for(i in 1:5) {
     x <- dat_inc %>% filter(L3C == L3_inc_5 & Priority == i)
     if(nrow(x) != 0) {
          df_inc_priorities[counter, 1] <- i
          df_inc_priorities[counter, 2] <- nrow(x)
          df_inc_priorities[counter, 3] <- max(x$Dur_Time)
          df_inc_priorities[counter, 4] <- min(x$Dur_Time)
          df_inc_priorities[counter, 5] <- round(mean(x$Dur_Time), digits=1)
          df_inc_priorities[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          counter <- counter + 1
     }
     else {
          df_inc_priorities[counter, 1] <- i
          df_inc_priorities[counter, 2] <- 0
          df_inc_priorities[counter, 3] <- 0
          df_inc_priorities[counter, 4] <- 0
          df_inc_priorities[counter, 5] <- 0
          df_inc_priorities[counter, 6] <- 0
          counter <- counter + 1
          return
     }
}
cat("Priority Breakdown for incident category 5:",L3_inc_5_name)
df_inc_priorities

ggplot(df_inc_priorities, aes(x = Priority, y = Events)) +
     geom_bar(fill = "steelblue", stat="identity") +
     geom_text(aes(Priority, Events, label = Events), 
               vjust = 1.5, color = "white", size = 4) + theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     labs(title="Events by Priority", x="Priority", y = "Events")

# Correlations 
df_inc_priorities.cor <- cor(df_inc_priorities[-6])
cat("Correlations summary for incident category 5:",L3_inc_5_name)
df_inc_priorities.cor

#*****************************************************************************
#
# Service Requests breakdowns via the 89 categories
#
#*****************************************************************************

# Define a new dataframe to hold the output
mat_svr <- data.frame("L1"       = 1:num_rows_svr, 
                      "L2"       = 1:num_rows_svr,
                      "L3"       = 1:num_rows_svr,
                      "Events"   = 1:num_rows_svr,
                      "MaxDur"   = 1:num_rows_svr,
                      "MinDur"   = 1:num_rows_svr,
                      "AvgDur"   = 1:num_rows_svr,
                      "MMSpread" = 1:num_rows_svr,
                      "L3Name"   = 1:num_rows_svr)

# Loop through the data file looking for combinations that actually exist
counter <- 1
for(i in 1:num_L1) {
     for(j in 1:num_L2) {
          for(k in 1:num_L3) {
               x <- dat_svr %>% filter(L1C == i & L2C == j & L3C == k)
               if(nrow(x) != 0) {
                    mat_svr[counter, 1] <- i
                    mat_svr[counter, 2] <- j
                    mat_svr[counter, 3] <- k
                    mat_svr[counter, 4] <- nrow(x)
                    mat_svr[counter, 5] <- max(x$Dur_Time)
                    mat_svr[counter, 6] <- min(x$Dur_Time)
                    mat_svr[counter, 7] <- round(mean(x$Dur_Time), digits=1)
                    mat_svr[counter, 8] <- max(x$Dur_Time) - min(x$Dur_Time)
                    mat_svr[counter, 9] <- Level_codes[k,5]
                    counter <- counter + 1
               }
               else {
                    mat_svr[counter, 1] <- i
                    mat_svr[counter, 2] <- j
                    mat_svr[counter, 3] <- k
                    mat_svr[counter, 4] <- 0
                    mat_svr[counter, 5] <- 0
                    mat_svr[counter, 6] <- 0
                    mat_svr[counter, 7] <- 0
                    mat_svr[counter, 8] <- 0
                    mat_svr[counter, 9] <- Level_codes[k,5]
                    counter <- counter + 1
                    return
               }
          }
     }
}
# Take the newly populated dataframe and eliminate unneeded rows 
newmat_svr <- mat_svr %>% filter(mat_svr$Events != 0 & mat_svr$L3 < 90)

# Sort the dataframe in descending order / largest # of events at the top
newmat_svr <- newmat_svr[order(newmat_svr$Events, decreasing=TRUE),]

# Verify that you have accounted for all events
events_svr <- sum(newmat_svr$Events) 

# Loop to identify the events that = the top 80% of all events
# The variable epi captures the index / row of when we have 80%
ept    <- events_svr * .8
epi    <- 0
for(i in 1:nrow(newmat_svr)) {
     ifelse(ept <= 0, return, {
          epi <- i
          ept <- ept - newmat_svr[i,4]
     })
}

# Create the final version of the dataframe and total the events
finmat_svr <- newmat_svr[c(1:epi), c(1:9)]

# Display the results
cat("A total of",epi,"combinations out of",nrow(newmat_svr),"account for 80% of the service requests")

finmat_svr

# As before, create the names and codes for the top 5 requests
L3_svr_1 <- finmat_svr[1,3]
L3_svr_2 <- finmat_svr[2,3]
L3_svr_3 <- finmat_svr[3,3]
L3_svr_4 <- finmat_svr[4,3]
L3_svr_5 <- finmat_svr[5,3]

L3_svr_1_name <- finmat_svr[1,9]
L3_svr_2_name <- finmat_svr[2,9]
L3_svr_3_name <- finmat_svr[3,9]
L3_svr_4_name <- finmat_svr[4,9]
L3_svr_5_name <- finmat_svr[5,9]

#
# Service Request 1
#

L3_svr_1_name

# define a dataframe to hold the output
df_svr_priorities <- data.frame("Priority" = 1:5,
                                "Events"   = 1:5,
                                "MaxDur"   = 1:5,
                                "MinDur"   = 1:5,
                                "AvgDur"   = 1:5,
                                "MMSpread" = 1:5)

# Loop through the data file
counter <- 1
for(i in 1:5) {
     x <- dat_svr %>% filter(L3C == L3_svr_1 & Priority == i)
     if(nrow(x) != 0) {
          df_svr_priorities[counter, 1] <- i
          df_svr_priorities[counter, 2] <- nrow(x)
          df_svr_priorities[counter, 3] <- max(x$Dur_Time)
          df_svr_priorities[counter, 4] <- min(x$Dur_Time)
          df_svr_priorities[counter, 5] <- round(mean(x$Dur_Time), digits=1)
          df_svr_priorities[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          counter <- counter + 1
     }
     else {
          df_svr_priorities[counter, 1] <- i
          df_svr_priorities[counter, 2] <- 0
          df_svr_priorities[counter, 3] <- 0
          df_svr_priorities[counter, 4] <- 0
          df_svr_priorities[counter, 5] <- 0
          df_svr_priorities[counter, 6] <- 0
          counter <- counter + 1
          return
     }
}

# Display the results
cat("Priority Breakdown for service request category 1:",L3_svr_1_name)
df_svr_priorities

# Correlations
df_svr_priorities.cor <- cor(df_svr_priorities[-6])
cat("Correlations summary for service request category 1:",L3_svr_1_name)
df_svr_priorities.cor

#
# Service Request 2
#

L3_svr_2_name

# define a dataframe to hold the output
df_svr_priorities <- data.frame("Priority" = 1:5,
                                "Events"   = 1:5,
                                "MaxDur"   = 1:5,
                                "MinDur"   = 1:5,
                                "AvgDur"   = 1:5,
                                "MMSpread" = 1:5)

# Loop through the data file
counter <- 1
for(i in 1:5) {
     x <- dat_svr %>% filter(L3C == L3_svr_2 & Priority == i)
     if(nrow(x) != 0) {
          df_svr_priorities[counter, 1] <- i
          df_svr_priorities[counter, 2] <- nrow(x)
          df_svr_priorities[counter, 3] <- max(x$Dur_Time)
          df_svr_priorities[counter, 4] <- min(x$Dur_Time)
          df_svr_priorities[counter, 5] <- round(mean(x$Dur_Time), digits=1)
          df_svr_priorities[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          counter <- counter + 1
     }
     else {
          df_svr_priorities[counter, 1] <- i
          df_svr_priorities[counter, 2] <- 0
          df_svr_priorities[counter, 3] <- 0
          df_svr_priorities[counter, 4] <- 0
          df_svr_priorities[counter, 5] <- 0
          df_svr_priorities[counter, 6] <- 0
          counter <- counter + 1
          return
     }
}
# Display the results
cat("Priority Breakdown for service request category 2:",L3_svr_2_name)
df_svr_priorities

# Correlations 
df_svr_priorities.cor <- cor(df_svr_priorities[-6])
cat("Correlations summary for service request category 2:",L3_svr_2_name)
df_svr_priorities.cor

#
# Service Request 3
#

L3_svr_3_name

# define a dataframe to hold the output
df_svr_priorities <- data.frame("Priority" = 1:5,
"Events"   = 1:5,
"MaxDur"   = 1:5,
"MinDur"   = 1:5,
"AvgDur"   = 1:5,
"MMSpread" = 1:5)

# Loop through the data file
counter <- 1
     for(i in 1:5) {
          x <- dat_svr %>% filter(L3C == L3_svr_3 & Priority == i)
          if(nrow(x) != 0) {
               df_svr_priorities[counter, 1] <- i
               df_svr_priorities[counter, 2] <- nrow(x)
               df_svr_priorities[counter, 3] <- max(x$Dur_Time)
               df_svr_priorities[counter, 4] <- min(x$Dur_Time)
               df_svr_priorities[counter, 5] <- round(mean(x$Dur_Time), digits=1)
               df_svr_priorities[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          counter <- counter + 1
          }
          else {
               df_svr_priorities[counter, 1] <- i
               df_svr_priorities[counter, 2] <- 0
               df_svr_priorities[counter, 3] <- 0
               df_svr_priorities[counter, 4] <- 0
               df_svr_priorities[counter, 5] <- 0
               df_svr_priorities[counter, 6] <- 0
               counter <- counter + 1
               return
          }
     }

# Display the results
cat("Priority Breakdown for service request category 3:",L3_svr_3_name)
df_svr_priorities

# Correlations 
df_svr_priorities.cor <- cor(df_svr_priorities[-6])
cat("Correlations summary for service request category 3:",L3_svr_3_name)
df_svr_priorities.cor

#
# Service Request 4
#

L3_svr_4_name

# define a dataframe to hold the output
df_svr_priorities <- data.frame("Priority" = 1:5,
                                "Events"   = 1:5,
                                "MaxDur"   = 1:5,
                                "MinDur"   = 1:5,
                                "AvgDur"   = 1:5,
                                "MMSpread" = 1:5)

# Loop through the data file
counter <- 1
for(i in 1:5) {
     x <- dat_svr %>% filter(L3C == L3_svr_4 & Priority == i)
     if(nrow(x) != 0) {
          df_svr_priorities[counter, 1] <- i
          df_svr_priorities[counter, 2] <- nrow(x)
          df_svr_priorities[counter, 3] <- max(x$Dur_Time)
          df_svr_priorities[counter, 4] <- min(x$Dur_Time)
          df_svr_priorities[counter, 5] <- round(mean(x$Dur_Time), digits=1)
          df_svr_priorities[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          counter <- counter + 1
     }
     else {
          df_svr_priorities[counter, 1] <- i
          df_svr_priorities[counter, 2] <- 0
          df_svr_priorities[counter, 3] <- 0
          df_svr_priorities[counter, 4] <- 0
          df_svr_priorities[counter, 5] <- 0
          df_svr_priorities[counter, 6] <- 0
          counter <- counter + 1
          return
     }
}

# Display the results
cat("Priority Breakdown for service request category 4:",L3_svr_4_name)
df_svr_priorities

# Correlations 
df_svr_priorities.cor <- cor(df_svr_priorities[-6])
cat("Correlations summary for service request category 4:",L3_svr_4_name)
df_svr_priorities.cor

#
# Service Request 5
#

L3_svr_5_name

# define a dataframe to hold the output
df_svr_priorities <- data.frame("Priority" = 1:5,
                                "Events"   = 1:5,
                                "MaxDur"   = 1:5,
                                "MinDur"   = 1:5,
                                "AvgDur"   = 1:5,
                                "MMSpread" = 1:5)

# Loop through the data file
counter <- 1
for(i in 1:5) {
     x <- dat_svr %>% filter(L3C == L3_svr_5 & Priority == i)
     if(nrow(x) != 0) {
          df_svr_priorities[counter, 1] <- i
          df_svr_priorities[counter, 2] <- nrow(x)
          df_svr_priorities[counter, 3] <- max(x$Dur_Time)
          df_svr_priorities[counter, 4] <- min(x$Dur_Time)
          df_svr_priorities[counter, 5] <- round(mean(x$Dur_Time), digits=1)
          df_svr_priorities[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          counter <- counter + 1
     }
     else {
          df_svr_priorities[counter, 1] <- i
          df_svr_priorities[counter, 2] <- 0
          df_svr_priorities[counter, 3] <- 0
          df_svr_priorities[counter, 4] <- 0
          df_svr_priorities[counter, 5] <- 0
          df_svr_priorities[counter, 6] <- 0
          counter <- counter + 1
          return
     }
}

# Display the Results
cat("Priority Breakdown for service request category 5:",L3_svr_5_name)
df_svr_priorities

# Correlations 
df_svr_priorities.cor <- cor(df_svr_priorities[-6])
cat("Correlations summary for service request category 5:",L3_svr_5_name)
df_svr_priorities.cor

#*****************************************************************************
#
# Owners and Duration Times
#
#*****************************************************************************

# Set up the list of owners to go through
owners_list <- staff_names %>% filter(SD == "Y")
x_range_for_plot_max <- nrow(owners_list)

# define a dataframe to hold the output
df_inc_owners <- data.frame("Owner_ID" = 1:x_range_for_plot_max,
                            "Events"   = 1:x_range_for_plot_max,
                            "MaxDur"   = 1:x_range_for_plot_max,
                            "MinDur"   = 1:x_range_for_plot_max,
                            "AvgDur"   = 1:x_range_for_plot_max,
                            "MMSpread" = 1:x_range_for_plot_max,
                            "SD"       = 1:x_range_for_plot_max,
                            "Owner"    = 1:x_range_for_plot_max)

# Loop through the data file
counter <- 1
for(i in owners_list$Owner_ID) {
     x <- dat_inc %>% filter(Owner_ID == i)
     if(nrow(x) != 0) {
          df_inc_owners[counter, 1] <- i
          df_inc_owners[counter, 2] <- nrow(x)
          df_inc_owners[counter, 3] <- max(x$Dur_Time)
          df_inc_owners[counter, 4] <- min(x$Dur_Time)
          df_inc_owners[counter, 5] <- round(mean(x$Dur_Time), digits=1)
          df_inc_owners[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          df_inc_owners[counter, 7] <- round(sd(x$Dur_Time), digits=1)
          df_inc_owners[counter, 8] <- staff_names[i,1]
          counter <- counter + 1
     }
     else {
          df_inc_owners[counter, 1] <- i
          df_inc_owners[counter, 2] <- 0
          df_inc_owners[counter, 3] <- 0
          df_inc_owners[counter, 4] <- 0
          df_inc_owners[counter, 5] <- 0
          df_inc_owners[counter, 6] <- 0
          df_inc_owners[counter, 7] <- 0
          df_inc_owners[counter, 8] <- staff_names[i,1]
          counter <- counter + 1
          }
}

# Display the list of owners and events
df_inc_owners

# Plot the owners and events
ggplot(data = df_inc_owners, aes(x=Owner, y=Events)) +
     geom_bar(fill = "steelblue", stat="identity") +
     geom_text(aes(Owner, Events, label = Events), 
               vjust = -0.4, color = "black", size = 3.5) +
     theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     labs(title="Incidents - Events by Owner",
          x="Owner Name", y = "Incidents")

# Plot Owners and average duration times
ggplot(data=df_inc_owners, aes(x=Owner, y=AvgDur)) +
     geom_bar(fill = "steelblue", stat="identity") + 
     geom_text(aes(Owner, AvgDur, label = AvgDur), 
               vjust = -0.5, color = "black", size = 3.5) +
     theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     labs(title="Incidents - Average Duration (Days) by Owner",
          x="Owner Name", y = "Average Duration")

# Plot owners and standard deviations
ggplot(df_inc_owners, aes(x=Owner, y=SD)) +
     geom_bar(fill = "steelblue", stat="identity") +
     geom_text(aes(Owner, SD, label = SD), 
               vjust = -0.5, color = "black", size = 3.5) +
     theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     labs(title="Incidents - Standard Deviations by Owner",
          x="Owner Name", y = "Standard Deviation")

# Service Requests

# Set up the list of owners to go through
owners_list <- staff_names %>% filter(SD == "Y")
x_range_for_plot_max <- nrow(owners_list)

# define a dataframe to hold the output
df_svr_owners <- data.frame("Owner_ID" = 1:x_range_for_plot_max,
                            "Events"   = 1:x_range_for_plot_max,
                            "MaxDur"   = 1:x_range_for_plot_max,
                            "MinDur"   = 1:x_range_for_plot_max,
                            "AvgDur"   = 1:x_range_for_plot_max,
                            "MMSpread" = 1:x_range_for_plot_max,
                            "SD"       = 1:x_range_for_plot_max,
                            "Owner"    = 1:x_range_for_plot_max)

# Loop through the data file
counter <- 1
for(i in owners_list$Owner_ID) {
     x <- dat_svr %>% filter(Owner_ID == i)
     if(nrow(x) != 0) {
          df_svr_owners[counter, 1] <- i
          df_svr_owners[counter, 2] <- nrow(x)
          df_svr_owners[counter, 3] <- max(x$Dur_Time)
          df_svr_owners[counter, 4] <- min(x$Dur_Time)
          df_svr_owners[counter, 5] <- round(mean(x$Dur_Time), digits=1)
          df_svr_owners[counter, 6] <- max(x$Dur_Time) - min(x$Dur_Time)
          df_svr_owners[counter, 7] <- round(sd(x$Dur_Time), digits=1)
          df_svr_owners[counter, 8] <- staff_names[i,1]
          counter <- counter + 1
     }
     else {
          df_svr_owners[counter, 1] <- i
          df_svr_owners[counter, 2] <- 0
          df_svr_owners[counter, 3] <- 0
          df_svr_owners[counter, 4] <- 0
          df_svr_owners[counter, 5] <- 0
          df_svr_owners[counter, 6] <- 0
          df_svr_owners[counter, 7] <- 0
          df_svr_owners[counter, 8] <- staff_names[i,1]
          counter <- counter + 1
          return
     }
}

# Display results
df_svr_owners

# Sort in descending order
df_svr_owners_desc <- df_svr_owners[order(df_svr_owners$Events, decreasing=TRUE),]

# Plot the result
ggplot(data=df_svr_owners, aes(x=Owner, y=Events)) +
     geom_bar(fill = "steelblue", stat="identity") +
     geom_text(aes(Owner, Events, label = Events), 
               vjust = -0.5, color = "black", size = 3.5) +
     theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     labs(title="Service Requests - Events by Owner",
          x="Owner Name", y = "Service Requests")

# Plot average duration for each owner
ggplot(data=df_svr_owners, aes(x=Owner, y=AvgDur)) +
     geom_bar(fill = "steelblue", stat="identity") + 
     geom_text(aes(Owner, AvgDur, label = AvgDur), 
               vjust = -0.5, color = "black", size = 3.5) +
     theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     labs(title="Service Requests - Average Duration by Owner",
          x="Owner Name", y = "Average Duration")

# Plot standard deviation for each owner
ggplot(df_svr_owners, aes(x=Owner, y=SD)) +
     geom_bar(fill = "steelblue", stat="identity") +
     geom_text(aes(Owner, SD, label = SD), 
               vjust = -0.5, color = "black", size = 3.5) +
     theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     labs(title="Service Requests - Standard Deviations by Owner",
          x="Owner Name", y = "Standard Deviation")

# Print out the staff rankings for each owner (done manually at this point)
staff_rankings

#*****************************************************************************
#
# **Predictive Model Overview**
#
#*****************************************************************************

# Print out basic statistics
cat("Events will take",round(mean(dat$Dur_Time, digits=1)),"days")
cat("Incidents will take",round(mean(dat_inc$Dur_Time, digits=1)),"days")
cat("Service requests will take",round(mean(dat_svr$Dur_Time, digits=1)),"days")

# Average durations for each incident category
cat_inc_pred <- newmat_inc %>%
     select(L3Name, AvgDur) %>%
     filter(AvgDur != 0, L3Name != "Bus Apps Placeholder") %>%
     arrange(L3Name)

cat("Incident resolution times (predict Zero if not listed):")
cat_inc_pred

# Average durations for each service request category
cat_svr_pred <- newmat_svr %>%
     select(L3Name, AvgDur) %>%
     filter(AvgDur != 0, L3Name != "Bus Apps Placeholder") %>%
     arrange(L3Name)

# Print out result
cat("Service Request resolution times (predict Zero if not listed):")
cat_svr_pred

#
# Build illustrative table of all staff against all categories (incidents)
#

# Collect the data pieces for joining
df_inc <- dat_inc %>%
     select(L3C, Owner_ID, Priority, Dur_Time) 

sn_inc <- staff_names %>%
     select(Owner, Owner_ID)

lc_inc <- Level_codes %>%
     select(L3C, L3N)

# Join the owners to the base data
df_inc_1 <- join(df_inc, sn_inc, by = "Owner_ID", type = "inner")
head(df_inc_1, 50)

# Join the names of the categories to the first join
df_inc_2 <- join(df_inc_1, lc_inc, by = "L3C", type = "left")
head(df_inc_2, 20)

# Set up the list of owners to go through
owners_list <- staff_names %>% filter(SD == "Y")
x_range_for_plot_max <- nrow(Level_codes)

# define a dataframe to hold the output
df_coa <- data.frame("Category"    = 1:x_range_for_plot_max,
                     "Owner"       = 1:x_range_for_plot_max,
                     "AvgDur"      = 1:x_range_for_plot_max)

# Loop through the data file
counter <- 1
for(i in 1:num_L3) {
     for(j in owners_list$Owner_ID) {
          x <- df_inc_2 %>% filter(L3C == i & Owner_ID == j)
          if(nrow(x) != 0) {
               df_coa[counter, 1] <- Level_codes[i,5]
               df_coa[counter, 2] <- staff_names[j,1]
               df_coa[counter, 3] <- round(mean(x$Dur_Time, digits = 2))
               counter <- counter + 1
          }
          else {
               df_coa[counter, 1] <- 0
               df_coa[counter, 2] <- Level_codes[j,5]
               df_coa[counter, 3] <- 0
          }
     }
}

# Display base output of categories, owners, and durations
df_coa 

# Reshape the data to display category / owner table
df_coa_table <- dcast(df_coa, Category~Owner,value.var = "AvgDur")
df_coa_table

#-----------------------------------------------------------------------
#
# A model to predict duration times more accurately
#
#-----------------------------------------------------------------------

# Set up the dataset for incidents only
dat_inc_predict <- dat_inc %>% select(A_ID, L3C, L3N, Dur_Time, Owner_ID, Owned_By)

# Remove lines with less than 2 instances (else the aver)
cat_index <- as.data.frame(table(dat_inc_predict$L3C)) %>%
     filter(Freq > 1) %>% select(Var1) 

# Subset the dataframe to include just those rows with multiple values
inc_predict <- subset(dat_inc_predict, L3C %in% cat_index$Var1)

# Partition the dataset / create the training and test sets
set.seed(1)
test_index <- createDataPartition(y = inc_predict$Dur_Time, times = 1, p = 0.2, list = FALSE)
train_set  <- inc_predict[-test_index,]
temp       <- inc_predict[test_index,]

# Make sure Owner_ID and L3C in the test_set are also in the train_set
test_set <- temp %>% 
     semi_join(train_set, by = "L3C") %>%
     semi_join(train_set, by = "Owner_ID")

# Add rows removed from test_set back into train_set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Develop the RMSE for model comparisons
RMSE <- function(act_duration, pred_duration){
     sqrt(mean((act_duration - pred_duration)^2))
}

#
# Establish a baseline - the average duration for all incidents
#

mu <- mean(train_set$Dur_Time)
avg_rmse <- RMSE(train_set$Dur_Time, mu)

# set up a dataframe to hold the results of this average and further RMSEs
rmse_results <- data_frame(method = "Simple Average (Mean) Duration", RMSE = avg_rmse)

# Display the initial result
rmse_results

#
# Calculate b_c as the average of a given category's duration minus the
# overall average
# 

mu <- mean(train_set$Dur_Time)

# Set up dataframe to capture the results
category_avgs <- data.frame("L3C" = 1:num_L3,
                            "b_c" = 1:num_L3)

counter <- 1
for(i in 1:num_L3) {
     x <- train_set %>% filter(L3C == i)
     if(nrow(x) != 0) {
          category_avgs[counter,1] <- i
          category_avgs[counter,2] <- mean(x$Dur_Time - mu)
          counter <- counter + 1
     }
     else {
          category_avgs[counter,1] <- i
          category_avgs[counter,2] <- 0
          counter <- counter + 1
     }
}
# The result is a dataframe with 89 rows, one for each category
head(category_avgs)

# Generate predicted durations returned by the model
predicted_durations <- mu + test_set %>% 
     left_join(category_avgs, by = 'L3C') %>%
     pull(b_c)

# Calculate the RMSE
model_1_rmse <- RMSE(predicted_durations, test_set$Dur_Time)

# Add the results to the rmse_results summary
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Category Effects Model",  
                                     RMSE = model_1_rmse))

# Display the result
rmse_results

# 
# Introduce b_o, to account for owner-specific effects
#

# Reset the average
mu <- mean(train_set$Dur_Time)

# Build owner averages data subset
owner_avgs <- train_set %>% 
     left_join(category_avgs, by = 'L3C') %>%
     select(L3C, L3N, Dur_Time, Owner_ID, b_c)

# Set up dataframe to capture the b_o values for each owner
b_o_values <- data.frame("Owner_ID"  = 1:num_staff,
                         "b_o_value" = 1:num_staff,
                         "s"         = 1:num_staff,
                         "dur_sum"   = 1:num_staff,
                         "n_u"       = 1:num_staff)

# Loop through and calculate the b_o for each owner
counter <- 1
for(i in 1:num_staff) {
     x <- owner_avgs %>% filter(Owner_ID == i)
     if(nrow(x) != 0) {
          b_o_values[counter,1] <- i
          b_o_values[counter,2] <- mean(x$Dur_Time - mu - x$b_c)
          b_o_values[counter,3] <- sum(x$Dur_Time - mu)
          b_o_values[counter,4] <- sum(x$Dur_Time)
          b_o_values[counter,5] <- nrow(x)
          counter <- counter + 1
     }
     else {
          b_o_values[counter,1] <- i
          b_o_values[counter,2] <- 0
          b_o_values[counter,3] <- 0
          b_o_values[counter,4] <- 0
          b_o_values[counter,5] <- 0
          counter <- counter + 1
     }
}

# Filter out instances where the b_o value is zero
b_o_values <- b_o_values %>% filter(b_o_value != 0)

# Generate predicted durations returned by the model
predicted_durations <- test_set %>% 
     left_join(category_avgs, by = 'L3C') %>%
     left_join(b_o_values, by = 'Owner_ID') %>%
     mutate(pred = mu + b_c + b_o_value) %>%
     pull(pred)

# Calculate the RMSE
model_2_rmse <- RMSE(predicted_durations, test_set$Dur_Time)

# Add the results to the rmse_results summary
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Owner Effects Model",  
                                     RMSE = model_2_rmse))

# Display the updated result
rmse_results

#
# See if regularization improves the model
#

# Establish the average duration for all incidents
mu <- mean(train_set$Dur_Time)

# Take the sum of each of the movie ratings minus the average rating
# and calculate the number of ratings for that movie
sum_category_dur_time <- data.frame("L3C"  = 1:num_L3,
                                    "scdt" = 1:num_L3,
                                    "n_c"  = 1:num_L3,
                                    "n_cv" = 1:num_L3)

# Loop to build dataframe
counter <- 1
for(i in 1:num_L3) {
     x <- train_set %>% filter(L3C == i)
     if(nrow(x) != 0) {
          sum_category_dur_time[counter,1] <- i
          sum_category_dur_time[counter,2] <- sum(x$Dur_Time - mu) 
          sum_category_dur_time[counter,3] <- nrow(x)
          sum_category_dur_time[counter,4] <- sum(x$Dur_Time - mu)/(nrow(x))
          counter <- counter + 1
     }
     else {
          counter <- counter + 1
     }
}

# Select a Lambda (range here was arrived at experimentally)
lambdas <- seq(15, 500, 1)

# Join to the training set, calc new b_c and prediction
rmses <- sapply(lambdas, function(l){
     predicted_durations <- test_set %>% 
          left_join(sum_category_dur_time, by = 'L3C') %>% 
          mutate(b_c = scdt / (n_c + l)) %>%
          mutate(pred = mu + b_c) %>%
          pull(pred)
     return(RMSE(predicted_durations, test_set$Dur_Time))
})

# Plot lambdas and display lambda with lowest value
qplot(lambdas, rmses)  

# Establish the minimum lambda
lambdas[which.min(rmses)]
paste("Optimal lambda value:",lambdas[which.min(rmses)])

# Use optimal lambda to calculate the new b_c
lambda <- lambdas[which.min(rmses)]

# Overall average
mu <- mean(train_set$Dur_Time)

# Set up dataframe
category_reg_avgs <- data.frame("L3C"  = 1:num_L3,
                                "b_c"  = 1:num_L3,
                                "lcra" = 1:num_L3)

# Populate dataframe
counter <- 1
for(i in 1:num_L3) {
     x <- train_set %>% filter(L3C == i)
     category_reg_avgs[counter,1] <- i
     category_reg_avgs[counter,2] <- sum(x$Dur_Time - mu) / (nrow(x) + lambda) 
     category_reg_avgs[counter,3] <- nrow(x)
     counter <- counter + 1
}

# Print result - a dataframe with 89 rows, one for each category
head(category_reg_avgs)

# Generate new prediction
predicted_durations <- test_set %>% 
     left_join(category_reg_avgs, by = "L3C") %>%
     mutate(pred = mu + b_c) %>%
     pull(pred)

# Calculate RMSE
model_3_rmse <- RMSE(predicted_durations, test_set$Dur_Time)

# Update RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Regularized category Effects Model",  
                                     RMSE = model_3_rmse))

# Display updated results
rmse_results

#########################################################################
# End of code
#########################################################################
