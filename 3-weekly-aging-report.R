##############################################################################
#
# Cherwell Weekly Aging Report
# Bill James / jamesw@csps.com
#
# Files:  https://github.com/wjamesTMC/tsg-cherwell.git
#
##############################################################################

#
# Library setups
#

# Import libraries
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(scales)
library(reshape2)
library(stringi)
library(expss)
library(grid)
library(gridExtra)
library(lattice)
library(janitor)
library(rmarkdown)
library(kableExtra)
library(lubridate)

#--------------------------------------------------------------------
#
# File open, cleanup, and set up for the analysis
#
#--------------------------------------------------------------------

#
# Identify and open open tickets file
#

# Import and Open the data file / Establish the data set
data_filename <- readline("Data file name: ")
dat <- read.csv(file = data_filename, skip = 1, header = TRUE, stringsAsFactors = FALSE) 

# Clean up column / vector names
dat <- rename(dat, replace = c("Incident.ID" = "ID",
                               "Created.Date.Time" = "Created",
                               "Customer.Display.Name" = "Customer",
                               "Incident.Type" = "Type",
                               "Short.Description" = "Desc",
                               "Owned.By" = "Owner",
                               "SLA.Resolve.By.Deadline" = "Due"))

# Get rid of unneeded column and row information and sort by created date
dat <- dat %>% select(ID, Created, Customer, Type, Desc, Owner, Due)
dat <- dat %>% filter(dat$Created != "")

# Shorten some fields
dat[dat == "Incident"]            <- "Inc"
dat[dat == "Service Request"]     <- "SR"

# Simplify timestamp to simple dates
dat$Created <- sub(' .*', '', dat$Created)
dat$Created <- as.Date(dat$Created, format = "%m/%d/%Y")

dat$Due <- sub(' .*', '', dat$Due)
dat$Due <- as.Date(dat$Due, format = "%m/%d/%Y")

# Add column for days open (Duration)
dat <- dat %>% mutate(Duration = difftime(Sys.Date(), dat$Created, units = "days"))
dat <- dat %>% arrange(desc(Duration))

# Establish groupings for the open tickets
days_07 <- dat %>% filter(Duration <=  7) %>% select(-Desc)
days_30 <- dat %>% filter(Duration <= 30 & Duration >  7) %>% select(-Desc)
days_60 <- dat %>% filter(Duration <= 60 & Duration > 30) %>% select(-Desc)
over_60 <- dat %>% filter(Duration >  60) %>% select(-Desc)

# Establish groupings for SLA compliance
inc_sla      <- dat %>% filter(Type == "Inc" & Duration <=  2) %>% select(-Desc)
inc_sla_perc <- (nrow(inc_sla) / nrow(dat %>% filter(Type == "Inc"))) * 100
inc_sla_perc <- round(inc_sla_perc, digits = 2)

SR_sla       <- dat %>% filter(Type =="SR" & Duration <= 10) %>% select(-Desc)
SR_sla_perc  <- (nrow(SR_sla) / nrow(dat %>% filter(Type == "SR"))) * 100
SR_sla_perc  <- round(SR_sla_perc, digits = 2)


#
# Print out results
#

cat("Total open tickets:", nrow(dat), "/ Incidents:", 
    nrow(dat %>% filter(Type == "Inc")), "/ SR's:",
         nrow(dat %>% filter(Type == "SR")))
cat("Incidents meeting SLA guidelines:", inc_sla_perc, "%")
cat("Service Requests meeting SLA guidelines:", SR_sla_perc, "%")

# Part 1 - print out chronological groupings sorted by owner within grouping

cat("===========================================================================")
cat("Part 1: Tickets Sorted by Owner Within Age")
cat("===========================================================================", "\n")

cat("Open Tickets Aged over 60 Days")
over_60 <- over_60 %>% arrange(Owner, desc(Duration))
over_60
cat("\n")

cat("Open Tickets Aged 30 - 60 Days")
days_60 <- days_60 %>% arrange(Owner, desc(Duration))
days_60
cat("\n")

cat("Open Tickets Aged up to 30 Days")
days_30 <- days_30 %>% arrange(Owner, desc(Duration))
days_30
cat("\n")

cat("Open Tickets Aged 1 Week")
days_07 <- days_07 %>% arrange(Owner, desc(Duration))
days_07
cat("\n")

# Part 2 - print out tickets in chronological groupings

# cat("===========================================================================")
# cat("Part 2 - Tickets Sorted Chronologically by Age")
# cat("===========================================================================", "\n")
# 
# cat("Aged over 60 Days -", nrow(over_60), "tickets", round((nrow(over_60) / nrow(dat) * 100), digits = 3), "% of total")
# over_60 <- over_60 %>% arrange(desc(Duration))
# over_60
# cat("\n")
# 
# cat("Aged 30 - 60 Days -", nrow(days_60), "tickets", round((nrow(days_60) / nrow(dat) * 100) , digits = 3), "% of total")
# days_60 <- days_60 %>% arrange(desc(Duration))
# days_60
# cat("\n")
# 
# cat("Aged up to 30 Days -", nrow(days_30), "tickets", round((nrow(days_30) / nrow(dat) * 100), digits = 3), "% of total")
# days_30 <- days_30 %>% arrange(desc(Duration))
# days_30
# cat("\n")
# 
# cat("Aged 1 Week or less -", nrow(days_07), "tickets", round((nrow(days_07) / nrow(dat) * 100), digits = 3), "% of total")
# days_07 <- days_07 %>% arrange(desc(Duration))
# days_07
# cat("\n")

#
# Capture statistics for this week and append to file
#

Opens         <- nrow(dat)
Incidents     <- nrow(dat %>% filter(Type == "Inc"))
Inc_Percent   <- round(((Incidents / Opens) * 100), digits = 1)
Inc_Mtg_SLA   <- inc_sla_perc
SRs           <- nrow(dat %>% filter(Type == "SR"))
SR_Percent    <- round(((SRs / Opens) * 100), digits = 1)
SR_Mtg_SLA    <- SR_sla_perc
Days_07       <- nrow(days_07)
Days_30       <- nrow(days_30)
Days_60       <- nrow(days_60)
Over_60       <- nrow(over_60)

# Establish the week ending date for these particular statistics
Week_Ending <- readline("What is the week ending date for this report [yyyy-mm-dd]? ")

# Create dataframe to hold the results
week_ending_data <- data.frame(Week_Ending,
                               Opens,
                               Incidents,
                               Inc_Percent,
                               Inc_Mtg_SLA,
                               SRs,
                               SR_Percent,
                               SR_Mtg_SLA,
                               Days_07,
                               Days_30,
                               Days_60,
                               Over_60)

# Open historical data file and append current week's data
write.table(week_ending_data, file = "0_Output_cherwell_stats.csv", append = TRUE, sep = ",", col.names = FALSE)


#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------



