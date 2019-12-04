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
library(stringi)
library(grid)
library(gridExtra)
library(lattice)
library(janitor)
library(rmarkdown)
library(kableExtra)
library(lubridate)
library(stringr)
library("googledrive")
library(googlesheets)

#--------------------------------------------------------------------
#
# File open, cleanup, and set up for the analysis
#
#--------------------------------------------------------------------

#
# Open the Google Sheet
#

# Open the data file - a Google Sheet in user's top level directory
data_filename <- gs_title("CAR-input-2019-10-25")
dat <- gs_read(data_filename, skip = 1, header = TRUE, stringsAsFactors = FALSE)

# Generate the week ending data from the data file name
Week_Ending <- str_sub(data_filename[2], 11, 20)

# Clean up column / vector names
dat <- rename(dat, replace = c("Incident ID" = "ID",
                               "Created Date Time" = "Created",
                               "Customer Display Name" = "Customer",
                               "Incident Type" = "Type",
                               "Owned By" = "Owner",
                               "SLA Resolve By Deadline" = "Due"))

# Get rid of unneeded column and row information and sort by created date
dat <- dat %>% select(ID, Created, Customer, Type, Owner, Due)
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
dat <- dat %>% mutate(Duration = difftime(as.Date(Week_Ending), dat$Created, units = "days"))
dat <- dat %>% arrange(desc(Duration))

# Establish groupings for the open tickets
days_07 <- dat %>% filter(Duration <=  7) 
days_30 <- dat %>% filter(Duration <= 30 & Duration >  7) 
days_60 <- dat %>% filter(Duration <= 60 & Duration > 30) 
over_60 <- dat %>% filter(Duration >  60) 

# Establish groupings for SLA compliance
inc_sla      <- dat %>% filter(Type == "Inc" & Duration <=  2) 
inc_sla_perc <- (nrow(inc_sla) / nrow(dat %>% filter(Type == "Inc"))) * 100
inc_sla_perc <- round(inc_sla_perc, digits = 2)

SR_sla       <- dat %>% filter(Type =="SR" & Duration <= 10) 
SR_sla_perc  <- (nrow(SR_sla) / nrow(dat %>% filter(Type == "SR"))) * 100
SR_sla_perc  <- round(SR_sla_perc, digits = 2)

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

# Create dataframe to hold the results for writing to the file
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
output_file <- gs_title("CAR-output-cherwell-stats")
gs_add_row(output_file, ws = 1, input = week_ending_data, verbose = FALSE)

# Part 1 - ticket sorted by age within owner

owner_durs <- dat %>% arrange(Owner, desc(Duration)) 

# Open file with list of staff names
staff_filename <- gs_title("CAR-SD_Staff_List")
staff_list     <- gs_read(staff_filename, header = TRUE, stringsAsFactors = TRUE)
sd_staff       <- staff_list %>% filter(WAR == "Y") %>% select(Owner)
nsd_staff      <- staff_list %>% filter(WAR == "N") %>% select(Owner)

# sd_data_list   <- list()

for (i in 1:nrow(sd_staff)) {
     x <- owner_durs %>% filter(Owner == sd_staff$Owner[i])
     x <- data.frame(x)
     x <- x %>% arrange(desc(Duration))
     # sd_data_list[[i]] <- x
     op_file_name <- paste(Week_Ending, "[",i,"]", sd_staff$Owner[i])
     gs_new(title = op_file_name, input = x)
     # drive_mv(op_file_name, path = "Shared drives/Data Analytics Projects/TSG - Weekly Cherwell Aging Reports/Weekly Stafff Reports/")
}

#
# Print out Summary and Results
#

cat("Total open tickets:", nrow(dat), "/ Incidents:", 
    nrow(dat %>% filter(Type == "Inc")), "/ SR's:",
         nrow(dat %>% filter(Type == "SR")))
cat("Incidents meeting SLA guidelines:", inc_sla_perc, "%")
cat("Service Requests meeting SLA guidelines:", SR_sla_perc, "%")

# Part 1 - ticket sorted by age within owner

owner_durs <- dat %>% arrange(Owner, Duration)

cat("===========================================================================")
cat("Part 1: Tickets Sorted by Owner Within Age")
cat("===========================================================================", "\n")

cat("Open Tickets by Owner")
owner_durs

# Part 2 - print out chronological groupings sorted by owner within grouping

cat("===========================================================================")
cat("Part 2: Tickets Sorted by Owner Within Age")
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
output_file <- gs_title("CAR-output-cherwell-stats")
gs_add_row(output_file, ws = 1, input = week_ending_data, verbose = TRUE)

# Write out individual staff files as Google Sheets
for(i in 1:no_staff_list) {
     staff_dat <- dat %>% filter(dat$Owner == staff_list$Owner[i])
     staff_ofn <- paste("CAR", staff_list$Owner[i], Week_Ending)
     staff_ofd <- gs_new(staff_ofn)
     gs_edit_cells(staff_ofd, ws = "Sheet1", anchor = "A1", input = staff_dat, byrow = TRUE)
}

#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------



