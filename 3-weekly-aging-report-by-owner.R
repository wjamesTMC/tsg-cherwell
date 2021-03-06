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
data_filename <- gs_title("CAR-input-2019-10-18")
dat <- gs_read(data_filename, skip = 1, header = TRUE, stringsAsFactors = FALSE)

# Generate the week ending data from the data file name
Week_Ending <- str_sub(data_filename[2], 11, 20)

# Clean up column / vector names
dat <- rename(dat, replace = c("Incident ID" = "ID",
                               "Created Date Time" = "Created",
                               "Customer Display Name" = "Customer",
                               "Incident Type" = "Type",
                               "Short Description" = "Desc",
                               "Owned By" = "Owner",
                               "SLA Resolve By Deadline" = "Due"))

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
dat <- dat %>% mutate(Duration = difftime(as.Date(Week_Ending), dat$Created, units = "days"))
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


pagebreak <- function() {
     if(knitr::is_latex_output())
          return("\\newpage")
     else
          return('<div style="page-break-before: always;" />')
}

# Print out tickets by owner
owner_list <- unique(dat$Owner)

for(i in 1:2) {
     cat("This is page", i, "\n", pagebreak())
}

# for(i in 1:length(owner_list)) {
for(i in 1:2) {     
     owner_file <- paste("CAR-", owner_list[i], "Tickets", Week_Ending)
     owner_file <- sub(" ", "", owner_file)
     sheet_data <- dat %>% filter(Owner == owner_list[i])
     gs_new(title = owner_file, input = sheet_data)
}


#
# Print out results
#

cat("Total open tickets:", nrow(dat), "/ Incidents:", 
    nrow(dat %>% filter(Type == "Inc")), "/ SR's:",
         nrow(dat %>% filter(Type == "SR")))
cat("Incidents meeting SLA guidelines:", inc_sla_perc, "%")
cat("Service Requests meeting SLA guidelines:", SR_sla_perc, "%")

# Part 1 - ticket sorted by age within owner

owner_durs <- dat %>% arrange(Owner, Duration) %>% select(-Desc)

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

#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------



