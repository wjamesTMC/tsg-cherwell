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
# Download and open survey file
#


# Import and Open the data file / Establish the data set
data_filename <- "0_Input_open-incidents.csv"
dat <- read.csv(data_filename, stringsAsFactors = FALSE)

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

cat("===========================================================================")
cat("Part 2 - Tickets Sorted Chronologically by Age")
cat("===========================================================================", "\n")

cat("Aged over 60 Days -", nrow(over_60), "tickets", round((nrow(over_60) / nrow(dat) * 100), digits = 3), "% of total")
over_60 <- over_60 %>% arrange(desc(Duration))
over_60
cat("\n")

cat("Aged 30 - 60 Days -", nrow(days_60), "tickets", round((nrow(days_60) / nrow(dat) * 100) , digits = 3), "% of total")
days_60 <- days_60 %>% arrange(desc(Duration))
days_60
cat("\n")

cat("Aged up to 30 Days -", nrow(days_30), "tickets", round((nrow(days_30) / nrow(dat) * 100), digits = 3), "% of total")
days_30 <- days_30 %>% arrange(desc(Duration))
days_30
cat("\n")

cat("Aged 1 Week or less -", nrow(days_07), "tickets", round((nrow(days_07) / nrow(dat) * 100), digits = 3), "% of total")
days_07 <- days_07 %>% arrange(desc(Duration))
days_07
cat("\n")


#--------------------------------------------------------------------
#
# Build graphics from summary dataframe
#
#--------------------------------------------------------------------

# Number of coments and responses by survey
num_c_and_r <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=num_resps, color = "Responses"), group=1, size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=num_comments, color = "Comments"), group=1, size=2) +
  scale_colour_manual("", 
                      breaks = c("Responses", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Count of Comments and Responses", subtitle = "Numbers of each by Survey") + ylab("Number") +
  theme(legend.position = c(0.18,0.85))

# Ratio of comments to responses
ratio_c_to_r <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=c_to_r_ratio, color = "Ratios", group=1), size=2) +
  scale_colour_manual("", breaks = c("Ratios"), values = c("mediumblue")) +
  labs(title = "Ratio of Comments to Responses", subtitle = "Ratio By Survey") + ylab("Proportion of Comments") +
  theme(legend.position = c(0.15,0.88))

# Arrange the two plots for pasting into deck
grid.arrange(num_c_and_r, ratio_c_to_r, ncol = 2)

# Positive words vs. comments
pw_vs_c <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=num_pos_words, color = "Positive Words", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=num_comments, color = "Comments", group=1), size=2) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_colour_manual("", 
                      breaks = c("Positive Words", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Positive Words vs. Comments", subtitle = "Number of each by Survey") + ylab("Number of Each") +
  theme(legend.position = c(0.2,0.85))

# Negative words vs. comments
nw_vs_c <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=num_neg_words, color = "Negative Words", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=num_comments, color = "Comments", group=1), size=2) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_colour_manual("", 
                      breaks = c("Negative Words", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Negative Words vs. Comments", subtitle = "Number of each by Survey") + ylab("Number of Each") +
  theme(legend.position = c(0.22,0.85))

# Neutral words vs. comments
neu_vs_c <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=num_neu_words, color = "Neutral Words", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=num_comments, color = "Comments", group=1), size=2) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_colour_manual("", 
                      breaks = c("Neutral Words", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Neutral Words vs. Comments", subtitle = "Number of each by Survey") + ylab("Number of Each") +
  theme(legend.position = c(0.22,0.85))

# Arrange the two plots for pasting into deck
grid.arrange(pw_vs_c, nw_vs_c, neu_vs_c, ncol = 3)

# Positive and negative words to comments ratios
p_vs_n <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=pw_to_c_ratio, color = "Positive", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=nw_to_c_ratio, color = "Negative", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=neu_to_c_ratio, color = "Neutral", group=1), size=2) +
  scale_colour_manual("", 
                      breaks = c("Positive Words", "Negative Words", "Neutral Words"),
                      values = c("indianred4", "gray40", "green4")) +
  labs(title = "Ratios of Positive Negative & Neutral Words to Comments", subtitle = "Ratio Comparisons by Survey") +
  ylab("# Words / # Comments") 

# Arrange the two plots for pasting into deck
grid.arrange(p_vs_n, ncol = 2)


#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------


