##############################################################################
#
# Cherwell Weekly Aging Report - Charts and Graphs
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
library(googlesheets)

#--------------------------------------------------------------------
#
# Build graphics from historical data
#
#--------------------------------------------------------------------

# Open File
data_filename <- gs_title("CAR-output-cherwell-stats")
dat <- gs_read(data_filename, header = TRUE, stringsAsFactors = FALSE)

# Limit the number of weeks displayed to the most recent 3 months / quarter
dat <- tail(dat, 13)

# Force the dates to sort (and plot) in ascending order
dat$Week_Ending <- as.character(dat$Week_Ending)
dat$Week_Ending <- factor(dat$Week_Ending, levels=unique(dat$Week_Ending))

# Number of Opens by Week
Opens_by_Week <- ggplot() +
     geom_line(data = dat, aes(x = dat$Week_Ending, y = dat$Opens, color = "Opens", group = 1)) +
     theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     theme(legend.title = element_blank()) +
     labs(title = "Number of Opens", subtitle = "By Week") + xlab("Week") + ylab("Number")

# Number of incidents meeting SLAs
inc_mtg_slas <- ggplot() +
     geom_line(data = dat, aes(x = dat$Week_Ending, y = dat$Inc_Mtg_SLA, color = "SLA %", group = 1)) +
     theme_economist() +
     scale_y_continuous(limits=c(0, 20)) +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     theme(legend.title = element_blank()) +
     labs(title = "Percent of Incidents Meeting SLAs", subtitle = "By Week") + xlab("Week") + ylab("Number")

# Number of service requests meeting SLAs
sr_mtg_slas <- ggplot() +
     geom_line(data = dat, aes(x = dat$Week_Ending, y = dat$SR_Mtg_SLA, color = "SLA %", group = 1)) +
     theme_economist() +
     scale_y_continuous(limits=c(0, 20)) +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     theme(legend.title = element_blank()) +
     labs(title = "Percent of Service Requests Meeting SLAs", subtitle = "By Week") + xlab("Week") + ylab("Number")

# Arrange the plots
grid.arrange(inc_mtg_slas, sr_mtg_slas, ncol = 2)

# Number of 7 days
plot_days07 <- ggplot() +
     geom_line(data = dat, aes(x = dat$Week_Ending, y = dat$Days_07, color = "Days <= 7", group = 1)) +
     theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     theme(legend.title = element_blank()) +
     labs(title = "Tickets Open 7 Days or Less", subtitle = "By Week") + xlab("Week") + ylab("Number")

# Number of 30 days
plot_days30 <- ggplot() +
     geom_line(data = dat, aes(x = dat$Week_Ending, y = dat$Days_30, color = "Days <= 30", group = 1)) +
     theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     theme(legend.title = element_blank()) +
     labs(title = "Tickets Open 8 to 30 Days", subtitle = "By Week") + xlab("Week") + ylab("Number")

plot_days60 <- ggplot() +
     geom_line(data = dat, aes(x = dat$Week_Ending, y = dat$Days_60, color = "Days <= 60", group = 1)) +
     theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     theme(legend.title = element_blank()) +
     labs(title = "Tickets Open 31 to 60 Days", subtitle = "By Week") + xlab("Week") + ylab("Number")

plot_over60 <- ggplot() +
     geom_line(data = dat, aes(x = dat$Week_Ending, y = dat$Over_60, color = "Days over 60", group = 1)) +
     theme_economist() +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
     theme(legend.title = element_blank()) +
     labs(title = "Tickets Open Over 60 Days", subtitle = "By Week") + xlab("Week") + ylab("Number")

# Arrange the plots
grid.arrange(plot_days07, plot_days30, plot_days60, plot_over60, ncol = 2)

#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------