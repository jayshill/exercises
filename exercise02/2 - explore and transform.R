####################################################################
#   RTI CDS Analytics Exercise 02                                  #
#   NTSB database of aviation accidents                            #
#   Jay Hill                                                       #
#                                                                  #
#   This script performs some basic exploration of 'events' data   #
#   frame using simple tables and plots. Some new variables are    #
#   created.                                                       #
####################################################################

library(magrittr)

# Basic summaries of the events data
table(events$InvestigationType, useNA = "ifany")  
    # 74,207 accidents, 3050 incidents
table(events$Country, useNA = "ifany")
table(events$AircraftDamage, useNA = "ifany")
table(events$AircraftCategory, useNA = "ifany")  # 60,737 missing
table(events$AmateurBuilt, useNA = "ifany")   # 69,198 No
table(events$NumberOfEngines, useNA = "ifany")
table(events$EngineType, useNA = "ifany")
table(events$FARDescription, useNA = "ifany")   # 60,592 missing
table(events$Schedule, useNA = "ifany")   
    # 65,878 missing, 3814 non-scheduled, 3466 scheduled, 4099 unknown
table(events$PurposeOfFlight, useNA = "ifany")   
    # 43,360 personal. 9,272 instructional
table(events$AirCarrier, useNA = "ifany")   # messy
table(events$TotalFatalInjuries, useNA = "ifany")  
    # 40,363 with 0. 21,466 missing
table(events$TotalSeriousInjuries, useNA = "ifany")  
    # 42,955 with 0. 23,513 missing
table(events$TotalMinorInjuries, useNA = "ifany") 
    # 40,342 with 0. 22,524 missing
table(events$TotalUninjured, useNA = "ifany")  # not useful on its own
table(events$WeatherCondition, useNA = "ifany")
    # 68,764 visual met conditions. 5,580 instrument met conditions
table(events$BroadPhaseOfFlight, useNA = "ifany") 
    # 18,553 landing. 14,740 takeoff
table(events$ReportStatus, useNA = "ifany") # 72,264 probable cause

# Look at incidents only (no accidents)
incidents <- events[events$InvestigationType=="Incident", ]
table(incidents$TotalFatalInjuries, useNA = "ifany")
# 17 with 1 fatality, 1 with 2 fatalities, 1 with 3 fatalities
incidents[which(incidents$TotalFatalInjuries > 0), ]
# nothing obvious jumps out

# Create new variable for total passengers
passengers <- events[, c("TotalFatalInjuries", "TotalSeriousInjuries",
                         "TotalMinorInjuries", "TotalUninjured")]
events$TotalPassengers <- rowSums(passengers, na.rm = T)
# If all four categories are missing, mare TotalPassengers as missing.
# Otherwise assume TotalPassengers is the sum of the non-missing categories.
allmissing <- rowSums(is.na(passengers)) == 4
events$TotalPassengers <- events$TotalPassengers * ifelse(allmissing, NA, 1)
summary(events$TotalPassengers)
    # 662 events with no passenger information
    # median is 2; at least half of all incidents have two or fewer passengers
