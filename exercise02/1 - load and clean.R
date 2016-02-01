####################################################################
#   RTI CDS Analytics Exercise 02                                  #
#   NTSB database of aviation accidents                            #
#   Jay Hill                                                       #
#                                                                  #
#   This script loads the xml and json files into R data frames.   #
#   Some preliminary exploration and cleaning is also performed.   #
####################################################################

library(xml2)
library(magrittr)
library(dplyr)
library(stringi)
library(tm)
library(jsonlite)

##### Read in the raw data files and process into data frames ##### 
# Read the AviationData XML file and explore its structure
doc <- read_xml("data\\AviationData.xml")
xml_name(doc)
xml_children(doc)
    # only one child node: ROWS

# Find the number of observations and variables
rows <- xml_children(xml_children(doc))
numrows <- length(rows)  # There are 77,257 observations
vars <- names(xml_attrs(rows[1])[[1]])
numvars <- length(vars)

# Extract the row elements into a data frame
events <- unlist(xml_attrs(rows)) %>% 
    matrix(nrow=numrows, byrow=T) %>%
    data.frame(stringsAsFactors = F)
names(events) <- vars

# Convert variables to correct type and check for problems
events$InvestigationType <- as.factor(events$InvestigationType)
events$EventDate <- as.Date(events$EventDate, "%m/%d/%Y")
summary(events$EventDate)
missing <- which(is.na(events$EventDate))
xml_attr(rows[missing], attr = "EventDate")
    # three missing values for EventDate
events$Latitude <- as.numeric(events$Latitude)
events$Longitude <- as.numeric(events$Longitude)
events$AircraftDamage <- as.factor(events$AircraftDamage)
events$AircraftCategory <- as.factor(events$AircraftCategory)
events$AmateurBuilt <- as.factor(events$AmateurBuilt)
events$NumberOfEngines <- as.numeric(events$NumberOfEngines)
events$EngineType <- as.factor(events$EngineType)
events$FARDescription <- as.factor(events$FARDescription)
events$Schedule <- as.factor(events$Schedule)
events$PurposeOfFlight <- as.factor(events$PurposeOfFlight)
events$TotalFatalInjuries <- as.numeric(events$TotalFatalInjuries)
events$TotalSeriousInjuries <- as.numeric(events$TotalSeriousInjuries)
events$TotalMinorInjuries <- as.numeric(events$TotalMinorInjuries)
events$TotalUninjured <- as.numeric(events$TotalUninjured)
events$WeatherCondition <- as.factor(events$WeatherCondition)
events$BroadPhaseOfFlight <- as.factor(events$BroadPhaseOfFlight)
events$ReportStatus <- as.factor(events$ReportStatus)
events$PublicationDate <- as.Date(events$PublicationDate, "%m/%d/%Y")

summary(events$PublicationDate)
missing <- which(is.na(events$PublicationDate))
xml_attr(rows[missing], attr = "PublicationDate")
    # 13188 missing values for PublicationDate

# Read the first JSON file into a new data frame.
jsonFile <- "data\\NarrativeData_000.json"
jsonData <- fromJSON(jsonFile)
names(jsonData)
names(jsonData[[1]])
narratives <- jsonData[[1]]
# Cycle through the remaining JSON files and append them to the data frame
for (i in seq(from = 499, to = 70999, by = 500)) {
    jsonFile <- paste0("data\\NarrativeData_", i, ".json")
    jsonData <- fromJSON(jsonFile)
    narratives <- rbind(narratives, jsonData[[1]])
}
jsonFile <- "data\\NarrativeData_999999.json"
jsonData <- fromJSON(jsonFile)
narratives <- rbind(narratives, jsonData[[1]])


##### Data cleaning #####
# Do the EventID values in narrative match up with EventID in events?
str(narratives)
    # There are 76,133 observations
length(intersect(events$EventId, narratives$EventId))
    # All of the EventID values in narratives match up to EventID in events
table(!is.element(events$EventId, narratives$EventId))
    # But all of the EventIDs in events match up to an EventID in narratives
length(unique(events$EventId))
    # There are only 76,133 unique EventID values, so some are repeated in events
duplicates <- duplicated(events$EventId) %>% which
duplicates2 <- duplicated(events$EventId, fromLast = T) %>% which
events[6:7,]
events[43:44,]
    # Repeated EventID values occur because >1 plane was involved in the event
narratives[narratives$EventId == events$EventId[6], ]
narratives[narratives$EventId == events$EventId[43], ]
    # The narrative descriptions confirm repeated EventID's are due to collisions

# check dates for inconsistencies
tail(events[c(3, 4, 31)], 20)
    # observations 77251:77254 have EventDate inconsistent with AccidentNumber and
    # PublicationDate -- change these EventDate values to missing.
events$EventDate[77251:77254] <- NA
which(events$EventDate > events$PublicationDate)
    # There are two cases where PublicationDate is before EventDate.
    # Mark these PublicationDate values to missing.
events$PublicationDate[c(38376, 40126)] <- NA

# check latitude and longitude
summary(events[7:8])
which(events$Longitude < -180)
    # one event with out of bounds longitude. Mark it missing.
events$Longitude[2803] <- NA

### Impute missing values where possible ###
select(events, InjurySeverity, TotalFatalInjuries:TotalUninjured) %>%
    filter(InjurySeverity == "Non-Fatal") %>% summary
# For non-fatal crashes, set missing Total and Fraction fatal to 0
events[events$InjurySeverity == "Non-Fatal", "TotalFatalInjuries"] <- 0
# For serious and minor injuries, assume NA = 0
events[is.na(events$TotalSeriousInjuries), "TotalSeriousInjuries"] <- 0
events[is.na(events$TotalMinorInjuries), "TotalMinorInjuries"] <- 0
    # For total uninjured NA's, we can't really impute anything

## Use same Model to fill in missing NumberOfEngines
events$Model <- toupper(events$Model)
# create a list of unique models and their number of engines
ModelAndEngines <-select(events, Model, NumberOfEngines) %>% 
    filter(!is.na(Model)) %>% filter(!is.na(NumberOfEngines))
ModelAndEngines$Model <- removePunctuation(ModelAndEngines$Model) %>%
    stripWhitespace %>%
    stri_trim_both
ModelAndEngines <- unique(ModelAndEngines) %>%
    arrange(Model, desc(NumberOfEngines))
ModelAndEngines <- ModelAndEngines[-which(duplicated(ModelAndEngines$Model)), ]
ModelAndEngines <- ModelAndEngines[-1, ]  # first row has " " model
# find the row numbers for the observations with missing number of engines
missingEngines <- which(is.na(events$NumberOfEngines))
# fill in the missing values if possible
for (i in missingEngines) {
    tempModel <- events[i, "Model"]
    tempModel <- removePunctuation(tempModel) %>% stripWhitespace %>%
        stri_trim_both
    tempNumEng <- ModelAndEngines[ModelAndEngines$Model == tempModel, 2]
    if (tempModel %in% ModelAndEngines$Model) {
        events$NumberOfEngines[i] <- tempNumEng
    }
}
summary(events$NumberOfEngines)
# For Number of Engines, 1st quartile, median, and 3rd quartile all equal 1.
# Impute remaining NumberOfEngines NA's as 1 as that is most likely.
events[is.na(events$NumberOfEngines), "NumberOfEngines"] <- 1

# Some factor variables have empty string for a label; replace as "(missing)"
levels(events$AircraftDamage)[1] <- "(missing)"
levels(events$AircraftCategory)[1] <- "(missing)"
levels(events$AmateurBuilt)[1] <- "(missing)"
levels(events$EngineType)[1] <- "(missing)"
levels(events$FARDescription)[1] <- "(missing)"
levels(events$Schedule)[1] <- "(missing)"
levels(events$PurposeOfFlight)[1] <- "(missing)"
levels(events$WeatherCondition)[1] <- "(missing)"
levels(events$BroadPhaseOfFlight)[1] <- "(missing)"

# Other character variables have "" values; change to "(missing)"
events[events$Location == "", "Location"] <- "(missing)"
events[events$Country == "", "Country"] <- "(missing)"
events[events$AirportCode == "", "AirportCode"] <- "(missing)"
events[events$AirportName == "", "AirportName"] <- "(missing)"
events[events$InjurySeverity == "", "InjurySeverity"] <- "(missing)"
events[events$RegistrationNumber == "", "RegistrationNumber"] <- "(missing)"
events[events$Make == "", "Make"] <- "(missing)"
events[events$Model == "", "Model"] <- "(missing)"
events[events$AirCarrier == "", "AirCarrier"] <- "(missing)"
