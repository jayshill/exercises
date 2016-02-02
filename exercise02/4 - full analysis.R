####################################################################
#   RTI CDS Analytics Exercise 02                                  #
#   NTSB database of aviation accidents                            #
#   Jay Hill                                                       #
#                                                                  #
####################################################################

library(dplyr)
library(magrittr)
library(ggplot2)
library(scales)
library(lubridate)
library(reshape2)

# Add the text clusters to the 'events' data frame
events <- merge(x = events, y = textFinal, all.x = T)

##### Look at changes in narrative cluster prevalence over time. #####
# add month and year variables; sort by date
events <- arrange(events, EventDate) %>%
    mutate(EventMonth = cut(EventDate, breaks = "months"),
           EventYear = cut(EventDate, breaks = "years"))
# Only 1 observation each in 1979 and 1981, seven missing dates; remove these
events <- events[3:(nrow(events)-7), ]
eventsGrouped <- group_by(events, EventYear, NarrativeCluster)
# remove observations with missing narrative clusters (231 of 77,248)
eventsGrouped <- filter(eventsGrouped, !is.na(NarrativeCluster))
# summarize for each year and cluster type
(by_cluster <- summarise(eventsGrouped, numPerClusterPerYear = n()))
#
gg1 <- ggplot(by_cluster, aes(x = as.Date(as.character(EventYear)), 
                              y = numPerClusterPerYear)) + 
    geom_area(aes(colour = as.character(NarrativeCluster), 
                  fill = as.character(NarrativeCluster)))
gg1


##### Look at distributions of events within a year #####
monthlevels = c("January", "February", "March", "April", "May", "June", "July", 
                "August", "September", "October", "November", "December")
events$EventMonthName <- months(events$EventDate) %>% 
    factor(levels = monthlevels)
plot(events$EventMonthName)
    # there is an annual pattern to the number of events: highest in summer,
    # lowest in winter
plot(events$EventMonthName[as.Date(events$EventYear) >= as.Date("1982-01-01") &
                               as.Date(events$EventYear) <= as.Date("1989-01-01")])
plot(events$EventMonthName[as.Date(events$EventYear) >= as.Date("1990-01-01") &
                               as.Date(events$EventYear) <= as.Date("1999-01-01")])
plot(events$EventMonthName[as.Date(events$EventYear) >= as.Date("2000-01-01") &
                               as.Date(events$EventYear) <= as.Date("2009-01-01")])
    # no differences by decade
ggplot(events, aes(x = EventMonthName)) +
    geom_bar(aes(fill = as.character(NarrativeCluster)))
    # no differences by narrative cluster
ggplot(events, aes(x = EventMonthName)) + 
    geom_bar(aes(fill = as.character(InvestigationType)))
    # no trend in investigation vs. accident
ggplot(events, aes(x = EventMonthName)) +
    geom_bar(aes(fill = as.character(NarrativeCluster)))


## stacked bar chart with counts of aircraft suffering each level of damage
## in each month
gg2 <- ggplot(filter(events, AircraftDamage != "(missing)"), 
              aes(x = EventMonthName)) +
    geom_bar(aes(fill = as.character(AircraftDamage)))
gg2
    # pronounced increase in events during the summer months; destroyed 
    # aircraft are slightly more likely in the winter (need to add annotations)

## stacked bar chart with proportional axis showing fraction of aircraft
## suffering each level of damage in each month
eventsGrouped2 <- group_by(events, EventMonthName, AircraftDamage) %>%
    filter(AircraftDamage != "(missing)")
# summarize for each year and cluster type
(by_damage <- summarise(eventsGrouped2, numPerDamagePerYear = n()))
ggplot(by_damage, aes(x = EventMonthName, y = numPerDamagePerYear,
                             fill = as.character(AircraftDamage))) + 
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous(labels = percent_format())
    # destroyed aircraft are slightly more likely in the winter


##### Casualty counts over time #####
events <- mutate(events, EventYearOnly = year(EventYear))
eventsGrouped3 <- events[!duplicated(events$EventId), ] # to avoid double counts
eventsGrouped3 <- group_by(eventsGrouped3, EventYearOnly) %>%
    select(EventYearOnly, TotalFatalInjuries:TotalMinorInjuries) %>%
    summarise(FatalPerYear = sum(TotalFatalInjuries, na.rm=T),
              SeriousPerYear = sum(TotalSeriousInjuries, na.rm=T),
              MinorPerYear = sum(TotalMinorInjuries, na.rm=T)) %>%
    melt(id.vars = "EventYearOnly", variable.name = "Injuries",
         value.name = "NumberPerYear")
gg3 <- ggplot(eventsGrouped3, aes(x = EventYearOnly, y = NumberPerYear, 
                                  group = Injuries, color = Injuries)) +
    geom_line()
gg3
    # Fatal injuries are the most common, serious are least common

##### Relationships between aircraft damage and casualties #####
table(events$AircraftDamage, events$InvestigationType)
    # Most incidents had minor aircraft damage; most accidents had substantial
    # damage or destroyed aircraft
table(events$AircraftDamage, events$AmateurBuilt)   # nothing interesting
table(events$InvestigationType, events$AmateurBuilt)   # nothing interesting
table(events$AircraftDamage, events$WeatherCondition)
boxplot(FractionFatalInjuries ~ AircraftDamage, data = eventsGrouped)
hist(events$FractionFatalInjuries[events$AircraftDamage == "Destroyed"])
hist(events$FractionFatalInjuries[events$AircraftDamage == "Substantial"])
hist(events$FractionFatalInjuries[events$AircraftDamage == "Minor"])
ggplot(events, aes(x = FractionFatalInjuries)) +
    geom_histogram(aes(fill = as.character(AircraftDamage)))

levels(events$AircraftDamage)
events$AircraftDamage <- relevel(events$AircraftDamage, 2)
events$AircraftDamage <- relevel(events$AircraftDamage, 4)
events$AircraftDamage <- relevel(events$AircraftDamage, 4)
levels(events$AircraftDamage)

ggplot(filter(events, AircraftDamage != "(missing)"),
              aes(x = AircraftDamage, y = FractionFatalInjuries)) +
    geom_violin()
    # fatalities are more likely as aircraft damage intensifies
ggplot(filter(events, AircraftDamage != "(missing)"), 
       aes(x = AircraftDamage, y = FractionSeriousInjuries)) +
    geom_violin()
ggplot(filter(events, AircraftDamage != "(missing)"), 
       aes(x = AircraftDamage, y = FractionMinorInjuries)) +
    geom_violin()

gg4 <- ggplot(filter(events, AircraftDamage == "Destroyed", !is.na(NarrativeCluster)),
              aes(x = AircraftDamage, y = FractionFatalInjuries)) +
    geom_violin(aes(fill = as.character(NarrativeCluster)))
gg4
    # For destroyed aircraft, the fraction of fatalities among passengers
    # is highest for narrative cluster 1 and lowest for cluster 3.


##### create a panel plot #####
# create a list of plots
# Set up the page
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 2)))
# Make each plot, in the correct location
print(gg1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))    
print(gg2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))    
print(gg3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))    
print(gg4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))


##### Export figures #####
gg1
ggsave("roughFigs\\gg1.png")
gg2
ggsave("roughFigs\\gg2.png")
gg3
ggsave("roughFigs\\gg3.png")
gg4
ggsave("roughFigs\\gg4.png")
