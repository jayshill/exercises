####################################################################
#   RTI CDS Analytics Exercise 02                                  #
#   NTSB database of aviation accidents                            #
#   Jay Hill                                                       #
#                                                                  #
####################################################################

library(dplyr)
library(magrittr)
library(ggplot2)

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
                              y = numPerClusterPerYear))
gg1 <- gg1 + geom_area(aes(colour = as.character(NarrativeCluster), 
                           fill = as.character(NarrativeCluster)))
gg1


##### Look at distributions of events within a year #####
monthlevels = c("January", "February", "March", "April", "May", "June", "July", 
                "August", "September", "October", "November", "December")
events$EventMonthName <- months(events$EventDate) %>% 
    factor(levels = monthlevels)



