library(ggplot2)
library(dplyr)
library(lubridate)
setwd("C:/Kemp Solutions/Dataquest/Articles/SharkWeek")
load("C:/Kemp Solutions/Dataquest/Articles/SharkWeek/sharks_v02.RData")
ds$yr <- year(ds$dt)
ds <- filter(ds, dt >= "1900-01-01")

ds$fatal <- ds$Fatal..Y.N.
ds[!ds$Fatal..Y.N. %in% c("Y", "N"), "fatal"] <- "Unknown"

a <- ds %>% group_by(yr, type2) %>% summarise(n = n())
ggplot(a, aes(x = factor(yr), y = n, fill = type2)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

a1 <- ds %>% filter(!species2 %in% c("Unknown", "Wobbegong")) %>% group_by(yr, species2, type2) %>% summarise(n = n())
ggplot(a1, aes(x = factor(yr), y = n, fill = type2)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~ species2, ncol = 1)

a2 <- ds %>% filter(!species2 %in% c("Unknown", "Wobbegong"),
                    activity2 %in% c("Fishing", "Other", "Surfing", "Swimming", "Spearfishing")) %>%
    group_by(yr, activity2, species2, type2) %>% summarise(n = n())
ggplot(a2, aes(x = factor(yr), y = n, fill = type2)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(activity2 ~ species2)

b <- ds %>% group_by(yr, activity2) %>% summarise(n = n())
bb <- ds %>% filter(activity2 != "Other") %>% group_by(yr, activity2) %>% summarise(n = n())
b2 <- ds %>% group_by(activity2, fatal) %>% summarise(n = n())
b2a <- ds %>% group_by(activity2) %>% summarise(n = n())
b2 <- merge(b2, b2a, by.x = "activity2", by.y = "activity2", all.x = T)
b2$pct <- b2$n.x/b2$n.y
ggplot(bb, aes(x = factor(yr), y = n, fill = activity2)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(bb, aes(x = factor(yr), y = n, group = activity2)) + geom_line(stat = "identity", aes(colour = activity2), size = 1.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(b2, aes(x = factor(activity2), y = pct, fill = fatal)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

c <- ds %>% group_by(yr, gender) %>% summarise(n = n())
ggplot(c, aes(x = factor(yr), y = n, fill = gender)) + geom_bar(stat = "identity")

d <- ds %>% group_by(yr, USA) %>% summarise(n = n())
ggplot(d, aes(x = factor(yr), y = n, fill = USA)) + geom_bar(stat = "identity")

e <- ds %>% filter(species2 != "Unknown") %>% group_by(yr, species2) %>% summarise(n = n())
ggplot(e, aes(x = factor(yr), y = n, fill = species2)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(e, aes(x = factor(yr), y = n, group = species2)) + geom_line(stat = "identity", aes(colour = species2), size = 1.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


