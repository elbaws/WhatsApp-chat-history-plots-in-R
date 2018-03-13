rm(list = ls())

library(ggplot2)
library(stringr)
library(extrafont)
library(tokenizers)
library(lubridate)
library(scales) #Add option squish to plots, to cut of values at a certain point.
library(ggrepel) #geom_text_repel, additional text placing options in plots
library(ngram) #To determine ngrams
library(circlize) #For circular reply probability plot

setwd("/Users/jeroencontent/stack/Data visualisatie/Chat historie/Opas")
chathistory <- read.csv("WhatsApp_Chat_with_Opas_N.csv")
colnames(chathistory)[1] <- "raw"
chathistory$raw <- as.character(chathistory$raw)

#https://github.com/stopwords-iso, used to filter out stopwords
stopwords <- readLines(file("../stopwords-nl.txt", open = "r"))

#Colors: Set3 of colorbrewer2.org
set3 <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#979797", "#bc80bd", "#ffed6f") #12: #ccebc5
set3.drk <- c("#62938b", "#b2b27d", "#858298", "#af5950", "#597c93", "#b17e44", "#7d9b49", "#b08fa0", "#979797", "#835984", "#b2a54d") #12: #8ea48a

persons <- c("Adam", "Bram", "Casper", "Daan", "Fabian", "Julian", "Koen", "Lucas", "Niels", "Sven", "Younes")

#Create a DF with all day from start to end of sample period. Used to fill up inactive days.
dates <- as.Date("12-03-07", "%y-%m-%d")
for (i in 1:2176){
  dates <- c(dates, tail(dates, n = 1) + 1)
}
rm(i)
dates <- rbind(data.frame(date = dates, person = persons[1]), data.frame(date = dates, person = persons[2]), data.frame(date = dates, person = persons[3]), 
               data.frame(date = dates, person = persons[4]), data.frame(date = dates, person = persons[5]), data.frame(date = dates, person = persons[6]),
               data.frame(date = dates, person = persons[7]), data.frame(date = dates, person = persons[8]), data.frame(date = dates, person = persons[9]),
               data.frame(date = dates, person = persons[10]), data.frame(date = dates, person = persons[11]))

######################################################
################Setting up the data###################
######################################################

#Break-up into columns
chathistory.2$date <- as.Date(gsub("\\s.*$", "", chathistory.2$raw, perl = T), "%d-%m-%y")
chathistory.2$time <- gsub("^\\S+.?([^:]*:[^:]*):.*$", "\\1", chathistory.2$raw, perl = T)
chathistory.2$person <- gsub("^\\S+\\s+\\S+\\s+([^:]*):.*$", "\\1", chathistory.2$raw, perl = T)
chathistory.2$message <- gsub("^.*?:.*?:.*?:.*?:", "", chathistory.2$raw, perl = T)

#Clean
chathistory <- subset(chathistory, !is.na(date))
chathistory <- subset(chathistory, nchar(time) == 5)
chathistory <- subset(chathistory, nchar(person) <= 21)

#Consolidate persons, some have had multiple numbers during the period. Fill-in correct names here:
levels(chathistory$person)
chathistory$person <- as.factor(chathistory$person)
levels(chathistory$person)[levels(chathistory$person) == "Adam Surname"] <- persons[1]
levels(chathistory$person)[levels(chathistory$person) == "Bram Surname"] <- persons[2]
levels(chathistory$person)[levels(chathistory$person) == "Casper Surname"] <- persons[3]
levels(chathistory$person)[levels(chathistory$person) == "Daan Surname"] <- persons[4]
levels(chathistory$person)[levels(chathistory$person) == "Fabian Surname"] <- persons[5]
levels(chathistory$person)[levels(chathistory$person) == "Julian Surname"] <- persons[6]
levels(chathistory$person)[levels(chathistory$person) == "Koen Surname"] <- persons[7]
levels(chathistory$person)[levels(chathistory$person) == "Lucas Surname"] <- persons[8]
levels(chathistory$person)[levels(chathistory$person) == "Niels Surname"] <- persons[9]
levels(chathistory$person)[levels(chathistory$person) == "Sven Surname nr.1"] <- persons[10]
levels(chathistory$person)[levels(chathistory$person) == "Sven Surname nr.2"] <- persons[10]
levels(chathistory$person)[levels(chathistory$person) == "Younes Surname nr.1"] <- persons[11]
levels(chathistory$person)[levels(chathistory$person) == "Younes Surname nr.2"] <- persons[11]
levels(chathistory$person)[levels(chathistory$person) == "Younes Surname nr.3"] <- persons[11]

chathistory <- subset(chathistory, chathistory$person %in% persons)
chathistory$person <- factor(chathistory$person)

#Break-up further
chathistory$year <- as.factor(format(chathistory$date ,"%Y"))
chathistory$month <- as.factor(format(chathistory$date ,"%Y-%m"))
chathistory$hour <- as.factor(as.numeric(gsub(":.*$", "", chathistory$time)))

#Media messages
chathistory$media <- 0
chathistory$media[grepl("<.*>", chathistory$message)] <- 1
chathistory$media <- chathistory$media

#Text messages
chathistory$messages <- 0
chathistory$messages[chathistory$media == 0] <- 1

#Construct the dataset
cths <- data.frame(person = chathistory$person, date = chathistory$date,
                   year = chathistory$year, month = chathistory$month, hour = chathistory$hour,
                   nchar = nchar(chathistory$message), words = str_count(chathistory$message, '\\w+'), media = chathistory$media,
                   messages = chathistory$messages)

#Aggregate to hours
cths.hour <- data.frame(person = cths$person, hour = cths$hour, messages = cths$messages, words = cths$words)
cths.hour <- aggregate(. ~ person + hour, cths.hour, sum)
cths.hour$wordspm <- cths.hour$words/cths.hour$messages

#Aggregate to hours total (not split for persons)
cths.hour.t <- aggregate(. ~ hour, cths.hour, sum)
cths.hour.t$person <- NULL

#Aggregate to days
cths.day <- data.frame(person = cths$person, day = cths$date, messages = cths$messages, words = cths$words, media = cths$media)
cths.day <- aggregate(. ~ person + day, cths.day, sum)
cths.day$wordspm <- cths.day$words/cths.day$messages

#Insert missing days
cths.day <- merge(cths.day, dates, by.x = c("day", "person"), by.y = c("date", "person"), all.y = T)
cths.day[is.na(cths.day)] <- 0
rm(dates)

#Aggregate to days total (not split for persons)
cths.day.t <- aggregate(. ~ day, cths.day, sum)
cths.day.t$person <- NULL

#Include percentages per person for days DF
cths.day <- merge(cths.day, cths.day.t, by = "day")
cths.day$wordspm.y <- NULL
cths.day$media.y <- NULL
names(cths.day)[names(cths.day)=="messages.x"] <- "messages"
names(cths.day)[names(cths.day)=="words.x"] <- "words"
names(cths.day)[names(cths.day)=="wordspm.x"] <- "wordspm"
names(cths.day)[names(cths.day)=="messages.y"] <- "messages.t"
names(cths.day)[names(cths.day)=="words.y"] <- "words.t"
names(cths.day)[names(cths.day)=="media.x"] <- "media"

cths.day$messages.pc <- cths.day$messages/cths.day$messages.t
cths.day$words.pc <- cths.day$words/cths.day$words.t

cths.day$messages.t <- NULL
cths.day$words.t <- NULL

#Aggregate to weeks (Monday start of the week)
cths.week <- data.frame(person = cths.day$person, date = cths.day$day, messages = cths.day$messages, words = cths.day$words, media = cths.day$media)
cths.week$week <- strftime(cths.week$date, "%Y - %W")
cths.week <- aggregate(. ~ person + week, cths.week, sum)
cths.week$date <- NULL
cths.week$wordspm <- cths.week$words/cths.week$messages
cths.week[is.na(cths.week)] <- 0
cths.week$week.f <- as.factor(cths.week$week)
cths.week$week.f <- factor(cths.week$week.f, levels = rev(levels(cths.week$week.f)))

#Aggregate to months
cths.month <- data.frame(person = cths.day$person, month = as.factor(format(cths.day$day ,"%Y-%m")), messages = cths.day$messages, words = cths.day$words, media = cths.day$media)
cths.month <- aggregate(. ~ person + month, cths.month, sum)
cths.month$wordspm <- cths.month$words/cths.month$messages
cths.month$wordspm[cths.month$wordspm == "Inf"] <- 0
cths.month[is.na(cths.month)] <- 0

#Aggregate to months total (not split for persons)
cths.month.t <- aggregate(. ~ month, cths.month, sum)
cths.month.t$person <- NULL

#Include percentages per person for months DF
cths.month <- merge(cths.month, cths.month.t, by = "month")
cths.month$wordspm.y <- NULL
names(cths.month)[names(cths.month)=="messages.x"] <- "messages"
names(cths.month)[names(cths.month)=="words.x"] <- "words"
names(cths.month)[names(cths.month)=="media.x"] <- "media"
names(cths.month)[names(cths.month)=="wordspm.x"] <- "wordspm"
names(cths.month)[names(cths.month)=="messages.y"] <- "messages.t"
names(cths.month)[names(cths.month)=="words.y"] <- "words.t"
names(cths.month)[names(cths.month)=="media.y"] <- "media.t"

cths.month$messages.pc <- cths.month$messages/cths.month$messages.t
cths.month$words.pc <- cths.month$words/cths.month$words.t
cths.month$media.pc <- cths.month$media/cths.month$media.t

#cths.month$messages.t <- NULL
cths.month$words.t <- NULL
cths.month$media.t <- NULL

#Aggregate to years
cths.year <- data.frame(person = cths.day$person, year = as.factor(format(cths.day$day ,"%Y")), messages = cths.day$messages, words = cths.day$words, media = cths.day$media)
cths.year <- aggregate(. ~ person + year, cths.year, sum)
cths.year$wordspm <- cths.year$words/cths.year$messages
cths.year$wordspm[cths.year$wordspm == "Inf"] <- 0
cths.year[is.na(cths.year)] <- 0

#Aggregate to years total (not split for persons)
cths.year.t <- aggregate(. ~ year, cths.year, sum)
cths.year.t$person <- NULL

#Include percentages per person for years DF
cths.year <- merge(cths.year, cths.year.t, by = "year")
cths.year$wordspm.y <- NULL
names(cths.year)[names(cths.year)=="messages.x"] <- "messages"
names(cths.year)[names(cths.year)=="words.x"] <- "words"
names(cths.year)[names(cths.year)=="media.x"] <- "media"
names(cths.year)[names(cths.year)=="wordspm.x"] <- "wordspm"
names(cths.year)[names(cths.year)=="messages.y"] <- "messages.t"
names(cths.year)[names(cths.year)=="words.y"] <- "words.t"
names(cths.year)[names(cths.year)=="media.y"] <- "media.t"

cths.year$messages.pc <- cths.year$messages/cths.year$messages.t
cths.year$words.pc <- cths.year$words/cths.year$words.t
cths.year$media.pc <- cths.year$media/cths.year$media.t

cths.year$messages.t <- NULL
cths.year$words.t <- NULL
cths.year$media.t <- NULL


######################################################
##################Let the fun start###################
######################################################

#################
#Pie's

#One pie for all persons, 11 timelines behind each other, logarithm of number of messages.
cths.week.long <- rbind(subset(cths.week, person == persons[1]), subset(cths.week, person == persons[2]),  subset(cths.week, person == persons[3]), 
                        subset(cths.week, person == persons[4]),  subset(cths.week, person == persons[5]),  subset(cths.week, person == persons[6]), 
                        subset(cths.week, person == persons[7]),  subset(cths.week, person == persons[8]),  subset(cths.week, person == persons[9]), 
                        subset(cths.week, person == persons[10]),  subset(cths.week, person == persons[11]))
cths.week.long$week.f.j <- as.factor(paste0(as.character(cths.week.long$person), " - ", cths.week.long$week.f))

cths.week.long$messages.ln <- log(cths.week.long$messages)
cths.week.long$messages.ln[cths.week.long$messages.ln == 0] <- 0.0001
cths.week.long$messages.ln[cths.week.long$messages.ln == "-Inf"] <- 0

p0 <- ggplot(cths.week.long, aes(x = week.f.j, y = messages.ln)) +
  geom_bar(aes(color = person), stat = "identity") +
  geom_smooth(aes(group = person), method = "loess", span = .75, linetype = "dashed", size = .5, alpha = 1/2, se = T, color = "white") +
  theme(text = element_text(family = "Helvetica-Light", size = 7),
        axis.title = element_blank(), panel.grid = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  #scale_x_discrete(breaks = levels(cths.week.long$week.f.j)[seq(158, 3329, 317)]) +
  scale_x_discrete(breaks = "none") +
  scale_colour_manual(values = set3) +
  coord_polar()

p0
#ggsave("p0.eps", p0, bg = "transparent", width = 20, height = 20, units = "cm", fonts = "Helvetica-Light")
#ggsave("p0.png", p0, bg = "transparent", width = 10, height = 10, units = "cm")

#Seperate pie per year, share in total messages
p1a <- ggplot(cths.year, aes(x = factor(1), y = messages.pc, fill = person)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  facet_wrap(~year, nrow = 1) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), text = element_text(family = "Helvetica-Light"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_brewer(palette = "Set3")

cths.year$pos <- do.call(c, tapply(cths.year$messages.pc, cths.year$year, FUN = cumsum))
cths.year$pos = cths.year$pos - cths.year$messages.pc/2
p1b <- ggplot(subset(cths.year, messages.pc != 0), aes(x = factor(1), y = messages.pc, fill = person)) +
  geom_bar(width = 1, stat = "identity", position = position_stack(reverse = TRUE)) +
  facet_wrap(~year, nrow = 1) +
  coord_polar(theta = "y", start = 0) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), text = element_text(family = "Helvetica-Light"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_brewer(palette = "Set3") +
  geom_text_repel(aes(x = 1.5, y = pos, label = round(messages.pc*100, digits = 1)), size = 2.5, nudge_x = .4, segment.size = .3, min.segment.length = .2)

#Seperate pie per year, share in total words
p2a <- ggplot(cths.year, aes(x = factor(1), y = words.pc, fill = person)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  facet_wrap(~year, nrow = 1) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), text = element_text(family = "Helvetica-Light"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_brewer(palette = "Set3")

cths.year$pos <- do.call(c, tapply(cths.year$words.pc, cths.year$year, FUN = cumsum))
cths.year$pos = cths.year$pos - cths.year$words.pc/2
p2b <- ggplot(subset(cths.year, words.pc != 0), aes(x = factor(1), y = words.pc, fill = person)) +
  geom_bar(width = 1, stat = "identity", position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y", start = 0) +
  facet_wrap(~year, nrow = 1) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), text = element_text(family = "Helvetica-Light"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_brewer(palette = "Set3") +
  geom_text_repel(aes(x = 1.5, y = pos, label = round(words.pc*100, digits = 1)), size = 2.5, nudge_x = .4, segment.size = .3, min.segment.length = .2)

#Seperate pie per year, share in total media messages
p3a <- ggplot(cths.year, aes(x = factor(1), y = media.pc, fill = person)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  facet_wrap(~year, nrow = 1) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), text = element_text(family = "Helvetica-Light"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_brewer(palette = "Set3")

p1a
p1b
p2a
p2b
p3a

# ggsave("p1a.eps", p1a, bg = "transparent", width = 16, height = 4, units = "cm", fonts = "Helvetica-Light")
# ggsave("p2a.eps", p2a, bg = "transparent", width = 16, height = 4, units = "cm", fonts = "Helvetica-Light")
# ggsave("p3a.eps", p3a, bg = "transparent", width = 16, height = 4, units = "cm", fonts = "Helvetica-Light")


#################
#Lines plots

#Number of messages
p3a <- ggplot(cths.month, aes(x = as.Date(paste0(cths.month$month, "-1"), "%Y-%m-%d"), y = messages)) +
  geom_smooth(aes(group = person, color = person), method = "loess", span = .5, linetype = "solid", size = .5, se = F) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 months", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light"),
        axis.title = element_blank(), legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_colour_manual(values = set3)

#Number of words
p3b <- ggplot(cths.month, aes(x = as.Date(paste0(cths.month$month, "-1"), "%Y-%m-%d"), y = words)) +
  geom_smooth(aes(group = person, color = person), method = "loess", span = .5, linetype = "solid", size = .5, se = F) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 months", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light"),
        axis.title = element_blank(), legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_colour_manual(values = set3)

#Number of words per message
p3c <- ggplot(cths.month, aes(x = as.Date(paste0(cths.month$month, "-1"), "%Y-%m-%d"), y = wordspm)) +
  geom_smooth(aes(group = person, color = person), method = "loess", span = .5, linetype = "solid", size = .5, se = F) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 months", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light"),
        axis.title = element_blank(), legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_colour_manual(values = set3)

#Number of media messages
p3d <- ggplot(cths.month, aes(x = as.Date(paste0(cths.month$month, "-1"), "%Y-%m-%d"), y = media)) +
  geom_smooth(aes(group = person, color = person), method = "loess", span = .5, linetype = "solid", size = .5, se = F) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 months", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light"),
        axis.title = element_blank(), legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_colour_manual(values = set3)

p3a
p3b
p3c
p3d

# ggsave("p3a.eps", p3a, bg = "transparent", width = 16, height = 4, units = "cm", fonts = "Helvetica-Light")
# ggsave("p3b.eps", p3b, bg = "transparent", width = 16, height = 4, units = "cm", fonts = "Helvetica-Light")
# ggsave("p3c.eps", p3c, bg = "transparent", width = 16, height = 4, units = "cm", fonts = "Helvetica-Light")
# ggsave("p3d.eps", p3d, bg = "transparent", width = 16, height = 4, units = "cm", fonts = "Helvetica-Light")


#################
#Bar activty plots

#Seperate vertical bar plot per person for number of messages (including media)
for (i in 1:length(persons)) {
  p4.x <- ggplot(subset(cths.week, person == persons[i]), aes(x = week.f)) +
    geom_bar(aes(y = messages + media), stat = "identity", fill = set3[i]) +
    theme(text = element_text(family = "Helvetica-Light", size = 8),
          axis.title = element_blank(), panel.grid = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(color = "#fdfdf5"),
          plot.background = element_rect(fill = "transparent", colour = NA)) +
    scale_x_discrete(breaks = levels(cths.week$week.f)[seq(1, 316, 20)], 
                     labels = strftime(as.Date(paste0(levels(cths.week$week.f)[seq(1, 316, 20)], " - 1"), "%Y - %W - %u"), "%b - %Y")) +
    scale_y_continuous(limits = c(0, 150), breaks = c(0, 25, 75, 150), oob = squish) +
    coord_flip()
  assign(paste0("p4.", i) , p4.x)
}
rm(p4.x)

p4.1
p4.2
p4.3
p4.4
p4.5
p4.6
p4.7
p4.8
p4.9
p4.10
p4.11

# ggsave("p4-1.eps", p4.1, bg = "transparent", width = 2.4, height = 8, units = "cm", fonts = "Helvetica-Light")
# ggsave("p4-2.eps", p4.2, bg = "transparent", width = 2.4, height = 8, units = "cm", fonts = "Helvetica-Light")
# ggsave("p4-3.eps", p4.3, bg = "transparent", width = 2.4, height = 8, units = "cm", fonts = "Helvetica-Light")
# ggsave("p4-4.eps", p4.4, bg = "transparent", width = 2.4, height = 8, units = "cm", fonts = "Helvetica-Light")
# ggsave("p4-5.eps", p4.5, bg = "transparent", width = 2.4, height = 8, units = "cm", fonts = "Helvetica-Light")
# ggsave("p4-6.eps", p4.6, bg = "transparent", width = 2.4, height = 8, units = "cm", fonts = "Helvetica-Light")
# ggsave("p4-7.eps", p4.7, bg = "transparent", width = 2.4, height = 8, units = "cm", fonts = "Helvetica-Light")
# ggsave("p4-8.eps", p4.8, bg = "transparent", width = 2.4, height = 8, units = "cm", fonts = "Helvetica-Light")
# ggsave("p4-9.eps", p4.9, bg = "transparent", width = 2.4, height = 8, units = "cm", fonts = "Helvetica-Light")
# ggsave("p4-10.eps", p4.10, bg = "transparent", width = 2.4, height = 8, units = "cm", fonts = "Helvetica-Light")
# ggsave("p4-11.eps", p4.11, bg = "transparent", width = 2.4, height = 8, units = "cm", fonts = "Helvetica-Light")


#################
#Frequency plots

#Activity over whole period
p6a <- ggplot() +
  #geom_smooth(data = cths.week, aes(y = (messages + media)*20, x = as.Date(paste0(cths.week$week, " - 1"), "%Y - %U - %u")), method = "loess", span = .2, linetype = "dashed", size = .5, alpha = 1/4) +
  geom_bar(data = cths.week, aes(y = messages + media, x = as.Date(paste0(cths.week$week, " - 1"), "%Y - %U - %u"), fill = person), stat = "identity", position = "stack") +
  theme(legend.position = "none") +
  scale_fill_manual(values = set3) +
  scale_x_date(date_breaks = "7 months", date_minor_breaks = "3 months", date_labels = "%Y - %b") +
  theme(text = element_text(family = "Helvetica-Light"),
        #axis.text.x = element_text(angle = 45),
        axis.title = element_blank(), legend.position = "none",
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),  panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA))
p6a

#ggsave("p6a.eps", p6a, bg = "transparent", width = 35, height = 8, units = "cm", fonts = "Helvetica-Light")

#Activity over whole period, logarithm of messages
cths.day.t$messages.ln <- log(cths.day.t$messages)
cths.day.t$messages.ln[cths.day.t$messages.ln == 0] <- 0.0001
cths.day.t$messages.ln[cths.day.t$messages.ln == "-Inf"] <- 0

p6b <- ggplot(cths.day.t, aes(x = day, y = messages.ln)) +
  geom_bar(stat = "identity", aes(color = messages.ln), width = .9) +
  geom_smooth(method = "loess", span = .5, linetype = "dotted", size = .5, alpha = 3/4, se = F) +
  scale_x_date(date_breaks = "12 months", date_labels = "%Y") +
  theme(text = element_text(family = "Helvetica-Light"),
        axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "none",
        panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  coord_polar() +
  scale_colour_gradient(high = "#c8c8c8", low = "#6e8e3c")
p6b

# ggsave("p6b.png", p6b, bg = "transparent", width = 10, height = 10, units = "cm")


#Day of the week
cths.wkday <- cths.day
cths.wkday$wkday <- as.factor(strftime(cths.wkday$day, "%u"))
cths.wkday <- aggregate(. ~ person + wkday, cths.wkday, sum)
cths.wkday$wordspm <- cths.wkday$words/cths.wkday$messages

p7a <- ggplot(cths.wkday, aes(x = wkday, y = messages, group = person, fill = person)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thurstday", "5" = "Friday", "6" = "Saturday", "7" = "Sunday")) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light", size = 16),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_manual(values = set3)

p7b <- ggplot(cths.wkday, aes(x = wkday, y = messages, group = person, fill = person)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = c("1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thurstday", "5" = "Friday", "6" = "Saturday", "7" = "Sunday")) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light", size = 16),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_manual(values = set3)

p7a
p7b

# ggsave("p7a.eps", p7a, bg = "transparent", width = 20, height = 16, units = "cm", fonts = "Helvetica-Light")
# ggsave("p7b.eps", p7b, bg = "transparent", width = 20, height = 16, units = "cm", fonts = "Helvetica-Light")

#Hour of the day
cths.hour$hour <- factor(cths.hour$hour, levels = c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
p8a <- ggplot(cths.hour, aes(x = hour, y = messages, group = person, fill = person)) +
  geom_bar(stat = "identity") +
  scale_x_discrete() +
  theme(text = element_text(family = "Helvetica-Light", size = 16),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_brewer(palette = "Set3")
p8a
#ggsave("p8a.eps", p8a, bg = "transparent", width = 20, height = 16, units = "cm", fonts = "Helvetica-Light")

p8b <- ggplot(cths.hour, aes(x = hour, y = messages, group = person, fill = person)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete() +
  theme(text = element_text(family = "Helvetica-Light", size = 16),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_brewer(palette = "Set3")
p8b

###################################################
#############Per hour over a week##################
###################################################
chts.wkhr <- data.frame(person = chathistory$person, wkday = strftime(chathistory$date, "%u"), hour = substr(chathistory$time, 1, 2), messages = 1)
chts.wkhr <- aggregate(. ~ person + wkday + hour, chts.wkhr, sum)
chts.wkhr$datetime <- as.POSIXlt(paste0("2018-01-0", chts.wkhr$wkday, " ", chts.wkhr$hour, ":00"))

p8c <- ggplot(chts.wkhr, aes(x = datetime, y = messages, group = person, fill = person)) +
  geom_bar(stat = "identity") +
  scale_x_datetime(date_labels = "%a %Hh", date_breaks = "9 hours") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light", size = 16),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_brewer(palette = "Set3")
p8c
#ggsave("p8c.eps", p8c, bg = "transparent", width = 20, height = 16, units = "cm", fonts = "Helvetica-Light")

chts.wkhr.weekend <- subset(chts.wkhr, wkday == 5 & as.numeric(hour) >= 12 | wkday == 6 | wkday == 7)
p8d <- ggplot(chts.wkhr.weekend, aes(x = datetime, y = messages, group = person, fill = person)) +
  geom_bar(stat = "identity") +
  scale_x_datetime(date_labels = "%a %Hh", date_breaks = "4 hours") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light", size = 16),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_brewer(palette = "Set3")
p8d
#ggsave("p8d.eps", p8d, bg = "transparent", width = 20, height = 16, units = "cm", fonts = "Helvetica-Light")


#################
#Most words used

words <- tokenize_words(chathistory$message)
wordsa <- words[[1]]
for (i in 2:length(words)){
  wordsa <- c(wordsa, words[[i]])  
}
words <- data.frame(word = wordsa, frq = 1)
words <- aggregate(. ~ word, words, sum)
words <- subset(words, !(word %in% stopwords))

#words to exlcude
words <- subset(words, word !=  "image" & word != "omitted" & word != "2" & word != "1" & word != "2013" & word != "video" & word != "https")
words <- subset(words, !(word %in% tolower(persons)))
words <- words[order(-words$frq),]

p9 <- ggplot(head(words, n = 20), aes(y = frq, x = reorder(word, frq))) +
  geom_bar(stat = "identity", width = .8, aes(fill = frq)) +
  coord_flip() +
  theme(axis.title = element_blank(),
        text = element_text(family = "Helvetica-Light"),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text = element_blank()) +
  scale_fill_gradient2(high = "#91b05e", mid = "#91b05e", low = "#c8c8c8", midpoint = 800) +
  geom_text(aes(label = frq), size = 3.75, hjust = 1.25)
p9

#ggsave("p9b.eps", p9b, bg = "transparent", width = 20, height = 20, units = "cm", fonts = "Helvetica-Light")


#################
#Most used word pairs

wordpairs <- subset(chathistory, str_count(message, " ") >= 2)
wordpairs$message <- tolower(wordpairs$message)
messages <- wordpairs$message[1]
for (i in 2:nrow(wordpairs)) {
  messages <- c(messages, wordpairs$message[i])
}
wordpairs.ngrams <- ngram(messages, n = 2, sep = ", ")
wordpairs.ngrams <- get.phrasetable(wordpairs.ngrams)
rm(messages, wordpairs)
wordpairs.ngrams$ngrams <- trimws(wordpairs.ngrams$ngrams, which = "both")

#Word pairs to exlude
ngrams.exl <- c("word pair1", "word pair2", "word pair3")
wordpairs.ngrams <- subset(wordpairs.ngrams, !(ngrams %in% ngrams.exl))
wordpairs.ngrams <- subset(wordpairs.ngrams, substr(ngrams, 1, 1) != "<")
head(wordpairs.ngrams, n = 30)

p9c <- ggplot(head(wordpairs.ngrams, n = 20), aes(y = freq, x = reorder(ngrams, freq))) +
  geom_bar(stat = "identity", width = .8, aes(fill = freq)) +
  coord_flip() +
  theme(axis.title = element_blank(),
        text = element_text(family = "Helvetica-Light"),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text = element_blank()) +
  scale_fill_gradient2(high = "#91b05e", mid = "#91b05e", low = "#c8c8c8", midpoint = 275) +
  geom_text(aes(label = freq), size = 3.75, hjust = 1.25)
p9c

#ggsave("p9c.eps", p9c, bg = "transparent", width = 20, height = 20, units = "cm", fonts = "Helvetica-Light")


#################
#Probabilities of replying per person
cths.pr <- data.frame(person = cths$person)
cths.pr$person <- as.character(cths.pr$person)

cths.pr$reactie <- "none"
for (i in 1:nrow(cths.pr)){
  cths.pr$reactie[i] <- cths.pr$person[i+1]
}

cths.pr$reacties <- 1
cths.pr <- aggregate(. ~ person + reactie, cths.pr, sum)
cths.pr <- subset(cths.pr, person != reactie)
cths.pr <- merge(cths.pr, aggregate(. ~ person, data.frame(person = cths.pr$person, reacties = cths.pr$reacties), sum), by = "person")
colnames(cths.pr)[colnames(cths.pr) == "reacties.x"] <- "reacties"
colnames(cths.pr)[colnames(cths.pr) == "reacties.y"] <- "reacties.t"
cths.pr$pr <- cths.pr$reacties/cths.pr$reacties.t

p10a <- ggplot(cths.pr, aes(x = factor(1), y = pr, fill = reactie)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  facet_wrap(~person) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), text = element_text(family = "Helvetica-Light"),
        legend.title = element_blank()) +
  scale_fill_brewer(palette = "Set3")
p10a

p10b <- ggplot(cths.pr, aes(x = factor(1), y = as.factor(pr), fill = reactie)) +
  geom_bar(width = 1, stat = "identity", position = "dodge") +
  facet_wrap(~person, nrow = 1) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "Helvetica-Light"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_fill_brewer(palette = "Set3")
p10b

#ggsave("p10b.eps", p10b, bg = "transparent", width = 20, height = 3, units = "cm", fonts = "Helvetica-Light")