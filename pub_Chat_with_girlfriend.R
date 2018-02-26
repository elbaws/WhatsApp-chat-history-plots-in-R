#Clear the workspace
rm(list = ls())

#Load the required packages
library(ggplot2) #We use this one for all the plots
library(stringr) #Used for splitting up message strings into seperate word strings
library(extrafont) #To be able to use some additional fonts in the plots

setwd("/Users/swable/Downloads/Chat data visualisatie/") #Change this to whatever path your data is.

#This is the raw data as provided by Whatsapp. I did however change the type
#from .txt to .csv to make it easier to read it in to R. Just open the .txt file
#in excel and then save it to .csv.
chathistory <- read.csv("WhatsApp_Chat_with_Swable.csv", sep = "")
colnames(chathistory)[1] <- "raw" #Rename the first column to raw
chathistory[2] <- NULL #And the delete the second column, which only includes some trash.
chathistory$raw <- as.character(chathistory$raw) #Convert the raw data into string.

#Because my data included some missing days (I changed phones at some point, and 
#something messed up part of the data), I generate a list of all days during my
#period of analysis (19-11-2014 - 20-02-2018).
dates <- as.Date("14-11-19", "%y-%m-%d") #This is the starting point
for (i in 1:1189){ #1189 days need to be added from that point to reach 20-02-2018.
  dates <- c(dates, tail(dates, n=1) + 1)
}
dates <- rbind(data.frame(date = dates, person = "A"), data.frame(date = dates, person = "B")) #Put the list in a DF, twice for person 'A' and person 'B'


######################################################
################Setting up the data###################
######################################################

#Clean a bit
chathistory <- subset(chathistory, grepl("^[0-9]", chathistory$raw))
chathistory <- subset(chathistory, nchar(raw) > 10)

#Break-up into columns
chathistory$date <- gsub(",.*$", "", chathistory$raw)
chathistory$time <- gsub("\\s+.*$", "", gsub("^\\S+\\s+", "", chathistory$raw))
chathistory$person <- substr(gsub(":.*$", "", gsub("^\\S+\\s+\\S+\\s+\\S+\\s+", "", chathistory$raw)), 1, 1)
chathistory$message <- gsub("^.*?:.*?:\\s+", "", chathistory$raw)

#Clean some more
chathistory <- subset(chathistory, person == "A" | person == "B")

#Break-up further
chathistory$year <- as.numeric(gsub("^.*/", "", chathistory$date)) + 2000
chathistory$month <- as.numeric(gsub("/.*$", "", chathistory$date))
chathistory$day <- as.numeric(gsub("^.*/", "", substr(chathistory$date, 1, nchar(chathistory$date)-3)))
chathistory$time <- as.character(chathistory$time)
chathistory$hour <- as.numeric(gsub(":.*$", "", chathistory$time))

#Media messages
chathistory$media <- 0
chathistory$media[chathistory$message == "<Media omitted>"] <- 1

#Construct the dataset
cths <- data.frame(person = chathistory$person, date = chathistory$date,
                   day = chathistory$day, month = chathistory$month, year = chathistory$year, hour = chathistory$hour,
                   nchar = nchar(chathistory$message), words = str_count(chathistory$message, '\\w+'), media = chathistory$media)
#cths$moyr <- as.factor(paste(cths$year,"-",cths$month))

#Aggregate to hours
cths$messages <- 1
cths.hour <- data.frame(person = cths$person, hour = cths$hour, messages = cths$messages, words = cths$words)
cths.hour <- aggregate(. ~ person + hour, cths.hour, sum)
cths.hour$wordspm <- cths.hour$words/cths.hour$messages

#Aggregate to hours total (not split for persons)
cths.hour.t <- aggregate(. ~ hour, cths.hour, sum)
cths.hour.t$person <- NULL

#Aggregate to days
cths.day <- data.frame(person = cths$person, day = cths$date, messages = cths$messages, words = cths$words)
cths.day <- aggregate(. ~ person + day, cths.day, sum)
cths.day$wordspm <- cths.day$words/cths.day$messages

#Insert missing days
cths.day$day <- as.Date(cths.day$day, "%m/%d/%y")
cths.day <- merge(cths.day, dates, by.x = c("day", "person"), by.y = c("date", "person"), all.y = T)
cths.day[is.na(cths.day)] <- 0
rm(dates)

#Aggregate to days total (not split for persons)
cths.day.t <- aggregate(. ~ day, cths.day, sum)
cths.day.t$person <- NULL

#Include percentages per person for days DF
cths.day <- merge(cths.day, cths.day.t, by = "day")
cths.day$wordspm.y <- NULL
names(cths.day)[names(cths.day)=="messages.x"] <- "messages"
names(cths.day)[names(cths.day)=="words.x"] <- "words"
names(cths.day)[names(cths.day)=="wordspm.x"] <- "wordspm"
names(cths.day)[names(cths.day)=="messages.y"] <- "messages.t"
names(cths.day)[names(cths.day)=="words.y"] <- "words.t"

cths.day$messages.pc <- cths.day$messages/cths.day$messages.t
cths.day$words.pc <- cths.day$words/cths.day$words.t

cths.day$messages.t <- NULL
cths.day$words.t <- NULL

#Aggregate to months
cths.month <- data.frame(person = cths.day$person, month = cths.day$day, messages = cths.day$messages, words = cths.day$words)
cths.month$month <- as.factor(substr(cths.month$month, 1, 7))
cths.month <- aggregate(. ~ person + month, cths.month, sum)
cths.month$wordspm <- cths.month$words/cths.month$messages

#Aggregate to months total (not split for persons)
cths.month.t <- aggregate(. ~ month, cths.month, sum)
cths.month.t$person <- NULL

#Include percentages per person for months DF
cths.month <- merge(cths.month, cths.month.t, by = "month")
cths.month$wordspm.y <- NULL
names(cths.month)[names(cths.month)=="messages.x"] <- "messages"
names(cths.month)[names(cths.month)=="words.x"] <- "words"
names(cths.month)[names(cths.month)=="wordspm.x"] <- "wordspm"
names(cths.month)[names(cths.month)=="messages.y"] <- "messages.t"
names(cths.month)[names(cths.month)=="words.y"] <- "words.t"

cths.month$messages.pc <- cths.month$messages/cths.month$messages.t
cths.month$words.pc <- cths.month$words/cths.month$words.t

cths.month$messages.t <- NULL
cths.month$words.t <- NULL

#Aggregate to years
cths.year <- data.frame(person = cths.month$person, year = cths.month$month, messages = cths.month$messages, words = cths.month$words)
cths.year$year <- as.factor(substr(as.character(cths.year$year), 1, 4))
cths.year <- aggregate(. ~ person + year, cths.year, sum)
cths.year$wordspm <- cths.year$words/cths.year$messages

#Aggregate to years total (not split for persons)
cths.year.t <- aggregate(. ~ year, cths.year, sum)
cths.year.t$person <- NULL

#Include percentages per person for years DF
cths.year <- merge(cths.year, cths.year.t, by = "year")
cths.year$wordspm.y <- NULL
names(cths.year)[names(cths.year)=="messages.x"] <- "messages"
names(cths.year)[names(cths.year)=="words.x"] <- "words"
names(cths.year)[names(cths.year)=="wordspm.x"] <- "wordspm"
names(cths.year)[names(cths.year)=="messages.y"] <- "messages.t"
names(cths.year)[names(cths.year)=="words.y"] <- "words.t"

cths.year$messages.pc <- cths.year$messages/cths.year$messages.t
cths.year$words.pc <- cths.year$words/cths.year$words.t

cths.year$messages.t <- NULL
cths.year$words.t <- NULL


######################################################
##################Let the fun start###################
######################################################

#################
#Pie's

cths.year$piey.m <- cths.year$messages.pc/2
cths.year$piey.m[cths.year$person == "A"] <- 1-(cths.year$messages.pc[cths.year$person == "A"]/2)
p1 <- ggplot(cths.year, aes(x = factor(1), y = messages.pc, fill = person)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  facet_wrap(~year) +
  geom_text(aes(x = factor(1), y = piey.m, label = paste(round(messages.pc*100, digits = 1), "%")), size = 3) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), text = element_text(family = "Helvetica-Light"),
        legend.position = "none")
p1

cths.year$piey.w <- cths.year$words.pc/2
cths.year$piey.w[cths.year$person == "A"] <- 1-(cths.year$words.pc[cths.year$person == "A"]/2)
p2 <- ggplot(cths.year, aes(x = factor(1), y = words.pc, fill = person)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  facet_wrap(~year) +
  geom_text(aes(x = factor(1), y = piey.w, label = paste(round(words.pc*100, digits = 1), "%")), size = 3) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), text = element_text(family = "Helvetica-Light"),
        legend.title = element_blank(),
        legend.position = "none")
p2


#################
#Lines from here

#Number of messages
p3 <- ggplot(cths.month, aes(x = as.Date(paste0(cths.month$month, "-1"), "%Y-%m-%d"), y = messages)) +
  geom_line(aes(group = person, color = person)) +
  geom_smooth(aes(group = person, color = person), method = "loess", span = 1, linetype = "dotted", size = .5, alpha = 1/2, se = F) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 months", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light"),
        axis.title = element_blank(), legend.position = "none")
p3

#Number of words
p4 <- ggplot(cths.month, aes(x = as.Date(paste0(cths.month$month, "-1"), "%Y-%m-%d"), y = words)) +
  geom_line(aes(group = person, color = person)) +
  geom_smooth(aes(group = person, color = person), method = "loess", span = 1, linetype = "dotted", size = .5, alpha = 1/2, se = F) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 months", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light"),
        axis.title = element_blank(), legend.position = "none")
p4

#Number of words per message
p5 <- ggplot(cths.month, aes(x = as.Date(paste0(cths.month$month, "-1"), "%Y-%m-%d"), y = wordspm)) +
  geom_line(aes(group = person, color = person)) +
  geom_smooth(aes(group = person, color = person), method = "loess", span = 1, linetype = "dotted", size = .5, alpha = 1/2, se = F) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 months", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light"),
        axis.title = element_blank(), legend.position = "none")
p5

#################
#Frequency plots

#Activity over whole period
p6a <- ggplot(cths.day.t, aes(x = day, y = messages)) +
  geom_area() +
  geom_smooth(method = "loess", span = 1, linetype = "dotted", size = .5, alpha = 1/2, se = F) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 months", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light"),
        axis.title = element_blank(), legend.position = "none",
        panel.grid = element_blank())
p6a

p6b <- ggplot(cths.day, aes(x = day, y = messages, group = person, fill = person)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 months", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light"),
        axis.title = element_blank(), legend.position = "none")
#p6b


#Day of the week
cths.wkday <- cths.day
cths.wkday$wkday <- as.factor(strftime(cths.wkday$day, "%u"))
cths.wkday <- aggregate(. ~ person + wkday, cths.wkday, sum)
cths.wkday$wordspm <- cths.wkday$words/cths.wkday$messages

p7 <- ggplot(cths.wkday, aes(x = wkday, y = messages, group = person, fill = person)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thurstday", "5" = "Friday", "6" = "Saturday", "7" = "Sunday")) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light"),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none")
p7

#Hour of the day
cths.hour$hour <- as.factor(cths.hour$hour)
cths.hour$hour <- factor(cths.hour$hour, levels = c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
p8 <- ggplot(cths.hour, aes(x = hour, y = messages, group = person, fill = person)) +
  geom_bar(stat = "identity") +
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle = 90), text = element_text(family = "Helvetica-Light"),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none")
p8


#################
#Most words used

#Most used words (A)
words.a <- tokenize_words(chathistory$message[chathistory$person == "A"])
wordsa.a <- words.a[[1]]
for (i in 2:length(words.a)){
  wordsa.a <- c(wordsa.a, words.a[[i]])  
}
words.a <- data.frame(word = wordsa.a, frq = 1)
words.a <- aggregate(. ~ word, words.a, sum)
words.a <- words.a[order(-words.a$frq),]
rm(wordsa.a, i)

p9a <- ggplot(head(words.a, n = 15), aes(y = frq, x = reorder(word, frq))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.title = element_blank())
#p9a

#Most used words (B)
words.j <- tokenize_words(chathistory$message[chathistory$person == "B"])
wordsa.j <- words.j[[1]]
for (i in 2:length(words.j)){
  wordsa.j <- c(wordsa.j, words.j[[i]])  
}
words.j <- data.frame(word = wordsa.j, frq = 1)
words.j <- aggregate(. ~ word, words.j, sum)
words.j <- words.j[order(-words.j$frq),]
rm(wordsa.j, i)

p9j <- ggplot(head(words.j, n = 15), aes(y = frq, x = reorder(word, frq))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.title = element_blank())
#p9j

#Combined
words.a$person = "A"
words.j$person = "B"
words.aj <- rbind(words.a, words.j)

words.aj.m <- merge(words.a, words.j, by = "word")
words.aj.m$frq.t <- words.aj.m$frq.x + words.aj.m$frq.y
words.aj.m$frq.x <- NULL
words.aj.m$frq.y <- NULL
words.aj.m$person.x <- NULL
words.aj.m$person.y <- NULL
words.aj <- merge(words.aj, words.aj.m, by = "word")
words.aj <- words.aj[order(-words.aj$frq.t),]
rm(words.aj.m)

p9c <- ggplot(head(words.aj, n = 30), aes(y = frq, x = reorder(word, frq), fill = person, group = person)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.title = element_blank(), legend.position = "none", text = element_text(family = "Helvetica-Light"))
p9c


#################
#Most smilies used

