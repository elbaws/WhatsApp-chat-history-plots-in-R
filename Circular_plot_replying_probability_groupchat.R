rm(list = ls())

library(stringr)
library(circlize)

setwd("/Users/swable/Desktop")
chathistory <- read.csv("WhatsApp_Chat_with_Friends.csv")
colnames(chathistory)[1] <- "raw"
chathistory$raw <- as.character(chathistory$raw)

persons <- c("Adam", "Bram", "Casper", "Daan", "Fabian", "Julian", "Koen", "Lucas", "Niels", "Sven", "Younes")

#Colors: Set3 of colorbrewer2.org
set3 <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#979797", "#bc80bd", "#ffed6f") #12: #ccebc5

######################################################
################Setting up the data###################
######################################################

#Break-up into columns
chathistory$date <- as.Date(gsub("\\s.*$", "", chathistory$raw, perl = T), "%d-%m-%y")
chathistory$time <- gsub("^\\S+.?([^:]*:[^:]*):.*$", "\\1", chathistory$raw, perl = T)
chathistory$person <- gsub("^\\S+\\s+\\S+\\s+([^:]*):.*$", "\\1", chathistory$raw, perl = T)
chathistory$message <- gsub("^.*?:.*?:.*?:.*?:", "", chathistory$raw, perl = T)

#Clean
chathistory <- subset(chathistory, !is.na(date))
chathistory <- subset(chathistory, nchar(time) == 5)
chathistory <- subset(chathistory, nchar(person) <= 21)

#Consolidate persons: Some people in my chathistory have had more than 1 number. This is where I put them all under 1 label.
levels(chathistory$person)
chathistory$person <- as.factor(chathistory$person)
levels(chathistory$person)[levels(chathistory$person) == "Adam Surname"] <- persons[1]
levels(chathistory$person)[levels(chathistory$person) == "Bas Surname"] <- persons[2]
levels(chathistory$person)[levels(chathistory$person) == "Casper Surname"] <- persons[3]
levels(chathistory$person)[levels(chathistory$person) == "Daan Surname"] <- persons[4]
levels(chathistory$person)[levels(chathistory$person) == "Fabian Surname"] <- persons[5]
levels(chathistory$person)[levels(chathistory$person) == "Julian Surname"] <- persons[6]
levels(chathistory$person)[levels(chathistory$person) == "Koen Surname"] <- persons[7]
levels(chathistory$person)[levels(chathistory$person) == "Lucas Surname"] <- persons[8]
levels(chathistory$person)[levels(chathistory$person) == "Niels Surname"] <- persons[9]
levels(chathistory$person)[levels(chathistory$person) == "Sven Surname 1"] <- persons[10]
levels(chathistory$person)[levels(chathistory$person) == "Sven Surname 2"] <- persons[10]
levels(chathistory$person)[levels(chathistory$person) == "Younes Surname 1"] <- persons[11]
levels(chathistory$person)[levels(chathistory$person) == "Younes Surname 2"] <- persons[11]
levels(chathistory$person)[levels(chathistory$person) == "Younes Surname 3"] <- persons[11]
levels(chathistory$person)[levels(chathistory$person) == "Younes Surname 4"] <- persons[11]

chathistory <- subset(chathistory, chathistory$person %in% persons)

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


#################
#Probabilities of replying per person
cths.pr <- data.frame(person = cths$person)
cths.pr$person <- as.character(cths.pr$person)

cths.pr$reactie <- "nonyet"
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

#################
#Circular plot
cths.pr.plotdf <- data.frame(from = cths.pr$reactie, to = cths.pr$person, value = cths.pr$pr, stringsAsFactors = F)
set3.named <- set3
set3.named <- setNames(set3.named, persons)

chordDiagram(cths.pr.plotdf, grid.col = set3.named, transparency = 1/4,
             directional = 1, direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow", diffHeight = -uh(2, "mm"),
             annotationTrack = "grid")
for (j in unique(persons)) {
  highlight.sector(sector.index = j, col = NA, track.index = 1, text = j, text.vjust = -3, niceFacing = TRUE)
}
circos.clear()

