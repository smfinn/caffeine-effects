## Design Project

library(tidyverse)
library(perm)

## Heartrate data ##############################################################

### Input data
heartrate <- c(70, 67, 68, 73,
               68, 63, 65, 68,
               65, 67, 71, 74,
               67, 64, 64, 71)
replicates <- rep(c(1, 2), each = 8)
blocks <- rep(c("Jenna", "Sean"), times = 2, each = 4)
volume <- rep(c("six", "twelve"), times = 8)
beverage <- rep(c("tea", "coffee"), each = 2, times = 4)

project <- as.data.frame(cbind(heartrate, replicates, blocks, volume, beverage))

project <- project %>% mutate(replicates = as.factor(replicates),
                              blocks = as.factor(blocks),
                              volume = as.factor(volume),
                              beverage = as.factor(beverage),
                              heartrate = as.numeric(heartrate))
str(project)


### Run factorial design with blocking (ANOVA)
model <- lm(heartrate ~ blocks + volume*beverage)
model$effects
effects <- abs(model$effects[-1])

qq <- qqnorm(effects, type="n")
text(qq$x, qq$y, labels = names(effects))

summary(aov(model))


### Analyze residuals
shapiro.test(model$residuals)
bartlett.test(heartrate, volume)
bartlett.test(heartrate, beverage)

### Interaction plots
interaction.plot(volume, beverage, heartrate)
interaction.plot(volume, blocks, heartrate)
interaction.plot(beverage, blocks, heartrate)

jenna <- project[blocks == "Jenna",]
sean <- project[blocks == "Sean",]

par(mfrow = c(1,2))
interaction.plot(jenna$volume, jenna$beverage, jenna.subset$heartrate)
interaction.plot(sean$volume, sean$beverage, sean.subset$heartrate)


## Does drinking a higher volume of tea reduce heart rate? #####################

tea.subset <- project[beverage == "tea",]
str(tea.subset)

shapiro.test(tea.subset$heartrate)

t.test(tea.subset$heartrate[volume == "six"], 
       tea.subset$heartrate[volume == "twelve"],
       alternative = "greater")

six <- as.vector(na.omit(tea.subset$heartrate[volume == "six"]))
twelve <- as.vector(na.omit(tea.subset$heartrate[volume == "twelve"]))
permTS(six, twelve, alternative = "greater")



## Memory data #################################################################

### Input data
memory <- c(6, 7, 7, 8,
            6, 7, 6, 6,
            7, 7, 8, 7,
            7, 7, 7, 7)
replicates <- rep(c(1, 2), each = 8)
blocks <- rep(c("Jenna", "Sean"), times = 2, each = 4)
volume <- rep(c("six ounces", "twelve ounces"), times = 8)
beverage <- rep(c("tea", "coffee"), each = 2, times = 4)

project <- as.data.frame(cbind(memory, replicates, blocks, volume, beverage))

project <- project %>% mutate(replicates = as.factor(replicates),
                              blocks = as.factor(blocks),
                              volume = as.factor(volume),
                              beverage = as.factor(beverage),
                              memory = as.numeric(memory))
str(project)


### Run factorial design with blocking (ANOVA)
model <- lm(memory ~ blocks + volume*beverage)
model$effects
effects <- abs(model$effects[-1])

qq <- qqnorm(effects, type="n")
text(qq$x, qq$y, labels = names(effects))

summary(aov(model))


### Analyze residuals
shapiro.test(model$residuals)
bartlett.test(memory, volume)
bartlett.test(memory, beverage)

### Interaction plots
interaction.plot(volume, beverage, memory)
interaction.plot(volume, blocks, memory)
interaction.plot(beverage, blocks, memory)

jenna.subset <- project[blocks == "Jenna",]
sean.subset <- project[blocks == "Sean",]

par(mfrow = c(1,2))
interaction.plot(jenna.subset$volume, jenna.subset$beverage, jenna.subset$memory)
interaction.plot(sean.subset$volume, sean.subset$beverage, sean.subset$memory)


