library(readr)
library(tidyverse)
setwd("~/analytics")
sav16  <- read_csv("nba_savant 2015-16.csv", na = c(""))
sav17 <- read_csv("nba_savant 2016-17.csv", na = c(""))
sav18  <- read_csv("nba_savant 2017-18.csv", na = c(""))
sav16$year <- 16
sav17$year <- 17
sav18$year <- 18
sav11  <- read_csv("nba_savant 2010-11.csv", na = c(""))
sav12 <- read_csv("nba_savant 2011-12.csv", na = c(""))
sav13  <- read_csv("nba_savant 2012-13.csv", na = c(""))
sav14  <- read_csv("nba_savant 2013-14.csv", na = c(""))
sav15  <- read_csv("nba_savant 2014-15.csv", na = c(""))

sav11$year <- 11
sav12$year <- 12
sav13$year <- 13
sav14$year <- 14

sav15$year <- 15


# 
# levels18 <- unique(sav18$action_type)
# 
# levels17 <- unique(sav17$action_type)
# 
# notinlevels <- levels16[which(!(levels16 %in% levels18))]
# notinlevels


sav <- rbind(sav11, sav12, sav13, sav14, sav15, sav16, sav17, sav18)

nba.com16 <- read_csv("nba.com 2015-16.csv")
nba.com17 <- read_csv("nba.com 2016-17.csv")
nba.com18 <- read_csv("nba.com 2017-18.csv")

nba.com16$year <- 16
nba.com17$year <- 17
nba.com18$year <- 18

nba.com11 <- read_csv("nba.com 2010-11.csv")
nba.com12 <- read_csv("nba.com 2011-12.csv")
nba.com13 <- read_csv("nba.com 2012-13.csv")
nba.com14 <- read_csv("nba.com 2013-14.csv")
nba.com15 <- read_csv("nba.com 2014-15.csv")



nba.com <- rbind(nba.com11, nba.com12, nba.com13, nba.com14, nba.com15, nba.com16, nba.com17, nba.com18)


names(nba.com)[1] <- "team_name"

nba.com$team_name <- gsub("LA Clippers", "Los Angeles Clippers", nba.com$team_name)
sav$team_name <- gsub("LA Clippers", "Los Angeles Clippers", sav$team_name)

sav$opponent <- gsub("LA Clippers", "Los Angeles Clippers", sav$opponent)


# nba <- rbind(nba2017, nba2016)


library(tidyverse)

nba <- sav


#shot_types <- nba %>% group_by(action_type) %>% summarise(pct = mean(shot_made_flag), number = n())

is.three <- nba$shot_type

index <- which(colnames(nba) == 'shot_type')

nba <- nba[, -c(index)]

# nba$action_type <- paste(nba$action_type, is.three, sep = " ")
# 
# dunks <- c('Alley Oop Dunk Shot', 'Cutting Dunk Shot', 'Driving Dunk Shot', 'Driving Reverse Dunk Shot',
#            'Dunk Shot')
# layups <- c('Alley Oop Layup shot', 'Cutting Finger Roll Layup Shot', 'Driving Reverse Layup Shot',
#             'Cutting Layup Shot', 'Driving Layup Shot', 'Finger Roll Layup Shot',
#             'Driving Finger Roll Layup Shot', 'Driving Bank shot')
# hookshots <- c('Hook Bank Shot', 'Hook Shot', 'Driving Bank Hook Shot', 'Driving Hook Shot')
# jumpshots <- c('Driving Floating Bank Jump Shot', 'Driving Jump shot', 'Fadeaway Jump Shot', 'Jump Shot',
#                'Driving Floating Jump Shot', 'Floating Jump shot', 'Jump Bank Shot', 'Fadeaway Bank shot')
# nba$action_type <- with(nba, ifelse(action_type %in% dunks, "dunks",
#                                ifelse(action_type %in% layups, "layups",
#                                       ifelse(action_type %in% hookshots, "hookshots",
#                                              ifelse(action_type %in% jumpshots, "jumpshots", "other")))))

teamsoff <- nba %>% group_by(team_name, year, action_type) %>% summarise(number = n())
teamsdef <- nba %>% group_by(opponent, year, action_type) %>% summarise(number = n())




# offnames <- unique(teamsoff$team_name)
# 
# defnames <- unique(teamsdef$team_name)
# 
# notinlevels <- offnames[which(!(offnames %in% defnames))]
# notinlevels




teamsoff <- teamsoff %>% spread(key = action_type, value = number, fill = 0)
teamsoff
teamsdef <- teamsdef %>% spread(key = action_type, value = number, fill = 0)
teamsdef

teamsoff[,3:ncol(teamsoff)] <- (teamsoff[,3:ncol(teamsoff)] / rowSums(teamsoff[,3:ncol(teamsoff)])) * 100

teamsdef[,3:ncol(teamsdef)] <- (teamsdef[,3:ncol(teamsdef)] / rowSums(teamsdef[,3:ncol(teamsdef)])) * 100 

# 
# divider <- function(x, totaloffshots){
#   
#  return((x / totaloffshots) * 100)
# }
# 
# teamsoff[,3:ncol(teamsoff)] <- apply(teamsoff[,3:ncol(teamsoff)], MARGIN = 1, FUN = divider)
# 




names(teamsdef)[1] <- 'team'
names(teamsoff)[1] <- 'team'

paster <- function(x){
  
  return(paste(x, 'off', sep = " "))
  
}

colnames(teamsoff)[3:ncol(teamsoff)] <- lapply(colnames(teamsoff)[3:ncol(teamsoff)], FUN = paster)



paster <- function(x){
  
  return(paste(x, 'def', sep = " "))
  
}

colnames(teamsdef)[3:ncol(teamsdef)] <- lapply(colnames(teamsdef)[3:ncol(teamsdef)], FUN = paster)


teams <- merge(teamsdef, teamsoff)



# teams[, 2:end] <- 100* teams[, 2:end]/rowSums(teams[, 2:end])
# teams
# write.csv(teams, file = "NBA 2016-17 teams by shot type.csv", row.names = F)



names(nba.com)[1] <- 'team'


nbatotal <- merge(teams, nba.com)

names(nbatotal) <- gsub(" ", "", names(nbatotal))


end <- which(colnames(nbatotal) == 'GP') - 1



good <- apply(nbatotal[,3:end], MARGIN = 2, FUN = max)

big.prop <- good[which(good > 3)]






formula1 <- as.formula(paste("NETRTG", "~",
                             paste(names(big.prop), collapse = " + "), "+PACE",
                             sep = " "
))

formula1
library(MASS)

library(rlang)


library(leaps)



set.seed(1110)
library(caTools)

sample = sample.split(nbatotal$team, SplitRatio = .8)
train = subset(nbatotal, sample == TRUE)
test  = subset(nbatotal, sample == FALSE)



fit <- lm(formula1, data=train)
step <- stepAIC(fit, direction = "both")

library(broom)

# create a dataframe frm model's output
tm = tidy(step)

terms <- tm$term[tm$p.value < 0.05]

terms <- terms[-c(1)]

formula1 <- as.formula(paste("NETRTG", "~",
                             paste(terms, collapse = " + "),
                             sep = ""
))

fit <- lm(formula1, data=train)
step <- stepAIC(fit, direction = "both")
summary(step)



please <- predict(step, newdata = test)

crossscor <- cor(test$NETRTG, please)
crossscor
error <- test$NETRTG - please


lm_error <- sqrt(mean(error^2))
lm_error




ggplot(test, aes(please, test$NETRTG, label = team)) + 
  geom_point() + 
  geom_text() + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlab ("Fittedvalues") + 
  ylab ("Actualvalue") + 
  ggtitle("Predicted Net Rating versus Actual")


# 
# 
# 
# library(e1071)
# 
# #Fit a model. The function syntax is very similar to lm function
# 
# 
# 
# model_svm <- svm(formula1, nbatotal)
# 
# 
# 
# #Use the predictions on the data
# 
# pred <- predict(model_svm, nbatotal)
# 
# error_2 <- nbatotal$NETRTG - pred
# 
# 
# svm_error <- sqrt(mean(error_2^2))
# 