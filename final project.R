library(readr)

setwd("~/analytics")
nba  <- read_csv("nba2016.csv", na = c(""))
library(readr)

library(tidyverse)
nba.com <- read_csv("nba.com 2016-17.csv")

names(nba.com)[1] <- "team_name"

nba.com$team_name <- gsub("LA Clippers", "Los Angeles Clippers", nba.com$team_name)

nba.com <- nba.com %>% select(team_name, PACE)

# nba2017 <- read_csv("nba_savant 2017-18.csv", na = c(""))

# nba <- rbind(nba2017, nba2016)


library(tidyverse)


shot_types <- nba %>% group_by(action_type) %>% summarise(pct = mean(shot_made_flag), number = n())

nba$fullshot <- paste(nba$as, nba$shot_type, sep = " ")
teams <- nba %>% group_by(team_name, action_type) %>% summarise(number = n())

teams <- teams %>% spread(key = action_type, value = number, fill = 0)

end <- ncol(teams)




teams[, 2:end] <- 100* teams[, 2:end]/rowSums(teams[, 2:end])
teams
# write.csv(teams, file = "NBA 2016-17 teams by shot type.csv", row.names = F)


nbaeff <- read_csv("nba 2016-17 efficiency ratings.csv", na = "")

names(nbaeff)[1] <- "team_name"

teams$team_name <- gsub("LA Clippers", "Los Angeles Clippers", teams$team_name)

nbatotal <- merge(teams, nbaeff,  by = "team_name")
nbatotal <- merge(nbatotal, nba.com, by = "team_name")

tired <- function(x){
  
  x <- gsub(" ", "", x, fixed = T)
x  <- gsub("/", "", x , fixed = T)
}

colnames(nbatotal) <- lapply(colnames(nbatotal), FUN = tired )
colnames(nbatotal)

write.csv(nbatotal, file = "nba 2016-17 complete.csv")


# badcol <- which(colnames(nbatotal) == "DrivingJumpShot")
# 
# nbatotal <- nbatotal[-c(badcol)]


write(nbatotal, file = "nba 2016-17 by shot type.csv")
formula1 <- as.formula(paste("ORtgA", "~",
                 paste(colnames(nbatotal)[c(2:26)], collapse = "+"), "+PACE",
                 sep = ""
))

formula1
library(MASS)

library(rlang)


library(leaps)



# set.seed(120)
# split <- sample(nrow(nbatotal), 40)
# train <- nbatotal[split,]
# test <- nbatotal[-split,]
fit <- lm(formula1, data=nbatotal)
step <- stepAIC(fit, direction = "both")

please <- predict(step, newdata = nbatotal)

ggplot(nbatotal, aes(please, nbatotal$ORtgA, label = team_name)) + 
  geom_text() + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlab ("Fittedvalues") + 
  ylab ("Actualvalue") + 
  ggtitle("Predicted Adjusted Off Rating versus Actual")

ggplot(nbatotal, aes(PACE, nbatotal$ORtgA, label = team_name))+ 
  geom_text() + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlab ("Pace of Play") + 
  ylab ("Offensive Rating") + 
  ggtitle("Pace versus offensive rating")




