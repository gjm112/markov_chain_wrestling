---
  title: "Untitled"
author: "Robert Tedesco"
date: "7/21/2021"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(data.table)
library(repeated)
library(dplyr)
```

```{r data2014}
#Just change your path to the 14-15 data.
# path <- "C:\\Users\\rober\\OneDrive\\Desktop\\Hot Hand Chidden 5-19\\NBA shot log 14-15-regular season\\shot_logs.csv"

path <- "/Users/gregorymatthews/Dropbox/Tedesco/code/Hot Hand Chidden 5-19--working/NBA shot log 14-15-regular season/shot_logs.csv"
NBA.full <- read.csv(path)
#Filter relevant data for Klay Thompson.
klay <- NBA.full[NBA.full$player_name=="kawhi leonard",]
klay <- klay[,c(2,7,8,12,13,14,17,20)]
klay$GAME_CLOCK <- as.ITime(klay$GAME_CLOCK, format = "%M:%S")
#Remove below comment for 3 pointers only.
#klay <-klay[klay$PTS_TYPE==3, ]
klay <- klay[klay$SHOT_DIST>1 & klay$SHOT_DIST<40,]
#If a quarter/game is missed, we want to account for the respective 12 or 48 minutes.
#Reverse row order and create a gameid for each unique game.
klay <- klay %>% arrange(-row_number())
klay$id <- as.numeric(vctrs::vec_group_id(klay$MATCHUP))
klay <- klay %>% group_by(id, PERIOD) %>% arrange(desc(GAME_CLOCK), .by_group=T)
klay <- subset(klay, PERIOD!=5)
#Cumulative time in minutes calculated as:
#48(gameid-1) + 12(Q-1) + (12 - gameclock)
klay$game_minute <- (2880*(klay$id-1)) + (720*(klay$PERIOD-1)) + as.numeric(as.ITime(strptime(12, format="%M")) - klay$GAME_CLOCK)
klay$game_minute <- klay$game_minute/60
data <-klay$SHOT_RESULT[!duplicated(klay$game_minute)]
length(data)
#cmu= time constant
#Begins with = initial guess
#pcmu is LOGIT SCALE, guess of -2,2 corresponds 
mu <- function(p) array(p, c(1,2))
o <- data <- as.integer((data == "made") + 0)
dist <- klay$SHOT_DIST[!duplicated(klay$game_minute)]
close <- klay$CLOSE_DEF_DIST[!duplicated(klay$game_minute)]
t <- times <- unique(klay$game_minute)

max(dist)
```


```{r sd}
library(rjags)
#Create come simple data
set.seed(1234)
#Observed time indexes
# v <- c(1:20) 
# #Observed time
# t <- as.numeric(times)
# #observed outputs
# o <- c(1,1,1,1,1,1,0,1,0,1,0,0,0,0,1,0,0,1,0,0)
# o <- as.numeric(data)
# dist <- as.numeric(dist)

#Two states
pi <- c(0.5, 0.5)

#Not hidden
#Data are times and states
#Start with a two state model
set.seed(1234)
t <- c(0,cumsum(rexp(100,1/10)),1000)
d <- diff(t)
s <- c(2,rep(c(1,2,3),length = 100))
#Two states
init <- 1
pi <- c(0,1,0)

n <- as.matrix(table(s[-length(s)], s[-1]))

#Total amount of time spent in state
tot <- c()
for (j in 1:3) {
  tot[j] <- sum(diff(c(t,1000))[s == j])
}


model.str <- "model{
 for (i in 1:length(d)){
    d[i] ~ dexp(mu[i]) 
    mu[i] <- sum(Q[s[i],])
 }
  
  for (i in 2:length(d)){
    s[i] ~ dcat(Q[s[i-1],])
  }
  
  
  Q[1,1] <- 0
  Q[1,2] <- lambda[1]
  Q[1,3] <- lambda[2]
  Q[2,1] <- lambda[3]
  Q[2,2] <- 0
  Q[2,3] <- lambda[4]
  Q[3,1] <- lambda[5]
  Q[3,2] <- lambda[6]
  Q[3,3] <- 0
  
 
  #Priors
  for (j in 1:6){
  lambda[j] ~ dexp(1/1000)
  }
  
  
  
}"




#setwd("C:\\Users\\rober\\OneDrive\\Desktop")
write(model.str,"/Users/gregorymatthews/jags_model.bug")

load.module("msm")
jags<-jags.model("/Users/gregorymatthews/jags_model.bug",data=list('d'=d, 's' = s),n.chains=1,n.adapt=1000)

update(jags, 10000)

a<-jags.samples(jags,c('lambda'),1000)


matrix(apply(a$lambda,c(1),mean),ncol = 2, byrow = TRUE)
