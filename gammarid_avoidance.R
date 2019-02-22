library(tidyverse)
library(readxl)
library(cowplot)

d <- read_excel("rawdata_2.xlsx",1)
str(d)

d <- as_tibble(d)

### Turn into "long" format:
d2 <- d %>% gather(`2`:`11`,key = "time", value = "location")
str(d2)

## Time from character to integer and then sort by individual and time:
d2$time <- as.integer(d2$time)
d2 <- d2 %>% arrange(individual,time)

## Locations to integers (A=1,B=2,C=3) and call it y
d2$location <- ifelse(d2$location == "A",1,ifelse(d2$location == "B",2,3))
d2$location <- as.integer(d2$location)
d2 <- rename(d2, y = location)

## Create variable for location at previous time
## Except for time=2 because we know at time=1 y=3
d2$y_prev <- NA
for (i in 1:nrow(d2)){
  if (d2$time[i] > 2) d2$y_prev[i] <- d2$y[i-1]
}

## Make factors
d2$y_prev <- as.factor(d2$y_prev)
d2$time <- as.factor(d2$time)
d2$species <- as.factor(d2$species)
d2$test <- as.factor(d2$test)
d2$individual <- as.factor(d2$individual)
d2$infected <- as.factor(d2$infected)


####### brms models ###################################################################

library(brms)

d2$y1 <- d2$y
d2$y2 <- d2$y
d2$pred1 <- d2$species:d2$infected:d2$test
d2$pred2 <- d2$species:d2$infected:d2$test:d2$y_prev:d2$time
d2$sub1 <- ifelse(d2$time=="2",1,0)
d2$sub2 <- ifelse(d2$sub1==0,1,0)

bform1 <- bf(y1 | subset(sub1) ~ pred1) + categorical() +
  bf(y2 | subset(sub2) ~ pred2) + categorical() + set_rescor(F)

m1 <- brm(bform1, 
          prior = prior(normal(0, 5), class = "b"),
          data = d2,
          warmup = 1000,
          iter = 3500,
          chains = 4,
          cores = 4,
          file = "m1_brms")
