library(tidyverse)
library(dslabs)
library(dplyr)

##importing built in data. Murders in the US
data (murders)

install.packages(tidyverse)
library(tidyverse)

##importing data
data(murders)

##Preparing data for analysis
## Calcualting death rates
murders<-mutate(murders, rate==total/population*100000)

##Transforming population data
murders<-mutate(murders, population_in_millions=population/10^6)
head(murders)

##Rank cases base on the rate
murders_rank<- mutate(murders,rank(rate))
head(murders_rank)

## Number of deaths occuring in Northeast and West only
murders_nw<-filter(murders_rank,population<5000000 & region %in% c("Northeast", "West"))
head(murders_nw)
add new codes
