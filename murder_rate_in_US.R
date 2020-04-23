library(tidyverse)
library(dslabs)
library(dplyr)
library(purrr)

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

#Compare the murder rates in four groups of states: New England, West Coast, South, and other
#define categorical variables based on existing variables

murders %>% 
  mutate(group = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
    abb %in% c("WA", "OR", "CA") ~ "West Coast",
    region == "South" ~ "South",
    TRUE ~ "Other")) %>%
  group_by(group) %>%
  summarize(rate = sum(total) / sum(population) * 10^5) 

exp(mean(log(murders$population)))
murders %>% log(.$population) %>% 
  mean(.$population) %>% 
  exp(.$population)

compute_s_n <- function(n){
    x=1:n
  tibble(sum=sum(x))
}

n <- 1:100
s_n <- map_df(n, compute_s_n)
