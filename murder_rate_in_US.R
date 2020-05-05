install.packages(ggthemes)
library(tidyverse)
library(dslabs)
library(dplyr)
library(purrr)
library(readxl)
library(ggthemes)
library(ggrepel)

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

##Copying data file 
filename <- "murders.csv"
dir <- system.file("extdata", package = "dslabs") 
fullpath <- file.path(dir, filename)

file.copy(fullpath, "murders.csv")

list.files()

#Importing data files

read_lines("murders.csv",n_max = 3)
dat<- read.csv("murders.csv")

## Visualizing data using ggplot
murders %>% ggplot() + geom_point(aes(x=population/10^6, y=total))

p<-murders %>% ggplot(aes(population/10^6, total, label = abb))

## Reference line - Average murder rate in the US
r<- murders %>% summarise(rate= sum(total)/sum(population)*10^6)%>% pull(rate)

##ggplot with reference line
p<-p + geom_point(aes(col=region),size=3) + geom_text_repel(nudge_x=0.05) + scale_x_log10() + scale_y_log10() + 
  xlab("Population in millions (log scale)") + ylab("Total number of murders (log scale)") + 
  ggtitle("US gun murders in 2010") + geom_abline(intercept = log10(r),lty=2, color = "darkgrey")

##ggplot with theme
p+theme_economist()
















