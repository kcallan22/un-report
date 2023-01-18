library(tidyverse)
getwd()
gapminder_data <- read_csv("code/gapminder_data.csv")

summarize(gapminder_data, averageLifeExp=mean(lifeExp), medianLifeExp=median(lifeExp))

#Learning to pipe

gapminder_summary <-gapminder_data%>%
  summarise(averageLifeExp=mean(lifeExp))

gapminder_summary

# Filtering

gapminder_data%>%
  filter(year == 2007)%>%
  summarise(average = mean(lifeExp))
