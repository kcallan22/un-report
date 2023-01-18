library(tidyverse)
getwd()
gapminder_data <- read_csv("code/gapminder_data.csv")

summarize(gapminder_data, averageLifeExp=mean(lifeExp), medianLifeExp=median(lifeExp))

#Learning to pipe

gapminder_summary <-gapminder_data%>%
  summarise(averageLifeExp=mean(lifeExp))

gapminder_summary

# Filtering

gapminder_summary_2007<-gapminder_data%>%
  filter(year == 2007)%>%
  summarise(average = mean(lifeExp))

gapminder_data%>%
  filter(year == min(year))%>%
  summarize(Average_GDP = mean(gdpPercap))\

#using group_by ()

gapminder_data%>%
  group_by(year, continent)%>%
  summarise(average = mean(lifeExp),
            error = sd(lifeExp))

#Mutate function

gapminder_data%>%
  mutate(gdp = pop * gdpPercap)

# Mutate a new column which is population in millions

gapminder_data%>%
  mutate(popinMillions = pop / 1000000)

# select function

gapminder_data%>%
  select(pop, year)

gapminder_data%>%
  select(-continent)

# Reshape data to make long (Pivot_Wider)

gapminder_data%>%
  select(country, continent, year, lifeExp)%>%
  pivot_wider(names_from = year, values_from = lifeExp)%>%
  View()

# Working with messy data

co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emissions_dirty%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))%>%
  pivot_wider(names_from = series, values_from = value)

#Combine co2 emissions data with population data from each country and each year
#Filter co2 table for only 2005 and then get rid of it

co2_emissions <- co2_emissions_dirty%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))%>%
  pivot_wider(names_from = series, values_from = value)%>%
  filter(year == 2005)%>%
  select(-year)

# Join 2005 emissions data with pop data from 2007

gapminder_data_2007 <-read.csv("code/gapminder_data.csv")%>%
  filter(year == 2007)%>%
  select(country, pop, lifeExp, gdpPercap)

inner_join(co2_emissions, gapminder_data_2007, by = "country")

anti_join(co2_emissions, gapminder_data_2007, by = "country")

anti_join(gapminder_data_2007, co2_emissions, by = "country")

<- full_join(co2_emissions, gapminder_data_2007)
