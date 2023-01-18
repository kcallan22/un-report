# Analyze life expectancy and C02 emissions versus population with gapminder data
# Date: Jan 17th, 2023
# Author: Katrina Callan


#load in packages necessary for analysis
library("tidyverse")
library("readr")

# Read in data for analysis
gapminder_1997 <- read_csv("gapminder_1997.csv")

# Plotting data for visualization

ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap) + 
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy (yrs)") +
  geom_point() +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) +
  scale_color_brewer(palette = "Set3") +
  aes(font = "helvetica") +
  aes(size = pop) +
  aes(size = pop/1000000) + 
  labs(size = "Population (in millions)") +
  aes(shape = continent)

# Shorthanded ggplot
ggplot(data = gapminder_1997,
       aes(x = gdpPercap, y = lifeExp, color = Continent,
           shape = continent, size = pop)) +
  labs(x= "GDP Per Capita", y = "Life Expectancy",
       title = "Do people in wealthy countries live longer?",
       size = "Population (in millions)") +
  geom_point()

# Read in all of the data from gapminder (more years than 1997!)












name <- "Ben"
name
age <- 26
age
name <- "Harry Potter"
name
age


read_csv()
?read_csv()
read_csv(gapminder_1997.csv)
read_csv(file = "gapminder_1997.csv")

Sys.Date()
getwd()
sum(5, 6)

round()
round(3.1415)
round(3.1415,3)
?read_csv(round(3.1415))

round(x = 3.1415, digits = 2)
round(digits = 2, x = 3.1415)
round(2, 3.1415)
round(x = 3.1415)
round(3.1415, 2)

read_excel()

gapminder_1997 <- read_csv("gapminder_data.csv")
rm(gapminder_1997)
gapminder_data <- read_csv("gapminder_data.csv")
View(gapminder_data)
dim(gapminder_data)
head(gapminder_data)
tail(gapminder_data)

# Challenge predicting ggplot output
ggplot(data = gapminder_data) +
  aes(x= continent, y = lifeExp, color = continent)  +
  labs(color = "Continent")
  geom_violin(fill = "pink", color = "cornflowerblue") +
  geom_jitter(aes(size = pop))

#learn about data  
str(gapminder_data)

# histogram
ggplot(gapminder_1997) +
  aes(x=lifeExp) +
  geom_histogram(bins = 20) +
  theme_minimal()

ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(rows= vars(continent))

ggsave("awesome_plot.jpg", width = 6, height = 4)
?ggsave
ggsave("awesome_plot.tiff", width = 6, height = 4, 
       units = "cm")

ggsave(plot = lifeExp_hist_prism,
       file = "cool_prism_plot.png",
       device = "pnj"
       width = 4, height = 4)
?purr

ggplot(data = gapminder_1997) +
  aes(x= continent, y = lifeExp, color = continent)  +
  labs(color = Continent)
geom_violin(fill = "pink", color = "cornflowerblue") +
  geom_jitter(aes(size = pop))
