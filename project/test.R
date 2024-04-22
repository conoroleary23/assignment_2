install.packages('tidyverse')
install.packages('gapminder')
library(tidyverse)
library(gapminder)
data('gapminder')
head(gapminder)
filter(gapminder, country == 'Ireland')
gapminder_france <- filter(gapminder, country == 'France')
gapminder_france_2007 <- filter(gapminder_france, year == 2007)
gapminder_keep <- select(gapminder, country, year,  pop)
select(gapminder, country, pop, measure_year = year)
new_data <- select(gapminder, pop, year)
new_data
mutate(gapminder, gdp_total = pop * gdpPercap, xyz = year - pop)
mutate(gapminder, country_upper = toupper(country))
europe_2007 <- filter(gapminder, continent == 'Europe', year == 2007)
mutate(europe_2007, europe_pop = pop/sum(pop))
gapminder_group <- group_by(gapminder, continent, year)
summarise(gapminder_group, meanpop = mean(pop))
seq(1, 10) %>% sum()
starwars %>%
  select(name, height, mass, homeworld) %>%
  mutate(height = height * 0.0328084) %>% # overwrite height values to feet
  group_by(homeworld) %>%
  filter(height == max(height))
library(gapminder)
library(tidyverse)
ggplot(data = gapminder)
ggplot(data = gapminder) +
  aes(
    x = gdpPercap, 
    y = lifeExp, 
    color = continent
  )
ggplot(data = my_dataframe) +
  aes(x = my_xaxis_variable, y = my_yaxis_variable) +
  geom_point()
ggplot(data = gapminder) +
  aes(x = gdpPercap, y = lifeExp, color = continent) +
  geom_point()
ggplot(data = gapminder) +
  aes(x = year, y = lifeExp, color = country) +
  geom_line() +
  guides(color ="none")
gapminder_avg_continent_2007 <- gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarise(m_lifeExp = mean(lifeExp))
ggplot(data = gapminder_avg_continent_2007) +
  aes(x = continent, y = m_lifeExp, fill = continent) +
  geom_col()
gapminder_avg_continent_2007 <- gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarise(m_lifeExp = mean(lifeExp))
ggplot(data = gapminder_avg_continent_2007) +
  aes(x = continent, y = m_lifeExp, fill = continent) +
  geom_col()
ggplot(gapminder) +
  aes(x = continent, y = lifeExp, color = continent, fill = continent) +
  geom_boxplot()


ggplot(gapminder) +
  aes(x = year, y = pop, fill = continent) +
  geom_col()
options(scipen = 999)
ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, color = continent) +
  geom_point() +
  theme_dark()
ggplot(gapminder) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_point() +
  facet_wrap(~ continent) +
  theme_dark()
ggplot(gapminder) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_point() +
  facet_wrap(~ continent, nrow = 1) +
  labs(
    x = "Year (from 1952 to 2007)",
    y = "Life Expectancy",
    title = "Evolution of life expectancy from 1952 to 2007 per continent."
  ) +
  theme_dark()
ggplot(gapminder) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ continent, nrow = 1) +
  theme_classic()
ggplot(gapminder) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ continent, nrow = 1) +
  theme_classic()
ggplot(gapminder) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm") +
  facet_wrap(~ continent, nrow = 1) +
  scale_x_continuous(breaks = c(1960, 1980, 2000)) +
  labs(
    x = "Year (from 1952 to 2007)",
    y = "Life Expectancy",
    title = "Evolution of life expectancy from 1952 to 2007 per continent."
  ) +
  theme_dark() +
  theme(text = element_text(size = 20)) +
  guides(color ="none")
