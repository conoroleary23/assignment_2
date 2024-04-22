install.packages('tidyverse')
install.packages('gapminder')
install.packages('plotly')
library(tidyverse)
library(gapminder)
library(plotly)
library(dplyr)

library(gapminder)
library(tidyverse)



 
### assignment start
library(tidyverse)

unicef_indicator_2_ <- read_csv("unicef_indicator_2_.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

data_join <- full_join(unicef_indicator_2_, unicef_metadata)
data_join <- full_join (unicef_indicator_2_, unicef_metadata, by = join_by(country))

map_world <- map_data('world')

# map 1
obs_value_2021 <- unicef_indicator_2_ %>%
  filter(time_period == 2021) 
map_world <- map_data("world") 
map_obs_value_2021 <- full_join(map_world, obs_value_2021, by = c("region" = "country"))
ggplot(data = map_obs_value_2021) +
  aes(x = long, y = lat, group = group, fill = obs_value, color = Continent) +
  labs(title = '2021 world displacements') +
  geom_polygon() +
  scale_fill_gradient(low = "pink", high = "purple", na.value = "grey") +
  labs(
    title = "Number of child displacements in countires in 2021",
    subtitle = "Countries in grey have no data due to none being avaible from Unicef",
    caption = "Source: Unicef indicator 2 {Unicef}",
    x = "Longitude",
    y = "Latitude",
    fill = "Number of Child Displacements"
  ) 



#BARPLOT of continents 
ggplot(unicef_indicator_2_, aes(x = Continent, fill = Continent)) +
  geom_bar(width = 0.60) +
  scale_fill_brewer(palette = 'Dark2') +
  labs(title = "Combined overall Data by Continent",
       x = "Continent",
       y = "Displacements (in millions)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#Scatter plot

ggplot(unicef_indicator_2_) +
  aes(x = time_period, y = obs_value, color = Continent) +
  geom_point(alpha = 0.6, size = 2) +  # Adjust alpha and size here
  geom_smooth(method = 'lm', se = FALSE, color = "black", linetype = "dashed") +
  facet_wrap(~ Continent, nrow = 2) + 
  labs(title = 'Evolution of child displacements from 2008 to 2022 per continent',
       x = 'Years',
       y = 'No. of displacements') +
  theme_dark() +
  theme(text = element_text(size = 13)) +
  guides(color = "none")

#Time series

unicef_indicator_2_ %>%
  filter(country %in% c("India", "Ethiopia", "Ukraine", "Pakistan", "China")) %>%
  ggplot(aes(time_period, obs_value, color = country)) +
  geom_line(linetype = 'dashed') +
  geom_point() +
  theme_dark() +
  labs(y = 'Child Displacements',
       x = 'Year',
       title = 'Time Series Via Selected Country') 

#box plot

ggplot(unicef_indicator_2_, aes(x = obs_value, y = Continent)) +
  geom_boxplot(fill = "purple", color = "black", horizontal = TRUE) +
  labs(title = "Boxplot of Internally Displaced Children by Continent",
       x = "Child Displacements",
       y = "Continent") +
  theme_minimal()


#barchart of slected countires 

china_india_data <- unicef_indicator_2_ %>%
  filter(country %in% c("China", "India"))
ggplot(china_india_data, aes(x = country, y = obs_value)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Child Displacements in China and India",
       x = "Country",
       y = "Number of Displacements") +
  theme_minimal()

  










