library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

##Expectativa de vida de africa coloreada por region

gapminder %>% filter(continent=="Africa" & year==2012) %>%
  ggplot(aes( fertility,life_expectancy, color = region)) +
  geom_point() 

##Analisis del impacto de la guerra de Vietnam en la expectativa de vida 

years <- 1960:2010
countries <- c("United States", "Vietnam")
tab <- gapminder %>% filter(year %in% years & country %in% countries)

p <- tab %>%
  ggplot(aes(year, life_expectancy, color=country)) +
  geom_line()
p

##Evolucion de los ingresos de los paises Africanos, coloreado por Region

daydollars <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent =="Africa" & year%in%c(1970,2010) & !is.na(dollars_per_day)) 

daydollars %>%
  ggplot(aes(dollars_per_day, fill=region)) +
  geom_density(bw=0.5, position="stack") +
  scale_x_continuous(trans="log2") +
  facet_grid(year~.)

##Relacion entre la mortalidad infantil y los ingresos en los paises Africanos, ademas de su evolucion
##entre 1970 y 2010

gapminder_Africa_2010 <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in%c(1970,2010) & !is.na(dollars_per_day) & !is.na(infant_mortality))

gapminder_Africa_2010 %>%   
  ggplot(aes( dollars_per_day,infant_mortality, color=region, label = country)) +
  geom_point() +
  scale_x_continuous(trans='log2') +
  facet_grid(year~.)
