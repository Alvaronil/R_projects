library(dplyr)
library(ggplot2)
library(tidyverse)
library(titanic)

titanic %>%
  filter(Fare!=0) %>%
  ggplot(aes(Survived,Fare, fill=Sex)) +
  scale_y_continuous(trans="log2") +
  geom_boxplot() +
  labs(title="Explorando la relacion entre el pasaje pagado y la supervivencia en el titanic",
       x="Sobrevivio",
       y="Pasaje",
       subtitle = "Pasajes más altos se asocian a una mayor probabilidad de supervivencia\nmientras que los hombres presentan menores tasas de supervivencia")
