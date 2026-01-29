library(dslabs)
library(ggplot2)
library(dplyr)
library(tidyverse)
data(movielens)

movielens_long <- movielens %>%
  separate_rows(genres,sep = "\\|")
movielens_long

gens <- unique(movielens_long$genres)
ratings <- numeric(length(gens))
amount <- numeric(length(gens))
i <-1
for (gen in gens){
  ratings[i] <- movielens_long %>%
    filter(genres == gen) %>%
    summarise(mean = mean(rating)) %>%
    pull(mean)
  amount[i] <- sum(movielens_long$genres == gen)
  i <- i + 1
}

gen_rating <- data.frame(gender = gens, ratings = ratings, amount = amount)

gen_rating %>%
  mutate(gender = reorder(gender, ratings)) %>%
  ggplot(aes(ratings, gender, size = amount)) +
  labs(title="Valoracion por Genero Cinematográfico",
       subtitle = "El tamaño del punto representa la cantidad de valoraciones registradas",
       x = 'Valoracion',
       y = 'Genero',
       size = 'Cantidad de valoraciones') +
  geom_segment(aes(x = 3.3,
      xend = ratings,
      y = gender,
      yend = gender),
    color = "gray80",
    linewidth = 0.2) +
  geom_point() +
  scale_x_continuous(limits = c(min(gen_rating$ratings) - 0.05, 4)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )