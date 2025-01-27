install.packages("ggstatsplot")
library(ggstatsplot)
library(tidyverse)
data("sleep")

ggwithinstats(
  data=sleep,
  y=extra,
  x=group)

data("movies_long")

# czy komedie, dramaty i animacje różnią się istotnie ratingiem?

movies_long %>% 
  filter(genre %in% c("Comedy", "Drama", "Animated")) %>% 
  ggbetweenstats(
    y=rating,
    x=genre,
    bf.message = FALSE
  )

wyniki <- aov(rating~genre*mpaa, data=movies_long)
summary(wyniki)

?interaction.plot()
# attach(movies_long)

movies_long_filtered <- filter(movies_long, genre %in% c("Comedy", "Drama", "Animated"))
interaction.plot(movies_long_filtered$genre, 
                 movies_long_filtered$mpaa, 
                 movies_long_filtered$rating)
