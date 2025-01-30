install.packages("ggstatsplot")
install.packages("tidyverse")

library(ggstatsplot)
library(tidyverse)
library(RColorBrewer)
library(rlang)

Buy <- read.csv("cleanBuy.csv")
Rent <- read.csv("cleanRent.csv")

# custom_palette <- colorRampPalette(brewer.pal(9, "Set3"))(15)

Buy %>% 
  ggbetweenstats(
    y = price,
    x = city,
    bf.message = FALSE,
    pairwise.display = "none",
    title = "Distribution of price across cities"
  )

Rent %>% 
  ggbetweenstats(
    y=price,
    x=city,
    bf.message = FALSE,
    pairwise.display = "none",
    title = "Distribution of price length across cities"
  )

#todo: statistical significance between Amentatiies

#amentities <- c("hasBalcony", "hasElevator", "hasParkingSpace", "hasStorageRoom", "hasSecurity")
#for (i in amentities) {
  Buy %>%
    ggbetweenstats(
      y = price,
      x = hasBalcony,
      bf.message = FALSE,
      palette = "Dark2",
      title = "Price Distribution by Amenities"
    )
#}
  Buy %>%
    ggbetweenstats(
      y = price,
      x = hasBalcony,
      bf.message = FALSE,
      palette = "Dark2",
      title = "Price Distribution by Amenities"
    )

#todo: statistical significance between condition
  Rent %>%
  ggbetweenstats(
    y = price,
    x = condition,
    bf.message = FALSE,
    palette = "Dark2",
    title = "Price difference by condition"
  )

#todo: Anova
