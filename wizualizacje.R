library(ggplot2)
library(hrbrthemes)
library(plotly)
library(ISLR) # for Credit data
data("Credit")


#utwórz wykres ratingu
Credit %>%
  filter(Age>30) %>%
  ggplot(aes(x=Rating, fill=Gender)) + 
  geom_histogram(binwidth=50, color="black") +
  labs(title="Histogram ratingu", x="Rating", y="Liczba obserwacji") +
  theme_modern_rc() +
  facet_grid(~Married)
  
# utwórz wykres ratingu (boxplot)
wykres <- ggplot(Credit, aes(x=Married, y=Rating, fill=Gender)) +
  geom_violin(alpha=0.2) +
  geom_boxplot() +
  labs(title="Boxplot ratingu", x="Stan cywilny", y="Rating") +
  theme_classic() + 
  facet_grid(~Ethnicity) +
  coord_flip()

ggplotly(wykres)

# Podziel zmienną Age na 3 przedziały wiekowe:
Credit$AgeGroup <- cut(Credit$Age, breaks=c(20,40,60,100),
                       labels=c("20-40", "40-60", "60-100"))

# Podziel rating na przedziałki:
Credit$RatingGroup <- cut(Credit$Rating, breaks=c(0,200,400,600,800,1000), 
                          labels=c("0-200", "200-400", "400-600", "600-800", "800-1000"))

# Utwórz wykres dla liczby kard (Cards) w zależności od grupy wiekowej (AgeGroup):

ggplot(Credit, aes(x=AgeGroup, fill=RatingGroup)) + 
  geom_bar(position="stack") +
  labs(title="Liczba kart w zależności od grupy wiekowej", x="Grupa wiekowa", y="Liczba kart") +
  theme_classic()