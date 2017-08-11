library(ggmosaic)
library(ggplot2)

data(Titanic)
titanic <- as.data.frame(Titanic)
titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))

# without weight, geom_mosaic goes based off counts
ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Survived, Class), fill=Survived), divider=mosaic("h")) +
  theme(axis.text.x=element_text(angle=-25, hjust= .1)) +
  facet_grid(Sex~.)
