library(ggplot2)
library(dplyr)
library(multcompView)

data <- read.csv("Antibac+blast.csv")

str(data)
data$X <- NULL
data$Rep <- as.factor(data$Rep)
mod1 <- aov(X.inhibition ~ Bac, data)
Tukey <- TukeyHSD(mod1)

multcompLetters2(X.inhibition ~ Bac, Tukey$Bac[, 4], data)

multcompBoxplot(X.inhibition ~ Bac, data)

multcompLetters2(y ~ treatments, exp_tukey$treatments[,"p adj"], experiment)

summary(fm1 <- aov(breaks ~ wool + tension, data = warpbreaks))
TukeyHSD(fm1, "tension", ordered = TRUE)
plot(TukeyHSD(fm1, "tension"))

data %>% group_by(Bac) %>% summarise(xbar = mean(X.inhibition)) %>% arrange(desc())

data %>% ggplot() + geom_boxplot(aes(x = reorder(Bac, X.inhibition, mean) , y = X.inhibition)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

data %>% ggplot() + geom_boxplot(aes(x = reorder(Bac, X.inhibition, mean) , y = X.inhibition)) +  coord_flip() + 
  ylab("Inhibition(%)") + xlab("Isolate")

ggsave("ver_anibac.pdf", width = 5, height = 10)multcompLetters(extract_p(TukeyHSD(mod1)))
