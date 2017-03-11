library(ggplot2)
library(dplyr)
library(scales)
library(multcompView)

data <- read.csv("Antibac+blast.csv")
data <- read.csv("Antibac+brownspot.csv")

str(data)
data$X <- NULL
data$Rep <- as.factor(data$Rep)
mod1 <- aov(inhibition ~ Bac, data)
Tukey <- TukeyHSD(mod1)

multcompLetters2(inhibition ~ Bac, Tukey$Bac[, 4], data)

 multcompBoxplot(inhibition ~ Bac, data)

multcompLetters2(y ~ treatments, exp_tukey$treatments[,"p adj"], experiment)

summary(fm1 <- aov(breaks ~ wool + tension, data = warpbreaks))
TukeyHSD(fm1, "tension", ordered = TRUE)
plot(TukeyHSD(fm1, "tension"))

data %>% group_by(Bac) %>% summarise(xbar = mean(X.inhibition)) %>% arrange(desc())

data %>% ggplot() + geom_boxplot(aes(x = reorder(Bac, inhibition, mean) , y = inhibition)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

number_ticks <- function(n) {function(limits) pretty(limits, n)}

data %>% ggplot() + geom_boxplot(aes(x = reorder(Bac, inhibition, mean) , y = inhibition)) +  coord_flip() + 
  ylab("Inhibition(%)") + xlab("Isolate") + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  theme(text = element_text(size=20))




ggsave("fung_vs_brown.pdf", width = 5, height = 10)

multcompLetters(extract_p(TukeyHSD(mod1)))
