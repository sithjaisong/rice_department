library(dplyr)
library(agricolae)
data <- read.csv(file = "biop_jin.csv")

data_incidence <- data %>% select(treat, rep, incidence)
mod <- aov(incidence ~ treat, data_incidence)
summary(mod)
boxplot(incidence ~ treat, data_incidence)
multcompBoxplot(incidence ~ treat, data_incidence)
HDS_incidece <- HSD.test(mod, "treat", console=TRUE)


data_sever <- data %>% select(treat, rep, sever)

mod2 <- aov(sever ~ treat, data_sever)
summary(mod2)
HDS_sever <- HSD.test(mod2, "treat", console=TRUE)
multcompBoxplot(sever ~ treat, data_sever)
