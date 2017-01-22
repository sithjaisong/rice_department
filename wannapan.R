library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)

dat <- readxl::read_excel("C:\\Users\\user\\Documents\\GitHub\\rice_department\\survey_wannapan_eds.xls", sheet = 1)

# re-structure

dat <- dat %>% dplyr::mutate(province = as.character(province),
                             sex = as.factor(sex),
                             edu.level = as.factor(edu.level),
                             yield = as.numeric(yield),
                             var.wetseason.1 = as.factor(var.wetseason.1),
                             var.wetseason.2 = as.factor(var.wetseason.2),
                             var.wetseason.3 = as.factor(var.wetseason.3),
                             var.dryseason.1 = as.factor(var.dryseason.1),
                             var.dryseason.2 = as.factor(var.dryseason.2),
                             var.dryseason.3 = as.factor(var.dryseason.3),
                             ces = as.factor(ces))


dat$province <- revalue(dat$province, c("NKY" = "Nakorn Nayok", "PCR" = "Prachin Buri"))
dat$sex <-  revalue(as.factor(dat$sex), c("1" = "male", "2" = "female"))

dat$edu.level <-  revalue(dat$edu.level, c("1" = "elementary school", 
                                           "2" = "junior high school",
                                           "3" = "senior high school",
                                           "4" = "vocational",
                                           "5" = "high vocational",
                                           "6" = "bachelor")
                                           )

dat %>% ggplot(aes(x = province, fill = sex)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes( y = ..count.., label= ..count..), vjust=-1)

dat %>% group_by(province, sex) %>% summarise(number = n()) %>% ggplot(aes(x = province,y = number, fill = sex)) + 
  geom_bar(stat = "identity") + 
  facet_grid(~province, scales="free_x") 


dat %>% filter(province == "Nakorn Nayok") %>% ggplot(aes(x= edu.level)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + ggtitle("Nakorn Nayok") + ylab("Precent") + xlab("Educational level") + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=325, hjust=0, vjust = 1))

ggsave("edu.level_NKY.jpg")

dat %>% filter(province == "Prachin Buri") %>% ggplot(aes(x= edu.level)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +  ggtitle("Prachin Buri") + ylab("Precent") + xlab("Educational level") + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=325, hjust=0, vjust = 1))

ggsave("edu.level_PCR.jpg")

dat %>% filter(province == "Nakorn Nayok") %>% ggplot(aes(x = sex))+ geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +  labs(title = "Nakorn Nayok", y = "Percent") + theme(plot.title = element_text(hjust = 0.5))

dat %>% filter(province == "Prachin Buri") %>% ggplot(aes(x = sex)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +  labs(title = "Prachin Buri", y = "Percent") + theme(plot.title = element_text(hjust = 0.5))

## Yield

dat %>% ggplot(aes(x= province, y = yield)) + geom_boxplot(aes(color = province)) + xlab("Province") + ylab("Yield (ton/rai)") + guides(color = F)

ggsave("yield_data.jpg")

df2 <- data_summary(dat, varname="yield", 
                    groupnames=c("province"))

# Convert dose to a factor variable
df2$province = as.factor(df2$province)
head(df2)

df2 %>% ggplot(aes(x = province, y = yield, fill = province))+ geom_bar(stat = "identity")  + geom_errorbar(aes(ymin=yield-sd, ymax=yield+sd), width=.2) +  labs(y = "Average yield ha/rai")

#### rice varities
table(dat$var.dryseason.1)

var.table <- table(dat$var.dryseason.1)

#### CES

dat %>% filter(province == "Nakorn Nayok") %>%  ggplot(aes(x = ces)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +  
  labs(title = "Nakorn Nayok",
       x = "Crop establishment method",
       y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("CES_NKY.jpg")

dat %>% filter(province == "Prachin Buri") %>%  ggplot(aes(x = ces)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +  labs(title = "Prachin Buri",
                                               x = "Crop establishment method", 
                                               y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("CES_PCR.jpg")

#### pest ####


dat$pest[dat$pest == "0.000000"] <- "0"
dat$pest[dat$pest == " leaffolder"] <- "leaffolder"
dat$pest[dat$pest == "rat "] <- "rat"
dat$pest[dat$pest == " rat"] <- "rat"
dat$pest[dat$pest == "brown planthopper "] <- "brown planthopper"
dat$pest[dat$pest == " rice thrips"] <- "rice thrip"
dat$pest[dat$pest == " stem borer"] <- "stem borer"
dat$pest[dat$pest == " rice blast" ] <- "rice blast" 
dat$pest[dat$pest == " stem borer"] <- "stem borer"

dat$pest.yest.no <- NULL
dat$pest.yest.no <- ifelse(dat$pest == "0", "no", "yes")


dat %>% filter(province == "PCR") %>% ggplot(aes(x = pest.yest.no)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +  labs(title = "Prachin Buri",
                                               x = "Rice pest awareness", 
                                               y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))



ggsave("pest_awareness_PCR.jpg")


dat %>% filter(province == "NKY")%>% ggplot(aes(x = pest.yest.no)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +  labs(title = "Nakorn Nayok",
                                               x = "Rice pest awareness", 
                                               y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("pest_awareness_NKY.jpg")

table(dat$province, dat$pest.yest.no)


chisq.test(dat$province, dat$pest.yest.no, correct = T)

unique(dat$pest)

# NKY
dat %>% filter(province == "Nakorn Nayok", pest != 0) %>% ggplot(aes(x = pest)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25)  + 
  labs(title = "Nakorn Nayok", x = "Pest awareness", y = "Percent") +
  theme(axis.text.x = element_text(angle=325, hjust=0, vjust = 1), plot.title = element_text(hjust = 0.5))

ggsave("pest_NKY.jpg")

dat %>% filter(province == "Prachin Buri",  pest != 0) %>% ggplot(aes(x = pest)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25)  + 
  labs(title = "Prachin Buri", x = "Pest awareness", y = "Percent") +
  theme(axis.text.x = element_text(angle=325, hjust=0, vjust = 1), plot.title = element_text(hjust = 0.5))

ggsave("pest_PCR.jpg")
