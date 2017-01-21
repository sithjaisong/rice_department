library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)

dat <- readxl::read_excel("C:\\Users\\user\\Documents\\GitHub\\rice_department\\survey_wannapan_eds.xls", sheet = 1)

str(dat)

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
dat$sex <-  revalue(dat$sex, c("1" = "male", "2" = "female"))
#dat$edu.level <-  revalue(dat$edu.level, c("1" = "elementary school", 
#                                           "3" = "junior high school",
#                                           "2" = "senior high school",
#                                           )

dat %>% ggplot() + geom_bar(aes(x = province))
dat %>% ggplot() + geom_bar(aes(x = sex)) + facet_grid(~ province)


dat %>% filter(province == "Nakorn Nayok") %>% ggplot(aes(x = sex))+ geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +  labs(title = "Nakorn Nayok", y = "Percent") + theme(plot.title = element_text(hjust = 0.5))

dat %>% filter(province == "Prachin Buri") %>% ggplot(aes(x = sex)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +  labs(title = "Prachin Buri", y = "Percent") + theme(plot.title = element_text(hjust = 0.5))

dat %>% filter(yield > 50)


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

dat %>% filter(province == "Nakorn Nayok") %>%  ggplot(aes(x = ces, fill = ces)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +  labs(title = "Nakorn Nayok", y = "Percent") + theme(plot.title = element_text(hjust = 0.5))

dat %>% filter(province == "Prachin Buri") %>%  ggplot(aes(x = ces, fill = ces)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +  labs(title = "Prachin Buri",x = "Crop establishment method", y = "Percent") + theme(plot.title = element_text(hjust = 0.5))



