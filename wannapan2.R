library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)

dat2 <- readxl::read_excel("C:\\Users\\user\\Documents\\GitHub\\rice_department\\survey_wannapan_eds.xls", sheet = 2)

dat2$form[dat2$form == "0.000000"] <- "0"
dat2$form[dat2$form == "16-20--0"] <- "16-20-0"  
dat2$form[dat2$form == " 18-8-8"] <- "18-8-8"
dat2$form[dat2$form == "16-20-21"] <- "16-20-20"
dat2$form[dat2$form == " 46-0-0"] <- "46-0-0"
dat2$form[dat2$form == "16-20-00"] <- "16-20-0"

unique(dat2$form)

dat2 <- dat2 %>% dplyr::mutate(province = as.character(province),
                             stage  = as.factor(stage),
                             form = as.factor(form),
                             weight.level = as.character(weight.level)
                             )
levels(dat2$form)



dat2$stage <-  revalue(dat2$stage, c("1" = "15-30 DAS", 
                                                   "2" = "30-50 DAS",
                                                   "3" = "50-60 DAS",
                                                   "4" = "60-120 DAS"))
                       

dat2$weight.level <-  revalue(dat2$weight.level,c("0" = " 0 kg/rai",
                                                  "1" = "< 15 kg/rai", 
                                                  "2" = "15-20 kg/rai", 
                                                  "3" = "20-30 kg/rai",
                                                  "4" = "> 30 kg/rai"))

dat2$fert <- NULL

dat2$fert <- ifelse(dat2$form == 0, "No", "Yes")


dat2 %>% filter(province == "NKY") %>% ggplot(aes(fill = weight.level, x = form)) + 
  geom_bar() + facet_grid(~stage)

dat2 %>% filter(province == "NKY") %>% ggplot(aes(fill = weight.level, x = fert)) + 
  geom_bar() + facet_grid(~stage)

