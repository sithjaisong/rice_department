# multiple choice

#### seed sourse ####

dat3 <- readxl::read_excel("C:\\Users\\user\\Documents\\GitHub\\rice_department\\survey_wannapan_eds.xls", sheet = 3)

dat3 <- dat3 %>% mutate(seed.sourse1 = as.factor(seed.sourse1),
                seed.sourse2 = as.factor(seed.sourse2),
                seed.sourse3 = as.factor(seed.sourse3),
                recom1 = as.factor(recom1),
                recom2 = as.factor(recom2),
                recom3 = as.factor(recom3),
                recom4 = as.factor(recom4),
                recom5 = as.factor(recom5))

dat3$province <- revalue(dat3$province, c("NKY" = "Nakorn Nayok", "PCR" = "Prachin Buri"))

seed.sourse1 <- dat3 %>% group_by(province, seed.sourse1) %>% select(seed.sourse1) %>% summarise(count.1 = n()) %>% as.data.frame()

seed.sourse2 <- dat3 %>% group_by(province, seed.sourse2) %>% select(seed.sourse2) %>% summarise(count.2 = n()) %>% as.data.frame()

seed.sourse3 <- dat3 %>% group_by(province, seed.sourse3) %>% select(seed.sourse3) %>% summarise(count.3 = n()) %>% as.data.frame()

seed.all <- left_join(seed.sourse1, seed.sourse2, c("province" = "province", "seed.sourse1" = "seed.sourse2")) %>% 
  left_join(., seed.sourse3, c("province" = "province", "seed.sourse1" = "seed.sourse3"))

seed.all[is.na(seed.all)] <- 0

seed.source <- seed.all %>% filter(seed.sourse1 != "0") 


seed.source$seed.sourse1 <- revalue(seed.source$seed.sourse1, c("1" = "farmer saved seeds",
                                                                "2" = "comunity rice seed center",
                                                                "3" = "seed distributer",
                                                                "4" = "government agencies",
                                                                "5" = "neighborhood"))

seed.source %>% filter(province == "Nakorn Nayok") %>% group_by(province, seed.sourse1) %>% summarise(total = count.1 + count.2 + count.3) %>%
  ggplot(aes(x = seed.sourse1, y = total)) + geom_bar(stat = "identity")  + 
  theme(axis.text.x=element_text(angle=325, hjust=0, vjust = 1)) + labs(x = "seed source", y = "No. of farmers")
  

#### recom ####
dat3$recom_yn <- NULL


dat3$recom_yn <- ifelse(dat3$recom1 == "0", "No", "Yes")

dat3 %>% ggplot() + geom_bar(aes(x=recom_yn))


recom1 <- dat3 %>% group_by(province, recom1) %>% select(recom1) %>% summarise(count.1 = n()) %>% as.data.frame()

recom2 <- dat3 %>% group_by(province, recom2) %>% select(recom2) %>% summarise(count.2 = n()) %>% as.data.frame()

recom3 <- dat3 %>% group_by(province, recom3) %>% select(recom3) %>% summarise(count.3 = n()) %>% as.data.frame()

recom.all <- left_join(recom1, recom2, c("province" = "province", "recom1" = "recom2")) %>% 
  left_join(., recom3, c("province" = "province", "recom1" = "recom3"))

recom.all[is.na(recom.all)] <- 0

recom.part1 <- recom.all %>% filter(recom1 == 0) %>% group_by(province, recom1)%>%summarise(total = count.1)  %>% as.data.frame()

recom.part1

recom.part2 <- recom.all %>% filter(recom1 != 0) %>% group_by(province, recom1) %>% 
  summarise(total = count.1 + count.2 + count.3) %>% as.data.frame()
recom.part2

recom.total <- rbind(recom.part1, recom.part2)
recom.total

recom.total$recom <- NULL


recom.total$recom <- ifelse(recom.total$recom == "0", "No", "Yes")

recom.total %>% ggplot() + geom_bar(aes(x=recom))


recom.total$recom1 <- revalue(recom.total$recom1, c("0" = "no",
                                                      "1" = "negihbor",
                                                      "2" = "chemical store",
                                                      "3" = "chemical applicater",
                                                      "4" = "smart farmer",
                                                      "5" = "sales",
                                                      "6" = "others"))

recom.total %>% filter(province == "Nakorn Nayok") %>% group_by(recom1) %>% 
  ggplot(aes(x = reorder(recom1, total), y = total)) + geom_bar(stat = "identity") +
  labs(y = "No of farmers", x = "recommendation") +
  theme(axis.text.x=element_text(angle=325, hjust=0, vjust = 1))

ggsave("recom_source_NKY.jpg")

recom.total %>% filter(province == "Prachin Buri") %>% group_by(recom1) %>% 
  ggplot(aes(x = reorder(recom1, total), y = total)) + geom_bar(stat = "identity") +
  labs(y = "No of farmers", x = "recommendation") +
  theme(axis.text.x=element_text(angle=325, hjust=0, vjust = 1))

 ggsave("recom_source_PCR.jpg")

