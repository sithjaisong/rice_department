#load library

library(readxl)
library(tidyverse)
library(magrittr)


pathogen <- read_xlsx(path = "D:\\Google drive\\DMG_new\\seedpath\\seepath_data\\seed_pathogen_data.xlsx", sheet = 1)

seedstock_list <- read_xlsx(path = "D:\\Google drive\\DMG_new\\seedpath\\seepath_data\\seedstock_list.xlsx", sheet = 2)

#names(seedstock_list)

seedstock_list %<>% select(stock_code, varieties, location)

pathogen_long <- pathogen %>% gather(seedlot, inffection_percent, KL01:SPB13, factor_key=TRUE)

# combine pathgen

pathogen_data_with_profile <- pathogen_long %>% left_join(seedstock_list, c("seedlot" = "stock_code"))


# by seed source
names(pathogen_data_with_profile)

pathogen_data_with_profile %>% group_by(Path, location) %>% summarise(mean(inffection_percent))
