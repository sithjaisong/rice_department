#load library

library(readxl)
library(tidyverse)
library(magrittr)


pathogen <- read_xlsx(path = "D:\\Google drive\\DMG_new\\project_seedpath\\seepath_data\\seed_pathogen_data.xlsx", sheet = 1)

seedstock_list <- read_xlsx(path = "D:\\Google drive\\DMG_new\\project_seedpath\\seepath_data\\seedstock_list.xlsx", sheet = 2)

names(seedstock_list)

seedstock_list %<>% select(stock_code, varieties, location)

pathogen_long <- pathogen %>% gather(seedlot, infection_percent, KL01:SPB13, factor_key=TRUE)

# combine pathgen

pathogen_data_with_profile <- pathogen_long %>% left_join(seedstock_list, c("seedlot" = "stock_code"))


# by seed source
#names(pathogen_data_with_profile)

out <- pathogen_data_with_profile %>% group_by(Path, location) %>% summarise(xbar = mean(infection_percent), SEM = std.error(infection_percent))

#out$xbar.char <- as.character(out$xbar)
#out$SEM.char <- as.character(out$SEM)
# create a new column `x` with the three columns collapsed together
output  <- unite(out, sum, -c(1,2), sep = "+/-" ) %>% spread(Path, sum)

write_excel_csv(as.data.frame(output), path = "D:\\Google drive\\DMG_new\\project_seedpath\\seepath_data\\output_seedpath.csv")


