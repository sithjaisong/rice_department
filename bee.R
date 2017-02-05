# gm_center
library(XLConnect)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)
library(PerformanceAnalytics)

gm_center <- loadWorkbook("gm_center.xls", create = TRUE)

sheet1 <- readWorksheet(gm_center, sheet = 5, startRow = 3, endRow = 202, startCol = 1, header = FALSE)
sheet1 
sheet1 <- as.data.frame(sheet1)

 as.character(sheet1[1,])
 
 
 
 gm_center.list <- readWorksheet(gm_center, sheet = c(1:12), startRow = 2, 
                                  endRow = 202, startCol = 1, endCol = 29)
  
 
 gm_center.list[5]

gm_center.all <- rbindlist(gm_center.list)

gm_center.all[,16] <- NULL

colnames(gm_center.all) <- c("var", "date", "age", "sample", "silvershoot", "dwaft", 
                             "nomalplant", "gallmidge", "nym_gall", "spider", 
                             "mirid", "long_crick", "roove_beetle", "ladybird",
                             "ass_bug", "ground_beetle", "temp_c",
                             "RH", "chelisoches", "WPH", "BPH", "GLH", "ZZ", 
                             "SB","WM", "Rhis", "rice_grasshopper", "LF" )

gm_center.all <- as.data.frame(gm_center.all)

# fill up NA with 0

gm_center.all[is.na(gm_center.all)] <- 0

gm_center.all <- gm_center.all %>% mutate(var = as.factor(var),
                         date = as.POSIXct(date),
                         age = as.numeric(age),
                         sample = as.numeric(sample),
                         silvershoot = as.numeric(silvershoot),
                         dwaft = as.numeric(dwaft),
                         nomalplant = as.numeric(nomalplant),
                         gallmidge = as.numeric(gallmidge),
                         nym_gall = as.numeric(nym_gall),
                         spider = as.numeric(as.character(spider)),
                         mirid = as.numeric(as.character(mirid)),
                         long_crick = as.numeric(long_crick),
                         ground_beetle = as.numeric(ground_beetle),
                         temp_c = as.numeric(temp_c),
                         RH = as.numeric(RH),
                         chelisoches = as.numeric(chelisoches),
                         WPH = as.numeric(WPH),
                         GLH = as.numeric(GLH),
                         ZZ = as.numeric(ZZ),
                         SB = as.numeric(SB),
                         WM = as.numeric(WM),
                         Rhis = as.numeric(Rhis),
                         rice_grasshopper = as.numeric(rice_grasshopper),
                         LF = as.numeric(LF))



mean_gm_center <- gm_center.all %>% dplyr::group_by(var, date) %>% summarise_each(funs = "mean", silvershoot:LF)

long_mean_gm_center <- mean_gm_center  %>% filter(temp_c < 50) %>% gather(variable, value, -var, -date)

long_mean_gm_center$value[is.na(long_mean_gm_center$value)] <- 0

unique(long_mean_gm_center$variable)

long_mean_gm_center %>% ggplot() + geom_line(aes(x= date, y = value)) + 
  facet_grid(variable~. , scales = "free_y")

#[1] "silvershoot"      "dwaft"            "nomalplant"       "gallmidge"        "nym_gall"        
#[6] "spider"           "mirid"            "long_crick"       "roove_beetle"     "ladybird"        
#[11] "ass_bug"          "ground_beetle"    "temp_c"           "RH"               "chelisoches"     
#[16] "WPH"              "BPH"              "GLH"              "ZZ"               "SB"              
#[21] "WM"               "Rhis"             "rice_grasshopper" "LF" 

long_mean_gm_center %>% filter(variable == c("silvershoot", "roove_beetle", "ground_beetle", "GLH", "SB", "LF")) %>% 
  ggplot() + geom_line(aes(x= date, y = value)) + facet_grid(variable~. , scales = "free_y")

#chart.Correlation(mean_gm_center[,c(4,26:27)], histogram=TRUE, pch=19, method = "spearman")

# try_ seasonality

mean_gm_center$year <- year(mean_gm_center$date)

mean_gm_center$month <-month(mean_gm_center$date)

mean_gm_ptt1 <- mean_gm_center %>% filter(var == "PTT1")

silver_ptt1 <- mean_gm_ptt1 %>% ungroup() %>% select(year, month, silvershoot) 

silver_ptt1$silvershoot


#### time series analysis ####

time.ts <- silver_ptt1 %>% group_by(year, month)%>% summarise(silvershoot_mean = mean(silvershoot))%>% spread(month, silvershoot_mean)

time.ts[is.na(time.ts)] <- 0


silvershoottimeseries <- ts(c(silver_ptt1$silvershoot,silver_ptt1$silvershoot), frequency=12, start=c(2014,1))

silvertimeseriescomponents <- decompose(silvershoottimeseries)
plot(silvertimeseriescomponents)

mean_gm_center[is.na(mean_gm_center)] <- 0


#### correlation analysis ###
silver_weather <- mean_gm_center %>% select(temp_c, RH, silvershoot) %>% filter(temp_c < 50, RH >0)

silver_weather$silver_found <- ifelse(silver_weather$silvershoot == 0, "no", "yes")


fit <- rpart(as.factor(silver_found) ~ temp_c + RH,
             data = silver_weather, 
             method="anova")
print(fit)
plotcp(fit) 
summary(fit)

plot(fit)

fit <- ctree(silver_found ~ temp_c + RH, 
             data = silver_weather)

plot(fit, main="Conditional Inference Tree for Kyphosis")
