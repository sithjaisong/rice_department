#
weat <- loadWorkbook("weather.xls", create = TRUE)

weat.data <- readWorksheet(weat, sheet = 1)

colnames(weat.data) <- c("date", "temp.c", "hm", "rain")

weat.data$rain <- as.numeric(weat.data$rain)

long.weat <- weat.data %>% gather(variable, value, -date)

weat.graph <- long.weat %>% filter(date > 2014-03-25 | date <2015-02-24) %>% ggplot(aes(x = date, y = value)) + geom_line() + theme(legend.position="none") +
  facet_grid(variable~., scales = "free_y") 

names(long.weat)

silvershoot <- long_mean_gm_center %>% filter(variable == c("silvershoot")) %>% dplyr::select(date, variable, value)

names(silvershoot)

selected.long.gm_swing.mean <- as.data.frame(long.gm_swing.mean[,c(1,3,4)])


silvershoot_weather <- rbind(silvershoot[-1], long.weat)
silvershoot_weather <- rbind(silvershoot_weather, selected.long.gm_swing.mean)

str(silvershoot_weather)

#silvershoot_weather$variable <- 

silvershoot_weather$variable <- factor(silvershoot_weather$variable, 
                                        levels  = c("rain","hm","temp.c", "ss.percent", "Tanbeandugdae1", "BGoniozus", "Trichogramma"))


silvershoot_weather$variable <- as.factor(silvershoot_weather$variable)

levels(silvershoot_weather$variable) <- c("rain","hm","temp.c", "silvershoot", "Tanbeandugdae1", "BGoniozus", "Trichogramma", )  
  

rect <- data.frame(xmin= "2014-08-17", xmax = "2014-10-17", ymin = -Inf, ymax=Inf)
rect$xmin <- as.POSIXct(rect$xmin)
rect$xmax <- as.POSIXct(rect$xmax)



silvershoot_weather %>% filter(date > 2014-04-25 | date < 2015-02-24) %>% ggplot(aes(x = date, y = value)) + 
  geom_line()+ theme(legend.position="none") + scale_x_datetime (breaks = date_breaks("months")) +
  facet_grid(variable~., scales = "free_y") 

pdf("silter_weather.pdf")
grid.arrange(weat.graph, silver_center.graph, ncol=1, nrow = 2)
dev.off()
