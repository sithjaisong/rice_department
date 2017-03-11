#
gm_swing <- loadWorkbook("gm_swing.xls", create = TRUE)

gm_swing.all <- readWorksheet(gm_swing, sheet = 13, startRow = 5, 
                              endRow = 285, startCol = 1, endCol = 29)
gm_swing.list <- readWorksheet(gm_swing, sheet = c(1:12), startRow = 4, 
                                endRow = 202, startCol = 1, endCol = 29)


gm_swing.all <- rbindlist(gm_swing.list)

colnames(gm_swing.all) <- c("var", "date", "age", "rep","blank", "Trichogramma", 
                            "BGoniozus", "Tanbeandugdae1", "Tanbeandugdae2", "spider",
                            "drogonfry", "mirid", "longbushcrick", "longcricket", 
                            "orangeladybird","rovebeetle","ladybird", "assabug",
                            "groundbeetle","earwing","wph","bph" ,"glh","zz",
                            "stemborrer","whorlmaggot" ,"ricehispa", 
                            "shortcricket", "leaffolder")



gm_swing.all$blank <- NULL

gm_swing.all <- gm_swing.all %>% mutate(dragonfry = as.numeric(drogonfry),
                                        mirid = as.numeric(mirid),
                                        longbushcrick = as.numeric(longbushcrick),
                                        longcricket = as.numeric(longcricket),
                                        orangeladybird = as.numeric(orangeladybird),
                                        rovebeetle = as.numeric(rovebeetle),
                                        ladybird = as.numeric(ladybird),
                                        assabug = as.numeric(assabug),
                                        groundbeetle = as.numeric(groundbeetle),
                                        earwing = as.numeric(earwing))

gm_swing.all[is.na(gm_swing.all)] <- 0


gm_swing.mean <- gm_swing.all %>% group_by(date, age) %>% summarise_each(funs = "mean")


gm_swing.mean
gm_swing.mean$rep <- NULL

long.gm_swing.mean <- gm_swing.mean %>% dplyr::select(date, age, Trichogramma, BGoniozus, Tanbeandugdae1) %>% 
  gather(variable, value, -date, -age)

str(long.gm_swing.mean)
#long.gm_swing.mean <- as.data.frame(long.gm_swing.mean)

long.gm_swing.mean$date  <- as.Date(long.gm_swing.mean$date)
long.gm_swing.mean %>%  ggplot(aes(x = date, y = value)) + geom_line() + facet_grid(variable~., scales = "free_y") 

long.gm_swing.mean[is.na(long.gm_swing.mean)] <- 0

str(long.gm_swing.mean)

long.gm_swing.mean %>%  dplyr::filter(variable == c("Trichogramma", "BGoniozus", 
                                                    "Tanbeandugdae1", 
                                             "Tanbeandugdae2", "spider", "drogonfry", "mirid", 
                                             "longbushcrick", "longcricket",  "orangeladybird","rovebeetle","ladybird", 
                                             "assabug",  "groundbeetle","earwing")) %>% 
                                 ggplot() + geom_line(aes(x= date, y= value)) + 
                                 theme(legend.position="none") +
                                 facet_grid(variable~., scales = "free_y") 



long.gm_swing.mean %>%  
  ggplot() + geom_line(aes(x= date, y= value)) + 
  theme(legend.position="none") +
  facet_grid(variable~., scales = "free_y") 

temp_gm <- mean_gm_center %>% dplyr::select(var, date, silvershoot, temp_c, RH)


