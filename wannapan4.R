
dat4 <- readxl::read_excel("C:\\Users\\user\\Documents\\GitHub\\rice_department\\survey_wannapan_eds.xls", sheet = 4)



dat4$chem.name[dat4$chem.name == "0.000000"] <- "0"
dat4$chem.name[dat4$chem.name == " pymetrozine"] <- "pymetrozine"
dat4$chem.name[dat4$chem.name == " flubendiamide"] <- "flubendiamide" 
dat4$chem.name[dat4$chem.name == " chlorantraniliprole"] <- "chlorantraniliprole"
dat4$chem.name[dat4$chem.name == " propiconazole+difenoconazole"] <- "propiconazole+difenoconazole"   
dat4$chem.name[dat4$chem.name == " chlorpyrifos+carbosulfan"  ] <- "chlorpyrifos+carbosulfan"  
dat4$chem.name[dat4$chem.name == " pymetrozine+hormone"] <- "pymetrozine+hormone"
dat4$chem.name[dat4$chem.name == "pretilachlo"] <- "pretilachlor"
dat4$chem.name[dat4$chem.name == "indoxacarb "] <- "indoxacarb"
dat4$chem.name[dat4$chem.name == "hormone " ] <- "hormone"
dat4$chem.name[dat4$chem.name == "chlorpyrifos " ] <- "chlorpyrifos"
dat4$chem.name[dat4$chem.name == "chlorantraniliprole "] <- "chlorantraniliprole"
dat4$chem.name[dat4$chem.name == "butachlor+penoxsulam "] <- "butachlor+penoxsulam"
dat4$chem.name[dat4$chem.name == "beauveria " ] <- "beauveria"

dat4$chem.name[dat4$chem.name == "0"  ] <- "Not apply"  

dat4 <- dat4 %>% dplyr::mutate(province = as.character(province),
                               stage  = as.factor(stage),
                               chem.name = as.factor(chem.name)
)
levels(dat4$chem.name)
#dat4$chem.name <- 

factor(dat4$chem.name, levels(dat4$chem.name)[c(78,1:77, 79:121)])
levels(dat4$chem.name)

dat4$stage <-  revalue(dat4$stage, c("1" = "seedling",
                                    "2" = "tillering",
                                    "3" = "booting", 
                                    "4" = "flowering",
                                    "5" = "ripening")
                       )

dat4 %>% filter(province == "NKY") %>% ggplot(aes(x = chem.name)) + 
  geom_bar() + facet_grid(~stage, scales="free_x") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust = 0.5)) +labs(x = "chemicals", y = "No of farmers")

ggsave("chem_use_NKY.jpg", width = 25, height = 10, units = "in")

dat4 %>% filter(province == "PCR") %>% ggplot(aes(x = chem.name)) + 
  geom_bar() + facet_grid(~stage, scales="free_x") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust = 0.5)) +labs(x = "chemicals", y = "No of farmers")

ggsave("chem_use_PCR.jpg", width = 25, height = 10, units = "in")
