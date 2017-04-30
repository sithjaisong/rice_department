library(XLConnect)
library(plyr)
library(tidyverse)
library(ggplot2)
library(vegan)
library(iNEXT)

wb <- loadWorkbook(filename = "C:\\Users\\user\\Documents\\GitHub\\rice_department\\NKY_BPH_2.xlsx")

lst <- readWorksheet(wb, sheet = getSheets(wb))

lst$Sheet1 <- NULL

dat_all <- rbind.fill(lst)
dat_all[is.na(dat_all)] <- 0
head(dat_all)
n <- length(unique(dat_all$species))
sp <- unique(dat_all$species)

tidy_data <- dat_all %>% gather(key = rep, value = abundent, R1.value, R2.value, R3.value)

tidy_data %>% filter(species == sp[1]) %>% ggplot(aes(x= abundent)) + geom_dotplot() + facet_grid(treatment~time)

ggplot(data = tidy_data, aes(x = species, y = abundent)) + geom_boxplot() + facet_wrap(~treatment)

species_data <- tidy_data %>% spread(key = species, value = abundent)

id_species_data <- species_data %>% unite_(col = "id", c("time", "treatment", "rep"), remove = FALSE)

id_species_data$shnn <- diversity(id_species_data[5:46], index = "shannon")
id_species_data$sms <- diversity(id_species_data[5:46], index = "simpson")
id_species_data$invs <- diversity(id_species_data[5:46], index = "invsimpson")
id_species_data$fshr <- fisher.alpha(id_species_data[5:46], MARGIN = 1)
id_species_data$spnm <- specnumber(id_species_data[5:46], MARGIN = 1)

tim <- unique(id_species_data$time)
trt <- unique(id_species_data$treatment)
tim[4]

id_species_data %>% ggplot(aes(x = time, y = shnn, fill = factor(treatment))) + 
  geom_boxplot() +theme_classic()


id_species_data %>% filter(time == tim[4]) %>% kruskal.test(shnn ~ treatment, data = .)
id_species_data %>% filter(time == tim[3]) %>% glm(shnn ~ treatment, data = .) %>% summary()

time_four <- id_species_data %>% filter(time == tim[4]) %>% select(3, 5:46) 
out <- iNEXT(time_four, q=0, datatype="abundance")
g <- ggiNEXT(out, type=1, color.var = "site")
g
r
id_species_data %>% ggplot(aes(x = time, y = sms, fill = factor(treatment))) + 
  geom_boxplot() +theme_classic()

id_species_data %>% ggplot(aes(x = time, y = fshr, fill = factor(treatment))) + 
  geom_boxplot() +theme_classic()


#===rarecure
trt_one <- id_species_data %>% filter(time == tim[4]) %>% filter(treatment == trt[1]) %>% select(3, 5:46) %>% specaccum(., 'random')
trt_two <- id_species_data %>% filter(time == tim[4]) %>% filter(treatment == trt[2]) %>% select(3, 5:46) %>% specaccum(., 'random')
trt_three <- id_species_data %>% filter(time == tim[4]) %>% filter(treatment == trt[3]) %>% select(3, 5:46) %>% specaccum(., 'random')
trt_four <- id_species_data %>% filter(time == tim[4]) %>% filter(treatment == trt[4]) %>% select(3, 5:46) %>% specaccum(., 'random')
trt_five <- id_species_data %>% filter(time == tim[4]) %>% filter(treatment == trt[5]) %>% select(3, 5:46) %>% specaccum(., 'random')
trt_six <- id_species_data %>% filter(time == tim[4]) %>% filter(treatment == trt[6]) %>% select(3, 5:46) %>% specaccum(., 'random')

plot(trt_one, ci.type = "polygon", ci.col = "yellow", lty =3, ci.lty = 2)

plot(trt_one)
# random modification to BCI data to create data for a second curve
BCI2 <- as.matrix(BCI)
BCI2[sample(prod(dim(BCI2)), 10000)] <- 0
sp2 <- specaccum(BCI2, 'random')



# Combine the specaccum objects into a list 
l <- list(trt_one, trt_two, trt_three, trt_four, trt_five, trt_six) 

# Calculate required y-axis limits
ylm <- range(sapply(l, '[[', 'richness') + 
               sapply(l, '[[', 'sd') * c(-2, 2))

# Apply a plotting function over the indices of the list
sapply(seq_along(l), function(i) {
  if (i==1) { # If it's the first list element, use plot()
    with(l[[i]], {
      plot(sites, richness, type='l', ylim=ylm, 
           xlab='Sites', ylab='random', las=1)
      segments(seq_len(max(sites)), y0=richness - 2*sd, 
               y1=richness + 2*sd)
    })    
  } else {
    with(l[[i]], { # for subsequent elements, use lines()
      lines(sites, richness, col=i)
      segments(seq_len(max(sites)), y0=richness - 2*sd, 
               y1=richness + 2*sd, col=i)
    })     
  }
})

legend('bottomright', c('Site 1', 'Site 2', 'Site3'), col=1:6, lty=1, 
       bty='n', inset=0.025)
