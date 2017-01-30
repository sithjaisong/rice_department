file.choose()
library(readxl)
library(PerformanceAnalytics)


dat <- readxl::read_excel(path = "/Users/sithjaisong/Desktop/example1.xlsx", sheet = 1)
head(dat)

# fill up NA with 0

dat[is.na(dat)] <- 0

chart.Correlation(dat[, 3:28], histogram=TRUE, pch=19)
