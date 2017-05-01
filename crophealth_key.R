library(dplyr)
tab <-  src_mysql("crophealth", 
                  host="crophealththailandproj.coctt8gebg7j.us-west-2.rds.amazonaws.com",
                  port=3306, 
                  user = "crophealth",
                  password="crophealth")

