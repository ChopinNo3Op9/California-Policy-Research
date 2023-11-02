x <- list('magrittr','ggplot2','dplyr','readxl','arrow','plyr','tidyverse','qdap','rdrr','writexl','stringr','scales')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})


mergefee <- read.csv("D:\\Purdue\\research\\mergefee.csv")
mergefee <- mergefee[!is.na(mergefee$bestzip),]

uniquemergefee <- function(year){
  mergefeeyear <- mergefee[mergefee$year == year,]
  unique_bestzip <- unique(mergefeeyear$bestzip)
  UniqueMergefee <- data.frame()
  for (i in unique_bestzip) {
    sub <-  mergefeeyear[mergefeeyear$bestzip == i,]
    if (length(which(mergefeeyear$bestzip == i)) == 1){
      UniqueMergefee <- rbind(UniqueMergefee, sub)
    } else if (nrow(sub) - sum(grepl("CDP", sub$cityname)) == 1) {  # if exist city
      UniqueMergefee <- rbind(UniqueMergefee, sub[which(grepl("CDP", sub$cityname) == F),])
    } else {
      UniqueMergefee <- rbind(UniqueMergefee, sub[which(sub$pop == max(sub$pop)),])
    }
  }
  assign(paste0("unique",year), UniqueMergefee, env=.GlobalEnv)
}

for (i in (2012:2021)){
  uniquemergefee(i)
}
length(unique(unique2013$bestzip))
length(unique(unique2021$bestzip))
UniqueMergefee <- rbind.fill(unique2012,unique2013,unique2014,unique2015,unique2016,unique2017,unique2018,unique2019,unique2020,unique2021)



write.csv(UniqueMergefee, "D:\\Purdue\\research\\UniqueMergefee.csv", row.names = FALSE)
UniqueMergefee <- read.csv("D:\\Purdue\\research\\UniqueMergefee.csv")

