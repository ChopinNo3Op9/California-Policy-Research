x <- list('magrittr','ggplot2','dplyr','readxl','arrow','plyr','tidyverse','qdap','rdrr','writexl','stringr','scales')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})


path <- 'D:\\Purdue\\research\\population_ethnicity\\'

pop2010 <- read.csv(paste0(path, "ACSDP5Y2010.DP05-Data.csv"))
colnames(pop2010) <- pop2010[1,]
pop2010 <- pop2010[-1,]
colnames(pop2010)
pop2010 <- pop2010[,-grep("Margin of Error", colnames(pop2010))]
pop2010 <- pop2010[,-grep("Annotation of", colnames(pop2010))]
pop2010 <- pop2010[,-grep("year", colnames(pop2010))]
pop2010 <- pop2010[,-grep("Male", colnames(pop2010))]
pop2010 <- pop2010[,-grep("Female", colnames(pop2010))]
colnames(pop2010)
pop2010 <- pop2010[, c(1,2,3,grep("Percent", colnames(pop2010)))]
write.csv(pop2010, paste0(path, 'pop2010.csv'), row.names = FALSE)
pop2010 <- read.csv(paste0(path, 'pop2010.csv'))


pop2020 <- read.csv(paste0(path, 'ACSDP5Y2020.DP05-Data.csv'))
colnames(pop2020) <- pop2020[1,]
pop2020 <- pop2020[-1,]
colnames(pop2020)
pop2020 <- pop2020[,-grep("Margin of Error", colnames(pop2020))]
pop2020 <- pop2020[,-grep("Annotation of", colnames(pop2020))]
pop2020 <- pop2020[,-grep("year", colnames(pop2020))]
pop2020 <- pop2020[,-grep("Male", colnames(pop2020))]
pop2020 <- pop2020[,-grep("Female", colnames(pop2020))]
colnames(pop2020)
pop2020 <- pop2020[, c(1,2,3,grep("Percent", colnames(pop2020)))]
write.csv(pop2020, paste0(path, 'pop2020.csv'), row.names = FALSE)
pop2020 <- read.csv(paste0(path, 'pop2020.csv'))

