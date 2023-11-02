x <- list('magrittr','ggplot2','plyr','readxl','arrow','tidyverse','qdap','rdrr','writexl','data.table','haven',"foreign")
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})


path <- 'D:\\Purdue\\research\\'
dtabind <- data.frame(matrix(1:9, ncol = 9))
readdta <- function(year) {
  read <- read.dta(paste0(path, "TEEN_with_format-", year, ".dta"))
  read$dtayear <- year
  assign(paste0("dta", year), read, env = .GlobalEnv)
}
for (i in 2012:2021) {
  readdta(i)
}

dtabind <- rbind.fill(dtabind, dta2012, dta2013, dta2014, dta2015, dta2016, dta2017, dta2018, dta2019, dta2020, dta2021)[-1, -c(1:9)]
write.csv(dtabind, paste0(path, 'dtabind.csv'), row.names = FALSE)
dtabind <- read.csv(paste0(path, 'dtabind.csv'))

mergefee <- read.csv(paste0(path, 'UniqueMergefee.csv'))
dtabind_mergefee <- merge(x = mergefee, y = dtabind, by.x = c('bestzip', 'year'), by.y = c('bestzip', 'dtayear'), all.x = FALSE)
nrow(dtabind[dtabind$bestzip == 90001 & dtabind$dtayear == 2012, ]) # each diff dtabind merge to a single mergefee

write.csv(dtabind_mergefee, paste0(path, 'dtabind_mergefee.csv'), row.names = FALSE)
dtabind_mergefee <- read.csv(paste0(path, 'dtabind_mergefee.csv'))



