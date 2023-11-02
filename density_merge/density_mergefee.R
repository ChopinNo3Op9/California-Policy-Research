x <- list('magrittr','ggplot2','dplyr','readxl','arrow','tidyverse','qdap','rdrr','writexl','stringr','scales')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})


path <- 'D:\\Purdue\\research\\density_merge\\'

density_1222 <- read.csv(paste0(path, 'density_1222.csv'))
mergefee <- read.csv(paste0(path, 'UniqueMergefee.csv'))
mergefee <- mergefee[, -c(1, 4)]
for (i in 1:nrow(mergefee)) {
  if (grepl("CDP", mergefee[i,]$cityname, fixed = TRUE)) {
    mergefee[i,]$cityname <- paste(mergefee[i,]$countyname, "Unincorporated")
  }
}
mergefee <- mergefee[!duplicated(mergefee), ]

density_mergefee <- density_1222 %>% left_join(mergefee, 
                                               by = c('City' = 'cityname', 'County' = 'countyname', 'RetailerYear' = 'year'))

columns_to_convert <- c(
  "Total_RacialMinority",
  "Percent.below.poverty.level",
  "Highschool_higher_1824",
  "Highschool_higher_25",
  "Bachelor_higher_1824",
  "Bachelor_higher_25"
)
cleaned_values <- vector("list", length = length(columns_to_convert))
for (col in columns_to_convert) {
  cleaned_values[[col]] <- density_mergefee[[col]]
  cleaned_values[[col]][is.na(cleaned_values[[col]]) | cleaned_values[[col]] == ""] <- NA
  cleaned_values[[col]] <- as.numeric(sub("%", "", cleaned_values[[col]], fixed = TRUE))/100
  density_mergefee[[col]] <- cleaned_values[[col]]
}

write.csv(density_mergefee, paste0(path, 'density_mergefee.csv'), row.names = FALSE)
density_mergefee <- read.csv(paste0(path, 'density_mergefee.csv'))

