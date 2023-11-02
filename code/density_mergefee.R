x <- list('magrittr','ggplot2','dplyr','readxl','arrow','tidyverse','qdap','rdrr','writexl','stringr','scales')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})


density_1222 <- read.csv("D:\\Purdue\\research\\density_1222.csv")
mergefee <- read.csv("D:\\Purdue\\research\\UniqueMergefee.csv")
mergefee <- mergefee[,-c(1,4)]
for (i in 1:nrow(mergefee)) {
  if (grepl("CDP", mergefee[i,]$cityname, fixed = TRUE)) {
    mergefee[i,]$cityname <- paste(mergefee[i,]$countyname, "Unincorporated")
  }
}
mergefee <- mergefee[!duplicated(mergefee), ]

# density_mergefee <- merge(x = density_1222, y = mergefee, 
#                           by.x = c("City","County","RetailerYear"), by.y = c("cityname","countyname","year")) # inner join
density_mergefee <- density_1222 %>% left_join(mergefee, 
                             by=c('City'='cityname', 'County'='countyname', 'RetailerYear'='year'))


columns_to_convert <- c(
  "Total_RacialMinority",
  "Percent.below.poverty.level",
  "Highschool_higher_1824",
  "Highschool_higher_25",
  "Bachelor_higher_1824",
  "Bachelor_higher_25"
)
for (col in columns_to_convert) {
  cleaned_values[is.na(cleaned_values) | density_mergefee[[col]] == ""] <- NA
  cleaned_values <- sub("%", "", density_mergefee[[col]], fixed = TRUE)
  cleaned_values <- as.numeric(cleaned_values)/100
  density_mergefee[[col]] <- cleaned_values
}

write.csv(density_mergefee, "D:\\Purdue\\research\\density_mergefee.csv", row.names = FALSE)
density_mergefee <- read.csv("D:\\Purdue\\research\\density_mergefee.csv")

