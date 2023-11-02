x <- list('magrittr','ggplot2','dplyr','readxl','arrow','tidyverse','qdap','rdrr','writexl','stringr','scales')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})

path = "D:\\Purdue\\research\\sensitivity_analysis\\"


process_data <- function(input1, input2, output) {
  data1 <- read.csv(paste0(path, input1))
  data2 <- read.csv(paste0(path, input2))
  
  data2 <- data2 %>%
    filter(!grepl("CDP, CA", PlaceName)) %>%
    mutate(PlaceName = gsub(" city, CA", "", PlaceName),
           PlaceName = gsub(", CA", "", PlaceName))
  
  merged_data <- merge(data1, data2, by.x = "Geographic.Area.Name", by.y = "PlaceName")
  merged_data$Total <- merged_data$LandSQMI
  
  merged_data <- merged_data[, -c(13:17)]
  merged_data <- merged_data[, c(2, 1, 3:ncol(merged_data))]
  write.csv(merged_data, paste0(path, output), row.names = FALSE)
}
process_data("pop2010.csv", "geocorr2018_2327808729.csv", "mile2018.csv")
process_data("pop2020.csv", "geocorr2022_2327806627.csv", "mile2022.csv")

