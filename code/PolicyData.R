x <- list('magrittr','ggplot2','dplyr','readxl','arrow','plyr','tidyverse','qdap','rdrr','writexl')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})

unincorporated <- read.csv("D:\\Purdue\\research\\Unincorporated Zip codes - population_3.3.23.csv")
unincorporated <- unincorporated[-1,]
unincorporated$CountyName <- gsub(".{3}$", "", unincorporated$CountyName)  # remove the last 3 characters
colnames(unincorporated) <- c("bestzip","countyname","cityname","pop")
unincorporated <- unincorporated[grep("CDP", unincorporated$cityname), ]

fee <- read_excel("D:\\Purdue\\research\\fee.xls")
# fee1 <- gsub("[[:punct:][:blank:]]","", fee$Municipality)
# fee$Municipality <- gsub("[0-9]","", fee$Municipality)
fee$Municipality <- gsub("[[:punct:]]","", fee$Municipality)
fee$Municipality <- gsub("\\d+","", fee$Municipality)
fee$Municipality <- gsub("^\\s+","", fee$Municipality) # first space
fee$Municipality <- gsub("\\s+$","", fee$Municipality) # last space
fee$AnnualFee <- gsub("\\$", "", fee$AnnualFee)
fee$AnnualFee2 <- gsub("[\\/yr*]", "", fee$AnnualFee)
fee <- data.frame(fee[, c("Municipality","County","EnactmentDate","Population","Fee","AnnualFee","AnnualFee2","Application(Initial)Fee")])
fee$AnnualFee2[fee$AnnualFee2 == "No fee set"] <- 0
fee$AnnualFee2[fee$AnnualFee2 == "No fee"] <- 0
fee$AnnualFee2[fee$AnnualFee2 == "No fee planned"] <- 0
fee$AnnualFee2[fee$AnnualFee2 == "Unknown"] <- NA
fee$AnnualFee2[fee$AnnualFee2 == "TBD"] <- 0
fee$AnnualFee2[fee$AnnualFee2 == "Pending"] <- 0
write.csv(fee, "D:\\Purdue\\research\\feee.csv", row.names = FALSE)
# write_xlsx(fee, "D:\\Purdue\\research\\feee.xls")
fee <- read.csv("D:\\Purdue\\research\\feee.csv")
fee <- fee[,-c(4,6)]


policy <- read_excel("D:\\Purdue\\research\\Policydata_2012-21.xlsx")
colnames(policy)[9] <- c("emerging_license")
policy$emerging_license <- as.integer(as.logical(policy$emerging_license))
policy$retailrestrict <- as.integer(as.logical(policy$retailrestrict))
policy$flavored <- as.integer(as.logical(policy$flavored))
sum(is.na(policy$bestzip))
dim(policy[policy$bestzip == 'NA',])
unincorporated[colnames(policy)[5:11]] <- NA
which(is.na(policy$bestzip), arr.ind=TRUE)
merge <- data.frame()
count = 0
for (i in 1:nrow(policy)){
  if (is.na(policy[i,]$bestzip) == 1){
    count <- count + 1
    df <- unincorporated[which(unincorporated$countyname == policy[i,]$countyname),]
    df$year <- rep(policy[i,]$year, times = nrow(df))
    df$smokefree <- rep(policy[i,]$smokefree, times = nrow(df))
    df$citylicense <- rep(policy[i,]$citylicense, times = nrow(df))
    df$pharmacyban <- rep(policy[i,]$pharmacyban, times = nrow(df))
    df$emerging_license <- rep(policy[i,]$emerging_license, times = nrow(df))
    df$retailrestrict <- rep(policy[i,]$retailrestrict, times = nrow(df))
    df$flavored <- rep(policy[i,]$flavored, times = nrow(df))
    merge <- rbind(merge, df)
  }
  else {
    merge <- rbind(merge, policy[i,])
  }
}
merge[which(merge$cityname == 'Glenn'),]
write.csv(merge, "D:\\Purdue\\research\\merge.csv", row.names = FALSE)
merge <- read.csv("D:\\Purdue\\research\\merge.csv")


count(unique(merge[which(merge$countyname == 'Alameda'),]$cityname))
count(unique(fee[which(fee$County == 'Alameda County'),]$Municipality))
table(merge$cityname)
count(merge$cityname)
count(fee$Municipality)
# mergefee <- merge(x = merge, y = fee, by.x = "cityname", by.y = "Municipality", all.x=TRUE)
mergefee <- left_join(merge, fee, by = c("cityname" = "Municipality"))
# mergefee$index <- as.numeric(row.names(mergefee))
# mergefee <- mergefee[order(mergefee$index), ]
mergefee <- mergefee[order(mergefee$countyname, mergefee$year), ]
county <- fee[grep("County", fee$Municipality), ]
for (i in 1:nrow(mergefee)){
  if (str_detect(mergefee[i,]$cityname, 'CDP')){
    if(paste(mergefee[i,]$countyname, "County") %in% county$County){
      mergefee[i,]$County <- county[which(county$County == paste(mergefee[i,]$countyname, "County")),]$County
      mergefee[i,]$EnactmentDate <- county[which(county$County == paste(mergefee[i,]$countyname, "County")),]$EnactmentDate
      mergefee[i,]$Fee <- county[which(county$County == paste(mergefee[i,]$countyname, "County")),]$Fee
      mergefee[i,]$AnnualFee2 <- county[which(county$County == paste(mergefee[i,]$countyname, "County")),]$AnnualFee2
      mergefee[i,]$Application.Initial.Fee <- county[which(county$County == paste(mergefee[i,]$countyname, "County")),]$Application.Initial.Fee
    }
  }
}

mergefee <- mergefee[,-c(12,14)]
colnames(mergefee)[13] <- "AnnualFee"
mergefee <- mergefee[!duplicated(mergefee), ]
mergefee$AnnualFee <- as.numeric(mergefee$AnnualFee)
write.csv(mergefee, "D:\\Purdue\\research\\mergefee.csv", row.names = FALSE)
mergefee <- read.csv("D:\\Purdue\\research\\mergefee.csv")


