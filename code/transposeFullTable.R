x <- list('magrittr','ggplot2','dplyr','readxl','arrow','tidyverse','qdap','rdrr','writexl','stringr','scales')
lapply(x, FUN = function(X) {
  do.call("require", list(X))
})


fulltable <- read_excel("D:\\Purdue\\research\\Full table.xls")
t <- fulltable[,c(1,2,3,8,9)]
t <- na.omit(t)

u <- data.frame(unique(t[,1:3]))
transpose <- data.frame(matrix(1:9, ncol = 9))
for (i in 1:nrow(u)){
  trans <- cbind(u[i,], data.frame(t(t[t$Year == u[i,1] & t$Quarter == u[i,2] & t$`Location Description` == u[i,3], 5])))
  colnames(trans) <- colnames(transpose)
  transpose <- rbind(transpose, trans)
}
transpose <- transpose[-1,]
colnames(transpose) <- c("Year","Quarter","Location","License Required","Renewal Required","Renewal Frequency (Years)",
                         "License Fee Required","Minimum License Fee ($)","Maximum License Fee ($)")
t[t$Year == 2004 & t$Quarter == 4 & t$`Location Description` == "Utah",]

write.csv(transpose, "D:\\Purdue\\research\\transposeFulltable.csv", row.names = FALSE)
transpose <- read.csv("D:\\Purdue\\research\\transposeFulltable.csv")



