
library(haven)
mydata <- read_dta("Panel101.dta")

mydata$time = ifelse(mydata$year >= 1994, 1, 0)

mydata$treated <- ifelse(mydata$country %in% c(5, 6, 7), 1, 0)

mydata$did = mydata$time * mydata$treated

didreg = lm(y ~ treated + time + did, data = mydata)
summary(didreg)

didreg1 = lm(y ~ treated*time, data = mydata)
summary(didreg1)

ggplot(mydata, aes(x = time, y = y, color = treated, group = treated)) +
  geom_line() +
  geom_point() +
  labs(title = "Pre-Treatment Trends in y",
       x = "Year", y = "y")

