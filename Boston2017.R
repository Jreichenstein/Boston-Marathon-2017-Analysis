library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)

res <- read_csv("marathon_results_2017.csv")

#Converting times to seconds
time_to_seconds <- function(x){
  temp <- x %>% hms() %>% period_to_seconds()
}


#leaving Name, Age, M/Fm City, Country, 5k to Official Times, Overall, Gender, Division
cols <- c(3:6,8,11:25)

res_clean <- res[,cols]

#creating tibble of just correlation variables
num_cols <- c(2,6:14,17:19)

res_corr <- res_clean[,num_cols]

#Converting cols from hms to seconds
res_corr[,2:11] <- res_corr[,2:11] %>% sapply(time_to_seconds)

#Making all cols numerica and getting rid of NAs
res_corr <- res_corr %>% sapply(as.numeric) %>% na.omit()


#generating correlation matrix
corrMat <- cor(res_corr)

#Correlation plot
corrplot(corrMat, method = "ellipse", type = "upper")

#Groupoing by country
res_by_country <- group_by(res_clean, Country)

#average ofiicial time per country
avg_time_country <- res_by_country %>% summarise(avg_finish_time = mean(`Official Time`)) %>% arrange(avg_finish_time)

#Only USA results
res_USA <- res_clean %>% filter(Country == "USA")

#group by city
res_by_city <- group_by(res_USA, City)

#How many runners came from each city
runners_by_city <- count(res_by_city) %>% arrange(desc(n))

runners_by_city_top <- runners_by_city[1:10,]

ggplot(runners_by_city_top, aes(x=as.factor(City), y = n)) + geom_bar(stat = "identity") + xlab("City") + ylab("Number of Runners") + ggtitle("Runners in the Boston Marathon", subtitle = "by city") 

#Generating city avg, fastest, and slowest times
avg_time_city <- res_by_city %>% summarise(avg_finish_time = mean(`Official Time`), fastest_finish_time = min(`Official Time`), slowest_finish_time = max(`Official Time`)) %>% arrange(avg_finish_time) %>% arrange(fastest_finish_time)

avg_time_city