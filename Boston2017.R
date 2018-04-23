library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

res <- read_csv("marathon_results_2017.csv")

#leaving Name, Age, M/Fm City, Country, 5k to Official Times, Overall, Gender, Division
cols <- c(3:6,8,11:25)

res_clean <- res[,cols]

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