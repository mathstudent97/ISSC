library(tidyverse)
library(ggplot2)
library(mgcv)

# load data
jen_comments1 <- 
  read_csv("Desktop/PersonalStatsProj/Datafest/datasets/JenerationDIY/30 Life Hacks Using Coconut Oil.csv")
jen_comments2 <- 
  read_csv("Desktop/PersonalStatsProj/Datafest/datasets/JenerationDIY/Back to School Clothing Haul (Try on) 2018.csv")
jen_comments3 <- 
  read_csv("Desktop/PersonalStatsProj/Datafest/datasets/JenerationDIY/DIY Tumblr Clothes WITHOUT Transfer Paper.csv")
jen_comments4 <- 
  read_csv("Desktop/PersonalStatsProj/Datafest/datasets/JenerationDIY/DIY Tumblr Phone Cases Simple and Quick.csv")
jen_comments5 <- 
  read_csv("Desktop/PersonalStatsProj/Datafest/datasets/JenerationDIY/THRIFT FLIP back to school edition (no boys distracted).csv")
jen_comments <- rbind(jen_comments1,jen_comments2,jen_comments3,jen_comments4,jen_comments5)
rm(jen_comments1,jen_comments2,jen_comments3,jen_comments4,jen_comments5)



# count how many comments were left on each date 
jen_comments_ts <- as.Date(jen_comments$publishedAt)
jen_comments_ts = tibble(date = as.Date(Reduce(c, jen_comments_ts))) %>%
  group_by(date) %>% count()
jen_comments_ts <- jen_comments_ts %>% filter(date >= as.Date('2020-01-01'))

# plot the number of comments by date (exploratory plot)
ggplot(data = jen_comments_ts) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype = 2,color = "red")

# make variables for sin/cos functions and random time smoothing functions
jen_comments_ts$timeInt = as.numeric(jen_comments_ts$date)
jen_comments_ts$sin12 = sin(jen_comments_ts$timeInt/365.25)
jen_comments_ts$cos12 = cos(jen_comments_ts$timeInt/365.25)
jen_comments_ts$sin6 = sin(2*jen_comments_ts$timeInt/365.25)
jen_comments_ts$cos6 = cos(2*jen_comments_ts$timeInt/365.25)
baselineDate = as.Date('2020/1/1')
baselineDateInt = as.integer(baselineDate)

# gam model
res = mgcv::gam(n ~ date + 
                  cos12 + sin12 + cos6 + sin6 +
                  s(timeInt, k = 8, pc=baselineDateInt),
                data=jen_comments_ts, family=Gamma(link = "log"))

# include fitted values in dataset
jen_comments_ts$gam_fitted <- res$fitted.values

# final plot
ggplot(data = jen_comments_ts) + geom_line(aes(x = date, y = gam_fitted)) +
  ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype = 2,color = "red")


