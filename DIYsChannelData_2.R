library(tuber)
library(tidyverse)
library(lubridate)
library(stringi)
# install.packages("wordcloud")
library(wordcloud)
library(gridExtra)
library(ggplot2)


## OAuth Code
yt_oauth("200900094308-vaa125ff7h9j97besk6q9bopuueugnkv.apps.googleusercontent.com",
         "Ph6oRLG-ygQfOz8A_piFtwdl", token = "")


###
#### 5-Minute Crafts ####
###


# get channel stats for '5-Minute Crafts'
chstat_BuzzFeednifty = get_channel_stats("UChDhriK1Ar625KyY7JdnT7A")
chstat_HetalsArt = get_channel_stats("UCRqEfb8TRJ-WA597UT2kP6w")
chstat_CreativeChannel = get_channel_stats("UCFwdmgEXDNlEX8AzDYWXQEg")
chstat_TroomTroom = get_channel_stats("UCWwqHwqLSrdWMgp5DZG5Dzg")
chstat_iLiketoMakeStuff = get_channel_stats("UC6x7GwJxuoABSosgVXDYtTw")
chstat_KarinaGarcia = get_channel_stats("UCTTJMptGhfJA67e40DlqbNw")



# get stats for all the videos on '5-min crafts' 
VideosStats_BuzzFeednifty = yt_search(term="", type="video", channel_id = "UChDhriK1Ar625KyY7JdnT7A")
VideosStats_HetalsArt = yt_search(term="", type="video", channel_id = "UCRqEfb8TRJ-WA597UT2kP6w")

# get stats for all the videos published starting from Jan 1, 2020
AllVideos_BuzzFeednifty = VideosStats_BuzzFeednifty %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)
AllVideos_HetalsArt = VideosStats_HetalsArt %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

###

# collect comments for all videos in the AllVideos data frame ###ERROR###
comments_BuzzFeednifty = lapply(as.character(AllVideos_BuzzFeednifty$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})
###

# clean up video stats df
CleanVideoStats_5minCrafts = lapply(as.character(AllVideos_5minCrafts$video_id), function(x){
  get_stats(video_id = x)
})
CleanVideoStats_5minCrafts = do.call(rbind.data.frame, CleanVideoStats_5minCrafts)
CleanVideoStats_5minCrafts$title = AllVideos_5minCrafts$title
CleanVideoStats_5minCraftsr$date = AllVideos_5minCrafts$date
CleanVideoStats_5minCrafts = select(CleanVideoStats_5minCrafts, date, title, viewCount, likeCount, dislikeCount, 
                                    commentCount) %>% as.tibble() %>%
  mutate(viewCount = as.numeric(as.character(viewCount)),
         likeCount = as.numeric(as.character(likeCount)),
         dislikeCount = as.numeric(as.character(dislikeCount)),
         commentCount = as.numeric(as.character(commentCount)))

CleanVideoStats_HetalsArt = lapply(as.character(AllVideos_HetalsArt$video_id), function(x){
  get_stats(video_id = x)
})
CleanVideoStats_HetalsArt = do.call(rbind.data.frame, CleanVideoStats_HetalsArt)
CleanVideoStats_HetalsArt$title = AllVideos_HetalsArt$title
CleanVideoStats_HetalsArt$date = AllVideos_HetalsArt$date
CleanVideoStats_HetalsArt = select(CleanVideoStats_HetalsArt, date, title, viewCount, likeCount, dislikeCount, 
                                    commentCount) %>% as.tibble() %>%
  mutate(viewCount = as.numeric(as.character(viewCount)),
         likeCount = as.numeric(as.character(likeCount)),
         dislikeCount = as.numeric(as.character(dislikeCount)),
         commentCount = as.numeric(as.character(commentCount)))


# compile the dates for all the comments left on each video
CommentDates_5minCrafts = lapply(comments_5minCrafts, function(x){
  as.Date(x$publishedAt)
})

# count how many comments were left on each date 
CommentDates_HouseholdHacker = tibble(date = as.Date(Reduce(c, CommentDates_HouseholdHacker))) %>%
  group_by(date) %>% count()

# plot the number of comments by date (exploratory plot)
CommentbyDay_HouseholdHacker <- ggplot(data = CommentDates_HouseholdHacker) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-01-06")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-07")), linetype = 2,color = "red")



