library(tuber)
library(tidyverse)
library(lubridate)
library(stringi)
# install.packages("wordcloud")
library(wordcloud)
library(gridExtra)
library(ggplot2)


## OAuth Code
yt_oauth("794917682930-m79cr5oh5eof4naa6t5ela2u0c55rabv.apps.googleusercontent.com",
         "XKlo9DznDHF9r8FhTz8jIShV", token = "")

###
#### Mad Stuff with Rob ####
###

# get channel stats for 'Mad Stuff With Rob'
chstat_MadStuffWithRob = get_channel_stats("UCT8FGLq9AU1H6U3ePhegxJA")

# get stats for all the videos on 'Mad Stuff With Rob' channel
VideosStats_MadStuffWithRob = yt_search(term="", type="video", channel_id = "UCT8FGLq9AU1H6U3ePhegxJA")

# get stats for all the videos published starting from Jan 1, 2020
AllVideos_MadStuffwithRob = VideosStats_MadStuffWithRob %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

# collect comments for all videos in the AllVideos data frame
comments_MadStuffwithRob = lapply(as.character(AllVideos_MadStuffwithRob$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})

# clean up video stats df
videostats = lapply(as.character(AllVideos_MadStuffwithRob$video_id), function(x){
  get_stats(video_id = x)
})
videostats = do.call(rbind.data.frame, videostats)
videostats$title = AllVideos_MadStuffwithRob$title
videostats$date = AllVideos_MadStuffwithRob$date
videostats = select(videostats, date, title, viewCount, likeCount, dislikeCount, 
                    commentCount) %>% as.tibble() %>%
  mutate(viewCount = as.numeric(as.character(viewCount)),
         likeCount = as.numeric(as.character(likeCount)),
         dislikeCount = as.numeric(as.character(dislikeCount)),
         commentCount = as.numeric(as.character(commentCount)))

# compile the dates for all the comments left on each video
CommentDates_MadStuffwithRob = lapply(comments_MadStuffwithRob, function(x){
  as.Date(x$publishedAt)
})

# count how many comments were left on each date 
CommentDates_MadStuffwithRob = tibble(date = as.Date(Reduce(c, CommentDates_MadStuffwithRob))) %>%
  group_by(date) %>% count()

# plot the number of comments by date (exploratory plot)
CommentbyDay_MadStuffwithRob <- ggplot(data = CommentDates_MadStuffwithRob) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-01-06")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-07")), linetype = 2,color = "red")


###
#### Household Hacker ####
###


# get channel stats for 'Household Hacker'
chstat_HouseholdHacker = get_channel_stats("UCI4I6ldZ0jWe7vXpUVeVcpg")

# get stats for all the videos on 'Household Hacker' 
VideosStats_HouseholdHacker = yt_search(term="", type="video", channel_id = "UCI4I6ldZ0jWe7vXpUVeVcpg")

# get stats for all the videos published starting from Jan 1, 2020
AllVideos_HouseholdHacker = VideosStats_HouseholdHacker %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

# collect comments for all videos in the AllVideos data frame
comments_HouseholdHacker = lapply(as.character(AllVideos_HouseholdHacker$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})

# clean up video stats df
CleanVideoStats_HouseholdHacker = lapply(as.character(AllVideos_HouseholdHacker$video_id), function(x){
  get_stats(video_id = x)
})
CleanVideoStats_HouseholdHacker = do.call(rbind.data.frame, CleanVideoStats_HouseholdHacker)
CleanVideoStats_HouseholdHacker$title = AllVideos_HouseholdHacker$title
CleanVideoStats_HouseholdHacker$date = AllVideos_HouseholdHacker$date
CleanVideoStats_HouseholdHacker = select(CleanVideoStats_HouseholdHacker, date, title, viewCount, likeCount, dislikeCount, 
                    commentCount) %>% as.tibble() %>%
  mutate(viewCount = as.numeric(as.character(viewCount)),
         likeCount = as.numeric(as.character(likeCount)),
         dislikeCount = as.numeric(as.character(dislikeCount)),
         commentCount = as.numeric(as.character(commentCount)))

# compile the dates for all the comments left on each video
CommentDates_HouseholdHacker = lapply(comments_HouseholdHacker, function(x){
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

  
