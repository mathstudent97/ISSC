getwd()
library(httpuv)
library(tuber)
app_id <- "794917682930-8kobnks8ld97bo9lgou15sprja7dci8g.apps.googleusercontent.com"
app_secret <- "ZNKgeVes297ja3YcmEJ2D1GE"
#http://localhost:1410/
yt_oauth(app_id, app_secret, token ='')
data <- get_all_comments(video_id = "X5oD_thIk3c")
comments <- get_all_comments(video_id = "IFe6ag34eMg")
COMMENTS1 <- get_all_comments(video_id = "h0IPcqF2q9U")
write.csv(data, file = "youtubecomments.csv")
# do the same (line above w/ 'comments', 'COMMENTS'); copy above file

mini_donutcer <- get_stats(video_id = 'uAALnrhAXF0')
#write.csv(mini_donutcer, file = "tastyteststats.csv")

# = Channel stats = #
chstat <- get_channel_stats("UCJFp8uSYCjXOMnkUyb3CQ3Q")

# = Videos = #
videos <- yt_search(term="", type="video", channel_id = "UCJFp8uSYCjXOMnkUyb3CQ3Q")
videos <- videos %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2019-09-01") %>%
  arrange(date)
write.csv(videos, file = "videosTable.csv")

# = Comments = #
comments = lapply(as.character(videos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})

# = Prep the data = #
# = Video Stat Table = #
videostats = lapply(as.character(videos$video_id), function(x){
  get_stats(video_id = x)
})
videostats = do.call(rbind.data.frame, videostats)
videostats$title = videos$title
videostats$date = videos$date
videostats = select(videostats, date, title, viewCount, likeCount, dislikeCount, commentCount) %>%
  as.tibble() %>%
  mutate(viewCount = as.numeric(as.character(viewCount)),
         likeCount = as.numeric(as.character(likeCount)),
         dislikeCount = as.numeric(as.character(dislikeCount)),
         commentCount = as.numeric(as.character(commentCount)))
