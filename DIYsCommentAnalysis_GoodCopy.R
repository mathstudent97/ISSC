getwd()
library(httpuv)
library(tuber)
library(dplyr)
library(readr)
library(ggplot2)

app_id <- "568582582358-kupt9to4ti7qo8s5ki2up72kvoft563v.apps.googleusercontent.com"
app_secret <- "fR9Nx8R4wPwpnArx43OHqj32"
#http://localhost:1410/

yt_oauth(app_id, app_secret, token ='')


##5## !USE!

# comments of videos from Household Hacker
DIY_AntRemover <- get_all_comments(video_id = "cWyWnA3GojI")
DIY_HidingSpots <- get_all_comments(video_id = "H640jO_n1AQ")
DIY_HomeInvents <- get_all_comments(video_id = "l5lGF16kc50")
DIY_SecCamera <- get_all_comments(video_id = "y7h8L2zeLdE")
DIY_KitchenGadgets <- get_all_comments(video_id = "xpYqvrYy-xw")

FilteredVideos_AntRemover <- DIY_AntRemover %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_HidingSpots <- DIY_HidingSpots %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_SecCamera <- DIY_SecCamera %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_HomeInvents <- DIY_HomeInvents %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_KitchenGadgets <- DIY_KitchenGadgets %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)


HouseholdHacker <- rbind(FilteredVideos_AntRemover, FilteredVideos_HidingSpots,
                         FilteredVideos_SecCamera, FilteredVideos_HomeInvents,
                         FilteredVideos_KitchenGadgets)

write.csv(HouseholdHacker, file = "HouseholdHacker.csv")

# compile the dates for all the comments left on each video
HouseholdHacker_comments <- as.Date(HouseholdHacker$publishedAt)

# count how many comments were left on each date 
HouseholdHacker_comments <- tibble(date = as.Date(Reduce(c, HouseholdHacker_comments))) %>%
  group_by(date) %>% count()

# plot the number of comments by date (exploratory plot)
plot_HouseholdHacker <- ggplot(data = HouseholdHacker_comments) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-01-06")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-07")), linetype = 2,color = "red")



##6## !USE!

# comments of videos from Make
DIY_WoodenSpoon <- get_all_comments(video_id = "X6l9S08Scww") #low
DIY_SpiderRifle <- get_all_comments(video_id = "MnKJzCUy9Tw") #low
DIY_RoundTable <- get_all_comments(video_id = "LYDWp_0UWgI") #low
DIY_MusicBox <- get_all_comments(video_id = "lswfhea17WY") #low
DIY_SpaceShip <- get_all_comments(video_id = "hfY-SlC2XHc") #low

FilteredVideos_WoodenSpoon <- DIY_WoodenSpoon %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_SpiderRifle <- DIY_SpiderRifle %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_RoundTable <- DIY_RoundTable %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_MusicBox <- DIY_MusicBox %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_SpaceShip <- DIY_SpaceShip %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

Make <- rbind(FilteredVideos_WoodenSpoon, FilteredVideos_SpiderRifle,
              FilteredVideos_RoundTable, FilteredVideos_MusicBox,
              FilteredVideos_SpaceShip)
write.csv(Make, file = "Make.csv")

# compile the dates for all the comments left on each video
Make_comments <- as.Date(Make$publishedAt)

# count how many comments were left on each date 
Make_comments <- tibble(date = as.Date(Reduce(c, Make_comments))) %>%
  group_by(date) %>% count()

# plot the number of comments by date (exploratory plot)
plot_Make <- ggplot(data = Make_comments) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-01-06")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-07")), linetype = 2,color = "red")



##9## USE!

# comments of videos from Troom Troom
DIY_GiantCandy <- get_all_comments(video_id = "7KF3YAAOhvE")
DIY_SchoolSupplies <- get_all_comments(video_id = "xfsqdeIGeBQ")
DIY_SlimeStressBall <- get_all_comments(video_id = "ZfUyn42jeqk")
DIY_SuperheroFood <- get_all_comments(video_id = "YsbZZ5gGdf8")
DIY_FastFood <- get_all_comments(video_id = "RP2Dy8czN_c")

FilteredVideos_GiantCandy <- DIY_GiantCandy %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_SchoolSupplies <- DIY_SchoolSupplies %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_SlimeStressBall <- DIY_SlimeStressBall %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_SuperheroFood <- DIY_SuperheroFood %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_FastFood <- DIY_FastFood %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

TroomTroom <- rbind(FilteredVideos_GiantCandy, FilteredVideos_SchoolSupplies,
                    FilteredVideos_SlimeStressBall, FilteredVideos_SuperheroFood,
                    FilteredVideos_FastFood)

write.csv(TroomTroom, file = "TroomTroom.csv")

# compile the dates for all the comments left on each video
TroomTroom_comments <- as.Date(TroomTroom$publishedAt)

# count how many comments were left on each date 
TroomTroom_comments <- tibble(date = as.Date(Reduce(c, TroomTroom_comments))) %>%
  group_by(date) %>% count()

# plot the number of comments by date (exploratory plot)
plot_TroomTroom <- ggplot(data = TroomTroom_comments) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-01-06")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-07")), linetype = 2,color = "red")



##10## USE!

# comments of videos from 5min Crafts
DIY_ToyAccess<- get_all_comments(video_id = "bV_nCfuxaMI")
DIY_HomeDesign <- get_all_comments(video_id = "IF6KELrv4NA")
DIY_GadgetAccess <- get_all_comments(video_id = "hgvMjJVr0x4")
DIY_Solutions <- get_all_comments(video_id = "fBbKagy1dD8")
DIY_FaceTreatment <- get_all_comments(video_id = "a-vorgLxD1c")

FilteredVideos_ToyAccess <- DIY_ToyAccess %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_HomeDesign <- DIY_HomeDesign %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_GadgetAccess <- DIY_GadgetAccess %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_Solutions <- DIY_Solutions %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_FaceTreatment <- DIY_FaceTreatment %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)


FiveMinCrafts <- rbind(FilteredVideos_ToyAccess, FilteredVideos_HomeDesign,
                       FilteredVideos_GadgetAccess, FilteredVideos_Solutions,
                       FilteredVideos_FaceTreatment)

write.csv(FiveMinCrafts, file = "FiveMinCrafts.csv")

# compile the dates for all the comments left on each video
FiveMinCrafts_comments <- as.Date(FiveMinCrafts$publishedAt)

# count how many comments were left on each date 
FiveMinCrafts_comments <- tibble(date = as.Date(Reduce(c, FiveMinCrafts_comments))) %>%
  group_by(date) %>% count()

# plot the number of comments by date (exploratory plot)
plot_FiveMinCrafts <- ggplot(data = FiveMinCrafts_comments) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-01-06")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-07")), linetype = 2,color = "red")


