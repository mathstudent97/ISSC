getwd()
library(httpuv)
library(tuber)
library(dplyr)
library(readr)

app_id <- "794917682930-8kobnks8ld97bo9lgou15sprja7dci8g.apps.googleusercontent.com"
app_secret <- "n0Bk-BujXbAnxenW4-jMUhGT"
#http://localhost:1410/

yt_oauth(app_id, app_secret, token ='')

##1##

### comments of random videos...###
# comments of videos from Buzzfeed Nifty
DIY_BedroomProjects <- get_all_comments(video_id = "Cvkiw2Mwle8")
DIY_DormroomProjects <- get_all_comments(video_id = "KwuxJ0qQOvc")
DIY_4Blankets <- get_all_comments(video_id = "INg-qyJ4PWY")
DIY_5minProjects <- get_all_comments(video_id = "v0SjashPa3g")
DIY_RopeProjects <- get_all_comments(video_id = "n3Acmw0dUwQ")

FilteredVideos_BedroomProjects <- DIY_BedroomProjects %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_DormroomProjects <- DIY_DormroomProjects %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_4Blankets <- DIY_4Blankets %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_5minProjects <- DIY_5minProjects %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_RopeProjects <- DIY_RopeProjects %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)


write.csv(FilteredVideos_BedroomProjects, file = "FilteredVideos_BedroomProjects.csv")
write.csv(FilteredVideos_DormroomProjects, file = "FilteredVideos_DormroomProjects.csv")
write.csv(FilteredVideos_4Blankets, file = "FilteredVideos_4Blankets.csv")
write.csv(FilteredVideos_5minProjects, file = "FilteredVideos_5minProjects.csv")
write.csv(FilteredVideos_RopeProjects, file = "FilteredVideos_RopeProjects.csv")

Buzzfeed_nifty <- rbind(FilteredVideos_BedroomProjects, FilteredVideos_DormroomProjects,
                        FilteredVideos_4Blankets, FilteredVideos_5minProjects,
                        FilteredVideos_RopeProjects)
write.csv(Buzzfeed_nifty, file = "BuzzfeedNifty.csv")

# compile the dates for all the comments left on each video
Buzzfeed_nifty <- as.Date(Buzzfeed_nifty$publishedAt)

# count how many comments were left on each date 
Buzzfeed_nifty <- tibble(date = as.Date(Reduce(c, Buzzfeed_nifty))) %>%
  group_by(date) %>% count()

# plot the number of comments by date (exploratory plot)
ggplot(data = Buzzfeed_nifty) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-01-06")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-07")), linetype = 2,color = "red")



write.csv(DIY_BedroomProjects, file = "DIY_BedroomProjects_comments.csv")
write.csv(DIY_DormroomProjects, file = "DIY_DormroomProjects_comments.csv")
write.csv(DIY_4Blankets, file = "DIY_4Blankets_comments.csv")
write.csv(DIY_5minProjects, file = "DIY_5minProjects_comments.csv")
write.csv(DIY_RopeProjects, file = "DIY_RopeProjects _comments.csv")

##2##

# comments of videos from Mad Stuff With Rob
DIY_BalloonOrbs <- get_all_comments(video_id = "YgjXXmNwWuY")
DIY_SpidermanWebshooter <- get_all_comments(video_id = "GCfG9QAPrWY")
DIY_MarvelProjects <- get_all_comments(video_id = "X9mxmfmPJ1s")
DIY_TabletCover <- get_all_comments(video_id = "nsmWjs3kE-o")
DIY_WallpaperArt <- get_all_comments(video_id = "OWJwlrj2CzA")

DIY_HomeHacks <- get_all_comments(video_id = "KbLVaK_VFGI")

write.csv(DIY_BalloonOrbs, file = "DIY_BalloonOrbs_comments.csv")
write.csv(DIY_HomeHacks, file = "DIY_HomeHacks_comments.csv")
write.csv(DIY_MarvelProjects, file = "DIY_MarvelProjects_comments.csv")
write.csv(DIY_TabletCover, file = "DIY_TabletCover_comments.csv")
write.csv(DIY_WallpaperArt, file = "DIY_WallpaperArt_comments.csv")

##3##

# comments of videos from I Like To Make Stuff
DIY_Greenhouse <- get_all_comments(video_id = "sQWPnaiixe0")
DIY_PotteryWheel <- get_all_comments(video_id = "mqCJsYnI6Fc")
DIY_PaintBooth <- get_all_comments(video_id = "DFQ86Sw_OsQ")
DIY_SteadyCam <- get_all_comments(video_id = "r_ui5Btmiq4")
DIY_BackyardZipline <- get_all_comments(video_id = "h8JqU5Df_aw")

write.csv(DIY_Greenhouse, file = "DIY_Greenhouse_comments.csv")
write.csv(DIY_PotteryWheel, file = "DIY_PotteryWheel_comments.csv")
write.csv(DIY_PaintBooth, file = "DIY_PaintBooth_comments.csv")
write.csv(DIY_SteadyCam, file = "DIY_SteadyCam_comments.csv")
write.csv(DIY_BackyardZipline, file = "DIY_BackyardZipline_comments.csv")

##4##

# comments of videos from Hetal's Art
DIY_PhotoFrame <- get_all_comments(video_id = "lJy8xAc4aJY")
DIY_WindChime <- get_all_comments(video_id = "xNLXkScK_Hc")
DIY_Jewelry <- get_all_comments(video_id = "uUMCM5I8AjY")
DIY_PaperWall <- get_all_comments(video_id = "YRgOHJbQPls")
DIY_HangingDoor <- get_all_comments(video_id = "okgkccSttxY")

write.csv(DIY_PhotoFrame, file = "DIY_PhotoFrame_comments.csv")
write.csv(DIY_WindChime, file = "DIY_WindChime_comments.csv")
write.csv(DIY_Jewelry, file = "DIY_Jewelry_comments.csv")
write.csv(DIY_PaperWall, file = "DIY_PaperWall_comments.csv")
write.csv(DIY_HangingDoor, file = "DIY_HangingDoor_comments.csv")

##5## !USE!

# comments of videos from Household Hacker
DIY_AntRemover <- get_all_comments(video_id = "cWyWnA3GojI")
DIY_HidingSpots <- get_all_comments(video_id = "H640jO_n1AQ")
DIY_BathroomCleaners <- get_all_comments(video_id = "b6Pu7jKBKvM")
DIY_HoverShoes <- get_all_comments(video_id = "40S8G2ZuRpE")
DIY_KitchenGadgets <- get_all_comments(video_id = "xpYqvrYy-xw")

# low # of comments
DIY_LegoSafe <- get_all_comments(video_id = "WpIgzDlSqlw")
DIY_OrangeLamp <- get_all_comments(video_id = "-YJgdOFYtj8")
DIY_CloudLight <- get_all_comments(video_id = "07m0e7NwzKc")
DIY_HDSpeaker <- get_all_comments(video_id = "8m8fbnShPcw")
DIY_IceBalls <- get_all_comments(video_id = "Wv7T_Rh0lp4")
DIY_AC <- get_all_comments(video_id = "FaC0dlRENk0")

# low # of comments
FilteredVideos_LegoSafe <- DIY_LegoSafe %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_OrangeLamp <- DIY_OrangeLamp %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_AC <- DIY_AC %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_CloudLight <- DIY_CloudLight %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_HDSpeaker <- DIY_HDSpeaker %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_IceBalls <- DIY_IceBalls %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)


#---

write.csv(DIY_LegoSafe, file = "DIY_LegoSafe_comments.csv")
write.csv(DIY_OrangeLamp, file = "DIY_OrangeLamp_comments.csv")
write.csv(DIY_CloudLight, file = "DIY_CloudLight_comments.csv")
write.csv(DIY_HDSpeaker, file = "DIY_HDSpeaker_comments.csv")
write.csv(DIY_IceBalls, file = "DIY_IceBalls_comments.csv")

FilteredVideos_AntRemover <- DIY_AntRemover %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_HidingSpots <- DIY_HidingSpots %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_BathroomCleaners <- DIY_BathroomCleaners %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_HoverShoes <- DIY_HoverShoes %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

FilteredVideos_KitchenGadgets <- DIY_KitchenGadgets %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2020-01-01") %>%
  arrange(date)

write.csv(FilteredVideos_AntRemover, file = "FilteredVideos_AntRemover.csv")
write.csv(FilteredVideos_HidingSpots, file = "FilteredVideos_HidingSpots.csv")
write.csv(FilteredVideos_BathroomCleaners, file = "FilteredVideos_BathroomCleaners.csv")
write.csv(FilteredVideos_HoverShoes, file = "FilteredVideos_HoverShoes.csv")
write.csv(FilteredVideos_KitchenGadgets, file = "FilteredVideos_KitchenGadgets.csv")

HouseholdHacker <- rbind(FilteredVideos_AntRemover, FilteredVideos_HidingSpots,
                        FilteredVideos_BathroomCleaners, FilteredVideos_HoverShoes,
                        FilteredVideos_KitchenGadgets)
write.csv(HouseholdHacker, file = "HouseholdHacker.csv")

# compile the dates for all the comments left on each video
HouseholdHacker_comments <- as.Date(HouseholdHacker$publishedAt)

# count how many comments were left on each date 
HouseholdHacker_comments <- tibble(date = as.Date(Reduce(c, HouseholdHacker_comments))) %>%
  group_by(date) %>% count()

# plot the number of comments by date (exploratory plot)
HouseholdHacker_plot <- ggplot(data = HouseholdHacker_comments) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-01-06")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-07")), linetype = 2,color = "red")


##6##

# comments of videos from Make
DIY_KitchenKnife <- get_all_comments(video_id = "x5-IjsRxOr4")
DIY_SpiderRifle <- get_all_comments(video_id = "MnKJzCUy9Tw")
DIY_RoundTable <- get_all_comments(video_id = "LYDWp_0UWgI")
DIY_MusicBox <- get_all_comments(video_id = "lswfhea17WY")
DIY_SpaceShip <- get_all_comments(video_id = "hfY-SlC2XHc")

write.csv(DIY_KitchenKnife, file = "DIY_KitchenKnife_comments.csv")
write.csv(DIY_SpiderRifle, file = "DIY_SpiderRifle_comments.csv")
write.csv(DIY_RoundTable, file = "DIY_RoundTable_comments.csv")
write.csv(DIY_MusicBox, file = "DIY_MusicBox_comments.csv")
write.csv(DIY_SpaceShip, file = "DIY_SpaceShip_comments.csv")

##7##

# comments of videos from Creative Channel
DIY_ElectricBike <- get_all_comments(video_id = "ioWv_sePLU0")
DIY_GoKart <- get_all_comments(video_id = "wth7xV5Gew0")
DIY_Chainsaw <- get_all_comments(video_id = "aKuw5XafvTs")
DIY_EggIncubator <- get_all_comments(video_id = "w6k16xBbbLM")
DIY_BluetoothSpeaker <- get_all_comments(video_id = "Zz_tXeZ3aJ8")

write.csv(DIY_ElectricBike, file = "DIY_ElectricBike_comments.csv")
write.csv(DIY_GoKart, file = "DIY_GoKart_comments.csv")
write.csv(DIY_Chainsaw, file = "DIY_Chainsaw_comments.csv")
write.csv(DIY_EggIncubator, file = "DIY_EggIncubator_comments.csv")
write.csv(DIY_BluetoothSpeaker, file = "DIY_BluetoothSpeaker_comments.csv")

##8##

# comments of videos from Karina Garcia
DIY_NailPolish <- get_all_comments(video_id = "gFQ6f9OYtqE")
DIY_LiquidPens <- get_all_comments(video_id = "8qcU5uYK4oQ")
DIY_Lipstick <- get_all_comments(video_id = "UhfDUU6-JRE")
DIY_PhoneCase <- get_all_comments(video_id = "XvBPvsFDH4c")
DIY_KineticSand <- get_all_comments(video_id = "LFVqqGERWgI")

write.csv(DIY_NailPolish, file = "DIY_NailPolish_comments.csv")
write.csv(DIY_LiquidPens, file = "DIY_LiquidPens_comments.csv")
write.csv(DIY_Lipstick, file = "DIY_Lipstick_comments.csv")
write.csv(DIY_PhoneCase, file = "DIY_PhoneCase_comments.csv")
write.csv(DIY_KineticSand, file = "DIY_KineticSand_comments.csv")

##9##

# comments of videos from Troom Troom
DIY_GiantCandy <- get_all_comments(video_id = "7KF3YAAOhvE")
DIY_SchoolSupplies <- get_all_comments(video_id = "xfsqdeIGeBQ")
DIY_SlimeStressBall <- get_all_comments(video_id = "ZfUyn42jeqk")
DIY_Makeup <- get_all_comments(video_id = "tM-5hUMkXFU")
DIY_CheeseburgerPillow <- get_all_comments(video_id = "je6u2aYc3Ow")

write.csv(DIY_GiantCandy, file = "DIY_GiantCandy_comments.csv")
write.csv(DIY_SchoolSupplies, file = "DIY_SchoolSupplies_comments.csv")
write.csv(DIY_SlimeStressBall, file = "DIY_SlimeStressBall_comments.csv")
write.csv(DIY_Makeup, file = "DIY_Makeup_comments.csv")
write.csv(DIY_CheeseburgerPillow, file = "DIY_CheeseburgerPillow_comments.csv")

##10## USE!

# comments of videos from 5min Crafts
DIY_FamilyProjects <- get_all_comments(video_id = "8K-kB6yhUqc")
DIY_HomeDesign <- get_all_comments(video_id = "IF6KELrv4NA")
DIY_GadgetAccess <- get_all_comments(video_id = "hgvMjJVr0x4")
DIY_Garden <- get_all_comments(video_id = "mCBKshmaloI")
DIY_Furniture <- get_all_comments(video_id = "z4HcWDfO1IQ")

write.csv(DIY_FamilyProjects, file = "DIY_FamilyProjects_comments.csv")
write.csv(DIY_HomeDesign, file = "DIY_HomeDesign_comments.csv")
write.csv(DIY_GadgetAccess, file = "DIY_GadgetAccess_comments.csv")
write.csv(DIY_Garden, file = "DIY_Garden_comments.csv")
write.csv(DIY_Furniture, file = "DIY_Furniture_comments.csv")


