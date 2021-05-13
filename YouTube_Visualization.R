library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggmap)
library(gcookbook)
library(plyr)
library(lubridate)


my.dir <- "C:/Users/prath/Desktop/Syracuse/Syllabus and Material/SEM 3/IST719/Poster/"

YT <- read.csv(file = paste0(my.dir,"USvideos.csv")
                     ,header = TRUE
                     ,stringsAsFactors = FALSE)

str(YT)
#dropping the non important columns
YT <- subset(YT, select = -c(video_id,thumbnail_link,description))
#Checking NA's
colSums(is.na(YT))
#Changing column name from category_id to category
colnames(YT)[colnames(YT) == 'category_id'] <- 'category'

#renaming the category id to category names
YT$category[YT$category == "1"] <- "Film & Animation"
YT$category[YT$category == "2"] <- "Cars & Vehicles"
YT$category[YT$category == "10"] <- "Music"
YT$category[YT$category == "15"] <- "Pets & Animals"
YT$category[YT$category == "17"] <- "Sports"
YT$category[YT$category == "19"] <- "Travel & Events"
YT$category[YT$category == "20"] <- "Gaming"
YT$category[YT$category == "22"] <- "People & Blogs"
YT$category[YT$category == "23"] <- "Comedy"
YT$category[YT$category == "24"] <- "Entertainment"
YT$category[YT$category == "25"] <- "News & Politics"
YT$category[YT$category == "26"] <- "Howto & Style"
YT$category[YT$category == "27"] <- "Education"
YT$category[YT$category == "28"] <- "Science & Technology"
YT$category[YT$category == "29"] <- "Nonprofits & Activism"
YT$category[YT$category == "43"] <- "Shows"



#density for views
ggplot(YT, aes(views)) + geom_density(fill="#65c4c9", color="#65c4c9", alpha=0.8) +xlim(c(0,4000000))
#density for likes
ggplot(YT, aes(likes)) + geom_density(fill="#6bc7b5", color="#6bc7b5", alpha=0.8) +xlim(c(0,200000))
#density of dislikes
ggplot(YT, aes(dislikes)) + geom_density(fill="#7ecf70", color="#7ecf70", alpha=0.8) +xlim(c(0,10000))
#density of comment count
ggplot(YT, aes(comment_count)) + geom_density(fill="#b5c977", color="#b5c977", alpha=0.8) +xlim(c(0,20000))


typeof(YT$trending_date)
#Transforming character type dates to DATE
YT$trending_date <- ydm(YT$trending_date)
YT$publish_time <- ymd(substr(YT$publish_time,start = 1,stop = 10))
#Creating a new column with difference in days from publish time to trending time
YT$dif_days <- YT$trending_date-YT$publish_time
#View(YT)
YT_dif <- subset(YT, YT[,14] < 30)


#What is the most common content category that goes viral?
#Top Categories

YT2 <- YT %>% group_by(category) %>% dplyr::mutate(count_name_occur = n())

ggplot(YT2, aes(x = reorder(category,-count_name_occur), fill = category)) +
  geom_histogram(stat = "count") + geom_bar(color = "black")+
  labs(y = 'Count', title = 'Categories of Trending Videos') +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 0, hjust = 1), 
        legend.position="none") +coord_flip()

RColorBrewer::brewer.pal.info

#Time it takes for videos to trend?
ggplot(data = YT_dif, aes(x = as.factor(dif_days), fill = as.factor(dif_days))) + 
  geom_bar(color = "black") + theme(legend.position = "none") + xlab("No. of Days") + ylab("No. of Videos") +
  ggtitle("Time it takes for Videos to Trend") + ylim(c(0,5000)) + scale_color_grey()

#Lets see the time taken by categories to trend
diff_days <- data.frame(YT$category, YT$dif_days)
diff_days <- aggregate(diff_days$YT.dif_days, list(diff_days$YT.category), FUN = mean)
diff_days$x <- as.integer(diff_days$x)
#str(diff_days)
ggplot(data = diff_days, aes(x = reorder(Group.1, -x), y = x, fill = as.factor(x))) + 
  geom_bar(stat = "identity", color = "black") + ylim(c(0,50)) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(hjust = 1), 
        legend.position="none") + coord_flip()+
   ggtitle("Time Taken by Videos of Each Category to Trend") + xlab("Categories") + ylab("No. of Days")

#Views distribution for each category
ggplot(data = YT, aes(x = category, y = views)) + geom_boxplot(aes( fill = category)) + 
  ylim(c(0, 4000000)) +theme(plot.title = element_text(hjust = 0.5), 
                                      axis.text.x = element_text(angle = 45, hjust = 1), 
                                      legend.position="none") 

#Likes distribution for each category
ggplot(data = YT, aes(x = category, y = likes)) + geom_boxplot(aes( fill = category)) + 
  ylim(c(0, 150000)) +theme(plot.title = element_text(hjust = 0.5), 
                              axis.text.x = element_text(angle = 45, hjust = 1), 
                              legend.position="none")

#Dislikes distribution for each category
ggplot(data = YT, aes(x = category, y = dislikes)) + geom_boxplot(aes( fill = category)) + 
  ylim(c(0, 5000)) +theme(plot.title = element_text(hjust = 0.5), 
                            axis.text.x = element_text(angle = 45, hjust = 1), 
                            legend.position="none")

usatop <- data.frame(YT)
usatop = filter(usatop) %>% count(category) %>% arrange(desc(n)) %>% head(5)
usatop$fraction = usatop$n/ sum(usatop$n)
usatop$ymax = cumsum(usatop$fraction)
usatop$ymin = c(0, head(usatop$ymax, n=-1))
usatop$labelPosition = (usatop$ymax + usatop$ymin) / 2
usatop$label = paste0(usatop$category, "\n count: ", usatop$n)


usatop$fraction = usatop$n/ sum(usatop$n)
usatop$ymax = cumsum(usatop$fraction)
usatop$ymin = c(0, head(usatop$ymax, n=-1))
usatop$labelPosition = (usatop$ymax + usatop$ymin) / 2
usatop$label = paste0(usatop$category, "\n count: ", usatop$n)

ggplot(usatop, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette="Reds") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void()+ 
  theme(legend.position = "none")

#Top 5 channels across all the five categories 

# top 5 channel across comedy 
uschannel <- usnew
uscomedy <- uschannel[uschannel$category == "Comedy",]
uscomedy <- filter(uscomedy) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)
ggplot(data = uscomedy, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 Comedy Channel" ) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs( x = " Total number of views" , y = "Channels")

# Finding top 5 channel for the Entertainment
usenter <- uschannel[uschannel$category == "Entertainment",]
usenter <- filter(usenter) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)
ggplot(data = usenter, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 Entertainment Channel" ) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs( x = " Total number of views" , y = "Channels")

# Finding the top 5 music channel 
usmusic <- uschannel[uschannel$category == "Music",]
usmusic <- filter(usmusic) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)
ggplot(data = usmusic, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 Music Channel" ) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs( x = " Total number of views" , y = "Channels")

#How to and Style top5 channel
usstyle <- uschannel[uschannel$category == "How to and Style",]
usstyle <- filter(usstyle) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)
ggplot(data = usstyle, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 How to and Style Channel" ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( x = " Total number of views" , y = "Channels")

#People and Blogs
usblog <- uschannel[uschannel$category == "People and Blogs",]
usblog <- filter(usblog) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)
ggplot(data = usblog, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 People and Blogs Channel" ) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs( x = " Total number of views" , y = "Channels")




