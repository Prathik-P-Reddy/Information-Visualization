my.dir <- "C:/Users/prath/Desktop/Syracuse/Syllabus and Material/SEM 3/IST719/"

sales <- read.csv(file = paste0(my.dir,"sales.csv")
                ,header = TRUE
                ,stringsAsFactors = FALSE)

library(RColorBrewer)
display.brewer.all() #Shows built in color palettes for visualizations


rand.dat <- replicate(8, rnorm(27,27,sd = 1.5)) #replicates the function 8 times and
#rand.dat                                       #syres it in a matrix
boxplot(rand.dat,col = brewer.pal(8, "Spectral"))#8 colors from specified palette
#colors are shown in hexadecimal values. RGB :First two value is red, then green and then blue.


num.colors <- 8
FUN <- colorRampPalette(c("red","#CD950C","blue"))
my.cols <- FUN(num.colors)
boxplot(rand.dat,col=my.cols)


plot(sales$expenses,sales$recipt,pch = 16,cex =1.2) #cex gives the size of the point. #pch for shape of the point
dim(sales)
#There are so many overlapping points and it's difficultto find if there's a pattern hidden
#You can use cluster analysis, but let's use colors here
col.vec <- rep(rgb(30,144,255,maxColorValue = 255)
               ,nrow(sales))
# I have a theory that male spend more to get sales
col.vec[sales$rep.sex == 1] <- rgb(255,64,64,
                                   maxColorValue = 255)
plot(sales$expenses,sales$recipt,pch = 16,cex =1.2
     , col = col.vec)
#My hypothesis is wrong, or you can't conclude with this



col.vec <- rep(rgb(30,144,255,maxColorValue = 255)
               ,nrow(sales))
# I have a theory that male spend more to get sales
col.vec[sales$type=="red"] <- rgb(255,64,64,
                                   maxColorValue = 255)
plot(sales$expenses,sales$recipt,pch = 16,cex =1.2
     , col = col.vec)



hist(sales$unit.price)
col.vec <- rep(rgb(30,144,255,maxColorValue = 255)
               ,nrow(sales))
# I have a theory that male spend more to get sales
col.vec[sales$unit.price > 9.5] <- rgb(255,64,64,
                                   maxColorValue = 255)
col.vec[sales$unit.price > 14] <- rgb(64,255,64,
                                      maxColorValue = 255)
plot(sales$expenses,sales$recipt,pch = 16,cex =1.2
     , col = col.vec)




#Overplotting is a problem!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#1 You can reduce the cex
plot(sales$expenses,sales$recipt,pch = 16,cex =.4
     , col = col.vec)
#2Alpha for transparency or density
col.vec <- rep(rgb(100,144,50,alpha =30
                   ,maxColorValue = 255)
                ,nrow(sales))
plot(sales$expenses,sales$recipt,pch = 16,cex =1
     , col = col.vec)



agg.dat <- aggregate(sales$units.sold
                       ,list(type=sales$type,wine=sales$wine)
                       ,sum)
wine.cols <- c(rgb(255,240,150,maxColorValue = 255)
               ,rgb(160,30,65,maxColorValue = 255))
pie(c(10,10),col=wine.cols) #Just to check

barplot(agg.dat$x, names.arg = agg.dat$wine)

agg.dat$color <- "green"
agg.dat$color[agg.dat$type=="red"]<-wine.cols[2]
agg.dat$color[agg.dat$type=="white"]<-wine.cols[1]

barplot(agg.dat$x, names.arg = agg.dat$wine
        , col = agg.dat$color
        , horiz = T
        ,las=1
        ,main = "wine's by types")


agg.data <- aggregate(sales$units.sold
                     ,list(type=sales$type,wine=sales$wine)
                     ,sum)
agg.data.recipts <- aggregate(sales$recipt
                     ,list(type=sales$type,wine=sales$wine)
                     ,sum)
colnames(agg.data) <- c("type","wine","units.sold")
agg.data$recipts <- agg.data.recipts$x


options(scipen = 99)
library(png)
ima <- readPNG(paste0(my.dir,"bottles.png"))
r1 <- readPNG(paste0(my.dir,"r1.png"))
w1 <- readPNG(paste0(my.dir,"w1.png"))

pch <- rep("w",nrow(agg.data))
pch[agg.data$type=="red"] <-"R"
par(mar = c(5,4,4,2))
plot(agg.data$units.sold,agg.data$recipts
     ,pch=pch,bty="n",cex=2
     ,xlab = "Units Sold",ylab = "Income"
     ,main = "Wine Sales Data"
     ,xlim = c(0,(1.25*max(agg.data$units.sold)))
     ,ylim = c(0,(1.25*max(agg.data$recipts))))
#We need space on the sides if you want to add more items, so we use xlim and ylim

#Now to plot wine glass and give a background
lim <- par()
lim$usr # x1 x2 y1 y2 (max and min limits of the plot space parameters)
rasterImage(ima,lim$usr[1],lim$usr[3]
            ,lim$usr[2],lim$usr[4])  #x1 y1 x2 y2
rect(lim$usr[1],lim$usr[3],lim$usr[2],lim$usr[4]
     ,col=rgb(1,1,1,alpha = .85),border = "white")

r1.x1 <-agg.data$units.sold[agg.data$type=="red"]
r1.x2 <- r1.x1 + 3000
r1.y1 <- agg.data$recipts[agg.data$type=="red"]
r1.y2 <- r1.y1 +65000

rasterImage(r1,r1.x1,r1.y1,r1.x2,r1.y2)

w1.x1 <-agg.data$units.sold[agg.data$type=="white"]
w1.x2 <- w1.x1 + 2600
w1.y1 <- agg.data$recipts[agg.data$type=="white"]
w1.y2 <- w1.y1 +68500

rasterImage(w1,w1.x1,w1.y1,w1.x2,w1.y2)
text(agg.data$units.sold,agg.data$recipts
     ,labels = agg.data$wine,adj=0,cex=1.2) #adj= 0 is right,1 is left,0.5 is center
