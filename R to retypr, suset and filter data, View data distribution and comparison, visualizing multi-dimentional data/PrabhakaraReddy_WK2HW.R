my.dir <- "C:/Users/prath/Desktop/Syracuse/Syllabus and Material/SEM 3/IST719/"
list.files(my.dir)

hotdogs <-
  read.csv("http://datasets.flowingdata.com/hot-dogcontest-
winners.csv",
           sep=",", header=TRUE)
#hotdogs <- read.csv(file = paste0(my.dir,"hot-dog-contest-winners.csv")
 #                ,header = TRUE
  #               ,stringsAsFactors = FALSE)

View(hotdogs)
summary(hotdogs)

barplot(hotdogs$Dogs.eaten)
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year)

#If it's a win for the United States,specify red. Otherwise, specify gray. Here's the code to
fill_colors <- c()
for ( i in 1:length(hotdogs$Country) ) {
  if (hotdogs$Country[i] == "United States") {
    fill_colors <- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col=fill_colors,
        border=NA, xlab="Year", ylab="Hot dogs and buns eaten")

#Highlight years when a record was broken.
fill_colors <- c()
for ( i in 1:length(hotdogs$New.record) ) {
  if (hotdogs$New.record[i] == 1) {
    fill_colors <- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
} 
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col=fill_colors,
          border=NA, xlab="Year", ylab="Hot dogs and buns (HDB) eaten")


barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col=fill_colors,
        border=NA, space=0.3,title(main = "Nathan's Hotdog Eating Contest Results, 1980 - 2010" ), xlab="Year", ylab="Hot dogs and
buns (HDB)
eaten")
##############################################################################################################################################
#Plot 2 You use stacked bar charts when there are subcategories, and the sum of these subcategories is meaningful.

hot_dog_places <- read.csv("https://raw.githubusercontent.com/swinton/Visualize-This/master/ch04/data/hot-dog-places.csv", header=TRUE)
View(hot_dog_places)
names(hot_dog_places) <- c("2000", "2001", "2002", "2003", "2004",
    "2005", "2006", "2007", "2008", "2009", "2010")
#To pass all the preceding values to barplot(), you need to convert hot_dog_places to a matrix. Right now it's a data frame.
hot_dog_matrix <- as.matrix(hot_dog_places)
barplot(hot_dog_matrix, border=NA, space=0.25, ylim=c(0, 200),
        xlab="Year", ylab="Hot dogs and buns (HDBs) eaten",
        main="Hot Dog Eating Contest Results, 1980-2010")
##################################################################################################################################################
#plot3 Scatter Plot
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv",sep=",", header=TRUE)
#View(subscribers) #str(subscribers)
plot(subscribers$Subscribers, type="h", ylim=c(0, 30000),
     xlab="Day", ylab="Subscribers")
points(subscribers$Subscribers, pch=19, col="black")
###################################################################################################################################################
#Plot4 Continuous Time series Data
#Similar to scatter plot, but instead of using type p, we use type l
population <- read.csv("http://datasets.flowingdata.com/world-population.csv",sep=",", header=TRUE)
head(population)
plot(population$Year, population$Population, type="l",
     ylim=c(0, 7000000000), xlab="Year", ylab="Population")
####################################################################################################################################################
#PLot5 Step Chart
#One of the drawbacks of the standard line plot is that it implies steady change from point A to point B.
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", sep=",", header=TRUE)
head(postage)
plot(postage$Year, postage$Price, type="s",
     main="US Postage Rates for Letters, First Ounce, 1991-2010",
     xlab="Year", ylab="Postage Rate (Dollars)")
######################################################################################################################################################

#Part 2
art <- read.csv(file = paste0(my.dir,"art.csv")
                         ,header = TRUE
                        ,stringsAsFactors = FALSE)
#View(art)

par(mfrow=c(2,2))
plot(art$units.sold)
plot(sort(art$units.sold))
##############
hist(art$total.sale,col = "blue", main = "Total Sale Histogram Distribution")
boxplot(art$units.sold)
#############
d <- density(art$total.sale)
plot(d,main = "Density Distribution of Total Sales")
############
Drawing <- art$total.sale[art$paper == "drawing"]
Watercolor <- art$total.sale[art$paper == "watercolor"]
length(Drawing)
length(Watercolor)
length(art$paper)

boxplot(Drawing,ylim=c(0,100),ylab = "Drawing", main="Boxplot of Drawing")
boxplot(Watercolor,ylab="Watercolor",main="Boxplot of Watercolor")









##
art.agg.data1 <- tapply(art$total.sale
                          ,list(art$store)
                            ,sum)
##
art.agg.data2 <- tapply(art$units.sold
                        ,list(art$paper.type)
                        ,sum) 
##
art.agg.data3 <- tapply(art$units.sold
                        ,list(Paper = art$paper,Type =art$paper.type)
                        ,sum)

##
M <- tapply(art$units.sold,list(art$store,art$year),sum)
M
options(scipen = 99)
x<- as.numeric(colnames(M))
##
par(mfrow=c(2,2))

barplot(art.agg.data1,beside = TRUE,col ="red",main = "Store Total Sales",ylab = "Total Sold",xlab = "Store")

barplot(art.agg.data2,beside = TRUE, col ="blue",main = "Paper Type Units",ylab = "Total Units",xlab = "Paper Type")

plot(x,M[1,],type ='l',ylim =c(0,max(M)),bty ="n"
     , xlab ="",ylab = "Units Sold",col ="red",lwd =3, main = "Units sold by stores over time")
lines(x,M[2,],col="blue",lwd=3)
lines(x,M[3,],col="green",lwd=3)
lines(x,M[4,],col="grey",lwd=3)
legend("bottomleft",legend = rownames(M),lwd = 2
       ,lty = 1,col = c("red","blue","green","grey")
       ,bty ="n")

barplot(art.agg.data3,beside = TRUE
        ,legend.text = rownames(art.agg.data3)
        ,col = c("blue","gold"),main = "Units sold of Paper Type Categories",ylab = "Units Sold")
###################################################################################################################################






















