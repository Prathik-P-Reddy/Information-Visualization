my.dir <- "C:/Users/prath/Desktop/Syracuse/Syllabus and Material/SEM 3/IST719/"
list.files(my.dir)

hotdogs <- read.csv(file = paste0(my.dir,"hot-dog-contest-winners.csv")
                ,header = TRUE
               ,stringsAsFactors = FALSE)



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


hot_dog_places <- read.csv("https://raw.githubusercontent.com/swinton/Visualize-This/master/ch04/data/hot-dog-places.csv", header=TRUE)
View(hot_dog_places)
names(hot_dog_places) <- c("2000", "2001", "2002", "2003", "2004",
                           "2005", "2006", "2007", "2008", "2009", "2010")
#To pass all the preceding values to barplot(), you need to convert hot_dog_places to a matrix. Right now it's a data frame.
hot_dog_matrix <- as.matrix(hot_dog_places)
barplot(hot_dog_matrix, border=NA, space=0.25, ylim=c(0, 200),
        xlab="Year", ylab="Hot dogs and buns (HDBs) eaten",
        main="Hot Dog Eating Contest Results, 1980-2010")



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


unemployment <- read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv",sep=",")
unemployment[1:10,]
# Plain scatter plot
plot(1:length(unemployment$Value), unemployment$Value)
scatter.smooth(x=1:length(unemployment$Value),
               y=unemployment$Value, ylim=c(0,11), degree=2, col="#CCCCCC", span=0.5)
