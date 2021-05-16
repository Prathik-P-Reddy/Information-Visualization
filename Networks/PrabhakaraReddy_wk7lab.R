library(rgl)
library(rmarkdown)

my.dir <- "C:/Users/prath/Desktop/Syracuse/Syllabus and Material/SEM 3/IST719/"

node.attri <- read.csv(file = paste0(my.dir,"2020NodeAttributes.csv")
                 ,header = TRUE
                 ,stringsAsFactors = FALSE)
class.data <- read.csv(file = paste0(my.dir,"2020FclassNetwork.csv")
                       ,header = TRUE
                       ,stringsAsFactors = FALSE)

colnames(class.data) #Spaces are replaces with periods when read in R. 
node.info <- node.info[node.info$Name != "",]
colnames(class.data) <- gsub("\\.","",colnames(class.data))

dim(class.data)
head(class.data)
class.data$X <- colnames(class.data)[-1] #So the row names matches the column names

cbind(node.attri$name,class.data$X) #TO check if the two columns from the two datasets are in the same order


View(class.data)
class(class.data) #Need to convert it into a matrix
M <- as.matrix(class.data[, -1]) #We don't want the first column, we just want it as rownames. We want a square matrix
rownames(M) <- class.data$X
View(M)

install.packages("igraph")
library(igraph)

#M[is.na(M)] <- 0

g <- graph_from_adjacency_matrix(M) 
g
vcount(g) #Number of vertices (order of graph)
ecount(g) #Number of edges
plot(g) #This also works with social network graph
plot.igraph(g) #f1 to get help on the function
?igraph.plotting() #help(igraph.plotting)

g <- simplify(g) #to delete the self-loops
plot.igraph(g)

par(mar=c(0,0,3,0))
plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0, main="IST 719 (Tues and Weds)")


# who claims the most friends?
par(mar=c(5,4,4,1))
degree(g) # How many links each person in the network has. It is a centrality measurement: It says how important a node is.
plot(sort(degree(g)))
hist(degree(g))

#We can also do a bar chart. 
par(mar=c(5,7,4,2), cex.axis = .5)
barplot(sort(degree(g,mode = "out")), horiz = T, las=2
        , border=NA, col = "darkgreen"
        , main="Most friends", space = 1)
##### SAVE THE PLOT

# Who listening more to their friends 
barplot(sort(degree(g, mode = "in"))
        , horiz = T, las=2
        , border=NA, col = "darkorange"
        , main="most popular", space = 1)

##### SAVE THE PLOT

# Who nominated the most friends? Mode = "out": Who has listed most friends
barplot(sort(degree(g, mode = "out")), horiz = T, las=2
        , border=NA, col = "darkorchid"
        , main="most friendly", space = 1)

##### SAVE THE PLOT

#Another centrality measurement is "Betweeness", but this says how important a node is, if you remove that node would there still be a connection in the network
barplot(sort(betweenness(g)), horiz = T, las=2
        , border=NA, col = "red3"
        , main="Betweeness", space = 1)
##### SAVE THE PLOT



#SECOND HALF OF CLASS


V(g)$name
cbind(V(g)$name,node.attri$name) #These two should be in the same order if you want to set attributes inside the network object. You have to organize data

node.attri$name <- gsub(" ", "", node.attri$name)

index <- match(V(g)$name,node.attri$name)
cbind(V(g)$name,node.attri$name[index])
colnames(node.attri)
V(g)$class <- "" #Creating a new attribute for vertices
V(g)$class <- node.attri$class[index]

V(g)$name[which(is.na(V(g)$class))] 
V(g)$class[which(is.na(V(g)$class))]<- "Wednesday" 



V(g)$color <- "green"
V(g)$color[V(g)$class=="Tuesday"] <- "cadetblue"
V(g)$color[V(g)$class=="Wednesday"] <- "red3"

par(mar=c(0,0,3,0))
plot.igraph(g,edge.arrow.size = 0
            , edge.arrow.width=0
            ,main = "IST719 M & W")

colnames(node.attri)
View(node.attri)
table(node.attri$country)
V(g)$color <- "green"
V(g)$country <- "" #Creating a new network object called country
V(g)$country <- node.attri$country[index]
cbind(V(g)$name,V(g)$country)

V(g)$color <- 1
V(g)$color <- as.numeric(factor(V(g)$country)) #Colors can be references using numbers as well, see the chart

par(mar=c(0,0,3,0))
plot.igraph(g,edge.arrow.size = 0
            , edge.arrow.width=0
            ,main = "Color by Country IST719 M & W")
###### SAVE THE PLOT

E(g)$color <- "cornsilk4"
E(g)$arrow.size <- 0
E(g)$arrow.width <- 0
plot.igraph(g)

V(g)$size <- 2
plot.igraph(g)
V(g)$bet <- betweenness(g) #Creating an attribute
range(V(g)$bet) # Scale the data as the range is too much for size
size <- V(g)$bet/max(V(g)$bet)
V(g)$size <- round(5 + (17*size),0)
plot.igraph(g,main = "Scaling Nodes")


V(g)$color[V(g)$name == "PrathikPrabhakaraReddy"]<-"red"
plot.igraph(g,main = "Scaling")
E(g)$color <- "bisque3"
E(g)[from("PrathikPrabhakaraReddy")]$color <- "red"
E(g)[to("PrathikPrabhakaraReddy")]$color <- "red"
plot.igraph(g,main = "Scaling Nodes")

E(g)$width <- 1
E(g)[from("PrathikPrabhakaraReddy")]$width <- 3
E(g)[to("PrathikPrabhakaraReddy")]$width <- 3
plot.igraph(g,main = "Neighborhood")

l <- layout_on_grid(g) #This layout places vertices on a rectangulat grid, in two or three dimensions.
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g,main="Neighborhood")


l <- layout_with_kk(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g,main="Neighborhood")

l <- layout_as_star(g, center = "PrathikPrabhakaraReddy")
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g,main="Neighborhood")

l <- layout_in_circle(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g,main="Layout")

l <- layout_as_tree(g, root = "PrathikPrabhakaraReddy")
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g,main="Neighborhood")



library(rgl) #Allows you to build 3D representations of data
l<- layout_with_kk(g,dim = 3) # kk layout works well with small network, takes a lot of time for big ones
V(g)$x <- l[,1]
V(g)$y <- l[,2]
V(g)$z <- l[,3]
V(g)$label <- ""
rglplot(g)

V(g)$color <- "gold"
E(g)$color <- "green"
rglplot(g)
par3d(windowRect = c(100,100,640,640))
rgl.bg(color = "black")
rgl.viewpoint(0,20)
max.loops <- 360*3
for (i in 1:max.loops) {
        rgl.viewpoint(i,20,zoom = 1.2 - i/max.loops)
}

rgl.snapshot(paste0(my.dir,"network3.png"))


