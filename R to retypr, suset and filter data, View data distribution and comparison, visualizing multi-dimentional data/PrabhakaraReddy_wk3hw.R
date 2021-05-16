library(tidyverse)
my.dir <- "C:/Users/prath/Desktop/Syracuse/Syllabus and Material/SEM 3/IST719/"

art <- read.csv(file = paste0(my.dir,"art.csv")
                ,header = TRUE
                ,stringsAsFactors = FALSE)
#View(art)
#str(art)


#1 Relationship between the unit price of art goods and their units sold?
art.agg.data1 <- tapply(art$units.sold
                        ,list(art$unit.price)
                        ,sum)
barplot(art.agg.data1,col = "blue"
        ,main = "Relationship between unit price and units sold"
        ,xlab = "Unit Price"
        ,ylab = "Units Sold"
        ,ylim = c(0,12000))



#2 Does the art company sell more units of drawing paper or watercolor paper?
art.agg.data2 <- tapply(art$units.sold
                        ,list(Paper = art$paper)
                        ,sum)

barplot(art.agg.data2,beside = TRUE, col ="red"
        ,main = "Paper Type Units"
        ,ylab = "Total Units",xlab = "Paper Type")



#3 Does the art company bring in more money (revenue) selling drawing paper or watercolor paper?
art.agg.data3 <- tapply(art$total.sale
                        ,list(Paper = art$paper)
                        ,sum)

barplot(art.agg.data3,beside = TRUE,ylim = c(0,max(art.agg.data3)), col ="green"
        ,main = "Paper Total Sales"
        ,ylab = "Total Sales",xlab = "Paper Type")



#4 For drawing paper only, make a plot that allows the viewer to compare which subtypes of drawing paper sell more (and less) units across the stores.
art_drawing = filter(art, paper=="drawing")

art.agg.data3 <- tapply(art_drawing$units.sold
                        ,list(art_drawing$paper.type)
                        ,sum)
barplot(art.agg.data3,beside = TRUE
        ,legend.text = rownames(art.agg.data3)
        ,col = c("blue","gold"),main = "Drawing Paper Type Units Sold",ylab = "Units Sold",xlab = "Paper Type")



#5 The dataset covers 4 years of data. Compare the revenue gained each year from the sales drawing paper with that of watercolor paper. Are sales growing over time for both?

paper_sales <- tapply(art$total.sale
                      ,list(art$paper,art$year)
                      ,sum)
options(scipen = 99)
x<- as.numeric(colnames(paper_sales))

plot(x,paper_sales[1,]
     ,type = "l",bty ="n"
     ,main = "Paper Sales over the Years"
     ,xlab = "Year",ylab = "Sales"
     ,col = "red",lwd = 3
     ,ylim = c(min(paper_sales),max(paper_sales)))
lines(x,paper_sales[2,],col="blue",lwd=3)

legend("center",legend = rownames(paper_sales),lwd = 2
       ,lty = 1,col = c("red","blue")
       ,bty ="n")
#Sales are growing overtime for both, but the sales of watercolor paper is higher than that of drawing



