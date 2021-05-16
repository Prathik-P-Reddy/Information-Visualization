my.dir <- "C:/Users/prath/Desktop/Syracuse/Syllabus and Material/SEM 3/IST719/"

sales <- read.csv(file = paste0(my.dir,"sales.csv")
                  ,header = TRUE
                  ,stringsAsFactors = FALSE)
colnames(sales)
library(ggplot2)
#Grammer of Graphics
#ggplot you always have to plot dataframes and not vectors. When you have vectors you have to put them in a dataframe
#You can't use par, layout etc. as ggplot will overwrite it
#ggplot tends to use more memory than base R, as it makes a copy of the df tosend to ggplot
#ggplot is better at building complicated graphics

p <- ggplot(sales, aes(x = expenses, y = recipt))
p + geom_point()

class(p)
attributes(p)

ggplot(sales) +
  aes( x = expenses, y =recipt) +
    geom_point(col = 'Blue') #setting color

ggplot(sales) +
  aes( x = expenses, y =recipt, color = type) +
  geom_point() #mapping color

ggplot(sales) +
  aes( x = expenses
       , y =recipt
       , size = units.sold
       , shape = type
       , alpha = rep.region
       , color = year) +
  geom_point()     #Over mapping

#You can also give conditions
p <- ggplot(sales) +
  aes( x = expenses, y =recipt, color = unit.price>14) +
  geom_point()
  
p + scale_color_manual(values = c("red", "gold")) #you can manually add preferred colors

p+ geom_rug()

#Add a linear model line
income.pred <- predict(lm(sales$recipt ~ sales$expenses))
ggplot(sales) +
  aes( x = expenses, y =recipt) +
  geom_point()+
  geom_line(aes(y = income.pred)
            , color = "red", lwd = 1)

ggplot(sales) +
  aes( x = expenses, y =recipt) +
  geom_point() +
  geom_smooth()
#You can also add the lm line with geom_smooth
ggplot(sales) +
  aes( x = expenses, y =recipt) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() #Black and White theme

library(viridis)
ggplot(sales) +
  aes( x = expenses, y =recipt) +
  geom_hex(bins = 100) +
  scale_fill_viridis(option = "cividis") +
  theme_light()
library(hexbin)

price <- ifelse(sales$units.price >14
                , "Expensive", "Moderate")
price[sales$unit.price < 9] <- "Cheap"
ggplot(sales) +
  aes( x = expenses, y =recipt, color = price) +
  geom_bin2d(bins = 50) +
  geom_smooth(color = "gold")

ggplot(sales) +
  aes( x = recipt) +
  geom_histogram(binwidth = 100, fill = "Orange") +
  theme_classic()

ggplot(sales) +
  aes( x = rep.region, y = recipt) +
  geom_boxplot() +
  theme_classic()

df  <- aggregate(sales$units.sold
                 , list(year = sales$year
                        , region = sales$rep.region)
                 ,sum)
max(df$x)
ggplot(df) + 
  aes(x = year, y = x, color = region) +
  geom_line(lwd = 1) +
  ylim(c(0,10000)) +
  ggtitle("Sales by region over time") +
  theme_classic() #Here we can make multi-line chart easily

ggplot(sales) +
  aes(x = rep.region, fill = type)  +
  geom_bar() +
  ggtitle("Stacked Bar Chart") +
  theme_classic() # It is difficult to compare as the heights are different
  
ggplot(sales) +
  aes(x = rep.region, fill = type)  +
  geom_bar(position = "dodge") +
  ggtitle("Bars Beside (dodge)") +
  theme_classic()


df  <- aggregate(sales$units.sold
                 , list(region = sales$rep.region
                        , type = sales$type)
                 ,sum)
colnames(df)[3] <- "Sales"
ggplot(df) +
  aes(x = df$region, y = df$Sales, fill = df$type) +
  geom_bar(stat = "identity",position = "dodge") +
  theme_classic()

ggplot(df) +
  aes(x = region, y= Sales, fill = region ) +
  geom_bar(width = .95, stat = "identity") +
  coord_polar(theta = "y") +
  ylim(c(0,60000)) +
  xlab("") +ylab("") +
  geom_text(data = df, hjust = .9, size =7
            , aes(x = region, y= 0, label = region)) +
  theme(legend.position = "none"
        , axis.text.y = element_blank()
        , axis.ticks = element_blank()
        , panel.background = element_blank()) +
  scale_fill_manual(values = c(
    "darkslateblue", "dodgerblue4", "deepskyblue4"
    , "cornflowerblue", "deepskyblue"
  ))
