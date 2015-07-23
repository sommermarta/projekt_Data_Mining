install.packages("psych")
library(psych)
pairs.panels(attitude)   #see the graphics window
data(iris)
pairs.panels(iris[1:4],bg=c("red","yellow","blue")[iris$Species],
             pch=21,main="Fisher Iris data by Species") #to show color grouping
pairs.panels(iris[1:4],bg=c("red","yellow","blue")[iris$Species],
             pch=21,main="Fisher Iris data by Species",hist.col="red") 
#to show changing the diagonal
#demonstrate not showing the data points
data(sat.act)
pairs.panels(sat.act,show.points=FALSE)
#better yet is to show the points as a period
pairs.panels(sat.act,pch=".", main="giwo")
#show many variables with 0 gap between scatterplots
# data(bfi)
# pairs.panels(bfi,show.points=FALSE,gap=0)



pairs.panels(se[,-c(1:3,8:12,14)],bg=c("yellow","blue")[se$class],
                              pch=21, cex=0.8, font.labels=2, cex.labels=1.9,
             main="Macierz korelacji", cex.main=2)

