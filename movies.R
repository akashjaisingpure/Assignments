#reading the data
library(arules)

movie <- read.transactions(file.choose(),format="basket")
View(movie)

summary(movie)
inspect(movie[1:10])
itemFrequencyPlot(movie, topN=10)

#applying apriori rules#
rules <- apriori(movie,parameter = list(support=0.0909,confidence= 0.85, minlen=3))

inspect(sort(rules,by="lift"))

inspect(tail(sort(rules, by="lift")))

#lower the confidence higher the rules#
#higher confidence level and minimum support gives common rules #
#for uncommon rules increase the support level and decrease the confidence#

##plotting the rules
library(arulesViz)

plot(rules, method = "scatterplot")
#above plot does not make sense so other plots#
plot(rules, method = "grouped")
plot(rules, method = "graph")
