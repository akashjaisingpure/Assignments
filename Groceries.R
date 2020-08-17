#reading the data
library(arules)
##given data may be curupted##
data("Groceries")
summary(Groceries)
inspect(Groceries[1:5])
itemFrequencyPlot(Groceries, topN=10)

#applying apriori rules#
rules <- apriori(Groceries,parameter = list(support=0.00405,confidence= 0.65, minlen=4))

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
