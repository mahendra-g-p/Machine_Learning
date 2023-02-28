library(viridis)
library(arules)
library(TSP)
library(data.table)
library(dplyr)
library(devtools)
library(purrr)
library(tidyr)
library(arulesViz)

setwd("/Users/mahendra_g_p/Downloads/Academic/Spring_2023/Machine_Learning/Project")

shipping_Details <- read.transactions("transactions.csv",
                           rm.duplicates = FALSE, 
                           format = "basket",  ##if you use "single" also use cols=c(1,2)
                           sep=",",  ## csv file
                           cols=NULL)

inspect(shipping_Details)

# Let's find rules which are having minimum support and minimum confidence of 0.35
# and minimum lenght of rules is defined as 2
Shipping_rules = arules::apriori(shipping_Details, parameter = list(support=.35, 
                                                 confidence=.35,minlen=2))
inspect(Shipping_rules)

# Interested to know from the entire transactions, which terms are more frequent.
itemFrequencyPlot(shipping_Details, topN=20, type="absolute")

# Ok, it is clear that more non-fragile shipments are placed to transport 
# and roadways is hte most preferred means of transport. 
# This may be for the reasons like major chunk of customers are working class 
# and the delivery location is not remote.

# Let's start sorting the rules with lift, support, confidence
SortedRules_lift <- sort(Shipping_rules, by="lift", decreasing=FALSE)
inspect(SortedRules_lift[1:15])
(summary(SortedRules_lift))

SortedRules_support <- sort(Shipping_rules, by="support", decreasing=FALSE)
SortedRules_confidence <- sort(Shipping_rules, by="confidence", decreasing=FALSE)


# Interested to know what are the factors that are highly significant to opt for the roadways 
# as means of transport.
Roadways_Rules <- apriori(data=shipping_Details,parameter = list(supp=.35, conf=.35, minlen=2),
                     appearance = list(default="lhs", rhs={"Roadways"}),
                     control=list(verbose=FALSE))
Roadways_Rules <- sort(Roadways_Rules, decreasing=TRUE, by="confidence")
inspect(Roadways_Rules)

# It is clearly evident that if the shipment is not international one and delivery location
# is not remote & also if the shipment is a not fragile thing, then mostly likely preferred
# transportation is roadways alone.


# When we know the reasons for most of the shipments being done are through roadways,
# then what can the reasons for choosing the airways as the transportation method.
Airways_Rules <- apriori(data=shipping_Details,parameter = list(supp=.15, conf=.15, minlen=2),
                          appearance = list(default="lhs", rhs={"Airways"}),
                          control=list(verbose=FALSE))
Airways_Rules <- sort(Airways_Rules, decreasing=TRUE, by="confidence")
inspect(Airways_Rules)
# So in the possible cases where express shipment must be done, Airways is preferred.

# Now let's see what are the most likely choices of transport option for a Working class customer.
Working_Class_Rules <- apriori(data=shipping_Details,parameter = list(supp=.35, conf=.35, minlen=2),
                         appearance = list(default="rhs", lhs={"Working Class"}),
                         control=list(verbose=FALSE))
Working_Class_Rules <- sort(Working_Class_Rules, decreasing=TRUE, by="confidence")
inspect(Working_Class_Rules)
# It tells us that usually working class people will rarely request for Express shipment.

Wealthy_Rules <- apriori(data=shipping_Details,parameter = list(supp=.15, conf=.35, minlen=2),
                               appearance = list(default="rhs", lhs={"Wealthy"}),
                               control=list(verbose=FALSE))
Wealthy_Rules <- sort(Wealthy_Rules, decreasing=TRUE, by="confidence")
inspect(Wealthy_Rules)
# Similarly, rich people tend to look out for express shipment options.

# Visualize top 15 rules sorted on the basis of lift.
subrules_lift <- head(sort(SortedRules_lift, by="lift"),15)
plot(subrules_lift)
plot(subrules_lift, method="graph", engine="htmlwidget")

# Visualize top 15 rules sorted on the basis of support
subrules_support <- head(sort(SortedRules_support, by="support"),15)
plot(subrules_support)
plot(subrules_support, method="graph", engine="htmlwidget")

# Visualize top 15 rules sorted on the basis of confidence
subrules_confidence <- head(sort(SortedRules_confidence, by="confidence"),15)
plot(subrules_confidence)
plot(subrules_confidence, method="graph", engine="htmlwidget")

# Rules for Airways being selected as means of transportation, sorted on the basis of lift
subrules_support <- head(sort(Airways_Rules, by="lift"),15)
plot(subrules_support)
plot(subrules_support, method="graph", engine="htmlwidget")

