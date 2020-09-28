## Read in the happiness data

happiness = read.csv("Happiness/HappyData.csv", stringsAsFactors = TRUE)

## Data Summary
summary(happiness)





##Check missingness of the data
library(DataExplorer)
plot_missing(happiness)



##Drop variables with more than 50% missing
library(dplyr)
happiness = select(happiness, -Most.people.can.be.trusted..Gallup)
happiness = select(happiness, -Most.people.can.be.trusted..WVS.round.1981.1984)
happiness = select(happiness, -Most.people.can.be.trusted..WVS.round.1989.1993)
happiness = select(happiness, -Most.people.can.be.trusted..WVS.round.1994.1998)
happiness = select(happiness, -Most.people.can.be.trusted..WVS.round.1999.2004)
happiness = select(happiness, -Most.people.can.be.trusted..WVS.round.2005.2009)
happiness = select(happiness, -Most.people.can.be.trusted..WVS.round.2010.2014)
happiness = select(happiness, -GINI.index..World.Bank.estimate.)
