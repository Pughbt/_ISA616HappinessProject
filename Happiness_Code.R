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

##Rename variables and drop old ones
happiness$sdofhappiness = happiness$Standard.deviation.of.ladder.by.country.year
happiness = select(happiness, -Standard.deviation.of.ladder.by.country.year)
happiness$meansdofhappiness = happiness$Standard.deviation.Mean.of.ladder.by.country.year
happiness = select(happiness, -Standard.deviation.Mean.of.ladder.by.country.year)
happiness$GINIWorldBank = happiness$GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel
happiness = select(happiness, -GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel)
happiness$GINIHousehold = happiness$gini.of.household.income.reported.in.Gallup..by.wp5.year
happiness = select(happiness, -gini.of.household.income.reported.in.Gallup..by.wp5.year)


##Show the correlation for the variables left in the data set
happiness_numeric = select(happiness, -Country.name)
M = cor(happiness_numeric, use = "complete.obs")
library(corrplot)
corrplot(M, method = "color")

##Drop highly correlated variabels
happiness = select(happiness, -Delivery.Quality)



##Aggregating the data
happiness.agg = happiness %>%
  group_by(Country.name) %>%
  summarize(avg.Life.Lader = mean(Life.Ladder, na.rm = T),
            avg.log.GDP.per.capita = mean(Log.GDP.per.capita, na.rm = T),
            avg.social.support = mean(Social.support, na.rm =T),
            avg.life.expectancy = mean(Healthy.life.expectancy.at.birth, na.rm = T),
            avg.freedom = mean(Freedom.to.make.life.choices, na.rm = T),
            avg.generosity = mean(Generosity, na.rm = T),
            avg.perception.of.corrpution = mean(Perceptions.of.corruption, na.rm = T),
            avg.positive.affect = mean(Positive.affect, na.rm = T),
            avg.negative.affect = mean(Negative.affect, na.rm = T),
            avg.confidence.in.government = mean(Confidence.in.national.government, na.rm = T),
            avg.democratic.quality = mean(Democratic.Quality, na.rm = T),
            avg.sd.of.happiness = mean(sdofhappiness, na.rm = T),
            avg.mean.sd.of.happiness = mean(meansdofhappiness, na.rm = T),
            avg.GINI.world.bank = mean(GINIWorldBank, na.rm = T),
            avg.gini.household = mean(GINIHousehold, na.rm = T)
            )

##Filling in NA with mean of the variables
plot_missing(happiness.agg)
rows = which(is.nan(happiness.agg$avg.confidence.in.government))
print(rows)
happiness.agg$avg.confidence.in.government[rows] = mean(happiness.agg$avg.confidence.in.government, na.rm = T)

rows1 = which(is.nan(happiness.agg$avg.negative.affect))
print(rows1)
happiness.agg$avg.negative.affect[rows1] = mean(happiness.agg$avg.negative.affect, na.rm = T)

rows2 = which(is.nan(happiness.agg$avg.social.support))
print(rows2)
happiness.agg$avg.social.support[rows2] = mean(happiness.agg$avg.social.support, na.rm = T)

rows3 = which(is.nan(happiness.agg$avg.democratic.quality))
happiness.agg$avg.democratic.quality[rows3] = mean(happiness.agg$avg.democratic.quality, na.rm = T)

rows4 = which(is.nan(happiness.agg$avg.positive.affect))
happiness.agg$avg.positive.affect[rows4] = mean(happiness.agg$avg.positive.affect, na.rm = T)

rows5 = which(is.nan(happiness.agg$avg.gini.household))
happiness.agg$avg.gini.household[rows5] = mean(happiness.agg$avg.gini.household, na.rm = T)

rows6 = which(is.nan(happiness.agg$avg.log.GDP.per.capita))
happiness.agg$avg.log.GDP.per.capita[rows6] = mean(happiness.agg$avg.log.GDP.per.capita, na.rm = T)

rows7 = which(is.nan(happiness.agg$avg.generosity))
happiness.agg$avg.generosity[rows7] = mean(happiness.agg$avg.generosity, na.rm = T)

rows8 = which(is.nan(happiness.agg$avg.life.expectancy))
happiness.agg$avg.life.expectancy[rows8] = mean(happiness.agg$avg.life.expectancy, na.rm = T)

rows9 = which(is.nan(happiness.agg$avg.perception.of.corrpution))
happiness.agg$avg.perception.of.corrpution[rows9] = mean(happiness.agg$avg.perception.of.corrpution, na.rm= T)

rows10 = which(is.nan(happiness.agg$avg.GINI.world.bank))
happiness.agg$avg.GINI.world.bank[rows10] = mean(happiness.agg$avg.GINI.world.bank, na.rm = T)

plot_missing(happiness.agg)


model1 = lm(avg.Life.Lader~ avg.log.GDP.per.capita + avg.social.support + avg.life.expectancy
            + avg.freedom + avg.generosity + avg.perception.of.corrpution + avg.positive.affect 
            + avg.negative.affect + avg.confidence.in.government + avg.democratic.quality
             + avg.GINI.world.bank +
              avg.gini.household, data = happiness.agg)
summary(model1)

cor(happiness.agg$avg.confidence.in.government, happiness.agg$avg.Life.Lader)

