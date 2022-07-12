library("rcompanion")
library("car")
library("IDPmisc")
library("dplyr")

# Number of suicides by generation by country, with country being the repeated factor

## Check Assumptions

### Normality
plotNormalHistogram(suicide$suicides.100k.pop)

suicide$suicides.100k.popSQRT <- sqrt(suicide$suicides.100k.pop)
plotNormalHistogram(suicide$suicides.100k.popSQRT)

suicide$suicides.100k.popLOG <- log(suicide$suicides.100k.pop)

suicide4 <- NaRV.omit(suicide)

plotNormalHistogram(suicide4$suicides.100k.popLOG)

#Using log

#Homogeneity of Variance

leveneTest(suicides.100k.popLOG ~ generation, data=suicide4)

#This failed the assumption, but proceed anyway for learning purposes

#Sample size -you have more than enough data

#Run the analysis

RManova1 <- aov(suicides.100k.popLOG~(generation*year)+Error(ï..country/(year)), suicide4)
summary(RManova1)

#There's a generational effect to suicide, and an interaction to how the year has affected the generation

#Post hocs

pairwise.t.test(suicide4$suicides.100k.popLOG, suicide4$generation, p.adjust="bonferroni")
#There's a difference in suicide rates in all of the generations.

#Determine Means  
suicideMeans <- suicide4 %>% group_by(generation, year) %>% summarize(Mean=mean(suicides.100k.pop))

#Draw Conclusions:
# Generation Z is the least likely to commit suicide. 
#Other generations such asgen z and gen x are seemingly steady. 