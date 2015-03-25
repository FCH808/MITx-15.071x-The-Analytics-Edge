setwd("C:/Users/fch80_000/Dropbox/-- edX Courses--/MITx 15.071x The Analytics Edge/Week 1")

who<- read.csv("WHO.csv")
str(who)
summary(who)
head(who)

who.eu<- subset(who, Region == "Europe")
str(who.eu)

write.csv(who.eu, "WHO_Europe.csv")

mean(who$Under15)
sd(who$Under15)
summary(who$Under15)
which.min(who$Under15)
who$Country[which.min(who$Under15)]
which.max(who$Under15)
who$Country[124]

plot(who$GNI, who$FertilityRate)

outliers<- subset(who, GNI > 10000 & FertilityRate > 2.5)
nrow(outliers)
outliers[c("Country", "GNI", "FertilityRate")]

hist(who$CellularSubscribers)
boxplot(who$LifeExpectancy ~ who$Region, xlab = "Region", ylab = "Life Expetancy", main = "Title")

table(who$Region)
tapply(who$Over60, who$Region, mean)
tapply(who$LiteracyRate, who$Region, min, na.rm = T)

mean(who$Over60)
who$Country[which.min(who$Over60)]
who$Country[which.max(who$LiteracyRate)]

usda<- read.csv("USDA.csv")
str(usda)
summary(usda)

usda$Description[which.max(usda$Sodium)]

high.sodium<- subset(usda, Sodium > 10000)
high.sodium$Description

usda$Sodium[match("CAVIAR", usda$Description)]
summary(usda$Sodium)
sd(usda$Sodium, na.rm = T)

plot(usda$Protein, usda$TotalFat, xlab = "Protein", ylab = "Fat", main = "title")

hist(usda$VitaminC, xlim = c(0, 100), breaks = 2000)

boxplot(usda$Sugar, main = "Boxplot" )

usda$HighSodium<- as.numeric(usda$Sodium > mean(usda$Sodium, na.rm = T))
usda$HighProtein<- as.numeric(usda$Protein > mean(usda$Protein, na.rm = T))
usda$HighFat<- as.numeric(usda$TotalFat > mean(usda$TotalFat, na.rm = T))
usda$HighCarbs<- as.numeric(usda$Carbohydrate > mean(usda$Carbohydrate, na.rm = T))
str(usda)

table(usda$HighSodium, usda$HighFat)
tapply(usda$Iron, usda$HighProtein, mean, na.rm=T)
tapply(usda$VitaminC, usda$HighCarbs, summary, na.rm=T)

gta<- read.csv("mvtWeek1.csv")
str(gta)
table(gta$Arrest)

max(gta$ID)
min(gta$Beat)

usda$Sodium[match("CAVIAR", usda$Description)]
gta[match("ALLEY", gta$LocationDescription)]

alley<- subset(gta, gta$LocationDescription == "ALLEY")
nrow(alley)
table(gta$LocationDescription == "ALLEY")

gta$Date[1]
