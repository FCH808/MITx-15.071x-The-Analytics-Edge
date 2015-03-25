setwd("C:/Users/fch80_000/Dropbox/-- edX Courses--/MITx 15.071x The Analytics Edge/Week 2")

x = c(0, 1, 1)
y = c(2, 8, 2)
xy<- data.frame(x, y)

1- ( ((2-2)**2+(2-5)**2+(8-5)**2) / ((2-4)**2+(2-4)**2+(8-4)**2) )
#RSS

wine<- read.csv("wine.csv")
wine.test<- read.csv("wine_test.csv")

str(wine)
summary(wine)

model1<- lm(Price ~ AGST, data = wine)
summary(model1)

model1$residuals
SSE = sum(model1$residuals**2)
SSE

model2<- lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)

SSE = sum(model2$residuals**2)
SSE
names(wine)

model3 = lm(Price ~ ., data = wine)
summary(model3)

SSE = sum(model3$residuals**2)
SSE

lm1<- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(lm1)

lm4<- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(lm4)

cor(wine)

lm5<- lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
summary(lm5)

lm6<- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(lm6)

str(wine.test)
predict.test<- predict(lm4, newdata=wine.test)
predict.test

SSE <- sum((wine.test$Price - predict.test)**2)
SST <- sum((wine.test$Price - mean(wine$Price))**2)
1-(SSE/SST)

baseball<- read.csv("baseball.csv")
str(baseball)

moneyball<- subset(baseball, Year < 2002)

str(moneyball)

moneyball$RD<- moneyball$RS - moneyball$RA

win1<- lm(W~RD, data = moneyball)
summary(win1)

713-614
RD = 99
pred.data<- data.frame(RD)

predict(win1, newdata = pred.data)

run.lm<- lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(run.lm)

run.RS<- lm(RS ~ OBP + SLG, data = moneyball)
summary(run.lm2)

run.RA<- lm(RA ~ OOBP + OSLG, data = moneyball)
summary(run.RA)

pred.new<- data.frame(OBP = 0.311, SLG = 0.405, OOBP = 0.297, OSLG = 0.370)
pred.new

predict(run.RS, newdata = pred.new)
predict(run.RA, newdata = pred.new)

pred.new2<- data.frame( OBP = c(.338, .391, .369, .313, .361), SLG = c(.540, .450, .374, .447, .500))
predict(run.RS, newdata = pred.new2)

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)

wins2013 = c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)

