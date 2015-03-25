##Week 2 Assignment 1 - State Data
setwd("C:/Users/fch80_000/Dropbox/-- edX Courses--/MITx 15.071x The Analytics Edge/Week 2")

##STATE DATA
#We often take data for granted. However, one of the hardest parts about analyzing a problem you're interested in can be to find good data to answer the questions you want to ask. As you're learning R, though, there are many datasets that R has built in that you can take advantage of.

#In this problem, we will be examining the "state" dataset, which has data from the 1970s on all fifty US states. For each state, the dataset includes the population, per capita income, illiteracy rate, murder rate, high school graduation rate, average number of frost days, area, latitude and longitude, division the state belongs to,  region the state belongs to, and two-letter abbreviation.

#Load the dataset and convert it to a data frame by running the following two commands in R:
  
#  data(state)

#statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)

#Inspect the data set using the command: str(statedata)

#For more information about this data set, type ?state in the R console.




data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)

#PROBLEM 1.1 - DATA EXPLORATION  (1/1 point)
#We begin by exploring the data by examining the latitude and longitude of each state. Plot all of the states' centers with latitude on the y axis (the "y" variable in our dataset) and longitude on the x axis (the "x" variable in our dataset). The shape of the plot should be the familiar outline of the United States! Note that Alaska and Hawaii have had their coordinates adjusted to appear just off of the west coast.

#In the R command you used to generate this plot, which variable name did you use as the first argument?

plot(statedata$x, statedata$y)
#statedata$x

#Using the tapply command, determine which region of the US (West, North Central, South, or Northeast) has the highest average high school graduation rate of all the states in the region:

tapply(statedata$HS.Grad, statedata$state.region, mean, na.rm=T)
#West

#Now, make a boxplot of the murder rate by region (for more information about creating boxplots in R, type ?boxplot in your console).

#Which region has the highest median murder rate?
boxplot(statedata$Murder ~ statedata$state.region)
#South

#You should see that there is an outlier in the Northeast region of the boxplot you just generated. Which state does this correspond to? (Hint: There are many ways to find the answer to this question, but one way is to use the subset command to only look at the Northeast data.)
NE<- subset(statedata, state.region == "Northeast")
NE$state.name[which.max(NE$Murder)]
#New York

names(statedata)

#We would like to build a model to predict life expectancy by state using the state statistics we have in our dataset.

#Build the model with all potential variables included (Population, Income, Illiteracy, Murder, HS.Grad, Frost, and Area). Note that you should use the variable "Area" in your model, NOT the variable "state.area".

#What is coefficient for income?
lm1<- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(lm1)
#-2.180e-05

#Call the coefficient for income x (the answer to Problem 2.1). What is the interpretation of the coefficient x?


#For a one unit increase in income, predicted life expectancy decreases by |x| 


summary(lm2)

#Now plot a graph of life expectancy vs. income using the command:

#plot(statedata$Income, statedata$Life.Exp)

#Visually observe the plot. What appears to be the relationship?
plot(statedata$Income, statedata$Life.Exp)
#Life expectancy is somewhat positively correlated with income. Status: correct

#The model we built does not display the relationship we saw from the plot of life expectancy vs. income. Which of the following explanations seems the most reasonable?
#Multicollinearity

#Recall that we discussed the principle of simplicity: that is, a model with fewer variables is preferable to a model with many unnnecessary variables. Experiment with removing independent variables from the original model. Remember to use the significance of the coefficients to decide which variables to remove (remove the one with the largest "p-value" first, or the one with the "t value" closest to zero), and to remove them one at a time (this is called "backwards variable selection"). This is important due to multicollinearity issues - removing one insignificant variable may make another previously insignificant variable become significant.

#You should be able to find a good model with only 4 independent variables, instead of the original 7. Which variables does this model contain?
lm2<- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
#Population, Murder, Frost, HS.Grad

#Removing insignificant variables changes the Multiple R-squared value of the model. By looking at the summary output for both the initial model (all independent variables) and the simplified model (only 4 independent variables) and using what you learned in class, which of the following correctly explains the change in the Multiple R-squared value?

#We expect the "Multiple R-squared" value of the simplified model to be slightly worse than that of the initial model. It can't be better than the "Multiple R-squared" value of the initial model. Status: correct

#Using the simplified 4 variable model that we created, we'll now take a look at how our predictions compare to the actual values.

#Take a look at the vector of predictions by using the predict function (since we are just looking at predictions on the training set, you don't need to pass a "newdata" argument to the predict function).

#Which state do we predict to have the lowest life expectancy? (Hint: use the sort function)
sort(predict(lm2, statedata))
#Alabama
statedata$state.name[which.min(statedata$Life.Exp)]
#South Carolina

#Which state do we predict to have the highest life expectancy?
sort(predict(lm2, statedata))
#Washington
statedata$state.name[which.max(statedata$Life.Exp)]
#Hawaii

#Take a look at the vector of residuals (the difference between the predicted and actual values).
#For which state do we make the smallest absolute error?
pred.state<- predict(lm2, statedata)
sort(abs(lm2$residuals))
#Indiana
#For which state do we make the largest absolute error?
#Hawaii



