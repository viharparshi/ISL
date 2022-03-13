# 2.8This exercise relates to the College data set, which can be found in
#the file College.csv on the book website. It contains a number of
#variables for 777 different universities and colleges in the US. The
#variables are


#(a) Use the read.csv() function to read the data into R. Call the
#loaded data college. Make sure that you have the directory set
#to the correct location for the data.


College = read.csv("../Downloads/College.csv", header = TRUE)

#(b) Look at the data using the View() function. You should notice
#that the first column is just the name of each university. We don’t
#really want R to treat this as data. However, it may be handy to
#have these names for later.

rownames (College) <- College[, 1]
View (College)

College <- College[, -1]
View (College)

#(c) (i).Use the summary() function to produce a numerical summary of the variables in the data set.
summary(College)

#(ii). Use the pairs() function to produce a scatterplot matrix of
#the first ten columns or variables of the data. Recall that
#you can reference the first ten columns of a matrix A using
#A[,1:10].

College[,1] = as.numeric(factor(College[,1]))
pairs(College[, 1:10])

#(iii). Use the plot() function to produce side-by-side boxplots of
#Outstate versus Private.

plot(Outstate ~ Private, data = College, 
     xlab = "Private University", 
     ylab = "Tuition in $")
Elite <- rep ("No", nrow (College))
Elite[College$Top10perc > 50] <- " Yes "
Elite <- as.factor (Elite)
College <- data.frame (College , Elite)
College$Elite <- as.factor(ifelse(College$Top10perc > 50, "Yes", "No"))
summary(Elite)
plot(Outstate ~ Elite, data = College, 
     xlab = "Elite University", 
     ylab = "Tuition in $")
par(mfrow=c(2,2))
hist(College$Apps, xlab = "Applications Received", main = "")
hist(College$perc.alumni, col=2, xlab = "% of alumni who donate", main = "")
hist(College$S.F.Ratio, col=3, breaks=10, xlab = "Student/faculty ratio", main = "")
hist(College$Expend, breaks=100, xlab = "Instructional expenditure per student", main = "")

#EXPLORING DATA
# university with the most students in the top 10% of class
#lowest acceptance rate
## High tuition correlates to high graduation rate

row.names(College)[which.max(College$Top10perc)]
acceptance_rate <- College$Accept / College$Apps
row.names(College)[which.min(acceptance_rate)]
plot(Grad.Rate ~ Outstate, data = College)
AcceptPerc = College$Accept / College$Apps * 100
College = data.frame(College, AcceptPerc)
par(mfrow = c(1, 2))
plot(College$Private, College$AcceptPerc, xlab = "Private", ylab = "Acceptance Rate")
plot(College$Elite, College$AcceptPerc, xlab = "Elite", ylab = "Acceptance Rate")

# 2(9). This exercise involves the Auto data set studied in the lab. Make sure
#that the missing values have been removed from the data.

Auto<-read.csv("../Downloads/Auto.csv", header= TRUE, na.strings = "?")
Auto = na.omit(Auto)
dim(Auto)
#a.Which of the predictors are quantitative, and which are qualitative?

head(Auto)
#b.What is the range of each quantitative predictor? You can answer this using the range() function
?range

range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)

#c.What is the mean and standard deviation of each quantitative predictor?

colMeans(Auto[, 1:7])
apply(Auto[, 1:7], MARGIN = 2, FUN = "sd")

#d.Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the
#subset of the data that remains?

apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "range")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "mean")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "sd")
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg, xlab = "Engine displacement (cubic inches)", ylab = "Miles per gallon")
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
plot(Auto$weight, Auto$mpg, xlab = "Car weight (pounds)", ylab = "Miles per gallon")
plot(Auto$year, Auto$mpg, xlab = "Model Year", ylab = "Miles per gallon")


#e.Using the full data set, investigate the predictors graphically,
#using scatterplots or other tools of your choice. Create some plots
#highlighting the relationships among the predictors. Comment on your findings

par(mfrow = c(2, 2))
plot(Auto$weight, Auto$acceleration, xlab = "Car weight (pounds)", ylab = "0 to 60mph time (seconds)")
plot(Auto$cylinders, Auto$acceleration, xlab = "Number of engine cylinders", ylab = "0 to 60mph time (seconds)")
plot(Auto$displacement, Auto$acceleration, xlab = "Engine displacement (cubic inches)", ylab = "0 to 60mph time (seconds)")
plot(Auto$horsepower, Auto$acceleration, xlab = "Horsepower", ylab = "0 to 60mph time (seconds)")

#f.Suppose that we wish to predict gas mileage (mpg) on the basis
#of the other variables. Do your plots suggest that any of the
#other variables might be useful in predicting mpg? Justify your answer.

Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
plot(Auto$origin, Auto$mpg, xlab = "Country of origin", ylab = "Miles per gallon")


#10. This exercise involves the Boston housing data set.
#a.To begin, load in the Boston data set. The Boston data set is part of the ISLR2 library.

Boston<-read.csv("../Downloads/Boston.csv", header= TRUE, na.strings = "?")
head(Boston)

#How many rows are in this data set? How many columns? What do the rows and columns represent?
#Ans:The corrected Boston data set has 506 rows and 20 columns. Each row represents a particular tract of land within the city of Boston.
#Columns are Town, townno,tract,lon,lat,medv,cmedv,crim,zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,b,lstat.

#b.Make some pairwise scatterplots of the predictors (columns) in
#this data set. Describe your findings.

data(Boston, package = "MASS")
help(Boston, package = "MASS")
dim(Boston)
summary(Boston)
pairs(Boston)

#c.Are any of the predictors associated with per capita crime rate? If so, explain the relationship.
# Older homes, more crime

plot(crim ~ age, data = Boston, log = "xy")

# Closer to work-area, more crime

plot(crim ~ dis, data = Boston)

# Closer to work-area, more crime

plot(crim ~ dis, data = Boston, log = "xy")

# Higher index of accessibility to radial highways, more crime

plot(crim ~ rad, data = Boston, log = "xy")

# as box plots, since rad appears to be categorical

plot(crim ~ as.factor(rad),
     log = "y",
     data = Boston,
     xlab = "Accessibility to radial highways",
     ylab = "log of crime")

# Higher tax rate, more crime

plot(crim ~ tax, log = "xy", data = Boston)

# Higher pupil:teacher ratio, more crime

plot(crim ~ ptratio, log = "xy", data = Boston)
cor(Boston)


#d.Do any of the census tracts of Boston appear to have particularly
#high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.
# to have a crime rate > 20, reaching to above 80

par(mfrow=c(1,3))

hist(Boston$crim[Boston$crim > 1], breaks=25)
# there is a large divide between suburbs with low tax rates and a peak at 660-680

hist(Boston$tax, breaks=25)

#ratio
hist(Boston$ptratio, breaks=25)

#e.How many of the census tracts in this data set bound the Charles river?

sum(Boston$chas == 1)

#f.What is the median pupil-teacher ratio among the towns in this data set?

median(Boston$ptratio)

#g.Which census tract of Boston has lowest median value of owneroccupied homes? What are the values of the other predictors
#for that census tract, and how do those values compare to the
#overall ranges for those predictors? Comment on your findings

t(subset(Boston, medv == min(medv)))

summary(Boston)

#h.In this data set, how many of the census tracts average more than seven rooms per dwelling? More than eight rooms per
#dwelling? Comment on the census tracts that average more than eight rooms per dwelling

sum(Boston$rm > 7)

sum(Boston$rm > 8)

#Suburbs that average more than eight rooms per dwelling

summary(subset(Boston, rm > 8))




#3.7

#(8) This question involves the use of simple linear regression on the Auto data set.

Auto<-read.csv("../Downloads/Auto.csv", header= TRUE, na.strings = "?")

library(MASS)

head(Auto)

#a.Use the lm() function to perform a simple linear regression with mpg as the response and horsepower as the predictor. Use the
#summary() function to print the results

auto.lin.fit = lm(mpg ~ horsepower, data = Auto)
summary(auto.lin.fit)

#i.Is there a relationship between the predictor and the response?

predict(auto.lin.fit, data.frame(horsepower = 98), interval = "prediction")
predict(auto.lin.fit,data.frame(horsepower= 98),interval="confidence")

#Plugging in a horsepower value of 98 gives a predicted mpg of 24.46708. The 95% confidence interval for this prediction is (23.97308, 24.96108)
#and the 95% prediction interval is (14.8094, 34.12467)


#2.Plot the response and the predictor. Use the abline() function to display the least squares regression line.
attach(Auto)
plot(horsepower,mpg)
abline(auto.lin.fit, lwd = 3, col = "red")

#3.Use the plot() function to produce diagnostic plots of the least squares regression fit. Comment on any problems you see with
#the fit.
par(mfrow = c(2, 2))
plot(auto.lin.fit)
#when looking at the Residuals vs. Leverage plot, there are some high leverage points (remember that after dropping the rows with null values, there 
#are 392 observations in the data set, giving an average leverage value of  2/392???0.0051 ) which also have high standardized residual values (greater than 2),
#which is also of concern for the simple linear regression model. 


#9.This question involves the use of multiple linear regression on the
#Auto data set.
Auto<-read.csv("C:\\Users\\D Sai suma\\OneDrive\\Documents\\isl\\Auto.csv", header= TRUE, na.strings = "?")
library(MASS)
head(Auto)
#a.Produce a scatterplot matrix which includes all of the variables in the data set.
#Since origin and name are categorical columns, this are excluding from the scatterplot matrix.
pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + year, Auto)

#b.Compute the matrix of correlations between the variables using
#the function cor(). You will need to exclude the name variable, cor() which is qualitative
Auto$name<-NULL
cor(Auto,method = c("pearson"))

#c.Use the lm() function to perform a multiple linear regression
#with mpg as the response and all other variables except name as
#the predictors. Use the summary() function to print the results.
lm.fit<-lm(mpg~.,data=Auto)
summary(lm.fit)
#c.i.Is there a relationship between the predictors and the response?
#Since the F-statistic is 224.5, giving a p-value of essentially zero for the null hypothesis  H0:??j=0 for all j ,
#there is strong evidence to believe that there is a relationship between the predictors and the response.
#ii.The predictors that appear to have a statistically significant relationship to the response mpg are displacement with a p-value 
#of 0.001863, and weight, year, originEuropean, and originJapanese with p-values of essentially zero.

#d.Use the plot() function to produce diagnostic plots of the linear
#regression fit. Comment on any problems you see with the fit.
par(mfrow = c(2,2))
plot(lm.fit)
#Do the residual plots suggest any unusually large outliers? Does
#the leverage plot identify any observations with unusually high leverage?
# Ans:The first graph shows that there is a non-linear relationship between the responce and the predictors; 
#The second graph shows that the residuals are normally distributed and right skewed;
#The third graph shows that the constant variance of error assumption is not true for this model; 
#The fourth graphs shows that there are no leverage points. However, there on observation that stands out as a potential leverage point


#e.Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions
#appear to be statistically significant?
lm.fit = lm(mpg ~.-name+displacement:weight, data = Auto)
summary(lm.fit)
mpg.fit.all.interactions = lm(mpg ~ (. - name)^2, data = Auto)
summary(mpg.fit.all.interactions)
mpg.fit.reduced.interactions = update(mpg.fit.all.interactions, ~ . - horsepower:origin)
summary(mpg.fit.reduced.interactions)
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:origin)
summary(mpg.fit.reduced.interactions)
mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:displacement)
summary(mpg.fit.reduced.interactions)
summary(lm(mpg ~ . + cylinders:displacement - name, data = Auto))

#f.Try a few different transformations of the variables, such as log(X),
#???X, X2. 
par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg)
plot(Auto$horsepower, Auto$mpg)
plot(Auto$weight, Auto$mpg)
plot(Auto$acceleration, Auto$mpg)
lm.fit = lm(mpg ~.-name+I((displacement)^2)+log(displacement)+displacement:weight, data = Auto)
summary(lm.fit)
par(mfrow = c(2, 2))
plot(lm(mpg ~ acceleration, data = Auto))
par(mfrow = c(2, 2))
plot(lm(mpg ~ log(acceleration), data = Auto))
displacement.linear = lm(mpg ~ displacement, data = Auto)
summary(displacement.linear)
displacement.quadratic = lm(mpg ~ poly(displacement, 2), data = Auto)
summary(displacement.quadratic)
anova(displacement.linear, displacement.quadratic)


#10.This question should be answered using the Carseats data set.
Carseats<-read.csv("C:\\Users\\D Sai suma\\OneDrive\\Documents\\isl\\Carseats.csv", header= TRUE, na.strings = "?")
head(Carseats)
#a.Fit a multiple regression model to predict Sales using Price,Urban, and US.
str(Carseats)
lm.fit = lm(Sales ~ Price+Urban+US, data= Carseats)
summary(lm.fit)

#b.Provide an interpretation of each coefficient in the model. Be
#careful-some of the variables in the model are qualitative?

##Ans:Price: suggests a relationship between price and sales given the low p-value of the t-statistic. 
#In otherwords, when price increases by $1000, the number of carseats sold decrease by 54,459.
#A store's sale is not affected by whether or not it is in a Urban area.
#A store in the US sales 1200 more carseats (in average) than a store that is abroad.
#The coefficient of -0.054459 for Price means that, for a given location (i.e. fixed values of Urban and US), 
#increasing the price of a car seat by $1 results in a decrease of sales by approximately 54.46 units, on average, in the model.

#c.Write out the model in equation form, being careful to handle the qualitative variables properly.
# Sales = 13.04 + -0.05 Price + -0.02 UrbanYes + 1.20 USYes

#d.For which of the predictors can you reject the null hypothesis H0 : ??j = 0?
##Ans:The predictor 'Urban'. Its p-value is not statistically significant with a value of 0.936.

#e. On the basis of your response to the previous question, fit a
#smaller model that only uses the predictors for which there is
#evidence of association with the outcome.
lm.fit2 = lm(Sales ~ Price+US, data= Carseats)
summary(lm.fit2)

#f.How well do the models in (a) and (e) fit the data?
#Based on their respective R-square values(in summary tables), these two models are mediocre 
#(only 24% change in response explained).

#g.Using the model from (e), obtain 95 % confidence intervals for the coefficient(s).
confint(Carseats)
carseats.fit.1 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(carseats.fit.1)
carseats.fit.2 = lm(Sales ~ Price + US, data = Carseats)
summary(carseats.fit.2)
par(mfrow = c(2, 2))
plot(carseats.fit.1)
par(mfrow = c(2, 2))
plot(carseats.fit.2)
confint(carseats.fit.2)

#h.Is there evidence of outliers or high leverage observations in the model from (e)?
par(mfrow=c(2,2))
plot(lm.fit2)  
#Based on the Normal.q-q pot and the Residuals vs Leverage plot, there are no evidence of such points.
#The Residuals vs Leverage graph presents many high leverage points, but the most leverage points are not the outliners detect above.