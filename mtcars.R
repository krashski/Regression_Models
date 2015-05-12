### Regression Models Course Project

## Load data and packages
# load the data
data(mtcars)
df.car <- mtcars # duplicate data file so as not to modify original
rm(mtcars)

# load required packages
library(car)
library(lattice)
library(psych)

## Regression Models

# convert am and vs variables to factors for regression models
df.car$am <- factor(df.car$am, labels = c("Automatic", "Manual"))
df.car$vs <- factor(df.car$vs, labels = c("V", "Straight"))
# create power-to-weight ratio variable
df.car$p2w <- df.car$hp / df.car$wt # number of hp per 1000lbs
# create new models starting with power-to-weight and am and 
# adding remaining variables in decreasing order of correlation with mpg
f2 <- lm(mpg ~ p2w + am, df.car)
f3 <- update(f2, mpg ~ p2w + am + drat)
f4 <- update(f2, mpg ~ p2w + am + drat + vs)
f5 <- update(f2, mpg ~ p2w + am + drat + vs + gear)
f6 <- update(f2, mpg ~ p2w + am + drat + vs + gear + qsec)

# evaluate models
anova(f2, f3, f4, f5, f6)

# Cook's distance -- compare to (4 * number of observations) 
# to identify datapoints with high influence
round(cooks.distance(f4)[cooks.distance(f4) > (4 / nrow(df.car))], 3)

# Remove Lotus Europa and re-run regression
df.car2 <- df.car[-28, ]
f4b <- lm(mpg ~ p2w + am + drat + vs, df.car2)
# summary stats for final model
summary(f4b)

# confidence intervals for mpg
confint(f4b)[3, ]

# variance inflation factor
vif(f4b)

## Appendix Figures
# convert am and cyl variables to factors for boxplot
df.car$am <- factor(df.car$am, labels = c("Automatic", "Manual"))
df.car$cyl <- factor(df.car$cyl, labels = c("4 cyl", "6 cyl", "8 cyl"))

with(df.car, bwplot(mpg ~ am | cyl, layout = c(3, 1),
                    main = "Figure 1: MPG by Number of Cylinders and Transmission Type",
                    xlab = "Transmission Type", ylab = "MPG"))

# convert am and cyl variables back to numeric for correlations
df.car$am <- as.numeric(df.car$am)
df.car$cyl <- as.numeric(df.car$cyl)

# scatterplot matrix
pairs.panels(df.car[, c(1, 2, 3, 4, 6, 11)], 
             main = "Figure 2: Predictors Negatively Correlated With MPG")
pairs.panels(df.car[, c(1, 5, 7, 8, 9, 10)], 
             main = "Figure 3: Predictors Positively Correlated With MPG")

# plot regression lines with and without Lotus Europa
plot(mpg ~ p2w, df.car, pch = 19, 
     main = "Figure 4: Lotus Europa Is An Influential Data Point",
     xlab = "Power-To-Weight Ratio (Horsepower/1000 lbs)",
     ylab = "MPG")
text(df.car$p2w[28], df.car$mpg[28], row.names(df.car)[28], pos = 1)
abline(lm(df.car$mpg ~ df.car$p2w), lwd = 2, col = "blue")
abline(lm(df.car2$mpg ~ df.car2$p2w), lwd = 2, col = "orange")
legend('topright', c("With Lotus Europa", "Without Lotus Europa"),
       lty = 1, lwd = 2, col = c("blue", "orange"))

# diagnostic plots for final model
par(mfrow = c(2, 2))
plot(f4b, main = "Figure 5: Diagnotic Plots")
par(mfrow = c(1, 1))


