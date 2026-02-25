# Don't forget to set the working directory (uncomment line below)!
# setwd(YOUR_PATH)

library(ggplot2)

Sweden <- readxl::read_excel("data/Sweden_electricity.xlsx")
str(Sweden)
# Monthly electricity data (Available to internal markets) for Sweden

# Fixing the format of the Date variable --> using a "trick" to add the first
# day (01) for each month to use the months() function and so on...
Sweden$Date <- paste0(Sweden$Date, "-01")
Sweden$Date <- as.Date(Sweden$Date) # other variable is fine (numeric)

ggplot(data = Sweden, aes(x=Date)) +
  geom_line(aes(y = Sweden, colour="Available to internal market"),
            linewidth=1)+
  theme_minimal()

# Initial data exploration (EDA)------------------------------------------------
ggplot(Sweden, aes(y=Sweden)) + geom_boxplot(fill="red")+
  labs(title = "Boxplot of Sweden's electricity supply for its internal market")+
  theme_minimal()
  
ggplot(Sweden, aes(x=Sweden)) + geom_histogram(fill="red", bins = 25)+
  labs(title = "Histogram of Sweden's electricity supply for its internal market")+
  theme_minimal()
# no outliers in data can be identified
# distribution doesn't have a particular strong left/right tail

# Data source: EUROSTAT, Energy statistics, 
#           Supply, transformation and consumption of electricity - monthly data
# Additional data: Daily Mean temeperature for Sweden, from openmeteo
# Zippenfenig, P. (2023). Open-Meteo.com Weather API [Computer software]. Zenodo. 
# https://doi.org/10.5281/ZENODO.7970649

# Avg. temp by month (total)
aggregate(x = mean_temp ~ months(Date), data = Sweden, FUN = mean)

# Deterministic model 1: linear trend + season (additive approach)--------------
Sweden$t <- 1:nrow(Sweden)
linmodel <- lm(Sweden ~ t, data = Sweden)
summary(linmodel)
# R^2: 7.8%, weak explanatory power

# checking Goodness of Fit
ggplot(data = Sweden, aes(x=Date)) +
  geom_line(aes(y = Sweden, colour="Available to internal market"),
            linewidth=1)+
  geom_line(aes(y = linmodel$fitted.values, colour="Linear Trend"), linewidth=1)+
  theme_minimal()

# adding in the seasonality effect with seasonal dummies
Sweden$months <- months(Sweden$Date)
Sweden$months <- as.factor(Sweden$months)

## dummy (seasonal-linear-"additive") model-------------------------------------
dummymodel <- lm(Sweden ~ t + months, data = Sweden)
summary(dummymodel)
# R^2: 94.17%, strong explanatory power

ggplot(data = Sweden, aes(x=Date)) +
  geom_line(aes(y = Sweden, colour="Available to internal market"),
            linewidth=1)+
  geom_line(aes(y = linmodel$fitted.values, colour="Linear Trend"), linewidth=1)+
  geom_line(aes(y = dummymodel$fitted.values, colour="Seasonal dummy"), linewidth=1)+
  theme_minimal()

# adding in the seasonality effect with contrast coding for additive model
contrasts(Sweden$months) # dummy coding
contrastmatrix <- contr.sum(12)
contrastmatrix
rownames(contrastmatrix) <- rownames(contrasts(Sweden$months))
contrastmatrix
colnames(contrastmatrix) <- c("April","August","December","February","January",
                              "July",'June','March','May','November','October')
contrastmatrix
contrasts(Sweden$months) <- contrastmatrix

## contrast (seasonal-linear-additive) model------------------------------------
contrastmodel <- lm(Sweden ~ t+months, data = Sweden)
summary(contrastmodel)
# R^2: 94.17%, strong explanatory power (same as dummymodel)

ggplot(data = Sweden, aes(x=Date)) +
  geom_line(aes(y = Sweden, colour="Available to internal market"),
            linewidth=1)+
  geom_line(aes(y = linmodel$fitted.values, colour="Linear Trend"), linewidth=1)+
  geom_line(aes(y = contrastmodel$fitted.values, colour="Seasonal contrast"), 
            linewidth=1)+
  theme_minimal()

# seems OK for now...

## Quadratic model (additive-contrast)------------------------------------------
quadraticmodel <- lm(Sweden ~ t + I(t^2) +months, data = Sweden)
summary(quadraticmodel)
# partial t-test suggests that the quadratic term is NOT significant in the
# population!

# comparing contrastmodel and quadratic one
broom::glance(contrastmodel)
broom::glance(quadraticmodel)
# based on AIC and BIC contrastmodel is better!
# Thus the contrastmodel is kept

## Exponential model (additive-contrast)----------------------------------------
exptrend <- lm(log(Sweden) ~ t, data = Sweden)
summary(exptrend)
ggplot(data = Sweden, aes(x=Date)) +
  geom_line(aes(y = Sweden, colour="Available to internal market"),
            linewidth=1)+
  geom_line(aes(y = linmodel$fitted.values, colour="Linear Trend"), linewidth=1)+
  geom_line(aes(y = exp(exptrend$fitted.values), colour="Exponential Trend"), 
            linewidth=1)+
  theme_minimal()

# for Trend fit comparison S_e will be used
se_e_lin <- sqrt(sum((Sweden$Sweden - linmodel$fitted.values)^2) / 
                   (nrow(Sweden)-length(linmodel$coefficients)))
se_e_exp <- sqrt(sum((Sweden$Sweden - exp(exptrend$fitted.values))^2) / 
                   (nrow(Sweden)-length(exptrend$coefficients)))
# 1896 (Linear) < 1902 (Exponential)
# Linear trend is better, moving on with contrastmodel!

## Mean temperature added model-------------------------------------------------
# Checking whether added variable (mean_temp) results in a significantly better
# model
tempmodel <- lm(Sweden ~ t +months + mean_temp, data = Sweden)
summary(tempmodel)
# R^2: 96.96%, very strong explanatory power, an increase from contrastmodel as
# expected (because of added variable)
# Partial t-test shows that temperature is significant even in the population

ggplot(data = Sweden, aes(x=Date)) +
  geom_line(aes(y = Sweden, colour="Available to internal market"),
            linewidth=1)+
  geom_line(aes(y = linmodel$fitted.values, colour="Linear Trend"), linewidth=1)+
  geom_line(aes(y = tempmodel$fitted.values, colour="Seasonal contrast with temperature"), 
            linewidth=1)+
  theme_minimal()

# Comparing with contrastmodel:
broom::glance(contrastmodel)
broom::glance(tempmodel)
# based on AIC and BIC tempmodel is better!
# Thus the tempmodel is kept

## STL decomposition------------------------------------------------------------
# Checking whether additive approach was suitable
Sweden_TS <- ts(Sweden$Sweden, start = c(2008,1), frequency = 12)
stldecomp <- stl(Sweden_TS, s.window = 7)

plot(stldecomp, main = "STL decomposition") # additive approach is reasonable as 
#                 STL decomposition shows additive approach when looking at
#                 seasonal component as it contains seasonal differences
# LOESS is used to estimate the trend component, some non-linear effects are needed
# that the tempmodel cannot capture --> can be seen in error term!
# For deterministic approach sticking with tempmodel is sensible, since it
# can be simply written as an equation, thus being deterministic.
# STL decomposition approach (or using LOESS for Trend) is great but introduces
# non-linearity as it is a smoothing technique.
# Not suitable for deterministic approach.

plot(decompose(x = ts(Sweden$Sweden, start = c(2008,1), frequency = 12),
          type = "additive"))
plot(decompose(x = ts(Sweden$Sweden, start = c(2008,1), frequency = 12),
               type = "multiplicative"))

# some seasonal effects are still reside in the error term...the stochastic
# school has an answer for this, but with the deterministic this is the most
# that can be explained
# checking this further with the autocorrelation of the tempmodel

# Autocorrelation in tempmodel--------------------------------------------------
# Using the Durbin-Watson (DW) test for first order autocorrelation
lmtest::dwtest(formula = tempmodel)
# H0: there is no first order autocorrelation
# p-value < 2.2e-16 ~ 0% --> reject H0, there is first order autocorrelation

# Using the Breusch-Godfrey test for serial autocorrelation (BG test)
lmtest::bgtest(formula = resid(tempmodel)~1, order = 12)
# H0: There is NO autocorrelation up to 12 (month) lags (a whole year)
# p-value = 1.412e-13 << 1% --> reject H0, there is autocorrelation

# It is known that there is some information in the residual term, meaning it can
# still be explained; it is NOT totally random!
# It can be said that the error term is NOT a White Noise (WN) process, need to
# continue modelling with stochastic principles, because with the deterministic
# approach this is the most that can be done, as here the error term is just an
# error.

acf(tempmodel$residuals)
# Additionally, ACF should equal to 0 for a WN residual, here it can be seen
# that it is not.

# Seasonal adjustment for structural breaks-------------------------------------
# In order to use Chow test to find structural breaks, a seasonally adjusted 
# model is needed.

## Manual calculation-----------------------------------------------------------
# Seasonal differences (observed - Trend):
trend_component <- coef(tempmodel)["(Intercept)"] + coef(tempmodel)["t"] * Sweden$t +
  coef(tempmodel)["mean_temp"] * Sweden$mean_temp

ggplot(data = Sweden, aes(x=Date)) +
  geom_line(aes(y = Sweden, colour="Available to internal market"),
            linewidth=1)+
  geom_line(aes(y = trend_component, colour="Trend"), linewidth=1)+
  theme_minimal()

Sweden$diff <- Sweden$Sweden - trend_component

# "raw" seasonal differences:
seasonaldiff <- aggregate(diff ~ months, data = Sweden, FUN = mean)
mean(seasonaldiff$diff)
# should be 0, it is -1.208278e-11 ~ 0, no need for correction

Sweden <- merge(x = Sweden, y = seasonaldiff[,c("months", "diff")], 
                by = "months",
               all.x = TRUE)
colnames(Sweden)[colnames(Sweden) == "diff.y"] <- "seasdiff"
colnames(Sweden)[colnames(Sweden)=='diff.x'] <- "diff"
Sweden <- Sweden[order(Sweden$Date),]

# Seasonal adjustment
Sweden$adjusted <- Sweden$Sweden - Sweden$seasdiff
plot.ts(Sweden$adjusted, main = "Seasonally Adjusted Data (manual)")

Sweden$estimated <- trend_component + Sweden$seasdiff

ggplot(data = Sweden, aes(x=Date))+
  geom_line(aes(y=Sweden, color="observed"), linewidth=1)+
  geom_line(aes(y=estimated, color="estimated"), linewidth=1)+
  theme_minimal()

struckbreaks <- strucchange::breakpoints(Sweden$adjusted~1)
length(struckbreaks$breakpoints)
# 1 big structural break

Sweden$Date[struckbreaks$breakpoints[1]]
# 2017-04 (April)
# Explanation: in paper, but shortly: no definite answer
#             Potentially: nuclear power plant final shutdown (2 parts)

## Making the final model with structural breaks--------------------------------
Sweden$sections <- "part1"
Sweden$sections[Sweden$Date > Sweden$Date[struckbreaks$breakpoints[1]]] <- "part2"
unique(Sweden$sections)
Sweden$sections <- as.factor(Sweden$sections)

strucmodel <- lm(Sweden ~ t +months + mean_temp + sections + t*sections, 
                 data = Sweden)
summary(strucmodel)
# R^2: 97.45%, extremely strong explanatory power

# Linear Trend part without temperatures (2 sections)
struc_trend <- lm(Sweden ~ t + sections + t*sections, data = Sweden)

# Check Goodness of Fit
ggplot(data = Sweden, aes(x=Date)) +
  geom_line(aes(y = Sweden, colour="Available to internal market"),
            linewidth=1)+
  geom_line(aes(y = struc_trend$fitted.values, colour="Structural trend w/o temp"), linewidth=1)+
  geom_line(aes(y = strucmodel$fitted.values, colour="Structural temp model"), 
            linewidth=1)+
  theme_minimal()

# compare this model to tempmodel
broom::glance(tempmodel)
broom::glance(strucmodel)
# based on AIC & BIC & Adjusted R^2 --> structrual model wins and is presented
# as best deterministic model to model Swedish electricity demand (available to
# internal markets, in gigawatt-hours)

# Chow-test for COVID and UA war, bonus-----------------------------------------
# could be logical that if e.g. the war has driven up electricity prices, then
# electricity available for internal markets in Sweden would drop, i.e. there
# would be a structural break in this regard

# COVID-19 Pandemic onset in Europe: March 2020
covid_break_index <- which(Sweden$Date == as.Date("2020-03-01"))
covid_break_index

# Outbreak of War in Ukraine, impacting European energy markets: February 2022
ukraine_break_index <- which(Sweden$Date == as.Date("2022-02-01"))
ukraine_break_index

# H0: No structural break at the specified point.
# H1: There is a structural break.

# Test for a break at the onset of COVID-19
chow_test_covid <- strucchange::sctest(tempmodel, type = "Chow", 
                                       point = covid_break_index)
chow_test_covid

# Test for a break at the start of the Ukraine War
chow_test_ukraine <- strucchange::sctest(tempmodel, type = "Chow", 
                                         point = ukraine_break_index)
chow_test_ukraine

# in both cases p-value >> 10%, i.e. all common significance levels, thus we 
# fail to reject H0, meaning there are no structural break at the previously 
# specified points

# Model shortcomings------------------------------------------------------------
# 1) Multicollinearity issue (before structural breaks)
car::vif(tempmodel)
# months    14.294622
# mean_temp 14.343331

# It is fairly obvious that e.g. in July it is warmer than in January. This 
# multicollinearity issue was expected.
# However, the parameter estimations remain UNBIASED!
# Here, mean_temp is still very much significant, even in the final model
# with structural breaks
# The only problem is efficiency...

# 2) Heteroskedasticity
# White test,
#   H0: Homoskedasticity is present
#   H1: Heteroskedasticity is present
skedastic::white(strucmodel, interactions = TRUE)
# p-value ~ 1 --> fail to reject H0, there is no heteroskedasticity present,
#                 homoskedastic residual term

# 3) Autocorrelation...
# error term is NOT WN! --> stochastic ARMA modelling

# Output for written analysis---------------------------------------------------
stargazer::stargazer(list(strucmodel),
                     type = "html", digits = 3, out = "./strucmodel.html")
