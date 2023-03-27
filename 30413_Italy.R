##### 30413 ECONOMETRICS ASSIGNMENT Y22/23 ####

# Alessandro Morosini - 3149076
# Francesco Vacca - 3140929
# Tancredi Dorsi - 3161375


################################### SET-UP #####################################


# Clear the workspace
rm(list = ls())

# Install and loading packages
if (!require("rstudioapi")) install.packages("rstudioapi")
library(rstudioapi)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("farver")) install.packages("farver")
library(farver)
if (!require("lmtest")) install.packages("lmtest")
library(lmtest)
if (!require("plotly")) install.packages("plotly")
library(plotly)
if (!require("reshape2")) install.packages("reshape2")
library(reshape2)
if (!require("olsrr")) install.packages("olsrr")
library(olsrr)
if (!require("tseries")) install.packages("tseries")
library(tseries)
if (!require("ggnewscale")) install.packages("ggnewscale")
library(ggnewscale)
if (!require("forecast")) install.packages("forecast")
library(forecast)
if (!require("glmnet")) install.packages("glmnet")
library(glmnet)
if (!require("styler")) install.packages("styler")
library(styler)
if (!require("MASS")) install.packages("MASS")
library(MASS)

# Set directories
current_path <- rstudioapi::getActiveDocumentContext()$path
current_dir <- dirname(current_path)
data_dir <- file.path(current_dir, "data", "clean")


#################################### DATA ######################################
# Load the data and create a do some preliminary visualization

# Load data for Taylor Regression
interest_rate <- read.csv(
  file.path(data_dir, "interest_rate.csv"),
  sep = "\t"
)

inflation_rate <- read.csv(
  file.path(data_dir, "inflation_rate.csv"),
  sep = "\t"
)

inflation_target <- read.csv(
  file.path(data_dir, "inflation_target.csv"),
  sep = "\t"
)

output_gap <- read.csv(
  file.path(data_dir, "output_gap.csv"),
  sep = "\t"
)

gdp <- read.csv(
  file.path(data_dir, "gdp.csv"),
  sep = "\t"
)

gap <- inflation_rate[, 2] - inflation_target[, 2]
inflation_gap <- data.frame(
  year = seq(1980, 2002),
  inflation_gap = inflation_rate[, 2] - inflation_target[, 2]
)

# Create dataframe containing relevant data
df <- merge(interest_rate, output_gap, by = "year")
df <- merge(df, gdp, by = "year")
df <- merge(df, inflation_gap, by = "year")

# Plot interest rates
ggplot(interest_rate, aes(x = year, y = interest_rate)) +
  geom_point() +
  labs(
    title = "Italian Interest Rates 1980-2002",
    x = "Year",
    y = "Interest Rate"
  ) +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)

# Plot GDP
ggplot(gdp, aes(x = year, y = gdp)) +
  geom_point() +
  labs(
    title = "Italian GDP 1980-2002",
    x = "Year",
    y = "Gdp"
  ) +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)

# Plot output gap, i.e. difference between gdp and potential gdp
ggplot(output_gap, aes(x = year, y = output_gap)) +
  geom_point() +
  labs(
    title = "Italian Output Gap 1980-2002",
    x = "Year",
    y = "Output Gap"
  ) +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)

# Plot inflation gap, i.e. difference between actual inflation and target
ggplot(inflation_gap, aes(x = year, y = inflation_gap)) +
  geom_point() +
  labs(
    title = "Italian Inflation Gap 1980-2002",
    x = "Year",
    y = "Inflation Gap"
  ) +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)

# Plot inflation rate
ggplot(inflation_rate, aes(x = year, y = inflation_rate)) +
  geom_point() +
  labs(
    title = "Italian Inflation Rate 1980-2002",
    x = "Year",
    y = "Inflation Rate"
  ) +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)


############################### TAYLOR REGRESSION ##############################
# The idea is to regress interest rate on output gap and inflation gap


# A 3d box plot of the three variable follows
plot_ly(
  data = df,
  x = ~output_gap,
  y = ~inflation_gap,
  z = ~interest_rate,
  type = "scatter3d",
  mode = "markers",
  marker = list(color = c("black", "black", "black"))
)

# Run OLS regression using taylor rule
taylor_reg <- lm(interest_rate ~ output_gap + inflation_gap, data = df)
summary(taylor_reg)

# Draw the fitting hyper-plane
axis_x <- seq(min(df$output_gap), max(df$output_gap), by = 0.05)
axis_y <- seq(min(df$inflation_gap), max(df$inflation_gap), by = 0.05)

taylor_surface <- expand.grid(output_gap = axis_x, inflation_gap = axis_y, KEEP.OUT.ATTRS = F)
taylor_surface$interest_rate <- predict.lm(taylor_reg, newdata = taylor_surface)
taylor_surface <- acast(taylor_surface, inflation_gap ~ output_gap, value.var = "interest_rate")

fit_plot <- plot_ly(
  data = df,
  x = ~output_gap,
  y = ~inflation_gap,
  z = ~interest_rate,
  type = "scatter3d",
  mode = "markers",
  marker = list(color = c("black", "black", "black"))
)
fit_plot <- add_trace(
  p = fit_plot,
  z = taylor_surface,
  x = axis_x,
  y = axis_y,
  type = "surface"
)
fit_plot

# Draw fitted values vs real values
ggplot(df) +
  aes(x = year) +
  geom_point(aes(y = interest_rate, color = "Interest Rate"), size = 3) +
  geom_point(aes(y = fitted(taylor_reg), color = "Fitted Value"), size = 3) +
  scale_color_manual(name = "Legend", values = c("Interest Rate" = "blue", "Fitted Value" = "red")) +
  labs(x = "Year", y = "Interest Rate") +
  theme_minimal()


# Do some comments of the model here
# interepreation of coefficients, fitted values ..


################################### TESTS ######################################
# We will mostly work on residuals to run tests on the OLS assumptions.


# Preliminary visualization of residuals
residuals <- residuals(taylor_reg)
residuals <- data.frame(residuals)
residuals$ind <- rep(1980:2002)
par(mfrow = c(2, 2))
plot(taylor_reg)
par(mfrow = c(1, 1))

# TEST FOR LINEARITY

# run Reset Test
resettest(taylor_reg, power = 2, type = "fitted")
resettest(taylor_reg, power = 3, type = "fitted")
resettest(taylor_reg, power = 4, type = "fitted")

# at a 5% significance level, we do not reject the null hypothesis that
# the relationship is linear (for the three power tested).


# TEST FOR NORMALITY OF RESIDUALS

# Plot distribution of errors
ggplot(residuals) +
  aes(x = residuals) +
  geom_histogram(aes(y = stat(count) / sum(count)), colour = "black", fill = "white", bins = 10) +
  geom_density(alpha = .3, fill = "#FF6666", size = 1) +
  labs(
    title = "Residuals",
    x = "Residual Value",
    y = "Relative Frequency"
  )

# Run Jarque Bera Test
jarque.bera.test(residuals(taylor_reg))

# at a 5% significance level, we do not reject the null hypothesis that
# that the errors are normally distributed.


# TEST FOR HETEROSCEDASTICITY

# Plot residuals over time
ggplot(data = residuals) +
  aes(y = residuals(taylor_reg), x = ind) +
  geom_point() +
  stat_smooth(method = "lm", alpha = 0, formula = y ~ x)

# Run Goldfeld-Quandt Test with different alternative hypoteses
gqtest(taylor_reg, point = 0.5, alternative = "less", fraction = 0.2, order.by = df$year)
gqtest(taylor_reg, point = 0.5, alternative = "greater", fraction = 0.2, order.by = df$year)
gqtest(taylor_reg, point = 0.5, alternative = "two.sided", fraction = 0.2, order.by = df$year)

# at a 5% significance level, we do not reject the null hypothesis that
# the errors are homoscedastic over the years.
# Moreover, as the normality condition holds (as shown out by by Jarque-Bera test),
# we can also run Breush Pagan Test

bptest(taylor_reg) # varformula = output + inflation

# The results confirms our findings form the previous test.


# TEST FOR CORRELATION OF RESIDUALS

# Plot autocorrelation of errors
taylor_acf <- acf(residuals, plot = FALSE)
plot(
  taylor_acf$lag,
  taylor_acf$acf,
  type = "h",
  xlab = "Lag",
  ylab = "Autocorrelation",
  main = "Autocorrelation Plot",
  xlim = c(0, 8),
)

# run Durbin-Watson Test for serial correlation

dwtest(taylor_reg)

# the test rejects the null hypothesis that errors are uncorrelated with respect to
# the previous year. We can also test for correlation up to longer time span.

# run Breusch-Godfrey Test for serial correlation up to 5 years
bgtest(taylor_reg, 1)
bgtest(taylor_reg, 2)
bgtest(taylor_reg, 3)
bgtest(taylor_reg, 4)
bgtest(taylor_reg, 5)

# At a 5% confidence level, we do reject the null hypothesis that errors are serially
# uncorrelated up to a order 3 year time span.
# This evidence of serial correlation hints to a likely problem of omitted variables.
# This is why we proceed to extend the model.


############################### EXTENDED MODEL #################################
# The idea is to start from scratch.
# We try to predict the interest rate from a very simple model and we add regressors.
# We test their significance and see how the model performance varies.
# We expect the model explainability to increase as we introduce new regessors,
# but we must pay attention to incur in the trap of overfitting.


# Based also on the observations derived from Sarcinelli's paper, we will start by adding exchange rate
# to the model, as up until the mid-90s it was considered a measure much more impactful on the interest rates
# than the inflation rate

# Load some additional data

exchange_rate <- read.csv(
  file.path(data_dir, "exchange_rate.csv"),
  sep = "\t"
)

unemployment_rate <- read.csv(
  file.path(data_dir, "unemployment_rate.csv"),
  sep = "\t"
)

us_bond_yield <- read.csv(
  file.path(data_dir, "us_bond_yield.csv"),
  sep = "\t"
)

us_gdp <- read.csv(
  file.path(data_dir, "us_gdp.csv"),
  sep = "\t"
)

terms_of_trade <- read.csv(
  file.path(data_dir, "terms_of_trade.csv"),
  sep = "\t"
)

# Extent dataset
df <- merge(df, exchange_rate, by = "year")
df <- merge(df, unemployment_rate, by = "year")
df <- merge(df, us_bond_yield, by = "year")
df <- merge(df, us_gdp, by = "year")
df <- merge(df, terms_of_trade, by = "year")

# add Lira/USD Exchange Rate as explanatory variable
extended_reg <- lm(
  interest_rate ~ output_gap + inflation_gap + exchange_rate,
  data = df
)
summary(extended_reg)

# add Unemployment Rate as explanatory variable
extended_reg <- lm(
  interest_rate ~ output_gap + inflation_gap + exchange_rate + unemployment_rate,
  data = df
)
summary(extended_reg)

# add American Bond Yield as explanatory variable
extended_reg <- lm(
  interest_rate ~ output_gap + inflation_gap + exchange_rate + unemployment_rate + us_bond_yield, 
  data = df
)
summary(extended_reg)

# add Foreign Output (US)  as explanatory variable
extended_reg <- lm(
  interest_rate ~ output_gap + inflation_gap + exchange_rate + unemployment_rate + us_bond_yield, 
  data = df
)
summary(extended_reg)

# add Terms of Trade as explanatory variable
extended_reg <- lm(
  interest_rate ~ output_gap + inflation_gap + exchange_rate + unemployment_rate + us_bond_yield + terms_of_trade,
  data = df
)
summary(extended_reg)



######################### RIDGE AND LASSO REGRESSION ###########################
# The idea is to run Shrinkage Regression Models on the latter model which contains many regressors.

# Lasso
y <- df$interest_rate
x <- data.matrix(
  df[, c("output_gap", "inflation_gap", "exchange_rate", "unemployment_rate", "us_bond_yield", "us_gdp", "terms_of_trade")]
)

cv_model_lasso <- cv.glmnet(x, y, alpha = 1)
best_lambda_lasso <- cv_model_lasso$lambda.min
plot(cv_model_lasso)

best_model_lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda_lasso)
coef(best_model_lasso)

# Ridge
y <- df$interest_rate
x <- data.matrix(
  df[, c("output_gap", "inflation_gap", "exchange_rate", "unemployment_rate", "us_bond_yield", "us_gdp", "terms_of_trade")]
)

cv_model_ridge <- cv.glmnet(x, y, alpha = 0)
best_lambda_ridge <- cv_model_ridge$lambda.min
plot(cv_model_ridge)

best_model_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge)
coef(best_model_ridge)


############################# FEATURE SELECTION ################################


# Let's try with Backwards, Forward and Best Subset selection

taylor_complete <- lm(interest_rate ~ output_gap + inflation_gap + exchange_rate + unemployment_rate + us_bond_yield + terms_of_trade, data = df)


# Best Subset

k <- ols_step_all_possible(taylor_complete)
plot(k) # check

mod <- ols_step_best_subset(taylor_complete)
mod

plot(mod)

# Forward Selection

stepAIC(taylor_complete, direction = 'forward')

# Backwards Selection

stepAIC(taylor_complete, direction = 'backward')

# Both 

stepAIC(taylor_complete, direction = 'both') 

#From best subset selection we get that the best subset is the one containing
# 1. Output gap
# 2. Exchange rate
# 3. Unemployment rate
# 4. Us bond yield

