##### 30413 ECONOMETRICS ASSIGNMENT Y22/23 ####

# Alessandro Morosini - 3149076
# Francesco Vacca - 3140929
# Tancredi Dorsi - 3161375

# Clearing the workspace
rm(list = ls())

# installing packages
# install.packages("rstudioapi")
# install.packages("ggplot2")
# install.packages("farver")
# install.packages("lmtest")
library(ggplot2)
library(farver)
library(lmtest)

# Set directories
current_path <- rstudioapi::getActiveDocumentContext()$path
current_dir <- dirname(current_path)
data_dir <- file.path(current_dir, "data", "clean")


# Load data for Taylor Regression
interest_rate <- read.csv(
  file.path(data_dir, "interest_rate.csv"),
  sep = "\t"
)

gdp <- read.csv(
  file.path(data_dir, "gdp.csv"),
  sep = "\t"
)

inflation_rate <- read.csv(
  file.path(data_dir, "inflation_rate.csv"),
  sep = "\t"
)

output_gap <- read.csv(
  file.path(data_dir, "output_gap.csv"),
  sep = "\t"
)

# Load some additional data
exchange_rate <- read.csv(
  file.path(data_dir, "exchange_rate.csv"),
  sep = "\t"
)

us_bond_yield <- read.csv(
  file.path(data_dir, "us_bond_yield.csv"),
  sep = "\t"
)

unemployment_rate <- read.csv(
  file.path(data_dir, "unemployment_rate.csv"),
  sep = "\t"
)


# Create dataframe
data <- merge(interest_rate, gdp, by = "year")
data <- merge(data, inflation_rate, by = "year")
data <- merge(data, output_gap, by = "year")

# Run regression and test here

# Some data visualization
ggplot(interest_rate, aes(x = year, y = interest_rate)) +
  geom_point() +
  labs(
    title = "Italian Interest Rates 1980-2002",
    x = "Year",
    y = "Interest Rate"
  ) +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)

ggplot(gdp, aes(x = year, y = gdp)) +
  geom_point() +
  labs(
    title = "Italian GDP 1980-2002",
    x = "Year",
    y = "Gdp"
  ) +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)

ggplot(inflation_rate, aes(x = year, y = inflation_rate)) +
  geom_point() +
  labs(
    title = "Italian Inflation Rate 1980-2002",
    x = "Year",
    y = "Inflation Rate"
  ) +
  geom_smooth(formula = y ~ x, alpha = 0)

ggplot(output_gap, aes(x = year, y = output_gap)) +
  geom_point() +
  labs(
    title = "Italian Output Gap 1980-2002",
    x = "Year",
    y = "Output Gap"
  ) +
  geom_smooth(formula = y ~ x, alpha = 0)


# Run OLS
taylor <- lm(interest_rate ~ output_gap + inflation_rate, data = data)
summary(taylor)

# 3d plot of linear regression
# install.packages("reshape2")
# install.packages("plotly")

library(plotly)
library(reshape2)

# Graph Resolution
graph_reso <- 0.05

# Setup Axis
axis_x <- seq(min(data$output_gap), max(data$output_gap), by = graph_reso)
axis_y <- seq(min(data$inflation_rate), max(data$inflation_rate), by = graph_reso)

# Sample points
taylor_surface <- expand.grid(output_gap = axis_x, inflation_rate = axis_y, KEEP.OUT.ATTRS = F)
taylor_surface$interest_rate <- predict.lm(taylor, newdata = taylor_surface)
taylor_surface <- acast(taylor_surface, inflation_rate ~ output_gap, value.var = "interest_rate") # y ~ x

hcolors <- c("black", "black", "black")
iris_plot <- plot_ly(data,
  x = ~output_gap,
  y = ~inflation_rate,
  z = ~interest_rate,
  type = "scatter3d",
  mode = "markers",
  marker = list(color = hcolors)
)
iris_plot <- add_trace(
  p = iris_plot,
  z = taylor_surface,
  x = axis_x,
  y = axis_y,
  type = "surface"
)

iris_plot

# plotting fitted values(red) vs true values(black)
ggplot(data) +
  aes(x = year) +
  geom_point(aes(y = interest_rate)) +
  geom_point(aes(y = fitted(taylor)), color = "red")

# plotting density of residuals
res <- residuals(taylor)
res <- data.frame(res)
res$ind <- rep(1:23)

ggplot(res) +
  aes(x = res) +
  geom_histogram(aes(y = stat(count) / sum(count)), colour = "black", fill = "white", bins = 10) +
  geom_density(alpha = .3, fill = "#FF6666", size = 1) +
  labs(
    title = "Residuals",
    x = "Residual Value",
    y = "Relative Frequency"
  )



# Run tests for OLS assumptions

# TESTING FOR LINEARITY
resettest(taylor, power = 2, type = "fitted") # p-value = 6.335e-05
resettest(taylor, power = 3, type = "fitted")

# TESTING FOR NORMALITY OF RESIDUALS
jarque.bera.test(residuals(taylor))



# TESTING FOR HETEROSCEDASTICITY
# preliminary analysis: plotting residuals (ordered by year)
ggplot(data = res) +
  aes(y = res, x = ind) +
  geom_point() +
  stat_smooth(method = "lm", alpha = 0, formula = y ~ x)


# Goldfeld-Quandt test

# with point = 0.2
gqtest(taylor, point = 0.2, alternative = "less", order.by = data$output_gap)
gqtest(taylor, point = 0.2, alternative = "less", order.by = data$inflation_rate)
gqtest(taylor, point = 0.2, alternative = "less", order.by = data$year)

# with point = 0.4
gqtest(taylor, point = 0.4, alternative = "less", order.by = data$output_gap)
gqtest(taylor, point = 0.4, alternative = "less", order.by = data$inflation_rate)
gqtest(taylor, point = 0.4, alternative = "less", order.by = data$year)

# POINT = 0.4
# H0 is strongly rejected: we may have a problem of heteroscedasticity

# since by Jarque-Bera test (see above) normality hold, we can also try the BP test:
# Breush-Pagan Test (taking output & inflation into consideration)
bptest(taylor) # varformula = output + inflation

# TESTING FOR CORRELATION OF RESIDUALS

# Durbin-Watson test for serial correlation
dwtest(taylor)

# Breusch-Godfrey test for serial correlation of order up to 3
bgtest(taylor, 3)

# DW test strongly rejects H0,
# BG test (m = 3) rejects H0 down to alpha = 0.008

# Strong indicators of serial correlation hint to a likely problem of omitted variables.
# ...hence we proceed to extend the model.

# EXTENDED MODEL

# Additional Regressors:
# 1. Unemployment Rate
# 2. Lira/USD Exchange Rate
# 3. Foreign Interest Rates (US)
# 4. US 10-Year Treasury Rate
