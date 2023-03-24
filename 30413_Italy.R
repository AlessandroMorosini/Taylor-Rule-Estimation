##### 30413 ECONOMETRICS ASSIGNMENT Y22/23 ####

# Alessandro Morosini - 3149076
# Francesco Vacca - 3140929
# Tancredi Dorsi - 3161375


# installing packages
# install.packages("rstudioapi")
# install.packages("ggplot2")
# install.packages("farver")
# install.packages("lmtest")
library(ggplot2)
library(farver)
library(lmtest)

# Set directories
current_path = rstudioapi::getActiveDocumentContext()$path
current_dir = dirname(current_path)
data_dir <- file.path(current_dir, "data", "clean")


# Load data for Taylor Regression
interest_rate <- read.csv(
  file.path(data_dir, 'interest_rate.csv'), 
  sep = "\t"
)

gdp <- read.csv(
  file.path(data_dir, 'gdp.csv'), 
  sep = "\t"
)

inflation_rate <- read.csv(
  file.path(data_dir, 'inflation_rate.csv'), 
  sep = "\t"
)

output_gap <- read.csv(
  file.path(data_dir, 'output_gap.csv'), 
  sep = "\t"
)

# Load some additional data
exchange_rate <- read.csv(
  file.path(data_dir, 'exchange_rate.csv'), 
  sep = "\t"
)

us_bond_yield <- read.csv(
  file.path(data_dir, 'us_bond_yield.csv'), 
  sep = "\t"
)

unemployment_rate <- read.csv(
  file.path(data_dir, 'unemployment_rate.csv'), 
  sep = "\t"
)


# Create dataframe 
data <- merge(interest_rate, gdp, by = "year")
data <- merge(data, inflation_rate, by = "year")
data <- merge(data, output_gap, by = "year")

# Run regression and test here

# Some data visualization 
ggplot(interest_rate, aes(x = year, y  = interest_rate)) + 
  geom_point() + 
  labs(
    title = 'Italian Interest Rates 1980-2002',
    x = 'Year',
    y = 'Interest Rate'
  ) 

ggplot(gdp, aes(x = year, y  = gdp)) + 
  geom_point() + 
  labs(
    title = 'Italian GDP 1980-2002',
    x = 'Year',
    y = 'Gdp'
  ) 

ggplot(inflation_rate, aes(x = year, y  = inflation_rate)) + 
  geom_point() + 
  labs(
    title = 'Italian Inflation Rate 1980-2002',
    x = 'Year',
    y = 'Inflation Rate'
  ) 

ggplot(output_gap, aes(x = year, y  = output_gap)) + 
  geom_point() + 
  labs(
    title = 'Italian Output Gap 1980-2002',
    x = 'Year',
    y = 'Output Gap'
  ) 


# Run OLS
taylor <- lm(interest_rate ~ output_gap + inflation_rate, data = data)
summary(taylor)


# Run tests for OLS assumptions

resettest(taylor,power = 2, type="fitted") # 2nd power significant?
resettest(taylor,power = 3, type="fitted")

# TESTING FOR NORMALITY OF RESIDUALS
jarque.bera.test(residuals(taylor_reg))     
#H0 is accepted with pval = 0.1991


# TESTING FOR HETEROSCEDASTICITY
# preliminary analysis: plotting residuals (ordered by year)
ggplot(data = res) + 
  aes(y = res, x = ind) + 
  geom_point() + 
  stat_smooth(method = 'lm', alpha = 0, formula = y~x)

#visual inspection suggesting downward trend, decreasing variance

#Goldfeld-Quandt test

# with point = 0.2
gqtest(taylor_reg, point = 0.2, alternative = 'less',order.by = df_short$GAP)
gqtest(taylor_reg, point = 0.2, alternative = 'less',order.by = df_short$INF)
gqtest(taylor_reg, point = 0.2, alternative = 'less',order.by = df_short$YEAR)

# with point = 0.4
gqtest(taylor_reg, point = 0.4, alternative = 'less',order.by = df_short$GAP)
gqtest(taylor_reg, point = 0.4, alternative = 'less',order.by = df_short$INF)
gqtest(taylor_reg, point = 0.4, alternative = 'less',order.by = df_short$YEAR)

#POINT = 0.4
#H0 is strongly rejected: we may have a problem of heteroscedasticity

#since by Jarque-Bera test (see above) normality hold, we can also try the BP test:
#Breush-Pagan Test (taking output & inflation into consideration)
bptest(taylor_reg) # varformula = output + inflation

# TESTING FOR CORRELATION OF RESIDUALS

# Durbin-Watson test for serial correlation 
dwtest(taylor_reg)

# Breusch-Godfrey test for serial correlation of order up to 3
bgtest(taylor_reg, 3)

# DW test strongly rejects H0,
# BG test (m = 3) rejects H0 down to alpha = 0.008

# Strong indicators of serial correlation hint to a likely problem of omitted variables.
# ...hence we proceed to extend the model.



