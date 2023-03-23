##### 30413 ECONOMETRICS ASSIGNMENT Y22/23 ####

# Alessandro Morosini - 3149076
# Francesco Vacca - 3140929
# Tancredi Dorsi - 3161375


# installing packages
# install.packages("rstudioapi")
# install.packages("ggplot2")


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





