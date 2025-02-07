library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("C:/Users/dasha/Downloads/NY-House-Dataset.csv")

dataset <- NY_House_Dataset

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset$PROPERTYSQFT[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(dataset)

## fit linear model
lmod <- lm(PRICE~PROPERTYSQFT, data = dataset)

lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod)

plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

#***additional code I did

install.packages("dplyr")  # Only needed once
library(dplyr)

# Data Cleaning
dataset <- dataset %>% filter(PRICE < 195000000)  # Remove extreme outlier in Price
dataset <- dataset %>% filter(PROPERTYSQFT > 100) # Remove very small properties

# Log transform for better model fit
dataset <- dataset %>%
  mutate(
    log_PRICE = log10(PRICE),
    log_PROPERTYSQFT = log10(PROPERTYSQFT + 1),  # Avoid log(0)
    log_BEDS = log10(BEDS + 1),
    log_BATH = log10(BATH + 1)
  )

dataset$log_PRICE <- log10(dataset$PRICE)
dataset$log_PROPERTYSQFT <- log10(dataset$PROPERTYSQFT + 1)  # Avoid log(0)
dataset$log_BEDS <- log10(dataset$BEDS + 1)
dataset$log_BATH <- log10(dataset$BATH + 1)

# Check if log columns exist
head(dataset)

# Fit three linear models
model1 <- lm(log_PRICE ~ log_PROPERTYSQFT, data = dataset)
model2 <- lm(log_PRICE ~ log_PROPERTYSQFT + log_BEDS, data = dataset)
model3 <- lm(log_PRICE ~ log_PROPERTYSQFT + log_BEDS + log_BATH, data = dataset)

# Function to print model summary and plots
analyze_model <- function(model, dataset, xvar, xlabel) {
  print(summary(model))
  
  # Scatter plot with regression line
  ggplot(dataset, aes_string(x = xvar, y = "log_PRICE")) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = "lm", col = "red") +
    labs(x = xlabel, y = "Log(Price)", title = paste("Regression: Log(Price) vs", xlabel))
  
  # Residual plot
  residuals_df <- data.frame(Fitted = fitted(model), Residuals = residuals(model))
  ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Fitted Values", y = "Residuals", title = "Residual Plot")
}

# Analyze models
analyze_model(model1, dataset, "log_PROPERTYSQFT", "Log(PropertySqFt)")
analyze_model(model2, dataset, "log_PROPERTYSQFT", "Log(PropertySqFt)")
analyze_model(model3, dataset, "log_PROPERTYSQFT", "Log(PropertySqFt)")

# Compare models
cat("\nModel 1 R-squared:", summary(model1)$r.squared)
cat("\nModel 2 R-squared:", summary(model2)$r.squared)
cat("\nModel 3 R-squared:", summary(model3)$r.squared)

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = PRICE)) +
  geom_point() + stat_smooth(method = "lm", col = "pink")


