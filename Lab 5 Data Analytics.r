library(e1071)     # For SVM
library(ggplot2)   # For plotting
library(caret)

raw_data <- read.csv('C:/Users/dasha/Downloads/NY-House-Dataset.csv')

df <- raw_data[, c("PRICE", "PROPERTYSQFT")]

df$PRICE <- suppressWarnings(as.numeric(df$PRICE))
df$SQFT <- suppressWarnings(as.numeric(df$PROPERTYSQFT))

df <- df[!is.na(df$PRICE) & !is.na(df$SQFT) & df$PRICE > 0 & df$SQFT > 0, ]


set.seed(123)
train_idx <- sample(seq_len(nrow(df)), size = 0.8 * nrow(df))
train <- df[train_idx, ]
test <- df[-train_idx, ]

svm_model <- svm(PRICE ~ SQFT, data = train)
svm_preds <- predict(svm_model, newdata = test)

# Train Linear Regression model
lm_model <- lm(PRICE ~ SQFT, data = train)
lm_preds <- predict(lm_model, newdata = test)

ggplot(NULL, aes(x = test$PRICE, y = svm_preds)) +
  geom_point(alpha = 0.6) +
  geom_abline(color = "pink") +
  labs(title = "SVM: Predicted vs Actual Price",
       x = "Actual Price", y = "Predicted Price")

ggplot(NULL, aes(x = test$PRICE, y = lm_preds)) +
  geom_point(alpha = 0.6) +
  geom_abline(color = "lightblue") +
  labs(title = "Linear Regression: Predicted vs Actual Price",
       x = "Actual Price", y = "Predicted Price")

svm_resid <- test$PRICE - svm_preds
lm_resid <- test$PRICE - lm_preds

ggplot(NULL, aes(x = test$SQFT, y = svm_resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "pink") +
  labs(title = "SVM Residuals", x = "Square Footage", y = "Residuals")

ggplot(NULL, aes(x = test$SQFT, y = lm_resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "lightblue") +
  labs(title = "Linear Regression Residuals", x = "Square Footage", y = "Residuals")
