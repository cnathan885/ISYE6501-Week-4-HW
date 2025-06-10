#-----Question 9.1----#

install.packages('gamlr')
library(gamlr)
#load data
data <- read.table('uscrime.txt', header=TRUE)
crime_rate <- data$Crime
predictors <- data[, -ncol(data)]
predictors

#perform pca
pca <- prcomp(data[,1:15], center = TRUE, scale.=TRUE)

summary(pca)

#visualize the principal components
plot(pca, type='line')

#set number of principal components
pc <- 4

pca_scores <- as.data.frame(pca$x[, 1:pc])

#create regression model using principal components
pca_model <- lm(crime_rate ~ ., data = pca_scores)

summary(pca_model)

#specify my model in terms of the original variables
pca_coeffs <- coef(pca_model)
pca_intercept <- pca_coeffs[1]
pca_component_coeffs <- pca_coeffs[-1]

pca_loadings <- pca$rotation[, 1:pc]

#get scaled means and standard deviations used in PCA
scaled_center <- pca$center
scaled_scale <- pca$scale

#calculate coefficients for original variable, initialize to zero
original_coeffs <- rep(0, ncol(predictors))
names(original_coeffs) <- colnames(predictors)

#calculate new coefficients for original variables
for (j in 1:ncol(predictors)) {
  for (k in 1:pc) {
    original_coeffs[j] <- original_coeffs[j] + (pca_component_coeffs[k] * pca_loadings[j, k] / scaled_scale[j])
  }
}

#find new intercept
new_intercept <- pca_intercept - sum(original_coeffs * scaled_center)

new_intercept


#comparing with model from question 8.2
pca_adj_r_squared <- summary(pca_model)$adj.r.squared
pca_adj_r_squared

pca_aic <- AIC(pca_model)
pca_bic <- BIC(pca_model)
pca_aic
pca_bic

corrected_aic <- AICc(pca_model)
corrected_aic












