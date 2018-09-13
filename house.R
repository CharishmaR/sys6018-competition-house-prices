# sys6018-competition-house-prices
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques

library(readr)  
library(dplyr) 
library(caret)

house_train <- read_csv("train.csv")
house_pred <- read_csv("test.csv")
# sample <- read_csv("sample_submission.csv")

# After preliminary examination of data set and its descriptions, we first study the relationship between house areas and SalePrice. Specifically, we look at variables LotArea, GrLivArea, GarageArea, OpenPorchSF and PoolArea. 

attach(house_train)
par(mfrow = c(2, 3))
plot(LotArea, SalePrice)
plot(GrLivArea, SalePrice)
plot(GarageArea, SalePrice)
plot(OpenPorchSF, SalePrice)
plot(PoolArea, SalePrice)

# We observe a moderately strong positive linear relationship between GrLivArea and SalePrice, OpenPorchSF and SalePrice as well as GarageArea and SalePrice. 

# We also factorize some other variables that proved to be significant: 

# YearBuilt -> New (in or after 2000) or Old (before 2000)
new_train <- logical()
new_pred <- logical()

for (i in 1:nrow(house_train)) {
  if (house_train[i, "YearBuilt"] >= 2000) {
    new_train[i] = TRUE
  }
  else {
    new_train[i] = FALSE
  }
}

for (i in 1:nrow(house_pred)) {
  if (house_pred[i, "YearBuilt"] >= 2000) {
    new_pred[i] = TRUE
  }
  else {
    new_pred[i] = FALSE
  }
}

house_train$New <- new_train
house_pred$New <- new_pred

# OpenPorch

op_train <- logical()
op_pred <- logical()

for (i in 1:nrow(house_train)) {
  if (house_train[i, "OpenPorchSF"] > 0) {
    op_train[i] = TRUE
  }
  else {
    op_train[i] = FALSE
  }
}

for (i in 1:nrow(house_pred)) {
  if (house_pred[i, "OpenPorchSF"] > 0) {
    op_pred[i] = TRUE
  }
  else {
    op_pred[i] = FALSE
  }
}

house_train$HasOpenPorch <- op_train
house_pred$HasOpenPorch <- op_pred

house_train$New <- factor(house_train$New)
house_pred$New <- factor(house_pred$New)
house_train$HasOpenPorch <- factor(house_train$HasOpenPorch)
house_pred$HasOpenPorch <- factor(house_pred$HasOpenPorch)


# CV
train <- createDataPartition(house_train$SalePrice, p=0.6, list=FALSE)
training <- house_train[train, ]
testing <- house_train[-train, ]


lm1 <- lm(SalePrice ~ GrLivArea + poly(OverallQual, 2) + GarageArea + HasOpenPorch + New, data = training)
summary(lm1)$r.squared
# 0.731582
# a moderately high value. 

# filling NA values
house_pred_nafill <- house_pred
house_pred_nafill$GarageArea[is.na(house_pred_nafill$GarageArea)] <- mean(house_train$GarageArea)

lm_all <- lm(SalePrice ~ GrLivArea + poly(OverallQual, 2) + GarageArea + HasOpenPorch + New, data = house_train)
house_output <- data.frame(house_pred$Id, predict(lm_all, newdata = house_pred_nafill))

write.table(house_output, file = "house.csv", row.names=F, col.names=c("Id", "SalePrice"), sep=",")
