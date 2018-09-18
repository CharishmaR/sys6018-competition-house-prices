library(readr)  
library(dplyr) 
library(caret)

# Read in data
house_train <- read_csv("train.csv")
house_pred <- read_csv("test.csv")

# Data exploration
# Feature Engineering
new_train <- logical()
new_pred <- logical()

# adding a column that states whether a house is considered "new" (built after 2000) or not
house_train$New <- as.numeric(house_train$YearBuilt > 2000 )
house_pred$New <- as.numeric(house_pred$YearBuilt > 2000)

# adding a column that states whether a house has a porch or not
house_train$HasOpenPorch <- as.numeric(house_train$OpenPorchSF > 0)
house_pred$HasOpenPorch <- as.numeric(house_pred$OpenPorchSF > 0)

# adding a column that states whether a house has a basement or not
house_train$isBasement <- as.numeric(house_train$TotalBsmtSF > 0)
house_pred$isBasement <- as.numeric(house_pred$TotalBsmtSF > 0)

# adding a column that states whether a house has been remodeled recently (during or after 1990) or not
house_train$isnewremodel <- as.numeric(house_train$YearRemodAdd > 1990)
house_pred$isnewremodel <- as.numeric(house_pred$YearRemodAdd > 1990)

# adding a column that states whether a house has a garage or not
house_train$isgarage <- as.numeric(house_train$GarageArea > 0)
house_pred$isgarage <- as.numeric(house_pred$GarageArea > 0)

# adding a column that states whether a house has a fireplace (one or more) or not
house_train$Fireplaces <- as.numeric(house_train$Fireplaces > 0)
house_pred$Fireplaces <- as.numeric(house_pred$Fireplaces > 0)

# Normalizing some Data
house_train$QualNorm <- (OverallQual - mean(OverallQual))/sd(OverallQual)
house_train$GrLivAreaNorm <- (GrLivArea - mean(GrLivArea))/sd(GrLivArea)
house_pred$QualNorm <- (house_pred$OverallQual - mean(house_pred$OverallQual))/sd(house_pred$OverallQual)
house_pred$GrLivAreaNorm <- (house_pred$GrLivArea - mean(house_pred$GrLivArea))/sd(house_pred$GrLivArea)

# testing with normalized data
norm_train_sub <- select(house_train, QualNorm, GrLivAreaNorm, Neighborhood, SalePrice)
norm_test_sub <- select(house_pred, QualNorm, GrLivAreaNorm, Neighborhood)
norm_output <- data.frame(testing, knn(norm_train_sub, norm_test_sub))

# Below are some things for testing out the model with CV
# norm_train_sub <- select(training, QualNorm, GrLivAreaNorm, OpenPorchNorm, Neighborhood, SalePrice)
# norm_test_sub <- select(testing, QualNorm, GrLivAreaNorm, OpenPorchNorm, Neighborhood)
# norm_output <- data.frame(norm_test_sub, knn(norm_train_sub, norm_test_sub))
# reg_output$reg_output_resid = abs(reg_output$SalePrice - reg_output$knn.house_train_sub..k_output..house_pred_sub.)
# norm_output$norm_output_resid = abs(norm_output$knn.norm_train_sub..norm_test_sub. - testing$SalePrice)
# mean(norm_output$norm_output_resid)
# plot(norm_output$SalePrice, norm_output$norm_output_resid)


# plots
attach(house_train)
par(mfrow = c(2, 3))
plot(GrLivArea, SalePrice)
plot(GarageArea, SalePrice)
plot(OpenPorchSF, SalePrice)
plot(BedroomAbvGr, SalePrice)
plot(YearBuilt, SalePrice)
# plot(Neighborhood, SalePrice)
plot(house_train$isgarage, SalePrice)
plot(house_train$isnewremodel, SalePrice)
plot(OverallQual, SalePrice)
table(house_train$isBasement)

# Data cleaning (missing values, outliers, etc.)
house_pred_nafill <- house_pred
house_pred_nafill$GarageArea[is.na(house_pred_nafill$GarageArea)] <- mean(house_train$GarageArea)

# Rationale for the selected statistical modeling methods
# Correct implementation and use of statistical modeling methods

# Parametric
# partitioning the train dataset into training and testing sections
train <- createDataPartition(house_train$SalePrice, p=0.6, list=FALSE)
training <- house_train[train, ]
testing <- house_train[-train, ]

# performing parametric modeling using a linear regression model
lm1 <- lm(SalePrice ~ GrLivArea + poly(OverallQual, 2) + GarageArea + HasOpenPorch + isnewremodel + New + YearBuilt, data = training)
summary(lm1)$r.squared
# r.squared value : 0.8303971

write.table(house_output, file = "house.csv", row.names=F, col.names=c("Id", "SalePrice"), sep=",")

# non-Parametric
# Define key functions
# Euclidean distance

edist <- function(a, b){
  d = 0
  for (i in (1:(length(a)))) {
    d = d + (a[[i]] - b[[i]]) ^ 2
  }
  return(sqrt(d))
}

# KNN
knn <- function(trainer, predictions){
  price_all <- c()
  
  for (i in 1:nrow(predictions)) {   
    distances =c()  # create some empty vectors we will fill in with necessary info
    saleprices = c()
    my_neighborhoods = c()
    
    variables = length(predictions[i,])-1 # get the number of variables to be analyzed
    for (j in 1:nrow(trainer)) {
      distances <- c(distances, edist(trainer[j,1:variables], predictions[i,1:variables])) # store distances in a vector
      saleprices <- c(saleprices, trainer[j, ]$SalePrice) # store the sale prices in a vector
      my_neighborhoods <- c(my_neighborhoods, trainer[j, ]$Neighborhood) # store the neighborhood of each comparison which will be used to subset the data
    }
    euclidean <- data.frame(distances, saleprices, my_neighborhoods) # combine the data we stored above
    euc_subset <- euclidean[predictions[i,]$Neighborhood == euclidean$my_neighborhoods,] # subset to only look at the houses which are the same neighborhood as the observation we are looking at
    number_in_neighborhood = dim(euc_subset)[1] # look at how many houses are in that neighborhood set
    k = sqrt(number_in_neighborhood) # use k as square root of the number of houses that match the neighborhood
    euclidean_new <- euc_subset[order(euc_subset$distances), ][1:k, ] # order on euclidian distance and only take the top k
    price_all <- c(price_all, mean(euclidean_new$saleprices)) # take the mean sale price of the k houses we selected and store it in the price vector
  }
  return(price_all)
}

# creating the ouput csv file
house_output <- data.frame(house_pred$Id, knn(norm_train_sub, norm_test_sub))
write.table(house_output, file = "house_nonpar.csv", row.names=F, col.names=c("Id", "SalePrice"), sep=",")

View(house_train)
