# sys6018-competition-house-prices
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques

library(readr)  
library(dplyr) 
library(caret)

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

# Read in data
house_train <- read_csv("train.csv")
house_pred <- read_csv("test.csv")

# After preliminary examination of data set and its descriptions, we first study the relationship between house areas and SalePrice. Specifically, we look at variables LotArea, GrLivArea, GarageArea, OpenPorchSF and PoolArea. 

attach(house_train)

# We observe a moderately strong positive linear relationship between GrLivArea and SalePrice, OpenPorchSF and SalePrice as well as GarageArea and SalePrice. 

# filling NA values
house_pred_nafill <- house_pred
house_pred_nafill$GarageArea[is.na(house_pred_nafill$GarageArea)] <- mean(house_train$GarageArea)

# Normalizing some Data
house_train$QualNorm <- (OverallQual - mean(OverallQual))/sd(OverallQual)
house_train$GrLivAreaNorm <- (GrLivArea - mean(GrLivArea))/sd(GrLivArea)
# house_train$OpenPorchNorm <- as.numeric(OpenPorchSF > 0)
# house_train$PoolNorm <- as.numeric(PoolArea > 0)
# house_train$GarageNorm <- GarageArea/mean(GarageArea)
# house_train$BsmtNorm <- TotalBsmtSF/mean(TotalBsmtSF)
# house_train$new_old <- as.numeric(YearBuilt > 1995)
# house_train$LotAreaNorm <- LotArea/mean(LotArea)

house_pred$QualNorm <- (house_pred$OverallQual - mean(house_pred$OverallQual))/sd(house_pred$OverallQual)
house_pred$GrLivAreaNorm <- (house_pred$GrLivArea - mean(house_pred$GrLivArea))/sd(house_pred$GrLivArea)
# house_pred$GarageNorm <- house_pred$GarageArea/mean(house_pred$GarageArea)
# house_pred$BsmtNorm <- house_pred$TotalBsmtSF/mean(house_pred$TotalBsmtSF)
# house_pred$BsmtNorm <- house_pred$LotArea/mean(house_pred$LotArea)
# house_pred$new_old <-  as.numeric(house_pred$YearBuilt > 1995)



# CV
# set.seed(3)
# train <- createDataPartition(house_train$SalePrice, p=0.7, list=FALSE)
# training <- house_train[train, ]
# testing <- house_train[-train, ]

# feature enginering

# choosing variables OverallQual, GrLivArea, GarageArea and OpenPorchSF
# house_train_sub <- select(training,OverallQual, GrLivArea, GarageArea,OpenPorchSF, SalePrice,Neighborhood)
# house_pred_sub <- select(testing, OverallQual, GrLivArea, GarageArea, OpenPorchSF, SalePrice,Neighborhood)
# reg_output <- data.frame(house_pred_sub, knn(house_train_sub, house_pred_sub))


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

house_output <- data.frame(house_pred$Id, knn(norm_train_sub, norm_test_sub))
write.table(house_output, file = "house_nonpar.csv", row.names=F, col.names=c("Id", "SalePrice"), sep=",")
