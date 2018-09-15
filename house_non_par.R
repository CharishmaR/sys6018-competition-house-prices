# sys6018-competition-house-prices
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques

library(readr)  
library(dplyr) 
library(caret)

house_train <- read_csv("train.csv")
house_pred <- read_csv("test.csv")

# After preliminary examination of data set and its descriptions, we first study the relationship between house areas and SalePrice. Specifically, we look at variables LotArea, GrLivArea, GarageArea, OpenPorchSF and PoolArea. 

attach(house_train)

# We observe a moderately strong positive linear relationship between GrLivArea and SalePrice, OpenPorchSF and SalePrice as well as GarageArea and SalePrice. 


# filling NA values
house_pred_nafill <- house_pred
house_pred_nafill$GarageArea[is.na(house_pred_nafill$GarageArea)] <- mean(house_train$GarageArea)

# Normalizing some Data
house_train$QualNorm <- OverallQual/mean(OverallQual)
house_train$GrLivAreaNorm <- GrLivArea/mean(GrLivArea)
house_train$OpenPorchNorm <- as.numeric(OpenPorchSF > 0)
#house_train$GarageAreaNorm <- GarageArea/mean(GarageAreaNorm)


house_pred$QualNorm <- house_pred$OverallQual/mean(house_pred$OverallQual)
house_pred$GrLivAreaNorm <- house_pred$GrLivArea/mean(house_pred$GrLivArea)

# CV
# train <- createDataPartition(house_train$SalePrice, p=0.9, list=FALSE)
# training <- house_train[train, ]
# testing <- house_train[-train, ]

# feature enginering

# choosing variables OverallQual, GrLivArea, GarageArea and OpenPorchSF
# house_train_sub <- select(training,OverallQual,GrLivArea,SalePrice,Neighborhood)
# house_pred_sub <- select(testing,OverallQual,GrLivArea, SalePrice,Neighborhood)
# reg_output <- data.frame(house_pred_sub, knn(house_train_sub, house_pred_sub))


# testing the normal method vs with normalized data
norm_train_sub <- select(house_train,QualNorm,GrLivAreaNorm,Neighborhood,SalePrice)
norm_test_sub <- select(house_pred,QualNorm,GrLivAreaNorm,Neighborhood)
#norm_output <- data.frame(norm_test_sub, knn(norm_train_sub, norm_test_sub))
#reg_output$reg_output_resid = abs(reg_output$SalePrice - reg_output$knn.house_train_sub..k_output..house_pred_sub.)
#norm_output$norm_output_resid = abs(norm_output$SalePrice - norm_output$knn.norm_train_sub..k_output..norm_test_sub.)

#reg_ss = mean(reg_output$reg_output_resid)
#norm_ss = mean(norm_output$norm_output_resid)

#norm_ss/reg_ss

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

house_output <- data.frame(house_pred$Id, knn(norm_train_sub, norm_test_sub))
write.table(house_output, file = "house_nonpar_after.csv", row.names=F, col.names=c("Id", "SalePrice"), sep=",")
