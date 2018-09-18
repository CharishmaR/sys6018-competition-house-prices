# UVA MSDS F18 SYS 6018

# C1-13: sys6018-competition-house-prices

# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/overview

Current Stage: 
- ran KNN with normalized variables OverallQual, LotArea, and GrLivArea, subsetting by neighborhood
- result submitted to Kaggle; score: 0.18949

Questions to Ponder: 
- any suggestions / help with data cleaning and / or standardizing? 
- CV & how to measure the accuracy of KNN, thus compare different instances of KNN (the choice of values of K?). 
- I am still not sure if we should select predict variables to put into KNN (i.e.: feature engineering) or if we should simply use everything. KNN is actually not the best model for mixed types of predictor variables (categorical + quantitative). Currently I only choose variables that were used in our parametric model. I feel that using all variables will make the KNN computation even heavier. Do we have a protocal for choosing KNN's predictor variables? 
