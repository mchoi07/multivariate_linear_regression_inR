# # # #                            # # # #
# # # # Data Prep and manipulation # # # # 
# # # #                            # # # #

crime_data <- read.delim("uscrime.txt", header = TRUE)
# crime_data <- read.table("uscrime.txt, stringsAsFactors = FALSE, header = TRUE)

head(crime_data)

# M So   Ed  Po1  Po2    LF   M.F Pop   NW    U1  U2 Wealth Ineq     Prob    Time Crime
# 1 15.1  1  9.1  5.8  5.6 0.510  95.0  33 30.1 0.108 4.1   3940 26.1 0.084602 26.2011   791
# 2 14.3  0 11.3 10.3  9.5 0.583 101.2  13 10.2 0.096 3.6   5570 19.4 0.029599 25.2999  1635
# 3 14.2  1  8.9  4.5  4.4 0.533  96.9  18 21.9 0.094 3.3   3180 25.0 0.083401 24.3006   578
# 4 13.6  0 12.1 14.9 14.1 0.577  99.4 157  8.0 0.102 3.9   6730 16.7 0.015801 29.9012  1969
# 5 14.1  0 12.1 10.9 10.1 0.591  98.5  18  3.0 0.091 2.0   5780 17.4 0.041399 21.2998  1234
# 6 12.1  0 11.0 11.8 11.5 0.547  96.4  25  4.4 0.084 2.9   6890 12.6 0.034201 20.9995   682

# # # #                            # # # #
# # # #      Regression Tree       # # # # 
# # # #                            # # # #

# Let's fit the model without splitting data into training and testing sets

# # # #                            # # # #
# # # #        Random Forest       # # # # 
# # # #                            # # # #

install.packages("randomForest")
library("randomForest")

number_of_features = 4
random_forest = randomForest(Crime~., data = crime_data, mtry = number_of_features, importance = TRUE)
random_forest

y_hat_rfmodel = predict(random_forest)
SSE_rfmodel = sum((y_hat_rfmodel-crime_data$Crime)^2)
SST_rfmodel = sum((crime_data$Crime-mean(crime_data$Crime))^2)
R2 = 1 - SSE_rfmodel/SST_rfmodel
R2

importance(random_forest)
varImpPlot(random_forest)
