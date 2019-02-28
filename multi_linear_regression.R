# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # Multiple Linear Regression with 15 + predictors # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Fit the entire predictors of the dataset 
lm_normal = lm(Crime~., data = crime_data)

# Test the model with the data manually made for the validation
testing_point <- data.frame(M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0)

pred_result_lm_normal = predict (lm_normal, testing_point)
pred_result_lm_normal # value = 155.4

qqnorm(crime_data$Crime, ylab = "Crime")
min(crime_data$Crime) # value = 342
max(crime_data$Crime) # value = 1993 

# Test result says that the predictive value from the testing data point is 155.4. However, if we look up the original dataset the range of Crime value fall around 340 to about 2000.

summary(lm_normal)

# According to the p-values of the predictors, 'M', 'Ed', 'Po1', 'U2', 'Ineq', 'Prob' are relavent. Let's fit the new model with those predictors.

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # Multiple Linear Regression with 6 relavent predictors # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

lm_normal_2nd = lm(Crime~M+Ed+Po1+U2+Ineq+Prob, crime_data)
summary(lm_normal_2nd) # R2 = 0.766 and R2_adj = 0.731

# Test the model the data manually made for the validation
testing_point_2nd <- data.frame(M = 14.0, Ed = 10.0, Po1 = 12.0, U2 = 3.6, Ineq = 20.1, Prob =0.04)

# predict the crime rate for the test data point
pred_result_lm_normal_2nd <- predict(lm_normal_2nd, testing_point_2nd)
pred_result_lm_normal_2nd # value = 1304

# Test result seems more reasonable with those 6 predictors

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # Multiple Linear Regression with cross validation # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# DAAG = Data Analysis and Graphics Data and Functions -> Various data sets used in examples and exercises 
install.packages('DAAG')
library('DAAG')


set.seed(1) # Set the random seed
lm_crossVal = cv.lm(crime_data, lm_normal_2nd, m=4)

# # # # # # # # # # # # # # # # # # # # # #  
# # # # Principal Component Analysis # # # # 
# # # # # # # # # # # # # # # # # # # # # # 

principal_data = prcomp(crime_data[,1:15], scale. = TRUE)
summary(principal_data)

screeplot(principal_data, type="lines")

# Get the variances and proportion of variances from the principle component 
# get back eigenvalues, square the standard deviation
variety = (principal_data$sdev)^2

# get proportional variance by dividing each eigenvalue by the sum of eigenvalues 
proportional_variance = variety/(sum(variety))

plot(proportional_variance,
     xlab = "Principal Component Variable",
     ylab = "Variances Ratio",
     ylim = c(0,1) , type= "b")

# Determine the relavent principle variables. According to Kaiser method, any standard deviation greater than one is relavent 
screeplot(principal_data ,main = "Scree Plot", type = "line")
abline(h=1, col="red")

# Geting first 5 PCs
pca_var = principal_data$x[,1:5]
attributes(principal_data$x)
principal_data$x

# fit the model with 5 PCs
pca_crime = cbind(pca_var, crime_data[,16])

lm_pca = lm(V6~., data = as.data.frame(pca_crime))
summary(lm_pca)

# Getting coef from original data from PCA coef
beta0 <- lm_pca$coefficients[1]
betas <- lm_pca$coefficients[2:6]

beta0
## (Intercept) 
## 905 

betas
##   PC1    PC2    PC3    PC4    PC5 
##  65.2  -70.1   25.2   69.4 -229.0 

# Transform the PC coef into coef for the original variables 
alphas <- principal_data$rotation[,1:5] %*% betas 

t(alphas) # this is from scaled data. We have to convert back to the original data

##  M   So   Ed Po1 Po2   LF M.F  Pop   NW   U1   U2 Wealth Ineq  Prob Time
## 60.8 37.8 19.9 117 111 76.3 108 58.9 98.1 2.87 32.3   35.9 22.1 -34.6 27.2

original_alpha <- alphas/sapply(crime_data[,1:15],sd)
original_beta0 <- beta0 - sum(alphas*sapply(crime_data[,1:15],mean)/sapply(crime_data[,1:15],sd))

etm <- as.matrix(crime_data[,1:15]) %*% original_alpha + original_beta0
etm # estimates 

# calculate R^2 and R^2_adj

SSE = sum((etm - crime_data[,16])^2)
SStot = sum((crime_data[,16] - mean(crime_data[,16]))^2)
1 - SSE/SStot # 0.645 

R2 <- 1 - SSE/SStot
R2 - (1 - R2)*4/(nrow(crime_data)-4-1) # 0.611

## R2 = 0.645 and R2_adj = 0.611

## Conclustion: Since the size of the data is really small. The PCA regression model is under-performaing. However, if we have more data, it will work better. 
