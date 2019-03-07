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

install.packages("tree")
library('tree')

tree_model = tree(Crime~., data = crime_data)
summary(tree_model)

# Regression tree:
# tree(formula = Crime ~ ., data = crime_data)
# Variables actually used in tree construction:
# [1] "Po1" "Pop" "LF"  "NW" 
# Number of terminal nodes:  7 
# Residual mean deviance:  47390 = 1896000 / 40 
# Distribution of residuals:
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -573.900  -98.300   -1.545    0.000  110.600  490.100 

# 4 predictors were choosed for our model 

tree_model$frame
#      var  n        dev      yval splits.cutleft splits.cutright
#1     Po1 47 6880927.66  905.0851          <7.65           >7.65
#2     Pop 23  779243.48  669.6087          <22.5           >22.5
#4      LF 12  243811.00  550.5000        <0.5675         >0.5675
#8  <leaf>  7   48518.86  466.8571                               
#9  <leaf>  5   77757.20  667.6000                               
#5  <leaf> 11  179470.73  799.5455                               
#3      NW 24 3604162.50 1130.7500          <7.65           >7.65
#6     Pop 10  557574.90  886.9000          <21.5           >21.5
#12 <leaf>  5  146390.80 1049.2000                               
#13 <leaf>  5  147771.20  724.6000                               
#7     Po1 14 2027224.93 1304.9286          <9.65           >9.65
#14 <leaf>  6  170828.00 1041.0000                               
#15 <leaf>  8 1124984.88 1502.8750  

plot(tree_model)
text(tree_model)

# Let's perform a pruning the tree. It may improve performance of the model. 
# I am going to use cross-validation method. We could assume the quality of model from
# the deviance of trees with different number of terminal nodes 

cross_val_tree = cv.tree(tree_model)
plot(cross_val_tree$size, cross_val_tree$dev, type = "b")

prune_tree = prune.tree(tree_model, best = 6)

plot(prune_tree) 
text(prune_tree)

# I would like to see the performance of the unpruned model first. Let's get the R^2

y_hat = predict(tree_model)
SSE = sum((y_hat-crime_data$Crime)^2) # 1895722
SST <- sum((crime_data$Crime - mean(crime_data$Crime))^2) # 6880928
R2 <- 1 - (SSE/SST)
R2 # 0.7244962

# Plot of actual vs. predicted crime values
plot(crime_data$Crime, y_hat)
abline(0,1)

# Plot the residuals
plot(crime_data$Crime, scale(y_hat - crime_data$Crime))
abline(0,0)

# -> without any validation or using test dataset. the R2 of the model is 0.724
# -> Let's see the performance of the result with proper validation step

prune.tree(prune_tree)$size
prune.tree(prune_tree)$dev

# > prune.tree(prune_tree)$size
# [1] 6 5 4 3 2 1
# > prune.tree(prune_tree)$dev
# [1] 2013257 2276670 2632631 3364043 4383406 6880928

# -> compare this to sum of squared errors in cross-validation 

cross_val_tree$size
cross_val_tree$dev

# > cross_val_tree$size
# [1] 7 6 5 4 3 2 1
# > cross_val_tree$dev
# [1] 6982833 7222409 6911081 7367316 7789484 7359048 8343310

# Apparently, our model is too bad to use it. It seems like it's overfitted. 
# Well if you see the result, the deviation of cross validation is worse than pruned model. It seems like it's overfitted. 
# It may because we have small set of data but large number of features. 
# We can't do much better than that; there won't be enough data points to build a regression model. 
# However, suppose we only have one branch tree with only two leaves? We could control this on our side. 

prune_tree_2nd = prune.tree(tree_model,best=2)
prune_tree_2nd
# node), split, n, deviance, yval
#      * denotes terminal node

# 1) root 47 6881000  905.1  
# 2) Po1 < 7.65 23  779200  669.6 *
# 3) Po1 > 7.65 24 3604000 1131.0 *

prune_tree_2nd$where
#  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 
#  2  3  2  3  3  3  3  3  2  2  3  2  2  2  2  3  2  3  3  3  2  2  3  3  2  3  2  3  3  2  2  3  2  3  3  3  2  2  2  3  2  2  2  3  2  3  3 

# It branches on `Po1` at the value of 7.65. Just like the previous model, the estimates are the average in the branch. 

y_hat_2nd = predict(prune_tree_2nd)
1 - sum((y_hat_2nd - crime_data$Crime)^2)/SST # 0.3629629

cross_val_tree_2nd <- cv.tree(prune_tree_2nd)
cross_val_tree_2nd$size
cross_val_tree_2nd$dev

# It branches on `Po1` at the value of 7.65. Just like the previous model, the estimates are the average in the branch. 
# And again, the model does not perform really well. 
# What I am going to try is that I will build a regression model for each leaf - I've used the average value. 
# And since our data is too small, using average value is not really working well.

prune_data_1 = crime_data[which(prune_tree_2nd$where == 2),]
prune_data_2 = crime_data[which(prune_tree_2nd$where == 3),]

# first model from leafs 
leaf_lm_1 = lm(Crime~.,data = prune_data_1)
summary(leaf_lm_1)

# I got 4 possiable siginificant feautres 
# Regression with 4 marginally significant features

leaf_lm_1_4s = lm(Crime~Ed+Pop+Prob+Time,data = prune_data_1)
summary(leaf_lm_1_4s)

# I got 2 possiable siginificant feautres 
# Regression with 2 marginally significant features

leaf_lm_1_2s = lm(Crime~Pop+Time,data = prune_data_1)
summary(leaf_lm_1_2s)


# second model from leafs 
leaf_lm_2 = lm(Crime~.,data = prune_data_2)
summary(leaf_lm_2)

# I got 4 possiable siginificant feautres 
# Regression with 4 marginally significant features

leaf_lm_2_4s = lm(Crime~Ed+Pop+Prob+Time,data = prune_data_2)
summary(leaf_lm_2_4s)

# I got 2 possiable siginificant feautres 
# Regression with 2 marginally significant features

leaf_lm_2_2s = lm(Crime~Pop+Time,data = prune_data_2)
summary(leaf_lm_2_2s)



