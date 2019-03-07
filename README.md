# multivariate_linear_regression_in R

Using the data describe below, I fit the regression model with several different method. 

* `multi_linear_regression.R` : This is just regular mult-regression model. I tested model in several ways. You can also see PCA technique - it didn't work well because of samll size of the data. 

* `tree_regression.R` : In CART (Classification and Regression Trees), we could use tree model for a regression method. 

* `random_forest_regression.R` : 

* `stepwise_lasso_elasticNet.R` :

## Data Description

Description
Criminologists are interested in the effect of punishment regimes on crime rates. This has been studied using aggregate data on 47 states of the USA for 1960. The data set contains the following columns:

* M		percentage of males aged 14–24 in total state population
* So		indicator variable for a southern state
* Ed		mean years of schooling of the population aged 25 years or over
* Po1		per capita expenditure on police protection in 1960
* Po2		per capita expenditure on police protection in 1959
* LF		labour force participation rate of civilian urban males in the age-group 14-24
* M.F		number of males per 100 females
* Pop		state population in 1960 in hundred thousands
* NW		percentage of nonwhites in the population
* U1		unemployment rate of urban males 14–24
* U2		unemployment rate of urban males 35–39
* Wealth		wealth: median value of transferable assets or family income
* Ineq		income inequality: percentage of families earning below half the median income
* Prob		probability of imprisonment: ratio of number of commitments to number of offenses
* Time		average time in months served by offenders in state prisons before their first release
* Crime		crime rate: number of offenses per 100,000 population in 1960
