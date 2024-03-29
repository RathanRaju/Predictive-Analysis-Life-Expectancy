# Predictive-Analysis-Life-Expectancy

#### Tools used - RStudio

- This project is about “Life Expectancy” provides data regarding the average Life Expectancy of several countries along with other information of the respective countries. 

- The project aims to create a model that predicts Life Expectancy, given , the rest of the predictive factors. 

- A general linear model predicts the value of a response variable , using a multitude of factors. 

- However such a method is challenged by the presents of missing values in the data , or collinearity between different predictors . 

- ‘Imputation’ is used to deal with missing values and collinearity is measured using variance inflation factor. 

- The ‘features or the predictor variables for the model are selected using stepwise selection from ‘Wrapper method for feature selection’. 

- The average life expectancy of the unknown countries is then predicted.One way analysis of Variance (ANOVA) is used to determine whether there are any statistically significant differences between the average life expectancies across continents.

- The missing values were removed using multiple imputations. 

- Multicollinearity was detected between  variables, namely, GDP per capita and Mortality Rate (Female) and Mortality Rate. 

- The feature  selection rendered a model with AIC around 859 and containing 11 predictor variables, considered as  the best model. 

- However, the model was further defined for predictions taking into account collinearity between few variables, leaving us with nine predictor variables. 

- The model fit value shows good significance measures and the predictions for eleven countries are obtained. 

- Three continents – Asia, Oceania and Americas has differences in average life expectancies which are not statistically significant. So, the null hypothesis has been rejected which says all populations means are equal.

