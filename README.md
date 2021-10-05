# Production Prediction from Multivariate Unconventional data
We explore a multivariate data set consisting of 1,000 different wells in unconventional formations in a certain oil and gas basin of the US. This data set contains 8 different measurements of rock properties, including Porosity, Log Permeability, Acoustic Impedance, Brittleness, Total Organic Carbon, Vitrinite Reflectance, and Production. The goal is to predict Production based on the other petrophysical rock properties, making this a regression problem.
<p align="center"> <img src="https://github.com/misaelmmorales/MV-Production-Prediction/blob/main/images/corr_mat.png"> </p>

The different statistical learning methods used in this predictive modeling are linear regression, LASSO regularization, decision trees, boosting, random forest, and principal component analysis. We ultimately see that most of these methods agree with the prediction of production in this unconventional data set, where Brittleness and Porosity are the main predictors of Production.
<p align="center"> <img src="https://github.com/misaelmmorales/MV-Production-Prediction/blob/main/images/prod_por_britt.png"><img src="https://github.com/misaelmmorales/MV-Production-Prediction/blob/main/images/rel_score.png"> </p>

***
**Remarks:** 
- Porosity and Permeability are highly correlated features.
- Brittleness is especially important in unconventional production.

**Conclusion:** 
Several methods were used to analyze a multivariate data set with petrophysical properties of an unconventional reservoir and predict gas production. Initial guesses hinted at the idea of Porosity and Permeability were the main factors in production, but all methods agreed that the two best predictors for Production are actually Porosity and Brittleness. The best statistical learning method based on minimizing MSE was Random Forest, followed by Boosting and Decision Trees, and lastly Linear Regression and LASSO Regularization. PCA confirmed the results, and therefore we could use this confidently to predict Production as a function of predictor variables
<p align="center"> <img src="https://github.com/misaelmmorales/MV-Production-Prediction/blob/main/images/results_table.png"> </p>
