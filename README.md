UVA Statistics Master's Final Exam
The objective of this project is to determine whether predicting one's likelihood of declaring default on a loan is feasible using various machine learning models.
Tested models are logistic, random forest, xgboost, lda,qda, and many more.
Based on AUC & Test error rate, the best-performing methods were logistic regression and xgboost.

However, the traditional test classification error and AUC are not reliable measures to assess performances due to the nature of the skewness of data.
Due to financial consequences, declaring defaults is not incentivized. Hence, the data suffers from skewness between the proportion of people declaring default or not.
One way to solve this problem is sampling equal distribution between defaults and non-defaults.
However, I think this isn't realistic b/c, in the real world, fewer people declare defaults and the # of people who declare defaults.

So, I wanted to change the scope to expected loss given the default. It would be financially more stable if defaults generally occurred on the lower outstanding balances of loans.
In this case, Xgboost performed better than logistic regression.

One thing to note is, that logistic regression is probably the simplest form of classification model. Whereas, xgboost is the most flexible before brining other advanced concepts like neural networks.
In a Machine learning perspective, it is a matter of controlling MSE which is bias^2 + variance. Logistic regression has lower variance but higher bias compared to xgboost yet the performance is similar.

I think the cause of this is the existence of bias underlying the dataset. In other words, this dataset is missing key variables that could be useful in predicting the likelihood. 
Thus, xgboost and logistic regression experienced similar performance despite their difference in flexibility.
Hence, additional variables are needed.

I think this project was a great opportunity to experience that quality data is more important than the models themselves
.

