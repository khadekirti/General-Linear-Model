# General-Linear-Model
Applying GLM to predict online news popularity

Analysis of UCI online news popularity dataset: exploratory data analysis, prediction, evaluation and inference with generalized linear models. The dataset and associated information can be found: https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity The aim in this project is to focus on predicting the popularity of online articles published by Mashable over two years from 7th Jan 2013, as measured through the number of times an article was shared online.
 
In particular, predict popularity based on explanatory variables (features) which would be available before the publication of the article, as mentioned in the accompanying paper by Fernandes et al. (2015) who collected the dataset. The articles have already been heavily processed to produce a large set of numerical and categorical attributes – so I will be only using them in this project. No additional variables will be created.   
 
Two models will be created to predict the popularity: 

<b> Model-1 </b>: The first target is as it is recorded, as the number of times the articles is shared over the period. 

<b> Model-2 </b>: The second target is a binary variable which discretises the above. In Fernandes et al. (section 3.1), an article is considered popular if it exceeds 1400 shares. Here we will reduce that threshold slightly and define an article as popular if it is shared 1000 or more times.

No model or variable selection is to be performed as part of this assignment (except as specifically mentioned in questions), despite the reasonable temptation. However, transformation of variables is allowed, and variables can be removed to help compliance with assumptions or due to other problems with them.
 
The aim of the project is to develop a solid theoretical and practical understanding of the models, rather than to achieve the best possible performance. Even within the constraints above, a large number of models are possible, and the Fernandes et al. article indicates some of the many other options. The aim of the study as mentioned by Fernandes et al. (p536) are: (i) “Predicting such popularity is valuable for authors, content providers, advertisers and even activists/politicians (e.g., to understand or influence public opinion) (ii) “allowing (as performed in this work) to improve content prior to publication

Other aspects of the report: 
-	Give relevant equations and assumptions to describe the general linear model and the logistic regression model for multivariate data (assume X has dimension p and Y is a single variable in each case). Define any notation used.
-	Briefly describe the algorithms used to fit each of these models and their mathematical basis.
-	Explain the interaction between a continuous variable and a binary categorical variable
-	Perform exploratory data analysis as relevant to the construction of the regression models. Investigate and highlight any apparent structure in the data. 
-	Search for at least one reasonable use of an interaction term between two explanatory variables and include it in your model
-	Verify assumptions of a multiple regression
-	Apply relevant Transformations 
-	Evaluate the predictive performance of each of the above two models using two appropriate metrics

 
 
