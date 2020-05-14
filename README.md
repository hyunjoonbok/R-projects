# R Portfolio
R Portfolio of data science projects from either original work or revised for a study and learning purpose. Portfolio in this repo is presented in the form of .R and .Rmd(R-markdown) files.

For a curated list of more visually-pleasant portfolio with graphs, check out [My Portfolio Website](https://hyunjoonbok.github.io/)

For a detailed code example and images, please refer to readme file in each folder under framework names (*Work-In-Progress*).

*Note: Data used in the projects is for learning and demo purposes only*


<hr>


## Motivation / Thought Process
As R is often less preferrable compared to other languages in terms of less production-ready codes, I think R is still a very powerful language. I personally am fond of and use R for everyday analysis from simple EDA to creating stunning visualizations and building a complex ML/DL models. I think R has its strong advantage in looking at codes and results at a controlled enviornmenets.  

This repository was origianlly to have a record of project progress and my own learning process, but I found that it would be helpful to who wants to improve data-science skills to next-level using R language, as it contains a numerious real-life data science example and notebooks created by [@hyunjoonbok](https://www.linkedin.com/in/hyunjoonbok/) and codes borrowed from authors who produced state-of-the-art results.

I tried to include the usage of packages and methods that have been consistently used in actual industries, in order to to solve the problems (even if it's a toy example). The repo contatins use-cases that can be readily applied to many of real-world datasets. 

<hr>

## Table of contents
* [All Projects](#Projects)
* [Technologies](#technologies)
* [Libraries](#features)
* [Setup](#setup)
* [TO-DOs](#TO-DOs)
* [Contact](#contact)

<hr>

## Projects

<hr>

- ## Machine Learning
   ### [Titanic Survival ML Algorithm](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/First%20ML%20algorithm.Rmd): 
   <p>
   The famous Titanic ML problem tackeld using R. Touches complete steps neccesary for ML modeling process (data-loading, data-cleaning, EDA, featureing engineering, ML modeling, Cross Validation, Scoring, and Next steps).
   </p>
   Sep 7, 2015
   
   ### [Time Series ML using H2O](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/H2O%20for%20Machine%20Learning.R): 
   <p>
   Predicting a future beer sale number using a historical data. Using H20's AUTOML feature to easily obtint the state-of-the-art ensemble results, and plot the errors to improve. 
   </p>
   July 10, 2017
   
   ### [ML Model Interpretability with DALEX](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/ML%20Model%20Interpretability%20with%20DALEX.R): 
     #### [2nd DALEX R file](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/Model%20Interpretability%20with%20DALEX.R)
   May 25, 2018
   <p>
   Often times, ML models are critized as being black-box (untracakble complex inside that magiaclaly solves the problem). Here we look at the problem of predicting the apartment prices using Linear Regression, SVM, Random Forest, and get the pacakge DALEX to help look how much each variables affect this prediction. 
   </p>
   
   ### Employee Churn Modeling
   #### [ML Model with Caret to predict Employee Chrun](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/Predict%20Employee%20Churn%20with%20CARET.R)
   #### [ML Model with LIME to understand and prevent Employee Chrun](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/ML%20Model%20Interpretability%20with%20DALEX.R): 
   <p>
   With the help of powerful Caret pacakge that help build ML model . Has complete steps to pre-process, fine-tune, train, and get ROC curve. Then, I use LIME (Local Interpretable Model-Agnostic Explanation) to understand ML model created. Use H2O to initiate modeling, and with the help of LIME, it gives both global and local interpretation of predictor variables. It gives a clear visual explanation of variable importance and how model is affected by those. 
   </p>
   June 23, 2019

   ### [Naïve Bayes Classifier](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/Na%C3%AFve%20Bayes%20Classifier.R): 
   <p>
   The Naïve Bayes classifier is a simple probabilistic classifier which is based on Bayes Theorem but with strong assumptions regarding independence. Historically, this technique became popular with applications in email filtering, spam detection, and document categorization. Here, I built a simple classification model with Caret and H2O.  
   </p>
   Feb 5, 2018

   ### [Predict Bank's Term Deposit (Classification) using H2O](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/PRedict%20Term%20Deposit%20(Yes%20or%20No%20label)%20using%20H2o.R): 
   <p>
   Build a simple ML from H2O to predict which customers more likely to enroll in Bank's Term Deposit. Shows how random Grid Search combined with Stacked Ensembles is a very powerful combination  
   </p>
   Nov 27, 2019
   
   ### [The Ultimate XGBoost Guide](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/The%20Ultimate%20XGBoost%20Guide.R): 
   <p>
   Contatins a complete steps in model-building with XGBoost in R. From CV, grid-serach, hyperparameter tuning to feature selection, optimization, training/evaluation and Prediction. Solves a real-world binary classification problem. 
   </p>
   May 6, 2017   

   ### [Machine Learning Problem Solving Guide](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/Ultimate%20Machine%20Learning%20Problem%20Solving.R)(*dataset are not included*): 
   <p>
   Contatins a complete steps in model-building and explanation of what's actaully going on in ML. Using 4 different method/packages (PDP, ICE, LIME, Shapley), it shows how Machine Learning can be explainable in some sense.
   </p>
   May 6, 2020  

   ### [Predict Airplane arrival delay](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/Use%20H2o%20to%20predict%20arrival%20delay%20using%20historical%20airline%20data.R): 
   <p>
   Looking at a toy example here to see how we could use H2O to predict arrival delay using historical airline data with Destination to Chicago Airport. Give a easy glance how easily H2O package could be utilized in a simple ML problem.
   </p>
   Feb 19, 2018  

<hr>

- ## Deep Learning

   ### [Basic Concept of CNN using Keras](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/CNN_Basic_Concept_Layers.R): 
   <p>
   How to stack Keras layers to basic CNN model. Solve MNIST using CNN in just a few lines of code.
   </p>
   Dec 13, 2017
   
   ### [Cat vs Dog Image classifier using CNN in R (Production)](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/CNN_R_Production_2.R): 
   <p>
   A Complete steps to load images, generate, compile, train, and test model in CNN to solve the famous Cat vs Dog image classification
   </p>
   Jan 11, 2019   
  
   ### [Predict Telco Customer Chrun using Keras](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/DEEP%20LEARNING%20WITH%20KERAS%20TO%20PREDICT%20CUSTOMER%20CHURN.R): 
   <p>
   A real-world example to predict customer retention / churn using Telecommunication company's data. Data Pre-processing, modeling, evaluating, prediction, checking performance, model explanation, feature importance visualization. 
   </p>
   Apr 14, 2020  
   
   ### [Tensorflow Estimator API to build Regression Model](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/Estimator%20API.R): 
   <p>
   Use Tensorflow's low level API to build both a linear regression model and Deep NeuralNet with the toy dataset. Activation of Tensorboard in R interface is also introduced.      
   </p>
   Oct 29, 2018  
   
   ### [High performance KERAS LSTM Algorithm](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/High%20performance%20deep%20learning%20algorithm.R): 
   <p>
   Developt a State-of-the-Art Keras LSTM algorithm to predict a sunspot by connecting to the R TensorFlow backend. Perform Time Series Cross Validation using Backtesting with the rsample package rolling forecast origin resampling.
   </p>
   May 2, 2018    
   
   ### [Predict the number of hostipal opening & closure](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/Predict%20the%20number%20of%20hostipal%20opening%20&%20closure%20(Deeplearning).R): 
   <p>
   Use real compeitition-type tabular data to predict the number of hospital opening/closure. Followed by initial data pre-processing, Boruta package to perform feature selection and eventually use H2O's radnom grid-serach and deep-learning algorithm to build and evaluate the model.
   </p>
   Feb 26, 2019 
   
   ### [IMDB Movie Rating prediction using Keras RNN and LSTM in R](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/Prediction%20of%20Movie%20Review%20using%20RNN%20and%20LSTM.R): 
   <p>
   The famous Movie rating prediction problem tackled by Keras RNN and LSTM layers in R interface
   </p>
   Nov 30, 2016    

   ### [IMDB Movie Review Sentiment Analysis  using Keras RNN and LSTM in R](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/Sentiment%20Analysis%20for%20Movie%20review%20text%20using%20Keras.R): 
   <p>
   2-way polarity (positive, negative) classification system for Movie Review texts. Goes through word-tokenizatino, modeling and evaluation using Keras. 
   </p>
   Dec 5, 2016  

<hr>

- ## Time Series Forecasting & Anomaly Detection

<hr>

- ## Database & Pararell Computing

<hr>

- ## Text Mining / Social Media Analysis

<hr>

- ## Visualization (ggplot2)

<hr>

- ## Statistic Concepts with real-world examples

<hr>

## Common Libraries
* XGBoost
* h2o
* tidyquant
* ggplot2
* tidyverse
* h2o
* XGBoost
* h2o
* XGBoost
* h2o
* XGBoost
* h2o
* XGBoost
* h2o
* XGBoost
* h2o

<hr>

## Setup
* Simply click one of the R files above and copy/paste on your own R scripts. 
  * *Please make sure to change the working directory!*

<hr>

## TO-DOs
List of features ready and TODOs for future development
* R Shiny Apps - Using work data : _in progress_
* Kaggle examples : _in progress_
* Data cleaning .ipynbs : _in progress_

<hr>

## Contact
Created by [@hyunjoonbok](https://www.linkedin.com/in/hyunjoonbok/) - feel free to contact me!
