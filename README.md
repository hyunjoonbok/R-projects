# R Portfolio
R Portfolio of data science projects from either original work or revised for a study and learning purpose. Portfolio in this repo is presented in the form of .R and .Rmd(R-markdown) files.

Each folder represents the fields of application (i.e. Timeseries, Deeplearning, MachineLearning, etc)

For a detailed code example and images, please refer to .readme file presented below.

*Note: Data used in the projects is for learning and demo purposes only*

<hr>

## Motivation / Thought Process
These days R is less preferrable in industory for various reasons (i.e less production-ready, non-scalable). However, I think R is still a very powerful language. I personally am fond of and use R for everyday analysis from simple EDA to creating stunning visualizations and building a complex ML/DL models. I think R has its strong advantage in looking at codes and results at a controlled enviornmenets.  

This repository was origianlly to have a record of project progress and my own learning process, but I found that it would be helpful to who wants to improve data-science skills to next-level using R language, as it contains a numerious real-life data science example and notebooks created by [@hyunjoonbok](https://www.linkedin.com/in/hyunjoonbok/) and codes borrowed from authors who produced state-of-the-art results.

I tried to include the usage of packages and methods that have been consistently used in actual industries, in order to to solve the problems (even if it's a toy example). The repo contatins use-cases that can be readily applied to many of real-world datasets. 

<hr>

## Table of contents
* [Projects](#Projects)
* [Setup](#setup)
* [To-Do](#TO-DOs)
* [Contact](#contact)

<hr>

## Projects

   #### [(3) Time Series Forecasting with Modeltime (Advanced)](https://github.com/hyunjoonbok/R-projects/blob/master/Times%20Series%20Forecasting%20%26%20Anomaly%20Detection/%5BAdvanced%5D%20Time%20Series%20Forecasting%20with%20Nest%20%2B%20Modeltime_2020_11_23.R)   
   <p>
   This workbook covers complete advanced steps to create a SOTA time-series forecasting model at scale. We use Walmart M4 Kaggle competition dataset to create foreacst for (7) different time-series. It introduces latest functions in Modeltime and techniques in R, which load data, preprocess, modelling, fitting, calibration, ensembling, and visualization. The codes are experiment-ready to be applied to any of custom time-series dataset. 
   </p>


   #### [(2) Time Series Forecasting with Modeltime (+Nest) (Basic)](https://github.com/hyunjoonbok/R-projects/blob/master/Times%20Series%20Forecasting%20%26%20Anomaly%20Detection/Forecasting%20ARIMA%20Models%20with%20Nest%20%2B%20Modeltime_2020_11_21.R)   
   <p>
   Often times, it's necceary for business who are performing any kind of time-series forecast model to scale it's model. This examples leverages "Nest" function to create several time-series at the same time in a single dataset, where the best-chosen ML algorithem is applied to create a forecast for entire groups. The possibility is endless. The model can be scaled to create thousands of models in pararell, with the help of "Nest" function.
   </p>


   #### [(1) Time Series Forecasting with ModelTime - Walkthrough (Basic)](https://github.com/hyunjoonbok/R-projects/blob/master/Times%20Series%20Forecasting%20%26%20Anomaly%20Detection/Time%20Series%20Forecasting%20using%20ModelTime.R)   
   <p>
   The file walk-through key processes that need to be performed to generate time-series in high-level. We are looking bike_sharing_daily time series data from 2011 to 2013 to predict the sales of it for the next 3 months. We set aside last 3-months of data as the testing set, and levere modeltime package to build different SOTA timeseires models including, ARIMA, Prophet, XGBoost, randomforest. Then we evaluate the Model by refitting data from the errors we got from initial models, and eventually multi-visualize the model. 
   </p> 

   #### [Customer Segmentation & Clustering with K-means](https://github.com/hyunjoonbok/R-projects/blob/master/Customer%20Segmentation%20%26%20Clustering%20with%20K-means%2C%20Principal%20Components%20Analysis%20and%20Bootstrap%20Evaluation.R)   
   <p>
   Looking at custmer transcation data to segment cusotmers into groups to better statify the business strategy. Use a K-means clustering Building and Bootstrap Evaluation to effectively group cusotmers, and create points of strategy to be possibly discussed with business stakeholders.   
   </p>


   #### [Ultimate Muti-label Classification with H2O Deep Learning](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/Ultimate%20Muti-label%20Classification%20with%20H2O%20Deep%20Learning.R)   
   <p>
   Building a deeplearning model using H2O, perform hyperparameter tuning through random grid serach, to solve multi-label classification problem.  
   </p>


   #### [GameTitle-Recommender with Market-Basket-Analysis (1)](https://github.com/hyunjoonbok/R-projects/blob/master/Title_Recommender_with_market_basket_analysis/Market_Basket_Analysis_with_recommender%20(1)%20-%20Data%20Preparation.R)
   #### [GameTitle-Recommender with Market-Basket-Analysis (2)](https://github.com/hyunjoonbok/R-projects/blob/master/Title_Recommender_with_market_basket_analysis/Market_Basket_Analysis_with_recommender%20(2)%20-%20Recommendation%20System.R)
   #### [GameTitle-Recommender with Market-Basket-Analysis (3)](https://github.com/hyunjoonbok/R-projects/blob/master/Title_Recommender_with_market_basket_analysis/Market_Basket_Analysis_with_recommender%20(3)%20-%20WebApp.R)   
   <p>
   A End-to-End recommendation system model building using the game title from data wragling, to building an algorithm and deplying to Shiny WebApp. A full comprehension of recommender algorithm could be gained and can be applied to any real-world data.
   </p>
   
* Reference: [Diego Usai's Website](https://diegousai.io/categories/multi-article-studies/)   

<hr>

   - ## Machine Learning
      ### [Time Series ML using H2O](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/H2O%20for%20Machine%20Learning.R)
      <p>
      Predicting a future beer sale number using a historical data. Using H20's AUTOML feature to easily obtint the state-of-the-art ensemble results, and plot the errors to improve. 
      </p>

      ### [ML Model Interpretability with DALEX](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/ML%20Model%20Interpretability%20with%20DALEX.R)
        #### [2nd DALEX R file](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/Model%20Interpretability%20with%20DALEX.R)
      <p>
      Often times, ML models are critized as being black-box (untracakble complex inside that magiaclaly solves the problem). Here we look at the problem of predicting the apartment prices using Linear Regression, SVM, Random Forest, and get the pacakge DALEX to help look how much each variables affect this prediction. 
      </p>

      ### Employee Churn Modeling
      #### [ML Model with Caret to predict Employee Chrun](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/Predict%20Employee%20Churn%20with%20CARET.R)
      #### [ML Model with LIME to understand and prevent Employee Chrun](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/ML%20Model%20Interpretability%20with%20DALEX.R)
      <p>
      With the help of powerful Caret pacakge that help build ML model . Has complete steps to pre-process, fine-tune, train, and get ROC curve. Then, I use LIME (Local Interpretable Model-Agnostic Explanation) to understand ML model created. Use H2O to initiate modeling, and with the help of LIME, it gives both global and local interpretation of predictor variables. It gives a clear visual explanation of variable importance and how model is affected by those. 
      </p>

      ### [Naïve Bayes Classifier](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/Na%C3%AFve%20Bayes%20Classifier.R)
      <p>
      The Naïve Bayes classifier is a simple probabilistic classifier which is based on Bayes Theorem but with strong assumptions regarding independence. Historically, this technique became popular with applications in email filtering, spam detection, and document categorization. Here, I built a simple classification model with Caret and H2O.  
      </p>

      ### [Predict Bank's Term Deposit (Classification) using H2O](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/PRedict%20Term%20Deposit%20(Yes%20or%20No%20label)%20using%20H2o.R)
      <p>
      Build a simple ML from H2O to predict which customers more likely to enroll in Bank's Term Deposit. Shows how random Grid Search combined with Stacked Ensembles is a very powerful combination  
      </p>

      ### [The Ultimate XGBoost Guide](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/The%20Ultimate%20XGBoost%20Guide.R)
      <p>
      Contatins a complete steps in model-building with XGBoost in R. From CV, grid-serach, hyperparameter tuning to feature selection, optimization, training/evaluation and Prediction. Solves a real-world binary classification problem. 
      </p>  

      ### [Machine Learning Problem Solving Guide](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/Ultimate%20Machine%20Learning%20Problem%20Solving.R) (*data not included*) 
      <p>
      Contatins a complete steps in model-building and explanation of what's actaully going on in ML. Using 4 different method/packages (PDP, ICE, LIME, Shapley), it shows how Machine Learning can be explainable in some sense.
      </p>

      ### [Predict Airplane arrival delay](https://github.com/hyunjoonbok/R-projects/blob/master/Machine-Learning/Use%20H2o%20to%20predict%20arrival%20delay%20using%20historical%20airline%20data.R)
      <p>
      Looking at a toy example here to see how we could use H2O to predict arrival delay using historical airline data with Destination to Chicago Airport. Give a easy glance how easily H2O package could be utilized in a simple ML problem.
      </p> 

   <hr>

   - ## Deep Learning

      ### [Basic Concept of CNN using Keras](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/CNN_Basic_Concept_Layers.R)
      <p>
      How to stack Keras layers to basic CNN model. Solve MNIST using CNN in just a few lines of code.
      </p>

      ### [Cat vs Dog Image classifier using CNN in R (Production)](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/CNN_R_Production_2.R)
      <p>
      A Complete steps to load images, generate, compile, train, and test model in CNN to solve the famous Cat vs Dog image classification
      </p> 

      ### [Predict Telco Customer Chrun using Keras](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/DEEP%20LEARNING%20WITH%20KERAS%20TO%20PREDICT%20CUSTOMER%20CHURN.R)
      <p>
      A real-world example to predict customer retention / churn using Telecommunication company's data. Data Pre-processing, modeling, evaluating, prediction, checking performance, model explanation, feature importance visualization. 
      </p>

      ### [Tensorflow Estimator API to build Regression Model](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/Estimator%20API.R)
      <p>
      Use Tensorflow's low level API to build both a linear regression model and Deep NeuralNet with the toy dataset. Activation of Tensorboard in R interface is also introduced.      
      </p> 

      ### [High performance KERAS LSTM Algorithm](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/High%20performance%20deep%20learning%20algorithm.R)
      <p>
      Developt a State-of-the-Art Keras LSTM algorithm to predict a sunspot by connecting to the R TensorFlow backend. Perform Time Series Cross Validation using Backtesting with the rsample package rolling forecast origin resampling.
      </p>   

      ### [Predict the number of hostipal opening & closure](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/Predict%20the%20number%20of%20hostipal%20opening%20&%20closure%20(Deeplearning).R)
      <p>
      Use real compeitition-type tabular data to predict the number of hospital opening/closure. Followed by initial data pre-processing, Boruta package to perform feature selection and eventually use H2O's radnom grid-serach and deep-learning algorithm to build and evaluate the model.
      </p>

      ### [IMDB Movie Rating prediction using Keras RNN and LSTM in R](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/Prediction%20of%20Movie%20Review%20using%20RNN%20and%20LSTM.R)
      <p>
      The famous Movie rating prediction problem tackled by Keras RNN and LSTM layers in R interface
      </p>  

      ### [IMDB Movie Review Sentiment Analysis  using Keras RNN and LSTM in R](https://github.com/hyunjoonbok/R-projects/blob/master/Deep-Learing/Sentiment%20Analysis%20for%20Movie%20review%20text%20using%20Keras.R)
      <p>
      2-way polarity (positive, negative) classification system for Movie Review texts. Goes through word-tokenizatino, modeling and evaluation using Keras. 
      </p> 

   <hr>

   - ## Time Series Forecasting & Anomaly Detection

      ### [ARIMA Time-series Forecast](https://github.com/hyunjoonbok/R-projects/blob/master/Times%20Series%20Forecasting%20%26%20Anomaly%20Detection/ARIMA%20Time-series%20Forecast%20method.R)
      <p>
      ARIMA model to understand and predict value in time series. Has steps to Decomspose, Stationary, Autocorrelations, Model Fitting/Evaluation 
      </p>

      ### [Anomaly Detection in R](https://github.com/hyunjoonbok/R-projects/blob/master/Times%20Series%20Forecasting%20&%20Anomaly%20Detection/Anomaly%20Detection%20in%20R.R)
      <p>
      A very simple but powerful Anomaly Dection model with the toy package download data. 
      </p>

      ### [3 Simple TimeSeries forecast Model](https://github.com/hyunjoonbok/R-projects/blob/master/Times%20Series%20Forecasting%20&%20Anomaly%20Detection/Anomaly%20Detection%20in%20R.R)
      <p>
      Breif introduction on choosing forecast model. Exponential State Smoothing, ARIMA and TBATS and their comparison is shown. 
      </p>  

      ## TimeSeries Machine Learning 

      ### [TimeSeries Machine Learning using TIMETK](https://github.com/hyunjoonbok/R-projects/blob/master/Times%20Series%20Forecasting%20&%20Anomaly%20Detection/TIMETK%20for%20Time%20series%20machine%20learning.R)
      ### [Tidying up TimeSeries Machine Learning using Sweep](https://github.com/hyunjoonbok/R-projects/blob/master/Times%20Series%20Forecasting%20%26%20Anomaly%20Detection/Use%20SWEEP%20for%20Forecast.R)
      <p>
      Time series machine learning to forecast time series data of beer sales. Augmentation on the data is supported. Then we clean the model (i.e. retrieve the model coefficients, residuals). Modeling / Error Investigation are followed.
      </p>

      ### [3 Simple TimeSeries forecast Model](https://github.com/hyunjoonbok/R-projects/blob/master/Times%20Series%20Forecasting%20&%20Anomaly%20Detection/Anomaly%20Detection%20in%20R.R)
      <p>
      Breif introduction on choosing forecast model. Exponential State Smoothing, ARIMA and TBATS and their comparison is shown. 
      </p> 

   <hr>

   - ## Database & Pararell Computing

      ### [Google BigQuery Connection with R](https://github.com/hyunjoonbok/R-projects/blob/master/Database%20%26%20Pararell%20Computing/Analyzing%20Google%20Analytics%20with%20BigQuery.R) 
      <p>
      Analyzing Google Analytics data (built-in as sample data) with BigQuery using R interface. It shows how we can locally connect to BigQuery using DBI pacakge.
      </p>

      ### [Database Fundamentals in R](https://github.com/hyunjoonbok/R-projects/blob/master/Database%20%26%20Pararell%20Computing/Database%20in%20R.Rmd)
      <p>
      Connection to BigQuery, usage of dplyr commands, Calculate k-means inside the data, and fianlly visualization of data using ggplot. 
      </p>
      
      ### [Parallel Computing in R](https://github.com/hyunjoonbok/R-projects/blob/master/Database%20%26%20Pararell%20Computing/Parallel%20Computing%20in%20R.R)
      <p>
      R provides a number of convenient facilities for parallel computing. This script shows how to setup and run a parallel process on your current multi-core device, without need for additional hardware.   
      </p>

      ### [SparklyR Complete Guide](https://github.com/hyunjoonbok/R-projects/blob/master/Database%20%26%20Pararell%20Computing/sparklyr%20Guide.R)
      <p>
      Introduces a R interface for Apache Spark. Connecting to Spark from a local machine. Learn to use distributed computing by fully utilizing Spark's engine, as Hadoop-based Data Lake is becoming a common practice at companies. 
      </p>

   <hr>

   - ## Text Mining / Social Media Analysis

      ### [Text-Mining using SparklyR](https://github.com/hyunjoonbok/R-projects/blob/master/Text%20Mining%20%26%20Social%20Media%20Analysis/Text%20Mining%20using%20Sparklyr.R)
      <p>
      For real-world text data that goes beyond GB/TB in file size, it's necessary to leverage Spark engine load and transform data. Eventally genearate a list of the most used words, and create basic wordcloud. 
      </p>

      ### [Complete Text-Mining Guide](https://github.com/hyunjoonbok/R-projects/blob/master/Text%20Mining%20%26%20Social%20Media%20Analysis/Text_mining.R)
      <p>
      Looking at the Jane Austen Book's text to learn a full function of Text-Mining (tidying up data, Sentiment analysis, word-frequnecy, TF-IDF, Wordcloud, Tokenizing by n-gram, Topic-modeling). Ready-to-be used in any real-world datasets.  
      </p>

      ### [Twitter Analysis and Visualization](https://github.com/hyunjoonbok/R-projects/blob/master/Text%20Mining%20%26%20Social%20Media%20Analysis/Twitter%20(Social%20Media)%20Analysis%20and%20Visualization.R)
      <p>
      Learn to serach tweets by length, location or any criteria set. Retrieve a list of all the accounts a user follows. Then plot the frequency of tweets for each user over time.
      </p> 

   <hr>

   - ## Visualization (ggplot2)

      ### Ready-to-Use ggplot2 

      #### [ggplot2 code 1](https://github.com/hyunjoonbok/R-projects/blob/master/GGPLOT2%20guide/Ready-to-use%20GGplot2%20codes%201.R)
      #### [ggplot2 code 2](https://github.com/hyunjoonbok/R-projects/blob/master/GGPLOT2%20guide/Ready-to-use%20GGplot2%20codes%202.R)
      <p>
      A few curated list of ggplot codes that generates beautiful plot with examples. Basic understanding of ggplot codes is required. 
      </p>     

   <hr>

   - ## Statistic Concepts with real-world examples

      ### [Logistic Regression](https://github.com/hyunjoonbok/R-projects/blob/master/Data%20EDA%20%26%20Statistics/Logistic%20Regression.R)
      <p>
      Concept of Logistic Regression displayed in R code. Solves a binary classification problem.
      </p>

      ### [Mutinomial Regression](https://github.com/hyunjoonbok/R-projects/blob/master/Data%20EDA%20%26%20Statistics/Multinomial%20Regression.R)
      <p>
      Multinomial regression is similar to logistic regression, but fits better when the response variable is a categorical variable with more than 2 levels.
      </p>   

      ### [Ordinal Logistic Regression](https://github.com/hyunjoonbok/R-projects/blob/master/Data%20EDA%20%26%20Statistics/Ordinal%20logistic%20regression.R)
      <p>
       Ordinal logistic regression can be used to model a ordered factor response. Here, we use ordered logistic regression to predict the car evaluation.
      </p>   

      ### [Ridge Regression](https://github.com/hyunjoonbok/R-projects/blob/master/Data%20EDA%20%26%20Statistics/Ridge%20Regression.R)
      <p>
       Ridge Regression is a commonly used technique to address the problem of "multi-collinearity". We looks at the result of Linear Regression vs Ridge Regression
      </p>   
      
      
      ### [Network Analysis and Manipulation](https://github.com/hyunjoonbok/R-projects/blob/master/Data%20EDA%20%26%20Statistics/Network%20Analysis%20and%20Manipulation%20using%20R.R)
      <p>
        Social Network Analysis is a set of methods used to visualize networks, describe specific characteristics of overall network structure, and build mathematical and statistical models of network structures and dynamics
      </p> 

<hr>

## Setup
* Simply click one of the R files above and copy/paste on your own R scripts. 
  * *Please make sure to change the working directory!*

<hr>

## TO-DOs
List of features ready and TO-DOs for future development

* Introduction to R Shiny Apps : _in progress_
* More Kaggle examples : _in progress_
* Data cleaning .ipynbs : _in progress_ --> In Python Portfolio

<hr>

## Contact
Created by [@hyunjoonbok](https://www.linkedin.com/in/hyunjoonbok/) - feel free to contact me!
