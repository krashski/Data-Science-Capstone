Capstone: Predicting Restaurant Failure Using Yelp Data
========================================================
date: November 17 2015
transition: rotate

Introduction
========================================================

Restaurants have one of the highest business failure rates of all the retail and service industries, with a 30% failure rate commonly accepted as the norm.

The primary question of interest for this project is to attempt to identify drivers of restaurant failure (i.e., predictors of restaurants that are no longer in business) using Yelp data. 

Given that Yelp data does not contain strong predictors such as economic information, the goal is to build a model that predicts restaurant failure using only business and review data.

Methods
========================================================

A total of 21,799 restaurants and 990,627 restaurant reviews were extracted from the [Yelp Dataset Challenge](http://www.yelp.com/dataset_challenge) business and review data files for model building.

Seven variables were feature-engineered for the predictive model: city, restaurant category, mean review sentiment score, % of reviews that mention 'manager' or 'management', % of one-star reviews, restaurant name length, and review length.

A gradient boosted decision tree classification model was fit to the training data using the `xgboost` package with 10-fold cross validation repeated 5 times, and further validated on test and holdout samples to avoid overfitting. Adjusting for class imbalance increased the sensitivity of the model from 43.1% to 76.5%.

Results
========================================================

The top 5 predictors of restaurant failure are review count, review length, city, category, and the attribute 'Good For Dinner'.

<img src="capstone_presentation-figure/unnamed-chunk-1-1.png" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" style="display: block; margin: auto;" />

Discussion
========================================================

The primary question of interest for this project was to identify drivers of restaurant failure using Yelp review data. Using a powerful prediction algorithm in the form of gradient boosted decision trees resulted in a predictive model with reasonably high sensitivity after the model was adjusted for class imbalance. 

As a result, it was possible to answer the primary question of interest despite the lack of strong predictors in the dataset. 

In terms of further analysis, it would be interesting to see if the relationship between business failure, review count and review length identified in this project generalizes to other businesses in other cities.
