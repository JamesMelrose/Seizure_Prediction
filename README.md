# Seizure_Prediction

Attempt at predicting subsequent seizures based on EEG data from 12 channels for NIH Kaggle challenge. 

Changes signal into the phase domain, bins the data to reduce dimentionality of the predictors, and then applies a support vector machine with cross validation to attempt to predict subsequent seizures.

Manages to predict seizures at about 78-80%.
