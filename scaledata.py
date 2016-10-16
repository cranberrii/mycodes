### http://machinelearningmastery.com/scale-machine-learning-data-scratch-python/
### http://chrisalbon.com/python/pandas_dataframe_importing_csv.html

import numpy as np
from pandas import Series, DataFrame
import pandas as pd
from csv import reader
from ggplot import *
from math import sqrt

### NORMALIZE DATA ###

# Load a CSV file
def load_csv(filename):
	file = open(filename, 'rt')
	lines = reader(file)
	dataset = list(lines)
	return dataset

# Convert string column to float
def str_column_to_float(dataset, column):
	for row in dataset:
		row[column] = float(row[column].strip())

# Find the min and max values for each column
def dataset_minmax(dataset):
	minmax = list()
	for i in range(len(dataset[0])):
		col_values = [row[i] for row in dataset]
		value_min = min(col_values)
		value_max = max(col_values)
		minmax.append([value_min, value_max])
	return minmax

# Rescale dataset columns to the range 0-1
def normalize_dataset(dataset, minmax):
	for row in dataset:
		for i in range(len(row)):
			row[i] = (row[i] - minmax[i][0]) / (minmax[i][1] - minmax[i][0])

# load csv dataset
# method 1
#df = pd.read_csv('pima-indians-diabetes.csv', names=['Col1', 'Col2', 'Col3', 'Col4', 'Col5', 'Col6', 'Col7', 'Col8', 'Col9'])
# method 2
filename = 'pima-indians-diabetes.csv'
df = load_csv(filename)
print('Loaded data file {0} with {1} rows and {2} columns'.format(filename, len(df), len(df[0])))

# convert string column to float
for i in range(len(df[0])):
	str_column_to_float(df, i)
print(df[0])
print(df[767])

# calculate min & max for each column
minmax = dataset_minmax(df)

# normalize columns
normalize_dataset(df, minmax)
print(df[0])
print(df[767])

print('fin')

### STANDARDIZE DATA ###

# calculate column means
def column_means(dataset):
	means = [0 for i in range(len(dataset[0]))]
	for i in range(len(dataset[0])):
		col_values = [row[i] for row in dataset]
		means[i] = sum(col_values) / float(len(dataset))
	return means

# calculate column standard deviations
def column_stdevs(dataset, means):
	stdevs = [0 for i in range(len(dataset[0]))]
	for i in range(len(dataset[0])):
		variance = [pow(row[i] - means[i], 2) for row in dataset]
		stdevs[i] = sum(variance)
	stdevs = [sqrt(x / (float(len(dataset) - 1))) for x in stdevs]
	return stdevs

# standardize dataset
def standardize_dataset(dataset, means, stdevs):
	for row in dataset:
		for i in range(len(row)):
			row[i] = (row[i] - means[i]) / stdevs[i]

# load csv dataset
filename = 'pima-indians-diabetes.csv'
df2 = load_csv(filename)
print('Loaded data file {0} with {1} rows and {2} columns'.format(filename, len(df2), len(df2[0])))

# convert str col to float
for i in range(len(df2[0])):
	str_column_to_float(df2, i)
print(df2[0])

# estimate mean & standard dev
means = column_means(df2)
stdevs = column_stdevs(df2, means)

# standardize dataset
standardize_dataset(df2, means, stdevs)
print(df2[0])

print('fin')