# Intro to R
# Rick Scavetta
# 27.06.18
# IMPRS-TP Munich

# Clear workspace
rm(list = ls())

# Load packages
library(tidyverse)

# R syntax:
n <- log2(8) # log2 of 8
# execute: ctrl + enter
# assign operator (<-): alt + -

# A simple workflow:
# A built-in dataset:
PlantGrowth

# How many groups? What are they?
levels(PlantGrowth$group)
nlevels(PlantGrowth$group)

# Descriptive statistics:
# ALL 30 values:
mean(PlantGrowth$weight)
sd(PlantGrowth$weight)

# Group-wise means:
# The dplyr way: (first install and load package)
# shift + ctrl + m for the "pipe" operator %>%
# say "and then ..."
PlantGrowth %>%
  group_by(group) %>%
  summarise(avg = mean(weight),
            stdev = sd(weight),
            med = median(weight)) -> results

# Make plots: load ggplot2
# 3 parts: data, aesthetics, geometries
# aesthetics: MAPPING data onto a visual scale (axes)
# e.g. x, y, col, size, shape

# The first two layers, +, then geom_:
ggplot(PlantGrowth, aes(group, weight)) +
  geom_point(position = position_jitter(0.1),
             alpha = 0.6)

# Doing stats:
# Group differences
# First build a linear model:
plant.lm <- lm(weight ~ group, data = PlantGrowth)

# t-test:
# Could do individual t-tests using t.test()
# But better, if we have an lm:
summary(plant.lm)

# true one-way ANOVA:
anova(plant.lm)

# If we did want to do ALL pair-wise t-tests then:
# use aov() instead.
plant.aov <- aov(weight ~ group, data = PlantGrowth)
TukeyHSD(plant.aov)

# Exercise for markdown, use this dataset:
chickwts

# Element 2: Functions
# Everything that happens, is because of a functions

34 + 6
`+`(34,6) # written as a function

# BEDMAS - order of operations
# brackets, exp, div, mult, add, sub
2 - 3/4 # 1.25
(2 - 3)/4 # -0.25

# Make some objects:
n <- 34
p <- 6

# Use like numbers:
n + p

# Exercise 1, p27:
m <- 1.12
b <- -0.4
m * 3 + b
m * 8 + b
m * 9 + b
m * 23 + b

# Generic form of functions:
# fun_name(fun_args)

# fun_args may be named or unnamed
# we can refer to them using names or only position
# functions can have zero to potentially infinite args


log2(8) # positional matching
log(x = 8, base = 2) # long form with names
log(8, 2) # long form with positional matching
log(8, base = 2) # mix name and position

# what about:
log(base = 2, x = 8) # not necessary
log(2, x = 8) # confusing :/ don't do it

# some basic and common functions:
# Combine/concatenate many unnamed arguments
xx <- c(3, 8, 9, 23)
xx

# With characters
myNames <- c("healthy", "tissue", "quantity")

# Seqential numbers: seq()
seq(from = 1, to = 100, by = 7)
foo1 <- seq(1, 100, 7)

# use objects in functions:
foo2 <- seq(1, n, p)

# Regular sequence with interval 1
1:10 # short for...
seq(1, 10, 1)

# Two major types of math functions
# Aggregration functions: return a single (or a small number) of value(s)
# summary stats: mean, sd
# sum, product, length (n-value)

# Transformation functions: return as many output as input
# standardisation: normalise to bkg, z-scores, log

# Exercise 3, p30:
foo2 + 100 # Transformation
foo2 + foo2 # Transformation
sum(foo2) + foo2 # aggr followed by trans
1:3 + foo2 # trans

1:4 + foo2 # trans

####### Fundamental concept # 1
####### Vector recycling

# e.g. Z-scores
# (x_i - x_bar)/sd
(foo2 - mean(foo2))/sd(foo2)

# or...
scale(foo2) # short cut to z-scores

# Exercise 3, p30
m * xx + b
# it doesn't matter how long xx is!
m * seq(1,100, 0.1) + b

# So what if I had two slope values?!?
m2 <- c(0, 1.12)
# I want to get 8 values (2slopes x 4values)
m2 * xx + b # only 4 values

# Solutions:
# split up m2 into two objects ---> inefficient :/
# for loop to reiterate through a function ---> nice, but old school :/
# reiterate through all items ---> new style, and nice :)

# first, make a function:
equation <- function(x, slope = 1.12, yint = -0.4) {
  slope * x + yint
}

m * xx + b
equation(xx, m, b)
equation(xx) # default values
equation(xx, 23, 500) # change slope and yint
equation(xx, yint = 500) # change only yint

# Call the function many times:
equation(xx, m2) # Still doesn't work!
# Use the purrr package
# "map" each "element" of m2 to a function:
map(m2, equation) # here it thinks m2 should be assigned to x (position 1)

# explictly call the second argument:
map(m2, ~ equation(xx, .)) # use . for a place-holder

# Element 3: Objects
# Anything that exists, is an object

# specifically, common data storage objects:

# Vectors - 1 Dimensional, homogenous
# e.g.
m2 # 2 "elements"
xx # 4 "elements"
foo1
foo2

# 4 most common user-defined "atomic" vector types
# Logical (aka boolean, binary) - TRUE/FALSE, T/F, 1/0
# Integer - whole numbers 0,1,2,3,...
# Double (floating point) - real numbers 0.12, 3.14, 6.2, ...
# Character (aka strings, text) - can contain anything

# numerical ---> integer or double

# find out a type:
typeof(foo1)
typeof(myNames)

# must be homogenous:
test <- c(1:10, "bob")
typeof(test)
mean(test)
# remove the 11th position
mean(test[-11])

############ Major problem # 1: Wrong vector type
############ Solution: coerce types using and "as." function
as.numeric(test)

foo3 <- c("Liver", "Brain", "Testes", "Muscle", "Intestine", "Heart")
typeof(foo3)

foo4 <- c(T, F, F, T, T, F)
typeof(foo4)

# Matrix - 2 dimensional, homogenous
yy <- 1:12
matrix(yy, nrow = 3)

# Start from scratch:
yy <- 1:12
attributes(yy) # none
# add a dimension attribute
dim(yy) <- c(3, 4)
yy
# therefore, a matrix, is just a vector with a dim attribute
# Return to vector
yy <- as.vector(yy)
yy
# as an array:
dim(yy) <- c(2,2,3)
yy

# Lists - 1 dimensional, heterogenous
typeof(plant.lm)
attributes(plant.lm) # see table 7.3 p 38
class(plant.lm) # an attribute that tells other
# functions what to do with this object i.e. object-oriented programming

names(plant.lm) # any named element can be accessed with $
plant.lm$coefficients

# Data frames - 2 dimensional, heterogenous
# A special class of a list where each elment
# is a vector of the same length - just like an excel table

# row = observation
# column = variable

foo.df <- data.frame(foo4, foo3, foo2)
typeof(foo.df)
class(foo.df)

attributes(foo.df)

# replace one chr vector with another
names(foo.df) <- myNames
foo.df
foo.df$quantity

dim(foo.df) # 6 rows, 3 columns
length(foo.df) # confusing :/ remember it's a list
# better to not use it.
str(foo.df)
glimpse(foo.df) # belongs to dplyr, more compact view
summary(foo.df) # class data frame
# but contrast summary to what we saw earlier:
summary(plant.lm) # because this is a class lm!!!

# Element 4: Logical Expressions
# Asking and combining questions
# Part 1: Relational operators: YES/NO questions
# == equivalency
# != non-equivalency
# >, <, >=, <=
# !x, negation of x, where x is a logical vector

n > p
n < p

# The output is ALWYAYS a logical vector

# don't confuse:
# ==, <=, <-, =, -

# Part 2: Logical operators: Combine YES/NO questions
# & AND a TRUE in EVERY question
# | OR a TRUE in at LEAST ONE question
# %in% WITHIN combine many == with |

# Can use subset(), here we'll use filter()
# from dplyr package

# Apply to logical data:
# All healthy
foo.df %>%
  filter(healthy == TRUE)
foo.df %>%
  filter(healthy)

# All unhealthy:
foo.df %>%
  filter(!healthy)

# What's happening:
# give all rows where we have TRUE
foo.df$healthy == TRUE

# Apply to numeric data (int or dbl)
# Below 10
foo.df %>%
  filter(quantity < 10)

# Range: 10-20
foo.df %>%
  filter(quantity %in% 10:20)
# A bit more intuitive:
foo.df %>%
  filter(quantity > 10 & quantity < 20)
foo.df %>%
  filter(quantity > 10, quantity < 20)
foo.df %>%
  filter(quantity > 10) %>%
  filter(quantity < 20)

foo.df$quantity > 10
foo.df$quantity < 20

# what if.. meaningless
foo.df %>%
  filter(quantity > 10 | quantity < 20)

# extreme values beyond 10,20
foo.df %>%
  filter(quantity < 10 | quantity > 20)

# impossible:
foo.df %>%
  filter(quantity < 10 & quantity > 20)

# Apply to character data
# Here: NO PATTERN MATCHING
# ALL Heart samples:
foo.df %>%
  filter(tissue == "Heart")

# 2 or more: Heart & Liver
# quick & dirty
foo.df %>%
  filter(tissue == "Heart" | tissue == "Liver")

# better: use a vector

# TERRIBLE!! NEVER DO THIS!!!
foo.df %>%
  filter(tissue == c("Heart", "Liver"))
foo.df %>%
  filter(tissue == c("Liver", "Heart"))

# because of vector recycling!!
foo.df$tissue

# Correct way:
foo.df %>%
  filter(tissue %in% c("Heart", "Liver"))
foo.df %>%
  filter(tissue %in% c("Liver", "Heart"))

# Element 5: Indexing
# Findine info according to position using []

# Vectors
foo1
foo1[6] # the 6th value
foo1[p] # the pth value
foo1[3:p] # 3rd to the pth value
foo1[p:length(foo1)] # the pth to the last value

# We can use combinations of:
# integers, objects, functions

# But... the exciting part is... logical vectors:
# i.e. return values at TRUE positions
# e.g. via logical expressions
foo1[foo1 < 50]

# Data frames with []
# 2 dimensiona: [ rows , columns ]
foo.df[3,] # 3rd row, ALL columns
foo.df[,3] # 3rd column, ALL rows
foo.df[3:p, 3] # 3rd column, rows 3-6
foo.df[3:p, "quantity"] # 3rd column, rows 3-6
foo.df[3:p, -1] # exclude healthy col, rows 3-6
# foo.df[3:p, -c("healthy")] # doesn't work :/
foo.df[3:p, -c(1,3)] # exclude 1&3 col, rows 3-6

# Combine in all variety of ways
# To prevent switching between vector and data frame:
# use tibbles :)
foo.df <- as_tibble(foo.df)
foo.df[,3] # Always get a data frame :)

# examples:
foo.df[foo.df$quantity < 10, "tissue"]

# which is the same as:
foo.df %>%
  filter(quantity < 10) %>%
  select(tissue)

# This doesn't work!
foo.df[foo.df$tissue == "Heart"] # only heart rows
# no comma is short-hand for select columns
foo.df[3] # yes, the third column
foo.df[,3] # the same things

# so I don't need a , in a [] for a data frame
# But what if I DO have one for a vector?
foo1[,6] # no-go

# Element 8: Factor variables (with levels)
# categorical variables (with groups)
# aka discrete, qualitative

# e.g.
PlantGrowth$group

# Factor is a special class of type integer
typeof(PlantGrowth$group)
class(PlantGrowth$group)
# i.e. an integer with associated labels
str(PlantGrowth)
# Also..
str(foo.df) # in a df, converted to factor
foo.df$tissue
foo3 # outside a df, it's a chr

# The integer values are 4 1 6 5 3 2
# The associated labels are:
levels(foo.df$tissue)
# "Brain"     "Heart"     "Intestine" "Liver"     "Muscle"    "Testes"

# The main, typical problem: type conversion
xx <- c(23:27, "bob")
xx
# a chr turns to a factor in a df:
test <- data.frame(xx)
test$xx

# convert to integer:
as.integer(test$xx) # direct from a factor :/
# so... conver first to a chr
as.integer(as.character(test$xx))

# Alternatively:
test <- data.frame(xx, stringsAsFactors = F)
str(test)
as.integer(test$xx) # now this works directly

# Element 9 & 10: tidy data and split-apply-combine

# Work on a new data:
source("PlayData.R")
PlayData

# Make the data tidy using the tidyr
# gather() - four arguments
# 1 - data frame
# 2&3 - key, value pair (the names of the OUTPUT columns)
# 4 - either the ID or the MEASURE variables

# with ID
gather(PlayData, key, value, -c(type, time))

# with MEASURE
PlayData.t <- gather(PlayData, key, value, c(height, width))

# Now... split-apply-combine (take a look at the protein project)

# Scenario 1: (group according to type and time)
PlayData.t %>%
  group_by(type, time) %>%
  summarise(avg = mean(value))

# Scenario 2: (group according to type and key)
PlayData.t %>%
  group_by(type, key) %>%
  summarise(avg = mean(value))

# Scenario 3: (group according to time and key)
PlayData.t %>%
  group_by(time, key) %>%
  summarise(avg = mean(value))
