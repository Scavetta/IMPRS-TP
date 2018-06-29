# Chapter 16: The meditation case study
# Rick Scavetta
# 29.7.2018

# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(Hmisc)

# Exercise 2: Read in the data
read.delim("Expression.txt") %>% 
  as_tibble() %>% 
  gather(key, value) -> medi.t

medi.t %>% 
  separate(key, c("treatment", "gene", "time"), "_") %>% 
  filter(!is.na(value)) %>% 
  group_by(gene, treatment, time) %>% 
  summarise(avg = mean(value),
            stdev = sd(value),
            n = n(),
            SEM = stdev/sqrt(n),
            lower95 = smean.cl.normal(value)[2],
            upper95 = smean.cl.normal(value)[3]) -> medi.summary

# save to your computer:
# use write.table(), write.csv(), or write.delim()
# or.... use the rio package
library(rio)
export(medi.summary, "summary data.txt")

# Do a single ANCOVA: use + or * for interaction
# use the broom package to clean up the results
library(broom)
medi.t %>% 
  separate(key, c("treatment", "gene", "time"), "_") %>% 
  filter(gene == "RIPK2") %>% 
  do(tidy(anova(lm(value ~ treatment + time, data = .))))

# so... generalise to all genes: replace filter with group_by
medi.t %>% 
  separate(key, c("treatment", "gene", "time"), "_") %>% 
  group_by(gene) %>% 
  do(tidy(anova(lm(value ~ treatment + time, data = .))))



# Easy calculation of 95% CI on a single variable
# use Hmisc package
smean.cl.normal(1:100)[2] # lower limit
smean.cl.normal(1:100)[3] # upper limit







