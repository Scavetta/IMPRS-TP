# SILAC Analysis
# Stable Isotope Labelling of Amino Acids in Cell cultures
# Rick Scavetta
# 27 June 2018
# Case study for workshop

# clear workspace
rm(list = ls())

# Load packages:
library(tidyverse) # includes purrr, dplyr, ggplot2 and others

# Read in the data
protein.df <- read.delim("Protein.txt")

# Examine the data
head(protein.df) # print the first six rows
str(protein.df) # structure
glimpse(protein.df) # better version of str()

# work on tibble (special kind of data frame)
class(protein.df) # data frame
protein.df <- as_tibble(protein.df)
class(protein.df) # data frame, AND tibble
# Why?
protein.df # much better print out!!!

# Transformations
# log10 of intensities
protein.df$Intensity.H <- log10(protein.df$Intensity.H)
protein.df$Intensity.M <- log10(protein.df$Intensity.M)
protein.df$Intensity.L <- log10(protein.df$Intensity.L)

# Add intensities:
protein.df$Intensity.H.M <- protein.df$Intensity.H + protein.df$Intensity.M
protein.df$Intensity.M.L <- protein.df$Intensity.M + protein.df$Intensity.L

# log2 of ratios:
protein.df$Ratio.H.M <- log2(protein.df$Ratio.H.M)
protein.df$Ratio.M.L <- log2(protein.df$Ratio.M.L)

# Exercises 1 - 4, p58-59:
# Ex1: Remove contaminants
# how many:
protein.df %>% 
  count(Contaminant) # provides a dataframe

sum(protein.df$Contaminant == "+")
# length(which(protein.df$Contaminant == "+"))
table(protein.df$Contaminant)/nrow(protein.df) # provides a vector

# now remove:
protein.df %>%
  filter(Contaminant != "+") -> protein.df

# Ex2: Extract specific uniprot IDs
proteinmouse <- paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE")
protein.df %>% 
  filter(Uniprot %in% proteinmouse) %>% 
  select(Uniprot, MW.kDa, Ratio.H.M)
# How about with []?
protein.df[protein.df$Uniprot %in% proteinmouse, c("Uniprot", "MW.kDa") ]
# Could also use c(2,4) instead of names

# Ex3: Get low HM ratio p-value proteins
# 104 proteins
protein.df %>% 
  filter(Ratio.H.M.Sig < 0.05)
# filter() automatically excludes NA values!
# How about with []? Watch out -- [] doesn't exclude NA values!
# 479 with NA
protein.df[protein.df$Ratio.H.M.Sig < 0.05 & !is.na(protein.df$Ratio.H.M.Sig), ]

# confirmation functions:
is.numeric(protein.df$Ratio.H.M.Sig) # is it numeric
is.character(protein.df$Ratio.H.M.Sig)
is.na(protein.df$Ratio.H.M.Sig) # which are NAs?
!is.na(protein.df$Ratio.H.M.Sig) # which are NOT NAs?

# Ex4: Get extreme log2 ratio proteins for HM
protein.df %>% 
  filter(Ratio.H.M > 2.0 | Ratio.H.M < -2.0)
# How about with []? Watch for the NA values! :/

# for completness, plot the data:
# log10-Intensity vs log2-Ratio
ggplot(protein.df, aes(Ratio.H.M, Intensity.H.M)) +
  geom_point()

# Exercises 1-3, page 64: revisit the above exercises using []

# Exercises 4-5, page 64: 
# top 20 highest HM and ML ratios
# use arrange() in the dplyr package
protein.df %>% 
  arrange(-Ratio.M.L)

# Alternatively:
arrange(protein.df, desc(Ratio.M.L))

protein.df %>% 
  arrange(desc(Ratio.M.L)) %>% 
  select(Uniprot) %>% 
  .[1:20,] -> topML

# slice(1:20)
# .[1:20,]
# head(20) # Alternative

protein.df %>% 
  arrange(desc(Ratio.H.M)) %>% 
  select(Uniprot) %>% 
  .[1:20,] -> topHM

# What is the intersection between these lists? see page 65
intersect(topHM, topML) # A2AE89 and MYO1C

#### Element 10: split-apply-combine

# start from scratch
rm(list = ls())
protein.df <- read.delim("Protein.txt", stringsAsFactors = F)
glimpse(protein.df)

# Make a tibble:
protein.df <- as_tibble(protein.df)
protein.df

# 3 main components of dplyr:
# 1 - %>% pipe operator (shift + ctrl + m)

# 2 - 5 verbs (the "grammar" of data analysis)
# 2a - filter
# 2b - arrange (lowest to highest "ascending")
# 2c - select (columns)
protein.df %>% 
  filter(Contaminant != "+") %>% 
  arrange(desc(Ratio.H.M)) %>% 
  select(Uniprot, Ratio.M.L, Ratio.H.M)

# 2c+ Helper functions (see page 98)
# All "R" columns, but not those ending in "Sig"
protein.df %>% 
  filter(Contaminant != "+") %>% 
  arrange(desc(Ratio.H.M)) %>% 
  select(Uniprot, starts_with("R"), -ends_with("Sig"))

# Apply functions
# 2d - mutate(), for transformation functions
# Individual mutations, explicit
protein.df %>% 
  filter(Contaminant != "+") %>% 
  mutate(Ratio.M.L = log2(Ratio.M.L),
         Ratio.H.M = log2(Ratio.H.M))

# better, use helper functions and variants of mutate (see page 101):
protein.df %>% 
  filter(Contaminant != "+") %>% 
  mutate_at(vars(starts_with("R"), -ends_with("Sig")), log2) %>% 
  mutate_at(vars(starts_with("I")), log10) %>% 
  mutate(Intensity.M.L = Intensity.M + Intensity.L,
         Intensity.H.M = Intensity.M + Intensity.H)
     
# 2e - summarise(), for aggregration functions
# 3 - group_by, an adverb for splitting
# return to main tutorial