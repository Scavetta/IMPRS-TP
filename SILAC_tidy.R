# SILAC Analysis
# Stable Isotope Labelling of Amino Acids in Cell cultures
# Rick Scavetta
# 28 June 2018
# Case study for workshop - Let's revist the SILAC Analysis using actual tidy data and see how it makes things easier

# clear workspace
rm(list = ls())

# Load packages:
library(tidyverse) # includes purrr, dplyr, ggplot2 and others

# Can you make this data tidy?
# Read in data, make tibble and remove contaminants:
read.delim("Protein.txt", stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  filter(Contaminant != "+") -> protein.df

# Process intensities:
protein.df %>% 
  mutate_at(vars(starts_with("Int")), log10) %>%    # Calculate log10
  mutate(H.M = Intensity.H + Intensity.M,           # Add log10s and rename columns
         M.L = Intensity.M + Intensity.L) %>% 
  select(Uniprot, H.M, M.L) %>%                     # Take columns of interest
  gather(Ratio, Intensity, -Uniprot) -> onlyInt     # gather and save

# Process Ratios:
protein.df %>% 
  select(Uniprot, starts_with("Rat"), -ends_with("Sig")) %>%                   # Calculate log2
  gather(Ratio, Expression, -Uniprot) %>%                                      # Gather
  filter(Ratio != "Ratio.H.L") %>%                                             # Remove uninteresting H.L Ratio
  mutate(Ratio = recode_factor(Ratio,                                          # Relabel ratios to match the Int data frame
                               `Ratio.M.L` = "M.L",
                               `Ratio.H.M` = "H.M")) %>% 
  group_by(Ratio) %>%                                                          # group according to ratios (2 groups)
  mutate(Expression = Expression - mean(Expression, na.rm = T)) -> onlyRatios  # center on 0 and save
# mutate(Expression = scale(Expression)[,1]) -> onlyRatios                     # Alternatively, apply a z-score and save

# Process significant values and merge to intensities and ratios:
protein.df %>% 
  select(Uniprot, ends_with("Sig")) %>%                          # Take columns of interest
  gather(Ratio, Significance, -Uniprot) %>%                      # Gather
  filter(Ratio != "Ratio.H.L.Sig") %>%                           # Remove uninteresting H.L Ratio
  mutate(Ratio = recode_factor(Ratio,                            # Relabel ratios to match the Int data frame
                               `Ratio.M.L.Sig` = "M.L",
                               `Ratio.H.M.Sig` = "H.M"),
         SigCat = cut(Significance,                              # Make colour labels for sig values
                      c(-Inf, 1e-11, 1e-4, 0.05, Inf),
                      c("red", "orange", "blue", "grey30"))) %>%
  full_join(onlyRatios) %>%                                      # Merge with the log2 ratios
  full_join(onlyInt) %>%                                         # Merge with the Intensities
  filter(complete.cases(.), Uniprot != "") %>%                   # Take only observations that have complete data and non-empty Uniprot 
  arrange(desc(Significance)) -> allData                         # Order according to sig so that low sig are plotted first

# Make a plot
ggplot(allData, aes(Expression, Intensity, col = SigCat)) +
  geom_point(alpha = 0.5, shape = 16) +
  # scale_x_continuous(limits = c(-5,5)) +
  scale_colour_identity() +
  facet_grid(. ~ Ratio) +
  theme_classic()