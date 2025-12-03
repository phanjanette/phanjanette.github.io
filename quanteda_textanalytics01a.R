# Sample program for using quanteda for text modeling and analysis
# Documentation: vignette("quickstart", package = "quanteda")
# Website: https://quanteda.io/
# Updated: 2025 - Fixed deprecated functions and modernized syntax

library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(readr)
library(ggplot2)

# Twitter data about President Biden and Xi summit in November 2021
# Do some background search/study on the event

summit <- read_csv("https://raw.githubusercontent.com/datageneration/datamethods/master/textanalytics/summit_11162021.csv")

# Instead of View() for reproducible scripts, use head() or glimpse()
head(summit, 10)

sum_twt <- summit$text

# Tokenize the text
toks <- tokens(sum_twt)
print(class(toks))

# Create document-feature matrix
sumtwtdfm <- dfm(toks)

# Latent Semantic Analysis 
# Reference: https://quanteda.io/reference/textmodel_lsa.html
# NOTE: The 'margin' parameter has been deprecated in recent versions
# Use textmodel_lsa() without margin parameter for standard LSA
sum_lsa <- textmodel_lsa(sumtwtdfm, nd = 4)
summary(sum_lsa)
head(sum_lsa$docs)
print(class(sum_lsa))

# Intrepretation of LSA results
# LSA reduced your high-dimensional space (63,972 features) into 
# 4 latent semantic dimensions while preserving most of the information.

# What Each Column Represents:
# **Column 1-4**: The 4 latent semantic dimensions
# **Each row** (text1-text6): A document's position in semantic space
# **Cell values**: The document's score/loading on that dimension

### Interpretation:
# | Aspect | Meaning |
#  |--------|---------|
#  | **Large positive values** | Document strongly represents that semantic concept |
#  | **Large negative values** | Document's content opposes that semantic concept |
#  | **Values near zero** | Document barely relates to that dimension |
# | **Text2 row** | Very small values overall = document is semantically "neutral" or sparse |
#  | **Text4 row** | Larger values = rich semantic content across dimensions |


# 

# ============================================================================
# HASHTAG ANALYSIS
# ============================================================================

# Create DFM with punctuation removed
tweet_dfm <- tokens(sum_twt, remove_punct = TRUE) |>
  dfm()
head(tweet_dfm)

# Select hashtags (pattern starting with #)
tag_dfm <- dfm_select(tweet_dfm, pattern = "#*")
toptag <- names(topfeatures(tag_dfm, 50))
head(toptag, 10)

# Feature co-occurrence matrix for hashtags
tag_fcm <- fcm(tag_dfm)
head(tag_fcm)

# Select top hashtags for visualization
# Updated: fcm_select() is still valid but ensure compatibility
topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)

# Create network plot with improved parameters
textplot_network(
  topgat_fcm, 
  min_freq = 50, 
  edge_alpha = 0.8, 
  edge_size = 1,
  vertex_size = 3
)

# ============================================================================
# USER MENTION ANALYSIS
# ============================================================================

# Select user mentions (pattern starting with @)
user_dfm <- dfm_select(tweet_dfm, pattern = "@*")
topuser <- names(topfeatures(user_dfm, 50))
head(topuser, 20)

# Feature co-occurrence matrix for users
user_fcm <- fcm(user_dfm)
head(user_fcm, 20)

# Select top users for visualization
user_fcm <- fcm_select(user_fcm, pattern = topuser)

# Create network plot for user mentions
textplot_network(
  user_fcm, 
  min_freq = 20, 
  edge_color = "firebrick", 
  edge_alpha = 0.8, 
  edge_size = 1,
  vertex_size = 3
)

# ============================================================================
# ADDITIONAL ANALYSIS OPTIONS (for reference)
# ============================================================================

# Optional: Keyword frequency analysis
top_features <- topfeatures(tweet_dfm, 30)
print(top_features)

library(ggplot2)
library(tidyverse)

# Get top features with frequencies
top_freq <- topfeatures(tweet_dfm, 30)

# Convert to dataframe for ggplot
freq_df <- tibble(
  word = names(top_freq),
  frequency = as.numeric(top_freq)
) %>%
  mutate(word = fct_reorder(word, frequency))

# Create frequency plot
ggplot(freq_df, aes(x = frequency, y = word, fill = frequency)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Top 30 Most Frequent Terms",
    subtitle = "Biden-Xi Summit Tweets",
    x = "Frequency",
    y = "Term"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 11),
    title = element_text(size = 13, face = "bold")
  )

