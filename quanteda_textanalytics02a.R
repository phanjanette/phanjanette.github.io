# Quanteda Text Statistics and Analysis Workshop
# Advanced text analysis using US Presidential Inaugural Addresses
# Documentation: https://quanteda.io/
# Updated: 2025 - Fixed issues and modernized code

library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)  # Add textstats library at top
library(readr)
library(ggplot2)
library(tidyverse)

# Set seed for reproducibility
set.seed(100)

# ============================================================================
# SECTION 1: WORDCLOUD - EARLY US PRESIDENTIAL SPEECHES (1789-1826)
# ============================================================================

cat("\n=== Creating wordcloud for historical inaugural speeches ===\n")

# Create DFM from speeches before 1826
dfm_inaug <- corpus_subset(data_corpus_inaugural, Year <= 1826) |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("english")) |>
  dfm() |>
  dfm_trim(min_termfreq = 10, verbose = FALSE)

# Generate wordcloud
textplot_wordcloud(dfm_inaug, max_words = 100)

# ============================================================================
# SECTION 2: COMPARISON WORDCLOUD - RECENT PRESIDENTS
# ============================================================================

cat("\n=== Creating comparison wordcloud (Trump, Obama, Bush) ===\n")

# Create comparison wordcloud for three presidents
comparison_wc <- corpus_subset(data_corpus_inaugural, 
                               President %in% c("Trump", "Obama", "Bush")) |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("english")) |>
  dfm() |>
  dfm_group(groups = President) |>
  dfm_trim(min_termfreq = 5, verbose = FALSE)

# Plot comparison wordcloud
textplot_wordcloud(comparison_wc, comparison = TRUE, max_words = 50)

# ============================================================================
# SECTION 3: CUSTOM COLORED WORDCLOUD
# ============================================================================

cat("\n=== Creating custom colored wordcloud ===\n")

textplot_wordcloud(
  dfm_inaug, 
  min_count = 10,
  max_words = 80,
  color = c("red", "pink", "green", "purple", "orange", "blue")
)

# ============================================================================
# SECTION 4: X-RAY PLOT - TEMPORAL KEYWORD ANALYSIS
# ============================================================================

cat("\n=== Creating X-ray plots for keyword temporal distribution ===\n")

# Subset speeches from 1950 onwards
data_corpus_inaugural_subset <- 
  corpus_subset(data_corpus_inaugural, Year > 1949)

# Single keyword x-ray plot: "american"
cat("Analyzing term: 'american'\n")

kwic_american <- kwic(tokens(data_corpus_inaugural_subset), 
                      pattern = "american")

textplot_xray(kwic_american)

# ──────────────────────────────────────────────────────────────────────────
# INTERPRETATION GUIDE: X-ray plot for "american"
# ──────────────────────────────────────────────────────────────────────────
# 
# What to look for:
# - PEAKS: Years with high frequency of "american" usage
#   * Likely during periods of national identity emphasis
#   * Post-war speeches often emphasize national identity
#   * Cold War speeches reference "American values"
#
# - TROUGHS: Years with low or no usage
#   * Some presidents may use alternative phrasing
#   * Different rhetorical strategies
#
# - HISTORICAL CONTEXT:
#   * 1950s-1960s: Cold War peak (anti-communist rhetoric)
#   * 1980s: Reagan's "American exceptionalism" themes
#   * 1990s-2000s: Post-Cold War identity reconstruction
#   * 2010s: Obama/Trump shifts in national identity framing
# ──────────────────────────────────────────────────────────────────────────

# ============================================================================
# SECTION 5: MULTI-KEYWORD X-RAY COMPARISON
# ============================================================================

cat("\n=== Creating multi-keyword X-ray plots ===\n")

# Extract keywords for multiple terms
kwic_american <- kwic(tokens(data_corpus_inaugural_subset), 
                      pattern = "american")
kwic_people <- kwic(tokens(data_corpus_inaugural_subset), 
                    pattern = "people")
kwic_communist <- kwic(tokens(data_corpus_inaugural_subset), 
                       pattern = "communist")

# Plot all three keywords
textplot_xray(
  kwic_american,
  kwic_people,
  kwic_communist
)

# ──────────────────────────────────────────────────────────────────────────
# INTERPRETATION: Why is "communist" missing?
# ──────────────────────────────────────────────────────────────────────────
# 
# The "communist" plot appears empty because:
# - Term has no occurrences after 1949 in the selected speeches, OR
# - The term appears infrequently/inconsistently
# 
# This makes historical sense:
# - Cold War rhetoric (1950s-1980s) did use "communist" more frequently
# - But it may appear in context clauses, not as main keyword
# - Term usage declined after Cold War (post-1991)
# - Modern speeches may use different terminology ("authoritarian", "hostile")
# ──────────────────────────────────────────────────────────────────────────

# ============================================================================
# SECTION 6: CUSTOMIZED MULTI-KEYWORD X-RAY WITH STYLING
# ============================================================================

cat("\n=== Creating styled multi-keyword X-ray plot ===\n")

# Create the x-ray plot
g <- textplot_xray(
  kwic_american,
  kwic_people,
  kwic_communist
)

# Add custom styling with ggplot2
g_styled <- g + 
  aes(color = keyword) + 
  scale_color_manual(
    values = c("american" = "blue", "people" = "red", "communist" = "green"),
    breaks = c("american", "people", "communist"),
    na.value = "gray80"
  ) +
  labs(
    title = "Temporal Distribution of Key Terms in Presidential Speeches",
    subtitle = "Post-1949 Inaugural Addresses",
    x = "Year",
    y = "Occurrence",
    color = "Keyword"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(g_styled)

# ============================================================================
# SECTION 7: FREQUENCY ANALYSIS - BASIC
# ============================================================================

cat("\n=== Analyzing term frequencies ===\n")

# Get top 100 features with frequency statistics
features_dfm_inaug <- textstat_frequency(dfm_inaug, n = 100)

# Sort by frequency (descending)
features_dfm_inaug <- features_dfm_inaug |>
  mutate(feature = reorder(feature, -frequency))

# Plot frequency distribution
ggplot(features_dfm_inaug, aes(x = feature, y = frequency)) +
  geom_point(color = "steelblue", size = 2) +
  geom_line(color = "steelblue", alpha = 0.3, group = 1) +
  labs(
    title = "Top 100 Terms: Historical Inaugural Speeches (1789-1826)",
    x = "Term",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    plot.title = element_text(face = "bold")
  )

# ============================================================================
# SECTION 8: FREQUENCY GROUPED BY PRESIDENT
# ============================================================================

cat("\n=== Analyzing frequency by president ===\n")

# Create DFM from recent speeches
data_corpus_recent <- corpus_subset(data_corpus_inaugural, Year > 1949)

# Calculate frequencies grouped by president
freq_by_pres <- dfm(tokens(data_corpus_recent, remove_punct = TRUE)) |>
  dfm_trim(min_termfreq = 2) |>
  textstat_frequency(groups = docvars(data_corpus_recent, "President"))

# Filter for specific term: "american"
freq_american <- freq_by_pres |>
  filter(feature %in% "american")

# Plot frequency of "american" by president
ggplot(freq_american, aes(x = group, y = frequency)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(color = "darkblue", alpha = 0.5, group = 1) +
    labs(
    title = 'Frequency of Term "american" by President',
    subtitle = "Post-1949 Inaugural Speeches",
    x = "President",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# ============================================================================
# SECTION 9: RELATIVE FREQUENCY ANALYSIS
# ============================================================================

cat("\n=== Analyzing relative frequency (normalized) ===\n")

# Create DFM with proportional weighting
dfm_rel_freq <- dfm(tokens(data_corpus_recent, remove_punct = TRUE)) |>
  dfm_trim(min_termfreq = 2) |>
  dfm_weight(scheme = "prop") |>
  (\(x) x * 100)()  # Convert to percentages

# Verify structure
head(dfm_rel_freq)

# Calculate relative frequencies grouped by president
rel_freq <- textstat_frequency(
  dfm_rel_freq, 
  groups = docvars(data_corpus_recent, "President")
)

# Filter for "american"
rel_freq_american <- rel_freq |>
  filter(feature %in% "american")

# Plot relative frequency
ggplot(rel_freq_american, aes(x = group, y = frequency)) +
  geom_point(size = 3, color = "coral") +
  geom_line(color = "coral", alpha = 0.5, group = 1) +
  labs(
    title = 'Relative Frequency of "american" by President (%)',
    subtitle = "Normalized by document length",
    x = "President",
    y = "Relative Frequency (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# ============================================================================
# SECTION 10: TOP TERMS BY PRESIDENT (FACETED VIEW)
# ============================================================================

cat("\n=== Creating faceted view of top terms by president ===\n")

# Create DFM for recent presidents (2000 onwards)
dfm_weight_pres <- corpus_subset(data_corpus_inaugural, Year > 2000) |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("english")) |>
  dfm() |>
  dfm_weight(scheme = "prop")

# Get top 10 terms for each president
freq_weight <- textstat_frequency(
  dfm_weight_pres,
  n = 10,
  groups = docvars(corpus_subset(data_corpus_inaugural, Year > 2000), "President")
)

# Create faceted plot
ggplot(
  data = freq_weight,
  aes(x = reorder(feature, frequency), y = frequency)
) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  facet_wrap(~group, scales = "free_x", ncol = 2) +
  coord_flip() +
  labs(
    title = "Top 10 Terms by President",
    subtitle = "Post-2000 Inaugural Speeches (Relative Frequency)",
    x = "Term",
    y = "Relative Frequency"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold")
  )

# ============================================================================
# SECTION 11: KEYNESS ANALYSIS - COMPARING TWO PRESIDENTS
# ============================================================================

cat("\n=== Analyzing keyness: Trump vs Obama ===\n")

# Create corpus for just Obama and Trump
pres_corpus <- corpus_subset(
  data_corpus_inaugural,
  President %in% c("Obama", "Trump")
)

# Create DFM grouped by president
pres_dfm <- tokens(pres_corpus, remove_punct = TRUE) |>
  tokens_remove(stopwords("english")) |>
  tokens_group(groups = President) |>
  dfm()

# Calculate keyness (Trump is target group)
result_keyness <- textstat_keyness(pres_dfm, target = "Trump")

# Display keyness statistics
print(head(result_keyness, 15))

# ──────────────────────────────────────────────────────────────────────────
# Plot 1: Keyness with reference (Obama) shown on left
# ──────────────────────────────────────────────────────────────────────────

textplot_keyness(result_keyness, show_reference = TRUE)

# ──────────────────────────────────────────────────────────────────────────
# Plot 2: Keyness without reference (cleaner look)
# ──────────────────────────────────────────────────────────────────────────

textplot_keyness(result_keyness, show_reference = FALSE)

# ──────────────────────────────────────────────────────────────────────────
# INTERPRETATION: Understanding Keyness
# ──────────────────────────────────────────────────────────────────────────
#
# KEYNESS measures statistical association of terms with one document group
# compared to another (target vs. reference).
#
# POSITIVE KEYNESS (right side, Trump's distinctive terms):
# - Terms more associated with Trump speeches
# - Higher frequency relative to Obama
# - Example: "American first" rhetoric, business terminology
#
# NEGATIVE KEYNESS (left side, Obama's distinctive terms):
# - Terms more associated with Obama speeches
# - Higher frequency relative to Trump
# - Example: "Change", "hope", multi-cultural references
#
# CHI-SQUARED TEST: Measures whether frequency differences are significant
# - Larger chi-squared values = stronger association
# - Statistical significance indicates real difference, not random variation
#
# ──────────────────────────────────────────────────────────────────────────

# ============================================================================
# OPTIONAL: SUMMARY STATISTICS
# ============================================================================

cat("\n=== SUMMARY: Data Corpus Overview ===\n")
cat("Total speeches in inaugural corpus:", ndoc(data_corpus_inaugural), "\n")
cat("Time period:", min(docvars(data_corpus_inaugural, "Year")),
    "-", max(docvars(data_corpus_inaugural, "Year")), "\n")
cat("Number of unique presidents:", length(unique(docvars(data_corpus_inaugural, "President"))), "\n")
cat("\n")


