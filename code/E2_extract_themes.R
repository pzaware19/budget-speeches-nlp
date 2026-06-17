# =============================================================================
# E2_extract_themes.R
# Author: Piyush Zaware
# Last updated: 2026-06-17
#
# Goal: Extract dominant themes from scraped PIB + PRS articles by year.
#       For each of Nirmala Sitharaman's budget years, produce a ranked
#       vocabulary list (TF-IDF) that captures what was being discussed
#       in the pre-budget environment. This feeds into F2_prediction.R.
#
# IN
#   output/news/raw/pib_{year}.txt   -- PIB press releases (E1)
#   output/news/raw/prs_{year}.txt   -- PRS analyses (E1)
#
# OUT
#   output/news/themes_by_year.csv   -- top 50 TF-IDF words per year per source
#   output/news/combined_themes.csv  -- merged, top words per year
#   output/figures/fig_news_themes.png
# =============================================================================

library(tidytext)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(glue)
library(purrr)

root   <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
RAWDIR <- file.path(root, "output", "news", "raw")
NEWDIR <- file.path(root, "output", "news")
FIGDIR <- file.path(root, "output", "figures")

ns_years <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)

# -- CUSTOM STOP WORDS --------------------------------------------------------
budget_stopwords <- c(
  "sir","madam","budget","speech","finance","minister","ministry","government",
  "india","indian","central","national","union","parliament","hon","ble",
  "rupee","rupees","rs","crore","crores","lakh","lakhs","paise","percent",
  "during","period","annual","per","cent","annum","year","years",
  "therefore","however","moreover","furthermore","accordingly","thus","hence",
  "also","well","shall","will","may","must","need","said","also","one","two",
  "new","total","including","number","press","release","pib","prs","www",
  "http","https","page","contents","th","nd","rd","st","table","figure",
  "january","february","march","april","may","june","july","august",
  "september","october","november","december","2019","2020","2021","2022",
  "2023","2024","2025","2026","fy","ye","fyi","pib.gov.in","prsindia.org"
)
all_stopwords <- bind_rows(
  stop_words,
  tibble(word = budget_stopwords, lexicon = "custom")
) %>% distinct(word)

# -- LOAD ALL ARTICLES --------------------------------------------------------
#{
load_source <- function(source_prefix) {
  map_dfr(ns_years, function(yr) {
    path <- file.path(RAWDIR, glue("{source_prefix}_{yr}.txt"))
    if (!file.exists(path)) {
      message(glue("  Missing: {path}"))
      return(tibble(year = yr, source = source_prefix, text = NA_character_))
    }
    text <- paste(readLines(path, warn = FALSE), collapse = " ")
    if (nchar(text) < 100) {
      message(glue("  Too short ({nchar(text)} chars): {path}"))
      return(tibble(year = yr, source = source_prefix, text = NA_character_))
    }
    tibble(year = yr, source = source_prefix, text = text)
  })
}

prs_corpus <- load_source("prs")
eco_corpus <- load_source("ecosurvey")
all_corpus <- bind_rows(prs_corpus, eco_corpus) %>% filter(!is.na(text))

message(glue("Loaded {nrow(all_corpus)} documents ({sum(!is.na(prs_corpus$text))} PRS, {sum(!is.na(eco_corpus$text))} EcoSurvey)"))
#}

# -- TF-IDF BY YEAR AND SOURCE ------------------------------------------------
#{
tidy_tokens <- all_corpus %>%
  unnest_tokens(word, text) %>%
  anti_join(all_stopwords, by = "word") %>%
  filter(nchar(word) >= 3,
         !str_detect(word, "^[0-9,.()+\\-/]+$"),
         str_detect(word, "^[a-z'\\-]+$"))

# Count words per year-source combination
word_counts <- tidy_tokens %>%
  count(year, source, word, sort = TRUE)

# TF-IDF: treat each year-source as a document
tfidf_scores <- word_counts %>%
  mutate(doc = paste(year, source)) %>%
  bind_tf_idf(word, doc, n) %>%
  arrange(year, source, desc(tf_idf))

# Top 50 words per year per source
themes_by_year <- tfidf_scores %>%
  group_by(year, source) %>%
  slice_max(tf_idf, n = 50, with_ties = FALSE) %>%
  ungroup()

write_csv(themes_by_year, file.path(NEWDIR, "themes_by_year.csv"))
message("Saved: themes_by_year.csv")

# Combined top words per year (merge PIB + PRS, rerank)
combined_themes <- tidy_tokens %>%
  count(year, word, sort = TRUE) %>%
  bind_tf_idf(word, year, n) %>%
  group_by(year) %>%
  slice_max(tf_idf, n = 30, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(year, desc(tf_idf))

write_csv(combined_themes, file.path(NEWDIR, "combined_themes.csv"))
message("Saved: combined_themes.csv")

# Print top 10 per year for inspection
message("\nTop 10 pre-budget themes by year:")
combined_themes %>%
  group_by(year) %>%
  slice_head(n = 10) %>%
  summarise(top_words = paste(word, collapse = ", "), .groups = "drop") %>%
  print(n = Inf)
#}

# -- FIGURE: THEME HEATMAP ----------------------------------------------------
#{
plot_data <- combined_themes %>%
  group_by(year) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf))

# Only plot if we have meaningful data
if (nrow(plot_data) > 10) {
  # Pick top words that appear in at least 2 years
  recurring <- plot_data %>%
    count(word) %>%
    filter(n >= 2) %>%
    pull(word)

  heatmap_data <- combined_themes %>%
    filter(word %in% recurring) %>%
    group_by(year) %>%
    slice_max(tf_idf, n = 20, with_ties = FALSE) %>%
    ungroup()

  if (nrow(heatmap_data) > 5) {
    p_themes <- ggplot(heatmap_data,
        aes(x = factor(year), y = reorder(word, tf_idf), fill = tf_idf)) +
      geom_tile(colour = "white", linewidth = 0.3) +
      scale_fill_gradient(low = "#f5f1ea", high = "#7a3b1e",
                          name = "TF-IDF") +
      labs(
        title    = "Pre-budget vocabulary by year (PIB + PRS)",
        subtitle = "Words distinctive to each year's pre-budget coverage",
        x = NULL, y = NULL
      ) +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 0),
            panel.grid  = element_blank())

    ggsave(file.path(FIGDIR, "fig_news_themes.png"), p_themes,
           width = 10, height = 7, dpi = 150)
    message("Saved: fig_news_themes.png")
  }
}
#}

message("\nE2 complete.")
