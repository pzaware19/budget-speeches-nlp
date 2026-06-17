# =============================================================================
# F1_sitharaman_trend.R
# Author: Piyush Zaware
# Last updated: 2026-06-17
#
# Goal: Extract trends from Sitharaman's 8 full budget speeches.
#       The full-corpus STM assigns ~99% of topic weight to the "GST era"
#       topic for all her speeches (correct, but not discriminating within
#       her tenure). Instead we use:
#         1. Part A STM (B3) topics — more within-NS variation
#         2. Ideology scores (axis_market, axis_nationalist)
#         3. TF-IDF keyword trends — what vocabulary grew/shrank year-on-year
#
# IN
#   output/dtm/stm_parta_k10.rds       -- Part A STM (B3)
#   output/dtm/parta_dfm.rds           -- Part A DFM
#   output/dtm/speech_meta.csv
#   output/dtm/ideology_scores.csv
#   output/corpus_parta/*.txt          -- Part A texts for NS speeches
#
# OUT
#   output/tables/tab_ns_topics.csv
#   output/tables/tab_ns_projection.csv
#   output/tables/tab_ns_vocab_trend.csv
#   output/figures/fig_ns_topic_trend.png
#   output/figures/fig_ns_ideology_trend.png
#   output/figures/fig_ns_vocab_trend.png
# =============================================================================

library(stm)
library(quanteda)
library(tidytext)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(glue)
library(purrr)

root    <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
DTMDIR  <- file.path(root, "output", "dtm")
PARTADIR<- file.path(root, "output", "corpus_parta")
FIGDIR  <- file.path(root, "output", "figures")
TABDIR  <- file.path(root, "output", "tables")

ns_doc_ids <- c("bs201920",
                "Budget_Speech_2020-21", "Budget_Speech_2021-22",
                "Budget_Speech_2022-23", "Budget_Speech_2023-24",
                "Budget_Speech_2024-25", "Budget_Speech_2025-26",
                "Budget_Speech_2026-27")

meta <- read_csv(file.path(DTMDIR, "speech_meta.csv"), show_col_types = FALSE)
ns_meta <- meta %>%
  filter(doc_id %in% ns_doc_ids, budget_type == "full") %>%
  mutate(fm_name = "Nirmala Sitharaman") %>%
  arrange(fy_start)

message(glue("Sitharaman full budgets: {nrow(ns_meta)}"))

# =============================================================================
# PART 1: PART A STM TOPICS
# =============================================================================
#{
stm_parta <- readRDS(file.path(DTMDIR, "stm_parta_k10.rds"))
dfm_parta <- readRDS(file.path(DTMDIR, "parta_dfm.rds"))

theta_p    <- stm_parta$theta
all_docs_p <- docnames(dfm_parta)
dropped_p  <- "bs197778_I_"
doc_names_p <- all_docs_p[!all_docs_p %in% dropped_p]
stopifnot(nrow(theta_p) == length(doc_names_p))
rownames(theta_p) <- doc_names_p

frex       <- labelTopics(stm_parta, n = 5)$frex
topic_labs <- paste0("T", 1:10, ": ", apply(frex, 1, function(r) r[1]))

# Filter to NS speeches present in Part A model
ns_in_parta <- ns_meta$doc_id[ns_meta$doc_id %in% rownames(theta_p)]
message(glue("NS speeches in Part A model: {length(ns_in_parta)} of {nrow(ns_meta)}"))

ns_theta <- theta_p[ns_in_parta, , drop = FALSE]
# Align to ns_meta order
ns_theta <- ns_theta[match(ns_in_parta, rownames(ns_theta)), , drop = FALSE]
colnames(ns_theta) <- paste0("topic_", 1:10)

ns_topics <- ns_meta %>%
  filter(doc_id %in% ns_in_parta) %>%
  bind_cols(as_tibble(ns_theta))

write_csv(ns_topics, file.path(TABDIR, "tab_ns_topics.csv"))
message("Saved: tab_ns_topics.csv")

# Print topic proportions
message("\nNS topic proportions (Part A model):")
ns_topics %>%
  select(fy_start, starts_with("topic_")) %>%
  pivot_longer(starts_with("topic_"), names_to = "topic", values_to = "prop") %>%
  mutate(topic_label = topic_labs[as.integer(str_extract(topic, "[0-9]+"))]) %>%
  group_by(topic_label) %>%
  summarise(mean_prop = round(mean(prop), 3), .groups = "drop") %>%
  arrange(desc(mean_prop)) %>% print()
#}

# =============================================================================
# PART 2: IDEOLOGY SCORES
# =============================================================================
#{
ideology   <- read_csv(file.path(DTMDIR, "ideology_scores.csv"),
                        show_col_types = FALSE)
ns_ideology <- ideology %>%
  filter(doc_id %in% ns_meta$doc_id) %>%
  arrange(fy_start)

message(glue("\nNS ideology records: {nrow(ns_ideology)}"))
print(ns_ideology %>% select(fy_start, axis_market, axis_nationalist))
#}

# =============================================================================
# PART 3: VOCABULARY TREND (TF-IDF ACROSS HER YEARS)
# =============================================================================
#{
budget_stopwords <- c(
  "sir","madam","speaker","honourable","hon","ble","member","members",
  "house","august","rise","present","budget","speech","interim",
  "rupee","rupees","rs","crore","crores","lakh","lakhs","per","cent",
  "year","years","india","indian","government","central","national","union",
  "therefore","however","also","well","shall","will","may","must","total"
)
all_stopwords <- bind_rows(
  stop_words,
  tibble(word = budget_stopwords, lexicon = "custom")
) %>% distinct(word)

# Load NS Part A texts
ns_parta_texts <- map_dfr(ns_meta$doc_id, function(did) {
  path <- file.path(PARTADIR, paste0(did, "_parta.txt"))
  if (!file.exists(path)) return(NULL)
  text <- paste(readLines(path, warn = FALSE), collapse = " ")
  tibble(doc_id = did, text = text)
}) %>%
  left_join(ns_meta %>% select(doc_id, fy_start), by = "doc_id")

message(glue("NS Part A texts loaded: {nrow(ns_parta_texts)}"))

ns_tokens <- ns_parta_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(all_stopwords, by = "word") %>%
  filter(nchar(word) >= 3,
         !str_detect(word, "^[0-9,.()+\\-]+$"),
         str_detect(word, "^[a-z'\\-]+$"))

ns_counts <- ns_tokens %>% count(fy_start, word, sort = TRUE)

# TF-IDF across years: what words are distinctive to each year
ns_tfidf <- ns_counts %>%
  bind_tf_idf(word, fy_start, n) %>%
  arrange(fy_start, desc(tf_idf))

# Top 15 words per year
ns_top_words <- ns_tfidf %>%
  group_by(fy_start) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup()

message("\nTop distinctive words per year (NS Part A):")
ns_top_words %>%
  group_by(fy_start) %>%
  summarise(words = paste(word, collapse=", "), .groups = "drop") %>%
  print(n = Inf)

# Track specific policy-theme word groups over time
policy_themes <- list(
  "Infrastructure"    = c("infrastructure","highway","railway","port","airport",
                           "road","metro","corridor","connectivity","logistics"),
  "Green/Climate"     = c("green","climate","renewable","solar","wind","energy",
                           "transition","emission","sustainable","battery","hydrogen"),
  "Digital/Tech"      = c("digital","technology","startup","innovation","aadhaar",
                           "upi","fintech","ai","data","cyber","platform"),
  "Welfare/Social"    = c("welfare","poor","women","farmer","tribal","health",
                           "education","nutrition","housing","scheme","pmgsy"),
  "Manufacturing/PLI" = c("manufacturing","pli","production","exports","msme",
                           "atmanirbhar","domestic","capacity","industrial"),
  "Fiscal/Deficit"    = c("fiscal","deficit","consolidation","debt","gdp",
                           "borrowing","revenue","expenditure","capex","glide")
)

theme_trends <- map_dfr(names(policy_themes), function(theme) {
  words <- policy_themes[[theme]]
  ns_tokens %>%
    filter(word %in% words) %>%
    count(fy_start) %>%
    left_join(ns_tokens %>% count(fy_start, name = "total_words"), by = "fy_start") %>%
    mutate(share = n / total_words * 1000,   # per 1000 words
           theme = theme)
})

write_csv(theme_trends, file.path(TABDIR, "tab_ns_vocab_trend.csv"))
message("Saved: tab_ns_vocab_trend.csv")
#}

# =============================================================================
# FIT TRENDS AND PROJECT
# =============================================================================
#{
project_series <- function(df, y_col, label, min_pts = 4) {
  df2 <- df %>% rename(y = all_of(y_col)) %>% filter(!is.na(y))
  if (nrow(df2) < min_pts) return(NULL)
  mod   <- lm(y ~ fy_start, data = df2)
  preds <- as.data.frame(predict(mod, newdata = data.frame(fy_start = 2027),
                                  interval = "prediction", level = 0.80))
  tibble(
    dimension = label,
    slope     = coef(mod)["fy_start"],
    last_val  = tail(df2$y, 1),
    pred_2027 = preds$fit[1],
    pred_lo   = preds$lwr[1],
    pred_hi   = preds$upr[1],
    r_squared = summary(mod)$r.squared,
    trend     = case_when(
      abs(coef(mod)["fy_start"]) < 0.0001 ~ "stable",
      coef(mod)["fy_start"] > 0             ~ "rising",
      TRUE                                   ~ "falling"
    )
  )
}

# Topic projections (Part A model)
topic_cols   <- paste0("topic_", 1:10)
topic_proj   <- map_dfr(seq_along(topic_cols), function(i) {
  project_series(ns_topics, topic_cols[i], topic_labs[i], min_pts = 4)
})

# Ideology projections
ideo_proj <- map_dfr(c("axis_market", "axis_nationalist"), function(col) {
  project_series(ns_ideology, col, col, min_pts = 4)
}) %>%
  mutate(trend = case_when(
    abs(slope) < 0.001 ~ "stable",
    slope > 0           ~ "rising",
    TRUE                ~ "falling"
  ))

# Theme projections
theme_proj <- map_dfr(unique(theme_trends$theme), function(th) {
  df <- theme_trends %>% filter(theme == th) %>% select(fy_start, share)
  project_series(df, "share", th, min_pts = 4)
})

projection <- bind_rows(
  topic_proj   %>% mutate(type = "topic"),
  ideo_proj    %>% mutate(type = "ideology"),
  theme_proj   %>% mutate(type = "theme")
)

write_csv(projection, file.path(TABDIR, "tab_ns_projection.csv"))
message("\nSaved: tab_ns_projection.csv")

message("\nIdeology trends:")
print(ideo_proj %>% select(dimension, last_val, pred_2027, slope, trend, r_squared) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4))))

message("\nPolicy theme trends:")
print(theme_proj %>% select(dimension, last_val, pred_2027, slope, trend, r_squared) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4))))
#}

# =============================================================================
# FIGURES
# =============================================================================
#{
# Figure 1: Ideology trends + projection
ns_ideo_long <- ns_ideology %>%
  select(fy_start, axis_market, axis_nationalist) %>%
  pivot_longer(-fy_start, names_to = "dimension", values_to = "score") %>%
  mutate(dimension = recode(dimension,
    "axis_market"      = "Market-liberal axis",
    "axis_nationalist" = "Nationalist axis"
  ))

proj_pts <- ideo_proj %>%
  mutate(
    fy_start  = 2027,
    score     = pred_2027,
    dimension = recode(dimension,
      "axis_market"      = "Market-liberal axis",
      "axis_nationalist" = "Nationalist axis"
    )
  )

p_ideo <- ggplot(ns_ideo_long, aes(x = fy_start, y = score)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_line(colour = "#444", linewidth = 0.9) +
  geom_point(colour = "#444", size = 2.5) +
  geom_point(data = proj_pts, colour = "#b5310e", shape = 17, size = 4) +
  geom_errorbar(data = proj_pts,
                aes(ymin = pred_lo, ymax = pred_hi),
                colour = "#b5310e", width = 0.3) +
  scale_x_continuous(breaks = c(2019:2026, 2027)) +
  facet_wrap(~ dimension, ncol = 1, scales = "free_y") +
  labs(
    title    = "Nirmala Sitharaman's ideological trajectory (2019-2026)",
    subtitle = "Red triangle = linear trend projection for 2027. Error bars = 80% PI.",
    x = "Budget year", y = "Score"
  ) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(FIGDIR, "fig_ns_ideology_trend.png"), p_ideo,
       width = 9, height = 7, dpi = 150)
message("Saved: fig_ns_ideology_trend.png")

# Figure 2: Policy theme trends
p_themes <- ggplot(theme_trends,
    aes(x = fy_start, y = share, colour = theme, group = theme)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2019:2026) +
  scale_y_continuous(name = "Mentions per 1,000 words") +
  scale_colour_brewer(palette = "Dark2", name = NULL) +
  labs(
    title    = "Policy theme vocabulary in NS budget speeches (Part A)",
    subtitle = "Word frequency per 1,000 words. Based on Part A text only.",
    x = "Budget year"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(FIGDIR, "fig_ns_vocab_trend.png"), p_themes,
       width = 10, height = 6, dpi = 150)
message("Saved: fig_ns_vocab_trend.png")
#}

message("\nF1 complete.")
