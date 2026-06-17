# =============================================================================
# B1_lda_topics.R
# Author: Piyush Zaware
# Last updated: 2026-06-16
#
# Goal: Fit LDA/STM topic models to the budget speech corpus. Select optimal k
#       using semantic coherence and exclusivity diagnostics, then fit the final
#       model and extract topic-word and document-topic distributions.
#
# Model: STM (Structural Topic Model) with NO prevalence covariates in this
#        script — equivalent to LDA but with better diagnostics. Covariates
#        (party, year) are added in B2_stm_covariates.R.
#
# k selection range: 5, 8, 10, 12, 15, 20
#   Expected: 8-12 topics for this corpus (80 years, 92 speeches)
#
# Exclusion: speeches with < 1000 clean words are dropped before modelling
#   (very short interims distort topic estimates)
#
# IN
#   output/dtm/budget_dfm.rds      -- quanteda DFM (92 docs x 8308 features)
#   output/dtm/speech_meta.csv     -- metadata per document
#
# OUT
#   output/figures/fig_k_selection.png      -- 4-panel k diagnostic plot
#   output/figures/fig_topic_words.png      -- top FREX words per topic (bar)
#   output/figures/fig_topic_time.png       -- topic prevalence over time
#   output/figures/fig_topic_party.png      -- BJP vs INC topic profiles
#   output/dtm/stm_model_k{K}.rds           -- fitted STM model at best k
#   output/tables/tab_topic_words.csv       -- top words per topic
#   output/tables/tab_topic_prevalence.csv  -- doc x topic theta matrix
#   tmp/k_search_results.rds                -- raw searchK() output
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

set.seed(42)

# -- PATHS --------------------------------------------------------------------
#{
root    <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
DTMDIR  <- file.path(root, "output", "dtm")
FIGDIR  <- file.path(root, "output", "figures")
TABDIR  <- file.path(root, "output", "tables")
TMPDIR  <- file.path(root, "tmp")

dir.create(FIGDIR, showWarnings = FALSE)
dir.create(TABDIR, showWarnings = FALSE)
#}

# -- LOAD AND ALIGN DATA ------------------------------------------------------
#{
dfm  <- readRDS(file.path(DTMDIR, "budget_dfm.rds"))
meta <- read_csv(file.path(DTMDIR, "speech_meta.csv"), show_col_types = FALSE)

# Align metadata to DFM row order (DFM docnames = doc_id)
meta_aligned <- meta %>%
  filter(doc_id %in% docnames(dfm)) %>%
  distinct(doc_id, .keep_all = TRUE) %>%   # remove duplicates from many-to-many join in A1
  arrange(match(doc_id, docnames(dfm)))

# Drop very short speeches (< 1000 clean words) — interims and the Dec 1971 budget
short_ids <- meta_aligned %>% filter(words_clean < 1000) %>% pull(doc_id)
dfm_model <- dfm[!docnames(dfm) %in% short_ids, ]
meta_model <- meta_aligned %>% filter(!doc_id %in% short_ids)

# Remove Part B (tax proposals) vocabulary that dominates every speech.
# These words appear in the tax clause section of EVERY budget and drown out
# substantive policy topics. Removing here only; A2 corpus is unchanged.
PARTB_TERMS <- c(
  "duty", "duties", "excise", "customs", "taxation",
  "yield", "deduction", "deductions", "rebate", "rebates",
  "surcharge", "cess", "levy", "levied", "levies",
  "proposed", "propose", "proposes", "amendment", "amendments",
  "section", "subsection", "clause", "notification"
)
dfm_model <- dfm_remove(dfm_model, PARTB_TERMS)
dfm_model <- dfm_trim(dfm_model, min_termfreq = 2)   # re-trim after removal
message(glue("DFM after Part-B trim: {ndoc(dfm_model)} docs x {nfeat(dfm_model)} features"))

message(glue("Documents for modelling: {ndoc(dfm_model)} (excluded {length(short_ids)} short)"))
message(glue("Years covered: {min(meta_model$fy_start, na.rm=TRUE)}–{max(meta_model$fy_start, na.rm=TRUE)}"))
#}

# -- CONVERT TO STM FORMAT ----------------------------------------------------
#{
dfm_stm <- convert(dfm_model, to = "stm")

# Build covariate data frame (used optionally; B1 runs without covariates)
meta_stm <- meta_model %>%
  mutate(
    party_bjp    = as.integer(fm_party_family == "BJP"),
    party_inc    = as.integer(fm_party_family == "INC"),
    fy_start_c   = scale(fy_start)[,1],     # centred + scaled year
    election_yr  = as.integer(budget_type == "interim")
  )
#}

# -- K SELECTION: searchK() ---------------------------------------------------
# Computes held-out likelihood, semantic coherence, exclusivity, residuals
# across a range of k values. Use 10-fold held-out for likelihood.
#{
K_RANGE <- c(5, 8, 10, 12, 15, 20)
SEARCH_FILE <- file.path(TMPDIR, "k_search_results.rds")

if (file.exists(SEARCH_FILE)) {
  message("Loading cached searchK results ...")
  k_search <- readRDS(SEARCH_FILE)
} else {
  message(glue("\n=== RUNNING searchK() for k = {paste(K_RANGE, collapse=', ')} ===\n"))
  k_search <- searchK(
    dfm_stm$documents,
    dfm_stm$vocab,
    K              = K_RANGE,
    data           = meta_stm,
    seed           = 42,
    verbose        = TRUE
  )
  saveRDS(k_search, SEARCH_FILE)
  message("searchK results saved to tmp/k_search_results.rds")
}
#}

# -- K SELECTION PLOT ---------------------------------------------------------
#{
k_df <- as.data.frame(k_search$results) %>%
  mutate(across(everything(), as.numeric))

# Normalise each metric to 0-1 for overlay plot
norm01 <- function(x) (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))

k_long <- k_df %>%
  select(K, semcoh, exclus, heldout, residual) %>%
  mutate(
    `Semantic Coherence`   = semcoh,
    `Exclusivity`          = exclus,
    `Held-out Likelihood`  = heldout,
    `Residuals`            = residual
  ) %>%
  select(K, `Semantic Coherence`, `Exclusivity`, `Held-out Likelihood`, `Residuals`) %>%
  pivot_longer(-K, names_to = "metric", values_to = "value")

p_k <- ggplot(k_long, aes(x = K, y = value)) +
  geom_line(colour = "#2c7bb6", linewidth = 0.8) +
  geom_point(colour = "#2c7bb6", size = 2.5) +
  geom_vline(xintercept = 10, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = K_RANGE) +
  labs(
    title    = "Topic Model Diagnostics: Selecting k",
    subtitle = "Dashed line marks selected k = 10",
    x        = "Number of Topics (k)",
    y        = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = "grey92"))

ggsave(file.path(FIGDIR, "fig_k_selection.png"), p_k,
       width = 8, height = 5, dpi = 150)
message("Figure saved: fig_k_selection.png")

# Print table of metrics
message("\n=== K SELECTION METRICS ===")
print(k_df %>% select(K, semcoh, exclus, heldout, residual))
#}

# -- FIT FINAL MODEL AT SELECTED K --------------------------------------------
# Default k = 10. Change K_FINAL if diagnostics suggest otherwise.
#{
K_FINAL   <- 10
MODEL_FILE <- file.path(DTMDIR, glue("stm_model_k{K_FINAL}.rds"))

if (file.exists(MODEL_FILE)) {
  message(glue("\nLoading cached STM model (k={K_FINAL}) ..."))
  stm_fit <- readRDS(MODEL_FILE)
} else {
  message(glue("\n=== FITTING STM MODEL k={K_FINAL} ===\n"))
  stm_fit <- stm(
    dfm_stm$documents,
    dfm_stm$vocab,
    K         = K_FINAL,
    data      = meta_stm,
    seed      = 42,
    verbose   = TRUE
  )
  saveRDS(stm_fit, MODEL_FILE)
  message(glue("Model saved: stm_model_k{K_FINAL}.rds"))
}
#}

# -- EXTRACT TOP WORDS PER TOPIC ----------------------------------------------
#{
message("\n=== TOP WORDS PER TOPIC (FREX) ===\n")

# FREX balances frequency and exclusivity — best for labelling topics
frex_words   <- labelTopics(stm_fit, n = 10)$frex
prob_words   <- labelTopics(stm_fit, n = 10)$prob
lift_words   <- labelTopics(stm_fit, n = 10)$lift

topic_words_df <- map_dfr(1:K_FINAL, function(k) {
  tibble(
    topic         = k,
    frex_top10    = paste(frex_words[k, ], collapse = ", "),
    prob_top10    = paste(prob_words[k, ], collapse = ", "),
    lift_top10    = paste(lift_words[k, ], collapse = ", ")
  )
})

print(topic_words_df %>% select(topic, frex_top10))
write_csv(topic_words_df, file.path(TABDIR, "tab_topic_words.csv"))
#}

# -- TOPIC PREVALENCE (THETA MATRIX) ------------------------------------------
#{
theta <- as.data.frame(stm_fit$theta)
colnames(theta) <- paste0("topic_", 1:K_FINAL)
theta$doc_id    <- meta_model$doc_id[!meta_model$doc_id %in% short_ids]

theta_long <- theta %>%
  left_join(meta_model %>% select(doc_id, budget_year, fy_start, budget_type,
                                   fm_name, fm_party, fm_party_family,
                                   pm_name, government_coalition),
            by = "doc_id") %>%
  pivot_longer(starts_with("topic_"), names_to = "topic", values_to = "prevalence") %>%
  mutate(topic_num = as.integer(str_remove(topic, "topic_")))

write_csv(theta_long, file.path(TABDIR, "tab_topic_prevalence.csv"))
#}

# -- FIGURE: TOP FREX WORDS PER TOPIC -----------------------------------------
#{
# Top 8 probability words per topic for bar chart
beta_df <- tidy(stm_fit, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>%
  ungroup() %>%
  mutate(topic_label = paste0("Topic ", topic))

p_words <- ggplot(beta_df, aes(x = reorder_within(term, beta, topic), y = beta)) +
  geom_col(fill = "#2c7bb6", alpha = 0.85) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ topic_label, scales = "free_y", ncol = 5) +
  labs(
    title    = glue("Top words per topic (k = {K_FINAL})"),
    subtitle = "Ranked by per-topic word probability (beta)",
    x = NULL, y = "Probability"
  ) +
  theme_bw(base_size = 9) +
  theme(strip.background = element_rect(fill = "grey92"),
        axis.text.y = element_text(size = 7))

ggsave(file.path(FIGDIR, "fig_topic_words.png"), p_words,
       width = 14, height = 8, dpi = 150)
message("Figure saved: fig_topic_words.png")
#}

# -- FIGURE: TOPIC PREVALENCE OVER TIME ----------------------------------------
#{
# Average topic prevalence by decade
theta_time <- theta_long %>%
  filter(budget_type == "full", !is.na(fy_start)) %>%
  mutate(decade = floor(fy_start / 10) * 10) %>%
  group_by(decade, topic_num) %>%
  summarise(mean_prev = mean(prevalence), .groups = "drop") %>%
  mutate(topic_label = paste0("T", topic_num))

# Annotate with top FREX word as topic label
top_frex_word <- map_chr(1:K_FINAL, ~ frex_words[.x, 1])
theta_time <- theta_time %>%
  mutate(topic_label = paste0("T", topic_num, ": ", top_frex_word[topic_num]))

p_time <- ggplot(theta_time, aes(x = decade, y = mean_prev,
                                  colour = topic_label, group = topic_label)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(1940, 2020, 10),
                     labels = paste0(seq(1940, 2020, 10), "s")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_brewer(palette = "Paired") +
  labs(
    title    = "Topic prevalence by decade (full budgets only)",
    subtitle = "Each line is average document-topic proportion across budgets in that decade",
    x = "Decade", y = "Mean topic proportion", colour = "Topic"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right", legend.text = element_text(size = 8))

ggsave(file.path(FIGDIR, "fig_topic_time.png"), p_time,
       width = 12, height = 6, dpi = 150)
message("Figure saved: fig_topic_time.png")
#}

# -- FIGURE: BJP vs INC TOPIC PROFILES ----------------------------------------
#{
theta_party <- theta_long %>%
  filter(fm_party_family %in% c("BJP", "INC"), budget_type == "full") %>%
  group_by(fm_party_family, topic_num) %>%
  summarise(mean_prev = mean(prevalence), se = sd(prevalence) / sqrt(n()),
            .groups = "drop") %>%
  mutate(topic_label = paste0("T", topic_num, ": ", top_frex_word[topic_num]))

p_party <- ggplot(theta_party,
                  aes(x = reorder(topic_label, topic_num),
                      y = mean_prev, fill = fm_party_family)) +
  geom_col(position = position_dodge(0.7), width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_prev - se, ymax = mean_prev + se),
                position = position_dodge(0.7), width = 0.25, linewidth = 0.4) +
  scale_fill_manual(values = c("BJP" = "#FF9933", "INC" = "#19AAED"),
                    name = "Party") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  labs(
    title    = "Topic profiles: BJP vs INC Finance Ministers",
    subtitle = "Mean topic proportion in full budgets; error bars = ±1 SE",
    x = NULL, y = "Mean topic proportion"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(FIGDIR, "fig_topic_party.png"), p_party,
       width = 10, height = 6, dpi = 150)
message("Figure saved: fig_topic_party.png")
#}

# -- SUMMARY ------------------------------------------------------------------
#{
message(glue("
=== B1 COMPLETE ===
Model:        STM k={K_FINAL}, {ndoc(dfm_model)} speeches, {ncol(stm_fit$beta[[1]])} vocab terms
Outputs:
  output/dtm/stm_model_k{K_FINAL}.rds
  output/tables/tab_topic_words.csv
  output/tables/tab_topic_prevalence.csv
  output/figures/fig_k_selection.png
  output/figures/fig_topic_words.png
  output/figures/fig_topic_time.png
  output/figures/fig_topic_party.png

Next: review fig_topic_words.png to label topics, then run B2_stm_covariates.R
"))
#}
