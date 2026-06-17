# =============================================================================
# B3_parta_model.R
# Author: Piyush Zaware
# Last updated: 2026-06-16
#
# Goal: Repeat the B2 STM covariate analysis on Part A text only (the
#       macroeconomic/expenditure half of each speech, before the tax
#       proposals section). Compare BJP-INC effects to the full-speech B2
#       results to see whether the null result holds or disappears.
#
# IN
#   output/corpus_parta/*.txt     -- Part A texts from A3
#   output/dtm/speech_meta.csv    -- metadata
#
# OUT
#   output/dtm/parta_dfm.rds              -- Part A DFM
#   output/dtm/stm_parta_k10.rds          -- STM on Part A (k=10)
#   output/dtm/parta_ideology_scores.csv  -- ideology scores on Part A
#   output/figures/fig_parta_forest.png   -- BJP-INC forest plot (Part A only)
#   output/figures/fig_parta_vs_full.png  -- compare Part A vs full-speech effects
#   output/tables/tab_parta_effects.csv   -- effects table
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

root     <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
PARTADIR <- file.path(root, "output", "corpus_parta")
DTMDIR   <- file.path(root, "output", "dtm")
FIGDIR   <- file.path(root, "output", "figures")
TABDIR   <- file.path(root, "output", "tables")
TMPDIR   <- file.path(root, "tmp")

party_patch <- tribble(
  ~doc_id,                   ~fm_party_family, ~fy_start,
  "bs199697_I_",             "INC",            1996L,
  "bs196566_August_",        "INC",            1965L,
  "bs198081_I_",             "Other",          1980L,
  "bs195253_I_",             "INC",            1952L,
  "bs196768_I_",             "INC",            1967L,
  "bs195657_November_",      "INC",            1956L,
  "bs195758_I_",             "INC",            1957L,
  "bs197475_july_",          "INC",            1974L,
  "bs199192_I_",             "Other",          1991L,
  "bs196263_I_",             "INC",            1962L,
  "bs",                      "INC",            2014L,
  "Budget_Speech_2025-26",   "BJP",            2025L,
  "Budget_Speech_2026-27",   "BJP",            2026L
)

# -- CUSTOM STOP WORDS (same as A2) ------------------------------------------
#{
budget_stopwords <- c(
  "sir","madam","speaker","honourable","hon","ble","member","members",
  "house","august","rise","present","budget","speech","interim",
  "commend","commends","year","years","current","previous","next","last",
  "during","period","annual","per","cent","percent","annum",
  "rupee","rupees","rs","crore","crores","lakh","lakhs","paise",
  "thousand","million","billion","india","indian","government","central",
  "state","states","national","country","countries","union",
  "therefore","however","moreover","furthermore","accordingly","thus",
  "hence","also","well","shall","will","may","must","need","good",
  "great","new","total","including","number","numbers","part",
  "page","contents","introduction","th","nd","rd","st"
)
all_stopwords <- bind_rows(
  stop_words,
  tibble(word = budget_stopwords, lexicon = "custom")
) %>% distinct(word)
#}

# -- BUILD PART A DFM ---------------------------------------------------------
#{
meta_raw <- read_csv(file.path(DTMDIR, "speech_meta.csv"), show_col_types = FALSE) %>%
  distinct(doc_id, .keep_all = TRUE) %>%
  rows_patch(party_patch, by = "doc_id", unmatched = "ignore")

split_log <- read_csv(file.path(TMPDIR, "split_log.csv"), show_col_types = FALSE)

# Only use speeches where Part A was cleanly split (boundary found)
# Treat "no boundary" speeches as full-speech Part A
parta_files <- list.files(PARTADIR, pattern = "_parta\\.txt$", full.names = TRUE)
message(glue("Part A files available: {length(parta_files)}"))

parta_corpus <- map_dfr(parta_files, function(path) {
  doc_id <- str_remove(basename(path), "_parta\\.txt$")
  text   <- paste(readLines(path, warn = FALSE), collapse = " ")
  tibble(doc_id = doc_id, text = text)
}) %>%
  filter(str_count(text, "\\S+") >= 500)   # drop very short parts

message(glue("Speeches with usable Part A (>=500 words): {nrow(parta_corpus)}"))

# Tokenise and build DFM
tidy_parta <- parta_corpus %>%
  unnest_tokens(word, text) %>%
  anti_join(all_stopwords, by = "word") %>%
  filter(nchar(word) >= 3,
         !str_detect(word, "^[0-9,.()+\\-]+$"),
         str_detect(word, "^[a-z'\\-]+$"))

counts_parta <- tidy_parta %>% count(doc_id, word)

n_docs   <- n_distinct(counts_parta$doc_id)
doc_freq <- counts_parta %>%
  group_by(word) %>% summarise(df = n_distinct(doc_id), .groups = "drop")

keep <- doc_freq %>% filter(df >= 3, df <= 0.95 * n_docs) %>% pull(word)
counts_filtered <- counts_parta %>% filter(word %in% keep)

dfm_parta <- cast_dfm(counts_filtered, doc_id, word, n)
saveRDS(dfm_parta, file.path(DTMDIR, "parta_dfm.rds"))
message(glue("Part A DFM: {ndoc(dfm_parta)} docs x {nfeat(dfm_parta)} features"))
#}

# -- REMOVE PARTB-STYLE VOCABULARY AND TRIM -----------------------------------
#{
PARTB_TERMS <- c(
  "duty","duties","excise","customs","taxation","yield",
  "deduction","deductions","rebate","rebates","surcharge","cess",
  "levy","levied","levies","proposed","propose","proposes",
  "amendment","amendments","section","subsection","clause","notification"
)

# Align metadata
meta <- meta_raw %>%
  filter(doc_id %in% docnames(dfm_parta)) %>%
  distinct(doc_id, .keep_all = TRUE)

short_ids  <- meta %>% filter(words_clean < 1000) %>% pull(doc_id)
dfm_model  <- dfm_remove(dfm_parta[!docnames(dfm_parta) %in% short_ids, ], PARTB_TERMS)
dfm_model  <- dfm_trim(dfm_model, min_termfreq = 2)
dfm_stm    <- convert(dfm_model, to = "stm")
doc_ids    <- names(dfm_stm$documents)

meta_model <- meta %>%
  filter(doc_id %in% doc_ids) %>%
  arrange(match(doc_id, doc_ids)) %>%
  mutate(party = factor(fm_party_family, levels = c("INC", "BJP", "Other")))

# Drop NA party
na_ids <- meta_model %>% filter(is.na(party)) %>% pull(doc_id)
if (length(na_ids) > 0) {
  dfm_model  <- dfm_model[!docnames(dfm_model) %in% na_ids, ]
  dfm_stm    <- convert(dfm_model, to = "stm")
  doc_ids    <- names(dfm_stm$documents)
  meta_model <- meta_model %>%
    filter(doc_id %in% doc_ids) %>%
    arrange(match(doc_id, doc_ids))
}

message(glue("Documents for modelling: {nrow(meta_model)}  |  NA party: {sum(is.na(meta_model$party))}"))
print(table(meta_model$party, useNA = "ifany"))
#}

# -- FIT STM WITH COVARIATES --------------------------------------------------
#{
K         <- 10
MOD_FILE  <- file.path(DTMDIR, "stm_parta_k10.rds")

if (file.exists(MOD_FILE)) {
  message("Loading cached Part A model ...")
  stm_parta <- readRDS(MOD_FILE)
} else {
  message(glue("\n=== FITTING STM (k={K}) ON PART A ===\n"))
  stm_parta <- stm(
    dfm_stm$documents, dfm_stm$vocab, K = K,
    prevalence = ~ party + s(fy_start, df = 5),
    data       = meta_model,
    seed       = 42, verbose = TRUE
  )
  saveRDS(stm_parta, MOD_FILE)
  message("Model saved.")
}

frex_words   <- labelTopics(stm_parta, n = 8)$frex
topic_labels <- paste0("T", 1:K, ": ", map_chr(1:K, ~ frex_words[.x, 1]))

message("\nPart A topic labels:")
walk(1:K, ~ message(glue("  T{.x}: {paste(frex_words[.x, 1:5], collapse=', ')}")))
#}

# -- ESTIMATE EFFECTS ---------------------------------------------------------
#{
effects   <- estimateEffect(1:K ~ party + s(fy_start, df = 5),
                             stm_parta, meta_model, uncertainty = "Global")
eff_sum   <- summary(effects)

extract_effect <- function(k) {
  tab     <- eff_sum$tables[[k]]
  bjp_row <- which(rownames(tab) == "partyBJP")
  if (length(bjp_row) == 0) return(NULL)
  tibble(
    topic       = k, topic_label = topic_labels[k],
    estimate    = tab[bjp_row, "Estimate"],
    se          = tab[bjp_row, "Std. Error"],
    pval        = tab[bjp_row, "Pr(>|t|)"],
    ci_lo       = tab[bjp_row, "Estimate"] - 1.96 * tab[bjp_row, "Std. Error"],
    ci_hi       = tab[bjp_row, "Estimate"] + 1.96 * tab[bjp_row, "Std. Error"],
    source      = "Part A only"
  )
}

effects_parta <- map_dfr(1:K, extract_effect) %>%
  mutate(sig = case_when(pval < 0.01 ~ "**", pval < 0.05 ~ "*", TRUE ~ ""),
         direction = if_else(estimate > 0, "BJP higher", "INC higher"))

write_csv(effects_parta, file.path(TABDIR, "tab_parta_effects.csv"))

message("\nPart A BJP-INC effects:")
print(effects_parta %>% select(topic, topic_label, estimate, se, pval, sig))
#}

# -- FIGURE 1: PART A FOREST PLOT ---------------------------------------------
#{
party_col <- c("BJP higher" = "#FF9933", "INC higher" = "#19AAED")

p_forest_parta <- ggplot(effects_parta,
    aes(x = reorder(topic_label, estimate), y = estimate, colour = direction)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.25, linewidth = 0.6) +
  geom_point(size = 3) +
  geom_text(aes(label = sig, y = ci_hi + 0.006),
            size = 4, fontface = "bold", show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(values = party_col, name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "BJP vs INC: Part A only (macro & expenditure text)",
    subtitle = "Tax-proposal section removed before modelling\n* p<0.05  ** p<0.01  |  95% CI",
    x = NULL, y = "Difference in topic proportion (BJP minus INC)"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())

ggsave(file.path(FIGDIR, "fig_parta_forest.png"), p_forest_parta,
       width = 9, height = 6, dpi = 150)
message("Saved: fig_parta_forest.png")
#}

# -- FIGURE 2: PART A vs FULL SPEECH COMPARISON -------------------------------
#{
# Load full-speech B2 effects for comparison
full_effects_file <- file.path(TABDIR, "tab_stm_effects.csv")
if (file.exists(full_effects_file)) {
  effects_full <- read_csv(full_effects_file, show_col_types = FALSE) %>%
    mutate(source = "Full speech")

  # Align topic numbers — match by topic number, relabel with Part A FREX words
  combined <- bind_rows(
    effects_parta %>% select(topic, estimate, ci_lo, ci_hi, pval, sig, source),
    effects_full  %>% select(topic, estimate, ci_lo, ci_hi, pval, sig, source)
  ) %>%
    mutate(
      topic_label = paste0("Topic ", topic),
      source      = factor(source, levels = c("Full speech", "Part A only"))
    )

  p_compare <- ggplot(combined,
      aes(x = topic_label, y = estimate,
          colour = source, shape = source,
          ymin = ci_lo, ymax = ci_hi)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
    geom_errorbar(position = position_dodge(0.55), width = 0.25, linewidth = 0.55) +
    geom_point(position   = position_dodge(0.55), size = 2.8) +
    coord_flip() +
    scale_colour_manual(values = c("Full speech" = "grey50", "Part A only" = "#B5440E"),
                        name = NULL) +
    scale_shape_manual(values  = c("Full speech" = 16, "Part A only" = 17), name = NULL) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title    = "Does removing tax-proposal text change the BJP-INC result?",
      subtitle = "Grey = full speech (B2)   |   Brown = Part A only (B3)\nDashed line at zero = no difference",
      x = NULL, y = "BJP minus INC topic proportion"
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "top", panel.grid.major.y = element_blank())

  ggsave(file.path(FIGDIR, "fig_parta_vs_full.png"), p_compare,
         width = 9, height = 6, dpi = 150)
  message("Saved: fig_parta_vs_full.png")
}
#}

# -- SUMMARY ------------------------------------------------------------------
#{
n_sig <- sum(effects_parta$pval < 0.05, na.rm = TRUE)
message(glue("\n=== B3 COMPLETE ==="))
message(glue("Part A documents: {nrow(meta_model)}"))
message(glue("Significant BJP-INC topics (p<0.05): {n_sig} of {K}"))
if (n_sig > 0) {
  effects_parta %>% filter(pval < 0.05) %>%
    select(topic_label, estimate, pval, sig) %>% print()
}
#}
