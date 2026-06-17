# =============================================================================
# D1_election_effects.R
# Author: Piyush Zaware
# Last updated: 2026-06-16
#
# Goal: Compare interim (pre-election, vote-on-account) budgets to full
#       budgets from the same Finance Minister. The within-FM comparison
#       controls for ideology and career stage — if interim budgets differ,
#       it is because of the electoral cycle, not the person.
#
# FMs with both interim and full budgets:
#   - Morarji Desai       (INC): full 1959–60 to 1963–64, interim 1967–68
#   - T.T. Krishnamachari (INC): full 1956–58, 1964–65
#   - P. Chidambaram      (INC): full 1997–98 to 2008–09, interim 2014–15
#   - Yashwant Sinha      (BJP): full 2000–01 to 2002–03, interim 1999–00
#   - Nirmala Sitharaman  (BJP): full 2019–20 to 2026–27, interim 2024–25
#   - Arun Jaitley        (BJP): full 2014–15 to 2017–18, interim 2019–20 (partial)
#
# Strategy:
#   1. Load full-speech clean DFM from B1/B2 (or rebuild from corpus_clean)
#   2. Tag each speech as interim=TRUE / interim=FALSE using metadata
#   3. Run TF-IDF to find vocabulary that shifts between interim and full budgets
#   4. Fit a topic model (STM, k=10) with prevalence ~ interim * fm_name
#   5. Use within-FM contrasts to test whether interim budgets push different topics
#   6. Dictionary scoring: populist/welfare words more common in interim budgets?
#
# IN
#   output/corpus_clean/*.txt      -- cleaned texts from A2
#   output/dtm/speech_meta.csv     -- metadata (includes is_interim flag)
#
# OUT
#   output/figures/fig_interim_tfidf.png      -- top words uniquely high in interim vs full
#   output/figures/fig_interim_fm_profiles.png -- per-FM ideology score: interim vs full
#   output/figures/fig_interim_stm.png        -- STM effect: interim flag coefficient
#   output/tables/tab_interim_vocabulary.csv  -- top 30 words by interim vs full LR
#   output/tables/tab_fm_interim_pairs.csv    -- FM pair summary table
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
library(forcats)

set.seed(42)

root     <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
CLEANDIR <- file.path(root, "output", "corpus_clean")
DTMDIR   <- file.path(root, "output", "dtm")
FIGDIR   <- file.path(root, "output", "figures")
TABDIR   <- file.path(root, "output", "tables")
TMPDIR   <- file.path(root, "tmp")

party_patch <- tribble(
  ~doc_id,                   ~fm_party_family, ~fm_name,              ~fy_start,
  "bs199697_I_",             "INC",            "Manmohan Singh",       1996L,
  "bs196566_August_",        "INC",            "T.T. Krishnamachari",  1965L,
  "bs198081_I_",             "Other",          "H.M. Patel",           1980L,
  "bs195253_I_",             "INC",            "C.D. Deshmukh",        1952L,
  "bs196768_I_",             "INC",            "Morarji Desai",        1967L,
  "bs195657_November_",      "INC",            "T.T. Krishnamachari",  1956L,
  "bs195758_I_",             "INC",            "T.T. Krishnamachari",  1957L,
  "bs197475_july_",          "INC",            "Yashwantrao Chavan",   1974L,
  "bs199192_I_",             "Other",          "Yashwant Sinha",       1991L,
  "bs196263_I_",             "INC",            "Morarji Desai",        1962L,
  "bs",                      "INC",            "P. Chidambaram",       2014L,
  "Budget_Speech_2025-26",   "BJP",            "Nirmala Sitharaman",   2025L,
  "Budget_Speech_2026-27",   "BJP",            "Nirmala Sitharaman",   2026L
)

# -- CUSTOM STOP WORDS --------------------------------------------------------
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

# -- LOAD METADATA AND TAG INTERIM SPEECHES -----------------------------------
#{
meta_raw <- read_csv(file.path(DTMDIR, "speech_meta.csv"), show_col_types = FALSE) %>%
  distinct(doc_id, .keep_all = TRUE) %>%
  rows_patch(party_patch, by = "doc_id", unmatched = "ignore")

# metadata already has budget_type column: "full" | "interim"
# patch party_patch rows may lack budget_type; infer from doc_id for those
meta <- meta_raw %>%
  mutate(
    budget_type = replace_na(budget_type, "full"),
    is_interim  = budget_type == "interim"
  )

interim_count <- sum(meta$is_interim, na.rm = TRUE)
message(glue("Total speeches: {nrow(meta)}"))
message(glue("Tagged as interim: {interim_count}"))
print(meta %>% filter(is_interim) %>% select(doc_id, fy_start, fm_name, budget_type))
#}

# -- FM PAIRS: ONLY FMs WITH BOTH INTERIM AND FULL ----------------------------
#{
# For each FM, count interim and full budgets
fm_summary <- meta %>%
  filter(!is.na(fm_name)) %>%
  group_by(fm_name, fm_party_family) %>%
  summarise(
    n_full    = sum(!is_interim, na.rm = TRUE),
    n_interim = sum(is_interim,  na.rm = TRUE),
    n_total   = n(),
    .groups   = "drop"
  ) %>%
  filter(n_interim >= 1, n_full >= 1) %>%
  arrange(desc(n_total))

message("\nFMs with both interim and full budgets:")
print(fm_summary)
write_csv(fm_summary, file.path(TABDIR, "tab_fm_interim_pairs.csv"))

paired_fms <- fm_summary$fm_name
#}

# -- LOAD CLEANED CORPUS AND BUILD DFM ----------------------------------------
#{
clean_files <- list.files(CLEANDIR, pattern = "\\.txt$", full.names = TRUE)
message(glue("\nLoading {length(clean_files)} cleaned speeches ..."))

corpus_raw <- map_dfr(clean_files, function(path) {
  doc_id <- str_remove(basename(path), "_clean\\.txt$")
  text   <- paste(readLines(path, warn = FALSE), collapse = " ")
  tibble(doc_id = doc_id, text = text)
})

corpus_meta <- corpus_raw %>%
  inner_join(meta, by = "doc_id") %>%
  filter(!is.na(fm_party_family))

message(glue("Speeches with metadata: {nrow(corpus_meta)}"))

# Tokenise
tidy_tokens <- corpus_meta %>%
  unnest_tokens(word, text) %>%
  anti_join(all_stopwords, by = "word") %>%
  filter(nchar(word) >= 3,
         !str_detect(word, "^[0-9,.()+\\-]+$"),
         str_detect(word, "^[a-z'\\-]+$"))

counts_all <- tidy_tokens %>% count(doc_id, word)
#}

# -- TF-IDF: INTERIM vs FULL BUDGET VOCABULARY --------------------------------
#{
# tidy_tokens already carries is_interim from corpus_meta inner_join
group_tokens <- tidy_tokens

group_counts <- group_tokens %>%
  mutate(group = if_else(is_interim, "interim", "full")) %>%
  count(group, word, sort = TRUE)

# Log-likelihood ratio of each word in interim vs full
total_interim <- sum(group_counts$n[group_counts$group == "interim"])
total_full    <- sum(group_counts$n[group_counts$group == "full"])

lr_table <- group_counts %>%
  pivot_wider(names_from = group, values_from = n, values_fill = 0) %>%
  mutate(
    rate_interim = (interim + 0.5) / (total_interim + 1),
    rate_full    = (full    + 0.5) / (total_full    + 1),
    log_ratio    = log2(rate_interim / rate_full),
    n_total      = interim + full
  ) %>%
  filter(n_total >= 15) %>%   # minimum frequency threshold
  arrange(desc(abs(log_ratio)))

write_csv(lr_table %>% slice_head(n = 100), file.path(TABDIR, "tab_interim_vocabulary.csv"))

# Plot top 20 words in each direction
top_interim <- lr_table %>% filter(log_ratio > 0) %>% slice_head(n = 20)
top_full    <- lr_table %>% filter(log_ratio < 0) %>% slice_head(n = 20)

vocab_plot_data <- bind_rows(
  top_interim %>% mutate(direction = "Overrepresented in INTERIM budgets"),
  top_full    %>% mutate(direction = "Overrepresented in FULL budgets")
) %>%
  mutate(word = fct_reorder(word, log_ratio))

p_tfidf <- ggplot(vocab_plot_data,
    aes(x = log_ratio, y = word, fill = direction)) +
  geom_col(show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c(
    "Overrepresented in INTERIM budgets" = "#D45F00",
    "Overrepresented in FULL budgets"    = "#1D6FA4"
  )) +
  facet_wrap(~ direction, scales = "free") +
  labs(
    title    = "Vocabulary shifts between interim and full Union Budgets",
    subtitle = "Log2 ratio of word rates (interim / full). Minimum 15 total uses.",
    x        = "Log2 rate ratio (positive = more common in interim)",
    y        = NULL
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(FIGDIR, "fig_interim_tfidf.png"), p_tfidf,
       width = 11, height = 7, dpi = 150)
message("Saved: fig_interim_tfidf.png")
#}

# -- IDEOLOGY SCORES ON INTERIM vs FULL ---------------------------------------
#{
dict_socialist   <- c("nationalise","nationalisation","cooperative","welfare",
                       "subsidy","subsidies","poor","poverty","labour","labourers",
                       "ration","rationing","planned","five year plan","directive",
                       "public sector","state sector","redistribution","equality",
                       "unemployment","rural","peasant","farmer","farmer",
                       "food grain","food grains","minimum support")

dict_capitalist  <- c("private sector","market","liberalise","liberalisation",
                       "privatise","privatisation","disinvest","disinvestment",
                       "equity","efficiency","competition","productivity",
                       "reform","deregulate","deregulation","fdi","foreign investment",
                       "investment","venture","enterprise","entrepreneurship")

dict_nationalist <- c("swadeshi","indigenous","self-reliance","atmanirbhar",
                       "import substitution","protection","protectionist","tariff",
                       "make in india","domestic industry","local")

dict_globalist   <- c("exports","export promotion","gatt","wto","convertibility",
                       "globalisation","globalization","multilateral","free trade",
                       "trade liberalisation","current account","balance of payments",
                       "foreign exchange","world bank","imf","open economy")

score_ideology <- function(tokens_df, doc_ids_vec) {
  score_axis <- function(tok, plus_words, minus_words) {
    tok %>%
      mutate(
        plus  = word %in% plus_words,
        minus = word %in% minus_words
      ) %>%
      group_by(doc_id) %>%
      summarise(
        n_plus   = sum(plus),
        n_minus  = sum(minus),
        n_words  = n(),
        score    = (n_plus - n_minus) / pmax(n_plus + n_minus, 1),
        .groups  = "drop"
      )
  }

  market_score      <- score_axis(tokens_df, dict_capitalist, dict_socialist)
  nationalist_score <- score_axis(tokens_df, dict_nationalist, dict_globalist)

  market_score %>%
    select(doc_id, market_score = score) %>%
    left_join(nationalist_score %>% select(doc_id, nationalist_score = score),
              by = "doc_id")
}

ideology_scores <- score_ideology(group_tokens, NULL) %>%
  left_join(meta %>% select(doc_id, fm_name, fm_party_family, fy_start, is_interim),
            by = "doc_id") %>%
  filter(!is.na(fm_name), fm_name %in% paired_fms)

# -- FIGURE: Per-FM ideology score, interim vs full ---------------------------

p_fm_profiles <- ideology_scores %>%
  filter(!is.na(is_interim)) %>%
  mutate(
    budget_type = if_else(is_interim, "Interim", "Full"),
    fm_label    = glue("{fm_name}\n({fm_party_family})")
  ) %>%
  ggplot(aes(x = market_score, y = nationalist_score,
             colour = budget_type, shape = budget_type,
             label = fy_start)) +
  geom_point(size = 3, alpha = 0.85) +
  geom_line(aes(group = fm_name), colour = "grey70", linewidth = 0.4) +
  scale_colour_manual(values = c("Full" = "#1D6FA4", "Interim" = "#D45F00"), name = NULL) +
  scale_shape_manual(values  = c("Full" = 16, "Interim" = 17), name = NULL) +
  facet_wrap(~ fm_label, ncol = 3, scales = "free") +
  labs(
    title    = "Do Finance Ministers talk differently in election-year (interim) budgets?",
    subtitle = "Each point = one budget speech. Lines connect same FM across budget types.",
    x        = "Market axis (right = more capitalist)",
    y        = "Nationalist axis (up = more nationalist)"
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "top", strip.text = element_text(size = 8))

ggsave(file.path(FIGDIR, "fig_interim_fm_profiles.png"), p_fm_profiles,
       width = 12, height = 8, dpi = 150)
message("Saved: fig_interim_fm_profiles.png")
#}

# -- STM WITH INTERIM FLAG ----------------------------------------------------
#{
# Build DFM and fit STM with interim flag as covariate,
# restricting to FMs with paired speeches

corpus_paired <- corpus_meta %>% filter(fm_name %in% paired_fms)
message(glue("\nPaired FM speeches: {nrow(corpus_paired)} ({sum(corpus_paired$is_interim)} interim)"))

tidy_paired <- corpus_paired %>%
  unnest_tokens(word, text) %>%
  anti_join(all_stopwords, by = "word") %>%
  filter(nchar(word) >= 3,
         !str_detect(word, "^[0-9,.()+\\-]+$"),
         str_detect(word, "^[a-z'\\-]+$"))

counts_paired <- tidy_paired %>% count(doc_id, word)

n_docs_p   <- n_distinct(counts_paired$doc_id)
doc_freq_p <- counts_paired %>%
  group_by(word) %>% summarise(df = n_distinct(doc_id), .groups = "drop")

keep_p <- doc_freq_p %>% filter(df >= 2, df <= 0.95 * n_docs_p) %>% pull(word)
counts_p_filt <- counts_paired %>% filter(word %in% keep_p)

dfm_paired <- cast_dfm(counts_p_filt, doc_id, word, n)
dfm_stm_p  <- convert(dfm_paired, to = "stm")
doc_ids_p  <- names(dfm_stm_p$documents)

meta_p <- corpus_paired %>%
  filter(doc_id %in% doc_ids_p) %>%
  arrange(match(doc_id, doc_ids_p)) %>%
  mutate(
    is_interim_int = as.integer(is_interim),
    fm_factor      = factor(fm_name)
  )

message(glue("Paired FM DFM: {ndoc(dfm_paired)} docs x {nfeat(dfm_paired)} features"))
message(glue("  Interim: {sum(meta_p$is_interim)} | Full: {sum(!meta_p$is_interim)}"))

K_p        <- 8   # smaller k for smaller corpus
MOD_P_FILE <- file.path(DTMDIR, "stm_paired_k8.rds")

if (file.exists(MOD_P_FILE)) {
  message("Loading cached paired FM model ...")
  stm_paired <- readRDS(MOD_P_FILE)
} else {
  message(glue("\n=== FITTING STM (k={K_p}) ON PAIRED FM SPEECHES ===\n"))
  stm_paired <- stm(
    dfm_stm_p$documents, dfm_stm_p$vocab, K = K_p,
    prevalence = ~ is_interim_int + fm_factor,
    data       = meta_p,
    seed       = 42, verbose = TRUE
  )
  saveRDS(stm_paired, MOD_P_FILE)
}

frex_p     <- labelTopics(stm_paired, n = 6)$frex
topic_lp   <- paste0("T", 1:K_p, ": ", map_chr(1:K_p, ~ frex_p[.x, 1]))

message("\nPaired FM topic labels:")
walk(1:K_p, ~ message(glue("  T{.x}: {paste(frex_p[.x, 1:5], collapse=', ')}")))

effects_p  <- estimateEffect(1:K_p ~ is_interim_int + fm_factor,
                               stm_paired, meta_p, uncertainty = "Global")
eff_sum_p  <- summary(effects_p)

extract_interim_effect <- function(k) {
  tab <- eff_sum_p$tables[[k]]
  row <- which(rownames(tab) == "is_interim_int")
  if (length(row) == 0) return(NULL)
  tibble(
    topic       = k,
    topic_label = topic_lp[k],
    estimate    = tab[row, "Estimate"],
    se          = tab[row, "Std. Error"],
    pval        = tab[row, "Pr(>|t|)"],
    ci_lo       = tab[row, "Estimate"] - 1.96 * tab[row, "Std. Error"],
    ci_hi       = tab[row, "Estimate"] + 1.96 * tab[row, "Std. Error"]
  )
}

effects_interim <- map_dfr(1:K_p, extract_interim_effect) %>%
  mutate(
    sig       = case_when(pval < 0.01 ~ "**", pval < 0.05 ~ "*", TRUE ~ ""),
    direction = if_else(estimate > 0, "More in interim", "More in full")
  )

message("\nInterim budget effect on topics:")
print(effects_interim %>% select(topic_label, estimate, pval, sig))

p_interim_stm <- ggplot(effects_interim,
    aes(x = reorder(topic_label, estimate), y = estimate, colour = direction)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.25, linewidth = 0.6) +
  geom_point(size = 3) +
  geom_text(aes(label = sig, y = ci_hi + 0.006),
            size = 4, fontface = "bold", show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(values = c("More in interim" = "#D45F00",
                                  "More in full"    = "#1D6FA4"), name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Interim vs full budgets: topic prevalence differences",
    subtitle = "Controlling for Finance Minister identity (within-FM comparison)\n* p<0.05  ** p<0.01  |  95% CI",
    x = NULL, y = "Difference in topic proportion (interim minus full)"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())

ggsave(file.path(FIGDIR, "fig_interim_stm.png"), p_interim_stm,
       width = 9, height = 6, dpi = 150)
message("Saved: fig_interim_stm.png")
#}

# -- SUMMARY ------------------------------------------------------------------
#{
n_sig_interim <- sum(effects_interim$pval < 0.05, na.rm = TRUE)
message(glue("\n=== D1 COMPLETE ==="))
message(glue("FMs with paired speeches: {length(paired_fms)}"))
message(glue("  {paste(paired_fms, collapse=', ')}"))
message(glue("Significant interim-vs-full topics (p<0.05): {n_sig_interim} of {K_p}"))
if (n_sig_interim > 0) {
  effects_interim %>% filter(pval < 0.05) %>%
    select(topic_label, estimate, pval, sig, direction) %>% print()
}
#}
