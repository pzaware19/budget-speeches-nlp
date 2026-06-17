# =============================================================================
# B2_stm_covariates.R
# Author: Piyush Zaware
# Last updated: 2026-06-16
#
# Goal: Fit STM with prevalence covariates (party + time spline) and estimate
#       formal BJP vs INC topic differences with uncertainty quantification.
#
# Model: STM k=10, prevalence = ~ party + s(fy_start, df=5)
#        INC is the baseline category for party.
#        estimateEffect() gives BJP-INC difference per topic with 95% CI.
#
# Difference from B1: B1 = pure LDA (no covariates), for topic discovery.
#                     B2 = STM with covariates, for causal inference on party.
#
# IN
#   output/dtm/budget_dfm.rds       -- quanteda DFM (same as B1)
#   output/dtm/speech_meta.csv      -- metadata (some party NAs patched below)
#
# OUT
#   output/dtm/stm_cov_k10.rds             -- fitted STM with covariates
#   output/figures/fig_bjp_inc_forest.png  -- forest plot BJP-INC per topic
#   output/figures/fig_topic_trends_cov.png -- time trends (party-conditional)
#   output/tables/tab_stm_effects.csv      -- point estimates + CI per topic
#   output/tables/tab_stm_effects.tex      -- LaTeX table for paper
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
#}

# -- PARTY PATCH TABLE --------------------------------------------------------
# Speeches where metadata join left fm_party_family = NA.
# Assigned manually from historical record.
#{
party_patch <- tribble(
  ~doc_id,                   ~fm_name,             ~fm_party_family, ~fy_start,
  "bs199697_I_",             "Manmohan Singh",      "INC",            1996L,
  "bs196566_August_",        "T.T. Krishnamachari", "INC",            1965L,
  "bs198081_I_",             "H.M. Patel",          "Other",          1980L,
  "bs195253_I_",             "C.D. Deshmukh",       "INC",            1952L,
  "bs196768_I_",             "Morarji Desai",       "INC",            1967L,
  "bs195657_November_",      "T.T. Krishnamachari", "INC",            1956L,
  "bs195758_I_",             "T.T. Krishnamachari", "INC",            1957L,
  "bs197475_july_",          "Yashwantrao Chavan",  "INC",            1974L,
  "bs199192_I_",             "Yashwant Sinha",      "Other",          1991L,
  "bs196263_I_",             "Morarji Desai",       "INC",            1962L,
  "bs",                      "P. Chidambaram",      "INC",            2014L,
  "Budget_Speech_2025-26",   "Nirmala Sitharaman",  "BJP",            2025L,
  "Budget_Speech_2026-27",   "Nirmala Sitharaman",  "BJP",            2026L
)
#}

# -- LOAD AND PREPARE DATA ----------------------------------------------------
#{
dfm  <- readRDS(file.path(DTMDIR, "budget_dfm.rds"))
meta <- read_csv(file.path(DTMDIR, "speech_meta.csv"), show_col_types = FALSE) %>%
  distinct(doc_id, .keep_all = TRUE) %>%
  rows_patch(party_patch, by = "doc_id", unmatched = "ignore")

# Apply the same DFM trimming as B1
PARTB_TERMS <- c(
  "duty","duties","excise","customs","taxation","yield",
  "deduction","deductions","rebate","rebates","surcharge","cess",
  "levy","levied","levies","proposed","propose","proposes",
  "amendment","amendments","section","subsection","clause","notification"
)
short_ids  <- meta %>% filter(words_clean < 1000) %>% pull(doc_id)
dfm_model  <- dfm_remove(dfm[!docnames(dfm) %in% short_ids, ], PARTB_TERMS)
dfm_model  <- dfm_trim(dfm_model, min_termfreq = 2)
dfm_stm    <- convert(dfm_model, to = "stm")
doc_ids    <- names(dfm_stm$documents)

# Align metadata: exact document order, INC as baseline party
meta_model <- meta %>%
  filter(doc_id %in% doc_ids) %>%
  arrange(match(doc_id, doc_ids)) %>%
  mutate(
    party   = factor(fm_party_family, levels = c("INC", "BJP", "Other")),
    fy_c    = as.numeric(scale(fy_start))
  )

n_na_party <- sum(is.na(meta_model$party))
message(glue("Documents: {nrow(meta_model)}  |  NA party remaining: {n_na_party}"))
message("Party breakdown:")
print(table(meta_model$party, useNA = "ifany"))

# Drop any remaining NA-party documents from the DFM and meta
if (n_na_party > 0) {
  na_ids    <- meta_model %>% filter(is.na(party)) %>% pull(doc_id)
  dfm_model <- dfm_model[!docnames(dfm_model) %in% na_ids, ]
  dfm_stm   <- convert(dfm_model, to = "stm")
  doc_ids   <- names(dfm_stm$documents)
  meta_model <- meta_model %>%
    filter(doc_id %in% doc_ids) %>%
    arrange(match(doc_id, doc_ids))
  message(glue("After dropping NA-party docs: {nrow(meta_model)} documents"))
}
#}

# -- FIT STM WITH PREVALENCE COVARIATES ---------------------------------------
#{
K_FINAL    <- 10
MODEL_FILE <- file.path(DTMDIR, "stm_cov_k10.rds")

if (file.exists(MODEL_FILE)) {
  message("Loading cached covariate STM model ...")
  stm_cov <- readRDS(MODEL_FILE)
} else {
  message(glue("\n=== FITTING STM (k={K_FINAL}) WITH COVARIATES ===\n"))
  stm_cov <- stm(
    dfm_stm$documents,
    dfm_stm$vocab,
    K          = K_FINAL,
    prevalence = ~ party + s(fy_start, df = 5),
    data       = meta_model,
    seed       = 42,
    verbose    = TRUE
  )
  saveRDS(stm_cov, MODEL_FILE)
  message(glue("Model saved: stm_cov_k10.rds"))
}

# Topic labels from FREX words (same as B1 for consistency)
frex_words    <- labelTopics(stm_cov, n = 8)$frex
top_frex_word <- map_chr(1:K_FINAL, ~ frex_words[.x, 1])
topic_labels  <- paste0("T", 1:K_FINAL, ": ", top_frex_word)
message("\nTopic labels (top FREX word):")
walk(1:K_FINAL, ~ message(glue("  T{.x}: {paste(frex_words[.x,1:5], collapse=', ')}")))
#}

# -- ESTIMATE EFFECTS ---------------------------------------------------------
#{
message("\n=== RUNNING estimateEffect() ===\n")
effects <- estimateEffect(
  formula     = 1:K_FINAL ~ party + s(fy_start, df = 5),
  stmobj      = stm_cov,
  metadata    = meta_model,
  uncertainty = "Global"
)

# summary() builds the coefficient tables; effects$tables does not exist directly
eff_sum <- summary(effects)
# eff_sum$tables is a named list: "Topic 1", "Topic 2", ...
# Each element is a matrix: rows = covariate levels, cols = Estimate / Std. Error / t value / Pr(>|t|)

extract_party_effect <- function(k) {
  # eff_sum$tables is indexed by integer, not by name
  tab  <- eff_sum$tables[[k]]
  if (is.null(tab)) return(NULL)
  rows <- rownames(tab)

  # BJP row is labelled "partyBJP" in the stm output
  bjp_row <- which(rows == "partyBJP")
  if (length(bjp_row) == 0) return(NULL)
  tibble(
    topic       = k,
    topic_label = topic_labels[k],
    estimate    = tab[bjp_row, "Estimate"],
    se          = tab[bjp_row, "Std. Error"],
    tstat       = tab[bjp_row, "t value"],
    pval        = tab[bjp_row, "Pr(>|t|)"],
    ci_lo       = tab[bjp_row, "Estimate"] - 1.96 * tab[bjp_row, "Std. Error"],
    ci_hi       = tab[bjp_row, "Estimate"] + 1.96 * tab[bjp_row, "Std. Error"]
  )
}

effects_df <- map_dfr(1:K_FINAL, extract_party_effect) %>%
  mutate(
    sig       = case_when(pval < 0.01 ~ "**", pval < 0.05 ~ "*", TRUE ~ ""),
    direction = if_else(estimate > 0, "BJP higher", "INC higher")
  )

write_csv(effects_df, file.path(TABDIR, "tab_stm_effects.csv"))
message("\nBJP vs INC effect estimates:")
print(effects_df %>% select(topic, topic_label, estimate, se, pval, sig))
#}

# -- FIGURE 1: FOREST PLOT (BJP - INC) ----------------------------------------
#{
p_forest <- ggplot(effects_df,
  aes(x = reorder(topic_label, estimate),
      y = estimate,
      colour = direction)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.25, linewidth = 0.6) +
  geom_point(size = 3) +
  geom_text(aes(label = sig, y = ci_hi + 0.005),
            size = 4, fontface = "bold", show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(values = c("BJP higher" = "#FF9933",
                                 "INC higher"  = "#19AAED"),
                      name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "BJP vs INC: Difference in topic prevalence",
    subtitle = "Estimate = BJP − INC (marginal effect, controlling for year)\n* p<0.05  ** p<0.01  |  95% CI shown",
    x = NULL,
    y = "Difference in topic proportion (BJP − INC)"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank())

ggsave(file.path(FIGDIR, "fig_bjp_inc_forest.png"), p_forest,
       width = 9, height = 6, dpi = 150)
message("Figure saved: fig_bjp_inc_forest.png")
#}

# -- FIGURE 2: TIME TRENDS PER TOPIC -----------------------------------------
# Expected topic proportion over year, separately for BJP and INC,
# from the STM prevalence model
#{
# Use the theta matrix from the covariate model (cleaner than B1 theta)
theta           <- as.data.frame(stm_cov$theta)
colnames(theta) <- paste0("topic_", 1:K_FINAL)
theta$doc_id    <- meta_model$doc_id

theta_long <- theta %>%
  left_join(meta_model %>% select(doc_id, fy_start, budget_type,
                                   fm_party_family, party),
            by = "doc_id") %>%
  pivot_longer(starts_with("topic_"), names_to = "topic", values_to = "prevalence") %>%
  mutate(
    topic_num   = as.integer(str_remove(topic, "topic_")),
    topic_label = topic_labels[topic_num]
  )

# Smooth average per party per year (loess)
theta_full <- theta_long %>%
  filter(budget_type == "full", fm_party_family %in% c("INC", "BJP"))

p_trends <- ggplot(theta_full,
    aes(x = fy_start, y = prevalence,
        colour = fm_party_family, fill = fm_party_family)) +
  geom_smooth(method = "loess", span = 0.5, se = TRUE, linewidth = 0.8, alpha = 0.15) +
  geom_point(size = 0.8, alpha = 0.5) +
  facet_wrap(~ topic_label, ncol = 5, scales = "free_y") +
  scale_colour_manual(values = c("BJP" = "#FF9933", "INC" = "#19AAED"), name = NULL) +
  scale_fill_manual(values  = c("BJP" = "#FF9933", "INC" = "#19AAED"), name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(1950, 1975, 2000)) +
  labs(
    title    = glue("Topic prevalence over time: BJP vs INC (k={K_FINAL})"),
    subtitle = "Each point is one budget speech; loess smooth ± 95% CI",
    x = "Year", y = "Topic proportion"
  ) +
  theme_bw(base_size = 9) +
  theme(strip.background = element_rect(fill = "grey92"),
        legend.position  = "top",
        axis.text.x      = element_text(size = 7))

ggsave(file.path(FIGDIR, "fig_topic_trends_cov.png"), p_trends,
       width = 16, height = 8, dpi = 150)
message("Figure saved: fig_topic_trends_cov.png")
#}

# -- LATEX TABLE: BJP vs INC EFFECTS ------------------------------------------
#{
# Format effect estimates as LaTeX table for the paper
effects_tex <- effects_df %>%
  mutate(
    est_fmt = sprintf("%.3f", estimate),
    se_fmt  = sprintf("(%.3f)", se),
    ci_fmt  = sprintf("[%.3f, %.3f]", ci_lo, ci_hi),
    pval_fmt = case_when(
      pval < 0.01 ~ "$<$0.01",
      pval < 0.05 ~ sprintf("%.2f", pval),
      TRUE        ~ sprintf("%.2f", pval)
    ),
    sig_fmt = sig
  )

tex_rows <- effects_tex %>%
  mutate(row = glue("{topic_label} & {est_fmt}{sig_fmt} & {se_fmt} & {pval_fmt} \\\\")) %>%
  pull(row)

tex_table <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{STM prevalence estimates: BJP vs INC Finance Ministers (INC baseline)}",
  "\\label{tab:stm_effects}",
  "\\begin{tabular}{lrrr}",
  "\\hline\\hline",
  "Topic & Estimate & (SE) & $p$-value \\\\",
  "\\hline",
  tex_rows,
  "\\hline",
  "\\multicolumn{4}{l}{\\footnotesize{Dependent variable: topic proportion (theta). Covariates: party + s(year, df=5).}} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize{INC is baseline. Estimate = BJP $-$ INC marginal effect. $^{*}p<0.05$, $^{**}p<0.01$.}} \\\\",
  "\\hline\\hline",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(tex_table, file.path(TABDIR, "tab_stm_effects.tex"))
message("LaTeX table saved: tab_stm_effects.tex")
#}

# -- SUMMARY ------------------------------------------------------------------
#{
n_sig   <- sum(effects_df$pval < 0.05)
bjp_hi  <- effects_df %>% filter(pval < 0.05, estimate > 0) %>% pull(topic_label)
inc_hi  <- effects_df %>% filter(pval < 0.05, estimate < 0) %>% pull(topic_label)

message(glue("
=== B2 COMPLETE ===
Topics with significant BJP-INC difference (p<0.05): {n_sig} of {K_FINAL}
  BJP higher: {if(length(bjp_hi)>0) paste(bjp_hi, collapse='; ') else 'none'}
  INC higher: {if(length(inc_hi)>0) paste(inc_hi, collapse='; ') else 'none'}

Outputs:
  output/dtm/stm_cov_k10.rds
  output/tables/tab_stm_effects.csv
  output/tables/tab_stm_effects.tex
  output/figures/fig_bjp_inc_forest.png
  output/figures/fig_topic_trends_cov.png
"))
#}
