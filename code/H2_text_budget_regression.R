# =============================================================================
# H2_text_budget_regression.R
# Author: Piyush Zaware
# Last updated: 2026-06-17
#
# Goal: Test whether budget speech vocabulary predicts actual fiscal outcomes.
#
# Three regression specifications per (theme, indicator) pair:
#   (1) Contemporaneous: fiscal_t = α + β·vocab_t + ε
#       — does the rhetoric mirror the allocation in the same year?
#   (2) Text-leads: fiscal_t = α + β·vocab_{t-1} + γ·fiscal_{t-1} + ε
#       — does last year's speech predict this year's fiscal outcome,
#         beyond what last year's fiscal outcome already predicts?
#   (3) Money-leads: vocab_t = α + β·fiscal_{t-1} + γ·vocab_{t-1} + ε
#       — does last year's spending shape this year's rhetoric?
#
# Note on timing: Budget speech presented February t covers FY t-t+1.
#   WDI data is calendar year. We match fy_start = t to WDI year = t
#   as an approximation. The "text-leads" regression therefore asks:
#   does the Feb-t speech predict the calendar-year-t fiscal outcome
#   (which is mostly the FY t-t+1 outturn)?
#
# Theme-indicator pairs:
#   Defence          → mil_pct_gdp
#   Welfare_Social   → edu_pct_gdp, health_pct_gdp
#   Fiscal_Deficit   → fiscal_deficit_gdp
#   Infrastructure   → gfcf_pct_gdp
#   Green_Climate    → renew_pct_elec
#   FDI_Openness     → fdi_pct_gdp
#   Manufacturing_PLI→ gfcf_pct_gdp (proxy; no dedicated indicator)
#
# Also tests ideology scores (axis_market, axis_nationalist) against all indicators.
#
# IN
#   output/tables/tab_all_vocab_themes.csv
#   output/tables/tab_fiscal_data.csv
#   output/dtm/ideology_scores.csv
#
# OUT
#   output/tables/tab_text_fiscal_regs.csv   -- all regression results
#   output/figures/fig_vocab_fiscal_scatter.png  -- scatter grid
#   output/figures/fig_text_leads_forest.png     -- forest plot (text-leads β)
#   output/figures/fig_vocab_fiscal_ts.png       -- time series overlays
# =============================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(purrr)
  library(stringr); library(ggplot2); library(ggrepel); library(glue)
  library(broom); library(scales)
})

root   <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
TABDIR <- file.path(root, "output", "tables")
FIGDIR <- file.path(root, "output", "figures")
DTMDIR <- file.path(root, "output", "dtm")

# =============================================================================
# LOAD AND MERGE
# =============================================================================
#{

# Vocabulary theme scores (all speeches)
themes_long <- read_csv(file.path(TABDIR, "tab_all_vocab_themes.csv"),
                         show_col_types = FALSE) %>%
  filter(budget_type == "full") %>%   # full budgets only
  select(fy_start, theme, share) %>%
  pivot_wider(names_from = theme, values_from = share,
              values_fn = mean)  # average if 2 speeches same year (1977-78 I & II)

# Ideology scores
ideo <- read_csv(file.path(DTMDIR, "ideology_scores.csv"),
                  show_col_types = FALSE) %>%
  filter(budget_type == "full") %>%
  group_by(fy_start) %>%
  summarise(axis_market      = mean(axis_market,      na.rm = TRUE),
            axis_nationalist = mean(axis_nationalist, na.rm = TRUE),
            .groups = "drop")

# Fiscal data (WDI)
fiscal <- read_csv(file.path(TABDIR, "tab_fiscal_data.csv"),
                    show_col_types = FALSE) %>%
  select(year, mil_pct_gdp, edu_pct_gdp, health_pct_gdp,
         gfcf_pct_gdp, fiscal_deficit_gdp, renew_pct_elec, fdi_pct_gdp)

# Merge: match fy_start to WDI year
panel <- themes_long %>%
  left_join(ideo, by = "fy_start") %>%
  left_join(fiscal, by = c("fy_start" = "year")) %>%
  arrange(fy_start)

message(glue("Panel: {nrow(panel)} full budget years, {ncol(panel)} columns"))
message(glue("Year range: {min(panel$fy_start)}-{max(panel$fy_start)}"))
#}

# =============================================================================
# DEFINE THEME–INDICATOR PAIRS
# =============================================================================
#{

pairs <- tribble(
  ~vocab,             ~fiscal,                ~label_vocab,           ~label_fiscal,
  "Defence",          "mil_pct_gdp",          "Defence vocabulary",   "Military spending % GDP",
  "Welfare_Social",   "edu_pct_gdp",          "Welfare vocabulary",   "Education spending % GDP",
  "Welfare_Social",   "health_pct_gdp",       "Welfare vocabulary",   "Health spending % GDP",
  "Fiscal_Deficit",   "fiscal_deficit_gdp",   "Fiscal/Deficit vocabulary","Fiscal deficit % GDP",
  "Infrastructure",   "gfcf_pct_gdp",         "Infrastructure vocabulary","GFCF % GDP",
  "Green_Climate",    "renew_pct_elec",        "Green/Climate vocabulary","Renewable elec. % total",
  "FDI_Openness",     "fdi_pct_gdp",          "FDI/Openness vocabulary","FDI inflows % GDP",
  "Manufacturing_PLI","gfcf_pct_gdp",         "Manufacturing vocabulary","GFCF % GDP",
  "axis_market",      "fdi_pct_gdp",          "Market-liberal axis",  "FDI inflows % GDP",
  "axis_nationalist", "mil_pct_gdp",          "Nationalist axis",     "Military spending % GDP"
)

#}

# =============================================================================
# RUN REGRESSIONS
# =============================================================================
#{

run_pair <- function(vocab_col, fiscal_col, df) {
  df_pair <- df %>%
    select(fy_start,
           v = all_of(vocab_col),
           f = all_of(fiscal_col)) %>%
    filter(!is.na(v), !is.na(f)) %>%
    arrange(fy_start) %>%
    mutate(v_lag = lag(v),
           f_lag = lag(f))

  n_obs <- nrow(df_pair)
  if (n_obs < 8) {
    return(tibble(spec = character(), n = integer(),
                  beta = numeric(), se = numeric(),
                  p_value = numeric(), r_squared = numeric()))
  }

  fit_spec <- function(formula_str, data) {
    fit <- tryCatch(
      lm(as.formula(formula_str), data = data),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)
    tidy_fit <- tidy(fit)
    glance_fit <- glance(fit)
    list(tidy = tidy_fit, glance = glance_fit, n = nrow(data))
  }

  results <- list()

  # Spec 1: contemporaneous
  s1 <- fit_spec("f ~ v", df_pair)
  if (!is.null(s1)) {
    r1 <- s1$tidy %>% filter(term == "v")
    results[["contemporaneous"]] <- tibble(
      spec = "contemporaneous", n = s1$n,
      beta = r1$estimate, se = r1$std.error,
      p_value = r1$p.value, r_squared = s1$glance$r.squared
    )
  }

  # Spec 2: text leads fiscal (vocab_{t-1} -> fiscal_t | fiscal_{t-1})
  df_lead <- df_pair %>% filter(!is.na(v_lag), !is.na(f_lag))
  if (nrow(df_lead) >= 8) {
    s2 <- fit_spec("f ~ v_lag + f_lag", df_lead)
    if (!is.null(s2)) {
      r2 <- s2$tidy %>% filter(term == "v_lag")
      results[["text_leads"]] <- tibble(
        spec = "text_leads", n = s2$n,
        beta = r2$estimate, se = r2$std.error,
        p_value = r2$p.value, r_squared = s2$glance$r.squared
      )
    }
  }

  # Spec 3: money leads text (fiscal_{t-1} -> vocab_t | vocab_{t-1})
  if (nrow(df_lead) >= 8) {
    s3 <- fit_spec("v ~ f_lag + v_lag", df_lead)
    if (!is.null(s3)) {
      r3 <- s3$tidy %>% filter(term == "f_lag")
      results[["money_leads"]] <- tibble(
        spec = "money_leads", n = s3$n,
        beta = r3$estimate, se = r3$std.error,
        p_value = r3$p.value, r_squared = s3$glance$r.squared
      )
    }
  }

  # Spec 4: detrended contemporaneous (partial out common year trend from both)
  df_dt <- df_pair %>% filter(!is.na(v), !is.na(f))
  if (nrow(df_dt) >= 10) {
    v_resid <- residuals(lm(v ~ fy_start, data = df_dt))
    f_resid <- residuals(lm(f ~ fy_start, data = df_dt))
    dt_df   <- tibble(v_r = v_resid, f_r = f_resid)
    s4 <- fit_spec("f_r ~ v_r", dt_df)
    if (!is.null(s4)) {
      r4 <- s4$tidy %>% filter(term == "v_r")
      results[["detrended"]] <- tibble(
        spec = "detrended", n = s4$n,
        beta = r4$estimate, se = r4$std.error,
        p_value = r4$p.value, r_squared = s4$glance$r.squared
      )
    }
  }

  bind_rows(results)
}

reg_results <- pairs %>%
  rowwise() %>%
  mutate(results = list(run_pair(vocab, fiscal, panel))) %>%
  unnest(results) %>%
  ungroup() %>%
  mutate(
    sig      = p_value < 0.1,
    sig_05   = p_value < 0.05,
    pair_label = glue("{label_vocab}\n→ {label_fiscal}")
  )

write_csv(reg_results, file.path(TABDIR, "tab_text_fiscal_regs.csv"))
message("\nRegression results:")
reg_results %>%
  filter(spec == "text_leads") %>%
  mutate(across(c(beta, se, p_value, r_squared), ~ round(.x, 4))) %>%
  select(label_vocab, label_fiscal, n, beta, se, p_value, r_squared, sig) %>%
  print(n = 20)
#}

# =============================================================================
# FIGURE 1: SCATTER GRID — contemporaneous vocab vs fiscal
# =============================================================================
#{

# Build scatter data for the cleanest pairs
scatter_data <- pmap_dfr(pairs[1:8, ], function(vocab, fiscal,
                                                  label_vocab, label_fiscal) {
  df_pair <- panel %>%
    select(fy_start, v = all_of(vocab), f = all_of(fiscal)) %>%
    filter(!is.na(v), !is.na(f))
  df_pair %>% mutate(pair = glue("{label_vocab}\n({label_fiscal})"))
})

decade_labels <- scatter_data %>%
  mutate(decade_start = (fy_start %/% 10) * 10) %>%
  group_by(pair, decade_start) %>%
  slice_min(abs(fy_start - (decade_start + 5)), n = 1) %>%
  ungroup()

p_scatter <- ggplot(scatter_data, aes(x = v, y = f)) +
  geom_smooth(method = "lm", se = TRUE, colour = "#2a5e2a",
              fill = "#c7e0c7", alpha = 0.35, linewidth = 0.9) +
  geom_point(aes(colour = fy_start), size = 2.2, alpha = 0.8) +
  geom_text_repel(data = decade_labels,
                  aes(label = fy_start), size = 2.4,
                  colour = "#444", max.overlaps = 6) +
  scale_colour_viridis_c(option = "D", name = "Year",
                         guide = guide_colourbar(barwidth = 6, barheight = 0.5)) +
  facet_wrap(~ pair, ncol = 2, scales = "free") +
  labs(
    title    = "Budget speech vocabulary vs actual fiscal outcomes",
    subtitle = "Each point = one full budget year. OLS line with 95% CI.",
    x = "Vocabulary theme (mentions per 1,000 words)",
    y = "Fiscal outcome"
  ) +
  theme_bw(base_size = 9) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "#f5f1ea"),
    strip.text       = element_text(face = "bold", size = 7),
    axis.text        = element_text(size = 7)
  )

ggsave(file.path(FIGDIR, "fig_vocab_fiscal_scatter.png"), p_scatter,
       width = 12, height = 14, dpi = 150)
message("Saved: fig_vocab_fiscal_scatter.png")
#}

# =============================================================================
# FIGURE 2: FOREST PLOT — text-leads regression coefficients
# =============================================================================
#{

forest_data <- reg_results %>%
  filter(spec == "text_leads") %>%
  mutate(
    ci_lo = beta - 1.64 * se,   # 90% CI
    ci_hi = beta + 1.64 * se,
    pair_label = glue("{label_vocab} → {label_fiscal}"),
    # Standardise: report in units of 1 SD of vocab → response
    direction = if_else(beta > 0, "Positive", "Negative")
  )

p_forest <- ggplot(forest_data,
    aes(x = beta, y = reorder(pair_label, beta),
        colour = sig_05, shape = sig_05)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = "dashed") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.25, linewidth = 0.8) +
  geom_point(size = 3.5) +
  scale_colour_manual(values = c("TRUE" = "#2a5e2a", "FALSE" = "#aaa"),
                      name = NULL,
                      labels = c("TRUE" = "p<0.05", "FALSE" = "n.s.")) +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                     guide  = "none") +
  geom_text(aes(label = glue("n={n}, p={round(p_value,3)}")),
            hjust = -0.1, size = 2.6, colour = "#555") +
  labs(
    title    = "Does last year's speech predict this year's fiscal outcomes?",
    subtitle = "Coefficient on vocab_{t-1} in: fiscal_t = α + β·vocab_{t-1} + γ·fiscal_{t-1}\n90% confidence intervals. Green filled = p<0.05.",
    x = "β coefficient (vocab_{t-1} → fiscal_t, controlling for fiscal_{t-1})",
    y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    axis.text.y     = element_text(size = 9)
  )

ggsave(file.path(FIGDIR, "fig_text_leads_forest.png"), p_forest,
       width = 11, height = 8, dpi = 150)
message("Saved: fig_text_leads_forest.png")
#}

# =============================================================================
# FIGURE 3: TIME SERIES OVERLAYS — top 3 pairs by |contemporaneous r_squared|
# =============================================================================
#{

top_pairs <- reg_results %>%
  filter(spec == "contemporaneous") %>%
  slice_max(r_squared, n = 3, with_ties = FALSE)

message("\nTop 3 contemporaneous pairs by R²:")
print(top_pairs %>% select(label_vocab, label_fiscal, r_squared, beta, p_value))

ts_plots <- map(seq_len(nrow(top_pairs)), function(i) {
  vv   <- top_pairs$vocab[i]
  ff   <- top_pairs$fiscal[i]
  lv   <- top_pairs$label_vocab[i]
  lf   <- top_pairs$label_fiscal[i]
  r2   <- round(top_pairs$r_squared[i], 2)
  pval <- round(top_pairs$p_value[i], 3)

  df_ts <- panel %>%
    select(fy_start, v = all_of(vv), f = all_of(ff)) %>%
    filter(!is.na(v), !is.na(f)) %>%
    # z-score both series for same-axis comparison
    mutate(v_z = scale(v)[,1],
           f_z = scale(f)[,1])

  ggplot(df_ts, aes(x = fy_start)) +
    geom_line(aes(y = v_z, colour = "Vocabulary"), linewidth = 1) +
    geom_line(aes(y = f_z, colour = "Fiscal outcome"), linewidth = 1,
              linetype = "dashed") +
    geom_hline(yintercept = 0, colour = "grey70") +
    scale_colour_manual(
      values = c("Vocabulary" = "#2a5e2a", "Fiscal outcome" = "#b5310e"),
      name   = NULL
    ) +
    labs(
      title    = glue("{lv} vs {lf}"),
      subtitle = glue("Both z-scored. Contemporaneous R²={r2}, p={pval}."),
      x = "Budget year", y = "Z-score"
    ) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom")
})

# Combine into one figure (3 panels stacked)
library(patchwork)
p_ts <- ts_plots[[1]] / ts_plots[[2]] / ts_plots[[3]] +
  plot_annotation(
    title    = "Budget speech vocabulary vs fiscal outcomes over time",
    subtitle = "Top 3 pairs by contemporaneous fit. Both series z-scored for same axis."
  )

ggsave(file.path(FIGDIR, "fig_vocab_fiscal_ts.png"), p_ts,
       width = 10, height = 13, dpi = 150)
message("Saved: fig_vocab_fiscal_ts.png")
#}

# =============================================================================
# PRINT SUMMARY
# =============================================================================
#{
message("\n=== SUMMARY ===")
message("\nContemporaneous (fiscal_t ~ vocab_t):")
reg_results %>%
  filter(spec == "contemporaneous") %>%
  mutate(across(c(beta, r_squared, p_value), ~ round(.x, 3))) %>%
  arrange(p_value) %>%
  select(label_vocab, label_fiscal, n, r_squared, p_value) %>%
  print(n = 15)

message("\nText leads fiscal (fiscal_t ~ vocab_{t-1} + fiscal_{t-1}):")
reg_results %>%
  filter(spec == "text_leads") %>%
  mutate(across(c(beta, r_squared, p_value), ~ round(.x, 3))) %>%
  arrange(p_value) %>%
  select(label_vocab, label_fiscal, n, beta, r_squared, p_value) %>%
  print(n = 15)

message("\nMoney leads text (vocab_t ~ fiscal_{t-1} + vocab_{t-1}):")
reg_results %>%
  filter(spec == "money_leads") %>%
  mutate(across(c(beta, r_squared, p_value), ~ round(.x, 3))) %>%
  arrange(p_value) %>%
  select(label_vocab, label_fiscal, n, beta, r_squared, p_value) %>%
  print(n = 15)
#}

message("\nH2 complete.")
