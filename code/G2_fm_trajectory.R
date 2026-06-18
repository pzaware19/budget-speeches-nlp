# =============================================================================
# G2_fm_trajectory.R
# Author: Piyush Zaware
# Last updated: 2026-06-17
#
# Goal: Does a Finance Minister's ideological register drift over their
#       tenure? Do they become more market-liberal or nationalist as they
#       find their footing? Does the final budget before an election
#       systematically differ from their other budgets?
#
# Method:
#   1. For each FM with 3+ full budgets, fit a within-FM linear trend:
#      ideology ~ tenure_year (1, 2, 3...) using the existing TF-IDF ideology
#      scores from ideology_scores.csv.
#   2. Classify FMs by drift direction and magnitude.
#   3. Pre-election budget test: compare each FM's last budget before an
#      election (identified as the budget immediately before an interim) against
#      their other full budgets.
#
# IN
#   output/dtm/ideology_scores.csv
#
# OUT
#   output/tables/tab_fm_trajectory.csv    -- slope + trend per FM
#   output/tables/tab_prelection.csv       -- pre-election budget ideology
#   output/figures/fig_fm_trajectory.png   -- per-FM ideology lines
#   output/figures/fig_fm_slope.png        -- slope comparison
#   output/figures/fig_prelection.png      -- pre-election vs other budgets
# =============================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr)
  library(stringr); library(ggplot2); library(ggrepel)
  library(purrr); library(glue)
})

root   <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
DTMDIR <- file.path(root, "output", "dtm")
FIGDIR <- file.path(root, "output", "figures")
TABDIR <- file.path(root, "output", "tables")

# =============================================================================
# LOAD AND PREPARE
# =============================================================================
#{
# Use budget_metadata.csv as authoritative budget_type source.
# ideology_scores has a malformed doc_id "bs" for Chidambaram's 2014-15
# INTERIM budget, incorrectly classified as "full". Joining on (fy_start,
# fm_name) against budget_metadata gets the correct type.
# Note: 2008-09 (Chidambaram's last UPA-I budget) is absent from the corpus.
bm_auth <- read_csv(file.path(root, "input", "budget_metadata.csv"),
                     show_col_types = FALSE) %>%
  select(fy_start, fm_name, auth_type = budget_type)

scores <- read_csv(file.path(DTMDIR, "ideology_scores.csv"),
                    show_col_types = FALSE) %>%
  filter(!is.na(fm_name), !is.na(fy_start)) %>%
  left_join(bm_auth, by = c("fy_start", "fm_name")) %>%
  mutate(budget_type = coalesce(auth_type, budget_type)) %>%
  filter(budget_type == "full") %>%
  arrange(fm_name, fy_start)

# Add tenure year (1 = first full budget)
# For FMs with non-consecutive stints (Chidambaram), we treat each stint
# as a continuous sequence by fy_start order
scores <- scores %>%
  group_by(fm_name) %>%
  mutate(tenure_year = rank(fy_start, ties.method = "first")) %>%
  ungroup()

# FM summary
fm_counts <- scores %>% count(fm_name, fm_party_family, name = "n_budgets") %>%
  arrange(desc(n_budgets))
message("FM full budget counts:")
print(fm_counts, n = 20)

# Keep only FMs with 3+ full budgets
fm_3plus <- fm_counts %>% filter(n_budgets >= 3) %>% pull(fm_name)
message(glue("\nFMs with 3+ full budgets: {length(fm_3plus)}"))

scores_3plus <- scores %>% filter(fm_name %in% fm_3plus)
#}

# =============================================================================
# FIT WITHIN-FM TRENDS
# =============================================================================
#{
fit_fm_trend <- function(df) {
  if (nrow(df) < 3) return(tibble())

  fit_axis <- function(y) {
    mod   <- lm(y ~ tenure_year, data = df)
    coefs <- coef(mod)
    pval  <- summary(mod)$coefficients["tenure_year", "Pr(>|t|)"]
    tibble(
      slope     = coefs["tenure_year"],
      intercept = coefs["(Intercept)"],
      r_squared = summary(mod)$r.squared,
      p_value   = pval,
      n         = nrow(df)
    )
  }

  bind_rows(
    fit_axis(df$axis_market)      %>% mutate(axis = "Market-liberal"),
    fit_axis(df$axis_nationalist) %>% mutate(axis = "Nationalist")
  )
}

fm_trends <- scores_3plus %>%
  group_by(fm_name, fm_party_family) %>%
  group_modify(~ fit_fm_trend(.x)) %>%
  ungroup() %>%
  mutate(
    sig      = p_value < 0.1,
    direction = case_when(
      slope > 0 & sig   ~ "Rising (p<0.1)",
      slope < 0 & sig   ~ "Falling (p<0.1)",
      slope > 0         ~ "Rising (n.s.)",
      TRUE              ~ "Falling (n.s.)"
    )
  )

write_csv(fm_trends, file.path(TABDIR, "tab_fm_trajectory.csv"))
message("\nFM trajectory summary:")
fm_trends %>%
  mutate(across(c(slope, r_squared, p_value), ~ round(.x, 4))) %>%
  arrange(axis, desc(abs(slope))) %>%
  print(n = 30)
#}

# =============================================================================
# PRE-ELECTION BUDGET TEST
# =============================================================================
# A "pre-election" budget is the last full budget before an interim budget
# by the same FM (the interim signals an election is coming)
#{
all_scores <- read_csv(file.path(DTMDIR, "ideology_scores.csv"),
                        show_col_types = FALSE) %>%
  filter(!is.na(fm_name), !is.na(fy_start)) %>%
  arrange(fm_name, fy_start)

# Find the fy_start of each FM's interim budget(s)
interim_years <- all_scores %>%
  filter(budget_type == "interim") %>%
  select(fm_name, interim_fy = fy_start)

# For each interim, find the last full budget by the same FM BEFORE it
pre_election <- all_scores %>%
  filter(budget_type == "full") %>%
  inner_join(interim_years, by = "fm_name", relationship = "many-to-many") %>%
  filter(fy_start < interim_fy) %>%
  group_by(fm_name, interim_fy) %>%
  slice_max(fy_start, n = 1, with_ties = FALSE) %>%   # the full budget immediately before
  ungroup() %>%
  mutate(pre_election = TRUE)

# Label regular full budgets
scored_full <- all_scores %>%
  filter(budget_type == "full") %>%
  left_join(pre_election %>% select(doc_id, pre_election), by = "doc_id") %>%
  mutate(pre_election = replace_na(pre_election, FALSE)) %>%
  filter(fm_name %in% fm_3plus)

tab_prelection <- scored_full %>%
  group_by(pre_election) %>%
  summarise(
    n              = n(),
    mean_market    = round(mean(axis_market, na.rm = TRUE), 5),
    sd_market      = round(sd(axis_market, na.rm = TRUE), 5),
    mean_nationalist = round(mean(axis_nationalist, na.rm = TRUE), 5),
    sd_nationalist   = round(sd(axis_nationalist, na.rm = TRUE), 5),
    .groups = "drop"
  )

write_csv(tab_prelection, file.path(TABDIR, "tab_prelection.csv"))
message("\nPre-election vs other full budgets (FMs with 3+ budgets):")
print(tab_prelection)

# Simple t-test
pre_e  <- scored_full %>% filter( pre_election) %>% pull(axis_market)
other  <- scored_full %>% filter(!pre_election) %>% pull(axis_market)
if (length(pre_e) >= 2 && length(other) >= 2) {
  tt <- t.test(pre_e, other)
  message(glue("\nt-test market axis: pre-election mean={round(mean(pre_e),5)}, other mean={round(mean(other),5)}, p={round(tt$p.value, 3)}"))
}

pre_e_n  <- scored_full %>% filter( pre_election) %>% pull(axis_nationalist)
other_n  <- scored_full %>% filter(!pre_election) %>% pull(axis_nationalist)
if (length(pre_e_n) >= 2 && length(other_n) >= 2) {
  tt_n <- t.test(pre_e_n, other_n)
  message(glue("t-test nationalist axis: pre-election mean={round(mean(pre_e_n),5)}, other mean={round(mean(other_n),5)}, p={round(tt_n$p.value, 3)}"))
}
#}

# =============================================================================
# FIGURES
# =============================================================================
#{

# -- Colour palette: party -----------------------------------------------
party_colours <- c(
  "INC"   = "#1a6bb5",   # blue
  "BJP"   = "#e07820",   # saffron
  "Other" = "#7a7a7a"
)

# -- Figure 1: Trajectory lines per FM, two panels (market / nationalist) ----
plot_data <- scores_3plus %>%
  select(fm_name, fm_party_family, fy_start, tenure_year,
         axis_market, axis_nationalist) %>%
  pivot_longer(c(axis_market, axis_nationalist),
               names_to  = "axis",
               values_to = "score") %>%
  mutate(axis = recode(axis,
    "axis_market"      = "Market-liberal axis",
    "axis_nationalist" = "Nationalist axis"
  ))

# Trend lines from fm_trends
trend_lines <- fm_trends %>%
  inner_join(scores_3plus %>%
               group_by(fm_name) %>%
               summarise(min_t = min(tenure_year),
                         max_t = max(tenure_year),
                         .groups = "drop"),
             by = "fm_name") %>%
  mutate(axis = recode(axis,
    "Market-liberal" = "Market-liberal axis",
    "Nationalist"    = "Nationalist axis"
  )) %>%
  rowwise() %>%
  mutate(
    x1 = min_t, x2 = max_t,
    y1 = intercept + slope * min_t,
    y2 = intercept + slope * max_t
  ) %>%
  ungroup()

p_traj <- ggplot(plot_data,
    aes(x = tenure_year, y = score,
        colour = fm_party_family, group = fm_name)) +
  geom_hline(yintercept = 0, colour = "grey70", linetype = "dashed") +
  geom_line(linewidth = 0.5, alpha = 0.5) +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_segment(data = trend_lines %>% filter(sig),
               aes(x = x1, xend = x2, y = y1, yend = y2,
                   colour = fm_party_family),
               linewidth = 1.4, inherit.aes = FALSE) +
  geom_segment(data = trend_lines %>% filter(!sig),
               aes(x = x1, xend = x2, y = y1, yend = y2,
                   colour = fm_party_family),
               linewidth = 0.7, linetype = "dashed", inherit.aes = FALSE) +
  facet_wrap(~ axis, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = party_colours, name = "Party") +
  scale_x_continuous(breaks = 1:9, name = "Budget within tenure") +
  labs(
    title    = "Finance Minister ideology over tenure",
    subtitle = "Each point = one full budget. Thick line = significant trend (p<0.1); dashed = n.s."
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "#f5f1ea"),
        strip.text = element_text(face = "bold"))

ggsave(file.path(FIGDIR, "fig_fm_trajectory.png"), p_traj,
       width = 9, height = 8, dpi = 150)
message("Saved: fig_fm_trajectory.png")

# -- Figure 2: Slope dot plot — which FMs drift most? ----------------------
slope_plot <- fm_trends %>%
  left_join(fm_counts, by = c("fm_name", "fm_party_family")) %>%
  mutate(
    fm_label = glue("{fm_name} (n={n_budgets})")
  )

p_slope <- ggplot(slope_plot,
    aes(x = slope, y = reorder(fm_label, slope),
        colour = fm_party_family, shape = sig)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = "dashed") +
  geom_point(size = 3.5) +
  scale_colour_manual(values = party_colours, name = "Party") +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                     name = NULL,
                     labels = c("TRUE" = "p<0.1", "FALSE" = "n.s.")) +
  facet_wrap(~ axis, ncol = 2, scales = "free_x") +
  labs(
    title    = "Ideological drift over tenure (slope per budget year)",
    subtitle = "Positive = more market-liberal / more nationalist over time.\nFilled dot = p<0.1; open dot = n.s.",
    x = "Slope (ideology score per additional budget year)", y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "#f5f1ea"),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 8))

ggsave(file.path(FIGDIR, "fig_fm_slope.png"), p_slope,
       width = 11, height = 7, dpi = 150)
message("Saved: fig_fm_slope.png")

# -- Figure 3: Pre-election budget comparison --------------------------------
pre_e_df <- scored_full %>%
  filter(fm_name %in% fm_3plus) %>%
  mutate(
    budget_type = if_else(pre_election, "Pre-election budget", "Other full budget")
  ) %>%
  select(fm_name, fm_party_family, fy_start, budget_type,
         axis_market, axis_nationalist) %>%
  pivot_longer(c(axis_market, axis_nationalist),
               names_to = "axis", values_to = "score") %>%
  mutate(axis = recode(axis,
    "axis_market"      = "Market-liberal axis",
    "axis_nationalist" = "Nationalist axis"
  ))

p_prelect <- ggplot(pre_e_df,
    aes(x = budget_type, y = score, colour = fm_party_family)) +
  geom_jitter(width = 0.15, alpha = 0.7, size = 2.2) +
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.4, linewidth = 0.7, colour = "#333") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  facet_wrap(~ axis, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = party_colours, name = "Party") +
  labs(
    title    = "Pre-election budgets vs other full budgets",
    subtitle = "Bar = group mean. Pre-election = last full budget before an interim.\nOnly FMs with 3+ full budgets.",
    x = NULL, y = "Ideology score"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "#f5f1ea"),
        strip.text = element_text(face = "bold"))

ggsave(file.path(FIGDIR, "fig_prelection.png"), p_prelect,
       width = 9, height = 6, dpi = 150)
message("Saved: fig_prelection.png")
#}

message("\nG2 complete.")
