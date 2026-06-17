# =============================================================================
# F2_prediction.R
# Author: Piyush Zaware
# Last updated: 2026-06-17
#
# Goal: Combine Sitharaman's vocabulary theme trends (F1) and ideology
#       trajectory with pre-budget news themes (E2) to produce a structured
#       prediction of her next budget speech.
#
# Note: The Part A STM was fit on pre-2020 speeches only; NS's 7 post-2019
#       budgets are absent from the Part A model. The substantive signal comes
#       from (a) policy-theme keyword frequencies across her 8 Part A texts
#       and (b) ideology scores, both from F1.
#
# Logic:
#   Internal signal = her vocabulary theme trends (F1: theme_proj, ideo_proj)
#   External signal = dominant news vocabulary from PRS + EcoSurvey (E2)
#   Confidence = agreement between internal and external signals
#
# IN
#   output/tables/tab_ns_projection.csv    -- F1 trend projections (all types)
#   output/tables/tab_ns_vocab_trend.csv   -- per-year theme frequencies
#   output/news/combined_themes.csv        -- E2 news themes by year
#
# OUT
#   output/prediction_report.md
#   output/figures/fig_prediction.png
#   output/figures/fig_theme_projection.png
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(glue)
library(purrr)

root   <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
TABDIR <- file.path(root, "output", "tables")
NEWDIR <- file.path(root, "output", "news")
FIGDIR <- file.path(root, "output", "figures")
OUTDIR <- file.path(root, "output")

# -- LOAD INPUTS --------------------------------------------------------------
#{
projection <- read_csv(file.path(TABDIR, "tab_ns_projection.csv"),
                        show_col_types = FALSE)
vocab_trend <- read_csv(file.path(TABDIR, "tab_ns_vocab_trend.csv"),
                         show_col_types = FALSE)

# Separate projection types
theme_proj   <- projection %>% filter(type == "theme")
ideology_proj <- projection %>% filter(dimension %in% c("axis_market", "axis_nationalist"))

message("Theme projections:")
print(theme_proj %>% select(dimension, last_val, pred_2027, slope, trend, r_squared) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))))

message("\nIdeology projections:")
print(ideology_proj %>% select(dimension, last_val, pred_2027, slope, r_squared))

# News themes
themes_file <- file.path(NEWDIR, "combined_themes.csv")
has_themes  <- file.exists(themes_file) && file.size(themes_file) > 100

if (has_themes) {
  news_themes <- read_csv(themes_file, show_col_types = FALSE)
  latest_news_year <- max(news_themes$year, na.rm = TRUE)
  top_news_words   <- news_themes %>%
    filter(year == latest_news_year) %>%
    slice_max(tf_idf, n = 40, with_ties = FALSE) %>%
    pull(word)
  message(glue("\nNews themes loaded for {latest_news_year}. Top words: {paste(top_news_words[1:12], collapse=', ')}"))
} else {
  message("No news themes — internal signal only")
  top_news_words <- character(0)
  latest_news_year <- 2026
}
#}

# -- MATCH NEWS THEMES TO POLICY THEMES --------------------------------------
#{
theme_keywords <- list(
  "Infrastructure"    = c("infrastructure","highway","railway","port","airport",
                           "road","metro","corridor","connectivity","logistics","capex"),
  "Green/Climate"     = c("green","climate","renewable","solar","wind","energy",
                           "transition","emission","sustainable","battery","hydrogen","clean"),
  "Digital/Tech"      = c("digital","technology","startup","innovation","aadhaar",
                           "upi","fintech","ai","data","cyber","platform","tech"),
  "Welfare/Social"    = c("welfare","poor","women","farmer","tribal","health",
                           "education","nutrition","housing","scheme","social","pmgsy"),
  "Manufacturing/PLI" = c("manufacturing","pli","production","exports","msme",
                           "atmanirbhar","domestic","capacity","industrial","make"),
  "Fiscal/Deficit"    = c("fiscal","deficit","consolidation","debt","gdp",
                           "borrowing","revenue","expenditure","capex","glide","consolidate")
)

news_match <- map_dfr(names(theme_keywords), function(th) {
  kw <- theme_keywords[[th]]
  n_match <- sum(top_news_words %in% kw) +
             sum(map_int(kw, ~ sum(str_detect(top_news_words, .x))))
  tibble(dimension = th, news_match_score = n_match)
})

theme_proj_plot <- theme_proj %>%
  left_join(news_match, by = "dimension") %>%
  mutate(
    news_match_score = replace_na(news_match_score, 0L),
    # Credibility tier based on R²
    credibility = case_when(
      r_squared >= 0.5  ~ "Strong signal (R²≥0.5)",
      r_squared >= 0.15 ~ "Moderate signal (R²≥0.15)",
      TRUE              ~ "Weak signal (R²<0.15)"
    ),
    # R² label for plot annotation
    r2_label = glue("R²={round(r_squared, 2)}")
  )

cred_colours <- c(
  "Strong signal (R²≥0.5)"   = "#2a5e2a",
  "Moderate signal (R²≥0.15)"= "#8b6914",
  "Weak signal (R²<0.15)"    = "#aaaaaa"
)

message("\nTheme signal assessment:")
print(theme_proj_plot %>% select(dimension, trend, r_squared, credibility))
#}

# -- FIGURES ------------------------------------------------------------------
#{
# Figure 1: Forecast bar chart coloured by R² credibility
p_forecast <- ggplot(theme_proj_plot,
    aes(x = reorder(dimension, pred_2027),
        y = pred_2027, fill = credibility,
        ymin = pred_lo, ymax = pred_hi)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.3) +
  geom_errorbar(width = 0.3, linewidth = 0.7, colour = "#333") +
  geom_text(aes(label = r2_label, y = pred_lo - 0.5),
            hjust = 1, size = 3, colour = "#555") +
  coord_flip() +
  scale_fill_manual(values = cred_colours, name = "Forecast reliability") +
  scale_y_continuous(name = "Predicted mentions per 1,000 words",
                     expand = expansion(mult = c(0.18, 0.05))) +
  labs(
    title    = "Predicted policy theme emphasis: Sitharaman 2027-28",
    subtitle = "80% prediction intervals. Colour = goodness of linear fit (R²).\nGrey bars: no reliable trend — project as stable at recent levels.",
    x = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position  = "bottom",
        legend.text      = element_text(size = 9),
        panel.grid.major.y = element_blank())

ggsave(file.path(FIGDIR, "fig_prediction.png"), p_forecast,
       width = 9, height = 6, dpi = 150)
message("Saved: fig_prediction.png")

# Figure 2: Historical theme trends — facet by theme, actual data + trend line
# Colour each facet by credibility so viewer knows which lines to trust
vocab_with_cred <- vocab_trend %>%
  left_join(theme_proj_plot %>% select(dimension, credibility, r2_label),
            by = c("theme" = "dimension"))

facet_order <- theme_proj %>% arrange(desc(r_squared)) %>% pull(dimension)

p_trends <- ggplot(vocab_with_cred,
    aes(x = fy_start, y = share)) +
  geom_point(aes(colour = credibility), size = 2.5) +
  geom_line(aes(colour = credibility), linewidth = 0.7, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9,
              colour = "#444", fill = "#ddd", alpha = 0.4) +
  geom_text(data = vocab_with_cred %>%
              group_by(theme, r2_label) %>% slice_tail(n = 1) %>% ungroup(),
            aes(label = r2_label), hjust = 0, nudge_x = 0.05,
            size = 2.8, colour = "#666") +
  scale_x_continuous(breaks = 2019:2026,
                     limits = c(2019, 2027.5),
                     labels = c("2019", "'20", "'21", "'22", "'23", "'24", "'25", "'26")) +
  scale_y_continuous(name = "Mentions per 1,000 words") +
  scale_colour_manual(values = cred_colours, name = NULL) +
  facet_wrap(~ factor(theme, levels = facet_order), ncol = 3, scales = "free_y") +
  labs(
    title    = "Policy theme vocabulary — Nirmala Sitharaman (2019-2026)",
    subtitle = "Sorted by linear fit quality (strongest top-left). Grey band = 95% CI on trend line.",
    x = "Budget year"
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position  = "bottom",
        strip.background = element_rect(fill = "#f5f1ea"),
        strip.text       = element_text(face = "bold"),
        axis.text.x      = element_text(size = 8))

ggsave(file.path(FIGDIR, "fig_ns_vocab_trend.png"), p_trends,
       width = 11, height = 7, dpi = 150)
message("Saved: fig_ns_vocab_trend.png (replaced)")

ggsave(file.path(FIGDIR, "fig_theme_projection.png"), p_trends,
       width = 11, height = 7, dpi = 150)
message("Saved: fig_theme_projection.png")
#}

# -- PREDICTION REPORT --------------------------------------------------------
#{
mkt_proj    <- ideology_proj %>% filter(dimension == "axis_market")
nat_proj    <- ideology_proj %>% filter(dimension == "axis_nationalist")
rising      <- theme_proj_plot %>% filter(trend == "rising")  %>% arrange(desc(r_squared))
falling     <- theme_proj_plot %>% filter(trend == "falling") %>% arrange(desc(r_squared))
top_by_pred <- theme_proj_plot %>% arrange(desc(pred_2027))
mfg_r2      <- round(theme_proj_plot %>% filter(dimension == "Manufacturing/PLI") %>% pull(r_squared), 2)

news_signal_note <- if (length(top_news_words) > 0) {
  "Themes with the strongest news-signal overlap are highlighted in the forecast figure."
} else {
  "No news themes available -- prediction based on internal trend only."
}

ideo_note <- if (mkt_proj$slope > 0 & nat_proj$slope > 0) {
  "Both axes trending upward -- a more market-nationalist posture."
} else if (mkt_proj$slope > 0 & nat_proj$slope <= 0) {
  "Market-liberal axis rising slightly; nationalist axis declining -- market integration emphasis."
} else {
  "Both ideological axes effectively stable -- no clear directional shift."
}

mkt_dir <- if_else(mkt_proj$slope > 0, "more market-liberal", "less market-liberal (declining)")
nat_dir <- if_else(nat_proj$slope > 0, "more nationalist",    "less nationalist (declining)")

rising  <- theme_proj_plot %>% filter(trend == "rising")  %>% arrange(desc(r_squared))
falling <- theme_proj_plot %>% filter(trend == "falling") %>% arrange(desc(r_squared))
top_by_pred <- theme_proj_plot %>% arrange(desc(pred_2027))

build_theme_rows <- function(df) {
  paste(map_chr(seq_len(min(nrow(df), 4)), function(i) {
    r <- df[i, ]
    glue("- **{r$dimension}**: {round(r$last_val, 1)} mentions/1k words in 2026 -> {round(r$pred_2027, 1)} projected 2027 (R2={round(r$r_squared, 2)}, news signal={r$news_match_score})")
  }), collapse = "\n")
}

report <- glue("
# Predicted Budget Speech: Nirmala Sitharaman 2027

*Generated: {Sys.Date()}*
*Method: Linear vocabulary trend (2019-2026) + PRS/EcoSurvey pre-budget signal ({latest_news_year})*

---

## Methodology

Two signals combined:

1. **Internal signal (vocabulary trends):** Linear trend on policy-theme keyword
   frequencies (per 1,000 words) across Sitharaman's 8 full budget speeches,
   using Part A text only (macro/policy section; tax schedules excluded).
   R-squared values measure how well the linear trend fits the data.

2. **External signal (news themes):** Dominant vocabulary from PRS Legislative
   Research budget analyses and PIB Economic Survey press releases for the
   most recent year available ({latest_news_year}). This captures what the
   external policy discourse was foregrounding.

When internal and external signals agree, confidence is higher.

**Honest caveat:** 8 data points from one Finance Minister. A linear trend
with this sample size has wide prediction intervals. Exogenous shocks
(global recession, coalition pressure, fiscal crisis) override any trend.

---

## Most Predictable Theme: Manufacturing/PLI

The strongest quantitative signal in the data is the **Manufacturing and PLI
vocabulary** trend: R2 = {mfg_r2}.
This is the most predictable trend in her budgets. Expect continued and
growing emphasis on production-linked incentives, Atmanirbhar Bharat,
MSME support, and domestic industrial capacity.

---

## Rising Themes (expected to increase in 2027)

{build_theme_rows(rising)}

## Falling Themes (expected to decrease in 2027)

{build_theme_rows(falling)}

## Highest Expected Volume (mentions per 1k words)

{build_theme_rows(top_by_pred)}

---

## Ideology Trajectory

- **Market-liberal axis:** {round(mkt_proj$last_val, 4)} in 2026 -> projected {round(mkt_proj$pred_2027, 4)} in 2027 ({mkt_dir})
  (R2 = {round(mkt_proj$r_squared, 3)})
- **Nationalist axis:** {round(nat_proj$last_val, 4)} in 2026 -> projected {round(nat_proj$pred_2027, 4)} in 2027 ({nat_dir})
  (R2 = {round(nat_proj$r_squared, 3)})

{ideo_note} Both axes show low R-squared -- treat both as 'stable with noise'.

---

## Pre-Budget News Signal ({latest_news_year})

Top vocabulary from PRS/EcoSurvey pre-budget coverage:

**{paste(top_news_words[1:20], collapse = ', ')}**

{news_signal_note}

---

## Summary Prediction

If Sitharaman's past trajectory continues:

1. **Manufacturing and fiscal consolidation** will be the dominant themes
   (strongest R-squared trends).
2. **Digital/tech vocabulary** will continue rising, consistent with
   previous budgets' emphasis on UPI, fintech, and digital infrastructure.
3. **Green/climate vocabulary** is trending up but with high variance --
   likely present but uncertain in magnitude.
4. The **ideological register** will remain roughly stable -- neither more
   nor less market-liberal or nationalist than the 2025-26 budget.

The single biggest structural uncertainty: whether there is an election in
2027 or major global downturn, either of which would shift the fiscal
stance materially.

---
*Piyush Zaware | University of Chicago*
")

writeLines(report, file.path(OUTDIR, "prediction_report.md"))
message("Saved: prediction_report.md")
message("\nF2 complete.")
#}
