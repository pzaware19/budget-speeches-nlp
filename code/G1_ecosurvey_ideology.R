# =============================================================================
# G1_ecosurvey_ideology.R
# Author: Piyush Zaware
# Last updated: 2026-06-17
#
# Goal: Compare the ideological position of the Economic Survey (written by
#       the Chief Economic Adviser) against the Budget Speech delivered the
#       next day (written by the Finance Ministry). When the two diverge, it
#       signals tension between the technocratic and political arms of fiscal
#       governance.
#
# Method: Apply the same four-pole ideology dictionaries from C1 to the
#         Economic Survey texts. Use plain term proportions (not TF-IDF) for
#         both ES and the corresponding budget speeches so the scores are on
#         a directly comparable scale. Re-scoring the 6 budget speeches with
#         term proportions is done separately from the full corpus TF-IDF
#         scores in ideology_scores.csv.
#
# Data: Economic Survey PIB press releases for 2020-2025.
#       ecosurvey_2020.txt = ES released Jan 2020, before Budget 2020-21 speech.
#       NOTE: 2020 text is only 157 words (PIB scraping returned minimal content).
#       We include it but flag it as unreliable.
#
# CEA tenure:
#   K.V. Subramanian : Dec 2018 – Dec 2021  (ES 2020, 2021)
#   V. Anantha Nageswaran : Jan 2022 – present  (ES 2022, 2023, 2024, 2025)
#
# IN
#   output/news/raw/ecosurvey_{year}.txt
#   output/corpus_clean/                     -- budget speech clean texts
#   output/dtm/speech_meta.csv
#
# OUT
#   output/tables/tab_es_ideology.csv        -- ES ideology scores
#   output/tables/tab_es_bs_comparison.csv   -- ES vs BS comparison
#   output/figures/fig_es_vs_bs.png          -- main comparison figure
#   output/figures/fig_es_divergence.png     -- divergence plot
# =============================================================================

suppressPackageStartupMessages({
  library(tidytext)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(ggrepel)
  library(glue)
  library(purrr)
})

root   <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
RAWDIR <- file.path(root, "output", "news", "raw")
CLEAN  <- file.path(root, "output", "corpus_clean")
DTMDIR <- file.path(root, "output", "dtm")
FIGDIR <- file.path(root, "output", "figures")
TABDIR <- file.path(root, "output", "tables")

# =============================================================================
# IDEOLOGY DICTIONARIES (from C1_ideology_scores.R)
# =============================================================================
#{
dict_socialist <- c(
  "nationalise","nationalised","nationalisation","psu","cooperative","cooperatives",
  "welfare","subsidy","subsidies","subsidise","subsidised",
  "poor","poverty","weaker","backward","disadvantaged","redistribution","egalitarian",
  "labour","workers","worker","wage","wages","workmen",
  "ration","foodgrain","pds","equity","inequalities","inequality",
  "planned","outlay","public investment"
)
dict_capitalist <- c(
  "private","entrepreneur","entrepreneurs","entrepreneurship",
  "market","markets","competition","competitive",
  "deregulate","deregulation","liberalise","liberalisation","liberalize","liberalization",
  "equity","shareholder","dividend","securities","ipo",
  "fdi","investor","investors",
  "disinvest","disinvestment","privatise","privatisation","privatize","privatization",
  "efficiency","productivity","reform","reforms","consolidation","discipline"
)
dict_globalist <- c(
  "exports","export","gatt","wto","multilateral","bilateral",
  "convertibility","globalisation","globalization","integration",
  "competitiveness","foreign"
)
dict_nationalist <- c(
  "swadeshi","indigenous","self-reliance","self-reliant","self-sufficient",
  "atmanirbhar","atmanirbharta",
  "domestic","protection","protective","protectionist",
  "strategic","national","make","local","locally","tariff","tariffs"
)

all_dicts <- list(
  socialist   = dict_socialist,
  capitalist  = dict_capitalist,
  globalist   = dict_globalist,
  nationalist = dict_nationalist
)
#}

# =============================================================================
# TERM-PROPORTION IDEOLOGY SCORING FUNCTION
# Inputs: a character string of text
# Returns: named vector with s_* scores and axis scores
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

score_text <- function(text, label = "doc") {
  tokens <- tibble(text = text) %>%
    unnest_tokens(word, text) %>%
    anti_join(all_stopwords, by = "word") %>%
    filter(nchar(word) >= 3,
           str_detect(word, "^[a-z'\\-]+$"),
           !str_detect(word, "^[0-9]+$"))

  n_total <- nrow(tokens)
  if (n_total == 0) return(NULL)

  words <- tokens$word

  score_pole <- function(dict) {
    # Single-word matches only (multi-word phrases ignored for consistency)
    sw <- dict[!str_detect(dict, "\\s")]
    sum(words %in% sw) / n_total
  }

  s_soc  <- score_pole(dict_socialist)
  s_cap  <- score_pole(dict_capitalist)
  s_glob <- score_pole(dict_globalist)
  s_nat  <- score_pole(dict_nationalist)

  tibble(
    label            = label,
    n_words          = n_total,
    s_socialist      = s_soc,
    s_capitalist     = s_cap,
    s_globalist      = s_glob,
    s_nationalist    = s_nat,
    axis_market      = s_cap  - s_soc,
    axis_nationalist = s_nat  - s_glob
  )
}
#}

# =============================================================================
# SCORE ECONOMIC SURVEY TEXTS
# =============================================================================
#{
# Year = year ES released (Jan); budget fy_start = same year (Feb budget)
es_meta <- tibble(
  year     = 2020:2025,
  fy_start = 2020:2025,
  cea      = c(
    "K.V. Subramanian", "K.V. Subramanian",
    "V.A. Nageswaran",  "V.A. Nageswaran",
    "V.A. Nageswaran",  "V.A. Nageswaran"
  ),
  short_flag = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)   # 2020/2022/2025 too short
)

es_scores <- map_dfr(es_meta$year, function(yr) {
  path <- file.path(RAWDIR, glue("ecosurvey_{yr}.txt"))
  if (!file.exists(path)) { message(glue("Missing: {path}")); return(NULL) }
  text <- paste(readLines(path, warn = FALSE), collapse = " ")
  score_text(text, label = as.character(yr))
}) %>%
  mutate(year = as.integer(label)) %>%
  left_join(es_meta, by = "year")

message("Economic Survey scores:")
print(es_scores %>% select(year, n_words, axis_market, axis_nationalist, cea, short_flag))
#}

# =============================================================================
# RE-SCORE CORRESPONDING BUDGET SPEECHES (term proportions, not TF-IDF)
# =============================================================================
#{
meta <- read_csv(file.path(DTMDIR, "speech_meta.csv"), show_col_types = FALSE)

# Map fy_start → clean text filename for NS's speeches
bs_file_map <- c(
  "2019" = "bs201920_clean.txt",
  "2020" = "Budget_Speech_2020-21_clean.txt",
  "2021" = "Budget_Speech_2021-22_clean.txt",
  "2022" = "Budget_Speech_2022-23_clean.txt",
  "2023" = "Budget_Speech_2023-24_clean.txt",
  "2024" = "Budget_Speech_2024-25_clean.txt",
  "2025" = "Budget_Speech_2025-26_clean.txt",
  "2026" = "Budget_Speech_2026-27_clean.txt"
)

bs_scores_tp <- map_dfr(names(bs_file_map), function(yr_str) {
  yr   <- as.integer(yr_str)
  path <- file.path(CLEAN, bs_file_map[yr_str])
  if (!file.exists(path)) return(NULL)
  text <- paste(readLines(path, warn = FALSE), collapse = " ")
  score_text(text, label = yr_str) %>% mutate(fy_start = yr)
})

message("\nBudget speech scores (term proportions):")
print(bs_scores_tp %>% select(fy_start, n_words, axis_market, axis_nationalist))
#}

# =============================================================================
# MERGE AND COMPUTE DIVERGENCE
# =============================================================================
#{
# Only years where we have both ES and BS
paired <- es_scores %>%
  filter(!short_flag) %>%               # drop 2020 — too short
  inner_join(
    bs_scores_tp %>% select(fy_start, bs_market = axis_market,
                             bs_nationalist = axis_nationalist,
                             bs_words = n_words),
    by = "fy_start"
  ) %>%
  mutate(
    div_market      = axis_market      - bs_market,       # positive = ES more market-liberal
    div_nationalist = axis_nationalist - bs_nationalist,  # positive = ES more nationalist
    year_label      = glue("{fy_start}-{fy_start - 1999}"),
    agree_market    = sign(axis_market) == sign(bs_market),
    agree_nationalist = sign(axis_nationalist) == sign(bs_nationalist)
  )

write_csv(es_scores,   file.path(TABDIR, "tab_es_ideology.csv"))
write_csv(paired,      file.path(TABDIR, "tab_es_bs_comparison.csv"))
message("\nSaved: tab_es_ideology.csv, tab_es_bs_comparison.csv")

message("\nES vs BS divergence:")
print(paired %>% select(fy_start, cea,
                         es_market = axis_market, bs_market,
                         div_market,
                         es_nat = axis_nationalist, bs_nationalist,
                         div_nationalist))

message(glue("\nMarket axis sign agreement: {sum(paired$agree_market)}/{nrow(paired)} years"))
message(glue("Nationalist axis sign agreement: {sum(paired$agree_nationalist)}/{nrow(paired)} years"))
#}

# =============================================================================
# FIGURES
# =============================================================================
#{

# -- Figure 1: ES vs BS on both axes (arrow plot) ----------------------------
# Arrow plot in 2D: arrow from BS to ES
arrow_data <- paired %>%
  select(fy_start, year_label, cea,
         x_start = bs_market, y_start = bs_nationalist,
         x_end   = axis_market, y_end   = axis_nationalist) %>%
  mutate(
    distance = sqrt((x_end - x_start)^2 + (y_end - y_start)^2)
  )

p_arrow <- ggplot(arrow_data) +
  # Axes
  geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = "dashed") +
  # Arrows from Budget Speech (tail) to Economic Survey (head)
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
                   colour = cea),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
               linewidth = 1.1) +
  # Budget speech points (squares)
  geom_point(aes(x = x_start, y = y_start, colour = cea),
             shape = 15, size = 3.5) +
  # Economic Survey points (circles)
  geom_point(aes(x = x_end, y = y_end, colour = cea),
             shape = 16, size = 3.5) +
  # Year labels near ES point
  geom_text_repel(aes(x = x_end, y = y_end, label = as.character(fy_start)),
                  size = 3.2, colour = "#444", nudge_x = 0.0003) +
  scale_colour_manual(values = c("K.V. Subramanian" = "#2a5e2a",
                                  "V.A. Nageswaran"  = "#b5310e"),
                      name = "Chief Economic Adviser") +
  labs(
    title    = "Economic Survey vs Budget Speech: ideological positions",
    subtitle = "Square = Budget Speech. Circle = Economic Survey (day before).\nArrow points from Budget Speech toward Economic Survey.",
    x = "Market-liberal axis  ←  state-led | market →",
    y = "Nationalist axis  ←  globalist | protectionist →"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(FIGDIR, "fig_es_vs_bs.png"), p_arrow,
       width = 8, height = 7, dpi = 150)
message("Saved: fig_es_vs_bs.png")

# -- Figure 2: Divergence bar chart per year, per axis -----------------------
div_long <- paired %>%
  select(fy_start, cea, div_market, div_nationalist) %>%
  pivot_longer(c(div_market, div_nationalist),
               names_to  = "axis",
               values_to = "divergence") %>%
  mutate(
    axis  = recode(axis,
      "div_market"      = "Market-liberal axis",
      "div_nationalist" = "Nationalist axis"
    ),
    direction = if_else(divergence > 0, "ES more →", "ES more ←")
  )

p_div <- ggplot(div_long,
    aes(x = factor(fy_start), y = divergence,
        fill = direction)) +
  geom_col(width = 0.6, colour = "white") +
  geom_hline(yintercept = 0, colour = "#333", linewidth = 0.5) +
  facet_wrap(~ axis, ncol = 1, scales = "free_y") +
  scale_fill_manual(
    values = c("ES more →" = "#2a5e2a", "ES more ←" = "#b5310e"),
    name   = NULL,
    labels = c(
      "ES more →" = "ES more market-liberal / more nationalist than BS",
      "ES more ←" = "ES more state-led / more globalist than BS"
    )
  ) +
  labs(
    title    = "Divergence between Economic Survey and Budget Speech",
    subtitle = "Divergence = ES score minus BS score. Years where CEA and FM pulled in opposite directions.",
    x = "Budget year", y = "ES minus BS"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "#f5f1ea"),
        strip.text       = element_text(face = "bold"))

ggsave(file.path(FIGDIR, "fig_es_divergence.png"), p_div,
       width = 8, height = 7, dpi = 150)
message("Saved: fig_es_divergence.png")
#}

message("\nG1 complete.")
