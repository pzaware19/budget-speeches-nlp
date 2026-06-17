# =============================================================================
# C1_ideology_scores.R
# Author: Piyush Zaware
# Last updated: 2026-06-16
#
# Goal: Score each budget speech on two ideological axes:
#
#   Axis 1 (X): State-led / Socialist  <-->  Market-liberal / Capitalist
#   Axis 2 (Y): Globalist / Open       <-->  Economic Nationalist / Protectionist
#
# Method: Dictionary-based TF-IDF scoring.
#   For each axis, two word lists define the poles.
#   Speech score = (TF-IDF mass of pole-A words - TF-IDF mass of pole-B words)
#                   / total TF-IDF mass in speech
#   This normalises for speech length and overall vocabulary richness.
#
# IN
#   output/dtm/budget_dfm.rds     -- quanteda DFM (92 docs x 8308 features)
#   output/dtm/speech_meta.csv    -- metadata (FM, party, year)
#
# OUT
#   output/dtm/ideology_scores.csv          -- speech-level ideology coordinates
#   output/figures/fig_ideology_2d.png      -- 2D map: all speeches + FM average
#   output/figures/fig_ideology_time.png    -- time series of both axes
#   output/figures/fig_ideology_fm.png      -- bar chart: FM average positions
#   output/figures/fig_ideology_bjp_inc.png -- BJP vs INC distributions
# =============================================================================

library(quanteda)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(scales)
library(glue)
library(purrr)

set.seed(42)

# -- PATHS --------------------------------------------------------------------
#{
root   <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
DTMDIR <- file.path(root, "output", "dtm")
FIGDIR <- file.path(root, "output", "figures")
TABDIR <- file.path(root, "output", "tables")
#}

# -- IDEOLOGY DICTIONARIES ----------------------------------------------------
# Words are matched against the cleaned DFM vocabulary (lower-case, no stop words).
# Two axes, four poles.
#{

# AXIS 1: State-led/Socialist  <-->  Market-liberal/Capitalist
# Positive score = more market-liberal; Negative score = more state/socialist

dict_socialist <- c(
  # Public ownership and control
  "nationalise", "nationalised", "nationalisation", "public sector", "psu",
  "cooperative", "cooperatives", "cooperative sector",
  # Redistribution and welfare
  "welfare", "subsidy", "subsidies", "subsidise", "subsidised",
  "poor", "poverty", "weaker", "backward", "disadvantaged",
  "redistribution", "redistributive", "egalitarian",
  # Labour
  "labour", "workers", "worker", "wage", "wages", "employment guarantee",
  "minimum wage", "workmen",
  # Social programmes
  "ration", "rationing", "foodgrain", "pds", "fair price",
  "social justice", "equity", "inequalities", "inequality",
  # Planning era
  "planned", "planning commission", "five year", "outlay",
  "public investment", "state enterprise", "government enterprise"
)

dict_capitalist <- c(
  # Private sector
  "private sector", "private enterprise", "private investment",
  "entrepreneur", "entrepreneurs", "entrepreneurship",
  # Markets and prices
  "market", "markets", "market forces", "competition", "competitive",
  "deregulate", "deregulation", "deregulated", "liberalise", "liberalisation",
  "liberalize", "liberalization",
  # Capital markets
  "equity", "shareholder", "shareholders", "dividend", "dividends",
  "capital market", "stock market", "securities", "ipo",
  # Foreign investment
  "fdi", "foreign investment", "foreign capital", "investor", "investors",
  # Privatisation
  "disinvest", "disinvestment", "privatise", "privatisation",
  "privatize", "privatization",
  # Efficiency and reform
  "efficiency", "productive", "productivity", "reform", "reforms",
  "fiscal consolidation", "fiscal discipline", "deficit reduction"
)

# AXIS 2: Globalist/Open  <-->  Economic Nationalist/Protectionist
# Positive score = more nationalist; Negative score = more globalist

dict_globalist <- c(
  # Trade openness
  "exports", "export promotion", "import liberalisation", "import liberalization",
  "trade liberalisation", "trade liberalization",
  "gatt", "wto", "multilateral", "bilateral",
  # Foreign capital and integration
  "foreign exchange", "convertibility", "current account",
  "globalisation", "globalization", "integration",
  # Export orientation
  "export oriented", "export competitiveness", "export growth",
  "foreign markets", "global markets", "international competitiveness"
)

dict_nationalist <- c(
  # Economic nationalism
  "swadeshi", "indigenous", "self-reliance", "self-reliant", "self-sufficient",
  "self-sufficiency", "atmanirbhar", "atmanirbharta",
  # Domestic industry protection
  "domestic industry", "domestic production", "domestic manufacture",
  "import substitution", "protection", "protective", "protectionist",
  # Strategic sectors
  "strategic", "strategic sector", "strategic industry",
  "national champion", "national interest", "national security",
  # Make in India era
  "make in india", "manufacturing hub", "local", "locally",
  "domestically produced", "domestic content",
  # Import barriers (in context of protecting domestic industry)
  "tariff", "tariffs", "customs duty", "import duty"
)

# Additional single-dimension dictionaries for standalone scoring
dict_agrarian <- c(
  "farmer", "farmers", "agriculture", "agricultural", "crop", "crops",
  "irrigation", "rural", "village", "msp", "minimum support price",
  "kisan", "farm", "farming", "soil", "seed", "fertilizer", "fertilizers",
  "monsoon", "drought", "flood", "cooperative society", "land reform",
  "tenant", "peasant"
)

dict_industrial <- c(
  "industry", "industries", "industrial", "factory", "factories",
  "manufacturing", "steel", "coal", "cement", "textile", "textiles",
  "power", "electricity", "energy", "petroleum", "mining", "production",
  "output", "capacity", "plant", "machinery", "equipment"
)
#}

# -- LOAD AND PREPARE DFM -----------------------------------------------------
#{
dfm_raw  <- readRDS(file.path(DTMDIR, "budget_dfm.rds"))
meta_raw <- read_csv(file.path(DTMDIR, "speech_meta.csv"), show_col_types = FALSE) %>%
  distinct(doc_id, .keep_all = TRUE)

# Party patch for NA entries (same as B2)
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

meta <- meta_raw %>%
  rows_patch(party_patch, by = "doc_id", unmatched = "ignore")

# Convert DFM to TF-IDF weighted matrix
dfm_tfidf <- dfm_tfidf(dfm_raw, scheme_tf = "prop", scheme_df = "inversemax")
vocab <- featnames(dfm_tfidf)
#}

# -- DICTIONARY SCORING FUNCTION ----------------------------------------------
# For each speech, sum TF-IDF weights of words that appear in the dictionary.
# Return the total as a fraction of the speech's total TF-IDF mass.
#{
score_dict <- function(dfm_tfidf, word_list) {
  # Keep only single words that exist in the DFM vocabulary
  single_words <- word_list[!str_detect(word_list, "\\s")]
  matched      <- intersect(single_words, featnames(dfm_tfidf))

  if (length(matched) == 0) return(rep(0, ndoc(dfm_tfidf)))

  sub_dfm  <- dfm_tfidf[, matched]
  rowSums(as.matrix(sub_dfm))
}

# Total TF-IDF mass per document (denominator for normalisation)
total_mass <- rowSums(as.matrix(dfm_tfidf))
total_mass[total_mass == 0] <- 1  # prevent division by zero

# Score each pole
s_socialist   <- score_dict(dfm_tfidf, dict_socialist)   / total_mass
s_capitalist  <- score_dict(dfm_tfidf, dict_capitalist)  / total_mass
s_globalist   <- score_dict(dfm_tfidf, dict_globalist)   / total_mass
s_nationalist <- score_dict(dfm_tfidf, dict_nationalist) / total_mass
s_agrarian    <- score_dict(dfm_tfidf, dict_agrarian)    / total_mass
s_industrial  <- score_dict(dfm_tfidf, dict_industrial)  / total_mass

# Axis scores: positive = more market-liberal / more nationalist
axis_market      <- s_capitalist  - s_socialist    # X axis
axis_nationalist <- s_nationalist - s_globalist    # Y axis

scores <- tibble(
  doc_id          = docnames(dfm_tfidf),
  s_socialist     = s_socialist,
  s_capitalist    = s_capitalist,
  s_globalist     = s_globalist,
  s_nationalist   = s_nationalist,
  s_agrarian      = s_agrarian,
  s_industrial    = s_industrial,
  axis_market     = axis_market,
  axis_nationalist = axis_nationalist
) %>%
  left_join(meta %>% select(doc_id, budget_year, fy_start, budget_type,
                             fm_name, fm_party_family, words_clean),
            by = "doc_id") %>%
  filter(!is.na(fy_start)) %>%
  mutate(
    party = case_when(
      fm_party_family == "BJP"   ~ "BJP",
      fm_party_family == "INC"   ~ "INC",
      fm_party_family == "Other" ~ "Other",
      TRUE                       ~ "Unknown"
    ),
    # Normalise axes to [-1, 1] range for easier interpretation
    axis_market_z      = as.numeric(scale(axis_market)),
    axis_nationalist_z = as.numeric(scale(axis_nationalist))
  )

write_csv(scores, file.path(DTMDIR, "ideology_scores.csv"))
message(glue("Ideology scores computed for {nrow(scores)} speeches"))

# Summary statistics
message("\n=== AXIS SUMMARY ===")
message("Market-liberal axis (capitalist - socialist):")
scores %>% group_by(party) %>%
  summarise(mean = round(mean(axis_market, na.rm=TRUE), 4),
            sd   = round(sd(axis_market, na.rm=TRUE), 4), .groups="drop") %>%
  print()
message("Nationalist axis (nationalist - globalist):")
scores %>% group_by(party) %>%
  summarise(mean = round(mean(axis_nationalist, na.rm=TRUE), 4),
            sd   = round(sd(axis_nationalist, na.rm=TRUE), 4), .groups="drop") %>%
  print()
#}

# -- FIGURE 1: 2D IDEOLOGICAL MAP ---------------------------------------------
# Each dot is one speech; large diamonds are FM averages.
#{
party_colours <- c("BJP" = "#FF9933", "INC" = "#19AAED", "Other" = "#888888")

# FM-level averages (full budgets only for cleaner picture)
fm_avg <- scores %>%
  filter(budget_type == "full") %>%
  group_by(fm_name, party) %>%
  summarise(
    x     = mean(axis_market_z, na.rm = TRUE),
    y     = mean(axis_nationalist_z, na.rm = TRUE),
    n     = n(),
    .groups = "drop"
  )

p_2d <- ggplot() +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +

  # Quadrant labels
  annotate("text", x = -2.2, y =  2.2, label = "STATE-LED\nNATIONALIST",
           colour = "grey55", size = 2.6, fontface = "bold", hjust = 0) +
  annotate("text", x =  0.8, y =  2.2, label = "MARKET\nNATIONALIST",
           colour = "grey55", size = 2.6, fontface = "bold", hjust = 0) +
  annotate("text", x = -2.2, y = -2.0, label = "STATE-LED\nGLOBALIST",
           colour = "grey55", size = 2.6, fontface = "bold", hjust = 0) +
  annotate("text", x =  0.8, y = -2.0, label = "MARKET\nGLOBALIST",
           colour = "grey55", size = 2.6, fontface = "bold", hjust = 0) +

  # Individual speeches (small, semi-transparent)
  geom_point(data = scores %>% filter(budget_type == "full"),
             aes(x = axis_market_z, y = axis_nationalist_z,
                 colour = party, shape = party),
             size = 1.8, alpha = 0.45) +

  # FM averages (larger, labelled)
  geom_point(data = fm_avg,
             aes(x = x, y = y, colour = party, shape = party),
             size = 4.5, alpha = 0.95, stroke = 1) +

  geom_label_repel(data = fm_avg,
                   aes(x = x, y = y, label = fm_name, colour = party),
                   size = 2.6, fontface = "bold",
                   label.padding = unit(0.15, "lines"),
                   box.padding   = 0.35,
                   max.overlaps  = 20,
                   show.legend   = FALSE) +

  scale_colour_manual(values = party_colours, name = "Party") +
  scale_shape_manual(values = c("BJP" = 17, "INC" = 16, "Other" = 15), name = "Party") +

  labs(
    title    = "Ideological map of Union Budget speeches (1947-2026)",
    subtitle = "Each point = one full budget speech. Large labelled points = Finance Minister average.\nX axis: state-led/socialist (left) vs market-liberal/capitalist (right)\nY axis: globalist/open economy (bottom) vs nationalist/protectionist (top)",
    x = "Market-liberal axis  (capitalist minus socialist vocabulary)",
    y = "Nationalist axis  (protectionist minus globalist vocabulary)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "top",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(FIGDIR, "fig_ideology_2d.png"), p_2d,
       width = 11, height = 8.5, dpi = 150)
message("Saved: fig_ideology_2d.png")
#}

# -- FIGURE 2: TIME SERIES OF IDEOLOGY AXES -----------------------------------
#{
scores_long <- scores %>%
  filter(budget_type == "full") %>%
  select(fy_start, fm_name, party, axis_market_z, axis_nationalist_z) %>%
  pivot_longer(c(axis_market_z, axis_nationalist_z),
               names_to = "axis", values_to = "score") %>%
  mutate(axis_label = if_else(axis == "axis_market_z",
                              "Market-liberal axis\n(capitalist vs socialist)",
                              "Nationalist axis\n(protectionist vs globalist)"))

p_time <- ggplot(scores_long,
    aes(x = fy_start, y = score, colour = party, shape = party)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey65", linewidth = 0.4) +
  geom_smooth(aes(group = 1), method = "loess", span = 0.35,
              se = TRUE, colour = "grey40", fill = "grey80",
              linewidth = 0.8, alpha = 0.25) +
  geom_point(size = 2.2, alpha = 0.8) +
  facet_wrap(~ axis_label, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = party_colours, name = NULL) +
  scale_shape_manual(values = c("BJP" = 17, "INC" = 16, "Other" = 15), name = NULL) +
  scale_x_continuous(breaks = seq(1950, 2025, 10)) +
  labs(
    title    = "Budget speech ideology over time",
    subtitle = "Each point is one full budget speech. Grey band = loess trend (all speeches).",
    x = "Year", y = "Score (standardised)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "top",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(FIGDIR, "fig_ideology_time.png"), p_time,
       width = 11, height = 8, dpi = 150)
message("Saved: fig_ideology_time.png")
#}

# -- FIGURE 3: FM AVERAGE POSITIONS (BAR CHART) ------------------------------
#{
fm_bar <- scores %>%
  filter(budget_type == "full") %>%
  group_by(fm_name, party) %>%
  summarise(
    market      = mean(axis_market_z, na.rm = TRUE),
    nationalist = mean(axis_nationalist_z, na.rm = TRUE),
    n           = n(),
    decade_start = min(fy_start, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(market, nationalist),
               names_to = "axis", values_to = "score") %>%
  mutate(
    axis_label = if_else(axis == "market",
                         "Market-liberal (capitalist vs socialist)",
                         "Nationalist (protectionist vs globalist)"),
    # Order FMs chronologically
    fm_ordered = reorder(fm_name, decade_start)
  )

p_fm <- ggplot(fm_bar,
    aes(x = fm_ordered, y = score, fill = party)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey40") +
  coord_flip() +
  facet_wrap(~ axis_label, ncol = 2, scales = "free_x") +
  scale_fill_manual(values = party_colours, name = NULL) +
  labs(
    title    = "Finance Minister ideological positions",
    subtitle = "Ordered chronologically (bottom = earliest). Score = average across full budgets presented by that FM.",
    x = NULL, y = "Score (standardised)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position  = "top",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(FIGDIR, "fig_ideology_fm.png"), p_fm,
       width = 13, height = 7, dpi = 150)
message("Saved: fig_ideology_fm.png")
#}

# -- FIGURE 4: BJP vs INC DISTRIBUTIONS ---------------------------------------
#{
scores_party <- scores %>%
  filter(party %in% c("BJP", "INC"))

scores_long2 <- scores_party %>%
  pivot_longer(c(axis_market_z, axis_nationalist_z),
               names_to = "axis", values_to = "val") %>%
  mutate(axis_label = if_else(axis == "axis_market_z",
                              "Market-liberal axis\n(capitalist vs socialist)",
                              "Nationalist axis\n(protectionist vs globalist)"))

p_dist <- ggplot(scores_long2,
    aes(x = val, y = party, fill = party, colour = party)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_boxplot(alpha = 0.35, width = 0.45, outlier.size = 1.2) +
  geom_jitter(height = 0.12, size = 1.8, alpha = 0.6) +
  facet_wrap(~ axis_label, ncol = 2, scales = "free_x") +
  scale_fill_manual(values   = party_colours, name = NULL) +
  scale_colour_manual(values = party_colours, name = NULL) +
  labs(
    title    = "BJP vs INC: distribution of ideology scores",
    subtitle = "Full budgets only. Each point is one speech. Box shows median and IQR.",
    x = "Score (standardised)", y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "none",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(FIGDIR, "fig_ideology_bjp_inc.png"), p_dist,
       width = 11, height = 5, dpi = 150)
message("Saved: fig_ideology_bjp_inc.png")
#}

# -- SUMMARY TABLE ------------------------------------------------------------
#{
summary_tbl <- scores %>%
  filter(budget_type == "full") %>%
  select(fm_name, party, fy_start, budget_year,
         s_socialist, s_capitalist, s_nationalist, s_globalist,
         axis_market_z, axis_nationalist_z) %>%
  arrange(fy_start)

write_csv(summary_tbl, file.path(TABDIR, "tab_ideology_scores.csv"))

message("\n=== TOP 5 MOST MARKET-LIBERAL FMs ===")
summary_tbl %>%
  group_by(fm_name, party) %>%
  summarise(market = mean(axis_market_z), .groups = "drop") %>%
  slice_max(market, n = 5) %>%
  print()

message("\n=== TOP 5 MOST SOCIALIST FMs ===")
summary_tbl %>%
  group_by(fm_name, party) %>%
  summarise(market = mean(axis_market_z), .groups = "drop") %>%
  slice_min(market, n = 5) %>%
  print()

message("\n=== TOP 5 MOST NATIONALIST FMs ===")
summary_tbl %>%
  group_by(fm_name, party) %>%
  summarise(natl = mean(axis_nationalist_z), .groups = "drop") %>%
  slice_max(natl, n = 5) %>%
  print()

message("\n=== C1 COMPLETE ===")
message(glue("Outputs:
  output/dtm/ideology_scores.csv
  output/tables/tab_ideology_scores.csv
  output/figures/fig_ideology_2d.png
  output/figures/fig_ideology_time.png
  output/figures/fig_ideology_fm.png
  output/figures/fig_ideology_bjp_inc.png"))
#}
