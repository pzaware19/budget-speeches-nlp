# =============================================================================
# H1_collect_fiscal.R
# Author: Piyush Zaware
# Last updated: 2026-06-17
#
# Goal: Two tasks.
#   (1) Compute policy-theme keyword frequencies for ALL budget speeches
#       (not just Sitharaman). This extends the F1 theme analysis to the
#       full 1947-2026 corpus so we can run long-panel regressions.
#   (2) Download World Bank (WDI) fiscal and macro indicators for India
#       to serve as the "actual money" side of the text-expenditure analysis.
#
# Theme keyword lists are identical to F1 (Infrastructure, Green/Climate,
# Digital/Tech, Welfare/Social, Manufacturing/PLI, Fiscal/Deficit) plus
# two additional themes (Defence, FDI/Openness) that map to WDI indicators.
#
# WDI indicators used:
#   MS.MIL.XPND.GD.ZS  -- military expenditure % GDP          (1960-2024)
#   SE.XPD.TOTL.GD.ZS  -- education expenditure % GDP         (1970-2022)
#   SH.XPD.GHED.GD.ZS  -- government health expenditure % GDP (2000-2021)
#   NE.GDI.FTOT.ZS     -- gross fixed capital formation % GDP (1960-2024)
#   GC.NLD.TOTL.GD.ZS  -- net lending/borrowing % GDP         (1990-2022)
#   EG.ELC.RNEW.ZS     -- renewable electricity % total       (1990-2022)
#   BX.KLT.DINV.WD.GD.ZS -- FDI net inflows % GDP            (1970-2023)
#
# IN
#   output/corpus_clean/           -- all 92 clean speech texts
#   output/dtm/speech_meta.csv     -- doc_id → fy_start, budget_type, fm_name
#
# OUT
#   output/tables/tab_all_vocab_themes.csv   -- theme freqs for all speeches
#   output/tables/tab_fiscal_data.csv        -- WDI fiscal indicators
# =============================================================================

suppressPackageStartupMessages({
  library(tidytext); library(readr); library(dplyr); library(tidyr)
  library(stringr); library(purrr); library(glue); library(WDI)
})

root   <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
CLEAN  <- file.path(root, "output", "corpus_clean")
DTMDIR <- file.path(root, "output", "dtm")
TABDIR <- file.path(root, "output", "tables")

# =============================================================================
# PART 1 — VOCABULARY THEME SCORES FOR ALL SPEECHES
# =============================================================================
#{

theme_keywords <- list(
  "Infrastructure"    = c("infrastructure","highway","railway","port","airport",
                          "road","metro","corridor","connectivity","logistics",
                          "capex","expressway","bridges","tunnel","waterway"),
  "Green_Climate"     = c("green","climate","renewable","solar","wind","energy",
                          "transition","emission","sustainable","battery",
                          "hydrogen","clean","environment","forest","carbon"),
  "Digital_Tech"      = c("digital","technology","startup","innovation","aadhaar",
                          "upi","fintech","artificial","data","cyber","platform",
                          "broadband","semiconductor","electronics","software"),
  "Welfare_Social"    = c("welfare","poor","women","farmer","tribal","health",
                          "education","nutrition","housing","scheme","social",
                          "pmgsy","antyodaya","bpl","destitute","marginalised"),
  "Manufacturing_PLI" = c("manufacturing","pli","production","exports","msme",
                          "atmanirbhar","domestic","capacity","industrial","make",
                          "factories","assembly","value","supply","chain"),
  "Fiscal_Deficit"    = c("fiscal","deficit","consolidation","debt","gdp",
                          "borrowing","revenue","expenditure","glide","surplus",
                          "liabilities","disinvestment","receipts","denominator"),
  "Defence"           = c("defence","defense","armed","forces","military","army",
                          "navy","air","security","strategic","modernisation",
                          "ordnance","border","paramilitary","jawans"),
  "FDI_Openness"      = c("foreign","investment","fdi","investor","multinational",
                          "global","bilateral","trade","export","import","wto",
                          "gatt","convertibility","liberalisation","globalisation")
)

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

# Load speech metadata
meta <- read_csv(file.path(DTMDIR, "speech_meta.csv"), show_col_types = FALSE) %>%
  select(doc_id, fy_start, budget_type, fm_name, fm_party_family)

# List all clean text files
clean_files <- list.files(CLEAN, pattern = "_clean\\.txt$", full.names = TRUE)
message(glue("Found {length(clean_files)} clean text files"))

# Score one file against all themes
score_themes <- function(filepath) {
  text <- paste(readLines(filepath, warn = FALSE), collapse = " ")
  tokens <- tibble(text = text) %>%
    unnest_tokens(word, text) %>%
    anti_join(all_stopwords, by = "word") %>%
    filter(nchar(word) >= 3,
           str_detect(word, "^[a-z'\\-]+$"),
           !str_detect(word, "^[0-9]+$"))

  n_total <- nrow(tokens)
  if (n_total < 100) return(NULL)  # skip nearly-empty files
  words <- tokens$word

  map_dfr(names(theme_keywords), function(th) {
    kw      <- theme_keywords[[th]]
    sw      <- kw[!str_detect(kw, "\\s")]  # single words only
    n_hits  <- sum(words %in% sw)
    tibble(theme = th,
           n_hits      = n_hits,
           total_words = n_total,
           share       = n_hits / n_total * 1000)
  })
}

# Build doc_id from filename (mirrors A2 naming convention)
all_theme_scores <- map_dfr(clean_files, function(fp) {
  fname  <- basename(fp)
  doc_id <- str_remove(fname, "_clean\\.txt$")
  res    <- score_themes(fp)
  if (is.null(res)) return(NULL)
  res %>% mutate(doc_id = doc_id)
})

# Join with metadata — deduplicate bs199192 (1991-92 has both Yashwant Sinha
# and Manmohan Singh mapped to the same text file; keep Manmohan Singh only,
# who presented the actual full liberalisation budget in July 1991).
meta_dedup <- meta %>%
  group_by(doc_id) %>%
  arrange(desc(fm_name == "Manmohan Singh")) %>%  # Manmohan Singh rows first
  slice_head(n = 1) %>%
  ungroup()

all_theme_scores <- all_theme_scores %>%
  left_join(meta_dedup, by = "doc_id") %>%
  filter(!is.na(fy_start))

message(glue("\nTheme scores computed for {n_distinct(all_theme_scores$doc_id)} speeches"))
message("Themes: ", paste(unique(all_theme_scores$theme), collapse = ", "))

write_csv(all_theme_scores, file.path(TABDIR, "tab_all_vocab_themes.csv"))
message("Saved: tab_all_vocab_themes.csv")
#}

# =============================================================================
# PART 2 — WDI FISCAL INDICATORS FOR INDIA
# =============================================================================
#{

wdi_indicators <- c(
  mil_pct_gdp      = "MS.MIL.XPND.GD.ZS",   # military % GDP
  edu_pct_gdp      = "SE.XPD.TOTL.GD.ZS",   # education % GDP
  health_pct_gdp   = "SH.XPD.GHED.GD.ZS",   # health % GDP
  gfcf_pct_gdp     = "NE.GDI.FTOT.ZS",       # gross fixed capital formation % GDP
  net_lending_gdp  = "GC.NLD.TOTL.GD.ZS",   # net lending % GDP (negative = deficit)
  renew_pct_elec   = "EG.ELC.RNEW.ZS",       # renewable electricity % total
  fdi_pct_gdp      = "BX.KLT.DINV.WD.GD.ZS" # FDI net inflows % GDP
)

message("\nDownloading WDI indicators for India...")
wdi_raw <- WDI(
  country   = "IN",
  indicator = unname(wdi_indicators),
  start     = 1960,
  end       = 2025,
  extra     = FALSE
)

# Rename columns to friendly names
wdi_clean <- wdi_raw %>%
  rename(!!!setNames(unname(wdi_indicators), names(wdi_indicators))) %>%
  select(year, all_of(names(wdi_indicators))) %>%
  arrange(year) %>%
  # fiscal_deficit_gdp: net lending is positive when surplus, negative when deficit
  # flip sign so positive = larger deficit (easier to interpret alongside spending)
  mutate(fiscal_deficit_gdp = -net_lending_gdp)

message(glue("WDI: {nrow(wdi_clean)} rows, years {min(wdi_clean$year)}-{max(wdi_clean$year)}"))

# Coverage report
for (v in names(wdi_indicators)) {
  n <- sum(!is.na(wdi_clean[[v]]))
  rng <- wdi_clean %>% filter(!is.na(.data[[v]])) %>%
    summarise(lo = min(year), hi = max(year)) %>% as.list()
  message(glue("  {v}: {n} non-NA  ({rng$lo}-{rng$hi})"))
}

write_csv(wdi_clean, file.path(TABDIR, "tab_fiscal_data.csv"))
message("\nSaved: tab_fiscal_data.csv")
#}

message("\nH1 complete.")
