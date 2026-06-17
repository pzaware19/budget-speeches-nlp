# =============================================================================
# A2_clean_text.R
# Author: Piyush Zaware
# Last updated: 2026-06-16
#
# Goal: Clean raw budget speech texts and build a document-term matrix (DTM)
#       ready for LDA/STM topic modelling in B1.
#
# Cleaning steps per speech:
#   1. Strip header block (title, FM name, date, CONTENTS table)
#   2. Strip end boilerplate (Jai Hind, date brackets, commendation phrase)
#   3. Remove paragraph numbers (artifacts of PDF extraction, e.g. "1 5 3 .")
#   4. Remove PART A / PART B section markers
#   5. Remove lines that are only numbers or TOC entries (word + trailing number)
#   6. Normalise whitespace
#
# DTM construction:
#   - Tokenise to unigrams
#   - Remove standard English stop words (Snowball) + custom budget stop words
#   - Minimum word length: 3 characters
#   - Minimum document frequency: 3 speeches (word must appear in >= 3 docs)
#   - Maximum document frequency: 95% (word must be absent from >= 5% of docs)
#
# IN
#   output/corpus/*.txt          -- raw extracted texts from A1
#   tmp/download_log.csv         -- metadata (FM, party, year, budget_type)
#
# OUT
#   output/corpus_clean/*.txt    -- one cleaned text file per speech
#   output/dtm/tidy_tokens.rds   -- long-format tibble: doc_id x word x n
#   output/dtm/budget_dfm.rds    -- quanteda DFM (documents x features)
#   output/dtm/speech_meta.csv   -- metadata for each document in the DTM
#   tmp/cleaning_log.csv         -- word counts before and after cleaning
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(quanteda)
library(glue)
library(purrr)

# -- PATHS --------------------------------------------------------------------
#{
root     <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
CORPDIR  <- file.path(root, "output", "corpus")
CLEANDIR <- file.path(root, "output", "corpus_clean")
DTMDIR   <- file.path(root, "output", "dtm")
TMPDIR   <- file.path(root, "tmp")
INPDIR   <- file.path(root, "input")

dir.create(CLEANDIR, showWarnings = FALSE)
dir.create(DTMDIR,   showWarnings = FALSE)
#}

# -- LOAD AND ENRICH METADATA -------------------------------------------------
#{
log <- read_csv(file.path(TMPDIR, "download_log.csv"), show_col_types = FALSE)

# Fix the bs.txt entry: identified as 2014-15 interim by P. Chidambaram
log <- log %>%
  mutate(
    budget_year     = if_else(pdf_filename == "bs_.pdf", "2014-15",    budget_year),
    budget_type     = if_else(pdf_filename == "bs_.pdf", "interim",    budget_type),
    fm_name         = if_else(pdf_filename == "bs_.pdf", "P. Chidambaram", fm_name),
    fm_party        = if_else(pdf_filename == "bs_.pdf", "INC",        fm_party),
    fm_party_family = if_else(pdf_filename == "bs_.pdf", "INC",        fm_party_family),
    pm_name         = if_else(pdf_filename == "bs_.pdf", "Manmohan Singh", pm_name),
    government_coalition = if_else(pdf_filename == "bs_.pdf", "UPA-II", government_coalition)
  )

# Build doc_id and text file path
log <- log %>%
  filter(download_status %in% c("success", "already_exists")) %>%
  mutate(
    txt_filename = str_replace(pdf_filename, "\\.pdf$", ".txt"),
    txt_path     = file.path(CORPDIR, txt_filename),
    doc_id       = str_remove(txt_filename, "\\.txt$")
  ) %>%
  filter(file.exists(txt_path))

message(glue("Speeches to process: {nrow(log)}"))
#}

# -- CUSTOM STOP WORDS --------------------------------------------------------
#{
budget_stopwords <- c(
  # Parliamentary boilerplate
  "sir", "madam", "speaker", "honourable", "hon", "ble", "member", "members",
  "house", "august", "rise", "present", "budget", "speech", "interim",
  "commend", "commends",

  # Generic temporal (in all speeches)
  "year", "years", "current", "previous", "next", "last", "during",
  "period", "annual", "per", "cent", "percent", "annum",

  # Currency / fiscal units (too common to distinguish)
  "rupee", "rupees", "rs", "crore", "crores", "lakh", "lakhs", "paise",
  "thousand", "million", "billion",

  # Generic India-government boilerplate
  "india", "indian", "government", "central", "state", "states", "national",
  "country", "countries", "union",

  # Very common transitions/connectors not caught by Snowball
  "therefore", "however", "moreover", "furthermore", "accordingly",
  "therefore", "thus", "hence", "also", "well", "shall", "will", "may",
  "must", "need", "good", "great", "new", "total", "including",
  "including", "including", "number", "numbers", "part",

  # OCR artifacts common in old scans
  "th", "nd", "rd", "st",

  # Section markers that survive line stripping
  "page", "contents", "introduction"
)

# Combine with tidytext Snowball stop words
all_stopwords <- bind_rows(
  stop_words,
  tibble(word = budget_stopwords, lexicon = "custom")
) %>%
  distinct(word)
#}

# -- TEXT CLEANING FUNCTION ---------------------------------------------------
#{
clean_speech <- function(raw_text, doc_id) {

  lines <- str_split(raw_text, "\n")[[1]]

  # --- Strip header block ---
  # Header ends at the first "Sir," / "Madam" / "Mr. Speaker" greeting line,
  # OR at the first paragraph that starts with a numeral and looks like content.
  # Strategy: drop lines before the first greeting OR first substantive sentence.
  greeting_pat  <- "^\\s*(Sir[,.]|Madam[,. ]|Mr\\.?\\s*Speaker|Madam Speaker|Hon.ble|I rise|I present)"
  greeting_idx  <- which(str_detect(lines, regex(greeting_pat, ignore_case = TRUE)))

  if (length(greeting_idx) > 0) {
    lines <- lines[min(greeting_idx):length(lines)]
  } else {
    # Fallback: drop first 15 lines (covers title + date block)
    lines <- lines[min(16, length(lines)):length(lines)]
  }

  # --- Strip TOC lines ---
  # Pattern: "Some Title Words   12" — a line ending with 1-3 digits after spaces
  toc_pat <- "^[A-Za-z ,'/()-]{5,}\\s{2,}\\d{1,3}\\s*$"
  lines   <- lines[!str_detect(lines, toc_pat)]

  # --- Strip PART A / PART B / PART C markers and section headers ---
  section_pat <- "^\\s*(PART[- ][A-C]|PART–[A-C]|TAX PROPOSALS|TAX REFORMS|DIRECT TAXES|INDIRECT TAXES|CUSTOMS|EXCISE|SERVICE TAX|GST PROPOSALS)\\s*$"
  lines <- lines[!str_detect(lines, regex(section_pat, ignore_case = TRUE))]

  # --- Strip end boilerplate ---
  end_pat <- "JAI HIND|Jai Hind|I commend (the|this) budget|\\[\\d{1,2}(st|nd|rd|th)?\\s+[A-Z][a-z]+|VANDE MATARAM"
  end_idx <- which(str_detect(lines, end_pat))
  if (length(end_idx) > 0) {
    lines <- lines[1:max(1, min(end_idx) - 1)]
  }

  # --- Remove paragraph number artifacts ---
  # PDF extraction sometimes turns "153." into " 1 5 3 ." with spaces
  # Also handles normal "2." or "2 ." at start of line
  lines <- str_replace(lines, "^\\s*(\\d\\s){1,3}\\d\\s*\\.\\s*", "")  # "1 5 3 ."
  lines <- str_replace(lines, "^\\s*\\d{1,3}\\.\\s*",              "")  # "153."

  # --- Remove lines that are only numbers, dashes, or very short ---
  lines <- lines[nchar(str_trim(lines)) >= 20]

  # --- Remove lines that are mostly numbers (tables) ---
  # If > 60% of non-space chars are digits/punctuation, it's a number table row
  is_table_row <- function(line) {
    chars     <- str_remove_all(line, "\\s")
    num_chars <- str_remove_all(chars, "[^0-9,.()+\\-]")
    nchar(chars) > 0 && nchar(num_chars) / nchar(chars) > 0.6
  }
  lines <- lines[!sapply(lines, is_table_row)]

  # --- Normalise whitespace ---
  text_clean <- paste(lines, collapse = " ")
  text_clean <- str_replace_all(text_clean, "\\s{2,}", " ")
  text_clean <- str_trim(text_clean)

  text_clean
}
#}

# -- APPLY CLEANING TO ALL SPEECHES ------------------------------------------
#{
message("\n=== CLEANING SPEECHES ===\n")

cleaning_log <- log %>%
  rowwise() %>%
  mutate(
    raw_text   = readLines(txt_path, warn = FALSE) %>% paste(collapse = "\n"),
    words_raw  = length(str_split(raw_text,   "\\s+")[[1]]),
    text_clean = clean_speech(raw_text, doc_id),
    words_clean = length(str_split(text_clean, "\\s+")[[1]])
  ) %>%
  ungroup()

# Save cleaned texts
walk2(cleaning_log$text_clean, cleaning_log$doc_id, function(txt, id) {
  writeLines(txt, file.path(CLEANDIR, paste0(id, "_clean.txt")))
})

message(glue("  Cleaned {nrow(cleaning_log)} speeches"))
message(glue("  Median words before cleaning: {median(cleaning_log$words_raw, na.rm=TRUE)}"))
message(glue("  Median words after  cleaning: {median(cleaning_log$words_clean, na.rm=TRUE)}"))

write_csv(
  cleaning_log %>% select(doc_id, budget_year, budget_type, fm_name, fm_party,
                           words_raw, words_clean),
  file.path(TMPDIR, "cleaning_log.csv")
)
#}

# -- TOKENISE AND BUILD TIDY TOKEN TABLE -------------------------------------
#{
message("\n=== TOKENISING ===\n")

tidy_tokens <- cleaning_log %>%
  select(doc_id, budget_year, fy_start, budget_type, fm_name, fm_party,
         fm_party_family, pm_name, government_coalition, text_clean) %>%
  unnest_tokens(word, text_clean) %>%
  # Remove stop words
  anti_join(all_stopwords, by = "word") %>%
  # Minimum word length
  filter(nchar(word) >= 3) %>%
  # Remove pure numbers and number strings
  filter(!str_detect(word, "^[0-9,.()+\\-]+$")) %>%
  # Remove words with non-ASCII characters (OCR artifacts)
  filter(str_detect(word, "^[a-z'\\-]+$"))

# Count tokens per doc
tidy_counts <- tidy_tokens %>%
  count(doc_id, word, sort = TRUE)

# Document frequency filter: keep words in >= 3 speeches and <= 95% of speeches
n_docs    <- n_distinct(tidy_counts$doc_id)
doc_freq  <- tidy_counts %>%
  group_by(word) %>%
  summarise(doc_freq = n_distinct(doc_id), .groups = "drop")

keep_words <- doc_freq %>%
  filter(doc_freq >= 3, doc_freq <= 0.95 * n_docs) %>%
  pull(word)

tidy_counts_filtered <- tidy_counts %>%
  filter(word %in% keep_words)

message(glue("  Vocabulary (after filters): {length(keep_words)} terms"))
message(glue("  Speeches: {n_docs}"))
message(glue("  Total tokens: {sum(tidy_counts_filtered$n)}"))

saveRDS(tidy_counts_filtered, file.path(DTMDIR, "tidy_tokens.rds"))
#}

# -- BUILD QUANTEDA DFM -------------------------------------------------------
#{
message("\n=== BUILDING DFM ===\n")

dtm_wide <- tidy_counts_filtered %>%
  cast_dfm(doc_id, word, n)

saveRDS(dtm_wide, file.path(DTMDIR, "budget_dfm.rds"))

message(glue("  DFM dimensions: {ndoc(dtm_wide)} docs x {nfeat(dtm_wide)} features"))
message(glue("  Sparsity: {round(sparsity(dtm_wide)*100, 1)}%"))
#}

# -- SAVE SPEECH METADATA -----------------------------------------------------
#{
speech_meta <- cleaning_log %>%
  select(doc_id, budget_year, fy_start, budget_type,
         fm_name, fm_party, fm_party_family, pm_name, government_coalition,
         words_raw, words_clean) %>%
  # Flag short speeches (interims/specials < 3000 words) as potentially unreliable
  mutate(is_short = words_clean < 3000)

write_csv(speech_meta, file.path(DTMDIR, "speech_meta.csv"))
message(glue("\nSpeech metadata saved: {nrow(speech_meta)} rows"))

# Quick summary
message("\n=== PARTY BREAKDOWN ===")
speech_meta %>%
  filter(budget_type == "full") %>%
  count(fm_party_family) %>%
  print()

message("\nA2 complete. Run B1_lda_topics.R next.")
#}
