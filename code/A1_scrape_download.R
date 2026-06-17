# =============================================================================
# A1_scrape_download.R
# Author: Piyush Zaware
# Last updated: 2026-06-16
#
# Goal: Download all Union Budget speech PDFs (1947-2026) from indiabudget.gov.in
#       and extract raw text from each PDF.
#
# URL architecture (reverse-engineered from site):
#   1947-48 to 2019-20  -> budget2019-20/doc/bspeech/bs{YYYYYYY}.pdf
#   2020-21 to 2025-26  -> budget{YYYY-YY}/doc/Budget_Speech.pdf
#   2026-27 (current)   -> doc/Budget_Speech.pdf
#   2008-09 to 2010-11  -> budget_archive/ub{YYYY-YY}/speech.htm  (HTML only)
#
# IN
#   input/budget_metadata.csv   -- FM name, party, year, government
#
# OUT
#   input/pdfs/                 -- raw PDF files
#   output/corpus/              -- extracted plain text (.txt per speech)
#   tmp/download_log.csv        -- status of every download attempt
# =============================================================================

library(httr)
library(rvest)
library(pdftools)
library(readr)
library(dplyr)
library(stringr)
library(glue)

# -- PATHS --------------------------------------------------------------------
#{
root    <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
INPDIR  <- file.path(root, "input")
PDFDIR  <- file.path(root, "input", "pdfs")
CODDIR  <- file.path(root, "code")
OUTDIR  <- file.path(root, "output")
CORPDIR <- file.path(root, "output", "corpus")
TMPDIR  <- file.path(root, "tmp")
#}

# -- BROWSER HEADERS ----------------------------------------------------------
# indiabudget.gov.in blocks non-browser user agents
#{
HEADERS <- add_headers(
  `User-Agent`      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
  `Accept`          = "text/html,application/pdf,*/*;q=0.8",
  `Accept-Language` = "en-US,en;q=0.9"
)
#}

# -- STEP 1: SCRAPE HISTORICAL SPEECH LINKS (1947-2019) -----------------------
# The budget2019-20 microsite hosts all speeches from 1947-48 to 2019-20
# as a single archive. We scrape the index page to get the full link list.
#{
HIST_BASE <- "https://www.indiabudget.gov.in/budget2019-20/"
HIST_PAGE <- paste0(HIST_BASE, "bspeech.php")

message("Scraping historical speech index from budget2019-20/bspeech.php ...")
page_html <- GET(HIST_PAGE, HEADERS, timeout(30))
links_raw <- read_html(content(page_html, as = "text")) %>%
  html_elements("a") %>%
  html_attr("href")

hist_pdfs <- links_raw[grepl("doc/bspeech/bs.*\\.pdf", links_raw, ignore.case = TRUE)]
hist_pdfs <- unique(hist_pdfs)
message(glue("  Found {length(hist_pdfs)} historical speech PDFs"))
#}

# -- STEP 2: BUILD COMPLETE URL TABLE -----------------------------------------
# Parse year from filename, tag interim vs full, assign full URL.
#{

parse_speech_filename <- function(href) {
  fname <- basename(href)                              # bs199192.pdf
  stem  <- str_remove(fname, "\\.pdf$")               # bs199192
  code  <- str_remove(stem, "^bs")                    # 199192 or 199192(I) etc.

  is_interim <- grepl("\\(I\\)", code, ignore.case = TRUE)
  is_special <- grepl("\\(", code) & !grepl("\\(I\\)", code, ignore.case = TRUE)

  # Extract numeric part only for year parsing
  code_num <- str_extract(code, "^[0-9]+")

  # Year decoding: bs194748 -> 1947; bs19992000 -> 1999
  fy_start <- case_when(
    nchar(code_num) == 6 ~ as.integer(str_sub(code_num, 1, 4)),  # bs194748 -> 1947
    nchar(code_num) == 8 ~ as.integer(str_sub(code_num, 1, 4)),  # bs19992000 -> 1999
    TRUE                 ~ NA_integer_
  )

  budget_year <- if (!is.na(fy_start)) {
    yr2 <- (fy_start + 1) %% 100
    sprintf("%d-%02d", fy_start, yr2)
  } else NA_character_

  tibble(
    href        = href,
    fname       = fname,
    budget_year = budget_year,
    fy_start    = fy_start,
    is_interim  = is_interim,
    is_special  = is_special,
    full_url    = paste0(HIST_BASE, href)
  )
}

hist_tbl <- bind_rows(lapply(hist_pdfs, parse_speech_filename))

# Add 2020-21 to 2025-26 (new microsite format)
new_years <- tibble(
  fy_start    = 2020:2025,
  budget_year = c("2020-21","2021-22","2022-23","2023-24","2024-25","2025-26"),
  is_interim  = FALSE,
  is_special  = FALSE
) %>%
  mutate(
    full_url = glue("https://www.indiabudget.gov.in/budget{budget_year}/doc/Budget_Speech.pdf"),
    fname    = glue("Budget_Speech_{budget_year}.pdf"),
    href     = NA_character_
  )

# Add 2026-27 (current year, served at root)
current_year <- tibble(
  fy_start    = 2026,
  budget_year = "2026-27",
  is_interim  = FALSE,
  is_special  = FALSE,
  full_url    = "https://www.indiabudget.gov.in/doc/Budget_Speech.pdf",
  fname       = "Budget_Speech_2026-27.pdf",
  href        = NA_character_
)

# Add 2024-25 interim (separate URL)
interim_2024 <- tibble(
  fy_start    = 2024,
  budget_year = "2024-25",
  is_interim  = TRUE,
  is_special  = FALSE,
  full_url    = "https://www.indiabudget.gov.in/budget2024-25(I)/doc/Budget_Speech.pdf",
  fname       = "Budget_Speech_2024-25_interim.pdf",
  href        = NA_character_
)

all_urls <- bind_rows(hist_tbl, new_years, current_year, interim_2024) %>%
  mutate(
    budget_type = case_when(
      is_interim ~ "interim",
      is_special ~ "special",
      TRUE       ~ "full"
    ),
    pdf_filename = case_when(
      !is.na(fname) ~ str_replace_all(fname, "[()/ ]", "_"),
      TRUE          ~ glue("bs_{budget_year}_{budget_type}.pdf")
    ),
    pdf_path = file.path(PDFDIR,  pdf_filename),
    txt_path = file.path(CORPDIR, str_replace(pdf_filename, "\\.pdf$", ".txt"))
  )

message(glue("Total speeches mapped: {nrow(all_urls)}"))
#}

# -- STEP 3: DOWNLOAD ALL PDFs ------------------------------------------------
#{
download_one <- function(url, pdf_path, label) {
  if (file.exists(pdf_path)) {
    return("already_exists")
  }
  tryCatch({
    resp <- GET(url, HEADERS, timeout(60), write_disk(pdf_path, overwrite = TRUE))
    sz   <- file.info(pdf_path)$size
    if (status_code(resp) == 200 && !is.na(sz) && sz > 3000) {
      message(glue("  [OK]     {label}"))
      return("success")
    } else {
      if (file.exists(pdf_path)) file.remove(pdf_path)
      message(glue("  [FAIL]   {label}  HTTP {status_code(resp)}  size={sz}"))
      return(glue("failed_{status_code(resp)}"))
    }
  }, error = function(e) {
    if (file.exists(pdf_path)) file.remove(pdf_path)
    message(glue("  [ERR]    {label}  {conditionMessage(e)}"))
    return("error")
  })
}

message("\n=== DOWNLOADING ===\n")

all_urls <- all_urls %>%
  rowwise() %>%
  mutate(
    download_status = {
      Sys.sleep(0.3)   # polite 300ms between requests
      download_one(full_url, pdf_path, glue("{budget_year} {budget_type}"))
    }
  ) %>%
  ungroup()
#}

# -- STEP 4: EXTRACT TEXT FROM PDFs ------------------------------------------
#{
extract_one <- function(pdf_path, txt_path, label) {
  if (!file.exists(pdf_path))  return("no_pdf")
  if (file.exists(txt_path))   return("already_extracted")
  tryCatch({
    pages     <- pdf_text(pdf_path)
    full_text <- paste(pages, collapse = "\n")
    full_text <- str_replace_all(full_text, "\f",       "\n")
    full_text <- str_replace_all(full_text, "[ \t]{2,}", " ")
    writeLines(full_text, txt_path)
    nw <- length(str_split(full_text, "\\s+")[[1]])
    message(glue("  [TXT]    {label}  {nw} words"))
    return(as.character(nw))
  }, error = function(e) {
    message(glue("  [ERR]    {label}  {conditionMessage(e)}"))
    return("extraction_error")
  })
}

message("\n=== EXTRACTING TEXT ===\n")

all_urls <- all_urls %>%
  rowwise() %>%
  mutate(
    word_count = extract_one(pdf_path, txt_path, glue("{budget_year} {budget_type}"))
  ) %>%
  ungroup()
#}

# -- STEP 5: MERGE WITH METADATA AND SAVE LOG ---------------------------------
#{
meta <- read_csv(file.path(INPDIR, "budget_metadata.csv"), show_col_types = FALSE) %>%
  mutate(budget_type = if_else(budget_type == "interim", "interim", "full"))

log <- all_urls %>%
  left_join(
    meta %>% select(budget_year, budget_type, fm_name, fm_party, fm_party_family,
                    pm_name, government_coalition),
    by = c("budget_year", "budget_type")
  ) %>%
  select(budget_year, fy_start, budget_type, fm_name, fm_party, fm_party_family,
         pm_name, government_coalition, full_url, pdf_filename,
         download_status, word_count)

write_csv(log, file.path(TMPDIR, "download_log.csv"))
#}

# -- SUMMARY ------------------------------------------------------------------
#{
n_ok      <- sum(log$download_status %in% c("success", "already_exists"))
n_fail    <- sum(str_starts(log$download_status, "failed") | log$download_status == "error")
n_txt     <- sum(!log$word_count %in% c("no_pdf", "extraction_error", NA) &
                 !is.na(log$word_count))

message(glue("
=== SUMMARY ===
PDFs on disk:      {n_ok}
Failed/error:      {n_fail}
Text files ready:  {n_txt}
Log: tmp/download_log.csv
"))

if (n_fail > 0) {
  message("Failed speeches:")
  log %>%
    filter(str_starts(download_status, "failed") | download_status == "error") %>%
    select(budget_year, budget_type, full_url, download_status) %>%
    print(n = Inf)
}

message("\nA1 complete. Run A2_clean_text.R next.")
#}
