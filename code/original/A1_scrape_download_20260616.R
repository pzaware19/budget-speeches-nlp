# =============================================================================
# A1_scrape_download.R
# Author: Piyush Zaware
# Last updated: 2026-06-16
#
# Goal: Download Union Budget speech PDFs (1947-2024) from indiabudget.gov.in
#       and extract raw text from each PDF.
#
# IN
#   budget_metadata.csv     -- FM name, party, year, URL (if known)
#
# OUT
#   input/pdfs/             -- raw PDF files, named {fy_start}_{budget_type}_{fm_slug}.pdf
#   output/corpus/          -- extracted plain text, one .txt per speech
#   tmp/download_log.csv    -- status of every download attempt
# =============================================================================

library(httr)
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

dir.create(PDFDIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(CORPDIR, showWarnings = FALSE, recursive = TRUE)
#}

# -- LOAD METADATA ------------------------------------------------------------
#{
meta <- read_csv(file.path(INPDIR, "budget_metadata.csv"), show_col_types = FALSE)

# Slugify FM name for filenames: "Nirmala Sitharaman" -> "nirmala_sitharaman"
meta <- meta %>%
  mutate(
    fm_slug    = str_to_lower(str_replace_all(fm_name, "[^a-zA-Z]", "_")),
    fm_slug    = str_replace_all(fm_slug, "_+", "_"),
    pdf_name   = glue("{fy_start}_{budget_type}_{fm_slug}.pdf"),
    txt_name   = str_replace(pdf_name, "\\.pdf$", ".txt"),
    pdf_path   = file.path(PDFDIR,  pdf_name),
    txt_path   = file.path(CORPDIR, txt_name)
  )
#}

# -- URL CONSTRUCTION ---------------------------------------------------------
# indiabudget.gov.in organises archive by financial year.
# Pattern for post-2014 speeches (verified structure):
#   https://www.indiabudget.gov.in/doc/Budget_Speech.pdf   (current year)
#   https://www.indiabudget.gov.in/bspeech/bs{YY}{YY}.pdf (archive, e.g. bs2324.pdf)
#
# For pre-2000 speeches, no reliable URL pattern is known.
# Those rows remain NA and are flagged for manual download below.
#{

build_url <- function(fy_start, budget_type) {
  # Two-digit year codes
  yy1 <- str_sub(as.character(fy_start),     3, 4)   # e.g. "14" for 2014
  yy2 <- str_sub(as.character(fy_start + 1), 3, 4)   # e.g. "15" for 2015

  if (fy_start >= 2014) {
    # Archive pattern confirmed for 2014-2024
    glue("https://www.indiabudget.gov.in/bspeech/bs{yy1}{yy2}.pdf")
  } else if (fy_start >= 1999) {
    # Earlier archive — same pattern but less reliable; script will try and log
    glue("https://www.indiabudget.gov.in/bspeech/bs{yy1}{yy2}.pdf")
  } else {
    NA_character_  # Pre-1999: manual download required
  }
}

meta <- meta %>%
  mutate(
    url_to_try = case_when(
      !is.na(source_url) ~ source_url,     # use provided URL if in metadata
      TRUE               ~ mapply(build_url, fy_start, budget_type)
    )
  )
#}

# -- DOWNLOAD FUNCTION --------------------------------------------------------
#{
download_speech <- function(url, pdf_path, budget_year, fm_name) {

  if (file.exists(pdf_path)) {
    message(glue("  [SKIP] {budget_year} {fm_name} — already exists"))
    return("already_exists")
  }

  if (is.na(url)) {
    message(glue("  [MANUAL] {budget_year} {fm_name} — no URL, needs manual download"))
    return("needs_manual")
  }

  tryCatch({
    resp <- GET(
      url,
      user_agent("Mozilla/5.0 (compatible; academic research bot)"),
      timeout(60),
      write_disk(pdf_path, overwrite = TRUE)
    )

    if (http_status(resp)$category == "Success" &&
        file.info(pdf_path)$size > 5000) {
      message(glue("  [OK]   {budget_year} {fm_name}"))
      return("success")
    } else {
      file.remove(pdf_path)
      message(glue("  [FAIL] {budget_year} {fm_name} — HTTP {status_code(resp)} or empty file"))
      return(glue("failed_http_{status_code(resp)}"))
    }
  }, error = function(e) {
    if (file.exists(pdf_path)) file.remove(pdf_path)
    message(glue("  [ERR]  {budget_year} {fm_name} — {conditionMessage(e)}"))
    return("error")
  })
}
#}

# -- RUN DOWNLOADS ------------------------------------------------------------
#{
message("\n=== DOWNLOADING BUDGET SPEECHES ===\n")
Sys.sleep(1)  # polite pause before starting

log <- meta %>%
  rowwise() %>%
  mutate(
    download_status = download_speech(url_to_try, pdf_path, budget_year, fm_name)
  ) %>%
  ungroup()

# brief pause between requests (be polite to the server)
# (built into the rowwise loop implicitly; add Sys.sleep if hitting rate limits)
#}

# -- TEXT EXTRACTION ----------------------------------------------------------
#{
message("\n=== EXTRACTING TEXT FROM PDFs ===\n")

extract_text <- function(pdf_path, txt_path, budget_year, fm_name) {

  if (!file.exists(pdf_path)) return("no_pdf")
  if (file.exists(txt_path))  return("already_extracted")

  tryCatch({
    pages <- pdf_text(pdf_path)
    full_text <- paste(pages, collapse = "\n")

    # Basic cleaning: collapse extra whitespace, strip page headers/footers
    full_text <- str_replace_all(full_text, "\\r", "")
    full_text <- str_replace_all(full_text, "\f", "\n")     # form feeds
    full_text <- str_replace_all(full_text, "[ \t]{2,}", " ")

    writeLines(full_text, txt_path)
    nchars <- nchar(full_text)
    message(glue("  [OK]   {budget_year} {fm_name} — {nchars} chars"))
    return(as.character(nchars))

  }, error = function(e) {
    message(glue("  [ERR]  {budget_year} {fm_name} — {conditionMessage(e)}"))
    return("extraction_error")
  })
}

log <- log %>%
  rowwise() %>%
  mutate(
    extraction_status = extract_text(pdf_path, txt_path, budget_year, fm_name)
  ) %>%
  ungroup()
#}

# -- SAVE DOWNLOAD LOG --------------------------------------------------------
#{
log_out <- log %>%
  select(budget_year, fy_start, budget_type, fm_name, fm_party, fm_party_family,
         pm_name, government_coalition, url_to_try,
         download_status, extraction_status, pdf_name, txt_name)

write_csv(log_out, file.path(TMPDIR, "download_log.csv"))
message(glue("\nLog saved to tmp/download_log.csv"))
#}

# -- SUMMARY REPORT -----------------------------------------------------------
#{
message("\n=== DOWNLOAD SUMMARY ===\n")

summary_tbl <- log_out %>%
  count(download_status) %>%
  arrange(desc(n))

print(summary_tbl)

n_success       <- sum(log_out$download_status == "success")
n_exists        <- sum(log_out$download_status == "already_exists")
n_manual        <- sum(log_out$download_status == "needs_manual")
n_failed        <- sum(str_starts(log_out$download_status, "failed"))
n_error         <- sum(log_out$download_status == "error")
n_extracted     <- sum(log_out$extraction_status != "no_pdf" &
                       log_out$extraction_status != "extraction_error" &
                       !is.na(log_out$extraction_status))

message(glue("
  Downloaded (new):   {n_success}
  Already on disk:    {n_exists}
  Needs manual:       {n_manual}  <-- see below
  Failed (HTTP):      {n_failed}
  Errors:             {n_error}
  Text files ready:   {n_extracted}
"))

# Print the manual download list clearly
manual_list <- log_out %>%
  filter(download_status == "needs_manual") %>%
  select(budget_year, fm_name, fm_party)

if (nrow(manual_list) > 0) {
  message("=== SPEECHES NEEDING MANUAL DOWNLOAD ===")
  message("Source: Lok Sabha archives https://loksabha.nic.in or scan from")
  message("        Ministry of Finance historical records\n")
  print(manual_list, n = Inf)
}
#}

message("\nA1 complete. Run A2_clean_text.R next.")
