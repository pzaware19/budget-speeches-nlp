# =============================================================================
# G4_scrape_ecosurvey.R
# Author: Piyush Zaware
# Last updated: 2026-06-22
#
# Goal: Download the FULL Economic Survey (all chapters, English) for recent
#       years from indiabudget.gov.in, replacing the short PIB press releases
#       used in G1/G3. Each survey is tabled a day before the Union Budget, so
#       the survey in URL budget{B}/economicsurvey precedes budget B.
#
# Source: https://www.indiabudget.gov.in/economicsurvey/allpes.php
#   Each year's index page links a complete English PDF:
#     doc/echapter.pdf           (and doc/echapter_vol2.pdf for 2-volume years)
#   Hindi (doc/hechapter*.pdf) is skipped.
#
# Survey label Y = the survey released in Jan of year Y (precedes Budget Y/Y+1),
#   matching the ecosurvey_{Y}.txt convention used in G1/G3. So:
#     label 2023 -> budget2023-24/economicsurvey -> "Economic Survey 2022-23".
#
# OUT
#   input/ecosurvey_pdfs/{Y}_{vol}.pdf     -- original downloaded PDFs
#   output/ecosurvey_full/{Y}.txt          -- extracted, concatenated text
# Requires: curl and pdftotext on PATH (verified present).
# =============================================================================

suppressPackageStartupMessages({ library(stringr); library(glue) })

root    <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
PDFDIR  <- file.path(root, "input",  "ecosurvey_pdfs")
TXTDIR  <- file.path(root, "output", "ecosurvey_full")
dir.create(PDFDIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TXTDIR, showWarnings = FALSE, recursive = TRUE)

UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"

labels  <- 2019:2026                                  # survey release year
base_of <- function(Y) glue("https://www.indiabudget.gov.in/budget{Y}-{sprintf('%02d', (Y+1) %% 100)}/economicsurvey")

curl_get <- function(url, outfile = NULL) {
  args <- c("-sL", "-A", shQuote(UA), "--max-time", "180")
  if (is.null(outfile)) {
    system2("curl", c(args, shQuote(url)), stdout = TRUE, stderr = FALSE)
  } else {
    system2("curl", c(args, "-o", shQuote(outfile), shQuote(url)), stdout = FALSE)
  }
}

for (Y in labels) {
  out_txt <- file.path(TXTDIR, glue("{Y}.txt"))
  if (file.exists(out_txt) && file.info(out_txt)$size > 50000) {
    message(glue("[{Y}] already have full text, skipping.")); next
  }
  base <- base_of(Y)
  html <- paste(curl_get(glue("{base}/index.php")), collapse = "\n")
  if (!nzchar(html)) { message(glue("[{Y}] index fetch failed: {base}")); next }

  # English complete-survey PDFs only: doc/echapter*.pdf  (exclude hechapter)
  hrefs <- str_match_all(html, 'href="(doc/echapter[^"]*\\.pdf)"')[[1]][, 2]
  hrefs <- unique(hrefs)
  hrefs <- hrefs[order(str_detect(hrefs, "vol2"))]    # vol1 before vol2
  if (length(hrefs) == 0) { message(glue("[{Y}] no English echapter PDF at {base}")); next }

  texts <- c()
  for (h in hrefs) {
    vol  <- if (str_detect(h, "vol2")) "vol2" else "vol1"
    pdf  <- file.path(PDFDIR, glue("{Y}_{vol}.pdf"))
    url  <- glue("{base}/{h}")
    if (!file.exists(pdf) || file.info(pdf)$size < 100000) {
      message(glue("[{Y}] downloading {vol}: {url}"))
      curl_get(url, pdf)
      Sys.sleep(1)
    }
    if (!file.exists(pdf) || file.info(pdf)$size < 100000) {
      message(glue("[{Y}] download failed or too small: {pdf}")); next
    }
    txt <- system2("pdftotext", c("-q", shQuote(pdf), "-"), stdout = TRUE, stderr = FALSE)
    texts <- c(texts, paste(txt, collapse = " "))
  }
  if (length(texts) == 0) { message(glue("[{Y}] no text extracted")); next }
  writeLines(paste(texts, collapse = " "), out_txt)
  nw <- length(str_split(paste(texts, collapse = " "), "\\s+")[[1]])
  message(glue("[{Y}] saved {basename(out_txt)}  ({length(hrefs)} vol, ~{nw} words)"))
}

message("G4 complete.")
