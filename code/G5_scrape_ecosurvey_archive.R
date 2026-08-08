# =============================================================================
# G5_scrape_ecosurvey_archive.R (v2)
# Author: Piyush Zaware
# Last updated: 2026-08-07
#
# Goal: Extend the Economic Survey corpus (G4 covers 2019-2025 only) back as
#       far as text-extraction quality allows.
#
# FINDING FROM MANUAL PROBING (see session notes): Economic Survey PDFs on
# indiabudget.gov.in before ~1997-98 are scanned images with NO embedded text
# layer -- pdftotext returns zero words, confirmed for 1957-58 through 1993-94
# directly. OCR would be required and was judged too unreliable/out of scope
# for a rigorous text-similarity piece. This script therefore targets the
# confirmed-good range: label 1998 through label 2018 (bridges to G4's 2019-2025).
#
# Formats discovered, in order tried per year:
#   (a) a convenience "chapt{YYYY}/chapter.zip" bundling all chapter PDFs
#       (works for most of 2005-06 through 2009-10)
#   (b) numbered "echap-NN.pdf" chapters linked directly from a survey.asp page
#       (2010-11 through 2013-14)
#   (c) two-level crawl: an index page (esmain.htm/welcome.html) links to
#       subject pages (general.htm, agriculture.htm, ...), each of which links
#       to the real chapter PDF(s) (1997-98 through 2004-05, and fallback for
#       any chapt-zip year that 404s)
#   (d) hardcoded specific URLs for 2014-15 through 2017-18, where the site
#       structure changes almost every year (verified individually)
#
# OUT
#   input/ecosurvey_pdfs/archive/{label}/*.pdf
#   output/ecosurvey_full/{label}.txt
#   output/ecosurvey_full/SCRAPE_LOG.csv
# =============================================================================

suppressPackageStartupMessages({ library(stringr); library(glue); library(purrr); library(dplyr) })

root    <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
PDFDIR  <- file.path(root, "input", "ecosurvey_pdfs", "archive")
TXTDIR  <- file.path(root, "output", "ecosurvey_full")
dir.create(PDFDIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TXTDIR, showWarnings = FALSE, recursive = TRUE)

UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"

curl_text <- function(url, timeout = 25) {
  out <- tryCatch(system2("curl", c("-sL", "-A", shQuote(UA), "--max-time", timeout, shQuote(url)),
                          stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  paste(out, collapse = "\n")
}
url_exists <- function(url, timeout = 15) {
  code <- tryCatch(system2("curl", c("-sL", "-A", shQuote(UA), "--max-time", timeout, "-o", "/dev/null",
                                      "-w", "%{http_code}", shQuote(url)), stdout = TRUE), error = function(e) "000")
  identical(code, "200")
}
curl_file <- function(url, outfile, timeout = 60) {
  tryCatch({
    system2("curl", c("-sL", "-A", shQuote(UA), "--max-time", timeout, "-o", shQuote(outfile), shQuote(url)))
    file.exists(outfile) && file.info(outfile)$size > 2000
  }, error = function(e) FALSE)
}
extract_links <- function(html, pattern) {
  links <- str_extract_all(html, 'href="[^"]*"')[[1]]
  links <- str_remove_all(links, 'href="|"')
  unique(links[str_detect(tolower(links), pattern)])
}
resolve <- function(base_url, link) {
  if (str_detect(link, "^https?://")) return(link)
  base_dir <- str_remove(base_url, "/[^/]+$")
  paste0(base_dir, "/", link)
}

pdf_text <- function(pdf_path) {
  tf <- tempfile(fileext = ".txt")
  ok <- tryCatch({ system2("pdftotext", c(shQuote(pdf_path), shQuote(tf)), stdout = FALSE, stderr = FALSE); file.exists(tf) },
                 error = function(e) FALSE)
  if (!ok) return("")
  txt <- paste(readLines(tf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  unlink(tf); txt
}

save_year <- function(label, pdf_urls, log_env) {
  if (length(pdf_urls) == 0) { log_env[[as.character(label)]] <- tibble(label=label, status="no_links", n_files=0, n_words=0); return(invisible()) }
  ydir <- file.path(PDFDIR, label); dir.create(ydir, showWarnings = FALSE, recursive = TRUE)
  texts <- c()
  n_ok <- 0
  for (i in seq_along(pdf_urls)) {
    f <- file.path(ydir, sprintf("ch_%03d.pdf", i))
    got <- file.exists(f) && file.info(f)$size > 2000
    if (!got) got <- curl_file(pdf_urls[i], f)
    if (got) { texts <- c(texts, pdf_text(f)); n_ok <- n_ok + 1 }
  }
  full <- paste(texts, collapse = "\n\n")
  nwords <- length(str_split(str_trim(full), "\\s+")[[1]])
  if (nwords < 500) { log_env[[as.character(label)]] <- tibble(label=label, status="scanned_or_empty", n_files=n_ok, n_words=nwords); return(invisible()) }
  writeLines(full, file.path(TXTDIR, glue("{label}.txt")), useBytes = TRUE)
  log_env[[as.character(label)]] <- tibble(label=label, status="ok", n_files=n_ok, n_words=nwords)
}

log_env <- new.env()

# ---- Strategy per label (1998-2018) -----------------------------------------
plan <- list(
  "1998" = list(kind="twolevel", idx="es97-98/welcome.html"),
  "1999" = list(kind="twolevel", idx="es98-99/welcome.html"),
  "2000" = list(kind="twolevel", idx="es99-2000/welcome.html"),
  "2001" = list(kind="twolevel", idx="es2000-01/welcome.html"),
  "2002" = list(kind="twolevel", idx="es2001-02/welcome.html"),
  "2003" = list(kind="twolevel", idx="es2002-03/esmain.htm"),
  "2004" = list(kind="twolevel", idx="es2003-04/esmain.htm"),
  "2005" = list(kind="twolevel", idx="es2004-05/esmain.htm"),
  "2006" = list(kind="zip_or_twolevel", idx="es2005-06/esmain.htm", zip="es2005-06/chapt2006/chapter.zip"),
  "2007" = list(kind="zip_or_twolevel", idx="es2006-07/esmain.htm", zip="es2006-07/chapt2007/chapter.zip"),
  "2008" = list(kind="zip_or_twolevel", idx="es2007-08/esmain.htm", zip="es2007-08/chapt2008/chapter.zip"),
  "2009" = list(kind="zip_or_twolevel", idx="es2008-09/esmain.htm", zip="es2008-09/chapt2009/chapter.zip"),
  "2010" = list(kind="zip_or_twolevel", idx="es2009-10/esmain.htm", zip="es2009-10/chapt2010/chapter.zip"),
  "2011" = list(kind="survey_asp", url="https://www.indiabudget.gov.in/budget2011-2012/survey.asp"),
  "2012" = list(kind="survey_asp", url="https://www.indiabudget.gov.in/budget2012-2013/survey.asp"),
  "2013" = list(kind="survey_asp", url="https://www.indiabudget.gov.in/budget2013-2014/survey.asp"),
  "2014" = list(kind="survey_asp", url="https://www.indiabudget.gov.in/budget2014-2015/survey.asp"),
  "2015" = list(kind="direct", urls=c(
    "https://www.indiabudget.gov.in/budget2015-2016/es2014-15/echapter-vol1.pdf",
    "https://www.indiabudget.gov.in/budget2015-2016/es2014-15/echapter-vol2.pdf")),
  "2016" = list(kind="direct", urls=c(
    "https://www.indiabudget.gov.in/budget2016-2017/es2015-16/echapter-vol1.pdf",
    "https://www.indiabudget.gov.in/budget2016-2017/es2015-16/echapter-vol2.pdf")),
  "2017" = list(kind="direct", urls=c(
    "https://www.indiabudget.gov.in/budget2017-2018/es2016-17/echapter.pdf",
    "https://www.indiabudget.gov.in/budget2017-2018/es2016-17/echapter_vol2.pdf")),
  "2018" = list(kind="index_pdf_english", url="https://www.indiabudget.gov.in/budget2018-2019/economicsurvey2017-2018/index.html")
)

ARCH <- "https://www.indiabudget.gov.in/budget_archive/"

for (label in names(plan)) {
  p <- plan[[label]]
  message(glue("=== {label} ({p$kind}) ==="))
  out_txt <- file.path(TXTDIR, glue("{label}.txt"))
  if (file.exists(out_txt) && file.info(out_txt)$size > 100000) { message("  already done, skipping"); next }

  pdf_urls <- character(0)
  if (p$kind == "twolevel") {
    idx_url <- paste0(ARCH, p$idx)
    html <- curl_text(idx_url)
    sub_links <- extract_links(html, "\\.htm$")
    sub_links <- sub_links[!str_detect(sub_links, "welcome\\.html|esmain\\.htm|links\\.htm|previousub\\.htm|\\.\\./")]
    for (sl in unique(sub_links)) {
      sub_url <- resolve(idx_url, sl)
      sub_html <- curl_text(sub_url)
      pdf_urls <- c(pdf_urls, map_chr(extract_links(sub_html, "\\.pdf$"), ~resolve(sub_url, .x)))
    }
    # some old years link pdfs directly on the index page itself
    pdf_urls <- c(pdf_urls, map_chr(extract_links(html, "\\.pdf$"), ~resolve(idx_url, .x)))
    pdf_urls <- unique(pdf_urls)
  } else if (p$kind == "zip_or_twolevel") {
    zip_url <- paste0(ARCH, p$zip)
    if (url_exists(zip_url)) {
      zdir <- file.path(PDFDIR, glue("{label}_zip")); dir.create(zdir, showWarnings=FALSE, recursive=TRUE)
      zf <- file.path(zdir, "chapter.zip")
      if (curl_file(zip_url, zf, timeout=90)) {
        unzip_dir <- file.path(zdir, "unz"); dir.create(unzip_dir, showWarnings=FALSE)
        system2("unzip", c("-o", "-q", shQuote(zf), "-d", shQuote(unzip_dir)))
        local_pdfs <- list.files(unzip_dir, pattern="\\.pdf$", full.names=TRUE, ignore.case=TRUE)
        if (length(local_pdfs) > 0) pdf_urls <- paste0("file://", local_pdfs)
      }
    }
    if (length(pdf_urls) == 0) {
      idx_url <- paste0(ARCH, p$idx)
      html <- curl_text(idx_url)
      sub_links <- extract_links(html, "\\.htm$")
      sub_links <- sub_links[!str_detect(sub_links, "welcome\\.html|esmain\\.htm|links\\.htm|previousub\\.htm|\\.\\./")]
      for (sl in unique(sub_links)) {
        sub_url <- resolve(idx_url, sl)
        sub_html <- curl_text(sub_url)
        pdf_urls <- c(pdf_urls, map_chr(extract_links(sub_html, "\\.pdf$"), ~resolve(sub_url, .x)))
      }
      pdf_urls <- unique(pdf_urls)
    }
  } else if (p$kind == "survey_asp") {
    html <- curl_text(p$url)
    links <- extract_links(html, "echap-\\d+\\.pdf$")
    pdf_urls <- map_chr(links, ~resolve(p$url, .x))
  } else if (p$kind == "direct") {
    pdf_urls <- p$urls
  } else if (p$kind == "index_pdf_english") {
    html <- curl_text(p$url)
    links <- extract_links(html, "\\.pdf$")
    links <- links[str_detect(links, "(?i)chapter") & !str_detect(links, "(?i)hindi")]
    pdf_urls <- unique(map_chr(links, ~resolve(p$url, .x)))
  }

  message(glue("  found {length(pdf_urls)} pdf(s)"))
  # handle file:// (already local from zip) vs remote
  local_idx <- str_detect(pdf_urls, "^file://")
  ydir <- file.path(PDFDIR, label); dir.create(ydir, showWarnings = FALSE, recursive = TRUE)
  texts <- c(); n_ok <- 0
  for (i in seq_along(pdf_urls)) {
    if (local_idx[i]) {
      fpath <- str_remove(pdf_urls[i], "^file://")
      texts <- c(texts, pdf_text(fpath)); n_ok <- n_ok + 1
    } else {
      f <- file.path(ydir, sprintf("ch_%03d.pdf", i))
      got <- file.exists(f) && file.info(f)$size > 2000
      if (!got) got <- curl_file(pdf_urls[i], f)
      if (got) { texts <- c(texts, pdf_text(f)); n_ok <- n_ok + 1 }
    }
  }
  full <- paste(texts, collapse = "\n\n")
  nwords <- length(str_split(str_trim(full), "\\s+")[[1]])
  status <- if (nwords < 500) "scanned_or_empty" else "ok"
  if (status == "ok") writeLines(full, out_txt, useBytes = TRUE)
  log_env[[label]] <- tibble(label=label, status=status, n_files=n_ok, n_words=nwords)
  message(glue("  status={status} n_files={n_ok} n_words={nwords}"))
}

log_df <- bind_rows(as.list(log_env)) %>% arrange(as.integer(label))
write.csv(log_df, file.path(TXTDIR, "SCRAPE_LOG_v2.csv"), row.names = FALSE)
print(log_df, n = 100)
cat("\nDone. See output/ecosurvey_full/SCRAPE_LOG_v2.csv\n")
