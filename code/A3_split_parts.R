# =============================================================================
# A3_split_parts.R
# Author: Piyush Zaware
# Last updated: 2026-06-16
#
# Goal: Split each raw budget speech into Part A (macro/expenditure) and
#       Part B (tax proposals) using the explicit section markers that appear
#       in almost every speech. Part A is the politically meaningful text;
#       Part B is largely a list of duty and tax rate changes.
#
# Detection cascade for the Part A/B boundary:
#   1. Standalone line matching "PART B", "PART-B", "PART–B" (most speeches)
#   2. Standalone line "TAX PROPOSALS"
#   3. Inline phrase: "move to Part B|now present my tax proposals|
#                      turn to Part B|turn to my tax proposals"
#   4. First standalone "DIRECT TAXATION" section heading (old pre-1960 speeches)
#   5. No boundary found: flag as Part-A-only (speech has no explicit Part B)
#
# IN
#   output/corpus/*.txt       -- raw extracted texts from A1
#   tmp/download_log.csv      -- metadata
#
# OUT
#   output/corpus_parta/*.txt -- Part A text per speech
#   output/corpus_partb/*.txt -- Part B text per speech (empty if not found)
#   tmp/split_log.csv         -- boundary line, method, parta/partb word counts
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(glue)
library(purrr)

root    <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
CORPDIR <- file.path(root, "output", "corpus")
PARTADIR <- file.path(root, "output", "corpus_parta")
PARTBDIR <- file.path(root, "output", "corpus_partb")
TMPDIR   <- file.path(root, "tmp")

dir.create(PARTADIR, showWarnings = FALSE)
dir.create(PARTBDIR, showWarnings = FALSE)

# -- DETECTION PATTERNS -------------------------------------------------------
# Each pattern returns the first matching line number (1-indexed), or NA.
#{

# Pattern 1: standalone "PART B / PART-B / PART–B / PART B."
pat_standalone <- regex(
  "^\\s*PART\\s*[-–]?\\s*B[:\\s.]*$",
  ignore_case = TRUE
)

# Pattern 2: standalone "TAX PROPOSALS"
pat_taxproposals <- regex(
  "^\\s*TAX\\s+PROPOSALS\\s*$",
  ignore_case = TRUE
)

# Pattern 3: inline transition phrase
pat_inline <- regex(
  paste(
    "move to Part B",
    "now present my tax proposals",
    "turn to Part B",
    "turn to the tax proposals",
    "now turn to my tax proposals",
    "now proceed to Part B",
    "I shall now deal with taxation",
    "now to my tax proposals",
    "come to the tax proposals",         # Jaswant Singh 2003-04
    "come to my proposals.*tax",
    "impatiently.*direct taxes",         # VP Singh 1990-91
    "now deal with my proposals relating to indirect",
    "I shall now deal with my proposals",
    sep = "|"
  ),
  ignore_case = TRUE
)

# Pattern 4: standalone DIRECT TAXATION heading (pre-1960 speeches)
pat_direct_tax_hdr <- regex(
  "^\\s*DIRECT\\s+TAXATION\\s*$",
  ignore_case = TRUE
)

find_boundary <- function(lines) {

  # Try each pattern in priority order
  m1 <- which(str_detect(lines, pat_standalone))
  if (length(m1) > 0) return(list(line = min(m1), method = "standalone_partb"))

  m2 <- which(str_detect(lines, pat_taxproposals))
  if (length(m2) > 0) return(list(line = min(m2), method = "tax_proposals_hdr"))

  m3 <- which(str_detect(lines, pat_inline))
  if (length(m3) > 0) return(list(line = min(m3), method = "inline_phrase"))

  m4 <- which(str_detect(lines, pat_direct_tax_hdr))
  if (length(m4) > 0) return(list(line = min(m4), method = "direct_tax_hdr"))

  return(list(line = NA_integer_, method = "not_found"))
}
#}

# -- PROCESS ALL SPEECHES -----------------------------------------------------
#{
txt_files <- list.files(CORPDIR, pattern = "\\.txt$", full.names = TRUE)
message(glue("Processing {length(txt_files)} speeches ..."))

split_log <- map_dfr(txt_files, function(path) {

  doc_id   <- str_remove(basename(path), "\\.txt$")
  lines    <- readLines(path, warn = FALSE)
  result   <- find_boundary(lines)
  bline    <- result$line
  method   <- result$method

  if (!is.na(bline)) {
    parta_lines <- lines[1:(bline - 1)]
    partb_lines <- lines[bline:length(lines)]
  } else {
    parta_lines <- lines
    partb_lines <- character(0)
  }

  parta_text <- paste(parta_lines, collapse = "\n")
  partb_text <- paste(partb_lines, collapse = "\n")

  writeLines(parta_text, file.path(PARTADIR, paste0(doc_id, "_parta.txt")))
  writeLines(partb_text, file.path(PARTBDIR, paste0(doc_id, "_partb.txt")))

  words <- function(txt) length(str_split(str_squish(txt), " ")[[1]])

  tibble(
    doc_id         = doc_id,
    boundary_line  = bline,
    boundary_pct   = if (!is.na(bline)) round(bline / length(lines), 2) else NA_real_,
    method         = method,
    total_lines    = length(lines),
    words_parta    = words(parta_text),
    words_partb    = words(partb_text)
  )
})

write_csv(split_log, file.path(TMPDIR, "split_log.csv"))

# Summary
found    <- sum(!is.na(split_log$boundary_line))
not_found <- sum(is.na(split_log$boundary_line))
message(glue("\nBoundary found:     {found} speeches"))
message(glue("Boundary not found: {not_found} speeches"))
message("\nDetection method breakdown:")
print(table(split_log$method))

message("\nSpeeches with NO Part B boundary:")
split_log %>%
  filter(is.na(boundary_line)) %>%
  select(doc_id, total_lines, words_parta) %>%
  print()

message("\nPart A / Part B word-count split (median across speeches with boundary):")
split_log %>%
  filter(!is.na(boundary_line)) %>%
  summarise(
    median_parta = median(words_parta),
    median_partb = median(words_partb),
    parta_share  = round(median(words_parta / (words_parta + words_partb)), 2)
  ) %>%
  print()
#}
