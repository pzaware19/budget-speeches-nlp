# =============================================================================
# _master.R  --  Budget Speech Ideal Points Project
# Author: Piyush Zaware
# Last updated: 2026-06-16
#
# Orchestrates the full pipeline. Run this file to reproduce all outputs.
# Each script can also be run standalone in sequence.
# =============================================================================

# -- PATHS --------------------------------------------------------------------
if (Sys.info()[["user"]] == "piyushzaware") {
  root <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
}

INPDIR  <- file.path(root, "input")
CODDIR  <- file.path(root, "code")
OUTDIR  <- file.path(root, "output")
TMPDIR  <- file.path(root, "tmp")

# -- EXECUTABLES --------------------------------------------------------------
rscript <- "/usr/local/bin/Rscript"

# -- PIPELINE -----------------------------------------------------------------
# A: Data collection
system2(rscript, file.path(CODDIR, "A1_scrape_download.R"))   # download PDFs + extract text
system2(rscript, file.path(CODDIR, "A2_clean_text.R"))        # clean text + build DFM

# B: Topic modelling
system2(rscript, file.path(CODDIR, "B1_lda_topics.R"))        # k selection + base STM (no covariates)
system2(rscript, file.path(CODDIR, "B2_stm_covariates.R"))    # STM with party + year covariates

# C: Validation and figures  [to be written]
# system2(rscript, file.path(CODDIR, "C1_fiscal_validation.R"))
# system2(rscript, file.path(CODDIR, "C2_figures.R"))
