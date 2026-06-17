# =============================================================================
# _master.R  --  Budget Speech NLP Project
# Author: Piyush Zaware
# Last updated: 2026-06-17
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
python3 <- "/usr/bin/python3"

# -- PIPELINE -----------------------------------------------------------------
# A: Data collection
system2(rscript, file.path(CODDIR, "A1_scrape_download.R"))   # download PDFs + extract text
system2(rscript, file.path(CODDIR, "A2_clean_text.R"))        # clean text + build DFM
system2(rscript, file.path(CODDIR, "A3_split_parts.R"))       # detect Part A / Part B boundary

# B: Topic modelling (full speeches)
system2(rscript, file.path(CODDIR, "B1_lda_topics.R"))        # k selection + base STM (no covariates)
system2(rscript, file.path(CODDIR, "B2_stm_covariates.R"))    # STM with party + year covariates
system2(rscript, file.path(CODDIR, "B3_parta_model.R"))       # Part A only STM + BJP-INC comparison

# C: Ideology scoring
system2(rscript, file.path(CODDIR, "C1_ideology_score.R"))    # dictionary-based 2D ideology scores
system2(rscript, file.path(CODDIR, "C2_ideology_figures.R"))  # ideology figures and FM profiles

# D: Election effects (interim vs full budgets)
system2(rscript, file.path(CODDIR, "D1_election_effects.R"))  # within-FM interim vs full comparison

# E: Pre-budget news scraping
system2(python3, file.path(CODDIR, "E1_scrape_news.py"))      # scrape PRS Legislative Research
system2(python3, file.path(CODDIR, "E1b_scrape_pib_fix.py"))  # scrape PIB Economic Survey releases
system2(rscript, file.path(CODDIR, "E2_extract_themes.R"))    # TF-IDF theme extraction from news

# F: Sitharaman trend analysis and 2027 prediction
system2(rscript, file.path(CODDIR, "F1_sitharaman_trend.R"))  # vocabulary + ideology trends
system2(rscript, file.path(CODDIR, "F2_prediction.R"))        # forecast + prediction report

# G: New analyses
system2(rscript, file.path(CODDIR, "G1_ecosurvey_ideology.R")) # Economic Survey vs Budget Speech ideology
system2(rscript, file.path(CODDIR, "G2_fm_trajectory.R"))      # FM ideological drift over tenure

# H: Budget words vs fiscal outcomes
system2(rscript, file.path(CODDIR, "H1_collect_fiscal.R"))     # vocab themes for all years + WDI download
system2(rscript, file.path(CODDIR, "H2_text_budget_regression.R")) # text-fiscal regressions + figures
