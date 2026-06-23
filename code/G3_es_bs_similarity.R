# =============================================================================
# G3_es_bs_similarity.R
# Author: Piyush Zaware
# Last updated: 2026-06-22
#
# Goal: Measure TEXTUAL similarity (not ideology) between the Economic Survey
#       and the Budget Speech. The Survey is the Chief Economic Adviser's
#       technocratic diagnosis; the Budget Speech is the Finance Minister's
#       political announcement the next day. How much does the budget actually
#       echo the survey's language?
#
# Method: TF-IDF over a shared vocabulary across all Survey + Budget documents,
#         then cosine similarity between every Survey year and every Budget
#         year. The off-diagonal (Survey year vs OTHER budget years) is the
#         baseline: if the same-year cell is reliably the brightest, the budget
#         tracks its own contemporaneous survey rather than generic fiscal talk.
#
# DATA: full Economic Surveys (all chapters, English) scraped by G4 into
#   output/ecosurvey_full/{Y}.txt (~50k-300k words each). Survey label Y is the
#   survey released in Jan of year Y, which precedes Budget Y/Y+1.
#
# IN
#   output/ecosurvey_full/{Y}.txt
#   output/corpus_clean/<budget speech>_clean.txt
# OUT
#   output/tables/tab_es_bs_similarity.csv     -- full cosine matrix (long)
#   output/tables/tab_es_bs_topterms.csv       -- top shared terms, same-year
#   output/figures/fig_es_bs_similarity.png    -- cosine heatmap
# =============================================================================

suppressPackageStartupMessages({
  library(tidytext); library(dplyr); library(tidyr); library(stringr)
  library(readr); library(purrr); library(glue); library(Matrix); library(ggplot2)
})

root    <- "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
FULLDIR <- file.path(root, "output", "ecosurvey_full")   # full surveys (G4)
CLEAN   <- file.path(root, "output", "corpus_clean")
FIGDIR <- file.path(root, "output", "figures")
TABDIR <- file.path(root, "output", "tables")

# -- documents to compare ------------------------------------------------------
#{
# All full surveys available from G4 (label Y = survey released Jan of year Y)
es_years <- sort(as.integer(str_remove(
  list.files(FULLDIR, pattern = "^[0-9]{4}\\.txt$"), "\\.txt$")))

# Budget speech file map (fy_start -> clean file). The Feb budget of year Y is
# preceded by the survey released in Jan of year Y, so they share the same key.
bs_file_map <- c(
  "2019" = "bs201920_clean.txt",
  "2020" = "Budget_Speech_2020-21_clean.txt",
  "2021" = "Budget_Speech_2021-22_clean.txt",
  "2022" = "Budget_Speech_2022-23_clean.txt",
  "2023" = "Budget_Speech_2023-24_clean.txt",
  "2024" = "Budget_Speech_2024-25_clean.txt",
  "2025" = "Budget_Speech_2025-26_clean.txt",
  "2026" = "Budget_Speech_2026-27_clean.txt"
)

read_doc <- function(path) paste(readLines(path, warn = FALSE), collapse = " ")

# Survey docs (the rows of interest) -- full surveys from G4
survey_docs <- map_dfr(es_years, function(y) {
  p <- file.path(FULLDIR, glue("{y}.txt"))
  if (!file.exists(p)) return(NULL)
  tibble(doc_id = glue("ES_{y}"), kind = "Economic Survey", text = read_doc(p))
})

# ALL budget speeches (92 docs) -- used so IDF is estimated on a real corpus,
# not just the handful being compared. This keeps common economic vocabulary
# (growth, fiscal, investment) meaningfully weighted instead of zeroed out.
all_bs_files <- list.files(CLEAN, pattern = "_clean\\.txt$", full.names = TRUE)
budget_docs <- tibble(
  path   = all_bs_files,
  doc_id = tools::file_path_sans_ext(basename(all_bs_files))
) %>% mutate(kind = "Budget Speech", text = map_chr(path, read_doc)) %>% select(-path)

# Lookup: fy_start -> budget doc_id, for the recent years we plot
bs_lookup <- tibble(
  bs_year = as.integer(names(bs_file_map)),
  doc_id  = tools::file_path_sans_ext(bs_file_map)
) %>% filter(doc_id %in% budget_docs$doc_id)

docs <- bind_rows(survey_docs, budget_docs)
#}

# -- tokenize + TF-IDF with IDF estimated over the FULL corpus -----------------
#{
budget_stopwords <- c(
  "sir","madam","speaker","honourable","hon","ble","member","members","house",
  "rise","present","budget","speech","interim","survey","economic","rupee",
  "rupees","rs","crore","crores","lakh","lakhs","per","cent","year","years",
  "india","indian","government","central","national","union","therefore",
  "however","also","shall","will","may","must","total"
)
sw <- bind_rows(stop_words, tibble(word = budget_stopwords, lexicon = "custom")) %>%
  distinct(word)

tokens <- docs %>%
  unnest_tokens(word, text) %>%
  anti_join(sw, by = "word") %>%
  filter(nchar(word) >= 3, str_detect(word, "^[a-z][a-z'\\-]+$"))

tfidf <- tokens %>%
  count(doc_id, word, name = "n") %>%
  bind_tf_idf(word, doc_id, n)

build_cos <- function(valcol) {
  m <- tfidf %>% select(doc_id, word, val = all_of(valcol)) %>%
    cast_sparse(doc_id, word, val)
  m <- m / sqrt(rowSums(m^2))            # L2-normalize -> cosine via tcrossprod
  as.matrix(tcrossprod(m))
}
cos_tfidf <- build_cos("tf_idf")         # distinctive-vocabulary similarity
cos_tf    <- build_cos("tf")             # plain word-usage overlap
#}

# -- Survey x Budget cosine matrix (rows = surveys, cols = recent budgets) -----
#{
to_long <- function(cosm, wname) {
  es_ids <- grep("^ES_", rownames(cosm), value = TRUE)
  sub <- cosm[es_ids, bs_lookup$doc_id, drop = FALSE]
  colnames(sub) <- as.character(bs_lookup$bs_year[match(colnames(sub), bs_lookup$doc_id)])
  as.data.frame(sub) %>% tibble::rownames_to_column("es") %>%
    pivot_longer(-es, names_to = "bs_year", values_to = wname) %>%
    mutate(es_year = as.integer(str_remove(es, "ES_")), bs_year = as.integer(bs_year))
}
sim_long <- left_join(to_long(cos_tfidf, "cosine_tfidf"), to_long(cos_tf, "cosine_tf"),
                      by = c("es", "es_year", "bs_year")) %>%
  mutate(same_year = es_year == bs_year)

write_csv(sim_long, file.path(TABDIR, "tab_es_bs_similarity.csv"))

summ <- function(v) sprintf("same-year %.3f vs other-year %.3f  (lift %.2fx)",
                            mean(v[sim_long$same_year]), mean(v[!sim_long$same_year]),
                            mean(v[sim_long$same_year]) / mean(v[!sim_long$same_year]))
cat("\n=== Survey-vs-Budget cosine similarity ===\n")
cat("TF-IDF (distinctive vocab): ", summ(sim_long$cosine_tfidf), "\n")
cat("TF (overall word overlap):  ", summ(sim_long$cosine_tf), "\n")

cat("\nSame-year similarity by year:\n")
sim_long %>% filter(same_year) %>% arrange(es_year) %>%
  transmute(year = es_year, cosine_tfidf = round(cosine_tfidf, 3),
            cosine_tf = round(cosine_tf, 3)) %>% print()

cat("\nFor each Survey, the budget it most resembles (TF-IDF):\n")
sim_long %>% group_by(es_year) %>% slice_max(cosine_tfidf, n = 1) %>%
  transmute(survey = es_year, closest_budget = bs_year, cosine = round(cosine_tfidf, 3),
            hit = if_else(survey == closest_budget, "<- same year", "")) %>%
  arrange(survey) %>% print()
#}

# -- top shared terms for same-year pairs -------------------------------------
#{
# "Shared vocabulary" = words both use heavily: pmin of the two term proportions.
shared_terms <- map_dfr(es_years, function(y) {
  bid <- bs_lookup$doc_id[bs_lookup$bs_year == y]
  if (length(bid) == 0) return(NULL)
  ev <- tfidf %>% filter(doc_id == glue("ES_{y}")) %>% select(word, es_tf = tf)
  bv <- tfidf %>% filter(doc_id == bid)          %>% select(word, bs_tf = tf)
  inner_join(ev, bv, by = "word") %>%
    mutate(year = y, shared = pmin(es_tf, bs_tf)) %>%
    slice_max(shared, n = 12)
})
write_csv(shared_terms, file.path(TABDIR, "tab_es_bs_topterms.csv"))
cat("\nTop shared vocabulary (both use heavily), sample 2023:\n")
print(shared_terms %>% filter(year == 2023) %>% transmute(word, shared = round(shared, 4)))
#}

# -- heatmap ------------------------------------------------------------------
#{
p <- ggplot(sim_long, aes(x = factor(bs_year), y = factor(es_year), fill = cosine_tfidf)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", cosine_tfidf),
                fontface = if_else(same_year, "bold", "plain")),
            size = 3.1, colour = "#222") +
  geom_tile(data = ~ filter(.x, same_year), fill = NA,
            colour = "#b5310e", linewidth = 1.1) +
  scale_fill_gradient(low = "#f7f4ef", high = "#2a5e2a", name = "Cosine") +
  labs(
    title    = "How closely does each budget echo its Economic Survey?",
    subtitle = "TF-IDF cosine similarity, full Economic Survey (row) vs Budget Speech (column).\nRed outline = same year (survey released weeks before that budget).",
    x = "Budget speech year", y = "Economic Survey year",
    caption = "Higher = more shared distinctive vocabulary. Same-year cells outlined."
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank())

ggsave(file.path(FIGDIR, "fig_es_bs_similarity.png"), p, width = 8.5, height = 6, dpi = 150)
cat("\nSaved: fig_es_bs_similarity.png\n")
#}

cat("\nG3 complete.\n")
