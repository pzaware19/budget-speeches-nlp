"""
E1_scrape_news.py
Author: Piyush Zaware
Last updated: 2026-06-17

Goal: Scrape pre-budget news and analysis articles for each of Nirmala
      Sitharaman's budget years from two sources:
        1. PIB (Press Information Bureau) — Finance Ministry press releases
           in the 60-day window before each budget presentation
        2. PRS Legislative Research — post-budget sector analyses, one page
           per year, the most detailed independent structured analysis available

Both sources are free, no paywall, and have stable URL patterns.

IN
  None (scrapes from web)

OUT
  output/news/raw/pib_{year}.txt       -- concatenated PIB press releases
  output/news/raw/prs_{year}.txt       -- PRS budget analysis text
  output/news/scrape_log.csv           -- what was found / failed
"""

import os
import re
import time
import csv
import requests
from bs4 import BeautifulSoup
from datetime import datetime, timedelta
from urllib.parse import urljoin

ROOT    = "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
OUTDIR  = os.path.join(ROOT, "output", "news", "raw")
os.makedirs(OUTDIR, exist_ok=True)

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0.0.0 Safari/537.36"
    )
}

# Nirmala Sitharaman budget presentation dates
BUDGET_DATES = {
    2019: datetime(2019, 7,  5),   # first budget (July, unusual)
    2020: datetime(2020, 2,  1),
    2021: datetime(2021, 2,  1),
    2022: datetime(2022, 2,  1),
    2023: datetime(2023, 2,  1),
    2024: datetime(2024, 2,  1),
    2025: datetime(2025, 2,  1),
    2026: datetime(2026, 2,  1),
}

PRE_BUDGET_DAYS = 60   # scrape this many days before each budget date

log_rows = []


# =============================================================================
# SOURCE 1: PIB — Finance Ministry press releases
# =============================================================================

def pib_search_url(from_date, to_date):
    """Build PIB allRel search URL for Finance Ministry (MinCode=18)."""
    fd = from_date.strftime("%d/%m/%Y")
    td = to_date.strftime("%d/%m/%Y")
    return (
        f"https://pib.gov.in/allRel.aspx"
        f"?reg=3&lang=1&strdate={fd}&enddate={td}&MinCode=18"
    )


def fetch_pib_release_text(url):
    """Fetch the full text of a single PIB press release."""
    try:
        r = requests.get(url, headers=HEADERS, timeout=15)
        if r.status_code != 200:
            return ""
        soup = BeautifulSoup(r.text, "html.parser")
        # PIB release body is in div with id ContentPlaceHolder1_lblPRContents
        body = soup.find(id="ContentPlaceHolder1_lblPRContents")
        if body:
            return body.get_text(separator=" ", strip=True)
        # fallback: any large paragraph block
        paras = soup.find_all("p")
        return " ".join(p.get_text(strip=True) for p in paras if len(p.get_text()) > 80)
    except Exception as e:
        return ""


def scrape_pib_year(fy_start):
    """Scrape PIB Finance Ministry releases in the 60-day pre-budget window."""
    bdate    = BUDGET_DATES[fy_start]
    from_dt  = bdate - timedelta(days=PRE_BUDGET_DAYS)
    to_dt    = bdate + timedelta(days=1)

    search_url = pib_search_url(from_dt, to_dt)
    print(f"\n[PIB {fy_start}] Searching: {from_dt.date()} to {to_dt.date()}")

    try:
        r = requests.get(search_url, headers=HEADERS, timeout=20)
        if r.status_code != 200:
            print(f"  HTTP {r.status_code} — skipping")
            log_rows.append({"source": "PIB", "year": fy_start,
                             "status": f"HTTP {r.status_code}", "n_articles": 0})
            return
    except Exception as e:
        print(f"  Error: {e}")
        log_rows.append({"source": "PIB", "year": fy_start,
                         "status": str(e), "n_articles": 0})
        return

    soup = BeautifulSoup(r.text, "html.parser")

    # Extract all release links from the results table
    release_links = []
    for a in soup.find_all("a", href=True):
        href = a["href"]
        if "PressReleseDetail" in href or "PressReleasePage" in href:
            full_url = urljoin("https://pib.gov.in/", href)
            if full_url not in release_links:
                release_links.append(full_url)

    print(f"  Found {len(release_links)} release links")

    texts = []
    for i, url in enumerate(release_links[:40]):   # cap at 40 per year
        text = fetch_pib_release_text(url)
        if len(text) > 200:
            texts.append(text)
        time.sleep(0.8)

    combined = "\n\n---\n\n".join(texts)
    out_path  = os.path.join(OUTDIR, f"pib_{fy_start}.txt")
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(combined)

    n = len(texts)
    print(f"  Saved {n} releases -> {out_path}")
    log_rows.append({"source": "PIB", "year": fy_start,
                     "status": "ok", "n_articles": n})


# =============================================================================
# SOURCE 2: PRS Legislative Research — budget analysis pages
# =============================================================================

# PRS publishes one comprehensive budget analysis per year.
# URL pattern (stable since 2018):
# https://prsindia.org/budgets/parliament/union-budget-{YYYY-YY}-analysis

PRS_URLS = {
    2019: "https://prsindia.org/budgets/parliament/union-budget-2019-20-analysis",
    2020: "https://prsindia.org/budgets/parliament/union-budget-2020-21-analysis",
    2021: "https://prsindia.org/budgets/parliament/union-budget-2021-22-analysis",
    2022: "https://prsindia.org/budgets/parliament/union-budget-2022-23-analysis",
    2023: "https://prsindia.org/budgets/parliament/union-budget-2023-24-analysis",
    2024: "https://prsindia.org/budgets/parliament/union-budget-2024-25-analysis",
    2025: "https://prsindia.org/budgets/parliament/union-budget-2025-26-analysis",
    2026: "https://prsindia.org/budgets/parliament/union-budget-2026-27-analysis",
}

# Fallback: PRS sometimes uses slightly different slugs
PRS_FALLBACKS = {
    2019: "https://prsindia.org/budgets/parliament/demand-grants-2019-20",
    2026: "https://prsindia.org/budgets/parliament/union-budget-2026-27",
}


def scrape_prs_year(fy_start):
    """Scrape PRS Legislative Research budget analysis for one year."""
    url = PRS_URLS.get(fy_start)
    if not url:
        print(f"\n[PRS {fy_start}] No URL defined — skipping")
        return

    print(f"\n[PRS {fy_start}] Fetching: {url}")

    def try_fetch(u):
        r = requests.get(u, headers=HEADERS, timeout=20)
        return r if r.status_code == 200 else None

    r = try_fetch(url)
    if r is None and fy_start in PRS_FALLBACKS:
        print(f"  Primary URL failed, trying fallback...")
        r = try_fetch(PRS_FALLBACKS[fy_start])

    if r is None:
        print(f"  Could not fetch PRS page — skipping")
        log_rows.append({"source": "PRS", "year": fy_start,
                         "status": "HTTP error", "n_articles": 0})
        return

    soup = BeautifulSoup(r.text, "html.parser")

    # PRS pages: main content in article or div.field-items or div.view-content
    content_divs = (
        soup.find("article") or
        soup.find("div", class_="field-items") or
        soup.find("div", class_=re.compile(r"content|body|main", re.I))
    )

    if content_divs:
        raw_text = content_divs.get_text(separator="\n", strip=True)
    else:
        # fallback: all paragraphs
        paras = soup.find_all("p")
        raw_text = "\n".join(p.get_text(strip=True) for p in paras
                             if len(p.get_text()) > 60)

    # Clean up whitespace
    lines     = [l.strip() for l in raw_text.splitlines() if l.strip()]
    clean     = "\n".join(lines)

    out_path  = os.path.join(OUTDIR, f"prs_{fy_start}.txt")
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(clean)

    words = len(clean.split())
    print(f"  Saved {words} words -> {out_path}")
    log_rows.append({"source": "PRS", "year": fy_start,
                     "status": "ok", "n_articles": 1, "words": words})

    time.sleep(1.2)


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    print("=" * 60)
    print("E1: Scraping pre-budget news — PIB + PRS")
    print("=" * 60)

    for year in sorted(BUDGET_DATES.keys()):
        scrape_pib_year(year)
        time.sleep(1.5)

    print("\n" + "=" * 60)
    print("PRS Legislative Research analyses")
    print("=" * 60)

    for year in sorted(PRS_URLS.keys()):
        scrape_prs_year(year)

    # Write log
    log_path = os.path.join(ROOT, "output", "news", "scrape_log.csv")
    with open(log_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["source", "year", "status",
                                                "n_articles", "words"])
        writer.writeheader()
        for row in log_rows:
            if "words" not in row:
                row["words"] = ""
            writer.writerow(row)

    print(f"\nLog saved: {log_path}")
    print("\nDone. Check output/news/raw/ for scraped files.")
