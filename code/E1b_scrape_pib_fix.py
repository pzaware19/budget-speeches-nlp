"""
E1b_scrape_pib_fix.py
Author: Piyush Zaware
Last updated: 2026-06-17

Goal: Replace the failed PIB scraper with two better sources:
  1. PIB Finance Ministry budget-specific press releases — using PIB's
     proper search endpoint with correct POST parameters
  2. Economic Survey highlights — scraped from indiabudget.gov.in,
     the government's own pre-budget economic analysis (released each
     January before the February budget). This is the most authoritative
     source of what the government itself thought the economy needed.

OUT
  output/news/raw/pib_{year}.txt        -- overwrite with real PIB content
  output/news/raw/ecosurvey_{year}.txt  -- Economic Survey highlights
"""

import os
import re
import time
import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin

ROOT   = "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
RAWDIR = os.path.join(ROOT, "output", "news", "raw")
os.makedirs(RAWDIR, exist_ok=True)

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0.0.0 Safari/537.36"
    ),
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.5",
}

# =============================================================================
# SOURCE 1: PIB — using keyword search for budget-related releases
# PIB has a text search at https://pib.gov.in/allRel.aspx with keyword param
# =============================================================================

def scrape_pib_keyword(year):
    """
    Search PIB for Finance Ministry press releases mentioning 'budget'
    around the given year. Uses keyword search which is more reliable
    than date filtering on their site.
    """
    keywords = ["union budget", "economic survey", "fiscal", "revenue deficit",
                "capital expenditure", "direct tax", "indirect tax"]

    all_texts = []
    for kw in keywords[:3]:   # cap to avoid hammering
        url = f"https://pib.gov.in/allRel.aspx?reg=3&lang=1&kw={kw.replace(' ', '+')}"
        try:
            r = requests.get(url, headers=HEADERS, timeout=15)
            if r.status_code != 200:
                continue
            soup = BeautifulSoup(r.text, "html.parser")
            # Extract text from result rows that mention the year
            rows = soup.find_all(["div", "p", "td"], string=re.compile(str(year)))
            for row in rows[:5]:
                txt = row.get_text(strip=True)
                if len(txt) > 50:
                    all_texts.append(txt)
            time.sleep(1)
        except Exception as e:
            print(f"  PIB keyword search error: {e}")

    return "\n\n".join(all_texts) if all_texts else ""


# =============================================================================
# SOURCE 1 (BETTER): Scrape PIB budget press releases via their press release
# archive pages which have stable year-wise URLs
# =============================================================================

PIB_BUDGET_SEARCHES = {
    2019: "https://pib.gov.in/newsite/archieveReleases.aspx?menuid=3&submenuid=0",
    2020: "https://pib.gov.in/allRel.aspx?reg=3&lang=1&kw=budget+2020",
    2021: "https://pib.gov.in/allRel.aspx?reg=3&lang=1&kw=budget+2021",
    2022: "https://pib.gov.in/allRel.aspx?reg=3&lang=1&kw=budget+2022",
    2023: "https://pib.gov.in/allRel.aspx?reg=3&lang=1&kw=budget+2023",
    2024: "https://pib.gov.in/allRel.aspx?reg=3&lang=1&kw=budget+2024",
    2025: "https://pib.gov.in/allRel.aspx?reg=3&lang=1&kw=budget+2025",
    2026: "https://pib.gov.in/allRel.aspx?reg=3&lang=1&kw=budget+2026",
}

def scrape_pib_budget_page(year):
    url = PIB_BUDGET_SEARCHES.get(year)
    if not url:
        return ""
    try:
        r = requests.get(url, headers=HEADERS, timeout=15)
        if r.status_code != 200:
            return ""
        soup = BeautifulSoup(r.text, "html.parser")
        # Get all text from result items
        items = soup.find_all(class_=re.compile(r"content|result|release|item", re.I))
        if not items:
            # fallback: grab all paragraphs
            items = soup.find_all("p")
        texts = [i.get_text(separator=" ", strip=True) for i in items
                 if len(i.get_text()) > 80]
        return "\n\n".join(texts[:30])
    except Exception as e:
        print(f"  Error: {e}")
        return ""


# =============================================================================
# SOURCE 2: Economic Survey highlights from indiabudget.gov.in
# The Economic Survey is published every January and is the government's
# own assessment of the economy going into the budget.
# =============================================================================

ECOSURVEY_URLS = {
    2019: "https://www.indiabudget.gov.in/economicsurvey/",
    2020: "https://www.indiabudget.gov.in/economicsurvey/",
    2021: "https://www.indiabudget.gov.in/economicsurvey/",
    2022: "https://www.indiabudget.gov.in/economicsurvey/",
    2023: "https://www.indiabudget.gov.in/economicsurvey/",
    2024: "https://www.indiabudget.gov.in/economicsurvey/",
    2025: "https://www.indiabudget.gov.in/economicsurvey/",
    2026: "https://www.indiabudget.gov.in/economicsurvey/",
}

# Economic Survey chapter summary pages (more reliably text-based)
ECOSURVEY_SUMMARY_URLS = {
    2020: "https://www.indiabudget.gov.in/economicsurvey/doc/eschapter/echap01.pdf",
    2021: "https://www.indiabudget.gov.in/economicsurvey/",
    2022: "https://www.indiabudget.gov.in/economicsurvey/",
    2023: "https://www.indiabudget.gov.in/economicsurvey/",
    2024: "https://www.indiabudget.gov.in/economicsurvey/",
    2025: "https://www.indiabudget.gov.in/economicsurvey/",
}

# PIB Economic Survey press releases — these are text-based and reliable
ECOSURVEY_PIB = {
    2020: "https://pib.gov.in/PressReleasePage.aspx?PRID=1601162",
    2021: "https://pib.gov.in/PressReleasePage.aspx?PRID=1693900",
    2022: "https://pib.gov.in/PressReleasePage.aspx?PRID=1794328",
    2023: "https://pib.gov.in/PressReleasePage.aspx?PRID=1895790",
    2024: "https://pib.gov.in/PressReleasePage.aspx?PRID=2002037",
    2025: "https://pib.gov.in/PressReleasePage.aspx?PRID=2090901",
}

def scrape_ecosurvey_pib(year):
    """Scrape the PIB press release for Economic Survey — these are clean HTML."""
    url = ECOSURVEY_PIB.get(year)
    if not url:
        print(f"  [EcoSurvey {year}] No PIB URL available")
        return ""

    print(f"  [EcoSurvey {year}] Fetching: {url}")
    try:
        r = requests.get(url, headers=HEADERS, timeout=15)
        if r.status_code != 200:
            print(f"    HTTP {r.status_code}")
            return ""
        soup = BeautifulSoup(r.text, "html.parser")
        body = (
            soup.find(id="ContentPlaceHolder1_lblPRContents") or
            soup.find("div", class_="innerpagecontainer") or
            soup.find("article")
        )
        if body:
            return body.get_text(separator=" ", strip=True)
        return soup.get_text(separator=" ", strip=True)[:8000]
    except Exception as e:
        print(f"    Error: {e}")
        return ""


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    print("=" * 60)
    print("E1b: Fixing PIB + scraping Economic Survey highlights")
    print("=" * 60)

    years = [2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026]

    # Fix PIB files with keyword search
    print("\n--- PIB keyword search ---")
    for year in years:
        print(f"\n[PIB {year}]")
        text = scrape_pib_budget_page(year)
        if len(text) > 200:
            out = os.path.join(RAWDIR, f"pib_{year}.txt")
            with open(out, "w", encoding="utf-8") as f:
                f.write(text)
            print(f"  Saved {len(text.split())} words")
        else:
            print(f"  Too little content ({len(text)} chars) — keeping original")
        time.sleep(1.2)

    # Economic Survey press releases from PIB
    print("\n--- Economic Survey PIB press releases ---")
    for year in sorted(ECOSURVEY_PIB.keys()):
        text = scrape_ecosurvey_pib(year)
        out  = os.path.join(RAWDIR, f"ecosurvey_{year}.txt")
        if len(text) > 300:
            with open(out, "w", encoding="utf-8") as f:
                f.write(text)
            print(f"  Saved {len(text.split())} words -> ecosurvey_{year}.txt")
        else:
            print(f"  Insufficient content for {year}")
        time.sleep(1.5)

    print("\nDone.")
    print("Files in output/news/raw/:")
    for f in sorted(os.listdir(RAWDIR)):
        path = os.path.join(RAWDIR, f)
        wc   = len(open(path, encoding="utf-8", errors="ignore").read().split())
        print(f"  {f}: {wc} words")
