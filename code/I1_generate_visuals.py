"""
I1_generate_visuals.py
Author: Piyush Zaware
Last updated: 2026-06-17

Generates professional decorative images for the Budget Speeches website:

  1. fig_hero_wordcloud.png  -- full-corpus word cloud, saffron/navy palette,
                                used as the page hero background
  2. fig_era_wordclouds.png  -- 2x2 grid of era-specific word clouds
                                (Nehruvian, License Raj, Liberalisation, BJP era)
  3. fig_ideology_art.png    -- stylised scatter of all budgets in ideology space,
                                coloured by decade, used as a section hero

IN
  output/corpus_clean/       -- all clean speech texts
  output/dtm/ideology_scores.csv

OUT
  output/figures/fig_hero_wordcloud.png
  output/figures/fig_era_wordclouds.png
  output/figures/fig_ideology_art.png
"""

import os, re, glob
from collections import Counter
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patheffects as pe
from matplotlib.colors import LinearSegmentedColormap
from wordcloud import WordCloud, STOPWORDS

ROOT    = "/Users/piyushzaware/Documents/Unsupervised ML/Budget_Speeches"
CLEAN   = os.path.join(ROOT, "output", "corpus_clean")
FIGDIR  = os.path.join(ROOT, "output", "figures")
DTMDIR  = os.path.join(ROOT, "output", "dtm")

# ── Custom stopwords ─────────────────────────────────────────────────────────
BUDGET_STOPS = {
    "sir","madam","speaker","honourable","hon","ble","member","members","house",
    "august","rise","present","budget","speech","interim","rupee","rupees","rs",
    "crore","crores","lakh","lakhs","per","cent","year","years","india","indian",
    "government","central","national","union","therefore","however","also","well",
    "shall","will","may","must","total","would","could","one","two","three",
    "scheme","schemes","new","during","under","plan","programme","programmes",
    "provide","provided","made","increased","increased","increased","billion",
    "million","thousand","hundred","propose","proposed","proposals","including",
    "based","take","make","need","number","additional","further","within",
    "across","per","last","next","full","part","first","second","third",
    "mr","ve","th","st","nd","rd","fy","ie","eg",
}
ALL_STOPS = STOPWORDS | BUDGET_STOPS

def load_era(fy_start_min, fy_start_max):
    """Concatenate all clean texts for speeches in the given fy_start range."""
    texts = []
    for fp in sorted(glob.glob(os.path.join(CLEAN, "*_clean.txt"))):
        fname = os.path.basename(fp)
        # Parse fy_start from filename
        m = re.search(r"bs(\d{4})", fname)
        if m:
            fy = int(m.group(1))
        else:
            m2 = re.search(r"(\d{4})-(\d{2})", fname)
            if m2:
                fy = int(m2.group(1))
            else:
                continue
        if fy_start_min <= fy <= fy_start_max:
            with open(fp, encoding="utf-8", errors="ignore") as f:
                texts.append(f.read())
    return " ".join(texts)

def load_all():
    texts = []
    for fp in sorted(glob.glob(os.path.join(CLEAN, "*_clean.txt"))):
        with open(fp, encoding="utf-8", errors="ignore") as f:
            texts.append(f.read())
    return " ".join(texts)

# ── Colour functions ──────────────────────────────────────────────────────────
def saffron_navy_colour(word, font_size, position, orientation, random_state=None, **kwargs):
    """Returns words in saffron (BJP) or navy (INC) or dark teal tones."""
    palette = [
        "#E07820",  # saffron
        "#1A3A6B",  # deep navy
        "#C0392B",  # crimson
        "#2C7873",  # teal
        "#8B1A1A",  # dark red
        "#1a6bb5",  # medium blue
        "#5D4037",  # brown
    ]
    idx = hash(word) % len(palette)
    return palette[idx]

def green_palette(word, font_size, position, orientation, random_state=None, **kwargs):
    rng = random_state if random_state is not None else np.random.RandomState()
    greens = ["#1B5E20","#2E7D32","#388E3C","#43A047","#66BB6A","#A5D6A7","#C8E6C9"]
    return greens[rng.randint(0, len(greens))]

# ── FIGURE 1: Hero word cloud ─────────────────────────────────────────────────
print("Generating hero word cloud...")
all_text = load_all()

wc_hero = WordCloud(
    width=2400, height=900,
    background_color="#0D1B2A",
    stopwords=ALL_STOPS,
    max_words=300,
    collocations=False,
    color_func=saffron_navy_colour,
    font_path=None,
    prefer_horizontal=0.85,
    min_font_size=10,
    max_font_size=180,
    relative_scaling=0.5,
    margin=4,
).generate(all_text)

fig, ax = plt.subplots(figsize=(24, 9), facecolor="#0D1B2A")
ax.imshow(wc_hero, interpolation="bilinear")
ax.axis("off")
plt.tight_layout(pad=0)
fig.savefig(os.path.join(FIGDIR, "fig_hero_wordcloud.png"),
            dpi=150, bbox_inches="tight", facecolor="#0D1B2A")
plt.close()
print("Saved: fig_hero_wordcloud.png")

# ── FIGURE 2: Era word clouds (2×2 grid) ─────────────────────────────────────
print("Generating era word clouds...")
eras = [
    ("Nehruvian Era\n(1947–1964)", 1947, 1964,  "#1B2A3B", "#E07820"),
    ("License Raj\n(1965–1990)",  1965, 1990,  "#1B2A3B", "#C0392B"),
    ("Liberalisation\n(1991–2013)", 1991, 2013, "#0D1B2A", "#2C7873"),
    ("Modi Era\n(2014–2026)",      2014, 2026,  "#0D1B2A", "#1A3A6B"),
]

def make_era_colour(accent):
    r2 = int(accent[1:3], 16)
    g2 = int(accent[3:5], 16)
    b2 = int(accent[5:7], 16)
    def _colour(word, font_size, position, orientation, random_state=None, **kwargs):
        # Use word hash for deterministic but varied colour
        t = (hash(word) % 100) / 100.0
        r1, g1, b1 = 0xFF, 0xFF, 0xFF
        r  = int(r1 + t * (r2 - r1))
        g  = int(g1 + t * (g2 - g1))
        b  = int(b1 + t * (b2 - b1))
        return f"#{r:02X}{g:02X}{b:02X}"
    return _colour

fig, axes = plt.subplots(2, 2, figsize=(20, 12),
                          facecolor="#111827")
plt.subplots_adjust(hspace=0.04, wspace=0.04)

for ax, (label, y0, y1, bg, accent) in zip(axes.flat, eras):
    text = load_era(y0, y1)
    if not text.strip():
        ax.set_visible(False)
        continue
    wc = WordCloud(
        width=960, height=540,
        background_color=bg,
        stopwords=ALL_STOPS,
        max_words=150,
        collocations=False,
        color_func=make_era_colour(accent),
        prefer_horizontal=0.8,
        min_font_size=8,
        max_font_size=120,
        relative_scaling=0.45,
        margin=3,
    ).generate(text)
    ax.imshow(wc, interpolation="bilinear")
    ax.axis("off")
    ax.text(0.03, 0.97, label, transform=ax.transAxes,
            color="white", fontsize=14, fontweight="bold",
            va="top", ha="left",
            path_effects=[pe.withStroke(linewidth=3, foreground="black")])

fig.savefig(os.path.join(FIGDIR, "fig_era_wordclouds.png"),
            dpi=150, bbox_inches="tight", facecolor="#111827")
plt.close()
print("Saved: fig_era_wordclouds.png")

# ── FIGURE 3: Ideology art ────────────────────────────────────────────────────
print("Generating ideology art...")
ideo = pd.read_csv(os.path.join(DTMDIR, "ideology_scores.csv"))
ideo = ideo.dropna(subset=["fy_start","axis_market","axis_nationalist"])
ideo = ideo[ideo["budget_type"].isin(["full","special"])]

# Decade bins for colour
ideo["decade"] = (ideo["fy_start"] // 10) * 10

decade_palette = {
    1940: "#8B1A1A", 1950: "#C0392B", 1960: "#E07820",
    1970: "#F39C12", 1980: "#2ECC71", 1990: "#1ABC9C",
    2000: "#1A3A6B", 2010: "#2980B9", 2020: "#8E44AD",
}

fig, ax = plt.subplots(figsize=(14, 10), facecolor="#0D1B2A")
ax.set_facecolor("#0D1B2A")

# Light grid
ax.axhline(0, color="#2a3f5f", linewidth=0.8, zorder=1)
ax.axvline(0, color="#2a3f5f", linewidth=0.8, zorder=1)
ax.grid(True, color="#152035", linewidth=0.4, zorder=0)

# Scatter by decade
for decade, grp in ideo.groupby("decade"):
    col = decade_palette.get(decade, "#aaaaaa")
    ax.scatter(grp["axis_market"], grp["axis_nationalist"],
               color=col, s=80, alpha=0.85, edgecolors="white",
               linewidths=0.5, zorder=3, label=f"{decade}s")

# Label notable speeches
notable = {
    1991: "Manmohan Singh\n(liberalisation)",
    2004: "Chidambaram\n(UPA-I)",
    2019: "Sitharaman I",
    1947: "Shanmukham\n(first budget)",
    1998: "Yashwant Sinha\n(BJP first)",
}
for _, row in ideo.iterrows():
    fy = int(row["fy_start"])
    if fy in notable:
        ax.annotate(notable[fy],
                    xy=(row["axis_market"], row["axis_nationalist"]),
                    xytext=(12, 8), textcoords="offset points",
                    color="white", fontsize=7.5, alpha=0.9,
                    arrowprops=dict(arrowstyle="-", color="#aaaaaa",
                                    lw=0.8, alpha=0.7))

ax.set_xlabel("Market-liberal axis  (capitalist words – socialist words)",
              color="#cccccc", fontsize=11)
ax.set_ylabel("Nationalist axis  (protectionist words – globalist words)",
              color="#cccccc", fontsize=11)
ax.tick_params(colors="#888888")
for spine in ax.spines.values():
    spine.set_edgecolor("#2a3f5f")

legend = ax.legend(title="Decade", title_fontsize=9,
                   fontsize=8, loc="upper right",
                   facecolor="#152035", edgecolor="#2a3f5f",
                   labelcolor="white")
legend.get_title().set_color("white")

ax.set_title("76 Years of Indian Fiscal Ideology (1947–2026)",
             color="white", fontsize=15, fontweight="bold", pad=14)

fig.savefig(os.path.join(FIGDIR, "fig_ideology_art.png"),
            dpi=150, bbox_inches="tight", facecolor="#0D1B2A")
plt.close()
print("Saved: fig_ideology_art.png")
print("\nI1 complete.")
