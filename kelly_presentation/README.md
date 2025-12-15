# Kelly Criterion Presentation - Beamer Format

## Overview
This package contains the Kelly Criterion replication study presentation in Quarto/Beamer format, matching Professor Zhuo's slide style.

## Format Details
- **Theme**: CambridgeUS (maroon header)
- **Color Theme**: spruce (green accents)
- **Aspect Ratio**: 16:9 (widescreen)
- **Font Size**: 10pt base (with \small, \footnotesize, \tiny used for dense slides)

## Files
```
kelly_beamer/
├── kelly_presentation.qmd    # Main Quarto file
├── figures/                  # All presentation images
│   ├── kelly_criterion_diagram.png
│   ├── monte_carlo_wealth_paths.png
│   ├── ... (33 total figures)
└── README.md                 # This file
```

## Rendering

### Prerequisites
1. [Quarto](https://quarto.org/docs/get-started/) installed
2. LaTeX distribution (TinyTeX, TeX Live, or MiKTeX)

### Install TinyTeX (if needed)
```bash
quarto install tinytex
```

### Render to PDF
```bash
cd kelly_beamer
quarto render kelly_presentation.qmd
```

This produces `kelly_presentation.pdf` - a Beamer PDF slideshow.

### Alternative: Render in RStudio
1. Open `kelly_presentation.qmd` in RStudio
2. Click the "Render" button

## Slide Count
32 slides total organized into sections:
- Introduction (6 slides)
- Methods (4 slides)  
- Monte Carlo Simulations (4 slides)
- In-Sample Backtests (8 slides)
- Out-of-Sample Analysis (7 slides)
- Conclusion (1 slide)

## Key Features
- 16:9 aspect ratio for modern projectors
- Consistent font sizing to prevent content cropping
- All figures sized to fit within slide boundaries
- Professor Zhuo's CambridgeUS/spruce theme

## Troubleshooting

### Content Overflow
If any slide content is cut off:
1. Reduce font size: Use `\tiny` or `\scriptsize` 
2. Reduce image height: Change `{height=70%}` to `{height=60%}`
3. Split slide into two slides

### LaTeX Errors
```bash
# Clean and rebuild
rm -rf _tex
quarto render kelly_presentation.qmd
```

### Missing Packages
```bash
tlmgr install beamer
tlmgr install pgf
```

---
**Authors**: Agna Chan, Syed Bashir Hydari, Pranav Kasibhatla, Aniqa Nayim  
**Course**: PS5846 Quantitative Risk Management  
**Date**: December 5, 2025
