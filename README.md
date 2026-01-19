# Greenfoot (ERSS) — Replication Package

This repository is the **replication package** for the paper:

> **[INSERT FULL PAPER TITLE]**  
> Benjamin Kirchler (and co-authors)  
> Submitted to / forthcoming in *Energy Research & Social Science* (ERSS)

It contains all code required to reproduce the descriptive statistics, figures, and main regression analyses reported in the paper, using the original survey data.

The repository follows a structured, reproducible research workflow and is based on a reusable project template (one repository = one paper). All outputs (tables/figures) are generated directly from code.

---

## How to reproduce the results

### Requirements
- **R** (and RStudio recommended)
- **Stata** (for the main regression analysis; version used: [INSERT])

### Step 1 — Run the R pipeline (data preparation + descriptives + figures)
1. Open the project in RStudio using `research-project-template.Rproj`
2. Run:

```r
source("masterfile.R")
