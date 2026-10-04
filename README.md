# The Disclosure Environment of Publicly Listed Entities in Post-Soviet Economies


This repository contains the data, code, and manuscript for the Master’s thesis *“The Disclosure Environment of Publicly Listed Entities in Post-Soviet Economies: An Empirical Study Focusing on Non-Financial Reporting”*, submitted for the degree of Master of Science (M.Sc.) in Business Administration at the School of Business and Economics, Humboldt-Universität zu Berlin.

The thesis compares financial and ESG disclosure among publicly listed, non-financial firms in the Baltic states and Central Asia, using a hand-collected cross-section of regulatory and firm-level data for fiscal year 2024, and examines which regulatory and firm-level characteristics are associated with ESG disclosure in the two regions.

The repository’s workflow infrastructure is adapted from the [`trr266/treat`](https://github.com/trr266/treat) template for reproducible empirical research.

## Repository Content

- `config`: Configuration files called by the code in `code`, keeping file paths and settings separate from the code itself. See `config/global_cfg.yaml`.

- `code/R`: All R code for the project — preparing the hand-collected sample (`prepare_data.R`), running the descriptive and regression analysis (`do_analysis.R`), and shared setup/config-loading code (`utils.R`).

- `data`: Input and generated data.

  - `external`: The hand-collected source data (`sample_all_listed_firms.csv`, `country_characteristics.csv`). See `data/data_readme.md` and `data/external/external_data_README.md` for details.
  - `generated`: Data derived from `external` by `prepare_data.R`.

- `doc`: The Quarto source for the thesis itself (`paper.qmd`) and for the tables-and-figures document (`tables-and-figures.qmd`), plus supporting LaTeX fragments (`titlepage.tex`, `preamble.tex`, `declaration.tex`) and the bibliography (`references.bib`).

- `output`: Rendered PDF and the saved analysis results (`results.rda`).

## How do I run the workflow and create the output?

To run this workflow, you need R and Quarto installed, and the make and yq command-line tools available in your terminal.

From the repository root, run:

``` bash
make -f Makefile_R
```

This runs the full pipeline in order: prepares the sample (`code/R/prepare_data.R`), runs the analysis (`code/R/do_analysis.R`), renders the tables-and-figures document, and finally renders the paper itself.

When it finishes, you should find the final thesis document in `output/thesis_paper.pdf`.
