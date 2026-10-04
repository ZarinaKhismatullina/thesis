This directory holds the project's data, organized into two active subdirectories:

- `external`: The hand-collected source data this project is built on — 
  `sample_all_listed_firms.csv` (firm-level disclosure, regulatory, ownership, 
  and financial data) and `country_characteristics.csv` (World Bank Worldwide 
  Governance Indicators and GDP data). 
  
  See `data/external/external_data_README.md` for the full variable codebook.

- `generated`: Derivative data produced by `code/R/prepare_data.R` from the 
  files in `external` — currently `base_sample.parquet`, the cleaned and 
  variable-constructed sample used throughout the analysis.