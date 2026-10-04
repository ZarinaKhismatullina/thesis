Two hand-collected external data files underlie this project. Both are plain CSV, first row = variable names.

#### `sample_all_listed_firms.csv`

Firm-level sample of publicly listed, non-financial firms across eight post-Soviet countries, fiscal year 2024.

### Identifiers

| Variable | Description |
|----|----|
| `ticker` | Exchange ticker symbol. |
| `company_name` | Legal/trading name of the firm. |
| `isin` | International Securities Identification Number. |
| `alterntive_ticker_ISIN` | Secondary ticker or ISIN, for firms with more than one listing. |
| `national_identifier` | Company registration number from the national business register. |

### Classification and location

| Variable | Description |
|----|----|
| `category` | Market segment at the firm's exchange (e.g. KASE's "Main" vs. "Alternative" market). |
| `type` | Type of instrument listed. |
| `icb_industry` | Industry Classification Benchmark (ICB) industry, assigned via AI-assisted classification per the thesis methodology. Drives `sensitive_industry` (Energy, Basic Materials, Utilities = 1). |
| `activity` | Free-text description of the firm's primary business activity. |
| `exchange` | Stock exchange code(s) the firm is listed on (e.g. `KASE`, `AIX`, `UZSE`, `KSE`, `BTS`, `AGB`, `CASE`, `TLN`, `RIG`, `VLN`); dual-listed firms may have more than one code. |
| `country` | Firm's home country. |
| `region` | Region grouping. |

### Time

| Variable | Description |
|----|----|
| `year` | Fiscal year of the observation (FY2024 throughout the current sample). |
| `listed_since` | Year the firm was first listed; used to filter to firms listed as of FY2024. |

### Disclosure behaviour

| Variable | Description |
|----|----|
| `annual_report` | 1 if the firm publishes an integrated annual report, 0 otherwise. |
| `ESG_info_annual_report` | 1 if the annual report contains ESG-related information. |
| `ESG_separate_report` | 1 if the firm publishes a standalone ESG/sustainability report. |
| `annual_fin_report` | 1 if the firm discloses any annual financial report. |
| `interim_fin_reports` | 1 if the firm discloses any interim (quarterly/semi-annual) financial report. |

### International exposure

| Variable | Description |
|----|----|
| `cross_listed` | 1 if the firm is cross-listed on another exchange. |
| `cross_listed_since` | Year of cross-listing, if applicable. |
| `un_global_compact_joined` | 1 if the firm has joined the UN Global Compact. |
| `orbis_data` | Flag indicating whether a firm is covered by Orbis. |
| `lseg_esg_data` | Flag indicating whether a firm is covered by LSEG ESG. |

### Soviet-era origin (three-source triangulation)

| Variable | Description |
|----|----|
| `year_founded_manual` | Hand-collected founding year, from national business registers/company documents. |
| `year_incorporated_orbis` | Incorporation year as reported by Orbis. |
| `soviet_era_ai_checked` | 1 if an AI-assisted historical trace was performed for this firm. |
| `soviet_era_ai_result` | 1/0 result of that AI-assisted check (only meaningful when `soviet_era_ai_checked` = 1). |

### Financials

| Variable | Description |
|----|----|
| `total_assets` | Total assets, original reporting currency. |
| `net_income` | Net income, original reporting currency. |
| `currency` | Original reporting currency code. |
| `fx_rate_to_eur` | Exchange rate used to convert to EUR. |
| `source_fx_rate` | Source for the FX rate used. |
| `number_of_employees` | Headcount, used for the Baltic \>500-employee CSRD/NFRD applicability threshold. |

### Ownership

| Variable | Description |
|----|----|
| `state_ownership` | 1 if a state body holds an ownership stake at or above the country-specific threshold. |
| `state_ownership_threshold` | Free-text note on the state-ownership determination at this firm's applicable disclosure threshold, recording the count of state-linked shareholders identified. |
| `foreign_ownership` | 1 if a foreign (non-domestic) shareholder holds a stake at or above the threshold. |
| `foreign_ownership_threshold` | Free-text note on the foreign-ownership determination at this firm's applicable disclosure threshold, recording the count of foreign shareholders identified. |
| `individual_ownership` | 1 if an individual shareholder holds a stake at or above the threshold. |
| `individual_ownership_threshold` | Free-text note on the individual-ownership determination at this firm's applicable disclosure threshold, recording the count of individual shareholders identified. |
| `as_of_date` | Date the ownership snapshot is current as of. |

### Documentation / sourcing notes

| Variable | Description |
|----|----|
| `source_reports` | Citation/URL for where the disclosure-report columns above were verified. |
| `notes_reports` | Free-text notes accompanying `source_reports`. |
| `source_ownership` | Citation/source for the ownership data. |
| `notes_ownership` | Free-text notes accompanying `source_ownership`. |

#### `country_characteristics.csv`

Country-year panel of World Bank indicators.

| Variable | Description |
|----|----|
| `Time` / `Time Code` | Year of the observation / World Bank's year code. |
| `Country Name` / `Country Code` | Country name and ISO3 code. |
| `GDP per capita, PPP (current international $)` | GDP per capita, PPP-adjusted. World Development Indicators - <https://databank.worldbank.org/source/world-development-indicators>. Last Updated: 07/01/2026 |
| `GDP per capita growth (annual %)` | Annual GDP per capita growth rate. World Development Indicators - <https://databank.worldbank.org/source/world-development-indicators>. Last Updated: 07/01/2026 |
| `Control of Corruption` / `Government Effectiveness` / `Political Stability` / `Regulatory Quality` / `Rule of Law` / `Voice and Accountability` — Governance estimate (approx. -2.5 to +2.5) | The six World Bank Worldwide Governance Indicators (WGI). `Rule of Law` is the indicator carried into the firm-level regression analysis; the other five describe institutional context. Worldwide Governance Indicators - <https://databank.worldbank.org/source/worldwide-governance-indicators#>. Last updated: 03/18/2026 |
