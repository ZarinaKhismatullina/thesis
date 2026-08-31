# ------------------------------------------------------------------------------
# Cleans the manually collected sample of listed firms, constructs analysis
# variables for the ESG disclosure study, and prepares final sample for analysis.
# ------------------------------------------------------------------------------

source("code/R/utils.R")


# --- Read raw sample ----------------------------------------------------------

sample_all_listed <- read_csv(
  global_cfg$sample_all_listed,
  na = character(0),
  col_types = cols()
)

names(sample_all_listed) <- trimws(names(sample_all_listed))

log_info("Loaded raw sample: {nrow(sample_all_listed)} rows, {ncol(sample_all_listed)} columns.")


# --- ISO country code ---------------------------------------------------------

sample_all_listed <- sample_all_listed %>%
  mutate(
    country_iso3 = case_when(
      country == "Kazakhstan"   ~ "KAZ",
      country == "Uzbekistan"   ~ "UZB",
      country == "Kyrgyzstan"   ~ "KGZ",
      country == "Tajikistan"   ~ "TJK",
      country == "Turkmenistan" ~ "TKM",
      country == "Estonia"      ~ "EST",
      country == "Latvia"       ~ "LVA",
      country == "Lithuania"    ~ "LTU",
      TRUE ~ NA_character_
    )
  )


# --- keep firms listed as of 2024 ---------------------------------------------

sample_all_listed <- sample_all_listed %>%
  mutate(listed_since_num = suppressWarnings(as.integer(listed_since)))

n_unclear <- sum(is.na(sample_all_listed$listed_since_num))

sample_all_listed <- sample_all_listed %>%
  filter(!is.na(listed_since_num) & listed_since_num <= 2024) %>%
  select(-listed_since_num)

log_info(
  "Eligibility filter (listed as of 2024): {nrow(sample_all_listed)} rows remain ",
  "({n_unclear} dropped for unclear/blank listed_since)."
)


# --- sensitive_industry -------------------------------------------------------

sensitive_industries <- c("Energy", "Basic Materials", "Utilities")

sample_all_listed <- sample_all_listed %>%
  mutate(
    sensitive_industry = case_when(
      icb_industry %in% c(NA, "", "NA", "N/A") ~ NA_real_,
      icb_industry %in% sensitive_industries    ~ 1,
      TRUE                                      ~ 0
    )
  )

log_info(
  "sensitive_industry: {sum(sample_all_listed$sensitive_industry == 1, na.rm = TRUE)} sensitive, ",
  "{sum(sample_all_listed$sensitive_industry == 0, na.rm = TRUE)} not sensitive, ",
  "{sum(is.na(sample_all_listed$sensitive_industry))} NA (missing icb_industry)."
)


# --- soviet_era ---------------------------------------------------------------

sample_all_listed <- sample_all_listed %>%
  mutate(
    year_founded_num = suppressWarnings(as.integer(year_founded)),
    soviet_era = case_when(
      is.na(year_founded_num) ~ NA_real_,
      year_founded_num < 1992 ~ 1,
      TRUE                    ~ 0
    )
  ) %>%
  select(-year_founded_num)

log_info(
  "soviet_era: {sum(sample_all_listed$soviet_era == 1, na.rm = TRUE)} Soviet-era, ",
  "{sum(sample_all_listed$soviet_era == 0, na.rm = TRUE)} post-Soviet, ",
  "{sum(is.na(sample_all_listed$soviet_era))} NA (missing year_founded)."
)


# --- any_esg ------------------------------------------------------------------

sample_all_listed <- sample_all_listed %>%
  mutate(
    esg_info_num = suppressWarnings(as.integer(ESG_info_annual_report)),
    esg_separate_num = suppressWarnings(as.integer(ESG_separate_report)),
    any_esg = case_when(
      is.na(esg_info_num) & is.na(esg_separate_num) ~ NA_real_,
      esg_info_num == 1 | esg_separate_num == 1      ~ 1,
      TRUE                                            ~ 0
    )
  ) %>%
  select(-esg_info_num, -esg_separate_num)

log_info(
  "any_esg: {sum(sample_all_listed$any_esg == 1, na.rm = TRUE)} disclosing, ",
  "{sum(sample_all_listed$any_esg == 0, na.rm = TRUE)} not disclosing, ",
  "{sum(is.na(sample_all_listed$any_esg))} NA."
)


# --- ESG disclosure regulation variables ---------------------------------------
#
# Following Krueger et al., state (government) and stock exchange mandates are
# tracked separately, and exchange-level voluntary guidance is tracked as its
# own flag:
#   mandatory_esg_state    - country-level ESG disclosure mandate (1/0)
#   mandatory_esg_exchange - stock exchange listing-rule ESG mandate (1/0)
#   mandatory_esg          - any mandate applies, state or exchange (1/0)
#   voluntary_esg_exchange - stock exchange publishes voluntary/recommended
#                             ESG reporting guidance for listed firms (1/0)
# Comply-or-explain regimes count as mandatory only when the duty attaches
# automatically at listing. Regimes gated behind a firm's own voluntary
# adoption of a corporate governance code (Kyrgyzstan's CCG, Latvia's CG
# Code) are excluded from mandatory_esg_state, since adoption cannot be
# traced per firm and the obligation is not automatic. Countries/exchanges
# for which no regulatory source document was reviewed are coded 0 (no
# mandate/guidance identified), not NA - absence of evidence, not missing
# data.
#
# Combines two 1/0/NA flags: 1 if either is confirmed 1, NA if either input
# is unresolved and neither is confirmed 1, 0 only if both are confirmed 0.
combine_flags <- function(a, b) {
  case_when(
    (!is.na(a) & a == 1) | (!is.na(b) & b == 1) ~ 1,
    is.na(a) | is.na(b)                         ~ NA_real_,
    TRUE                                        ~ 0
  )
}

# --- mandatory_esg_state: country-level mandate --------------------------------

esg_state_level <- c(KAZ = 0, UZB = 0, KGZ = 0, TKM = 0, TJK = 0)

# EU CSRD/NFRD-style mandate for EST/LVA/LTU applies above an employee threshold.
baltic_employee_threshold <- 500

sample_all_listed <- sample_all_listed %>%
  mutate(
    employees_num = suppressWarnings(as.integer(number_of_employees)),
    mandatory_esg_state = case_when(
      country_iso3 %in% names(esg_state_level) ~ esg_state_level[country_iso3],
      country_iso3 %in% c("EST", "LVA", "LTU") & is.na(employees_num) ~ NA_real_,
      country_iso3 %in% c("EST", "LVA", "LTU") ~
        if_else(employees_num > baltic_employee_threshold, 1, 0),
      TRUE ~ NA_real_
    )
  ) %>%
  select(-employees_num)


# --- mandatory_esg_exchange: stock exchange listing-rule mandate ---------------

# AIX: mandate applies only to ESG-Labelled/Green Bond issuers; none are in
# this sample, so all AIX firms are coded 0. VLN: Nasdaq Vilnius Listing
# Rules mandate ESG disclosure for regulated-market-listed companies (all
# Nasdaq Vilnius firms in this sample are regulated-market).
esg_exchange_mandate <- c(
  AIX  = 0,  # no ESG/green bond issuers in sample
  KSE  = 0,  # no exchange-level ESG mandate identified
  BTS  = 0,  # no exchange-level ESG mandate identified
  UZSE = 0,  # no exchange-level ESG mandate identified
  RIG  = 0,  # no exchange-level ESG mandate identified (Nasdaq Riga)
  VLN  = 1,  # mandatory for regulated-market-listed companies
  TLN  = 0   # no exchange-level ESG mandate identified (Nasdaq Tallinn)
)

# KASE is the one exchange with a segment-dependent rule: mandatory ESG
# disclosure for admittance initiators on 'Main' (from 26/09/2022) and on
# 'Alternative' only from 01/01/2025 - after this sample's FY2024 cutoff, so
# 'Alternative' is coded 0 here.
kase_esg_mandate <- function(category) {
  case_when(
    category == "Main"        ~ 1,
    category == "Alternative" ~ 0,
    TRUE                      ~ NA_real_
  )
}

sample_all_listed <- sample_all_listed %>%
  mutate(
    mandatory_esg_exchange = case_when(
      # dual listings: mandate applies if either applicable exchange requires it
      grepl("KASE", exchange, fixed = TRUE) & grepl("AIX", exchange, fixed = TRUE) ~
        combine_flags(kase_esg_mandate(category), esg_exchange_mandate["AIX"]),
      grepl("KSE", exchange, fixed = TRUE) & grepl("BTS", exchange, fixed = TRUE) ~
        combine_flags(esg_exchange_mandate["KSE"], esg_exchange_mandate["BTS"]),
      grepl("KASE", exchange, fixed = TRUE) ~ kase_esg_mandate(category),
      exchange %in% names(esg_exchange_mandate) ~ esg_exchange_mandate[exchange],
      grepl("RIG", exchange, fixed = TRUE) ~ esg_exchange_mandate["RIG"],
      grepl("VLN", exchange, fixed = TRUE) ~ esg_exchange_mandate["VLN"],
      grepl("TLN", exchange, fixed = TRUE) ~ esg_exchange_mandate["TLN"],
      TRUE ~ NA_real_
    )
  )


# --- mandatory_esg: any ESG disclosure mandate applies --------------------------

sample_all_listed <- sample_all_listed %>%
  mutate(
    mandatory_esg = combine_flags(mandatory_esg_state, mandatory_esg_exchange)
  )


# --- voluntary_esg_exchange: exchange publishes voluntary ESG guidance ---------

# 1 if the firm's stock exchange has published its own voluntary/recommended
# ESG reporting guidance for listed firms, 0 if no such guidance was identified. 

esg_exchange_voluntary <- c(
  AIX  = 1,  # AIX Voluntary Sustainability Reporting Guidance
  KASE = 1,  # KASE Methodology for Preparing an ESG Report
  KSE  = 1,  # Kyrgyz Stock Exchange ESG Guidance (2023)
  BTS  = 0,  # no exchange-level ESG guidance identified
  UZSE = 0,  # no exchange-level ESG guidance identified
  RIG  = 0,  # no exchange-level ESG guidance identified
  VLN  = 0,  # no exchange-level ESG guidance identified
  TLN  = 0   # no exchange-level ESG guidance identified
)

sample_all_listed <- sample_all_listed %>%
  mutate(
    voluntary_esg_exchange = case_when(
      # dual listings: guidance applies if either applicable exchange offers it
      grepl("KASE", exchange, fixed = TRUE) & grepl("AIX", exchange, fixed = TRUE) ~
        combine_flags(esg_exchange_voluntary["KASE"], esg_exchange_voluntary["AIX"]),
      grepl("KSE", exchange, fixed = TRUE) & grepl("BTS", exchange, fixed = TRUE) ~
        combine_flags(esg_exchange_voluntary["KSE"], esg_exchange_voluntary["BTS"]),
      exchange %in% names(esg_exchange_voluntary) ~ esg_exchange_voluntary[exchange],
      grepl("RIG", exchange, fixed = TRUE) ~ esg_exchange_voluntary["RIG"],
      grepl("VLN", exchange, fixed = TRUE) ~ esg_exchange_voluntary["VLN"],
      grepl("TLN", exchange, fixed = TRUE) ~ esg_exchange_voluntary["TLN"],
      TRUE ~ NA_real_
    )
  )

log_info(
  "mandatory_esg: {sum(sample_all_listed$mandatory_esg == 1, na.rm = TRUE)} under a mandate, ",
  "{sum(sample_all_listed$mandatory_esg == 0, na.rm = TRUE)} not, ",
  "{sum(is.na(sample_all_listed$mandatory_esg))} NA (state {sum(is.na(sample_all_listed$mandatory_esg_state))}, ",
  "exchange {sum(is.na(sample_all_listed$mandatory_esg_exchange))} unresolved)."
)

log_info(
  "voluntary_esg_exchange: {sum(sample_all_listed$voluntary_esg_exchange == 1, na.rm = TRUE)} with guidance, ",
  "{sum(sample_all_listed$voluntary_esg_exchange == 0, na.rm = TRUE)} without."
)


# --- mandatory_fin_state: state-level annual financial report mandate --------

# All eight state jurisdictions in the raw sample mandate disclosure of an
# annual financial report for ordinary listed equity issuers. Scope carve-outs 
# target sovereign issuers and small debt-only issuances, neither of which 
# applies to this study's non-financial equity sample, so the mandate is coded 1 
# for every firm currently in scope.

fin_state_mandate <- c(
  KAZ = 1, UZB = 1, KGZ = 1, TJK = 1, TKM = 1, EST = 1, LVA = 1, LTU = 1
)

sample_all_listed <- sample_all_listed %>%
  mutate(mandatory_fin_state = fin_state_mandate[country_iso3])

log_info(
  "mandatory_fin_state: {sum(sample_all_listed$mandatory_fin_state == 1, na.rm = TRUE)} ",
  "under a mandate, {sum(is.na(sample_all_listed$mandatory_fin_state))} NA."
)

# --- Country-level institutional/economic controls (WGI, GDP per capita) ------

country_chars <- read_csv(
  global_cfg$country_characteristics,
  col_types = cols()
) %>%
  filter(!is.na(`Country Code`)) %>%
  transmute(
    country_iso3 = `Country Code`,
    gdp_per_capita = `GDP per capita, PPP (current international $)`,
    wgi_rule_of_law = `Rule of Law - Governance estimate (approx. -2.5 to +2.5)`
  )

sample_all_listed <- sample_all_listed %>%
  left_join(country_chars, by = "country_iso3") %>%
  mutate(ln_gdp_per_capita = log(gdp_per_capita))

log_info(
  "Country-level controls: {sum(!is.na(sample_all_listed$wgi_rule_of_law))} of ",
  "{nrow(sample_all_listed)} firms matched to WGI/GDP data ",
  "({sum(is.na(sample_all_listed$wgi_rule_of_law))} unmatched)."
)


# --- Currency conversion to EUR -----------------------------------------------

sample_all_listed <- sample_all_listed %>%
  rename(
    total_assets_original = total_assets,
    net_income_original   = net_income,
    currency_original     = currency
  )

parse_number <- function(x) {
  x <- trimws(x)
  is_na_val <- x %in% c("", "NA", "N/A")
  neg <- grepl("^\\(.*\\)$", x)
  x_clean <- gsub("[(),]", "", x)
  val <- suppressWarnings(as.numeric(x_clean))
  val[neg] <- -val[neg]
  val[is_na_val] <- NA
  val
}

sample_all_listed <- sample_all_listed %>%
  mutate(
    total_assets_num = parse_number(total_assets_original),
    net_income_num   = parse_number(net_income_original),
    fx_rate_num      = suppressWarnings(as.numeric(fx_rate_to_eur)),
    total_assets_eur = total_assets_num / fx_rate_num,
    net_income_eur   = net_income_num / fx_rate_num
  ) %>%
  select(-total_assets_num, -net_income_num, -fx_rate_num)

log_info(
  "Currency conversion: {sum(!is.na(sample_all_listed$total_assets_eur))} of ",
  "{nrow(sample_all_listed)} firms have total_assets_eur; ",
  "{sum(!is.na(sample_all_listed$net_income_eur))} have net_income_eur."
)


# --- ln(Size) -----------------------------------------------------------------

sample_all_listed <- sample_all_listed %>%
  mutate(
    ln_total_assets_eur = log(total_assets_eur)
  )


# --- Return on assets ---------------------------------------------------------

sample_all_listed <- sample_all_listed %>%
  mutate(roa = net_income_eur / total_assets_eur)

log_info(
  "roa: {sum(!is.na(sample_all_listed$roa))} of {nrow(sample_all_listed)} firms have a value; ",
  "{sum(is.na(sample_all_listed$roa))} NA (missing total_assets or net_income)."
)


# --- Winsorize continuous model variables --------------------------------------

# Only ln_total_assets_eur and roa are continuous regressors. Capped (not 
# dropped) at the 5st/95th percentile. Kept as separate _w columns so the raw 
# values stay available for descriptives.

winsorized <- sample_all_listed %>%
  select(ln_total_assets_eur, roa) %>%
  treat_outliers(percentile = 0.05)

sample_all_listed <- sample_all_listed %>%
  mutate(
    ln_total_assets_eur_w = winsorized$ln_total_assets_eur,
    roa_w = winsorized$roa
  )

log_info(
  "Winsorized ln_total_assets_eur: {sum(sample_all_listed$ln_total_assets_eur != sample_all_listed$ln_total_assets_eur_w, na.rm = TRUE)} values capped; ",
  "roa: {sum(sample_all_listed$roa != sample_all_listed$roa_w, na.rm = TRUE)} values capped."
)

# --- Clean ownership flag types ------------------------------------------------

sample_all_listed <- sample_all_listed %>%
  mutate(
    state_ownership = suppressWarnings(as.integer(state_ownership)),
    foreign_ownership = suppressWarnings(as.integer(foreign_ownership)),
    individual_ownership = suppressWarnings(as.integer(individual_ownership))
  )


# --- Final column selection and ordering --------------------------------------

sample_all_listed <- sample_all_listed %>%
  select(
    # Identifiers
    ticker, company_name, isin, national_identifier,
    
    # Classification and location
    country, country_iso3, region, exchange, icb_industry, sensitive_industry,
    
    # Time
    year, listed_since, year_founded, soviet_era,
    
    # Disclosure behaviour
    annual_report, annual_fin_report, interim_fin_reports,
    ESG_info_annual_report, ESG_separate_report, any_esg,
    
    # International exposure
    cross_listed, cross_listed_since, un_global_compact_joined,
    
    # Financials
    total_assets_original, net_income_original, currency_original,
    fx_rate_to_eur, total_assets_eur, net_income_eur, ln_total_assets_eur, roa,
    ln_total_assets_eur_w, roa_w,
    
    # Ownership
    state_ownership, state_ownership_threshold,
    foreign_ownership, foreign_ownership_threshold,
    individual_ownership, individual_ownership_threshold,
    
    # Disclosure regulation (constructed)
    mandatory_esg_state, mandatory_esg_exchange, mandatory_esg,
    voluntary_esg_exchange, mandatory_fin_state,
    
    # Country-level controls
    gdp_per_capita, ln_gdp_per_capita, wgi_rule_of_law
  )

log_info(
  "Final cleaned sample: {nrow(sample_all_listed)} rows, ",
  "{ncol(sample_all_listed)} columns."
)

# --- Save cleaned sample ------------------------------------------------------

write_parquet(sample_all_listed, global_cfg$base_sample)

log_info("Cleaned sample saved to '{global_cfg$base_sample}'.")
