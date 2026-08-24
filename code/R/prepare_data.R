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


# --- reg_strength_esg_state -----------------------------------------------------

# Country-level ESG disclosure mandate for non-financial firms:
# 0 = no mandate, 1 = voluntary/recommended, 2 = mandatory
esg_state_level <- c(
  KAZ = 0,  # existing state rules apply only to banks/financial firms
  UZB = 0,  # 2024 mandate exists on paper but not enforced
  KGZ = 1,  # comply-or-explain for public companies
  TKM = 0,  # no regulation
  TJK = 0   # no regulation
)

# EU CSRD/NFRD only applies above an employee-count threshold ------------------

baltic_employee_threshold <- 500

sample_all_listed <- sample_all_listed %>%
  mutate(
    employees_num = suppressWarnings(as.integer(number_of_employees)),
    reg_strength_esg_state = case_when(
      country_iso3 %in% names(esg_state_level) ~ esg_state_level[country_iso3],
      country_iso3 %in% c("EST", "LVA", "LTU") & is.na(employees_num) ~ NA_real_,
      country_iso3 %in% c("EST", "LVA", "LTU") ~
        if_else(employees_num > baltic_employee_threshold, 2, 0),
      TRUE ~ NA_real_
    )
  ) %>%
  select(-employees_num)


# --- reg_strength_esg_exchange ----------------------------------------------------

# Exchange-level ESG disclosure requirement, for exchanges with one flat rule:
# 0 = no rule identified, 1 = voluntary/recommended, 2 = mandatory
esg_exchange_level <- c(
  AIX  = 1,  # voluntary ESG disclosure recommendations
  KSE  = 1,  # voluntary sustainability guidance
  BTS  = 0,  # no ESG rule identified
  UZSE = 0,  # no ESG rule identified
  RIG  = 1,  # Nasdaq Riga voluntary CSR recommendation
  VLN  = 1,  # Nasdaq Vilnius voluntary CSR recommendation
  TLN  = 0   # no Nasdaq Tallinn exchange-level ESG rule identified
)

# KASE is the one exchange without a flat rule - its ESG requirement depends
# on market segment: Main is mandatory, Alternative is voluntary (as of FY2024).
kase_esg_exchange_level <- function(category) {
  case_when(
    category == "Main"        ~ 2,
    category == "Alternative" ~ 1,
    TRUE                      ~ NA_real_
  )
}

sample_all_listed <- sample_all_listed %>%
  mutate(
    reg_strength_esg_exchange = case_when(
      # dual listings: take the stronger of the two applicable rules
      grepl("KASE", exchange, fixed = TRUE) & grepl("AIX", exchange, fixed = TRUE) ~
        pmax(kase_esg_exchange_level(category), esg_exchange_level["AIX"], na.rm = TRUE),
      grepl("KSE", exchange, fixed = TRUE) & grepl("BTS", exchange, fixed = TRUE) ~
        pmax(esg_exchange_level["KSE"], esg_exchange_level["BTS"], na.rm = TRUE),
      grepl("KASE", exchange, fixed = TRUE) ~ kase_esg_exchange_level(category),
      exchange %in% names(esg_exchange_level) ~ esg_exchange_level[exchange],
      grepl("RIG", exchange, fixed = TRUE) ~ esg_exchange_level["RIG"],
      grepl("VLN", exchange, fixed = TRUE) ~ esg_exchange_level["VLN"],
      grepl("TLN", exchange, fixed = TRUE) ~ esg_exchange_level["TLN"],
      TRUE ~ NA_real_
    )
  )


# --- reg_strength_fin --------------------------------------------------------------

# Country-level financial disclosure mandate: 0 = no mandate, 1 = voluntary, 2 = mandatory
fin_state_level <- c(
  KAZ = 2, UZB = 2, KGZ = 2, EST = 2, LVA = 2, LTU = 2
)

# Kyrgyzstan's mandate is conditional on OJSC status - overrides the flat lookup.
kyrgyzstan_fin_level <- function(company_name) {
  case_when(
    grepl("OJSC", company_name, fixed = TRUE) ~ 2,
    TRUE ~ NA_real_
  )
}

sample_all_listed <- sample_all_listed %>%
  mutate(
    reg_strength_fin = case_when(
      country_iso3 == "KGZ" ~ kyrgyzstan_fin_level(company_name),
      country_iso3 %in% names(fin_state_level) ~ fin_state_level[country_iso3],
      TRUE ~ NA_real_
    )
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


# --- Final column selection and ordering --------------------------------------

sample_all_listed <- sample_all_listed %>%
  select(
    # Identifiers
    ticker, company_name, isin, national_identifier,
    
    # Classification and location
    country, country_iso3, region, exchange, icb_industry, sensitive_industry,
    
    # Time
    year, listed_since, year_founded, soviet_era,
    
    # Disclosure flags and outcome variable
    annual_report, annual_fin_report, interim_fin_reports,
    ESG_info_annual_report, ESG_separate_report, any_esg,
    
    # International exposure
    cross_listed, cross_listed_since, un_global_compact_joined,
    orbis_data, lseg_esg_data,
    
    # Financials
    total_assets_original, net_income_original, currency_original,
    fx_rate_to_eur, total_assets_eur, net_income_eur, ln_total_assets_eur, roa, 
    
    # Ownership
    state_ownership, state_ownership_threshold,
    foreign_ownership, foreign_ownership_threshold,
    individual_ownership, individual_ownership_threshold,
    
    # Regulation strength (constructed)
    reg_strength_esg_state, reg_strength_esg_exchange, reg_strength_fin
  )

log_info(
  "Final cleaned sample: {nrow(sample_all_listed)} rows, ",
  "{ncol(sample_all_listed)} columns."
)

# --- Save cleaned sample ------------------------------------------------------

write_parquet(sample_all_listed, global_cfg$base_sample)

log_info("Cleaned sample saved to '{global_cfg$base_sample}'.")