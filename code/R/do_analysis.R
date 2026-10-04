# ------------------------------------------------------------------------------
# Runs analysis and prepares tables.
# ------------------------------------------------------------------------------

source("code/R/utils.R")


# --- Sample selection and composition, by region/country/exchange -------------

exchange_map <- tribble(
  ~exchange, ~country,       ~region,
  "KASE",    "Kazakhstan",   "Central Asia",
  "AIX",     "Kazakhstan",   "Central Asia",
  "UZSE",    "Uzbekistan",   "Central Asia",
  "KSE",     "Kyrgyzstan",   "Central Asia",
  "BTS",     "Kyrgyzstan",   "Central Asia",
  "AGB",     "Turkmenistan", "Central Asia",
  "CASE",    "Tajikistan",   "Central Asia",
  "TLN",     "Estonia",      "Baltic states",
  "RIG",     "Latvia",       "Baltic states",
  "VLN",     "Lithuania",    "Baltic states"
)
exchanges <- exchange_map$exchange

sample_selection <- tribble(
  ~step,                                                                              ~KASE, ~AIX, ~UZSE, ~KSE, ~BTS, ~AGB, ~CASE, ~TLN, ~RIG, ~VLN,
  "Initial number of observations (total listed firms)",                                 92,   53,   92,   38,   16,    7,    0,   31,   13,   25,
  "Less: Foreign listings",                                                              -7,  -11,    0,    0,    0,    0,    0,    0,    0,    0,
  "Less: Non-equity instruments (bonds), ETFs",                                            0,  -29,  -13,  -12,   -2,   -2,    0,    0,    0,    0,
  "Less: Duplicate observations (companies with more than one ticker/ISIN)",             -13,   -2,    0,    0,    0,    0,    0,    0,    0,    0,
  "Less: Financial companies",                                                           -22,    0,  -28,  -14,   -3,   -5,    0,   -4,   -3,   -3,
  "Less: Duplicate observations (companies listed on more than one domestic exchange)",    0,   -9,    0,    0,   -3,    0,    0,    0,    0,    0,
  "Less: Market segments outside the regulated market",                                     0,    0,    0,    0,    0,    0,    0,  -11,   -5,   -3,
  "Less: Not listed as of FY2024 (including missing listing date)",                            -2,    0,   -8,   -2,   -8,    0,    0,   -1,   -1,    0
)

# Total per exchange: initial count plus all attrition steps above.
total_exchange <- sample_selection %>%
  summarise(across(all_of(exchanges), sum)) %>%
  mutate(step = "Total number of observations per stock exchange", .before = 1)

total_row <- function(total_tbl, label, group_var = NULL) {
  long <- total_tbl %>%
    select(-step) %>%
    pivot_longer(everything(), names_to = "exchange", values_to = "n") %>%
    mutate(exchange = factor(exchange, levels = exchanges)) %>%
    left_join(exchange_map, by = "exchange") %>%
    arrange(exchange) %>%
    mutate(grp = if (is.null(group_var)) "all" else .data[[group_var]])
  
  long %>%
    group_by(grp) %>%
    mutate(n = if_else(exchange == first(exchange), sum(n), NA_integer_)) %>%
    ungroup() %>%
    select(exchange, n) %>%
    pivot_wider(names_from = exchange, values_from = n) %>%
    mutate(step = label, .before = 1)
}

sample_selection <- bind_rows(
  sample_selection,
  total_exchange,
  total_row(total_exchange, "Total number of observations per country", "country"),
  total_row(total_exchange, "Total number of observations per region", "region"),
  total_row(total_exchange, "Grand total (all regions) \u2014 disclosure sample")
)


# --- Read data ------------------------------------------------------------------

log_info("Reading data...")
smp <- read_parquet(global_cfg$base_sample)


# --- Restrict to the common regression-ready sample ------------------------------

reg_vars <- c(
  "any_esg", "mandatory_esg", "mandatory_esg_state", "mandatory_esg_exchange",
  "voluntary_esg_exchange", "foreign_ownership", "sensitive_industry",
  "soviet_era", "state_ownership", "ln_total_assets_eur_w", "roa_w",
  "region", "wgi_rule_of_law" #, "ln_gdp_per_capita"
)

n_before <- nrow(smp)
smp_reg <- smp %>%
  drop_na(all_of(reg_vars)) %>%
  mutate(region = factor(region, levels = c("Baltics", "Central Asia")))

log_info(
  "Regression-ready sample: {nrow(smp_reg)} of {n_before} firms have complete ",
  "data across all {length(reg_vars)} model variables ",
  "({n_before - nrow(smp_reg)} dropped)."
)


# --- Panel B: regression-sample attrition, computed live from smp/smp_reg -------
#
# Each reg_vars field is bucketed into a missingness category; categories are
# applied sequentially so a firm missing fields in two categories is only
# counted once, under whichever category is checked first. Categories that turn out
# to have zero drops are dropped from the table automatically.

missingness_categories <- list(
  "Less: missing ownership data (state and/or foreign ownership)" =
    c("state_ownership", "foreign_ownership"),
  "Less: missing financial statement data (total assets/net income)" =
    c("ln_total_assets_eur_w", "roa_w"),
  "Less: missing ESG mandate/guidance classification" =
    c("mandatory_esg", "mandatory_esg_state", "mandatory_esg_exchange", "voluntary_esg_exchange"),
  "Less: missing country-level institutional data (WGI)" = #and GDP
    c("region", "wgi_rule_of_law"), # "ln_gdp_per_capita"),
  "Less: missing base disclosure/classification data" =
    c("any_esg", "sensitive_industry"),
  "Less: missing Soviet-era classification" =
    c("soviet_era")
)

stopifnot(setequal(unlist(missingness_categories, use.names = FALSE), reg_vars))

count_by_exchange <- function(data) {
  exchanges %>%
    set_names() %>%
    map_int(~ sum(grepl(.x, data$exchange, fixed = TRUE), na.rm = TRUE)) %>%
    as.list() %>%
    as_tibble()
}

remaining <- smp
exclusion_rows <- list()
for (label in names(missingness_categories)) {
  vars <- missingness_categories[[label]]
  dropped <- remaining %>% filter(if_any(all_of(vars), is.na))
  exclusion_rows[[label]] <- count_by_exchange(dropped) %>%
    mutate(across(everything(), ~ -.x)) %>%
    mutate(step = label, .before = 1)
  remaining <- remaining %>% drop_na(all_of(vars))
}
stopifnot(setequal(remaining$isin, smp_reg$isin))

exclusion_rows <- bind_rows(exclusion_rows) %>%
  filter(rowSums(across(all_of(exchanges), ~ .x != 0)) > 0)

# Disclosure sample row reuses Panel A's own resolved total_exchange.
sample_selection_reg <- bind_rows(
  total_exchange %>% mutate(step = "Disclosure sample"),
  exclusion_rows
)

total_exchange_reg <- sample_selection_reg %>%
  summarise(across(all_of(exchanges), sum)) %>%
  mutate(step = "Total number of observations per stock exchange", .before = 1)

sample_selection_reg <- bind_rows(
  sample_selection_reg,
  total_exchange_reg,
  total_row(total_exchange_reg, "Total number of observations per country", "country"),
  total_row(total_exchange_reg, "Total number of observations per region", "region"),
  total_row(total_exchange_reg, "Grand total (all regions) \u2014 regression sample")
)


# --- Two separate tables, one per panel -------------------------------------

add_exchange_spanners <- function(gt_tbl) {
  gt_tbl %>%
    tab_spanner(label = "Kazakhstan",   columns = c(KASE, AIX), id = "kaz") %>%
    tab_spanner(label = "Uzbekistan",   columns = c(UZSE),      id = "uzb") %>%
    tab_spanner(label = "Kyrgyzstan",   columns = c(KSE, BTS),  id = "kgz") %>%
    tab_spanner(label = "Turkmenistan", columns = c(AGB),       id = "tkm") %>%
    tab_spanner(label = "Tajikistan",   columns = c(CASE),      id = "tjk") %>%
    tab_spanner(label = "Estonia",      columns = c(TLN),       id = "est") %>%
    tab_spanner(label = "Latvia",       columns = c(RIG),       id = "lva") %>%
    tab_spanner(label = "Lithuania",    columns = c(VLN),       id = "ltu") %>%
    tab_spanner(label = "Central Asia",  spanners = c("kaz", "uzb", "kgz", "tkm", "tjk")) %>%
    tab_spanner(label = "Baltic states", spanners = c("est", "lva", "ltu"))
}

fmt_attrition <- function(gt_tbl) {
  gt_tbl %>%
    fmt(
      columns = all_of(exchanges),
      fns = function(x) case_when(
        is.na(x) ~ "", x == 0 ~ "-", x < 0 ~ paste0("(", abs(x), ")"),
        TRUE ~ as.character(x)
      )
    )
}

sample_selection <- bind_rows(
  sample_selection,
  total_row(total_exchange_reg, "Grand total (all regions) \u2014 regression sample")
)

tab_sample_selection_a <- sample_selection %>%
  gt(rowname_col = "step") %>%
  fmt_attrition() %>%
  add_exchange_spanners()

tab_sample_selection_b <- sample_selection_reg %>%
  gt(rowname_col = "step") %>%
  fmt_attrition() %>%
  add_exchange_spanners()

# --- Disclosure coverage by region and country --------------------------------

to01 <- function(x) suppressWarnings(as.integer(x))

disclosure_counts <- function(data) {
  data %>%
    summarise(
      N = n(),
      `Integrated Annual Report` = sum(to01(annual_report) == 1, na.rm = TRUE),
      `ESG Disclosure` = sum(to01(any_esg) == 1, na.rm = TRUE),
      `(a) ESG Standalone Report` = sum(to01(ESG_separate_report) == 1, na.rm = TRUE),
      `(b) ESG in Integrated Annual Report` = sum(to01(ESG_info_annual_report) == 1, na.rm = TRUE),
      `(c) ESG in Both (Standalone and Integrated Annual Report)` = sum(
        to01(ESG_separate_report) == 1 & to01(ESG_info_annual_report) == 1,
        na.rm = TRUE
      ),
      `Annual Financial Disclosure` = sum(to01(annual_fin_report) == 1, na.rm = TRUE)
      # `Interim Financial Disclosure` = sum(to01(interim_fin_reports) == 1, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "step", values_to = "n")
}

country_counts <- smp %>%
  group_by(country) %>%
  group_modify(~ disclosure_counts(.x)) %>%
  ungroup() %>%
  pivot_wider(names_from = country, values_from = n)

region_counts <- smp %>%
  group_by(region) %>%
  group_modify(~ disclosure_counts(.x)) %>%
  ungroup() %>%
  mutate(
    region = if_else(region == "Central Asia", "Central Asia (Total)", "Baltic states (Total)")
  ) %>%
  pivot_wider(names_from = region, values_from = n)

all_counts <- disclosure_counts(smp) %>%
  mutate(country = "All Countries (Total)") %>%
  pivot_wider(names_from = country, values_from = n)

step_order <- c(
  "N", "Integrated Annual Report", "ESG Disclosure",
  "(a) ESG Standalone Report", "(b) ESG in Integrated Annual Report",
  "(c) ESG in Both (Standalone and Integrated Annual Report)", "Annual Financial Disclosure"
  # "Interim Financial Disclosure"
)

tab_disclosure_coverage <- country_counts %>%
  left_join(region_counts, by = "step") %>%
  left_join(all_counts, by = "step") %>%
  select(
    step, Kazakhstan, Uzbekistan, Kyrgyzstan, `Central Asia (Total)`,
    Estonia, Latvia, Lithuania, `Baltic states (Total)`, `All Countries (Total)`
  ) %>%
  mutate(step = factor(step, levels = step_order)) %>%
  arrange(step) %>%
  mutate(step = as.character(step)) %>%
  gt(rowname_col = "step")


# --- Mandate and compliance figures: ESG and financial reporting --------------

# Two questions per disclosure type: (1) how is the sample composed across
# mandate status x disclosure outcome (distribution), and (2) among mandated
# firms specifically, how many comply (compliance). For FIN, mandatory_fin_
# report is 1 for virtually the whole sample, so "compliance" and "overall
# disclosure rate" collapse into the same number - only the distribution
# chart is built for FIN, not a separate compliance chart.

country_order <- c("Kazakhstan", "Uzbekistan", "Kyrgyzstan", "Estonia", "Latvia", "Lithuania")

category_levels <- c(
  "Mandated & Disclosed", "Mandated & Not Disclosed",
  "Not Mandated & Disclosed", "Not Mandated & Not Disclosed",
  "Mandate Status Unresolved"
)
category_colors <- c(
  "Mandated & Disclosed" = "#2A4D77", "Mandated & Not Disclosed" = "#9DB8D6",
  "Not Mandated & Disclosed" = "#DD8452", "Not Mandated & Not Disclosed" = "#F3D1B8",
  "Mandate Status Unresolved" = "grey80"
)

# By country: % of firms in each of the 5 mandate x disclosure categories.
# The 5th category (grey) covers firms with an unresolved mandate status.
mandate_disclosure_breakdown <- function(data, mandate_var, disclosure_var) {
  data %>%
    mutate(
      mandate = to01(.data[[mandate_var]]),
      disclosed = to01(.data[[disclosure_var]])
    ) %>%
    group_by(country) %>%
    summarise(
      n = n(),
      `Mandated & Disclosed` = sum(mandate == 1 & disclosed == 1, na.rm = TRUE),
      `Mandated & Not Disclosed` = sum(mandate == 1 & disclosed == 0, na.rm = TRUE),
      `Not Mandated & Disclosed` = sum(mandate == 0 & disclosed == 1, na.rm = TRUE),
      `Not Mandated & Not Disclosed` = sum(mandate == 0 & disclosed == 0, na.rm = TRUE),
      `Mandate Status Unresolved` = sum(is.na(mandate)),
      .groups = "drop"
    ) %>%
    pivot_longer(-c(country, n), names_to = "category", values_to = "count") %>%
    mutate(pct = 100 * count / n, category = factor(category, levels = category_levels)) %>%
    arrange(country, category) %>%
    group_by(country) %>%
    mutate(ymax = cumsum(pct), ymin = ymax - pct, ymid = (ymin + ymax) / 2) %>%
    ungroup() %>%
    mutate(
      country = factor(country, levels = country_order),
      label_color = if_else(
        category %in% c("Mandated & Disclosed", "Not Mandated & Disclosed"), "white", "black"
      )
    ) %>%
    filter(!is.na(country))
}

plot_mandate_disclosure <- function(data) {
  labs_df <- distinct(data, country, n)
  country_labels <- setNames(paste0(labs_df$country, "\n(N=", labs_df$n, ")"), labs_df$country)
  
  ggplot(data, aes(x = country, y = pct, fill = category)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    geom_text(
      aes(y = ymid, label = if_else(pct >= 4, sprintf("%.0f%%", pct), ""), color = I(label_color)),
      size = 2.8
    ) +
    scale_fill_manual(values = category_colors) +
    scale_x_discrete(labels = country_labels) +
    labs(x = NULL, y = "% of firms", fill = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
}

# Among mandated firms only: % disclosed vs. not, by country. Countries with
# zero mandated firms show no bar and are labeled "No mandate".
mandate_compliance <- function(data, mandate_var, disclosure_var) {
  data %>%
    mutate(
      mandate = to01(.data[[mandate_var]]),
      disclosed = to01(.data[[disclosure_var]])
    ) %>%
    group_by(country) %>%
    summarise(
      n_mandated = sum(mandate == 1, na.rm = TRUE),
      Disclosed = sum(mandate == 1 & disclosed == 1, na.rm = TRUE),
      `Not Disclosed` = sum(mandate == 1 & disclosed == 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(c(Disclosed, `Not Disclosed`), names_to = "category", values_to = "count") %>%
    mutate(
      pct = if_else(n_mandated > 0, 100 * count / n_mandated, 0),
      category = factor(category, levels = c("Disclosed", "Not Disclosed"))
    ) %>%
    arrange(country, category) %>%
    group_by(country) %>%
    mutate(ymax = cumsum(pct), ymin = ymax - pct, ymid = (ymin + ymax) / 2) %>%
    ungroup() %>%
    mutate(
      country = factor(country, levels = country_order),
      label_color = if_else(category == "Disclosed", "white", "black")
    ) %>%
    filter(!is.na(country))
}

plot_mandate_compliance <- function(data) {
  labs_df <- distinct(data, country, n_mandated)
  country_labels <- setNames(paste0(labs_df$country, "\n(N=", labs_df$n_mandated, ")"), labs_df$country)
  
  ggplot(data, aes(x = country, y = pct, fill = category)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    geom_text(
      aes(y = ymid, label = if_else(pct >= 4, sprintf("%.0f%%", pct), ""), color = I(label_color)),
      size = 2.8
    ) +
    scale_fill_manual(values = c("Disclosed" = "#2A4D77", "Not Disclosed" = "#9DB8D6")) +
    scale_x_discrete(labels = country_labels) +
    labs(x = NULL, y = "% of mandated firms", fill = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
}

# --- ESG: distribution + compliance ---
fig_esg_disclosure <- plot_mandate_disclosure(
  mandate_disclosure_breakdown(smp, "mandatory_esg", "any_esg")
)
fig_esg_compliance <- plot_mandate_compliance(
  mandate_compliance(smp, "mandatory_esg", "any_esg")
)

# --- FIN: distribution only (mandate is ~universal, so this already shows compliance) ---
fig_fin_disclosure <- plot_mandate_disclosure(
  mandate_disclosure_breakdown(smp, "mandatory_fin_state", "annual_fin_report")
)


# --- WGI institutional-quality comparison, all 6 dimensions -------------------

wgi_dim_labels <- c(
  "Control of Corruption", "Government Effectiveness", "Political Stability",
  "Regulatory Quality", "Rule of Law", "Voice and Accountability"
)

# Matched by ISO3 code
country_iso3_order <- c(
  Kazakhstan = "KAZ", Kyrgyzstan = "KGZ", Tajikistan = "TJK",
  Turkmenistan = "TKM", Uzbekistan = "UZB",
  Estonia = "EST", Latvia = "LVA", Lithuania = "LTU"
)

country_order_wgi <- c(
  "Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan",
  "Estonia", "Latvia", "Lithuania"
)

wgi_long <- read_csv(global_cfg$country_characteristics, col_types = cols()) %>%
  filter(`Country Code` %in% country_iso3_order) %>%
  mutate(country = names(country_iso3_order)[match(`Country Code`, country_iso3_order)]) %>%
  select(country, matches(paste(wgi_dim_labels, collapse = "|"))) %>%
  rename_with(~ wgi_dim_labels, -country) %>%
  pivot_longer(-country, names_to = "dimension", values_to = "score") %>%
  mutate(
    country = factor(country, levels = country_order_wgi),
    dimension = factor(dimension, levels = wgi_dim_labels)
  )

# Per-country color (grouped by region hue, darker to brighter in
# alphabetical order within each region) and shape (circle = Central Asia,
# square = Baltic states).
country_colors_wgi <- c(
  "Kazakhstan" = "#7F1D1D", "Kyrgyzstan" = "#C1440E", "Tajikistan" = "#D9A520",
  "Turkmenistan" = "#B5456E", "Uzbekistan" = "#E8743B",
  "Estonia" = "#0B2E52", "Latvia" = "#3A6EA5", "Lithuania" = "#8FB3D9"
)
country_shapes_wgi <- c(
  "Kazakhstan" = 16, "Kyrgyzstan" = 16, "Tajikistan" = 16,
  "Turkmenistan" = 16, "Uzbekistan" = 16,
  "Estonia" = 15, "Latvia" = 15, "Lithuania" = 15
)

wgi_max_abs <- max(abs(wgi_long$score), na.rm = TRUE) * 1.1

fig_wgi_dotstrip <- ggplot(wgi_long, aes(x = score, y = dimension, color = country, shape = country)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 3) +
  scale_color_manual(values = country_colors_wgi) +
  scale_shape_manual(values = country_shapes_wgi) +
  scale_x_continuous(limits = c(-wgi_max_abs, wgi_max_abs)) +
  scale_y_discrete(limits = rev(wgi_dim_labels)) +
  labs(x = "WGI score (-2.5 to +2.5)", y = NULL, color = NULL, shape = NULL) +
  theme_minimal(base_size = 10)

# --- Regulations table: mandatory rules and voluntary guidance ----------------

regulations_mandatory <- tribble(
  ~id,           ~country_region, ~issuer,                                             ~regulation,                                                                                                    ~level,     ~effective_year, ~scope,                                                     ~disclosure_venue,
  "kaz_mne",     "Kazakhstan",    "Minister of National Economy",                       "MNE Order No. 21",                                                                                            "State",    2018,            "State-controlled, listed joint-stock companies",           "Annual Report or Standalone Report",
  "kaz_fma_86",  "Kazakhstan",    "Financial Market Agency",                            "Resolution No. 86",                                                                                           "State",    2027,            "Commercial banks",                                         "Annual Report or Standalone Report",
  "kaz_kase_m",  "Kazakhstan",    "KASE",                                               "Rules for Information Disclosure",                                                                            "Exchange", 2022,            "Admittance initiators on the \"Main\" market",             "Annual Report",
  "kaz_aix_m",   "Kazakhstan",    "AIX",                                                "AIX Business Rules",                                                                                          "Exchange", 2019,            "ESG-Labelled Bond and Green Bond issuers",                 "Annual Report",
  "uzb_com_221", "Uzbekistan",    "Cabinet of Ministers of the Republic",               "Resolution No. 221",                                                                                          "State",    2026,            "State-participated enterprises",                           "Annual Report or Standalone Report",
  "uzb_mef_3736","Uzbekistan",    "Ministry of Economy and Finance and Central Bank",   "Decision No. 3736",                                                                                            "State",    2027,            "Public interest entities",                                 "Standalone Report",
  "kgz_nbkr_gov","Kyrgyzstan",    "National Bank",                                      "Resolution No. 2024-P-12/71-4-(NPA)", "State", 2024,       "Commercial banks",                                         "Not Specified",
  "kgz_kse_m",   "Kyrgyzstan",    "KSE",                                                "Listing Rules of KSE", "Exchange", 2014, "Issuers of green, social, and other sustainability bonds", "Annual Report or Standalone Report",
  "balt_nfrd",   "Baltic states", "European Parliament & Council of the EU",            "NFRD \u2014 Directive 2014/95/EU",                                                                            "State",    2017,            "Public interest entities exceeding 500 employees",         "Management Report or Standalone Report",
  "balt_csrd",   "Baltic states", "European Parliament & Council of the EU",            "CSRD \u2014 Directive (EU) 2022/2464",                                                                        "State",    2024,            "Public interest entities exceeding 500 employees",         "Management Report"
)

n_affected_mandatory <- c(
  kaz_mne      = sum(smp$country == "Kazakhstan" & smp$mandatory_esg_state == 1, na.rm = TRUE),
  kaz_fma_86   = 0,  # financial firms are outside the sample
  kaz_kase_m   = sum(smp$country == "Kazakhstan" & smp$mandatory_esg_exchange == 1, na.rm = TRUE),
  kaz_aix_m    = 0,  # sample is restricted to shares; no Green/ESG-Labelled Bond issuers
  uzb_com_221  = 0,  # effective 2026, after the FY2024 sample period
  uzb_mef_3736 = 0,  # effective 2027, after the FY2024 sample period
  kgz_nbkr_gov = 0,  # financial firms are outside the sample
  kgz_kse_m    = 0,  # sample is restricted to shares; no sustainability-bond issuers
  balt_nfrd    = 0,  # superseded by CSRD as the binding instrument for FY2024
  balt_csrd    = sum(smp$region == "Baltics" & smp$mandatory_esg_state == 1, na.rm = TRUE)
)

tab_regulations_mandatory <- regulations_mandatory %>%
  mutate(n_affected = n_affected_mandatory[id]) %>%
  select(-id) %>%
  gt() %>%
  cols_label(
    country_region = "Country / Region", issuer = "Issuer", regulation = "Regulation",
    level = "Level", effective_year = "Effective Year", scope = "Scope",
    disclosure_venue = "Disclosure Venue", n_affected = "N Affected"
  )

regulations_guidance <- tribble(
  ~id,           ~country_region, ~issuer,                     ~regulation,                          ~level,     ~effective_year, ~scope,                                          ~disclosure_venue,
  "kaz_fma_291", "Kazakhstan",    "Financial Market Agency",    "ESG Disclosure Guidelines for Banks and Other Financial Institutions", "State", 2023, "Banks and other financial organizations",       "Annual Report or Standalone Report",
  "kaz_kase_g",  "Kazakhstan",    "KASE",                       "Methodology for Preparing an Environmental, Social and Governance Report", "Exchange", 2018, "All listed companies and members of KASE", "Annual Report or Standalone Report",
  "kaz_aix_g",   "Kazakhstan",    "AIX",                        "AIX Voluntary Sustainability Reporting Guidance", "Exchange", 2017,       "All reporting entities on AIX",                 "Annual Report or Standalone Report",
  "kgz_nbkr_sus","Kyrgyzstan",    "National Bank",              "Sustainability Disclosure Guidelines", "State",  2025,                    "Commercial banks, non-bank financial-credit organizations", "Annual Report or Standalone Report",
  "kgz_kse_g",   "Kyrgyzstan",    "KSE",                        "Guidance for Compiling and Publishing Reports on Sustainability, Social Responsibility and Corporate Governance Criteria", "Exchange", 2023, "Companies listed on KSE", "Annual Report or Standalone Report"
)

n_affected_guidance <- c(
  kaz_fma_291  = 0,  # financial firms are outside the sample
  kaz_kase_g   = sum(grepl("KASE", smp$exchange, fixed = TRUE)),
  kaz_aix_g    = sum(grepl("AIX", smp$exchange, fixed = TRUE)),
  kgz_nbkr_sus = 0,  # financial firms are outside the sample
  kgz_kse_g    = sum(grepl("KSE", smp$exchange, fixed = TRUE))
)

tab_regulations_guidance <- regulations_guidance %>%
  mutate(n_affected = n_affected_guidance[id]) %>%
  select(-id) %>%
  gt() %>%
  cols_label(
    country_region = "Country / Region", issuer = "Issuer", regulation = "Regulation",
    level = "Level", effective_year = "Effective Year", scope = "Scope",
    disclosure_venue = "Disclosure Venue", n_affected = "N Affected"
  )


# --- Shared helpers ---------------------------------------------------------------

star_label <- function(p) {
  case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
}

# Central Asia minus Baltic states mean difference, with significance stars from a
# two-sample t-test. Applied identically to binary (0/1) and continuous
# variables.
mean_diff <- function(x, region) {
  test <- tryCatch(t.test(x ~ region), error = function(e) NULL)
  if (is.null(test)) return("\u2014")
  diff <- unname(
    test$estimate["mean in group Central Asia"] -
      test$estimate["mean in group Baltics"]
  )
  paste0(sprintf("%.3f", diff), star_label(test$p.value))
}

group_stats <- function(x) {
  tibble(
    N = sum(!is.na(x)), Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE),
    P5 = quantile(x, 0.05, na.rm = TRUE), Median = median(x, na.rm = TRUE),
    P95 = quantile(x, 0.95, na.rm = TRUE)
  )
}

group_order <- c("Disclosure variables", "Regulatory variables", "Firm characteristics")


# --- Descriptive Statistics. Panel A: binary variables -------------------------

binary_vars <- tibble(
  var_name = c(
    "annual_report", "any_esg", "ESG_separate_report", "ESG_info_annual_report",
    "annual_fin_report",
    "mandatory_esg", "mandatory_esg_state", "mandatory_esg_exchange",
    "voluntary_esg_exchange",
    "foreign_ownership", "state_ownership", "soviet_era", "sensitive_industry"
  ),
  label = c(
    "Integrated Annual Report", "ESG Disclosure", "  ESG Standalone Report", "  ESG in Integrated Annual Report",
    "Annual Financial Disclosure",
    "Mandatory ESG", "Mandatory ESG (State)", "Mandatory ESG (Exchange)",
    "ESG Guidance (Exchange)",
    "Foreign Ownership", "State Ownership", "Soviet-Era Firm", "Sensitive Industry"
  ),
  group = c(
    rep("Disclosure variables", 5),
    rep("Regulatory variables", 4),
    rep("Firm characteristics", 4)
  )
)

binary_row <- function(var, label) {
  x <- smp_reg[[var]]
  r <- smp_reg$region
  tibble(
    label = label,
    N_full = sum(!is.na(x)), Mean_full = mean(x, na.rm = TRUE),
    N_ca = sum(!is.na(x[r == "Central Asia"])),
    Mean_ca = mean(x[r == "Central Asia"], na.rm = TRUE),
    N_baltics = sum(!is.na(x[r == "Baltics"])),
    Mean_baltics = mean(x[r == "Baltics"], na.rm = TRUE),
    Diff = mean_diff(x, r)
  )
}

tab_desc_panel_a <- map2_dfr(binary_vars$var_name, binary_vars$label, binary_row) %>%
  left_join(binary_vars %>% select(label, group), by = "label") %>%
  gt(groupname_col = "group", rowname_col = "label") %>%
  fmt_number(columns = c(Mean_full, Mean_ca, Mean_baltics), decimals = 3) %>%
  cols_label(
    N_full = "N", Mean_full = "Mean", N_ca = "N", Mean_ca = "Mean",
    N_baltics = "N", Mean_baltics = "Mean",
    Diff = "Diff (Central Asia \u2212 Baltic states)"
  ) %>%
  cols_align(align = "left", columns = Diff) %>%
  tab_spanner(label = "Full Sample", columns = c(N_full, Mean_full)) %>%
  tab_spanner(label = "Central Asia", columns = c(N_ca, Mean_ca)) %>%
  tab_spanner(label = "Baltic states", columns = c(N_baltics, Mean_baltics)) %>%
  row_group_order(groups = group_order)


# --- Descriptive Statistics. Panel B: continuous variables (winsorized at 5%/95%) ------

continuous_vars <- tibble(
  var_name = c("ln_total_assets_eur_w", "roa_w"),
  label = c("ln(Total Assets)", "ROA")
)

continuous_block <- function(var, label) {
  x <- smp_reg[[var]]
  r <- smp_reg$region
  bind_rows(
    group_stats(x) %>%
      mutate(row_label = paste(label, "\u2014 Full Sample"), Diff = mean_diff(x, r)),
    group_stats(x[r == "Central Asia"]) %>%
      mutate(row_label = paste(label, "\u2014 Central Asia"), Diff = ""),
    group_stats(x[r == "Baltics"]) %>%
      mutate(row_label = paste(label, "\u2014 Baltic states"), Diff = "")
  )
}

tab_desc_panel_b <- map2_dfr(continuous_vars$var_name, continuous_vars$label, continuous_block) %>%
  select(row_label, N, Mean, SD, P5, Median, P95, Diff) %>%
  gt(rowname_col = "row_label") %>%
  fmt_number(columns = c(Mean, SD, P5, Median, P95), decimals = 3) %>%
  cols_label(Diff = "Diff (Central Asia \u2212 Baltic states)") %>%
  cols_align(align = "left", columns = Diff)

# --- Country-level descriptive statistics (same variables as table 4) --------

country_order_composition <- c(
  "Kazakhstan", "Kyrgyzstan", "Uzbekistan", "Estonia", "Latvia", "Lithuania"
)

# --- Panel A: binary variables, N/Mean per country (column pairs) ------------

binary_row_country <- function(var, label) {
  x <- smp_reg[[var]]
  c <- smp_reg$country
  out <- tibble(label = label)
  for (ctry in country_order_composition) {
    out[[paste0("N_", ctry)]] <- sum(!is.na(x[c == ctry]))
    out[[paste0("Mean_", ctry)]] <- mean(x[c == ctry], na.rm = TRUE)
  }
  out
}

tab_country_panel_a <- map2_dfr(binary_vars$var_name, binary_vars$label, binary_row_country) %>%
  left_join(binary_vars %>% select(label, group), by = "label") %>%
  gt(groupname_col = "group", rowname_col = "label") %>%
  fmt_number(columns = starts_with("Mean_"), decimals = 3) %>%
  row_group_order(groups = group_order)

for (ctry in country_order_composition) {
  tab_country_panel_a <- tab_country_panel_a %>%
    cols_label(!!paste0("N_", ctry) := "N", !!paste0("Mean_", ctry) := "Mean") %>%
    tab_spanner(
      label = ctry,
      columns = c(paste0("N_", ctry), paste0("Mean_", ctry)),
      id = ctry
    )
}

tab_country_panel_a <- tab_country_panel_a %>%
  tab_spanner(label = "Central Asia", spanners = c("Kazakhstan", "Kyrgyzstan", "Uzbekistan")) %>%
  tab_spanner(label = "Baltic states", spanners = c("Estonia", "Latvia", "Lithuania"))

# --- Panel B: continuous variables, countries as columns (trimmed) -----------

continuous_row_country <- function(var, label) {
  x <- smp_reg[[var]]
  c <- smp_reg$country
  mean_row <- tibble(row_label = paste(label, "\u2014 Mean"))
  sd_row   <- tibble(row_label = paste(label, "\u2014 SD"))
  for (ctry in country_order_composition) {
    mean_row[[ctry]] <- mean(x[c == ctry], na.rm = TRUE)
    sd_row[[ctry]]   <- sd(x[c == ctry], na.rm = TRUE)
  }
  bind_rows(mean_row, sd_row)
}

tab_country_panel_b <- map2_dfr(continuous_vars$var_name, continuous_vars$label, continuous_row_country) %>%
  gt(rowname_col = "row_label") %>%
  fmt_number(columns = all_of(country_order_composition), decimals = 3) %>%
  tab_options(column_labels.hidden = TRUE)

# --- Untabulated: characteristics of disclosers vs. non-disclosers -----------
# Same comparison run on two groups: (1) firms with no personal ESG mandate
# ("voluntary disclosers" vs. not), and (2) firms with a personal ESG mandate
# (compliant vs. non-compliant). Uses the disclosure-sample object that feeds
# Table 2 / Figure 1.

compare_disclosers <- function(data, label) {
  summary_tbl <- data %>%
    group_by(any_esg) %>%
    summarise(
      n                      = n(),
      mean_ln_total_assets   = mean(ln_total_assets_eur_w, na.rm = TRUE),
      mean_roa               = mean(roa_w, na.rm = TRUE),
      pct_foreign_ownership  = mean(foreign_ownership, na.rm = TRUE),
      pct_state_ownership    = mean(state_ownership, na.rm = TRUE),
      pct_sensitive_industry = mean(sensitive_industry, na.rm = TRUE),
      pct_soviet_era         = mean(soviet_era, na.rm = TRUE)
    )
  
  log_info(label)
  print(summary_tbl, width = Inf)
  
  print(t.test(ln_total_assets_eur_w ~ any_esg, data = data))
  print(t.test(roa_w ~ any_esg, data = data))
  print(fisher.test(table(data$foreign_ownership, data$any_esg)))
  print(fisher.test(table(data$state_ownership, data$any_esg)))
  print(fisher.test(table(data$sensitive_industry, data$any_esg)))
  print(fisher.test(table(data$soviet_era, data$any_esg)))
}

smp_voluntary <- smp_reg %>% filter(mandatory_esg == 0)
compare_disclosers(smp_voluntary, "Voluntary disclosers vs. non-disclosers (no personal mandate):")

# Country-by-country breakdown of the voluntary-discloser comparison above.
smp_voluntary %>%
  group_by(country, any_esg) %>%
  summarise(
    n                      = n(),
    mean_ln_total_assets   = mean(ln_total_assets_eur_w, na.rm = TRUE),
    mean_roa               = mean(roa_w, na.rm = TRUE),
    pct_foreign_ownership  = mean(foreign_ownership, na.rm = TRUE),
    pct_state_ownership    = mean(state_ownership, na.rm = TRUE),
    pct_sensitive_industry = mean(sensitive_industry, na.rm = TRUE),
    pct_soviet_era         = mean(soviet_era, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, any_esg) %>%
  print(n = Inf, width = Inf)

smp_mandated <- smp_reg %>% filter(mandatory_esg == 1)
compare_disclosers(smp_mandated, "Compliant vs. non-compliant mandated firms:")

# Same country-by-country breakdown, for the mandated-firms comparison.
smp_mandated %>%
  group_by(country, any_esg) %>%
  summarise(
    n                      = n(),
    mean_ln_total_assets   = mean(ln_total_assets_eur_w, na.rm = TRUE),
    mean_roa               = mean(roa_w, na.rm = TRUE),
    pct_foreign_ownership  = mean(foreign_ownership, na.rm = TRUE),
    pct_state_ownership    = mean(state_ownership, na.rm = TRUE),
    pct_sensitive_industry = mean(sensitive_industry, na.rm = TRUE),
    pct_soviet_era         = mean(soviet_era, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, any_esg) %>%
  print(n = Inf, width = Inf)

# --- Correlation table -------------------------------------------------------

corr_vars <- tibble(
  var_name = c(
    "any_esg", "mandatory_esg", "mandatory_esg_state", "mandatory_esg_exchange",
    "voluntary_esg_exchange", "foreign_ownership", "state_ownership",
    "soviet_era", "sensitive_industry", "ln_total_assets_eur_w", "roa_w",
    "region_num", "wgi_rule_of_law"
  ),
  label = c(
    "ESG Disclosure", "Mandatory ESG", "Mandatory ESG (State)",
    "Mandatory ESG (Exchange)", "ESG Guidance (Exchange)", "Foreign Ownership",
    "State Ownership", "Soviet-Era Firm", "Sensitive Industry",
    "ln(Total Assets)", "ROA", "Region (Central Asia)",
    "Rule of Law"
  )
)

rlabels <- paste0("(", seq_len(nrow(corr_vars)), ") ", corr_vars$label)

# region_num: Central Asia = 1, Baltic states = 0

corr_mat <- smp_reg %>%
  mutate(region_num = as.integer(region == "Central Asia")) %>%
  select(all_of(corr_vars$var_name))
colnames(corr_mat) <- rlabels

pearson_with_stars <- function(data) {
  n <- ncol(data)
  out <- matrix("", n, n, dimnames = list(colnames(data), colnames(data)))
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        out[i, j] <- "1.000"
      } else if (i > j) {
        test <- cor.test(data[[i]], data[[j]])
        out[i, j] <- paste0(sprintf("%.3f", test$estimate), star_label(test$p.value))
      }
    }
  }
  as_tibble(out, rownames = " ")
}

tab_corr <- pearson_with_stars(corr_mat) %>%
  gt(rowname_col = " ") %>%
  cols_label(!!!setNames(paste0("(", seq_len(nrow(corr_vars)), ")"), rlabels))

# --- Variable definitions table -----------------------------------------------

var_definitions_order <- c(
  "Disclosure variables", "Regulatory variables",
  "Firm characteristics", "Country-level variables"
)

var_definitions <- tribble(
  ~variable, ~definition, ~source, ~group,
  
  "Integrated Annual Report",
  "Indicator equal to one if a firm discloses an integrated annual report, and zero otherwise.",
  "Hand-collected", "Disclosure variables",
  
  "ESG Disclosure",
  "Indicator equal to one if a firm discloses ESG information either within its integrated annual report or in a standalone ESG report, and zero otherwise. Constructed as the union of ESG information disclosed within the integrated annual report and a standalone ESG report.",
  "Hand-collected", "Disclosure variables",
  
  "ESG Standalone Report",
  "Indicator equal to one if a firm publishes a standalone ESG or sustainability report, and zero otherwise.",
  "Hand-collected", "Disclosure variables",
  
  "ESG in Integrated Annual Report",
  "Indicator equal to one if a firm's integrated annual report contains ESG-related information, and zero otherwise.",
  "Hand-collected", "Disclosure variables",
  
  "Annual Financial Disclosure",
  "Indicator equal to one if a firm discloses an annual financial report, and zero otherwise.",
  "Hand-collected", "Disclosure variables",
  
  # "Interim Financial Disclosure",
  # "Indicator equal to one if a firm discloses any interim (e.g., quarterly or semi-annual) financial report, and zero otherwise.",
  # "Hand-collected", "Disclosure variables",
  
  "Mandatory ESG",
  "Indicator equal to one if a firm is subject to an ESG disclosure mandate from either the state or its stock exchange, and zero otherwise. Combines the state- and exchange-level mandate variables via an OR rule.",
  "Hand-collected; national regulatory texts and exchange rules", "Regulatory variables",
  
  "Mandatory ESG (State)",
  "Indicator equal to one if a firm is subject to a state-issued mandatory ESG disclosure requirement, and zero otherwise.",
  "Hand-collected; national regulatory texts", "Regulatory variables",
  
  "Mandatory ESG (Exchange)",
  "Indicator equal to one if a firm's stock exchange imposes a binding ESG disclosure requirement as a listing rule, and zero otherwise.",
  "Hand-collected; exchange listing rules", "Regulatory variables",
  
  "ESG Guidance (Exchange)",
  "Indicator equal to one if a firm's stock exchange has published voluntary, non-binding ESG reporting guidance for listed firms, and zero otherwise.",
  "Hand-collected; exchange guidance documents", "Regulatory variables",
  
  "Country ESG Mandate",
  "Indicator equal to one if a binding ESG disclosure mandate - state or exchange-issued - is in force for FY2024 in the firm's country, for at least part of its listed non-financial firm population, regardless of whether this specific firm is itself bound by it, and zero otherwise.",
  "Hand-collected; national regulatory texts and exchange listing rules", "Regulatory variables",
  
  "Foreign Ownership",
  "Indicator that equals one if the firm has an identified foreign (non-domestic) ownership stake at or above the country-specific disclosure threshold, and zero otherwise. Thresholds reflect the minimum ownership stake disclosed under each country's own regulations: Kazakhstan, Kyrgyzstan, Latvia, Lithuania 5%; Estonia 10%; Uzbekistan 20%.",
  "Hand-collected; company annual reports, national business registers", "Firm characteristics",
  
  "State Ownership",
  "Indicator that equals one if the firm has an identified state ownership stake at or above the country-specific disclosure threshold, and zero otherwise. Thresholds reflect the minimum ownership stake disclosed under each country's own regulations: Kazakhstan, Kyrgyzstan, Latvia, Lithuania 5%; Estonia 10%; Uzbekistan 20%.",
  "Hand-collected; company annual reports, national business registers", "Firm characteristics",
  
  "Soviet-Era Firm",
  "Indicator equal to one if a firm's founding or incorporation date, per any of three independently checked sources (hand-collected founding year, Orbis incorporation year, or an AI-assisted check), is prior to 1992, and zero if every source that returned a determination places the firm's origin at or after 1992. Combined via OR logic across the three sources.",
  "Hand-collected; national business registers, Orbis; AI-assisted verification", "Firm characteristics",
  
  "Sensitive Industry",
  "Indicator equal to one if a firm operates in an environmentally sensitive industry (ICB industry: Energy, Basic Materials, or Utilities), and zero otherwise.",
  "ICB industry classification, Jain and Malhotra (2026)", "Firm characteristics",
  
  "ln(Total Assets)",
  "Natural logarithm of total assets, converted to EUR using firm-specific exchange rates as of the year-end. Winsorized at the 5th/95th percentile.",
  "Hand-collected; company annual reports; national central bank exchange rates", "Firm characteristics",
  
  "ROA",
  "Net income divided by total assets, both converted to EUR using firm-specific exchange rates as of the year-end. Winsorized at the 5th/95th percentile.",
  "Hand-collected annual reports", "Firm characteristics",
  
  "Region",
  "Factor identifying whether a firm's home country is in the Baltic states (Estonia, Latvia, Lithuania) or Central Asia (Kazakhstan, Uzbekistan, Kyrgyzstan, Tajikistan, Turkmenistan)",
  "Author's classification", "Country-level variables",
  
  "Rule of Law",
  "Captures perceptions of the extent to which agents respect and follow the rules of society, including contract enforcement, property rights, the police, courts, and the likelihood of crime and violence. Governance estimate from the aggregation model, in units of a standard normal distribution, i.e. ranging from approximately -2.5 to 2.5. Larger values correspond to better governance.",
  "World Bank, Worldwide Governance Indicators", "Country-level variables",
  
  "Control of Corruption",
  "Captures perceptions of the extent to which public power is used for private gain, including both petty and grand corruption, as well as capture of the state by elites and private interests. Governance estimate from the aggregation model, in units of a standard normal distribution, i.e. ranging from approximately -2.5 to 2.5. Larger values correspond to better governance.",
  "World Bank, Worldwide Governance Indicators", "Country-level variables",
  
  "Government Effectiveness",
  "Captures perceptions of the quality of public services, the civil service, policy formulation and implementation, and the credibility of a government's decisions. Governance estimate from the aggregation model, in units of a standard normal distribution, i.e. ranging from approximately -2.5 to 2.5. Larger values correspond to better governance.",
  "World Bank, Worldwide Governance Indicators", "Country-level variables",
  
  "Political Stability",
  "Captures perceptions of the extent to which political power and governance are secure from destabilization, and of the likelihood that authority will be challenged or altered through violent, coercive, or unconstitutional means. Governance estimate from the aggregation model, in units of a standard normal distribution, i.e. ranging from approximately -2.5 to 2.5. Larger values correspond to better governance.",
  "World Bank, Worldwide Governance Indicators", "Country-level variables",
  
  "Regulatory Quality",
  "Captures perceptions of the government's ability to design and implement policies and regulations that promote private sector development. Governance estimate from the aggregation model, in units of a standard normal distribution, i.e. ranging from approximately -2.5 to 2.5. Larger values correspond to better governance.",
  "World Bank, Worldwide Governance Indicators", "Country-level variables",
  
  "Voice and Accountability",
  "Captures perceptions of the extent to which citizens can participate in selecting their government including electoral integrity, and of accountability mechanisms for citizens - reflected in the ability to access information, governmental oversight bodies, and a robust traditional/digital media landscape. Governance estimate from the aggregation model, in units of a standard normal distribution, i.e. ranging from approximately -2.5 to 2.5. Larger values correspond to better governance.",
  "World Bank, Worldwide Governance Indicators", "Country-level variables"
  
  # "ln(GDP per capita)",
  # "Natural logarithm of GDP per capita, PPP-adjusted (current international $), by country.",
  # "World Bank", "Country-level variables"
)

tab_var_definitions <- var_definitions %>%
  gt(groupname_col = "group", rowname_col = "variable") %>%
  tab_stubhead(label = "Variable Name") %>%
  cols_label(definition = "Definition", source = "Source") %>%
  cols_width(
    definition ~ pct(55),
    source ~ pct(20)
  ) %>%
  row_group_order(groups = var_definitions_order)

# --- Regression analysis ------------------------------------------------------

mods_fe <- list(
  "(1)" = feols(any_esg ~ mandatory_esg | country, data = smp_reg),
  "(2)" = feols(any_esg ~ mandatory_esg + sensitive_industry +
                  ln_total_assets_eur_w + roa_w | country, data = smp_reg),
  "(3)" = feols(any_esg ~ mandatory_esg + sensitive_industry +
                  ln_total_assets_eur_w + roa_w + foreign_ownership +
                  state_ownership | country, data = smp_reg),
  "(4)" = feols(any_esg ~ mandatory_esg + sensitive_industry +
                  ln_total_assets_eur_w + roa_w + foreign_ownership +
                  state_ownership + soviet_era | country, data = smp_reg)
)

mods_pooled <- list(
  "(5)" = lm(any_esg ~ region + mandatory_esg, data = smp_reg),
  "(6)" = lm(any_esg ~ region + mandatory_esg + sensitive_industry +
               ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(7)" = lm(any_esg ~ region + mandatory_esg + sensitive_industry +
               ln_total_assets_eur_w + roa_w + foreign_ownership +
               state_ownership, data = smp_reg),
  "(8)" = lm(any_esg ~ region + mandatory_esg + sensitive_industry +
               ln_total_assets_eur_w + roa_w + foreign_ownership +
               state_ownership + soviet_era, data = smp_reg),
  "(9)" = lm(any_esg ~ wgi_rule_of_law + mandatory_esg + sensitive_industry +
               ln_total_assets_eur_w + roa_w + foreign_ownership +
               state_ownership + soviet_era, data = smp_reg)
)

mods_main <- c(mods_fe, mods_pooled)

var_labels <- c(
  "(Intercept)"             = "Intercept",
  "mandatory_esg"           = "Mandatory ESG",
  "foreign_ownership"       = "Foreign Ownership",
  "state_ownership"         = "State Ownership",
  "soviet_era"              = "Soviet-Era Firm",
  "sensitive_industry"      = "Sensitive Industry",
  "ln_total_assets_eur_w"   = "ln(Total Assets)",
  "roa_w"                   = "ROA",
  "regionCentral Asia"      = "Region (Central Asia)",
  "wgi_rule_of_law"         = "Rule of Law"
)

fe_row <- tibble(
  term = "Country Fixed Effects",
  `(1)` = "Yes", `(2)` = "Yes", `(3)` = "Yes", `(4)` = "Yes",
  `(5)` = "No",  `(6)` = "No",  `(7)` = "No",  `(8)` = "No", `(9)` = "No"
)
attr(fe_row, "position") <- 21

tab_reg_main <- modelsummary(
  mods_main,
  vcov = "HC1",
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.10),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = var_labels,
  add_rows = fe_row,
  gof_map = list(
    list(raw = "nobs", clean = "Observations", fmt = 0),
    list(raw = "adj.r.squared", clean = "Adj. R\u00b2 (overall)", fmt = function(x) sprintf("%.3f", x)),
    list(raw = "r2.within.adjusted", clean = "Adj. R\u00b2 (within)", fmt = function(x) sprintf("%.3f", x))
  ),
  output = "gt"
) %>%
  tab_spanner(
    label = "Within-Country (Country FE)",
    columns = c(`(1)`, `(2)`, `(3)`, `(4)`)
  ) %>%
  tab_spanner(
    label = "Between-Country (Pooled)",
    columns = c(`(5)`, `(6)`, `(7)`, `(8)`, `(9)`)
  )

# --- Variance Inflation Factors -------------------------------------------------

vif_check_models <- c("(7)", "(8)", "(9)")

for (m in vif_check_models) {
  vifs <- car::vif(mods_pooled[[m]])
  log_info(
    "VIF, Model {m}: {paste(names(vifs), round(vifs, 2), sep = '=', collapse = ', ')}"
  )
}

# --- Wald test: State Ownership vs. Foreign Ownership (Model 4 only) ----------

b4 <- coef(mods_fe[["(4)"]])
V4 <- vcov(mods_fe[["(4)"]], vcov = "HC1")
diff_4 <- b4["state_ownership"] - b4["foreign_ownership"]
se_diff_4 <- sqrt(
  V4["state_ownership", "state_ownership"] + V4["foreign_ownership", "foreign_ownership"] -
    2 * V4["state_ownership", "foreign_ownership"]
)
t_stat_4 <- diff_4 / se_diff_4
wald_4_p <- 2 * pt(-abs(t_stat_4), df = df.residual(mods_fe[["(4)"]]))
log_info("Wald test, Model (4) State = Foreign Ownership: p = {round(wald_4_p, 3)}")

# --- Untabulated: state ownership and disclosure, by country -----------------

smp_reg %>%
  group_by(country, state_ownership) %>%
  summarise(n = n(), pct_esg_disclosure = mean(any_esg, na.rm = TRUE), .groups = "drop") %>%
  arrange(country, state_ownership) %>%
  print(n = Inf, width = Inf)

# --- Untabulated: winsorization robustness (1st/99th vs. 5th/95th) -----------

winsorize <- function(x, probs = c(0.01, 0.99)) {
  bounds <- quantile(x, probs, na.rm = TRUE)
  pmin(pmax(x, bounds[1]), bounds[2])
}

smp_reg_w2 <- smp_reg %>%
  mutate(
    ln_total_assets_eur_w2 = winsorize(ln_total_assets_eur),
    roa_w2 = winsorize(roa)
  )

mods_fe_w2 <- list(
  "(2)" = feols(any_esg ~ mandatory_esg + sensitive_industry +
                  ln_total_assets_eur_w2 + roa_w2 | country, data = smp_reg_w2),
  "(3)" = feols(any_esg ~ mandatory_esg + sensitive_industry +
                  ln_total_assets_eur_w2 + roa_w2 + foreign_ownership +
                  state_ownership | country, data = smp_reg_w2),
  "(4)" = feols(any_esg ~ mandatory_esg + sensitive_industry +
                  ln_total_assets_eur_w2 + roa_w2 + foreign_ownership +
                  state_ownership + soviet_era | country, data = smp_reg_w2)
)

mods_pooled_w2 <- list(
  "(6)" = lm(any_esg ~ region + mandatory_esg + sensitive_industry +
               ln_total_assets_eur_w2 + roa_w2, data = smp_reg_w2),
  "(7)" = lm(any_esg ~ region + mandatory_esg + sensitive_industry +
               ln_total_assets_eur_w2 + roa_w2 + foreign_ownership +
               state_ownership, data = smp_reg_w2),
  "(8)" = lm(any_esg ~ region + mandatory_esg + sensitive_industry +
               ln_total_assets_eur_w2 + roa_w2 + foreign_ownership +
               state_ownership + soviet_era, data = smp_reg_w2),
  "(9)" = lm(any_esg ~ wgi_rule_of_law + mandatory_esg + sensitive_industry +
               ln_total_assets_eur_w2 + roa_w2 + foreign_ownership +
               state_ownership + soviet_era, data = smp_reg_w2)
)

mods_w2 <- c(mods_fe_w2, mods_pooled_w2)

for (m in names(mods_w2)) {
  log_info(
    "Winsorization check (1st/99th), Mandatory ESG {m}: ",
    "{round(coef(mods_w2[[m]])['mandatory_esg'], 3)} ",
    "(original 5th/95th: {round(coef(mods_main[[m]])['mandatory_esg'], 3)})"
  )
}

# --- Untabulated: Mandatory ESG disaggregated into state vs. exchange --------

mod_disagg_fe4 <- feols(
  any_esg ~ mandatory_esg_state + mandatory_esg_exchange + sensitive_industry +
    ln_total_assets_eur_w + roa_w + foreign_ownership + state_ownership +
    soviet_era | country,
  data = smp_reg
)

mod_disagg_pooled8 <- lm(
  any_esg ~ region + mandatory_esg_state + mandatory_esg_exchange +
    sensitive_industry + ln_total_assets_eur_w + roa_w + foreign_ownership +
    state_ownership + soviet_era,
  data = smp_reg
)

vif_disagg <- car::vif(mod_disagg_pooled8)
log_info(
  "Disaggregated Mandatory ESG, pooled(8): State = ",
  "{round(coef(mod_disagg_pooled8)['mandatory_esg_state'], 3)}, Exchange = ",
  "{round(coef(mod_disagg_pooled8)['mandatory_esg_exchange'], 3)}; VIF: ",
  "{paste(names(vif_disagg), round(vif_disagg, 2), sep = '=', collapse = ', ')}"
)
log_info(
  "Disaggregated Mandatory ESG, FE(4): State = ",
  "{round(coef(mod_disagg_fe4)['mandatory_esg_state'], 3)}, Exchange = ",
  "{round(coef(mod_disagg_fe4)['mandatory_esg_exchange'], 3)}"
)


# --- Robustness: Firth's penalized logit, Panel B specifications --------------

tidy.logistf <- function(x, ...) {
  tibble(
    term = names(x$coefficients),
    estimate = unname(x$coefficients),
    std.error = sqrt(diag(x$var)),
    p.value = unname(x$prob)
  )
}

glance.logistf <- function(x, ...) {
  tibble(
    nobs = x$n,
    pseudo_r2 = 1 - unname(x$loglik["full"] / x$loglik["null"])
  )
}

mods_logit <- list(
  "(5)" = logistf(any_esg ~ region + mandatory_esg, data = smp_reg),
  "(6)" = logistf(any_esg ~ region + mandatory_esg + sensitive_industry +
                    ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(7)" = logistf(any_esg ~ region + mandatory_esg + sensitive_industry +
                    ln_total_assets_eur_w + roa_w + foreign_ownership +
                    state_ownership, data = smp_reg),
  "(8)" = logistf(any_esg ~ region + mandatory_esg + sensitive_industry +
                    ln_total_assets_eur_w + roa_w + foreign_ownership +
                    state_ownership + soviet_era, data = smp_reg),
  "(9)" = logistf(any_esg ~ wgi_rule_of_law + mandatory_esg + sensitive_industry +
                    ln_total_assets_eur_w + roa_w + foreign_ownership +
                    state_ownership + soviet_era, data = smp_reg)
)

pseudo_r2_row <- tibble(
  term = "Pseudo R\u00b2",
  `(5)` = sprintf("%.3f", glance(mods_logit[["(5)"]])$pseudo_r2),
  `(6)` = sprintf("%.3f", glance(mods_logit[["(6)"]])$pseudo_r2),
  `(7)` = sprintf("%.3f", glance(mods_logit[["(7)"]])$pseudo_r2),
  `(8)` = sprintf("%.3f", glance(mods_logit[["(8)"]])$pseudo_r2),
  `(9)` = sprintf("%.3f", glance(mods_logit[["(9)"]])$pseudo_r2)
)

tab_reg_robustness <- modelsummary(
  mods_logit,
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.10),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = var_labels,
  gof_map = list(list(raw = "nobs", clean = "Observations", fmt = 0)),
  add_rows = pseudo_r2_row,
  output = "gt"
)


# --- Heterogeneity by region: interactions ------------------------------------

implied_effect <- function(model, base_term, interaction_term, level = 0.95) {
  b <- coef(model)
  V <- vcov(model, vcov = "HC1")
  implied <- unname(b[base_term] + b[interaction_term])
  se_implied <- unname(sqrt(
    V[base_term, base_term] + V[interaction_term, interaction_term] +
      2 * V[base_term, interaction_term]
  ))
  se_interaction <- unname(sqrt(V[interaction_term, interaction_term]))
  t_interaction <- unname(b[interaction_term]) / se_interaction
  df <- df.residual(model)
  crit <- qt(1 - (1 - level) / 2, df)
  tibble(
    implied_effect = implied,
    ci_low = implied - crit * se_implied,
    ci_high = implied + crit * se_implied,
    p_diff = 2 * pt(-abs(t_interaction), df)
  )
}

mods_het <- list(
  "(1)" = feols(
    any_esg ~ mandatory_esg + mandatory_esg:region + sensitive_industry +
      ln_total_assets_eur_w + roa_w | country, data = smp_reg
  ),
  "(2)" = feols(
    any_esg ~ foreign_ownership + foreign_ownership:region + sensitive_industry +
      ln_total_assets_eur_w + roa_w | country, data = smp_reg
  ),
  "(3)" = feols(
    any_esg ~ state_ownership + state_ownership:region + sensitive_industry +
      ln_total_assets_eur_w + roa_w | country, data = smp_reg
  ),
  "(4)" = feols(
    any_esg ~ soviet_era + soviet_era:region + sensitive_industry +
      ln_total_assets_eur_w + roa_w | country, data = smp_reg
  )
)

implied_1 <- implied_effect(mods_het[[1]], "mandatory_esg", "mandatory_esg:regionCentral Asia")
implied_2 <- implied_effect(mods_het[[2]], "foreign_ownership", "foreign_ownership:regionCentral Asia")
implied_3 <- implied_effect(mods_het[[3]], "state_ownership", "state_ownership:regionCentral Asia")
implied_4 <- implied_effect(mods_het[[4]], "soviet_era", "soviet_era:regionCentral Asia")

log_info(
  "Heterogeneity (Mandatory ESG x Region): implied Central Asia effect = ",
  "{round(implied_1$implied_effect, 3)} [{round(implied_1$ci_low, 3)}, ",
  "{round(implied_1$ci_high, 3)}], p (difference) = {round(implied_1$p_diff, 3)}."
)
log_info(
  "Heterogeneity (Foreign Ownership x Region): implied Central Asia effect = ",
  "{round(implied_2$implied_effect, 3)} [{round(implied_2$ci_low, 3)}, ",
  "{round(implied_2$ci_high, 3)}], p (difference) = {round(implied_2$p_diff, 3)}."
)
log_info(
  "Heterogeneity (State Ownership x Region): implied Central Asia effect = ",
  "{round(implied_3$implied_effect, 3)} [{round(implied_3$ci_low, 3)}, ",
  "{round(implied_3$ci_high, 3)}], p (difference) = {round(implied_3$p_diff, 3)}."
)
log_info(
  "Heterogeneity (Soviet-Era x Region): implied Central Asia effect = ",
  "{round(implied_4$implied_effect, 3)} [{round(implied_4$ci_low, 3)}, ",
  "{round(implied_4$ci_high, 3)}], p (difference) = {round(implied_4$p_diff, 3)}."
)

var_labels_het <- c(
  "mandatory_esg"                        = "Mandatory ESG",
  "mandatory_esg:regionCentral Asia"     = "Mandatory ESG \u00d7 Central Asia",
  "foreign_ownership"                    = "Foreign Ownership",
  "foreign_ownership:regionCentral Asia" = "Foreign Ownership \u00d7 Central Asia",
  "state_ownership"                      = "State Ownership",
  "state_ownership:regionCentral Asia"   = "State Ownership \u00d7 Central Asia",
  "soviet_era"                           = "Soviet-Era Firm",
  "soviet_era:regionCentral Asia"        = "Soviet-Era Firm \u00d7 Central Asia",
  "sensitive_industry"                   = "Sensitive Industry",
  "ln_total_assets_eur_w"                = "ln(Total Assets)",
  "roa_w"                                = "ROA"
)

implied_rows <- tibble(
  term = c(
    "Marginal Effect (Central Asia)", "95% CI (Central Asia)",
    "Country Fixed Effects"
  ),
  `(1) Mandatory ESG` = c(
    sprintf("%.3f", implied_1$implied_effect),
    sprintf("[%.3f, %.3f]", implied_1$ci_low, implied_1$ci_high),
    "Yes"
  ),
  `(2) Foreign Ownership` = c(
    sprintf("%.3f", implied_2$implied_effect),
    sprintf("[%.3f, %.3f]", implied_2$ci_low, implied_2$ci_high),
    "Yes"
  ),
  `(3) State Ownership` = c(
    sprintf("%.3f", implied_3$implied_effect),
    sprintf("[%.3f, %.3f]", implied_3$ci_low, implied_3$ci_high),
    "Yes"
  ),
  `(4) Soviet-Era Firm` = c(
    sprintf("%.3f", implied_4$implied_effect),
    sprintf("[%.3f, %.3f]", implied_4$ci_low, implied_4$ci_high),
    "Yes"
  )
)
attr(implied_rows, "position") <- 23

tab_reg_heterogeneity <- modelsummary(
  mods_het,
  vcov = "HC1",
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.10),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = var_labels_het,
  add_rows = implied_rows,
  gof_map = list(
    list(raw = "nobs", clean = "Observations", fmt = 0),
    list(raw = "adj.r.squared", clean = "Adj. R\u00b2", fmt = function(x) sprintf("%.3f", x))
  ),
  output = "gt"
)


# --- Additional analysis: regulatory spillover effects ------------------------

# Sample restricted to non-mandated firms only.

mods_spillover_check <- lm(
  any_esg ~ mandatory_esg + esg_mandate_in_force + sensitive_industry +
    ln_total_assets_eur_w + roa_w + foreign_ownership + state_ownership + soviet_era,
  data = smp_reg
)
b_sp <- coef(mods_spillover_check)
V_sp <- sandwich::vcovHC(mods_spillover_check, type = "HC1")
se_mandatory_sp <- sqrt(V_sp["mandatory_esg", "mandatory_esg"])
t_mandatory_sp <- b_sp["mandatory_esg"] / se_mandatory_sp
wald_sp_p <- 2 * pt(-abs(t_mandatory_sp), df = df.residual(mods_spillover_check))
log_info("Wald test, Mandatory ESG = Country ESG Mandate: p = {round(wald_sp_p, 3)}")

smp_spillover <- smp_reg %>% filter(mandatory_esg == 0)

mods_spillover <- list(
  "(1)" = lm(any_esg ~ esg_mandate_in_force + sensitive_industry +
               ln_total_assets_eur_w + roa_w, data = smp_spillover),
  "(2)" = lm(any_esg ~ esg_mandate_in_force + sensitive_industry +
               ln_total_assets_eur_w + roa_w + foreign_ownership +
               state_ownership + soviet_era, data = smp_spillover),
  "(3)" = lm(any_esg ~ esg_mandate_in_force + sensitive_industry +
               ln_total_assets_eur_w + roa_w + foreign_ownership +
               state_ownership + soviet_era + wgi_rule_of_law, data = smp_spillover)
)

vifs_spillover <- car::vif(mods_spillover[["(3)"]])
log_info(
  "VIF, Spillover Model (3): {paste(names(vifs_spillover), round(vifs_spillover, 2), sep = '=', collapse = ', ')}"
)

var_labels_spillover <- c(
  "(Intercept)"           = "Intercept",
  "esg_mandate_in_force"  = "Country ESG Mandate",
  "foreign_ownership"     = "Foreign Ownership",
  "state_ownership"       = "State Ownership",
  "soviet_era"            = "Soviet-Era Firm",
  "sensitive_industry"    = "Sensitive Industry",
  "ln_total_assets_eur_w" = "ln(Total Assets)",
  "roa_w"                 = "ROA",
  "wgi_rule_of_law"       = "Rule of Law"
)

tab_reg_spillover <- modelsummary(
  mods_spillover,
  vcov = "HC1",
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.10),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = var_labels_spillover,
  gof_map = list(
    list(raw = "nobs", clean = "Observations", fmt = 0),
    list(raw = "adj.r.squared", clean = "Adj. R\u00b2", fmt = function(x) sprintf("%.3f", x))
  ),
  output = "gt"
)

# --- Untabulated: leave-Kyrgyzstan-out, spillover specification --------------

smp_spillover_noKGZ <- smp_spillover %>% filter(country != "Kyrgyzstan")

mod_spillover_noKGZ <- lm(
  any_esg ~ esg_mandate_in_force + sensitive_industry +
    ln_total_assets_eur_w + roa_w + foreign_ownership +
    state_ownership + soviet_era,
  data = smp_spillover_noKGZ
)

V_noKGZ <- sandwich::vcovHC(mod_spillover_noKGZ, type = "HC1")
se_noKGZ <- sqrt(diag(V_noKGZ)["esg_mandate_in_force"])
t_noKGZ <- coef(mod_spillover_noKGZ)["esg_mandate_in_force"] / se_noKGZ
p_noKGZ <- 2 * pt(-abs(t_noKGZ), df = df.residual(mod_spillover_noKGZ))

log_info(
  "Leave-Kyrgyzstan-out (spillover): N = {nrow(smp_spillover_noKGZ)}, ",
  "Country ESG Mandate = {round(coef(mod_spillover_noKGZ)['esg_mandate_in_force'], 3)}, ",
  "SE = {round(se_noKGZ, 3)}, p = {round(p_noKGZ, 3)} ",
  "(full spillover sample estimate, column (2): ",
  "{round(coef(mods_spillover[['(2)']])['esg_mandate_in_force'], 3)})."
)

# --- Save -------------------------------------------------------------------------

log_info("Done. Storing output in '{global_cfg$results_r}'")

save(list = c(
  "tab_sample_selection_a", "tab_sample_selection_b", "tab_disclosure_coverage",
  "tab_regulations_mandatory", "tab_regulations_guidance", "fig_esg_disclosure",
  "fig_esg_compliance", "fig_fin_disclosure", "fig_wgi_dotstrip", "tab_desc_panel_a",
  "tab_desc_panel_b", "tab_country_panel_a", "tab_country_panel_b", "tab_corr", "tab_var_definitions",
  "tab_reg_main", "tab_reg_robustness", "tab_reg_heterogeneity", "tab_reg_spillover"),
  file = global_cfg$results_r)

