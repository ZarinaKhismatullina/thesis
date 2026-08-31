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
  "TLN",     "Estonia",      "Baltic States",
  "RIG",     "Latvia",       "Baltic States",
  "VLN",     "Lithuania",    "Baltic States"
)
exchanges <- exchange_map$exchange

sample_selection <- tribble(
  ~step,                                                                              ~KASE, ~AIX, ~UZSE, ~KSE, ~BTS, ~AGB, ~CASE, ~TLN, ~RIG, ~VLN,
  "Initial number of observations (total listed firms)",                                 92,   53,   92,   38,   16,    7,    0,   31,   13,   25,
  "Less: Foreign listings",                                                              -7,  -11,    0,    0,    0,    0,    0,    0,    0,    0,
  "Less: Non-equity instruments (bonds), ETFs",                                            0,  -29,  -13,  -12,   -2,   -2,    0,    0,    0,    0,
  "Less: Duplicate observations (companies with more than one ticker/ISIN)",             -13,   -2,    0,    0,    0,    0,    0,    0,    0,    0,
  "Less: Financial companies",                                                           -22,    0,  -28,  -12,   -3,   -5,    0,   -4,   -3,   -3,
  "Less: Duplicate observations (companies listed on more than one domestic exchange)",    0,   -9,    0,    0,   -3,    0,    0,    0,    0,    0,
  "Less: Irrelevant market categories/segments",                                            0,    0,    0,   -2,    0,    0,    0,  -11,   -5,   -3,
  "Less: Not listed as of FY2024 (including missing listing date)",                            -2,    0,   -8,   -2,   -8,    0,    0,   -1,   -1,    0
)

# Total per exchange: initial count plus all attrition steps above.
total_exchange <- sample_selection %>%
  summarise(across(all_of(exchanges), sum)) %>%
  mutate(step = "Total number of observations per stock exchange", .before = 1)

total_row <- function(label, group_var = NULL) {
  long <- total_exchange %>%
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
  total_row("Total number of observations per country", "country"),
  total_row("Total number of observations per region", "region"),
  total_row("Grand total (all regions)")
)

tab_sample_selection <- sample_selection %>%
  gt(rowname_col = "step") %>%
  # Zero -> "-", negative -> "(n)", NA -> blank - standard attrition-table
  # formatting; kept here since it's data formatting, not a note/title.
  fmt(
    columns = all_of(exchanges),
    fns = function(x) case_when(
      is.na(x) ~ "", x == 0 ~ "-", x < 0 ~ paste0("(", abs(x), ")"),
      TRUE ~ as.character(x)
    )
  ) %>%
  tab_spanner(label = "Kazakhstan",   columns = c(KASE, AIX), id = "kaz") %>%
  tab_spanner(label = "Uzbekistan",   columns = c(UZSE),      id = "uzb") %>%
  tab_spanner(label = "Kyrgyzstan",   columns = c(KSE, BTS),  id = "kgz") %>%
  tab_spanner(label = "Turkmenistan", columns = c(AGB),       id = "tkm") %>%
  tab_spanner(label = "Tajikistan",   columns = c(CASE),      id = "tjk") %>%
  tab_spanner(label = "Estonia",      columns = c(TLN),       id = "est") %>%
  tab_spanner(label = "Latvia",       columns = c(RIG),       id = "lva") %>%
  tab_spanner(label = "Lithuania",    columns = c(VLN),       id = "ltu") %>%
  tab_spanner(label = "Central Asia",  spanners = c("kaz", "uzb", "kgz", "tkm", "tjk")) %>%
  tab_spanner(label = "Baltic States", spanners = c("est", "lva", "ltu"))


# --- Read data ----------------------------------------------------------------

log_info("Reading data...")
smp <- read_parquet(global_cfg$base_sample)


# --- Disclosure coverage by region and country --------------------------------

to01 <- function(x) suppressWarnings(as.integer(x))

disclosure_counts <- function(data) {
  data %>%
    summarise(
      N = n(),
      `Annual report disclosed` = sum(to01(annual_report) == 1, na.rm = TRUE),
      `Any ESG information disclosed` = sum(to01(any_esg) == 1, na.rm = TRUE),
      `(a) Standalone ESG report disclosed` = sum(to01(ESG_separate_report) == 1, na.rm = TRUE),
      `(b) ESG information disclosed within annual report` = sum(to01(ESG_info_annual_report) == 1, na.rm = TRUE),
      `(c) Both (a) and (b) disclosed` = sum(
        to01(ESG_separate_report) == 1 & to01(ESG_info_annual_report) == 1,
        na.rm = TRUE
      ),
      `Annual financial report disclosed` = sum(to01(annual_fin_report) == 1, na.rm = TRUE),
      `Interim financial report disclosed` = sum(to01(interim_fin_reports) == 1, na.rm = TRUE)
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
    region = if_else(region == "Central Asia", "Central Asia (Total)", "Baltic States (Total)")
  ) %>%
  pivot_wider(names_from = region, values_from = n)

all_counts <- disclosure_counts(smp) %>%
  mutate(country = "All Countries (Total)") %>%
  pivot_wider(names_from = country, values_from = n)

# Fixed row order, independent of how the joins below happen to sort things.
step_order <- c(
  "N", "Annual report disclosed", "Any ESG information disclosed",
  "(a) Standalone ESG report disclosed", "(b) ESG information disclosed within annual report", 
  "(c) Both (a) and (b) disclosed", "Annual financial report disclosed", 
  "Interim financial report disclosed"
)

tab_disclosure_coverage <- country_counts %>%
  left_join(region_counts, by = "step") %>%
  left_join(all_counts, by = "step") %>%
  select(
    step, Kazakhstan, Uzbekistan, Kyrgyzstan, `Central Asia (Total)`,
    Estonia, Latvia, Lithuania, `Baltic States (Total)`, `All Countries (Total)`
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
# chart is built for FIN, not a separate (redundant) compliance chart.

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
# The 5th category (grey) covers firms with an unresolved mandate status, so
# every bar sums to 100% instead of leaving an unexplained gap.
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
# zero mandated firms show no bar and are labeled "No mandate" rather than a
# misleading 0%.
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

# Country-level only (6 rows) - read directly from country_characteristics.csv
# rather than through smp, since only wgi_rule_of_law is an actual regression
# covariate (already merged into base_sample.parquet in prepare_data.R). The
# other 5 dimensions appear only here, to show the Region/institutional-
# quality link holds across the full WGI battery, not just the one dimension
# used in the regressions.

wgi_dim_labels <- c(
  "Control of Corruption", "Government Effectiveness", "Political Stability",
  "Regulatory Quality", "Rule of Law", "Voice and Accountability"
)

# Matched by ISO3 code, not country name - the source file spells Kyrgyzstan
# as "Kyrgyz Republic", so a name-based filter silently drops it. Mapping
# back through country_iso3_order also restores the display names used
# everywhere else in this project.
country_iso3_order <- c(
  Kazakhstan = "KAZ", Uzbekistan = "UZB", Kyrgyzstan = "KGZ",
  Estonia = "EST", Latvia = "LVA", Lithuania = "LTU"
)

# matches() rather than exact column names sidesteps a trailing-space
# mismatch in the raw header ("Political Stability ... (+2.5) " has one) -
# column order in the source file already matches wgi_dim_labels' order.
wgi_long <- read_csv(global_cfg$country_characteristics, col_types = cols()) %>%
  filter(`Country Code` %in% country_iso3_order) %>%
  mutate(country = names(country_iso3_order)[match(`Country Code`, country_iso3_order)]) %>%
  select(country, matches(paste(wgi_dim_labels, collapse = "|"))) %>%
  rename_with(~ wgi_dim_labels, -country) %>%
  pivot_longer(-country, names_to = "dimension", values_to = "score") %>%
  mutate(
    country = factor(country, levels = country_order),
    dimension = factor(dimension, levels = wgi_dim_labels)
  )

# Per-country color (grouped by region hue) and shape (circle = Central Asia,
# square = Baltics) - mapping both aesthetics to the same variable merges
# them into one legend automatically.
country_colors_wgi <- c(
  "Kazakhstan" = "#8C2D19", "Uzbekistan" = "#D4703A", "Kyrgyzstan" = "#F0B27A",
  "Estonia" = "#0B2E52", "Latvia" = "#3A6EA5", "Lithuania" = "#8FB3D9"
)
country_shapes_wgi <- c(
  "Kazakhstan" = 16, "Uzbekistan" = 16, "Kyrgyzstan" = 16,
  "Estonia" = 15, "Latvia" = 15, "Lithuania" = 15
)

# Symmetric x-axis around zero, sized to the actual data range, so visual
# distance left/right of the dashed zero line is directly comparable.
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

# --- Restrict to the common regression-ready sample -----------------------------

# Descriptive statistics, correlations, and regressions all need to describe
# the same set of firms. Complete-case filter on the union of variables used
# across the base equation and its alternate specifications.
reg_vars <- c(
  "any_esg", "mandatory_esg", "mandatory_esg_state", "mandatory_esg_exchange",
  "voluntary_esg_exchange", "foreign_ownership", "sensitive_industry",
  "state_ownership", "ln_total_assets_eur_w", "roa_w",
  "region", "wgi_rule_of_law", "ln_gdp_per_capita"
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


# --- Shared helpers ---------------------------------------------------------------

star_label <- function(p) {
  case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
}

# Central Asia minus Baltics mean difference, with significance stars from a
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


# --- Panel A: binary variables ---------------------------------------------------

binary_vars <- tibble(
  var_name = c(
    "annual_report",
    "any_esg", "ESG_info_annual_report", "ESG_separate_report",
    "annual_fin_report",
    "mandatory_esg", "mandatory_esg_state", "mandatory_esg_exchange",
    "voluntary_esg_exchange",
    "sensitive_industry", "soviet_era", "state_ownership",
    "foreign_ownership", "individual_ownership"
  ),
  label = c(
    "Annual Report",
    "ESG Disclosure", "  ESG in Annual Report", "  ESG Standalone Report",
    "Financial Disclosure",
    "Mandatory ESG", "Mandatory ESG (State)", "Mandatory ESG (Exchange)",
    "ESG Guidance (Exchange)",
    "Sensitive Industry", "Soviet-Era Firm", "State Ownership",
    "Foreign Ownership", "Individual Ownership"
  ),
  group = c(
    rep("Disclosure variables", 5),
    rep("Regulatory variables", 4),
    rep("Firm characteristics", 5)
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
    Diff = "Diff (Central Asia \u2212 Baltics)"
  ) %>%
  tab_spanner(label = "Full Sample", columns = c(N_full, Mean_full)) %>%
  tab_spanner(label = "Central Asia", columns = c(N_ca, Mean_ca)) %>%
  tab_spanner(label = "Baltics", columns = c(N_baltics, Mean_baltics)) %>%
  row_group_order(groups = group_order)


# --- Panel B: continuous variables (winsorized at 5%/95%) ------------------------

continuous_vars <- tibble(
  var_name = c("ln_total_assets_eur_w", "roa_w"),
  label = c("ln(Total Assets)", "ROA")
)

continuous_block <- function(var, label) {
  x <- smp_reg[[var]]
  r <- smp_reg$region
  bind_rows(
    group_stats(x) %>%
      mutate(row_label = paste(label, "\u2013 Full Sample"), Diff = mean_diff(x, r)),
    group_stats(x[r == "Central Asia"]) %>%
      mutate(row_label = paste(label, "\u2013 Central Asia"), Diff = ""),
    group_stats(x[r == "Baltics"]) %>%
      mutate(row_label = paste(label, "\u2013 Baltics"), Diff = "")
  )
}

tab_desc_panel_b <- map2_dfr(continuous_vars$var_name, continuous_vars$label, continuous_block) %>%
  select(row_label, N, Mean, SD, P5, Median, P95, Diff) %>%
  gt(rowname_col = "row_label") %>%
  fmt_number(columns = c(Mean, SD, P5, Median, P95), decimals = 3) %>%
  cols_label(Diff = "Diff (Central Asia \u2212 Baltics)")


# --- Correlation table -------------------------------------------------------

# Pearson correlations only. With most variables binary, Spearman and Pearson
# are mathematically identical for any binary-binary pair (ranking a 0/1
# variable doesn't change it), so a dual Pearson/Spearman matrix would be
# redundant here.

corr_vars <- tibble(
  var_name = c(
    "any_esg", "mandatory_esg", "mandatory_esg_state", "mandatory_esg_exchange",
    "voluntary_esg_exchange", "sensitive_industry", "foreign_ownership",
    "state_ownership", "ln_total_assets_eur_w", "roa_w",
    "wgi_rule_of_law", "ln_gdp_per_capita"
  ),
  label = c(
    "ESG Disclosure", "Mandatory ESG", "Mandatory ESG (State)",
    "Mandatory ESG (Exchange)", "ESG Guidance (Exchange)", "Sensitive Industry",
    "Foreign Ownership", "State Ownership", "ln(Total Assets)", "ROA",
    "WGI: Rule of Law", "ln(GDP per capita)"
  )
)

# Number labels keep the matrix compact - full names appear once as row
# labels (stub), columns are just referenced by number.
rlabels <- paste0("(", seq_len(nrow(corr_vars)), ") ", corr_vars$label)

corr_mat <- smp_reg %>% select(all_of(corr_vars$var_name))
colnames(corr_mat) <- rlabels

# Lower-triangular Pearson matrix with significance stars, computed pairwise
# via cor.test() so p-values are available for star_label(); upper triangle
# left blank since it's a mirror image of the lower one.
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


# --- Regression analysis ------------------------------------------------------

# Linear probability models (OLS) with HC1-robust standard errors. Eight 
# specifications, each testing one idea from the design discussion, all 
# estimated on the same smp_reg sample for comparability across columns.

mods <- list(
  "(1)" = lm(any_esg ~ mandatory_esg + foreign_ownership + sensitive_industry +
               ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(2)" = lm(any_esg ~ mandatory_esg_state + mandatory_esg_exchange +
               foreign_ownership + sensitive_industry + ln_total_assets_eur_w +
               roa_w, data = smp_reg),
  "(3)" = lm(any_esg ~ mandatory_esg + voluntary_esg_exchange + foreign_ownership +
               sensitive_industry + ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(4)" = lm(any_esg ~ region + foreign_ownership + sensitive_industry +
               ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(5)" = lm(any_esg ~ mandatory_esg + region + foreign_ownership +
               sensitive_industry + ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(6)" = lm(any_esg ~ mandatory_esg + wgi_rule_of_law + foreign_ownership +
               sensitive_industry + ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(7)" = lm(any_esg ~ mandatory_esg + state_ownership + foreign_ownership +
               sensitive_industry + ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(8)" = lm(any_esg ~ mandatory_esg + ln_gdp_per_capita + foreign_ownership +
               sensitive_industry + ln_total_assets_eur_w + roa_w, data = smp_reg)
)

# --- Variance Inflation Factors -------------------------------------------------
# Checked for every specification that combines Mandatory ESG with a second
# regulation- or institutional-quality-adjacent variable, since that's where
# entangled identification (rather than plain collinearity) is a real risk -
# see Column (8)'s GDP/Kazakhstan interpretation.

vif_check_models <- c("(2)", "(3)", "(5)", "(6)", "(7)", "(8)")

for (m in vif_check_models) {
  vifs <- car::vif(mods[[m]])
  log_info(
    "VIF, Model {m}: {paste(names(vifs), round(vifs, 2), sep = '=', collapse = ', ')}"
  )
}


# --- Wald test: State Ownership vs. Foreign Ownership (Column 7 only) ----------
# Reported in text/notes, not as a table row - Column (7) is the only
# specification where both coefficients coexist.
wald_7 <- car::linearHypothesis(
  mods[["(7)"]], "state_ownership = foreign_ownership",
  vcov. = sandwich::vcovHC(mods[["(7)"]], type = "HC1")
)
wald_7_p <- wald_7$`Pr(>F)`[2]
log_info("Wald test, Column (7) State = Foreign Ownership: p = {round(wald_7_p, 3)}")

var_labels <- c(
  "(Intercept)"             = "Intercept",
  "mandatory_esg"           = "Mandatory ESG",
  "mandatory_esg_state"     = "Mandatory ESG (State)",
  "mandatory_esg_exchange"  = "Mandatory ESG (Exchange)",
  "voluntary_esg_exchange"  = "ESG Guidance (Exchange)",
  "regionCentral Asia"      = "Region (Central Asia)",
  "wgi_rule_of_law"         = "WGI: Rule of Law",
  "state_ownership"         = "State Ownership",
  "ln_gdp_per_capita"       = "ln(GDP per capita)",
  "foreign_ownership"       = "Foreign Ownership",
  "sensitive_industry"      = "Sensitive Industry",
  "ln_total_assets_eur_w"   = "ln(Total Assets)",
  "roa_w"                   = "ROA"
)

tab_reg <- modelsummary(
  mods,
  vcov = "HC1",
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.10),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = var_labels,
  gof_map = list(
    list(raw = "nobs", clean = "Observations", fmt = 0),
    list(raw = "adj.r.squared", clean = "Adj. R\u00b2", fmt = function(x) sprintf("%.3f", x))
  ),
  output = "gt"
)


# --- Robustness: Firth's penalized logit (Table 4) ----------------------------

# Firth's penalized logit, not plain logit: Kyrgyzstan is a fully deterministic
# subgroup (0/10 firms disclose), which risks unstable estimates under ordinary
# maximum-likelihood logit. Firth's penalty keeps estimates finite even under
# this kind of near-complete separation. Same eight specifications and same
# smp_reg sample.

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
  "(1)" = logistf(any_esg ~ mandatory_esg + foreign_ownership + sensitive_industry +
                    ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(2)" = logistf(any_esg ~ mandatory_esg_state + mandatory_esg_exchange +
                    foreign_ownership + sensitive_industry + ln_total_assets_eur_w +
                    roa_w, data = smp_reg),
  "(3)" = logistf(any_esg ~ mandatory_esg + voluntary_esg_exchange + foreign_ownership +
                    sensitive_industry + ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(4)" = logistf(any_esg ~ region + foreign_ownership + sensitive_industry +
                    ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(5)" = logistf(any_esg ~ mandatory_esg + region + foreign_ownership +
                    sensitive_industry + ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(6)" = logistf(any_esg ~ mandatory_esg + wgi_rule_of_law + foreign_ownership +
                    sensitive_industry + ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(7)" = logistf(any_esg ~ mandatory_esg + state_ownership + foreign_ownership +
                    sensitive_industry + ln_total_assets_eur_w + roa_w, data = smp_reg),
  "(8)" = logistf(any_esg ~ mandatory_esg + ln_gdp_per_capita + foreign_ownership +
                    sensitive_industry + ln_total_assets_eur_w + roa_w, data = smp_reg)
)

# Pseudo R2 added manually via add_rows rather than gof_map

pseudo_r2_row <- tibble(
  term = "Pseudo R\u00b2",
  `(1)` = sprintf("%.3f", glance(mods_logit[["(1)"]])$pseudo_r2),
  `(2)` = sprintf("%.3f", glance(mods_logit[["(2)"]])$pseudo_r2),
  `(3)` = sprintf("%.3f", glance(mods_logit[["(3)"]])$pseudo_r2),
  `(4)` = sprintf("%.3f", glance(mods_logit[["(4)"]])$pseudo_r2),
  `(5)` = sprintf("%.3f", glance(mods_logit[["(5)"]])$pseudo_r2),
  `(6)` = sprintf("%.3f", glance(mods_logit[["(6)"]])$pseudo_r2),
  `(7)` = sprintf("%.3f", glance(mods_logit[["(7)"]])$pseudo_r2),
  `(8)` = sprintf("%.3f", glance(mods_logit[["(8)"]])$pseudo_r2)
)

tab_reg_logit <- modelsummary(
  mods_logit,
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.10),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = var_labels,
  gof_map = list(list(raw = "nobs", clean = "Observations", fmt = 0)),
  add_rows = pseudo_r2_row,
  output = "gt"
)


# --- Heterogeneity by region --------------------------------------------------

# Central Asia and Baltics run the identical lean specification in Column (1)
# vs. (2) for direct comparability; Column (3) adds Soviet-era founding as an
# extra channel, Baltics-only, since soviet_era is a structural constant in
# Central Asia (= 0 for all 103 firms). Column headers are numbered, matching
# tab_reg/tab_reg_logit; which region each column uses is reported via a
# "Region" row - the same convention Krueger et al. use for their "Sample"
# row (Table 7) to mark which subsample a column runs on.

smp_ca <- smp_reg %>% filter(region == "Central Asia")
smp_balt <- smp_reg %>% filter(region == "Baltics")

log_info(
  "Region-split samples: Central Asia N={nrow(smp_ca)} ",
  "({sum(smp_ca$any_esg)} disclosing), Baltics N={nrow(smp_balt)} ",
  "({sum(smp_balt$any_esg)} disclosing, ",
  "{sum(!is.na(smp_balt$soviet_era))} with non-missing soviet_era)."
)

mods_region <- list(
  "(1)" = lm(any_esg ~ mandatory_esg + foreign_ownership, data = smp_ca),
  "(2)" = lm(any_esg ~ mandatory_esg + foreign_ownership, data = smp_balt),
  "(3)" = lm(any_esg ~ mandatory_esg + foreign_ownership + soviet_era,
             data = smp_balt)
)

var_labels_region <- c(var_labels, "soviet_era" = "Soviet-Era Firm")

# Region row sits right after the last coefficient/SE pair, before
# Observations - 4 terms (Intercept, Mandatory ESG, Foreign Ownership,
# Soviet-Era Firm) x 2 rows each = 8, so position 9. Confirm by eye once
# rendered and adjust if off.
region_row <- tibble(
  term = "Region",
  `(1)` = "Central Asia", `(2)` = "Baltics", `(3)` = "Baltics"
)
attr(region_row, "position") <- 9

tab_reg_region <- modelsummary(
  mods_region,
  vcov = "HC1",
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.10),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map = var_labels_region,
  add_rows = region_row,
  gof_map = list(
    list(raw = "nobs", clean = "Observations", fmt = 0),
    list(raw = "adj.r.squared", clean = "Adj. R\u00b2", fmt = function(x) sprintf("%.3f", x))
  ),
  output = "gt"
)


# --- Save -------------------------------------------------------------------------

log_info("Done. Storing output in '{global_cfg$results_r}'")
save(list = c(
  "tab_sample_selection", "tab_disclosure_coverage", "fig_esg_disclosure",
  "fig_esg_compliance", "fig_fin_disclosure", "fig_wgi_dotstrip", "tab_desc_panel_a",
  "tab_desc_panel_b", "tab_corr", "tab_reg", "tab_reg_logit", "tab_reg_region"),
  file = global_cfg$results_r)