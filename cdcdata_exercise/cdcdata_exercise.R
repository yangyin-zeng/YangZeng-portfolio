# CDC Data Processing Exercise: Influenza Vaccination Coverage
# Question: How does influenza vaccination coverage vary across age groups,
# geography, and flu seasons?

library(tidyverse)
library(janitor)

# 1. Import -------------------------------------------------------------------

data_file <-
  "YangZeng-portfolio/cdcdata_exercise/data_file/Influenza_Vaccination_Coverage_for_All_Ages_(6+_Months)_20260622.csv"

flu_raw <- read_csv(
  data_file,
  na = c("", "NA"),
  show_col_types = FALSE
)

# 2. Clean and select variables -----------------------------------------------

flu_processed <- flu_raw %>%
  clean_names() %>%
  transmute(
    vaccine,
    geography_type,
    geography,
    fips,
    season = season_survey_year,
    month,
    dimension_type,
    dimension,
    # Reported estimates and confidence intervals begin with a number.
    estimate = parse_double(if_else(
      str_detect(estimate_percent, "^[0-9]"),
      estimate_percent,
      NA_character_
    )),
    ci_lower = parse_number(if_else(
      str_detect(x95_percent_ci_percent, "^[0-9]"),
      x95_percent_ci_percent,
      NA_character_
    )),
    ci_upper = parse_double(str_extract(
      x95_percent_ci_percent,
      "(?<= to )[0-9.]+"
    )),
    sample_size
  )

# 3. Validate -----------------------------------------------------------------

stopifnot(
  nrow(flu_processed) > 0,
  all(is.na(flu_processed$month) |
        between(flu_processed$month, 1, 12)),
  all(is.na(flu_processed$estimate) |
        between(flu_processed$estimate, 0, 100)),
  all(is.na(flu_processed$sample_size) |
        flu_processed$sample_size >= 0),
  all(is.na(flu_processed$ci_lower) |
        is.na(flu_processed$ci_upper) |
        flu_processed$ci_lower <= flu_processed$ci_upper)
)

missing_summary <- flu_processed %>%
  group_by(season, geography_type, dimension_type) %>%
  summarize(
    rows = n(),
    missing_estimates = sum(is.na(estimate)),
    missing_percent = missing_estimates / rows,
    .groups = "drop"
  )

print(missing_summary)

# 4. Create comparable analysis subsets --------------------------------------

age_order <- c(
  "6 Months - 17 Years",
  "18-49 Years",
  "50-64 Years",
  ">=65 Years",
  ">=6 Months"
)

# Month 5 is the end-of-season observation. Restricting the vaccine,
# geography, month, and dimension prevents incompatible rows from being
# averaged together.
national_age <- flu_processed %>%
  filter(
    vaccine == "Seasonal Influenza",
    geography_type == "HHS Regions/National",
    geography == "United States",
    dimension_type == "Age",
    month == 5,
    dimension %in% age_order
  ) %>%
  mutate(dimension = factor(dimension, levels = age_order)) %>%
  arrange(season, dimension)

# Two-character FIPS codes identify states and the District of Columbia,
# excluding local-area records from the state distribution.
state_overall <- flu_processed %>%
  filter(
    vaccine == "Seasonal Influenza",
    geography_type == "States/Local Areas",
    str_length(fips) == 2,
    dimension_type == "Age",
    dimension == ">=6 Months",
    month == 5,
    !is.na(estimate)
  )

stopifnot(
  !anyDuplicated(national_age[c("season", "dimension")]),
  !anyDuplicated(state_overall[c("season", "fips")])
)

# 5. Summaries ----------------------------------------------------------------

national_age_summary <- national_age %>%
  select(season, dimension, estimate, ci_lower, ci_upper, sample_size)

state_season_summary <- state_overall %>%
  group_by(season) %>%
  summarize(
    states_reported = n(),
    median_estimate = median(estimate),
    q1 = quantile(estimate, 0.25),
    q3 = quantile(estimate, 0.75),
    .groups = "drop"
  )

print(national_age_summary)
print(state_season_summary)

# 6. Visualizations ------------------------------------------------------------

national_age_plot <- ggplot(
  national_age,
  aes(x = season, y = estimate, group = 1)
) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_line(color = "#2C7FB8", linewidth = 0.7) +
  geom_point(color = "#2C7FB8", size = 1.8) +
  facet_wrap(vars(dimension), ncol = 1) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = scales::label_percent(scale = 1)
  ) +
  labs(
    title = "US Influenza Vaccination Coverage by Age Group",
    subtitle = "End-of-season estimates with 95% confidence intervals",
    x = "Flu season",
    y = "Vaccination coverage",
    caption = "Source: CDC influenza vaccination coverage data"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

state_distribution_plot <- ggplot(
  state_overall,
  aes(x = season, y = estimate)
) +
  geom_boxplot(fill = "#A6CEE3", outlier.alpha = 0.5) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = scales::label_percent(scale = 1)
  ) +
  labs(
    title = "Distribution of State Influenza Vaccination Coverage",
    subtitle = "End-of-season estimates for people age 6 months and older",
    x = "Flu season",
    y = "Vaccination coverage",
    caption = "Source: CDC influenza vaccination coverage data"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(national_age_plot)
print(state_distribution_plot)
