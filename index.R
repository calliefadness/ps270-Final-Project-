## ----echo=FALSE-----------------------------------------------------------------------------------------------------

library(tidyverse)
library(modelsummary)

ideology <- readr::read_csv("data/county_election.csv", show_col_types = FALSE)

wi_county_income <- read_csv("data/CAINC1_WI_1969_2023.csv", show_col_types = FALSE)

ideology_wi <- ideology |>
  filter(state_po == "WI")

ideology_wi |>
  head(10) |>
  knitr::kable(caption = "Preview of County Election Data")

wi_county_income |>
  head(10) |>
  knitr::kable(caption = "Preview of BEA County Income Data")


## ----echo=FALSE-----------------------------------------------------------------------------------------------------
wi_income_clean <- wi_county_income |>
  select(GeoName, Description, `2023`) |>
  filter(Description == "Per capita personal income (dollars) 2/") |>
  mutate(
    income = as.numeric(`2023`),
    county_name = GeoName |>
      tolower() |>
      gsub(", wi", "", x = _) |>
      gsub(" county", "", x = _)
  ) |>
  select(county_name, income)

ideology_clean <- ideology_wi |>
  filter(state_po == "WI", party == "REPUBLICAN") |>
  mutate(county_name = tolower(county_name)) |>
  group_by(county_name) |>
  summarise(
    republican_share = sum(candidatevotes) / sum(totalvotes),
    .groups = "drop"
  )

wi_income_clean |>
  head(10) |>
  knitr::kable(
    col.names = c("WI County", "Income"),
    caption = "WI County Income Data"
  )

ideology_clean |>
  head(10) |>
  knitr::kable(
    col.names = c("WI County", "Republican Vote"),
    caption = "County Election Data with Republican Vote Share"
  )



## ----echo=FALSE-----------------------------------------------------------------------------------------------------
income_visual <- ggplot(wi_income_clean, aes(x = income)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  scale_x_continuous(
    labels = scales::dollar_format(),
    limits = c(30000, 100000)
  ) +
  labs(
    title = "Distribution of Median Household Income Across Wisconsin Counties",
    x = "Median Household Income (USD)",
    y = "Number of Counties"
  )
income_visual


## ----echo=FALSE-----------------------------------------------------------------------------------------------------
ggplot(merged_data, aes(x = republican_share)) +
  geom_histogram(bins = 30, fill = "red", color = "white") +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of Republican Vote Share Across Wisconsin Counties",
    x = "WI Republican Vote Share",
    y = "WI Proportion of Counties"
  )


## ----echo=FALSE-----------------------------------------------------------------------------------------------------
merged_data <- inner_join(wi_income_clean, ideology_clean, by = "county_name")

options(scipen = 999) 

ggplot(merged_data, aes(x = income, y = republican_share)) + 
  geom_point() + geom_smooth(method = "lm") + 
  labs( title = "Income vs Republican Vote Share (WI Counties)", 
        x = "Income", 
        y = "Republican Vote Share" )


## ----echo=FALSE-----------------------------------------------------------------------------------------------------

merged_data <- merged_data |>
  mutate(income_10k = income / 10000)

model <- lm(republican_share ~ income_10k, data = merged_data)

modelsummary(model, stars = TRUE, fmt = 5)

broom::tidy(model) |>
  knitr::kable(
    digits = 3,
    caption = "Regression Results: WI Republican Vote Share on Median Household Income"
  )



## -------------------------------------------------------------------------------------------------------------------
knitr::purl("index.Rmd")

