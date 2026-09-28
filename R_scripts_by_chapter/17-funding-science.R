# Funding Ecological and Environmental Science: Following the Money ----
# Source: 17-funding-science.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Background ----

## Activity ----

### Set up ----

#### code-chunk-1 ----
#| eval: false
install.packages("jsonlite", repos = "https://cloud.r-project.org")

#### code-chunk-2 ----
## load packages
library(jsonlite)
library(tidyverse)
library(googlesheets4)

### Part 1: Federal agency budgets, straight from the source ----

#### code-chunk-3 ----
## a named vector: the names are labels we choose, the values are USAspending's codes
agency_codes <- c(NSF  = "049",
                   EPA  = "068",
                   USDA = "012",
                   DOE  = "089",
                   DOI  = "014",
                   NASA = "080")

## a function that, given one toptier code, returns that agency's
## budgetary resources for every fiscal year USAspending has on file
get_budget <- function(code) {
  url <- paste0("https://api.usaspending.gov/api/v2/agency/", code,
                "/budgetary_resources/")
  result <- fromJSON(url)          # ask the API, and parse its JSON response
  result$agency_data_by_year       # this is the part we actually want: one row per fiscal year
}

#### code-chunk-4 ----
## map() applies get_budget() to each element of agency_codes;
## bind_rows(..., .id="agency") stacks the six results and labels each
budget_list <- map(agency_codes, get_budget)
budget_data <- bind_rows(budget_list, .id = "agency")

## take a look at what we got
names(budget_data)
head(budget_data)

#### fig-agency-budgets ----
#| label: fig-agency-budgets
#| fig-cap: "Total budgetary resources for six federal agencies relevant to ecological and environmental science, in nominal (not inflation-adjusted) dollars."
p_agency_budgets <- ggplot(data = budget_data,
       aes(x = fiscal_year, y = agency_budgetary_resources / 1e9,
           color = agency)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(x = "Fiscal year",
       y = "Budgetary resources (billion $, nominal)",
       color = "Agency",
       title = "Federal budgets for science-relevant agencies")
p_agency_budgets

#### code-chunk-6 ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-agency-budgets.png", plot = p_agency_budgets,
       width = 7, height = 5)

### Adjusting for inflation ----

#### code-chunk-7 ----
## Annual average CPI-U (all items, U.S. city average), 1982-84 = 100.
## Source, 2017-2025: U.S. Bureau of Labor Statistics,
## https://www.bls.gov/regions/mid-atlantic/data/consumerpriceindexannualandsemiannual_table.htm
## Note: the 2025 average is BLS's own preliminary figure--October 2025 data
## could not be collected during a lapse in appropriations that year.
## Source, 2026: BLS has not published a 2026 annual average yet--the year
## isn't over. Rather than leave 2026 blank, we use a *projected* value:
## the Federal Reserve Bank of Philadelphia's Survey of Professional
## Forecasters (SPF), Q2 2026 release, put median expected headline CPI
## inflation for 2026 at 3.5% (Q4-over-Q4 basis):
## https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/spf-q2-2026
## We applied that growth rate to the 2025 annual average (321.943 * 1.035
## = ~333.2) to get a rough 2026 estimate. This is explicitly an estimate,
## not an official BLS figure--replace it with the real annual average
## once BLS publishes it (early the following year).
## Stored in data/cpi.csv; add a row there each time you re-teach this
## chapter, replacing last year's projection with the real BLS figure and
## adding a new projected row for the new current year.
cpi <- read_csv("data/cpi.csv")

#### code-chunk-8 ----
## the CPI value for our reference year
base_cpi <- cpi$cpi_u[cpi$fiscal_year == 2025]

budget_real <- budget_data %>%
  left_join(cpi, by = "fiscal_year") %>%
  ## multiplying by (base year's CPI / that year's CPI) rescales every
  ## year's dollars into 2025 purchasing power
  mutate(budget_real_2025usd = agency_budgetary_resources * (base_cpi / cpi_u))

#### fig-agency-budgets-real ----
#| label: fig-agency-budgets-real
#| fig-cap: "The same six agency budgets as @fig-agency-budgets, expressed in constant 2025 dollars."
p_agency_budgets_real <- budget_real %>%
  filter(!is.na(budget_real_2025usd)) %>%
  ggplot(aes(x = fiscal_year, y = budget_real_2025usd / 1e9, color = agency)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(x = "Fiscal year",
       y = "Budgetary resources (billion 2025 $)",
       color = "Agency",
       title = "Federal budgets for science-relevant agencies, inflation-adjusted")
p_agency_budgets_real

#### code-chunk-10 ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-agency-budgets-real.png", plot = p_agency_budgets_real,
       width = 7, height = 5)

### How much of that is actually research? ----

#### code-chunk-11 ----
## Federal R&D OBLIGATIONS by agency--NOT each agency's entire budget, but the
## slice NCSES classifies as research and development (plus R&D facilities/
## equipment, i.e. "R&D plant"). In millions of dollars.
##
## IMPORTANT: nine of these ten rows are OBLIGATIONS, but DOE_BER is not.
## Source (NSF, EPA, USDA, NOAA, NASA, DOE, DOI, FWS, USGS, USFS): NCSES Survey
## of Federal Funds for Research and Development, Table 3, multiple survey
## years: https://ncses.nsf.gov/surveys/federal-funds-research-development.
## This is OBLIGATIONS: a legal commitment the agency made that year (a grant
## awarded, a contract signed)--the same "obligated" concept as
## agency_total_obligated back in Part 1. It is NOT the same as an OUTLAY
## (cash actually paid out, which can lag obligations by months or years),
## and it is NOT the same as an APPROPRIATION (the budget authority Congress
## granted, which an agency can carry into future years without fully
## obligating it the same year).
## Source (DOE_BER only): NCSES does not break DOE down by program office, so
## this row comes directly from DOE Office of Science's own congressional
## budget justifications for Biological and Environmental Research (BER).
## Those documents report ENACTED APPROPRIATIONS, not obligations--a related
## but different number (see above). Treat the DOE_BER row as "what Congress
## gave BER to work with," not "what BER legally committed," and keep that
## distinction in mind when comparing it to the other nine rows in
## @fig-rd-only-subagencies. See
## https://science.osti.gov/budget/Budget-by-Program/BER-Budget for the source
## documents by year.
## Stored in data/rd_only.csv. NCSES (and DOE) revise their own estimates in
## the following year's release (a "preliminary" figure becomes "final"), so a
## few of these will differ slightly from whatever is on the source website
## when you read this--that is normal for survey/budget data, not a mistake.
## Update/extend data/rd_only.csv when you re-teach the chapter.
rd_only <- read_csv("data/rd_only.csv")

#### fig-rd-only-departments ----
#| label: fig-rd-only-departments
#| fig-cap: "Research and development obligations only (not total agency budget), for the six toptier agencies, FY2019-2025."
p_rd_departments <- rd_only %>%
  filter(agency %in% c("NSF", "EPA", "USDA", "DOE", "DOI", "NASA")) %>%
  ggplot(aes(x = fiscal_year, y = rd_obligations / 1000, color = agency)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(x = "Fiscal year", y = "R&D obligations (billion $, nominal)",
       color = "Agency",
       title = "Research-only funding, toptier agencies")
p_rd_departments

#### code-chunk-13 ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-rd-only-departments.png", plot = p_rd_departments,
       width = 7, height = 5)

#### fig-rd-only-subagencies ----
#| label: fig-rd-only-subagencies
#| fig-cap: "Research and development obligations only, for NOAA, FWS, USGS, and the Forest Service (sub-agencies USAspending can't show on their own) and DOE BER (a program office isolated from DOE's much larger, mostly non-ecological R&D total), FY2019-2025."
p_rd_subagencies <- rd_only %>%
  filter(agency %in% c("NOAA", "FWS", "USGS", "USFS", "DOE_BER")) %>%
  ggplot(aes(x = fiscal_year, y = rd_obligations, color = agency)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(x = "Fiscal year", y = "R&D obligations (million $, nominal)",
       color = "Agency",
       title = "Research-only funding, sub-agencies and DOE BER")
p_rd_subagencies

#### code-chunk-15 ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-rd-only-subagencies.png", plot = p_rd_subagencies,
       width = 7, height = 5)

#### fig-total-vs-research ----
#| label: fig-total-vs-research
#| fig-cap: "Total agency budget (USAspending) versus research-only obligations (NCSES, or DOE's own budget justifications for DOE BER) for fiscal year 2024. NOAA, FWS, USGS, the Forest Service, and DOE BER have no bar for 'entire agency budget': the first four are not toptier agencies (see above), and DOE BER is a program office, not an agency--DOE's own 'entire agency budget' bar is what its BER bar should be compared against."
compare_year <- 2024

total_fy <- budget_data %>%
  filter(fiscal_year == compare_year) %>%
  transmute(agency, amount = agency_budgetary_resources / 1e6,
            category = "Entire agency budget")

research_fy <- rd_only %>%
  filter(fiscal_year == compare_year) %>%
  transmute(agency, amount = rd_obligations,
            category = "Research (R&D) obligations only")

p_total_vs_research <- bind_rows(total_fy, research_fy) %>%
  ggplot(aes(x = agency, y = amount / 1000, fill = category)) +
  geom_col(position = "dodge") +
  labs(x = "Agency", y = "Billion $",
       fill = NULL,
       title = paste("How much of each agency's FY", compare_year, "budget is research?"))
p_total_vs_research

#### code-chunk-17 ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-total-vs-research.png", plot = p_total_vs_research,
       width = 7, height = 5)

### Part 2: A different funding mechanism--state wildlife agencies ----

#### code-chunk-18 ----
#| eval: false
d_state <- read_sheet("PASTE_YOUR_CLASS_GOOGLE_SHEET_URL_HERE")

#### code-chunk-19 ----
#| echo: false
## a small stand-in data set so this document renders without a live class sheet
d_state <- tibble::tribble(
  ~state,     ~program,               ~fiscal_year, ~apportionment,
  "Ohio",     "Wildlife Restoration", 2022,          9800000,
  "Ohio",     "Wildlife Restoration", 2023,          10100000,
  "Ohio",     "Wildlife Restoration", 2024,          10400000,
  "Montana",  "Wildlife Restoration", 2022,          16200000,
  "Montana",  "Wildlife Restoration", 2023,          16500000,
  "Montana",  "Wildlife Restoration", 2024,          16900000
)

#### fig-state-funding ----
#| label: fig-state-funding
#| fig-cap: "Wildlife Restoration apportionments for a sample of states. Your class figure will include every state your classmates collected."
p_state_funding <- ggplot(data = d_state,
       aes(x = fiscal_year, y = apportionment / 1e6, color = state)) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~program) +
  labs(x = "Fiscal year", y = "Apportionment (million $)", color = "State")
p_state_funding

#### code-chunk-21 ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-state-funding.png", plot = p_state_funding,
       width = 7, height = 5)

### Part 3: Find and explain one funded research award ----

#### code-chunk-22 ----
## pick a keyword related to something you're curious about--
## try your own before settling on "biodiversity"
url <- "https://api.nsf.gov/services/v1/awards.json?keyword=biodiversity"
nsf_result <- fromJSON(url)

## the awards themselves are nested inside the response--explore its structure
## before assuming you know what's in it
str(nsf_result, max.level = 3)

#### code-chunk-23 ----
## look at the list of award titles to help you pick one that interests you
nsf_result$response$award$title

## pick one (change the row number to whichever award you chose)
my_award <- nsf_result$response$award[1, ]
my_award$title
my_award$abstractText

## the total dollar amount NSF obligated to this award
my_award$fundsObligatedAmt

## Overthinking: how much of NSF is really DEB? ----

### code-chunk-24 ----
## NSF's Division of Environmental Biology (DEB) budget--not NSF's total
## budget, and not even all of BIO's budget, but the one division that most
## directly funds population, community, and ecosystem ecology. In millions
## of dollars.
## Source: NSF's own Congressional Budget Justification, Biological Sciences
## (BIO) directorate chapter, which breaks BIO's budget down by division.
## Unlike DOE (which reports clean enacted appropriations for BER every
## year), NSF's public budget documents do not give DEB a consistent,
## fully-enacted multi-year series--figures are a mix of "Actual," "Base
## Plan," and "Request," and some years are marked "TBD" in the document
## available when that year's chapter was written. The `status` column
## below tells you which is which; do not treat all rows as equally solid.
## FY2021 Actual: https://nsf-gov-resources.nsf.gov/about/budget/fy2023/pdf/69_fy2023.pdf
## FY2023 Actual, FY2025 Request: https://nsf-gov-resources.nsf.gov/files/66_fy2025.pdf
## Stored in data/deb_budget.csv. Replace the FY2025 row with the enacted
## figure once it's final, and add newer years as NSF publishes them.
deb_budget <- read_csv("data/deb_budget.csv")

### fig-deb-budget ----
#| label: fig-deb-budget
#| fig-cap: "NSF's Division of Environmental Biology (DEB) budget--the part of NSF most directly funding population, community, and ecosystem ecology. FY2025 is a request, not yet a final enacted figure; see the `status` column."
p_deb_budget <- ggplot(deb_budget,
       aes(x = factor(fiscal_year), y = deb_budget_millions, fill = status)) +
  geom_col() +
  labs(x = "Fiscal year", y = "DEB budget (million $)", fill = "Status",
       title = "NSF's Division of Environmental Biology (DEB)")
p_deb_budget

### code-chunk-26 ----
#| eval: false
## Save a PNG file to your working directory.
ggsave("figs/fig-deb-budget.png", plot = p_deb_budget, width = 7, height = 5)

## Questions to answer ----

## Deliverables ----
