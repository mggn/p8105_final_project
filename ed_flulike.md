ED Flu Visits
================
Adam Whalen
11/20/2020

## Data Description

This dataset contains information about emergency department visits and
admissions for influenza-like illness and/or pneumonia in New York City.
It was collected by the NYC Department of Health and Mental Hygiene and
accessed via API from the NYC OpenData web platform. It contains
syndromic data, meaning that it is not characterized by diagnostic
testing and is inherently non-specific, but are more useful for tracking
trends over time rather than exact measures of morbidity. Each
extract\_date repeats previously recoreded entries; we should simply
filter on the last extract date to get the most up to date data, and
discard all others to avoid repeated entries.

There are 4.2 million rows in the full dataset; we don’t need that many.
Let’s just start with 5000 rows to get a better idea of what the dataset
contains.

``` r
ed_flu = 
  GET("https://data.cityofnewyork.us/resource/2nwg-uqyg.csv",
      query = list("$limit" = 10000)) %>% 
  content("parsed") %>% 
  mutate(
    extract_date = as.Date(extract_date, format = "%m/%d/%Y"),
    date = as.Date(date, format = "%m/%d/%Y"),
    mod_zcta = as.factor(mod_zcta), 
    pct_visits = ili_pne_visits / total_ed_visits,
    pct_adm = ili_pne_admissions / ili_pne_visits
  ) %>% 
  filter(extract_date == "2020-11-19")
```

    ## Parsed with column specification:
    ## cols(
    ##   extract_date = col_datetime(format = ""),
    ##   date = col_datetime(format = ""),
    ##   mod_zcta = col_double(),
    ##   total_ed_visits = col_double(),
    ##   ili_pne_visits = col_double(),
    ##   ili_pne_admissions = col_double()
    ## )
