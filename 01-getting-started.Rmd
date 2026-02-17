# Getting Ready

This short setup chapter ensures everyone can run the examples on their own laptop before class.

## Install R and RStudio

- Download R from <https://cran.r-project.org/> (any recent 4.x build).
- Download RStudio Desktop from <https://posit.co/download/rstudio-desktop/> (free version).

## Folder structure

Place the course folder anywhere convenient. The book assumes the working directory is the course root (where `ess.csv` lives). To set it inside RStudio: *Session* \> *Set Working Directory* \> *To Source File Location*.

````r
# check current working directory
getwd()
# list course files
list.files()
````

## Load the ESS data once

We use a pre-cleaned CSV with 80k+ respondents from GB, DE, and FR. The code below keeps only the variables used in the book and handles common missing codes ("", 7x, 8x, 9x often mean non-response in ESS).

````r
library(dplyr)
library(readr)

ess_raw <- read_csv("ess.csv", show_col_types = FALSE)

ess <- ess_raw |>
  filter(cntry %in% c("GB", "DE", "FR")) |>
  mutate(across(everything(), ~ na_if(.x, ""))) |>
  mutate(across(where(is.character), readr::parse_number, na = ""))

# quick glimpse
ess |> select(cntry, agea, gndr, ppltrst, netustm, nwsptot) |> slice_head(n = 5)
````

> Tip: keep `ess` in your environment while you work across chapters to avoid reloading.
