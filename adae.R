# =============================================================================
# Program:    adae.R
# Purpose:    Create Adverse Event Analysis Dataset (ADAE)
# Input:      SDTM domain: AE, ADSL
# Output:     adae.rds
# Programmer: Jeremy Barton
# Date:       2026-02-22
# =============================================================================

library(admiral)
library(pharmaversesdtm)
library(dplyr)
library(lubridate)

# Load source data
data(ae)                              # raw adverse events from SDTM
adsl <- readRDS("data/adam/adsl.rds") # load the ADSL we already built

# see the structure and first few rows
# glimpse(ae)
# head(ae)

# Keep only safety population patients
#    !! We only want AEs for patients who actually received treatment !!
adsl_safe <- adsl |>
  filter(SAFFL == "Y") |>
  select(STUDYID, USUBJID, TRTSDT, TRTEDT, ARM, ARMCD)

# Merge AE domain with ADSL
#    This adds treatment info to each adverse event record
#    Works like a LEFT JOIN in SQL
adae <- ae |>
  derive_vars_merged(
    dataset_add = adsl_safe,
    by_vars     = exprs(STUDYID, USUBJID) 
  )

# Convert AE date from character to date
#    AESTDTC is the AE start date in raw SDTM (character string)
adae <- adae |>
  mutate(AESTDT = as.Date(AESTDTC))

# Derive TRTEMFL - treatment emergent flag
#    An AE is "treatment emergent" if it started on or after treatment start
#    This is one of the most important flags in any safety analysis
adae <- adae |>
  mutate(
    TRTEMFL = case_when(
      !is.na(AESTDT) & !is.na(TRTSDT) & AESTDT >= TRTSDT ~ "Y",
      TRUE ~ "N"
    )
  )

# Derive severity as numeric (for sorting in tables)
adae <- adae |>
  mutate(
    AESEVN = case_when(
      AESEV == "MILD"     ~ 1,
      AESEV == "MODERATE" ~ 2,
      AESEV == "SEVERE"   ~ 3,
      TRUE                ~ NA_real_
    )
  )

# Save output
saveRDS(adae, "data/adam/adae.rds")
message("ADAE created: ", nrow(adae), " adverse event records")
message("Treatment emergent AEs: ", sum(adae$TRTEMFL == "Y", na.rm = TRUE))