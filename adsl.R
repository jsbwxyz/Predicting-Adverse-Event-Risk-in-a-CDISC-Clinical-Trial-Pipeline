# =============================================================================
# Program:    adsl.R
# Purpose:    Create Subject Level Analysis Dataset (ADSL)
# Input:      SDTM domains: DM, EX, DS, MH
# Output:     adsl.rds
# Programmer: Jeremy Barton
# Date:       2026-02-21
# =============================================================================

library(admiral)
library(pharmaversesdtm)
library(dplyr)
library(lubridate)

# DATA LOADING AND PACKAGE IMPORTING

# Load SDTM source data
data(dm)   # demographics
data(ex)   # exposure (to get treatment start/end)
data(ds)   # disposition (completion/withdrawal info)
data(mh)   # medical history (for baseline conditions)


# Pull in demographics information from pharmaversedtm package
adsl <- dm |>
  filter(ARMCD != "SCRNFAIL") |>   # exclude screen failures
  select(STUDYID, USUBJID, SUBJID, SITEID, AGE, AGEU, SEX, RACE, ETHNIC,
         ARM, ARMCD, ACTARM, ACTARMCD, COUNTRY, DMDTC, RFSTDTC, RFENDTC)

# Merging treatment dates from EX into ADSL.
# derive_vars_merged() is an admiral package function
# takes in EX domain and pulls 1 value per patient into ADSL

# names(ex)
adsl <- adsl |>
  derive_vars_merged(
    dataset_add = ex,
    by_vars     = exprs(STUDYID, USUBJID), # join key
    new_vars    = exprs(TRTSDTM = EXSTDTC, TRTEDTM = EXENDTC),
    order       = exprs(EXSTDTC),
    mode        = "first"   # first exposure = treatment start
  )

# 3. Deriving TRTSDT / TRTEDT (date only versions)
adsl <- adsl |>
  mutate(
    TRTSDT = as.Date(TRTSDTM),
    TRTEDT = as.Date(TRTEDTM)
  )

# 4. Categorize age groups into three sections. Under 18, 18 to 64 and 65+
adsl <- adsl |>
  mutate(
    AGEGR1 = case_when(
      AGE < 18              ~ "<18",
      AGE >= 18 & AGE <= 64 ~ "18-64",
      AGE >= 65             ~ ">=65"
    ),
    AGEGR1N = case_when(
      AGEGR1 == "<18"   ~ 1,
      AGEGR1 == "18-64" ~ 2,
      AGEGR1 == ">=65"  ~ 3
    )
  )

# Derive SAFFL (safety population flag)
#    Definition: randomized AND received at least one dose

# Checking SCRNFAIL, tells if the patient has a treatment start date.
# Filter on patients that actually received the treatment, even if it 
# was a placebo
adsl <- adsl |>
  mutate(SAFFL = if_else(!is.na(TRTSDT) & ARMCD != "SCRNFAIL", "Y", "N"))

# Save output for next step
saveRDS(adsl, "data/adam/adsl.rds")
message("ADSL created: ", nrow(adsl), " subjects")