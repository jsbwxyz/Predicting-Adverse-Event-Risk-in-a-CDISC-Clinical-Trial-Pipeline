# =============================================================================
# Program:    adtte.R
# Purpose:    Create Time to Event Analysis Dataset (ADTTE)
# Input:      ADSL, SDTM domain: DS (disposition)
# Output:     adtte.rds
# Programmer: Jeremy Barton
# Date:       2026-02-22
# =============================================================================

library(admiral)
library(pharmaversesdtm)
library(dplyr)
library(lubridate)

# Load source data
data(ds)                               # disposition domain - contains study completion/withdrawal info
adsl <- readRDS("/home/jeremy/Documents/GitHub/clinical-adam-pipeline/data/adam/adae.rds")  # load ADSL we already built

# Exploring what disposition reasons exist in DS
#    This tells us what events can be used
ds |> count(DSDECOD) |> print()

# Identify the event - did the patient die or discontinue?
#    DSDECOD = "DEATH" means the patient died
#    Any other discontinuation is treated as the event too
ds_event <- ds |>
  filter(DSCAT == "DISPOSITION EVENT") |>  # keep only disposition events
  mutate(
    EVNTDESC = DSDECOD,                    # description of what happened
    DTHFL    = if_else(DSDECOD == "DEATH", "Y", "N"),  # death flag
    DTHDTC   = if_else(DSDECOD == "DEATH", DSSTDTC, NA_character_)  # date of death
  ) |>
  select(STUDYID, USUBJID, EVNTDESC, DTHFL, DTHDTC, DSSTDTC)

# Merge disposition events with ADSL
#    We need treatment start date (TRTSDT) to calculate time to event
adtte <- adsl |>
  filter(SAFFL == "Y") |>               # safety population only
  select(STUDYID, USUBJID, TRTSDT, TRTEDT, ARM, ARMCD) |>
  derive_vars_merged(
    dataset_add = ds_event,
    by_vars     = exprs(STUDYID, USUBJID)
  )

# adsl["SAFFL"]
# Converting event date to usable format
adtte <- adtte |>
  mutate(
    STARTDT = TRTSDT,                    # time to event starts from treatment start
    EVNTDT  = as.Date(DSSTDTC)          # event date from disposition
  )

# Derive AVAL - the actual time to event value in days
#    and CNSR - the censoring flag
#    CNSR = 0 means the event happened (we observed it)
#    CNSR = 1 means the patient was censored (still in trial, lost to follow up)
adtte <- adtte |>
  mutate(
    AVAL = as.numeric(EVNTDT - STARTDT),  # number of days from treatment start to event
    CNSR = if_else(!is.na(EVNTDT), 0, 1), # 0 = event observed, 1 = censored
    PARAM    = "Time to First Disposition Event",  # parameter description
    PARAMCD  = "TTDE"                              # parameter code
  )

# Sanity check
adtte |>
  group_by(ARM, CNSR) |>
  summarise(
    n          = n(),
    median_days = median(AVAL, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  print()

# Save output
saveRDS(adtte, "/home/jeremy/Documents/GitHub/clinical-adam-pipeline/data/adam/adtte.rds")
message("ADTTE created: ", nrow(adtte), " patients")
message("Censored: ", sum(adtte$CNSR == 1, na.rm = TRUE))