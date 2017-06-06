library(tidyverse)
library(readxl)
library(edwr)

dir_raw <- "data/raw"

# use Chelsea's list of patients
fins <- read_excel("data/raw/Linking_Log.xls",
                   col_names = c("Patient", "FIN", "Control", "X1", "X2"),
                   skip = 1) %>%
    filter(!is.na(Patient))

edw <- concat_encounters(fins$FIN)

# run EDW query
#   * Identifiers
#       - Formatted Financial Number: edw

id <- read_data(dir_raw, "identifiers") %>%
    as.id() %>%
    left_join(fins, by = c("fin" = "FIN")) %>%
    select(-X1, -X2) %>%
    rename(study_id = Patient, group = Control)

write_rds(id, "data/tidy/identifiers.Rds", "gz")

edw_pie <- concat_encounters(id$pie.id)
mbo_id <- concat_encounters(id$millennium.id)

# run MBO queries using mbo_id
#   * Clinical Events - Prompt
#       - Clinical Event: Heparin Dosing Weight (kg)
#   * Labs - Coags
#   * Medications - Inpatient - Prompt
#       - Medication (Generic): heparin
#   * Vitals - Temp

# run EDW queries, using edw_pie
#   * Clinical Events - Prompt
#       - Clinical Event: Heparin Dosing Weight (kg)
#   * Orders - Prompt
#       - Order Catalog Mnemonic: CDM Hypothermia for Cardiac Arrest-Cold Phase
