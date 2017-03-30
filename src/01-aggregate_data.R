library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(edwr)

dir_raw <- "data/raw"

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

patients <- select(id, millennium.id, pie.id, study_id, group)

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

hypothermia_start <- read_data(dir_raw, "mpp") %>%
    as.order_by() %>%
    filter(action.type == "Order") %>%
    arrange(pie.id, action.datetime) %>%
    group_by(pie.id) %>%
    distinct(.keep_all = TRUE) %>%
    select(pie.id, hypothermia_start = action.datetime) %>%
    left_join(patients, by = "pie.id")

weights_hep <- read_data(dir_raw, "weights") %>%
    as.events() %>%
    dmap_at("event.result", as.numeric) %>%
    filter(event.result >= 20) %>%
    left_join(patients, by = "pie.id")

ref <- tibble(name = "heparin", type = "med", group = "cont")

meds_inpt <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    tidy_data(ref)

hep_cont <- filter(meds_inpt, !is.na(event.tag))

hep_start <- hep_cont %>%
    group_by(millennium.id) %>%
    arrange(millennium.id, med.datetime) %>%
    distinct(.keep_all = TRUE) %>%
    select(millennium.id, heparin.start = med.datetime)

weights_dosing <- weights_hep %>%
    left_join(hep_start, by = "millennium.id") %>%
    filter(event.datetime <= heparin.start) %>%
    arrange(millennium.id, desc(event.datetime)) %>%
    group_by(millennium.id) %>%
    distinct(.keep_all = TRUE) %>%
    select(millennium.id, weight_heparin = event.result)

# meds_sched <- read_data(dir_raw, "meds-sched") %>%
#     as.meds_sched()
#
# ref <- tibble(name = "heparin", type = "med", group = "cont")
#
# meds_cont <- read_data(dir_raw, "meds-cont") %>%
#     as.meds_cont() %>%
#     tidy_data(ref, meds_sched)

hep_cont_units_fixed <- hep_cont %>%
    left_join(weights_dosing, by = "millennium.id") %>%
    mutate(orig_rate = med.rate,
           med.rate = if_else(med.rate.units == "unit/hr" & med.rate >= 100,
                              orig_rate / weight_heparin,
                              orig_rate)) %>%
    dmap_at("med.rate", ~ coalesce(.x, 0))

attr(hep_cont_units_fixed, "data") <- "mbo"
hep_drip <- calc_runtime(hep_cont_units_fixed) %>%
    summarize_data() %>%
    left_join(hypothermia_start, by = "millennium.id") %>%
    filter(duration > 0,
           start.datetime < hypothermia_start + days(2),
           stop.datetime > hypothermia_start)

hep_drip_sum <- hep_drip %>%
    group_by(millennium.id) %>%
    arrange(millennium.id, start.datetime) %>%
    distinct(.keep_all = TRUE)

hep_join <- select(hep_drip_sum, millennium.id, hep.time.wt.avg = time.wt.avg)

# temp <- read_data(dir_raw, "^vitals") %>%
temp <- read_data(dir_raw, "mbo_vitals", FALSE) %>%
    as.vitals() %>%
    left_join(hypothermia_start, by = "millennium.id") %>%
    filter((vital.result > 80 & vital.result.units == "DegF"),
           vital.datetime >= hypothermia_start - hours(12),
           vital.datetime <= hypothermia_start + days(2)) %>%
    mutate(vital.type = vital) %>%
    dmap_at("vital", ~ "temperature") %>%
    arrange(millennium.id, vital.datetime)

attr(temp, "data") <- "mbo"
temp_sum <- temp %>%
    calc_runtime() %>%
    summarize_data()

temp_join <- select(temp_sum, millennium.id, temp.time.wt.avg = time.wt.avg)

# ptt <- read_data(dir_raw, "^labs") %>%
ptt <- read_data(dir_raw, "mbo_labs", FALSE) %>%
    as.labs() %>%
    tidy_data() %>%
    left_join(hypothermia_start, by = "millennium.id") %>%
    filter(lab == "ptt",
           lab.datetime >= hypothermia_start - hours(12),
           lab.datetime <= hypothermia_start + days(2)) %>%
    mutate(lab.result = if_else(is.na(lab.result) & censor.high, 201, lab.result)) %>%
    filter(!is.na(lab.result))

attr(ptt, "data") <- "mbo"
ptt_sum <- ptt %>%
    calc_runtime() %>%
    summarize_data()

ptt_join <- select(ptt_sum, millennium.id, ptt.time.wt.avg = time.wt.avg)

data_mbo <- patients %>%
    left_join(hep_join, by = "millennium.id") %>%
    left_join(ptt_join, by = "millennium.id") %>%
    left_join(temp_join, by = "millennium.id")

write_rds(data_mbo, "data/final/data_wt_avg.rds", "gz")

# chelsea's data ---------------------------------------

# df <- read_excel("data/raw/onesheet.xls") %>%
#     dmap_at("Patient", as.integer) %>%
#     dmap_at("Value", as.numeric) %>%
#     filter(!is.na(Patient))
#
# pts <- read_excel("data/raw/Linking_Log.xls",
#                   col_names = c("Patient", "FIN", "Control", "X1", "X2"),
#                   skip = 1) %>%
#     filter(!is.na(Patient)) %>%
#     select(Patient, Control) %>%
#     left_join(df, by = "Patient")
#
# hep_wt <- pts %>%
#     filter(Event == "Heparin Dosing Weight (kg)") %>%
#     arrange(Patient, Time) %>%
#     distinct(Patient, .keep_all = TRUE) %>%
#     select(Patient, hep_wt = Value)
#
# heparin <- pts %>%
#     filter(Event == "heparin") %>%
#     arrange(Patient, Time) %>%
#     group_by(Patient, Control) %>%
#     left_join(hep_wt, by = "Patient") %>%
#     mutate(duration = difftime(Time, first(Time), units = "hours"),
#            rate_wt_based = Value / hep_wt)
#
# study_only <- heparin %>%
#     filter(Control == "Study",
#            !is.na(hep_wt)) %>%
#     summarize(auc = auc(duration, rate_wt_based),
#               duration = last(duration)) %>%
#     filter(!is.na(auc), duration > 0) %>%
#     mutate(time_wt_avg_rate = auc / as.numeric(duration))
#
# ggplot(study_only, aes(x = time_wt_avg_rate)) +
#     geom_histogram(binwidth = 1) +
#     theme_bg()
#
# # <= 34; >34 <= 37; > 37
# temp_df <- df %>%
#     filter(str_detect(Event, "Temp"))
#     mutate(stage = if_else(str_detect(Event, "Temp") & Value <= 34, "Cold",
#                            if_else(str_detect(Event, "Temp") & Value > 34 & Value <= 37, "Moderate", "Normal")))
