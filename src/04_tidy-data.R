library(tidyverse)
library(purrrlyr)
library(stringr)
library(lubridate)
library(edwr)
library(MESS)

dir_raw <- "data/raw"

identifiers <- read_rds("data/tidy/identifiers.Rds")

patients <- select(identifiers, millennium.id, group)

mpp <- read_data(dir_raw, "mpp") %>%
    as.order_by() %>%
    left_join(identifiers[c("millennium.id", "group", "pie.id")], by = "pie.id") %>%
    select(millennium.id, group, everything(), -pie.id)

hypothermia_start <- mpp %>%
    filter(action.type == "Order",
           str_detect(order, "Cardiac Arrest")) %>%
    arrange(millennium.id, action.datetime) %>%
    group_by(millennium.id) %>%
    distinct(.keep_all = TRUE) %>%
    select(millennium.id, hypothermia_start = action.datetime)

heparin_mpp <- mpp %>%
    filter(action.type == "Order",
           !str_detect(order, "Cardiac Arrest")) %>%
    mutate(xa = str_detect(order, regex("anti-xa", ignore_case = TRUE))) %>%
    dmap_at("order", str_replace_all, pattern = ".*Stroke.*", replacement = "Stroke") %>%
    dmap_at("order", str_replace_all, pattern = ".*Deep Vein.*", replacement = "DVT") %>%
    dmap_at("order", str_replace_all, pattern = ".*ACS.*", replacement = "ACS") %>%
    dmap_at("order", str_replace_all, pattern = ".*Acute Coronary.*", replacement = "ACS") %>%
    dmap_at("order", str_replace_all, pattern = ".*Cardio.*", replacement = "Fixed") %>%
    arrange(millennium.id, action.datetime) %>%
    group_by(millennium.id, order) %>%
    distinct(.keep_all = TRUE)

weights_hep <- read_data(dir_raw, "weights") %>%
    as.events() %>%
    left_join(identifiers[c("millennium.id", "group", "pie.id")], by = "pie.id") %>%
    select(millennium.id, group, everything(), -pie.id) %>%
    dmap_at("event.result", as.numeric) %>%
    filter(event.result >= 20)

ref <- tibble(name = "heparin", type = "med", group = "cont")

meds_inpt <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    tidy_data(ref)

hep_cont <- filter(meds_inpt, !is.na(event.tag))

hep_bolus <- filter(meds_inpt, is.na(event.tag), route != "SUB-Q")

hep_start <- hep_cont %>%
    ungroup() %>%
    group_by(millennium.id) %>%
    arrange(millennium.id, med.datetime) %>%
    distinct(.keep_all = TRUE) %>%
    select(millennium.id, heparin.start = med.datetime)

hep_protocol <- hep_start %>%
    left_join(heparin_mpp, by = "millennium.id") %>%
    select(millennium.id, order, heparin.start, action.datetime) %>%
    filter(action.datetime <= heparin.start + hours(1)) %>%
    arrange(millennium.id, desc(action.datetime)) %>%
    distinct(.keep_all = TRUE)

weights_dosing <- weights_hep %>%
    left_join(hep_start, by = "millennium.id") %>%
    filter(event.datetime <= heparin.start) %>%
    arrange(millennium.id, desc(event.datetime)) %>%
    group_by(millennium.id) %>%
    distinct(.keep_all = TRUE) %>%
    select(millennium.id, weight_heparin = event.result)

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
    left_join(hep_protocol[c("millennium.id", "order")], by = "millennium.id") %>%
    filter(duration > 0,
           start.datetime < hypothermia_start + days(2),
           stop.datetime > hypothermia_start)

hep_run <- calc_runtime(hep_cont_units_fixed) %>%
    filter(run.time > 0) %>%
    left_join(hypothermia_start[c("millennium.id", "hypothermia_start")], by = "millennium.id") %>%
    left_join(hep_protocol[c("millennium.id", "order")], by = "millennium.id") %>%
    mutate(duration_hypothermia = as.numeric(difftime(rate.start, hypothermia_start, units = "hours")))

# check if there was a bolus given near hypothermia initiation
hep_bolus_initiation <- hep_bolus %>%
    left_join(hep_start, by = "millennium.id") %>%
    left_join(hypothermia_start, by = "millennium.id") %>%
    filter(!is.na(hypothermia_start)) %>%
    mutate(time_heparin = as.numeric(difftime(med.datetime, heparin.start, units = "hours")),
           time_hypothermia = as.numeric(difftime(med.datetime, hypothermia_start, units = "hours"))) %>%
    filter(time_hypothermia >= -12, time_hypothermia <= 6)

# bolus given at start of heparin infusion
hep_bolus_initial <- hep_bolus %>%
    left_join(hep_start, by = "millennium.id") %>%
    mutate(time_heparin = as.numeric(difftime(med.datetime, heparin.start, units = "hours"))) %>%
    filter(time_heparin >= -4, time_heparin <= 1)

hep_bolus_prn <- hep_bolus %>%
    left_join(hep_start, by = "millennium.id") %>%
    mutate(time_heparin = as.numeric(difftime(med.datetime, heparin.start, units = "hours"))) %>%
    filter(time_heparin > 1)

hep_drip_sum <- hep_drip %>%
    group_by(millennium.id) %>%
    arrange(millennium.id, start.datetime) %>%
    distinct(.keep_all = TRUE)

hep_join <- select(hep_drip_sum, millennium.id, hep.time.wt.avg = time.wt.avg)

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

# group temp ranges
# <= 93.2; 98.6

temp_bin <- mutate(temp, vital = if_else(vital.result <= 93.2, "cold",
                                         if_else(vital.result <= 98.6, "mild", "normal")))

attr(temp_bin, "data") <- "mbo"
temp_bin_sum <- temp_bin %>%
    calc_runtime() %>%
    summarize_data()

temp_hr <- mutate(temp, hour = floor_date(vital.datetime, "hours"))

ptt <- read_data(dir_raw, "mbo_labs", FALSE) %>%
    as.labs() %>%
    tidy_data() %>%
    left_join(hep_start, by = "millennium.id") %>%
    left_join(hypothermia_start, by = "millennium.id") %>%
    filter(lab == "ptt",
           # lab.datetime >= hypothermia_start - hours(12),
           # lab.datetime <= hypothermia_start + days(2),
           !is.na(hypothermia_start)) %>%
    mutate(lab.result = if_else(is.na(lab.result) & censor.high, 201, lab.result)) %>%
    filter(!is.na(lab.result))

attr(ptt, "data") <- "mbo"
ptt_sum <- ptt %>%
    calc_runtime() %>%
    summarize_data()

ptt_run <- calc_runtime(ptt) %>%
    mutate(duration_heparin = as.numeric(difftime(lab.datetime, heparin.start, units = "hours")),
           duration_hypothermia = as.numeric(difftime(lab.datetime, hypothermia_start, units = "hours")))

ptt_join <- select(ptt_sum, millennium.id, ptt.time.wt.avg = time.wt.avg)

ptt_hr <- mutate(ptt, hour = floor_date(lab.datetime, "hours"))

temp_ptt <- inner_join(temp_hr, ptt_hr, by = c("millennium.id", "hour")) %>%
    left_join(hep_run, by = "millennium.id") %>%
    filter(hour >= rate.start,
           hour <= rate.stop) %>%
    select(millennium.id, hour, temp = vital.result, ptt = lab.result, hep = med.rate)

temp_ptt_full <- temp_hr %>%
    full_join(ptt_hr, by = c("millennium.id", "hour", "hypothermia_start")) %>%
    arrange(millennium.id, hour) %>%
    group_by(millennium.id) %>%
    fill(vital, vital.result, lab, lab.result) %>%
    left_join(hep_run[c("millennium.id", "med.rate", "rate.start", "rate.stop", "order")], by = "millennium.id") %>%
    filter(is.na(med.rate) | (hour >= rate.start & hour <= rate.stop))

temp_hep <- temp %>%
    left_join(hep_run, by = "millennium.id") %>%
    filter(vital.datetime >= rate.start,
           vital.datetime <= rate.stop,
           vital.result > 85)

ptt_hep <- ptt %>%
    left_join(hep_run, by = "millennium.id") %>%
    filter(lab.datetime >= rate.start,
           lab.datetime <= rate.stop,
           med.rate < 25)

data_wt_avg <- hep_join %>%
    full_join(ptt_join, by = "millennium.id") %>%
    full_join(temp_join, by = "millennium.id")

# save final data --------------------------------------
write_rds(data_wt_avg, "data/final/data_wt_avg.Rds", "gz")
write_rds(temp_hep, "data/final/temp_hep.Rds", "gz")
write_rds(ptt_hep, "data/final/ptt_hep.Rds", "gz")
write_rds(temp_ptt, "data/final/temp_ptt.Rds", "gz")
write_rds(temp_ptt_full, "data/final/temp_ptt_full.Rds", "gz")
write_rds(hep_run, "data/final/hep_run.Rds", "gz")
write_rds(ptt_run, "data/final/ptt_run.Rds", "gz")
write_rds(hep_bolus_initiation, "data/final/hep_bolus_initiation.Rds", "gz")
write_rds(hep_protocol, "data/final/hep_protocol.Rds", "gz")
write_rds(hep_drip, "data/final/hep_drip.Rds", "gz")
write_rds(hep_bolus_initial, "data/final/hep_bolus_initial.Rds", "gz")
write_rds(hep_bolus_prn, "data/final/hep_bolus_prn.Rds", "gz")
