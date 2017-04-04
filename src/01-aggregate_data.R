library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(edwr)
library(MESS)

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
    filter(action.type == "Order",
           str_detect(order, "Cardiac Arrest")) %>%
    arrange(pie.id, action.datetime) %>%
    group_by(pie.id) %>%
    distinct(.keep_all = TRUE) %>%
    select(pie.id, hypothermia_start = action.datetime) %>%
    left_join(patients, by = "pie.id")

heparin_mpp <- read_data(dir_raw, "mpp") %>%
    as.order_by() %>%
    filter(action.type == "Order",
           !str_detect(order, "Cardiac Arrest")) %>%
    mutate(xa = str_detect(order, regex("anti-xa", ignore_case = TRUE))) %>%
    dmap_at("order", str_replace_all, pattern = ".*Stroke.*", replacement = "Stroke") %>%
    dmap_at("order", str_replace_all, pattern = ".*Deep Vein.*", replacement = "DVT") %>%
    dmap_at("order", str_replace_all, pattern = ".*ACS.*", replacement = "ACS") %>%
    dmap_at("order", str_replace_all, pattern = ".*Acute Coronary.*", replacement = "ACS") %>%
    dmap_at("order", str_replace_all, pattern = ".*Cardio.*", replacement = "Fixed") %>%
    arrange(pie.id, action.datetime) %>%
    group_by(pie.id, order) %>%
    distinct(.keep_all = TRUE) %>%
    # select(pie.id, hypothermia_start = action.datetime) %>%
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

hep_bolus <- filter(meds_inpt, is.na(event.tag), route != "SUB-Q")

hep_start <- hep_cont %>%
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
    left_join(hep_protocol[c("millennium.id", "order")], by = "millennium.id") %>%
    filter(duration > 0,
           start.datetime < hypothermia_start + days(2),
           stop.datetime > hypothermia_start)

# x <- hep_drip %>%
#     filter(is.na(order)) %>%
#     select(-order) %>%
#     left_join(heparin_mpp[c("millennium.id", "order", "xa", "action.datetime")], by = "millennium.id") %>%
#     select(millennium.id, order, start.datetime, action.datetime)

hep_run <- calc_runtime(hep_cont_units_fixed) %>%
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

# group temp ranges
# <= 93.2; 98.6

temp_bin <- mutate(temp, vital = if_else(vital.result <= 93.2, "cold",
                                         if_else(vital.result <= 98.6, "mild", "normal")))

attr(temp_bin, "data") <- "mbo"
temp_bin_sum <- temp_bin %>%
    calc_runtime() %>%
    summarize_data()

temp_hr <- mutate(temp, hour = floor_date(vital.datetime, "hours"))

# ptt <- read_data(dir_raw, "^labs") %>%
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

temp_ptt <- inner_join(temp_hr, ptt_hr, by = c("millennium.id", "hour", "group")) %>%
    left_join(hep_run, by = "millennium.id") %>%
    filter(hour >= rate.start,
           hour <= rate.stop) %>%
    select(millennium.id, hour, group, temp = vital.result, ptt = lab.result, hep = med.rate)

# temp_ptt %>%
#     filter(temp > 85) %>%
#     ggplot(aes(x = temp, y = ptt, color = group)) +
#     geom_point(shape = 1) +
#     geom_smooth()

temp_hep <- temp %>%
    left_join(hep_run, by = "millennium.id") %>%
    filter(vital.datetime >= rate.start,
           vital.datetime <= rate.stop,
           vital.result > 85)

# ggplot(temp_hep, aes(x = vital.result, y = med.rate)) +
#     geom_point(shape = 1) +
#     geom_smooth()
#
ptt_hep <- ptt %>%
    left_join(hep_run, by = "millennium.id") %>%
    filter(lab.datetime >= rate.start,
           lab.datetime <= rate.stop,
           med.rate < 25)

# ggplot(ptt_hep, aes(x = med.rate, y = lab.result)) +
#     geom_point(shape = 1) +
#     geom_smooth()
#
# mod <- lm(med.rate ~ vital.result, temp_hep)
# summary(mod)
#
# mod_ptt <- lm(ptt ~ temp, temp_ptt)
# summary(mod_ptt)
# library(broom)
#
# mod_ptt %>%
#     augment() %>%
#     ggplot(aes(x = .fitted, y = .resid)) +
#     geom_point(shape = 1) +
#     geom_smooth(se = FALSE)
#
# par(mfrow = c(2,2))
# plot(mod_ptt)

data_wt_avg <- patients %>%
    left_join(hep_join, by = "millennium.id") %>%
    left_join(ptt_join, by = "millennium.id") %>%
    left_join(temp_join, by = "millennium.id")

write_rds(data_wt_avg, "data/final/data_wt_avg.Rds", "gz")
write_rds(temp_hep, "data/final/temp_hep.Rds", "gz")
write_rds(ptt_hep, "data/final/ptt_hep.Rds", "gz")
write_rds(temp_ptt, "data/final/temp_ptt.Rds", "gz")

temp_ptt %>%
    select_if(is.numeric) %>%
    cor()

# chelsea's data ---------------------------------------

df <- read_excel("data/raw/onesheet.xls") %>%
    dmap_at("Patient", as.integer) %>%
    dmap_at("Value", as.numeric) %>%
    filter(!is.na(Patient)) %>%
    dmap_at("Event", str_replace_all, pattern = "heprain", replacement = "heparin") %>%
    dmap_at("Event", str_replace_all, pattern = "heparin_subQ", replacement = "heparin_subq") %>%
    dmap_at("Event", str_replace_all, pattern = "Temperature.*", replacement = "Temperature")

pts <- read_excel("data/raw/Linking_Log.xls",
                  col_names = c("Patient", "FIN", "Control", "X1", "X2"),
                  skip = 1) %>%
    filter(!is.na(Patient)) %>%
    select(Patient, Control) %>%
    left_join(df, by = "Patient")

hep_wt <- pts %>%
    filter(Event == "Heparin Dosing Weight (kg)") %>%
    arrange(Patient, Time) %>%
    distinct(Patient, .keep_all = TRUE) %>%
    select(Patient, hep_wt = Value)

ck_heparin <- pts %>%
    filter(Event == "heparin") %>%
    arrange(Patient, Time) %>%
    group_by(Patient, Control) %>%
    left_join(hep_wt, by = "Patient") %>%
    mutate(duration = difftime(Time, first(Time), units = "hours"),
           rate_wt_based = Value / hep_wt)

ck_ptt <- pts %>%
    filter(Event == "PTT") %>%
    group_by(Patient) %>%
    mutate(duration_ptt = difftime(Time, first(Time), units = "hours"))

ck_temp <- pts %>%
    filter(Event == "Temperature") %>%
    group_by(Patient) %>%
    mutate(duration_temp = difftime(Time, first(Time), units = "hours"))

write_rds(ck_heparin, "data/final/ck_heparin.Rds")
write_rds(ck_ptt, "data/final/ck_ptt.Rds")
write_rds(ck_temp, "data/final/ck_temp.Rds")


study_only <- ck_heparin %>%
    filter(Control == "Study",
           !is.na(hep_wt)) %>%
    summarize(auc = auc(duration, rate_wt_based),
              duration = last(duration)) %>%
    filter(!is.na(auc), duration > 0) %>%
    mutate(time_wt_avg_rate = auc / as.numeric(duration))

df2 <- df %>%
    filter(Event %in% c("heparin", "PTT", "Temperature")) %>%
    group_by(Patient, Event, Time) %>%
    summarize_at("Value", sum) %>%
    group_by(Patient, Time) %>%
    spread(Event, Value)

df %>%
    filter(Event == "PTT") %>%
    count(Patient) %>%
    summary()

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
