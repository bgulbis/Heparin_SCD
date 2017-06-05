library(tidyverse)
library(purrrlyr)
library(readxl)
library(stringr)
library(MESS)
library(aws.s3)

bucket <- "heparin-scd"

# prevent peer checking due to MH firewall
if (.Platform$OS.type == "windows") {
    httr::set_config(httr::config(ssl_verifypeer = 0L))
}

# chelsea's data ---------------------------------------

df <- read_excel("data/raw/onesheet.xls") %>%
    dmap_at("Patient", as.integer) %>%
    dmap_at("Value", as.numeric) %>%
    filter(!is.na(Patient)) %>%
    dmap_at("Event", str_replace_all, pattern = "heprain", replacement = "heparin") %>%
    dmap_at("Event", str_replace_all, pattern = "heparin_subQ", replacement = "heparin_subq") %>%
    dmap_at("Event", str_replace_all, pattern = "Temperature.*", replacement = "Temperature") %>%
    mutate(Value = if_else(Event == "Temperature" & Value >= 88, (Value - 32) / 1.8, Value))

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

study_only <- ck_heparin %>%
    filter(Control == "Study",
           !is.na(hep_wt)) %>%
    summarize(auc = auc(duration, rate_wt_based),
              duration = last(duration)) %>%
    filter(!is.na(auc), duration > 0) %>%
    mutate(time_wt_avg_rate = auc / as.numeric(duration))

ck_data <- pts %>%
    filter(Event %in% c("heparin", "PTT", "Temperature")) %>%
    group_by(Patient, Control, Event, Time) %>%
    summarize_at("Value", mean) %>%
    group_by(Patient, Control, Time) %>%
    spread(Event, Value) %>%
    group_by(Patient, Control) %>%
    fill(heparin, PTT, Temperature) %>%
    filter(Temperature < 40)

s3saveRDS(ck_heparin, "data/final/ck_heparin.Rds", bucket)
s3saveRDS(ck_ptt, "data/final/ck_ptt.Rds", bucket)
s3saveRDS(ck_temp, "data/final/ck_temp.Rds", bucket)
s3saveRDS(ck_data, "data/final/ck_data.Rds", bucket)

# # <= 34; >34 <= 37; > 37
# temp_df <- df %>%
#     filter(Event == "Temperature")
#     mutate(stage = if_else(str_detect(Event, "Temp") & Value <= 34, "Cold",
#                            if_else(str_detect(Event, "Temp") & Value > 34 & Value <= 37, "Moderate", "Normal")))
