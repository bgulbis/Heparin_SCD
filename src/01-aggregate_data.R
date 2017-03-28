library(tidyverse)
library(readxl)
library(stringr)
library(MESS)
library(themebg)

df <- read_excel("data/raw/onesheet.xls") %>%
    dmap_at("Patient", as.integer) %>%
    dmap_at("Value", as.numeric) %>%
    filter(!is.na(Patient))

pts <- read_excel("data/raw/Linking_Log.xls", col_names = c("Patient", "FIN", "Control", "X1", "X2"), skip = 1) %>%
    filter(!is.na(Patient)) %>%
    select(Patient, Control) %>%
    left_join(df, by = "Patient")

hep_wt <- pts %>%
    filter(Event == "Heparin Dosing Weight (kg)") %>%
    arrange(Patient, Time) %>%
    distinct(Patient, .keep_all = TRUE) %>%
    select(Patient, hep_wt = Value)

heparin <- pts %>%
    filter(Event == "heparin") %>%
    arrange(Patient, Time) %>%
    group_by(Patient, Control) %>%
    left_join(hep_wt, by = "Patient") %>%
    mutate(duration = difftime(Time, first(Time), units = "hours"),
           rate_wt_based = Value / hep_wt)

study_only <- heparin %>%
    filter(Control == "Study",
           !is.na(hep_wt)) %>%
    summarize(auc = auc(duration, rate_wt_based),
              duration = last(duration)) %>%
    filter(!is.na(auc), duration > 0) %>%
    mutate(time_wt_avg_rate = auc / as.numeric(duration))

ggplot(study_only, aes(x = time_wt_avg_rate)) +
    geom_histogram(binwidth = 1) +
    theme_bg()

# <= 34; >34 <= 37; > 37
temp <- pts %>%
    # filter(str_detect(Event, "Temp")) %>%
    mutate(stage = if_else(str_detect(Event, "Temp") & Value <= 34, "Cold",
                           if_else(str_detect(Event, "Temp") & Value > 34 & Value <= 37, "Moderate", "Normal")))
