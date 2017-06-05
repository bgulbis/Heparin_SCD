library(tidyverse)
library(edwr)
library(digest)
library(aws.s3)

dir_raw <- "data/raw"
bucket <- "heparin-scd"

# prevent peer checking due to MH firewall
if (.Platform$OS.type == "windows") {
    httr::set_config(httr::config(ssl_verifypeer = 0L))
}

identifiers <- read_rds("data/tidy/identifiers.Rds") %>%
    rename(millennium_orig = millennium.id,
           millennium.id = millennium_md5)

patients <- select(identifiers, millennium.id, group, pie.id, millennium_orig)

md5_id <- function(df) {
    df %>%
        group_by_(.dots = list(~millennium.id)) %>%
        mutate_(.dots = set_names(list(~digest(millennium.id, serialize = FALSE)), "md5")) %>%
        ungroup() %>%
        select_(.dots = list(~md5, ~everything(), quote(-millennium.id))) %>%
        rename_(.dots = set_names(list(~md5), "millennium.id"))
}

# mbo data ---------------------------------------------

meds_inpt <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    md5_id()

temp <- read_data(dir_raw, "mbo_vitals", FALSE) %>%
    as.vitals() %>%
    md5_id()

ptt <- read_data(dir_raw, "mbo_labs", FALSE) %>%
    as.labs() %>%
    md5_id()

# edw data ---------------------------------------------

mpp <- read_data(dir_raw, "mpp") %>%
    as.order_by() %>%
    left_join(patients[c("millennium.id", "group", "pie.id")], by = "pie.id") %>%
    select(millennium.id, group, everything(), -pie.id)

weights_hep <- read_data(dir_raw, "weights") %>%
    as.events() %>%
    left_join(patients[c("millennium.id", "group", "pie.id")], by = "pie.id") %>%
    select(millennium.id, group, everything(), -pie.id)

# sync data --------------------------------------------

s3saveRDS(meds_inpt, "data/raw/meds_inpt.Rds", bucket)
s3saveRDS(temp, "data/raw/temp.Rds", bucket)
s3saveRDS(ptt, "data/raw/ptt.Rds", bucket)
s3saveRDS(mpp, "data/raw/mpp.Rds", bucket)
s3saveRDS(weights_hep, "data/raw/weights_hep.Rds", bucket)
