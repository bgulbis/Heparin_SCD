library(tidyverse)
library(themebg)
library(ggvis)
library(broom)

x <- dirr::get_rds("data/final")
# data_wt_avg <- read_rds("../data/final/data_wt_avg.rds")

data_wt_avg %>%
    # filter(!is.na(temp.time.wt.avg),
           # !is.na(ptt.time.wt.avg))
    ggvis(~temp.time.wt.avg, ~ptt.time.wt.avg, fill = ~group) %>%
    layer_points() %>%
    layer_smooths()

data_wt_avg %>%
    ggvis(~ptt.time.wt.avg) %>%
    layer_histograms(width = input_slider(1, 20))
