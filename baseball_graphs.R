################################################################################
## Richmond Dukes Baseball Statistics
################################################################################

####################################
# Libraries
####################################

library("googlesheets4")
library("janitor")
library("tidyverse")
library("jpeg")
library("patchwork")

####################################
# Baseball acronyms
####################################

# AB: at bats
# R: runs
# H: hits
# RBI: runs batted in
# 2B: doubles (?)
# 3B: triples (?)
# HR: home run
# BB: base on balls ("walks")
# SB: stolen bases
# CS: caught stealing
# SO:
# AVG: average (?)
# OBP: on base percentage
# OPS: on base percentage plus slugging percentage
# PO: put outs
# A: assists
# E: errors
# HBP: hit by pitch
# IP: innings pitched
# H: hits
# R: runs
# ER: earned runs
# BB: walks
# SO: 
# ERA: earned runs average
# WHIP: walks plus hits divided by innings pitched

####################################
# Load data from Google sheet
####################################

dukes_sheet_url <- "https://docs.google.com/spreadsheets/d/1_SuKJbQN2LrYlc2Xv_bNt3RuZ_SGArKZ1EVhF1S9GR8/edit#gid=891823880"

hitters_2022 <- read_sheet(ss = dukes_sheet_url,
                   sheet = "2022 Season",
                   range = "2022 Season!C9:Z34") %>%
  janitor::clean_names()

pitchers_2022 <- read_sheet(ss = dukes_sheet_url,
                           sheet = "2022 Season",
                           range = "2022 Season!C37:M45") %>%
  janitor::clean_names() %>%
  dplyr::rename(player = pitchers)

results_2022 <- hitters_2022 %>%
  left_join(pitchers_2022, by = "player") %>%
  mutate(season = "2022")

hitters_2021 <- read_sheet(ss = dukes_sheet_url,
                           sheet = "2021 Season",
                           range = "2021 Season!C9:W31") %>%
  janitor::clean_names()

pitchers_2021 <- read_sheet(ss = dukes_sheet_url,
                            sheet = "2021 Season",
                            range = "2021 Season!C34:K41") %>%
  janitor::clean_names() %>%
  dplyr::rename(player = pitchers)

results_2021 <- hitters_2021 %>%
  left_join(pitchers_2021, by = "player") %>%
  mutate(season = "2021")

dukes_results <- results_2022 %>%
  # Remove columns not in 2021 results
  select(-c(x1b, slg, percent, k_9, k_bb)) %>%
  rbind(results_2021)

####################################
# Dukes Logo
####################################

logo <- jpeg::readJPEG("/Users/Joe/Pictures/richmond_dukes_logo.jpg")

####################################
# Plot graphs
####################################

# RBI
dukes_results  %>%
  filter(season == "2022") %>%
  ggplot(aes(x = reorder(player, desc(rbi)), y = rbi)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Runs batted in", 
       title = "RBI for 2022 season")

# OBP
dukes_results  %>%
  filter(season == "2022") %>%
  ggplot(aes(x = reorder(player, desc(obp)), y = obp)) +
  geom_point() +
  ylim(0, 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "On base percentage", 
       title = "What can he do? He gets on base")

# Walks
walks_plot <- dukes_results  %>%
  filter(season == "2022") %>%
  ggplot(aes(x = reorder(player, desc(bb.x)), y = bb.x)) +
  geom_col(fill = "#FF0000", colour = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Walks in 2022 season",
       title = "No-one walks like Gaston!")

# Walk percentage
dukes_results  %>%
  mutate(bb_percent = bb.x / ab) %>%
  filter(season == "2022") %>%
  ggplot(aes(x = reorder(player, desc(bb_percent)), y = bb_percent)) +
  geom_col(fill = "#FF0000", colour = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Walks",
       title = "No-one walks like Gaston!")

# Add logos to plots - not working yet
plot_logo <- walks_plot +
  patchwork::inset_element(p = logo,
                           left = 0.05,
                           bottom = 0.65,
                           right = 0.5,
                           top = 0.95)

####################################