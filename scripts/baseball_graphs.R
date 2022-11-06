################################################################################
## Richmond Dukes Baseball Statistics
################################################################################

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
# SO: strike outs
# AVG: batting average
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
# ERA: earned runs average
# WHIP: walks plus hits divided by innings pitched

####################################
# Libraries and resources
####################################

library("googlesheets4")
library("janitor")
library("tidyverse")
library("patchwork")
library("readxl")
library("png") 
library("ggpubr")

setwd( "C:/Users/Joe/Documents/richmond_dukes_baseball/")

####################################
# Plot resources
####################################

dukes_red <- "#CC0000"

opponent_grey <- "#CCCCCC"

# Consistent theme based on Alvin's comments
alvin_plot_theme <- theme(panel.grid = element_blank(),
                          axis.title = element_text(size = 12, face="bold"),
                          plot.title = element_text(size = 14, face="bold"),
                          axis.text = element_text(face="bold", size = 11,
                                                   colour = "black"),
                          strip.text = element_text(face = "bold",
                                                    size = 12),
                          # Margin to allow Alvin to crop if needed
                          plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Use consistent default coordinates for logo position
add_dukes_logo <- function(input_plot, l = 0.8, r = 0.95,
                           b = 0.05, t = 0.25) {
  
  dukes_logo <- readPNG("resources/dukes_logo.png", native = TRUE)
  
  output_plot <- input_plot +
    patchwork::inset_element(p = dukes_logo,
                             left = l,
                             right = r,
                             bottom = b,
                             top = t,
                             # Make sure logo isn't clipped
                             clip = FALSE)
  
  return(output_plot)
  
}

save_dukes_plot <- function(input_plot, filename) {
  
  # Save each plot with consistent formatting.
  
  plot_filename <- as.character
  
  ggsave(plot = input_plot, 
         filename = paste0(format(Sys.time(), "%Y%m%d_%H%M%S"),
                           "_",
                           filename,
                           ".jpeg"),
         path = "plots/", 
         device='jpeg', 
         dpi=600,
         units = "cm",
         width = 15,
         height = 15)
  
}

# Threshold for inclusion in hitting statistics (PA: plate appearances)
hitting_pa_threshold <- 20

# Threshold for inlcusion in pitching statistics (IP: innings pitched)
pitching_ip_threshold <- 13

# Subtitles for plots

hitting_subtitle <- paste0("For players with a minimum of ", hitting_pa_threshold,
                             " plate appearances")

pitching_subtitle <- paste0("For players with a minimum of ", pitching_ip_threshold,
                            " innings pitched")

#############################################################
# Load baseball data
#############################################################
# Load Dukes data from Google sheet
####################################

# Note: access to this Googlesheet is not public
dukes_sheet_url <- "https://docs.google.com/spreadsheets/d/1_SuKJbQN2LrYlc2Xv_bNt3RuZ_SGArKZ1EVhF1S9GR8/edit#gid=891823880"

hitters_2022 <- read_sheet(ss = dukes_sheet_url,
                   sheet = "2022 Season",
                   range = "2022 Season!C9:Z34") %>%
  janitor::clean_names() %>%
  mutate(xbh = x2b + x3b + hr) 

pitchers_2022 <- read_sheet(ss = dukes_sheet_url,
                           sheet = "2022 Season",
                           range = "2022 Season!C37:M45") %>%
  janitor::clean_names() %>%
  dplyr::rename(player = pitchers)

hitters_2021 <- read_sheet(ss = dukes_sheet_url,
                           sheet = "2021 Season",
                           range = "2021 Season!C9:W31") %>%
  janitor::clean_names() %>%
  mutate(xbh = x2b + x3b + hr)

pitchers_2021 <- read_sheet(ss = dukes_sheet_url,
                            sheet = "2021 Season",
                            range = "2021 Season!C34:K41") %>%
  janitor::clean_names() %>%
  dplyr::rename(player = pitchers)

####################################
# Load BBF single A data
####################################

# Statistics taken from British Baseball Federation website on 05/11/2022
# https://stats.britishbaseball.org.uk/en/events/2022-a-league/stats/general/1332

read_single_a <- function(category, num_lines) {
  
  stopifnot(category %in% c("fielding", "pitching", "hitting"))
  
  data_path <- "C:/Users/Joe/Documents/richmond_dukes_baseball/data/"
  
  all_data <- read_excel(paste0(data_path, "single_a_", category, 
                                "_regular_season.xlsx"),
                         n_max = num_lines) %>%
    janitor::clean_names() %>%
    filter(player != "Totals")
  
  first_names <- all_data %>%
    filter(is.na(team)) %>%
    rename(first_name = player) %>%
    select(first_name)
  
  surnames_and_data <- all_data %>%
    filter(!is.na(team)) %>%
    rename(surname = player)
  
  joined <- cbind(first_names, surnames_and_data)
  
  return(joined)
  
}

single_a_fielding <- read_single_a("fielding", 1066)

single_a_hitting <- read_single_a("hitting", 1198) %>%
  mutate(xbh = x2b + x3b + hr,
         # RIU is Richmond Dukes
         club = ifelse(team == "RIU", "Dukes", "Opponents"))

single_a_pitching <- read_single_a("pitching", 410) %>%
  mutate(strike_out_rate = (so/ip) * 9,
         whip = (bb+h) / ip)

single_a_hitting_filtered <- single_a_hitting %>%
  filter(ab >= hitting_pa_threshold)

single_a_pitching_filtered <- single_a_pitching %>%
  filter(ip >= pitching_ip_threshold)

#############################################################
# Plot graphs
#############################################################
# Dukes’ Offensive Improvement
####################################

# Specification from Alvin:
# Team total for hits (H), extra-base hits (XBH) (which are Doubles (2B) 
# + Triples (3B) + Home Runs (HR)), runs (R), runs batted in (RBI) and 
# stolen bases (SB) in 2022 compared to 2021.

hit_totals_2021 <- hitters_2021 %>%
  select(-c(player, number)) %>%
  summarise_all(sum) %>%
  mutate(year = "2021") %>%
  select(year, h, xbh, r, rbi, sb)
  
hit_totals_2022 <- hitters_2022 %>%
  select(-c(player, number)) %>%
  summarise_all(sum) %>%
  mutate(year = "2022") %>%
  select(year, h, xbh, r, rbi, sb)

totals_both_years <- rbind(hit_totals_2022, hit_totals_2021) %>%
  rename(Hits = h,
         `Extra base hits` = xbh,
         Runs = r,
         `Runs batted in` = rbi,
         `Stolen bases` = sb) %>%
  pivot_longer(cols = -(year),
               names_to = "category")

improving_plot <- ggplot(totals_both_years, aes(x = year, y = value)) +
  scale_fill_manual(values=c("#FFFFFF", dukes_red)) +
  geom_col(aes(fill = year), colour = "black") +
  facet_wrap(~category) +
  theme_bw() +
  alvin_plot_theme +
  theme(legend.position = "none") +
  labs(y = "", x = "", title = "Dukes’ Offensive Improvement",
       subtitle = hitting_subtitle)

improving_plot_logo <- add_dukes_logo(improving_plot, l = 1,
                                      r = 0.7, b = 0.1, t = 0.4)

save_dukes_plot(improving_plot_logo, "Dukes_offensive_improvement")

####################################
# Batting average versus slugging average
####################################

# Specification from Alvin:
# Batting average (AVE) and slugging (SLG) percentage 
# (for individual players & include the league ave).

avg_vs_slg <- ggplot(hitters_2022 %>%
         rename(`Batting Average` = avg,
                `Slugging Percentage` = slg) %>%
  filter(pa >= hitting_pa_threshold), 
  aes(x = `Batting Average`, y = `Slugging Percentage`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
    theme_bw() +
    alvin_plot_theme +
    labs(title = "Dukes’ Batting Average and Slugging Percentage",
         subtitle = hitting_subtitle) +
    # League average
    geom_point(shape = 3, size = 5,
               colour = "#000000", 
               aes(x = median(single_a_hitting_filtered$avg),
                   y = median(single_a_hitting_filtered$slg)),
               stroke = 2) +
    ylim(0, 1) +
    xlim(0, 0.6) +
    annotate("text", x = 0.51, 
             y = 0.45,
             label = "League average",
             fontface = "bold")

avg_vs_slg_logo <- add_dukes_logo(avg_vs_slg)

save_dukes_plot(avg_vs_slg_logo, "Batting average vs slugging")

####################################
# On-Base Percentage (OBP) vs Strike outs (K) 
####################################

# — On-Base Percentage (OBP) vs Strike outs (K) (for individual players & 
# include the league ave).

obp_vs_so <- ggplot(hitters_2022 %>%
                      rename(`On-base Percentage` = obp,
                             `Strikeouts` = so) %>%
         filter(pa >= hitting_pa_threshold), 
         aes(x = `Strikeouts`, y = `On-base Percentage`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  alvin_plot_theme +
  ylim(0, 1) +
  xlim(0, 15) +
  labs(title = "On-base Percentage versus Strikeouts",
       subtitle = hitting_subtitle) +
  # League average
  geom_point(shape = 3, size = 5,
             colour = "#000000", 
             stroke = 2,
             aes(y = median(single_a_hitting_filtered$obp), 
                 x = median(single_a_hitting_filtered$so))) +
  annotate("text", x = 5, 
           y = 0.8,
           label = "League average",
           fontface = "bold") +
  geom_segment(aes(x = 5,
                   y = 0.75,
                   xend = 5,
                   yend = 0.65),
               arrow = arrow(length = unit(0.5, "cm")))

obp_vs_so_logo <- add_dukes_logo(obp_vs_so)

save_dukes_plot(obp_vs_so_logo, "OBP versus Strike outs")

# Flip the axes (requested by Alvin)
so_vs_obp <- ggplot(hitters_2022 %>%
                      rename(`On-base percentage` = obp,
                             `Strikeouts` = so) %>%
                      filter(pa >= hitting_pa_threshold), 
                    aes(x = `On-base percentage`, y = `Strikeouts`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  alvin_plot_theme +
  xlim(0, 1) +
  ylim(0, 15) +
  labs(title = "Strikeouts versus On-base Percentage",
       subtitle = hitting_subtitle) +
  # League average
  geom_point(shape = 3, size = 5,
             colour = "#000000", 
             stroke = 2,
             aes(y =  median(single_a_hitting_filtered$so),
                 x = median(single_a_hitting_filtered$obp))) +
  annotate("text", x = 0.75, 
           y = 5,
           label = "League average",
           fontface = "bold")

so_vs_obp_logo <- add_dukes_logo(so_vs_obp)

save_dukes_plot(so_vs_obp_logo, "Strike outs versus OBP")

####################################
# Stolen Bases
####################################

# — Stolen bases (SB) (for individual players & include the league ave).

sb_plot <- ggplot(hitters_2022 %>%
                    rename(`Stolen Bases` = sb) %>%
                    filter(pa >= hitting_pa_threshold), 
                  aes(x = reorder(player, `Stolen Bases`),
                                         y = `Stolen Bases`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  alvin_plot_theme +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", title = "Stolen Bases",
       subtitle = hitting_subtitle) +
  ylim(0, 40) +
  geom_hline(yintercept = median(single_a_hitting_filtered$sb),
             linetype = "dashed") +
  annotate("text", x = 3, 
           y = 18,
           label = "League average",
           fontface = "bold")

sb_plot_logo <- add_dukes_logo(sb_plot)

save_dukes_plot(sb_plot_logo, "Stolen bases")

####################################
# Extra-base hits
####################################

# — Extra-base hits (XBH) (for individual players & include the league ave).

xbh_plot <- ggplot(hitters_2022 %>%
                    rename(`Extra Base Hits` = xbh) %>%
                    filter(pa >= hitting_pa_threshold), 
                   aes(x = reorder(player, `Extra Base Hits`),
                                         y = `Extra Base Hits`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  alvin_plot_theme +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", title = "Extra Base Hits",
       subtitle = hitting_subtitle) +
  geom_hline(yintercept = median(single_a_hitting_filtered$xbh),
             linetype = "dashed") +
  annotate("text", x = 3, 
           y = 3,
           label = "League average",
           fontface = "bold")

xbh_plot_logo <- add_dukes_logo(xbh_plot)

save_dukes_plot(xbh_plot_logo, "Extra base hits")

####################################
# Strike out rate
####################################

# — Pitching: Strike out rate (K/9) for individual players vs league average.

k9_plot <- ggplot(pitchers_2022 %>%
         rename(`Strikeout Rate` = k_9) %>%
         # Players need at least 13 innings pitched
         filter(ip > pitching_ip_threshold), 
         aes(x = reorder(player, `Strikeout Rate`),
                              y = `Strikeout Rate`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  alvin_plot_theme +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", title = "Pitching: Strikeout Rate (K/9)",
       subtitle = pitching_subtitle) +
  ylim(0, 20) +
  geom_hline(yintercept = median(single_a_pitching_filtered$strike_out_rate),
             linetype = "dashed") +
  annotate("text", x = 3, 
           y = 11,
           label = "League average",
           fontface = "bold")

k9_plot_logo <- add_dukes_logo(k9_plot)

save_dukes_plot(k9_plot_logo, "Strike out rate")

####################################
# Strike outs
####################################

# — Pitching: number of Strike outs (K) for individual players vs league average.

so_plot <- ggplot(pitchers_2022 %>%
                    rename(`Strikeouts` = so) %>%
                    # Players need at least 13 innings pitched
                    filter(ip > pitching_ip_threshold), 
                  aes(x = reorder(player, `Strikeouts`),
                                         y = `Strikeouts`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  alvin_plot_theme +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", title = "Pitchers: Strikeouts",
       subtitle = pitching_subtitle) +
  ylim(0, 100) +
  geom_hline(yintercept = median(single_a_pitching_filtered$so),
             linetype = "dashed") +
  annotate("text", x = 4, 
           y = 32,
           label = "League average",
           fontface = "bold")

so_plot_logo <- add_dukes_logo(so_plot)

save_dukes_plot(so_plot_logo, "Strike outs")

####################################
# Walks plus Hits per Inning Pitched
####################################

# — Pitching: Walks + Hits per Inning Pitched (WHIP) for individual players vs 
# league average.

whip_plot <- ggplot(pitchers_2022 %>%
                    rename(WHIP = whip) %>%
                    filter(ip > pitching_ip_threshold), 
                    aes(x = reorder(player, WHIP),
                                         y = WHIP)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  alvin_plot_theme +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", title = "Pitchers: Walks and Hits per Inning Pitched (WHIP)",
       subtitle = pitching_subtitle) +
  ylim(0, 5) +
  geom_hline(yintercept = median(single_a_pitching_filtered$whip),
             linetype = "dashed") +
  annotate("text", x = 2, 
           y = 4,
           label = "League average",
           fontface = "bold")

whip_plot_logo <- add_dukes_logo(whip_plot)

save_dukes_plot(whip_plot_logo, "WHIP")

####################################
# Walk off plot
####################################

# - Total games played in BBF Single A (337) compared to 
# the number of games ending in a walk-off (37) compared to 
# the number of games won by Richmond Dukes in a walk-off (3).

# Non stacked version

walk_off_data1 <- data.frame(games = c(1,1,1),
                            class = c("Total Single A games",
                                 "Total Single A walk-off wins",
                                 "Dukes walk-off wins"),
                       values = c(337, 37, 3)) %>%
  mutate(class = factor(class, levels  = c("Total Single A games",
                        "Total Single A walk-off wins",
                        "Dukes walk-off wins")))

walk_off_plot1 <- ggplot(walk_off_data1, aes(x = class, y = values, fill = class)) +
  geom_col(colour = "black") +
  scale_fill_manual(values = c("#FF999999", "#FF6666", dukes_red)) +
  labs(y = "Games", x = "",
       title = "BBF Single A walk-off wins",
       caption = "Data from 2022 regular season") +
  theme_bw() +
  alvin_plot_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  geom_text(aes(label = class), vjust = -0.5)

save_dukes_plot(walk_off_plot1, "Walk Off Games not stacked")

# Stacked version

walk_off_data2 <- data.frame(games = c(1,1,1),
                             class = c("Total Single A games",
                                       "Walk-off wins",
                                       "Dukes walk-off wins"),
                             values = c(300, 34, 3)) %>%
  mutate(class = factor(class, levels  = c("Total Single A games",
                                           "Walk-off wins",
                                           "Dukes walk-off wins")))

walk_off_plot2 <- ggplot(walk_off_data2, aes(x = games, y = values, fill = class)) +
  geom_col(colour = "black") +
  scale_fill_manual(values = c("#FF999999", "#FF6666", dukes_red)) +
  theme_bw() +
  alvin_plot_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  geom_text(aes(label = class), vjust = -0.5) +
  labs(y = "Games", x = "",
       title = "BBF Single A games",
       caption = "Data from 2022 regular season")

save_dukes_plot(walk_off_plot2, "Walk Off Games stacked")

####################################
# Plotting with the whole single A dataset
####################################

single_a_hitting_plot <- single_a_hitting %>%
  filter(ab >= hitting_pa_threshold) %>%
  ggplot(aes(x = avg, y = slg)) +
  xlim(0, 1) +
  theme_bw() +
  alvin_plot_theme +
  scale_fill_manual(values = c(dukes_red, "#FFFFFF")) +
  scale_alpha_manual(values = c(1,0.5), guide = "none") +
  geom_point(shape = 21, aes(fill = club, alpha = club), size = 3,
             colour = "black") +
  labs(x = "Batting Average", y = "Slugging Percentage",
       title = "Batting Average and Slugging Percentage\nin BBF Single A",
       subtitle = hitting_subtitle,
       caption = "Data from 2022 regular season") +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.4))

single_a_hitting_plot_logo <- add_dukes_logo(single_a_hitting_plot)

save_dukes_plot(single_a_hitting_plot_logo, "Single A Hitting")

####################################