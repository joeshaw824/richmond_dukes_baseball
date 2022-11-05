################################################################################
## Richmond Dukes Baseball Statistics
################################################################################

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

#############################################################
# Load baseball data
#############################################################
####################################
# Load Dukes data from Google sheet
####################################

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

results_2022 <- hitters_2022 %>%
  left_join(pitchers_2022, by = "player") %>%
  mutate(season = "2022")

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

results_2021 <- hitters_2021 %>%
  left_join(pitchers_2021, by = "player") %>%
  mutate(season = "2021")

dukes_results <- results_2022 %>%
  # Remove columns not in 2021 results
  select(-c(x1b, slg, percent, k_9, k_bb)) %>%
  rbind(results_2021) %>%
  # Extra base hit
  mutate(xbh = x2b + x3b + hr)

####################################
# Load BBF single A data
####################################

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
         club = ifelse(team == "RIU", "Dukes", "Opponents"))

single_a_pitching <- read_single_a("pitching", 410) %>%
  mutate(strike_out_rate = so/9,
         whip = (bb+h) / ip)

single_a_hitting_filtered <- single_a_hitting %>%
  # 20 "at bats" needed for stats
  filter(ab >= 20)

single_a_pitching_filtered <- single_a_pitching %>%
  filter(ip >= 13)

#############################################################
# Plot graphs
#############################################################

dukes_logo <- readPNG("resources/dukes_logo.png", native = TRUE)

dukes_red <- "#CC0000"

opponent_grey <- "#CCCCCC"

alvin_plot_theme <- theme(panel.grid = element_blank(),
                          axis.title = element_text(size = 12, face="bold"),
                          plot.title = element_text(size = 16, face="bold"),
                          axis.text = element_text(face="bold", size = 11,
                                                   colour = "black"))

####################################
# "Improving" Plot
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
  theme(legend.position = "none",   strip.text = element_text(face = "bold",
                                                              size = 12)) +
  labs(y = "", x = "", title = "Dukes Hitting Improvement",
       subtitle = "For players with minimum 20 plate appearances")

improving_plot_logo <- improving_plot +
  patchwork::inset_element(p = dukes_logo,
                left = 0.95,
                right = 0.75,
                bottom = 0.1,
                top = 0.3,
                # Make sure logo isn't clipped
                clip = FALSE)

ggsave(plot = improving_plot_logo, 
       filename = paste0("improving_plot_",
                         format(Sys.time(), "%Y%m%d_%H%M%S"),
                         ".jpeg"),
       path = "plots/", 
       device='jpeg', 
       dpi=600,
       units = "cm",
       width = 15,
       height = 18)

####################################
# Batting average versus slugging average
####################################

# Specification from Alvin:
# Batting average (AVE) and slugging (SLG) percentage 
# (for individual players & include the league ave).

avg_vs_slg <- ggplot(hitters_2022 %>%
         rename(`Batting average` = avg,
                `Slugging` = slg) %>%
  # Remember to only include Dukes players who have qualified 
  # for hitting categories (minimum 20 PA - plate appearances)
  filter(pa > 20), aes(x = `Batting average`, y = `Slugging`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  alvin_plot_theme +
  labs(title = "Dukes’ Batting Average and Slugging percentages",
       subtitle = "For players with minimum 20 plate appearances") +
  # League average
  geom_point(shape = 3, size = 5,
             colour = "#000000", aes(x = 0.422, y = 0.537),
             stroke = 2) +
  ylim(0, 1) +
  xlim(0, 0.6) +
  annotate("text", x = 0.52, y = 0.54, label = "League average",
           fontface = "bold")

avg_vs_slg_logo <- avg_vs_slg +
  patchwork::inset_element(p = dukes_logo,
                           left = 0.95,
                           right = 0.75,
                           bottom = 0.02,
                           top = 0.22,
                           # Make sure logo isn't clipped
                           clip = FALSE)

ggsave(plot = avg_vs_slg_logo, 
       filename = paste0("avg_vs_slg_",
                         format(Sys.time(), "%Y%m%d_%H%M%S"),
                         ".jpeg"),
       path = "plots/", 
       device='jpeg', 
       dpi=600,
       units = "cm",
       width = 15,
       height = 18)

####################################
# Combined plot - Batting
####################################

# — On-Base Percentage (OBP) vs Strike outs (K) (for individual players & include the league ave).

obp_vs_so <- ggplot(hitters_2022 %>%
                      rename(`On base percentage` = obp,
                             `Strike outs` = so) %>%
         filter(pa > 20), aes(x = `On base percentage`, y = `Strike outs`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  xlim(0, 1) +
  labs(title = "On base percentage versus strike-outs") +
  # League average
  geom_point(shape = 3, size = 5,
             colour = "#000000", aes(x = median(single_a_hitting_filtered$obp), 
                                     y = median(single_a_hitting_filtered$so)))

# — Stolen bases (SB) (for individual players & include the league ave).

sb_plot <- ggplot(hitters_2022 %>%
                    rename(`Stolen bases` = sb) %>%
                    filter(pa > 20), aes(x = reorder(player, `Stolen bases`),
                                         y = `Stolen bases`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", title = "Stolen Bases") +
  ylim(0, 40) +
  geom_hline(yintercept = median(single_a_hitting_filtered$sb),
             linetype = "dashed")

# — Extra-base hits (XBH) (for individual players & include the league ave).

xbh_plot <- ggplot(hitters_2022 %>%
                    rename(`Extra base hits` = xbh) %>%
                    filter(pa > 20), aes(x = reorder(player, `Extra base hits`),
                                         y = `Extra base hits`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", title = "Extra base hits") +
  geom_hline(yintercept = median(single_a_hitting_filtered$xbh),
             linetype = "dashed")

hitting_combo_plot <- ggpubr::ggarrange(sb_plot, xbh_plot, obp_vs_so,
                                   ncol = 2, nrow = 2)

####################################
# Combined plot - Pitching
####################################

# — Pitching: Strike out rate (K/9) for individual players vs league average.

k9_plot <- ggplot(pitchers_2022 %>%
         rename(`Strike out rate` = k_9) %>%
         # Players need at least 13 innings pitched
         filter(ip > 13), aes(x = reorder(player, `Strike out rate`),
                              y = `Strike out rate`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", title = "Pitchers: strike out rate") +
  ylim(0, 20) +
  geom_hline(yintercept = median(single_a_pitching_filtered$strike_out_rate),
             linetype = "dashed")

# — Pitching: number of Strike outs (K) for individual players vs league average.

so_plot <- ggplot(pitchers_2022 %>%
                    rename(`Strike outs` = so) %>%
                    # Players need at least 13 innings pitched
                    filter(ip > 13), aes(x = reorder(player, `Strike outs`),
                                         y = `Strike outs`)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", title = "Pitchers: strike outs") +
  ylim(0, 100) +
  geom_hline(yintercept = median(single_a_pitching_filtered$so),
             linetype = "dashed")

# — Pitching: Walks + Hits per Inning Pitched (WHIP) for individual players vs 
# league average.

whip_plot <- ggplot(pitchers_2022 %>%
                    rename(WHIP = whip) %>%
                    # Players need at least 13 innings pitched
                    filter(ip > 13), aes(x = reorder(player, WHIP),
                                         y = WHIP)) +
  geom_point(size = 4, colour = dukes_red, alpha= 0.8) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", title = "Pitchers: Walks and Hits per Inning Pitched (WHIP)") +
  ylim(0, 5) +
  geom_hline(yintercept = median(single_a_pitching_filtered$whip),
             linetype = "dashed")

pitching_plot <- ggpubr::ggarrange(so_plot, k9_plot, whip_plot, 
                                   ncol = 2, nrow = 2)

####################################
# Plotting with the whole single A dataset
####################################

# RIU is the Richmond Dukes
single_a_hitting_plot <- single_a_hitting %>%
  filter(ab >= 20) %>%
  ggplot(aes(x = avg, y = slg)) +
  geom_point(colour = opponent_grey, alpha = 0.8, size = 3) +
  xlim(0, 1) +
  theme_bw() +
  labs(x = "Batting average", y = "Slugging percentage",
       title = "Batting versus slugging in single A baseball: 2022 season") +
  theme(panel.grid = element_blank(),
        legend.title = element_blank()) +
  geom_point(data = single_a_hitting %>%
               filter(ab >= 20 & team == "RIU"),
             colour = dukes_red, alpha = 1, size = 4)

single_a_hitting_plot_logo <- single_a_hitting_plot +
  patchwork::inset_element(p = dukes_logo,
                           left = 0.95,
                           right = 0.75,
                           bottom = 0.02,
                           top = 0.22,
                           # Make sure logo isn't clipped
                           clip = FALSE)

ggsave(plot = single_a_hitting_plot_logo, 
       filename = paste0("single_a_hitting_plot_logo",
                         format(Sys.time(), "%Y%m%d_%H%M%S"),
                         ".jpeg"),
       path = "plots/", 
       device='jpeg', 
       dpi=600,
       units = "cm",
       width = 15,
       height = 18)


####################################
# Extra plots
####################################

dukes_results %>%
  ggplot(aes(x = player, y = sb)) +
  geom_point()+
  geom_hline(yintercept = 40, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90)) +
  annotate(geom = "text", label = "Dinner threshold",
           x = 5, y = 38)
# Add in dinner threshold

# RBI
dukes_results  %>%
  filter(season == "2022") %>%
  ggplot(aes(x = reorder(player, desc(rbi)), y = rbi)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Runs batted in", 
       title = "RBI for 2022 season")

dukes_results  %>%
  ggplot(aes(x = , y = h.x))+
  geom_jitter()

# OBP vs OPS
dukes_results  %>%
  ggplot(aes(x = obp, y = avg)) +
  geom_point(colour = "#FF0000", size = 4, alpha = 0.6) +
  ylim(0, 0.7) +
  labs(x = "On base percentage", y = "Batting average", 
       title = "What can he do? He gets on base") +
  geom_hline(yintercept = 0.2, linetype = "dashed") +
  annotate(geom = "text", label = "Mendoza line",
           x = 0.1, y = 0.22) +
  theme_bw()

# Hits and walks
dukes_results  %>%
  ggplot(aes(x = bb.x, y = so.x)) +
  geom_point(colour = "#FF0000", size = 4, alpha = 0.6) +
  #geom_abline() +
  ylim(0, 30) +
  xlim(0, 30) +
  labs(x = "Walks", y = "Strike outs")


  ylim(0, 0.7) +
  labs(x = "On base percentage", y = "Batting average", 
       title = "What can he do? He gets on base") +
  geom_hline(yintercept = 0.2, linetype = "dashed") +
  annotate(geom = "text", label = "Mendoza line",
           x = 0.1, y = 0.22) +
  theme_bw()

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

# Extra base hits plot
dukes_results %>% ggplot(aes(x = player, y = xbh)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

# Batting average
dukes_results %>% ggplot(aes(x = , y = avg)) +
  geom_boxplot()

####################################