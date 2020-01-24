# Libraries and Options ---------------------------------------------------------------
library(zeligverse)
library(stargazer)
library(plm)
library(data.table)
library(varian)
library(tidyverse)
library(broom)
options(scipen = 999)
options(max.print = 999999)
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Define directories ---------------------------------------------------------------
data_file <- paste0("~/Downloads/") # update this to import data
image_file <- paste0("~/Dropbox/EPSCoR/MSISNet Data/Weather and Climate Change Beliefs/Variation in Climate Change Beliefs (Figures)/") # update this to save images
source_file <- paste0("~/Dropbox/EPSCoR/MSISNet Data/Weather and Climate Change Beliefs/Variation in Climate Change Beliefs (Source Files)/") # update this to save images

# Plot theme ---------------------------------------------------------------
plot_theme <- theme_bw() + 
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        axis.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        axis.text = element_text(size = 7), 
        plot.title = element_text(size = 7, hjust = 0),
        panel.grid = element_blank())

# Get panel data ---------------------------------------------------------------
long.data <- fread(paste0(data_file, "w1_w20_longdata.csv")) # download the data here: http://crcm.ou.edu/epscordata/ - note that wave numbers will change as the survey continues 
long.data$wave_id <- factor(long.data$wave_id, levels = c(
  'Wave 1 (Winter 2014)',
  'Wave 2 (Spring 2014)',
  'Wave 3 (Summer 2014)',   
  'Wave 4 (Fall 2014)',
  'Wave 5 (Winter 2015)',  
  'Wave 6 (Spring 2015)', 
  'Wave 7 (Summer 2015)',   
  'Wave 8 (Fall 2015)',
  'Wave 9 (Winter 2016)',
  'Wave 10 (Spring 2016)', 
  'Wave 11 (Summer 2016)',  
  'Wave 12 (Fall 2016)',
  'Wave 13 (Winter 2017)',
  'Wave 14 (Spring 2017)',
  'Wave 15 (Summer 2017)',
  'Wave 16 (Fall 2017)',
  'Wave 17 (Winter 2018)', 
  'Wave 18 (Spring 2018)',
  'Wave 19 (Winter 2019)',
  'Wave 20 (Summer 2019)'))
long.data$wave_num <- as.numeric(long.data$wave_id)
long.data$p_id <- long.data$userid

long.data <- long.data %>% 
  filter(wave_num %in% 3:18) %>% # exclude start up waves (1 & 2) and most recent waves (19 & 20)
  filter(is_statewide == 1) %>% # exclude oversamples in study areas
  drop_na(glbcc, glbcc_cert) %>% # drop if missing climate change beliefs
  group_by(p_id) %>%
  add_count(name = "wave_count") %>% # add wave count variable
  filter(wave_count >= 2) # drop if wave count is less than 2
pdim(long.data) # n = 2115

# Add page times to panel data  ---------------------------------------------------------------
page_times <- read_csv("https://raw.githubusercontent.com/ripberjt/msisnet/master/glbcc_pagetimes.csv") # available here: https://github.com/ripberjt/msisnet
long.data <- merge(long.data, page_times, by = c("p_id", "wave_num"))
pdim(long.data) # n = 2115

# Make variables for analysis  ---------------------------------------------------------------
long.data <- long.data %>%
  mutate(glbcc_times_cert = ifelse(glbcc == 0, glbcc_cert * -1, glbcc_cert), # belief x certainty variable
         sd_glbcc_times_cert = sd_id(glbcc_times_cert, p_id), # belief x certainty sd 
         rmssd_glbcc_times_cert = rmssd_id(glbcc_times_cert, p_id), # belief x certainty rmssd
         sd_glbcc_cert = sd_id(glbcc_cert, p_id), # certainty sd
         sd_cncrn_secur = sd_id(cncrn_secur, p_id), # concern sd
         sd_cncrn_health = sd_id(cncrn_health, p_id),
         sd_cncrn_enrgy = sd_id(cncrn_enrgy, p_id),
         sd_cncrn_trns = sd_id(cncrn_trns, p_id),
         sd_cncrn_tax = sd_id(cncrn_tax, p_id),
         sd_cncrn_edu = sd_id(cncrn_edu, p_id),
         sd_cncrn_econ = sd_id(cncrn_econ, p_id))

long.data$party_lean <- NA # party lean variable
long.data$party_lean <- ifelse(long.data$party == 1, 1, long.data$party_lean)
long.data$party_lean <- ifelse(long.data$party == 3 & long.data$lean == 1, 2, long.data$party_lean)
long.data$party_lean <- ifelse(long.data$party == 3 & long.data$lean == 2, 3, long.data$party_lean)
long.data$party_lean <- ifelse(long.data$party == 2, 4, long.data$party_lean)

long.data$mean_sd_concern <- rowMeans(long.data[, c("sd_cncrn_secur", "sd_cncrn_health", 
                                                     "sd_cncrn_enrgy", "sd_cncrn_trns", 
                                                     "sd_cncrn_tax", "sd_cncrn_edu", 
                                                     "sd_cncrn_econ")])

quantile(long.data$time_glbcc, na.rm = TRUE, 0.9)
long.data$time_glbcc <- ifelse(long.data$time_glbcc >= 33, 33, long.data$time_glbcc) # trim at 90th percentile -- 33 seconds
pdim(long.data) # n = 2115

long.data.agg <- long.data %>% 
  group_by(p_id) %>%
  mutate(glbcc_change = ifelse(glbcc == dplyr::lag(glbcc), 0, 1)) %>%
  summarize(wave_count = mean(wave_count),
            count_glbcc_change = sum(glbcc_change, na.rm = TRUE) / (n() - 1), # glbcc changes / opportunities for change
            sd_glbcc_times_cert = mean(sd_glbcc_times_cert),
            sd_glbcc_cert = mean(sd_glbcc_cert),
            sd_cncrn_secur = mean(sd_cncrn_secur),
            sd_cncrn_health = mean(sd_cncrn_health),
            sd_cncrn_enrgy = mean(sd_cncrn_enrgy),
            sd_cncrn_trns = mean(sd_cncrn_trns),
            sd_cncrn_tax = mean(sd_cncrn_tax),
            sd_cncrn_edu = mean(sd_cncrn_edu),
            sd_cncrn_econ = mean(sd_cncrn_econ),
            mean_glbcc_cert = mean(glbcc_cert),
            mean_glbcc_times_cert = mean(glbcc_times_cert),
            rmssd_glbcc_times_cert = mean(rmssd_glbcc_times_cert),
            mean_variability = mean(mean_sd_concern),
            mean_time_glbcc = mean(time_glbcc, na.rm = TRUE),
            ideol = factor(Mode(ideol, na.rm = TRUE)), 
            party_lean = factor(Mode(party_lean, na.rm = TRUE)),
            age = mean(age, na.rm = TRUE),
            male = factor(ifelse(Mode(gender, na.rm = TRUE) == 1, "M", "F")),
            white = factor(ifelse(Mode(race, na.rm = TRUE) == 1, "W", "NW")),
            college = factor(ifelse(Mode(education, na.rm = TRUE) %in% c("6", "7", "8"), "C", "NC")),
            blue_dot = mean(ifelse(is.na(click_bluedot), 0, click_bluedot))) %>% 
  mutate(age = arm::rescale(age, "full")) %>% # scale by 2 sd
  mutate(raw_mean_variability = mean_variability) %>% # keep raw score for comparisons
  mutate(mean_variability = arm::rescale(mean_variability, "full")) %>% # scale by 2 sd
  mutate(raw_mean_time_glbcc = mean_time_glbcc) %>% # keep raw score for comparisons
  mutate(mean_time_glbcc = arm::rescale(mean_time_glbcc, "full")) %>% # scale by 2 sd
  mutate(blue_dot = arm::rescale(blue_dot, "full")) %>% # scale by 2 sd
  mutate(sd_glbcc_times_cert_group = case_when(
    sd_glbcc_times_cert < 1 ~ "G1", 
    between(sd_glbcc_times_cert, 1, 2) ~ "G2", 
    between(sd_glbcc_times_cert, 2, 3) ~ "G3", 
    sd_glbcc_times_cert > 3 ~ "G4")) %>% 
  mutate(rmssd_glbcc_times_cert_group = case_when(
    rmssd_glbcc_times_cert < 1 ~ "G1", 
    between(rmssd_glbcc_times_cert, 1, 2) ~ "G2", 
    between(rmssd_glbcc_times_cert, 2, 3) ~ "G3", 
    rmssd_glbcc_times_cert > 3 ~ "G4")) %>% 
  mutate(ideol = relevel(ideol, ref = "1"), 
         party_lean = relevel(party_lean, ref = "1"))

long.data.agg$male <- relevel(long.data.agg$male, ref = "F")
long.data.agg$white <- relevel(long.data.agg$white, ref = "NW")
long.data.agg$college <- relevel(long.data.agg$college, ref = "NC")
long.data.agg$ideol <- factor(long.data.agg$ideol, levels = c("1", "2", "3", "4", "5", "6", "7"))
long.data.agg$party_lean <- factor(long.data.agg$party_lean, levels = c("1", "2", "3", "4"))

long.data.agg$party_lean_rec <- NA
long.data.agg$party_lean_rec <- ifelse(long.data.agg$party_lean %in% c("1", "2"), "Democrat", long.data.agg$party_lean_rec)
long.data.agg$party_lean_rec <- ifelse(long.data.agg$party_lean %in% c("3", "4"), "Republican", long.data.agg$party_lean_rec)

long.data.agg %>%
  skimr::skim()

# Plot aggregate trends in beliefs (Fig 1) ---------------------------------------------------------------
glbcc_times_cert_means <- long.data %>%
  filter(wave_count >= 10) %>% 
  group_by(wave_num) %>%
  dplyr::summarise(mean = mean(glbcc_times_cert), 
                   n = n(), 
                   sd = sd(glbcc_times_cert), 
                   se = sd/sqrt(n -1)) %>% 
  mutate(wave_num = factor(wave_num))
range(glbcc_times_cert_means$n)

glbcc_times_cert_means_party <- long.data %>% 
  left_join(., long.data.agg %>% select(p_id, party_lean_rec), by = "p_id") %>% 
  drop_na(party_lean_rec) %>% 
  filter(wave_count >= 10) %>% 
  group_by(wave_num, party_lean_rec) %>%
  dplyr::summarise(mean = mean(glbcc_times_cert), 
                   n = n(), 
                   sd = sd(glbcc_times_cert), 
                   se = sd/sqrt(n -1))
glbcc_times_cert_means_party$wave_num <- factor(glbcc_times_cert_means_party$wave_num)
glbcc_times_cert_means_party %>% group_by(party_lean_rec) %>% summarise(min(n), max(n))

fig_1a <- ggplot(data = glbcc_times_cert_means, aes(x = wave_num, y = mean)) +
  geom_line(group = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = mean + 1.96 * se, ymax = mean - 1.96 * se), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  ylim(-10, 10) +
  scale_x_discrete(labels = 1:16) +
  labs(y = "Belief in Climate Change x Certainty", x = "Survey Wave", title = "a") +
  plot_theme

fig_1b <- ggplot(data = glbcc_times_cert_means_party, aes(x = wave_num, y = mean, color = party_lean_rec, group = party_lean_rec)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean + 1.96 * se, ymax = mean - 1.96 * se), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  ylim(-10, 10) +
  scale_x_discrete(labels = 1:16) +
  labs(y = "Belief in Climate Change x Certainty", x = "Survey Wave", color = "", title = "b") +
  scale_color_manual(values = c("Blue", "Red")) +
  plot_theme +
  theme(legend.position = c(0.15, 0.95), 
        legend.background = element_blank()) 

fig_1 <- gridExtra::grid.arrange(fig_1a, fig_1b, ncol = 2, nrow = 1)
ggsave(paste0(image_file, "Fig_1_Aggregate_Trends_in_Beliefs.pdf"), fig_1, height = 90, width = 180, dpi = "retina", units = "mm")

# Plot density of within-subject variation measures (Fig 2) ---------------------------------------------------------------
sd_group_props <- long.data.agg %>% 
  filter(wave_count >= 10) %>%
  group_by(sd_glbcc_times_cert_group) %>% 
  dplyr::summarise(n = n()) %>% 
  mutate(p = (n/sum(n)) * 100)
sum(sd_group_props$n)

fig_2 <- ggplot(filter(long.data.agg, wave_count >= 10), aes(x = sd_glbcc_times_cert)) +
  geom_density(fill = "grey", alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.1), limits = c(0, 0.3)) +
  geom_vline(xintercept = c(1:3), color = "red", linetype = "dashed", size = 0.25) +
  ylab("Density") + xlab("Within-Subject Standard Deviation") +
  plot_theme + 
  annotate("text", x = 0.5, y = 0.3, label = paste0(round(sd_group_props$p[1], 0), "%"), size = 2) +
  annotate("text", x = 1.5, y = 0.3, label = paste0(round(sd_group_props$p[2], 0), "%"), size = 2) +
  annotate("text", x = 2.5, y = 0.3, label = paste0(round(sd_group_props$p[3], 0), "%"), size = 2) +
  annotate("text", x = 5.5, y = 0.3, label = paste0(round(sd_group_props$p[4], 0), "%"), size = 2)
ggsave(paste0(image_file, "Fig_2_SD_Density_Plot.pdf"), fig_2, height = 70, width = 88, dpi = "retina", units = "mm")

# Plot examples from each within-subject variation group (Fig 3) ---------------------------------------------------------------
G1.data <- subset(long.data.agg, wave_count >= 10 & sd_glbcc_times_cert_group == "G1")
G2.data <- subset(long.data.agg, wave_count >= 10 & sd_glbcc_times_cert_group == "G2")
G3.data <- subset(long.data.agg, wave_count >= 10 & sd_glbcc_times_cert_group == "G3")
G4.data <- subset(long.data.agg, wave_count >= 10 & sd_glbcc_times_cert_group == "G4")

set.seed(10)
SG1.data <- subset(long.data, p_id %in% sample(unique(G1.data$p_id), 25, replace = FALSE))
SG2.data <- subset(long.data, p_id %in% sample(unique(G2.data$p_id), 25, replace = FALSE))
SG3.data <- subset(long.data, p_id %in% sample(unique(G3.data$p_id), 25, replace = FALSE))
SG4.data <- subset(long.data, p_id %in% sample(unique(G4.data$p_id), 25, replace = FALSE))

fig_3a <- ggplot(data = SG1.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  labs(y = "Belief in Climate Change x Certainty", x = "Survey Wave", title = "a") + 
  facet_wrap(~ p_id) +
  plot_theme

fig_3b <- ggplot(data = SG2.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  labs(y = "Belief in Climate Change x Certainty", x = "Survey Wave", title = "b") + 
  facet_wrap(~ p_id) + 
  plot_theme

fig_3c <- ggplot(data = SG3.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  labs(y = "Belief in Climate Change x Certainty", x = "Survey Wave", title = "c") + 
  facet_wrap(~ p_id) + 
  plot_theme

fig_3d <- ggplot(data = SG4.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  labs(y = "Belief in Climate Change x Certainty", x = "Survey Wave", title = "d") + 
  facet_wrap(~ p_id) + 
  plot_theme

fig_3 <- gridExtra::arrangeGrob(fig_3a, fig_3b, fig_3c, fig_3d, ncol = 2, nrow = 2)
ggsave(paste0(image_file, "Fig_3_Example_Tracks_by_SD_Group.pdf"), fig_3, height = 180, width = 180, dpi = "retina", units = "mm")

# Note proportions that change beliefs by within-subject variation group ---------------------------------------------------------------
long.data.agg %>% 
  filter(wave_count >= 10) %>% 
  mutate(change = ifelse(count_glbcc_change == 0, 0, 1)) %>% 
  group_by(sd_glbcc_times_cert_group, change) %>% 
  summarise(n = n()) %>% 
  mutate(p = n/sum(n))

# Model variation in beliefs (Table 1) ---------------------------------------------------------------
tab_1a <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 10))
tab_1b <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 10))
tab_1c <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 10))
stargazer(tab_1a, tab_1b, tab_1c, type = "text", single.row = TRUE, digits = 2)

# Plot ideology/party predictions from models (Fig 4) ---------------------------------------------------------------
fig_4a_fit <- zelig(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 10), model = "ls")
fig_4b_fit <- zelig(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 10), model = "ls")

set.seed(10)
fig_4a <- setx(fig_4a_fit, ideol = levels(long.data.agg$ideol)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = ideol, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 4) +
  ggtitle("a") +
  labs(x = "Political Ideology", y = "Within-Subject Standard Deviation") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Strongly\nliberal", 
                              "2" = "Liberal", 
                              "3" = "Slightly\nliberal",
                              "4" = "Middle",
                              "5" = "Slightly\nconservative",
                              "6" = "Conservative",
                              "7" = "Strongly\nconservative")) +
  theme(axis.text.x = element_text(size = 5))

set.seed(10)
fig_4b <- setx(fig_4b_fit, party_lean = levels(long.data.agg$party_lean)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = party_lean, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 4) +
  ggtitle("b") +
  labs(x = "Political Party", y = "Within-Subject Standard Deviation") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Democrat", 
                              "2" = "Independent\nlean Democrat", 
                              "3" = "Independent\nlean Republican",
                              "4" = "Republican")) +
  theme(axis.text.x = element_text(size = 5))
  
fig_4 <- gridExtra::grid.arrange(fig_4a, fig_4b, ncol = 2, nrow = 1)
ggsave(paste0(image_file, "Fig_4_SD_Political_Predictions.pdf"), fig_4, height = 90, width = 180, dpi = "retina", units = "mm")

# Note proportions that change beliefs by ideology/party ---------------------------------------------------------------
long.data.agg$ideol_part <- NA
long.data.agg$ideol_part <- ifelse(long.data.agg$ideol %in% c("1", "2", "3") & long.data.agg$party_lean == "1", "LibDem", long.data.agg$ideol_part)
long.data.agg$ideol_part <- ifelse(long.data.agg$ideol %in% c("5", "6", "7") & long.data.agg$party_lean == "4", "ConRep", long.data.agg$ideol_part)
long.data.agg %>% 
  filter(wave_count >= 10) %>% 
  mutate(change = ifelse(count_glbcc_change == 0, 0, 1)) %>% 
  group_by(ideol_part, change) %>% 
  summarise(n = n()) %>% 
  mutate(p = n/sum(n))

# Plot cross pressure and salience by party  (Fig 5) ---------------------------------------------------------------
wave_20_data <- fread(paste0(data_file, "w1_w20_longdata.csv")) %>% filter(wave_id == "Wave 20 (Summer 2019)") 
wave_20_data$p_id <- wave_20_data$userid

long.data.agg <- wave_20_data %>% 
  select(p_id, grn_pol, grn_sci, sal_glbcc, sal_imm, sal_hlth, sal_econ) %>% 
  right_join(., long.data.agg, by = "p_id")

filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(sal_glbcc, sd_glbcc_times_cert, party_lean_rec) %>% 
  group_by(party_lean_rec) %>% 
  summarise(n()) 

filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(grn_sci, sd_glbcc_times_cert, party_lean_rec) %>% 
  group_by(party_lean_rec) %>% 
  summarise(n()) 

fig_5a <- filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(sal_glbcc, sd_glbcc_times_cert, party_lean_rec) %>% 
  group_by(party_lean_rec, sal_glbcc) %>% 
  summarise(mu = mean(sd_glbcc_times_cert), sd = sd(sd_glbcc_times_cert), n = n() - 1, se = sd / sqrt(n)) %>% 
  mutate(p = round(n/sum(n) * 100, 0)) %>% 
  ggplot(., aes(x = factor(sal_glbcc), y = mu, color = factor(party_lean_rec), label = paste0(p, "%"))) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = mu - 1.96 * se, ymax = mu + 1.96 * se), width = 0.2, position = position_dodge(width = 0.4)) + 
  ylim(0, 4.5) +
  ggtitle("a") +
  labs(x = "Issue Salience", y = "Within-Subject Standard Deviation", color = "") +
  geom_text(aes(y = 0), position = position_dodge(width = 0.8), size = 2, show.legend = FALSE) +
  scale_x_discrete(labels = c("Never", "Less than\nonce per week", "About once\nper week", "Several times\nper week", "Several\ntimes a day")) +
  scale_color_manual(values = c("Blue", "Red")) +
  plot_theme +
  theme(axis.text.x = element_text(size = 5),
        legend.position = c(0.15, 0.95),
        legend.text.align = 0,
        legend.background = element_blank()) 

dem_data <- filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(sal_glbcc, sd_glbcc_times_cert, party_lean_rec) %>% 
  filter(party_lean_rec == "Democrat" & sal_glbcc %in% c(1, 2, 5)) %>% 
  mutate(sal_glbcc = ifelse(sal_glbcc %in% c(1, 2), 0, 1)) %>% 
  select(party_lean_rec, sal_glbcc, sd_glbcc_times_cert)
t.test(sd_glbcc_times_cert ~ sal_glbcc, data = dem_data)

rep_data <- filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(sal_glbcc, sd_glbcc_times_cert, party_lean_rec) %>% 
  filter(party_lean_rec == "Republican" & sal_glbcc %in% c(1, 2, 5)) %>% 
  mutate(sal_glbcc = ifelse(sal_glbcc %in% c(1, 2), 0, 1)) %>% 
  select(party_lean_rec, sal_glbcc, sd_glbcc_times_cert)
t.test(sd_glbcc_times_cert ~ sal_glbcc, data = rep_data)

fig_5b <- filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(grn_sci, sd_glbcc_times_cert, party_lean_rec) %>% 
  group_by(party_lean_rec, grn_sci) %>% 
  summarise(mu = mean(sd_glbcc_times_cert), sd = sd(sd_glbcc_times_cert), n = n() - 1, se = sd / sqrt(n)) %>%
  mutate(p = round(n/sum(n) * 100, 0)) %>% 
  ggplot(., aes(x = factor(grn_sci), y = mu, color = party_lean_rec, label = paste0(p, "%"))) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = mu - 1.96 * se, ymax = mu + 1.96 * se), width = 0.2, position = position_dodge(width = 0.4)) + 
  ylim(0, 4.5) +
  ggtitle("b") +
  labs(x = "Cues from Scientists", y = "Within-Subject Standard Deviation", color = "") +
  geom_text(aes(y = 0), position = position_dodge(width = 0.5), size = 2, show.legend = FALSE) +
  scale_x_discrete(labels = c("Most scientists believe that\nGHGs ARE NOT causing GCC", "Most scientists believe that\nGHGs ARE causing GCC")) +
  scale_color_manual(values = c("Blue", "Red")) +
  plot_theme +
  theme(axis.text.x = element_text(size = 5),
        legend.position = c(0.85, 0.95), 
        legend.text.align = 1,
        legend.background = element_blank()) 

dem_data <- filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(grn_sci, sd_glbcc_times_cert, party_lean_rec) %>% 
  filter(party_lean_rec == "Democrat") %>% 
  select(party_lean_rec, grn_sci, sd_glbcc_times_cert)
t.test(sd_glbcc_times_cert ~ grn_sci, data = dem_data)

rep_data <- filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(grn_sci, sd_glbcc_times_cert, party_lean_rec) %>% 
  filter(party_lean_rec == "Republican") %>% 
  select(party_lean_rec, grn_sci, sd_glbcc_times_cert)
t.test(sd_glbcc_times_cert ~ grn_sci, data = rep_data)

fig_5 <- gridExtra::grid.arrange(fig_5a, fig_5b, ncol = 2, nrow = 1)
ggsave(paste0(image_file, "Fig_5_Salience_and_Cross_Pressure_by_Party.pdf"), fig_5, height = 90, width = 180, dpi = "retina", units = "mm")

# Plot ideology/party predictions from uncertainty cross-section models  (Extended Data Fig 1) ---------------------------------------------------------------
us.cross <- fread(paste0(data_file, "national_crosssection.csv"))
us.cross$glbcc_times_cert <- ifelse(us.cross$glbcc == 0, us.cross$glbcc_cert * -1, us.cross$glbcc_cert)
us.cross$party_lean <- NA
us.cross$party_lean <- ifelse(us.cross$party == 1, 1, us.cross$party_lean)
us.cross$party_lean <- ifelse(us.cross$party == 3 & us.cross$lean == 1, 2, us.cross$party_lean)
us.cross$party_lean <- ifelse(us.cross$party == 3 & us.cross$lean == 2, 3, us.cross$party_lean)
us.cross$party_lean <- ifelse(us.cross$party == 2, 4, us.cross$party_lean)
us.cross$ideol <- factor(us.cross$ideol, levels = c("1", "2", "3", "4", "5", "6", "7"))
us.cross$party_lean <- factor(us.cross$party_lean, levels = c("1", "2", "3", "4"))
us.cross$male <- factor(ifelse(us.cross$gend == 1, "M", "F"))
us.cross$white <- factor(ifelse(us.cross$race == 1, "W", "NW"))
us.cross$college <- factor(ifelse(us.cross$education >= 6, "C", "NC"))
us.cross$college <- factor(relevel(factor(us.cross$college), ref = "NC"))
us.cross$age <- arm::rescale(us.cross$age, "full")

fig_E1a_ok_fit <- zelig(mean_glbcc_cert ~ age + male + white + college + ideol, data = filter(long.data.agg, wave_count >= 10), model = "ls")
fig_E1a_us_fit <- zelig(glbcc_cert ~ age + male + white + college + ideol, data = us.cross, model = "ls")
fig_E1b_ok_fit <- zelig(mean_glbcc_cert ~ age + male + white + college + party_lean, data = filter(long.data.agg, wave_count >= 10), model = "ls")
fig_E1b_us_fit <- zelig(glbcc_cert ~ age + male + white + college + party_lean, data = us.cross, model = "ls")

set.seed(10)
fig_E1a <- bind_rows(
  setx(fig_E1a_ok_fit, ideol = levels(long.data.agg$ideol), college = "NC") %>% 
    sim() %>% 
    zelig_qi_to_df() %>% 
    qi_slimmer() %>% 
    mutate(Sample = "Oklahoma"), 
  setx(fig_E1a_us_fit, ideol = levels(long.data.agg$ideol), college = "NC") %>% 
    sim() %>% 
    zelig_qi_to_df() %>% 
    qi_slimmer() %>% 
    mutate(Sample = "US")) %>% 
  ggplot(., aes(x = ideol, y = qi_ci_median)) +
  geom_point(size = 1) +
  ylim(2, 10) +
  ggtitle("a") +
  labs(x = "Political Ideology", y = "Certainty of GCC Beliefs") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  facet_wrap(~Sample) +
  scale_x_discrete(labels = c("1" = "Strongly\nliberal", 
                              "2" = "Liberal", 
                              "3" = "Slightly\nliberal",
                              "4" = "Middle",
                              "5" = "Slightly\nconservative",
                              "6" = "Conservative",
                              "7" = "Strongly\nconservative")) +
  plot_theme +
  theme(axis.text.x = element_text(size = 5),
        strip.background = element_rect(fill = "grey"), 
        strip.text.x = element_text(color = "black", size = 7))

set.seed(10)
fig_E1b <- bind_rows(
  setx(fig_E1b_ok_fit, party_lean = levels(long.data.agg$party_lean), college = "NC") %>% 
    sim() %>% 
    zelig_qi_to_df() %>% 
    qi_slimmer() %>% 
    mutate(Sample = "Oklahoma"), 
  setx(fig_E1b_us_fit, party_lean = levels(long.data.agg$party_lean), college = "NC") %>% 
    sim() %>% 
    zelig_qi_to_df() %>% 
    qi_slimmer() %>% 
    mutate(Sample = "US")) %>% 
  ggplot(., aes(x = party_lean, y = qi_ci_median)) +
  geom_point(size = 1) +
  ylim(2, 10) +
  ggtitle("b") +
  labs(x = "Political Party", y = "Certainty of GCC Beliefs") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  facet_wrap(~Sample) +
  scale_x_discrete(labels = c("1" = "Democrat",
                              "2" = "Independent\nlean Democrat", 
                              "3" = "Independent\nlean Republican",
                              "4" = "Republican")) +
  plot_theme +
  theme(axis.text.x = element_text(size = 5),
        strip.background = element_rect(fill = "grey"), 
        strip.text.x = element_text(color = "black", size = 7))

fig_E1 <- gridExtra::grid.arrange(fig_E1a, fig_E1b, ncol = 1, nrow = 2)
ggsave(paste0(image_file, "Fig_E1_Uncertainty_Political_Predictions_by_Sample.pdf"), fig_E1, height = 180, width = 180, dpi = "retina", units = "mm")

# Plot density of within-subject variation measures by wave count (Extended Data Fig 2) ---------------------------------------------------------------
nrow(long.data.agg %>%  filter(wave_count >= 2))
nrow(long.data.agg %>%  filter(wave_count >= 16))

fig_E2 <- bind_rows(
  long.data.agg %>% filter(wave_count >= 2) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "2+"),
  long.data.agg %>% filter(wave_count >= 3) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "3+"), 
  long.data.agg %>% filter(wave_count >= 4) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "4+"), 
  long.data.agg %>% filter(wave_count >= 5) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "5+"), 
  long.data.agg %>% filter(wave_count >= 6) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "6+"), 
  long.data.agg %>% filter(wave_count >= 7) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "7+"), 
  long.data.agg %>%  filter(wave_count >= 8) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "8+"), 
  long.data.agg %>%  filter(wave_count >= 9) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "9+"), 
  long.data.agg %>%  filter(wave_count >= 10) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "10+"), 
  long.data.agg %>%  filter(wave_count >= 11) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "11+"), 
  long.data.agg %>%  filter(wave_count >= 12) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "12+"), 
  long.data.agg %>%  filter(wave_count >= 13) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "13+"), 
  long.data.agg %>%  filter(wave_count >= 14) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "14+"), 
  long.data.agg %>%  filter(wave_count >= 15) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "15+"), 
  long.data.agg %>%  filter(wave_count >= 16) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "16")) %>% 
  mutate(`Wave Count` = factor(`Wave Count`, levels = c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+",
                                                        "11+", "12+", "13+", "14+", "15+", "16"))) %>% 
  ggplot(aes(x = sd_glbcc_times_cert, color = `Wave Count`)) +
  geom_density() +
  ylab("Density") + xlab("Within-Subject Standard Deviation") +
  guides(color = guide_legend(ncol = 2)) +
  plot_theme
ggsave(paste0(image_file, "Fig_E2_SD_Density_Plot_by_Wave Count.pdf"), fig_E2,  height = 90, width = 180, dpi = "retina", units = "mm")

# Model variation in beliefs by wave count (Extended Data Fig 3) ---------------------------------------------------------------
fig_E3a_fit_2 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 2))
fig_E3a_fit_3 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 3))
fig_E3a_fit_4 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 4))
fig_E3a_fit_5 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 5))
fig_E3a_fit_6 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 6))
fig_E3a_fit_7 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 7))
fig_E3a_fit_8 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 8))
fig_E3a_fit_9 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 9))
fig_E3a_fit_10 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 10))
fig_E3a_fit_11 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 11))
fig_E3a_fit_12 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 12))
fig_E3a_fit_13 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 13))
fig_E3a_fit_14 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 14))
fig_E3a_fit_15 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 15))
fig_E3a_fit_16 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 16))
stargazer(fig_E3a_fit_2, fig_E3a_fit_3, fig_E3a_fit_4, fig_E3a_fit_5, fig_E3a_fit_6, fig_E3a_fit_7, fig_E3a_fit_8, fig_E3a_fit_9, fig_E3a_fit_10, fig_E3a_fit_11, fig_E3a_fit_12, fig_E3a_fit_13, fig_E3a_fit_14, fig_E3a_fit_15, fig_E3a_fit_16,
          type = "text", single.row = TRUE, digits = 2)

estimates <- bind_rows(fig_E3a_fit_2$coefficients, fig_E3a_fit_3$coefficients, fig_E3a_fit_4$coefficients, fig_E3a_fit_5$coefficients, fig_E3a_fit_6$coefficients, fig_E3a_fit_7$coefficients,
                       fig_E3a_fit_8$coefficients, fig_E3a_fit_9$coefficients, fig_E3a_fit_10$coefficients, fig_E3a_fit_11$coefficients, fig_E3a_fit_12$coefficients, fig_E3a_fit_13$coefficients,
                       fig_E3a_fit_14$coefficients, fig_E3a_fit_15$coefficients, fig_E3a_fit_16$coefficients)
estimates$`Wave Count` <- c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", "13+", "14+", "15+", "16")
estimates$`Wave Count` <- factor(estimates$`Wave Count`, levels = c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", 
                                                                    "13+", "14+", "15+", "16")) 
estimates <- estimates %>% gather(term, est, `(Intercept)`:mean_time_glbcc) 
errors <- bind_rows(sqrt(diag(vcov(fig_E3a_fit_2))), sqrt(diag(vcov(fig_E3a_fit_3))), sqrt(diag(vcov(fig_E3a_fit_4))), sqrt(diag(vcov(fig_E3a_fit_5))), sqrt(diag(vcov(fig_E3a_fit_6))), 
                    sqrt(diag(vcov(fig_E3a_fit_7))), sqrt(diag(vcov(fig_E3a_fit_8))), sqrt(diag(vcov(fig_E3a_fit_9))), sqrt(diag(vcov(fig_E3a_fit_10))), sqrt(diag(vcov(fig_E3a_fit_11))),
                    sqrt(diag(vcov(fig_E3a_fit_12))), sqrt(diag(vcov(fig_E3a_fit_13))), sqrt(diag(vcov(fig_E3a_fit_14))), sqrt(diag(vcov(fig_E3a_fit_15))), sqrt(diag(vcov(fig_E3a_fit_16))))
errors <- errors %>% gather(term, est, `(Intercept)`:mean_time_glbcc) 
estimates$se <- errors$est
estimates$term <- factor(estimates$term, levels = c("(Intercept)", "age", "maleM", "whiteW", "collegeC", "blue_dot", "mean_variability", "mean_time_glbcc"), 
                         labels = c("Constant", "Age", "Male", "White", "College", "Survey\nAttention", "Survey\nVariability", "Survey\nTime"))

fig_E3a <- estimates %>% 
  ggplot(., aes(y = est, x = term, group = `Wave Count`, color = `Wave Count`)) +
  geom_point(position = position_dodge(width = 0.7), size = 1) +
  geom_linerange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), position = position_dodge(width = 0.7), size = 0.5) +
  labs(x = "Term", y = "Coefficient Estimate", title = "a") +
  ylim(-1, 4) +
  plot_theme + 
  guides(color = guide_legend(nrow = 2)) +
  theme(legend.justification = c(1, 0.8), 
        legend.position = c(1, 0.92),
        legend.background = element_rect(fill = "transparent"))

fig_E3b_fit_2 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 2))
fig_E3b_fit_3 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 3))
fig_E3b_fit_4 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 4))
fig_E3b_fit_5 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 5))
fig_E3b_fit_6 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 6))
fig_E3b_fit_7 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 7))
fig_E3b_fit_8 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 8))
fig_E3b_fit_9 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 9))
fig_E3b_fit_10 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 10))
fig_E3b_fit_11 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 11))
fig_E3b_fit_12 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 12))
fig_E3b_fit_13 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 13))
fig_E3b_fit_14 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 14))
fig_E3b_fit_15 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 15))
fig_E3b_fit_16 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 16))
stargazer(fig_E3b_fit_2, fig_E3b_fit_3, fig_E3b_fit_4, fig_E3b_fit_5, fig_E3b_fit_6, fig_E3b_fit_7, fig_E3b_fit_8, fig_E3b_fit_9, fig_E3b_fit_10, fig_E3b_fit_11, fig_E3b_fit_12, fig_E3b_fit_13, fig_E3b_fit_14, fig_E3b_fit_15, fig_E3b_fit_16,
          type = "text", single.row = TRUE, digits = 2)

estimates <- bind_rows(fig_E3b_fit_2$coefficients, fig_E3b_fit_3$coefficients, fig_E3b_fit_4$coefficients, fig_E3b_fit_5$coefficients, fig_E3b_fit_6$coefficients, fig_E3b_fit_7$coefficients,
                       fig_E3b_fit_8$coefficients, fig_E3b_fit_9$coefficients, fig_E3b_fit_10$coefficients, fig_E3b_fit_11$coefficients, fig_E3b_fit_12$coefficients, fig_E3b_fit_13$coefficients,
                       fig_E3b_fit_14$coefficients, fig_E3b_fit_15$coefficients, fig_E3b_fit_16$coefficients)
estimates$`Wave Count` <- c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", "13+", "14+", "15+", "16")
estimates$`Wave Count` <- factor(estimates$`Wave Count`, levels = c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", 
                                                                    "13+", "14+", "15+", "16")) 
estimates <- estimates %>% gather(term, est, `(Intercept)`:party_lean4) 

errors <- bind_rows(sqrt(diag(vcov(fig_E3b_fit_2))), sqrt(diag(vcov(fig_E3b_fit_3))), sqrt(diag(vcov(fig_E3b_fit_4))), sqrt(diag(vcov(fig_E3b_fit_5))), sqrt(diag(vcov(fig_E3b_fit_6))), 
                    sqrt(diag(vcov(fig_E3b_fit_7))), sqrt(diag(vcov(fig_E3b_fit_8))), sqrt(diag(vcov(fig_E3b_fit_9))), sqrt(diag(vcov(fig_E3b_fit_10))), sqrt(diag(vcov(fig_E3b_fit_11))),
                    sqrt(diag(vcov(fig_E3b_fit_12))), sqrt(diag(vcov(fig_E3b_fit_13))), sqrt(diag(vcov(fig_E3b_fit_14))), sqrt(diag(vcov(fig_E3b_fit_15))), sqrt(diag(vcov(fig_E3b_fit_16))))
errors <- errors %>% gather(term, est, `(Intercept)`:party_lean4) 
estimates$se <- errors$est
estimates$term <- factor(estimates$term, levels = c("(Intercept)", "age", "maleM", "whiteW", "collegeC", "blue_dot", "mean_variability", "mean_time_glbcc", "party_lean2", "party_lean3", "party_lean4"),
                         labels = c("Constant", "Age", "Male", "White", "College", "Survey\nAttention", "Survey\nVariability", "Survey\nTime", "Lean\nDem", "Lean\nRep", "Rep"))

fig_E3b <- estimates %>% 
  ggplot(., aes(y = est, x = term, group = `Wave Count`, color = `Wave Count`)) +
  geom_point(position = position_dodge(width = 0.7), size = 1) +
  geom_linerange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), position = position_dodge(width = 0.7), size = 0.5) +
  labs(x = "Term", y = "Coefficient Estimate", title = "b") +
  ylim(-1, 4) +
  plot_theme + 
  guides(color = FALSE)

fig_E3c_fit_2 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 2))
fig_E3c_fit_3 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 3))
fig_E3c_fit_4 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 4))
fig_E3c_fit_5 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 5))
fig_E3c_fit_6 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 6))
fig_E3c_fit_7 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 7))
fig_E3c_fit_8 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 8))
fig_E3c_fit_9 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 9))
fig_E3c_fit_10 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 10))
fig_E3c_fit_11 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 11))
fig_E3c_fit_12 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 12))
fig_E3c_fit_13 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 13))
fig_E3c_fit_14 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 14))
fig_E3c_fit_15 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 15))
fig_E3c_fit_16 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 16))
stargazer(fig_E3c_fit_2, fig_E3c_fit_3, fig_E3c_fit_4, fig_E3c_fit_5, fig_E3c_fit_6, fig_E3c_fit_7, fig_E3c_fit_8, fig_E3c_fit_9, fig_E3c_fit_10, fig_E3c_fit_11, fig_E3c_fit_12, fig_E3c_fit_13, fig_E3c_fit_14, fig_E3c_fit_15, fig_E3c_fit_16,
          type = "text", single.row = TRUE, digits = 2)

estimates <- bind_rows(fig_E3c_fit_2$coefficients, fig_E3c_fit_3$coefficients, fig_E3c_fit_4$coefficients, fig_E3c_fit_5$coefficients, fig_E3c_fit_6$coefficients, fig_E3c_fit_7$coefficients,
                       fig_E3c_fit_8$coefficients, fig_E3c_fit_9$coefficients, fig_E3c_fit_10$coefficients, fig_E3c_fit_11$coefficients, fig_E3c_fit_12$coefficients, fig_E3c_fit_13$coefficients,
                       fig_E3c_fit_14$coefficients, fig_E3c_fit_15$coefficients, fig_E3c_fit_16$coefficients)
estimates$`Wave Count` <- c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", "13+", "14+", "15+", "16")
estimates$`Wave Count` <- factor(estimates$`Wave Count`, levels = c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", 
                                                                    "13+", "14+", "15+", "16")) 
estimates <- estimates %>% gather(term, est, `(Intercept)`:ideol7) 

errors <- bind_rows(sqrt(diag(vcov(fig_E3c_fit_2))), sqrt(diag(vcov(fig_E3c_fit_3))), sqrt(diag(vcov(fig_E3c_fit_4))), sqrt(diag(vcov(fig_E3c_fit_5))), sqrt(diag(vcov(fig_E3c_fit_6))), 
                    sqrt(diag(vcov(fig_E3c_fit_7))), sqrt(diag(vcov(fig_E3c_fit_8))), sqrt(diag(vcov(fig_E3c_fit_9))), sqrt(diag(vcov(fig_E3c_fit_10))), sqrt(diag(vcov(fig_E3c_fit_11))),
                    sqrt(diag(vcov(fig_E3c_fit_12))), sqrt(diag(vcov(fig_E3c_fit_13))), sqrt(diag(vcov(fig_E3c_fit_14))), sqrt(diag(vcov(fig_E3c_fit_15))), sqrt(diag(vcov(fig_E3c_fit_16))))
errors <- errors %>% gather(term, est, `(Intercept)`:ideol7) 
estimates$se <- errors$est
estimates$term <- factor(estimates$term, levels = c("(Intercept)", "age", "maleM", "whiteW", "collegeC", "blue_dot", "mean_variability", "mean_time_glbcc", "ideol2", "ideol3", "ideol4", "ideol5", "ideol6", "ideol7"),
                         labels = c("Constant", "Age", "Male", "White", "College", "Survey\nAttention", "Survey\nVariability", "Survey\nTime", "Lib", "Slight\nLib", "Middle", "Slight\nCon", "Con", "Strong\nCon"))

fig_E3c <- estimates %>% 
  ggplot(., aes(y = est, x = term, group = `Wave Count`, color = `Wave Count`)) +
  geom_point(position = position_dodge(width = 0.7), size = 1) +
  geom_linerange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), position = position_dodge(width = 0.7), size = 0.5) +
  labs(x = "Term", y = "Coefficient Estimate", title = "c") +
  ylim(-1, 4) +
  plot_theme + 
  guides(color = FALSE)

fig_E3 <- gridExtra::grid.arrange(fig_E3a, fig_E3b, fig_E3c, ncol = 1, nrow = 3)
ggsave(paste0(image_file, "Fig_E3_Regression_Estimates_by_Wave Count.pdf"), fig_E3,  height = 225, width = 180, dpi = "retina", units = "mm")

# Plot density of RMSSD measures by wave count (Extended Data Fig 4) ---------------------------------------------------------------
rmssd_group_props <- long.data.agg %>% 
  filter(wave_count >= 10) %>%
  group_by(rmssd_glbcc_times_cert_group) %>% 
  dplyr::summarise(n = n()) %>% 
  mutate(p = (n/sum(n)) * 100)
sum(rmssd_group_props$n)

fig_E4 <- ggplot(filter(long.data.agg, wave_count >= 10), aes(x = rmssd_glbcc_times_cert)) +
  geom_density(fill = "grey", alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 13, 1), limits = c(0, 13)) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.1), limits = c(0, 0.3)) +
  geom_vline(xintercept = c(1:3), color = "red", linetype = "dashed", size = 0.25) +
  ylab("Density") + xlab("Within-Subject Root Mean Square of Successive Differences") +
  plot_theme + 
  annotate("text", x = 0.5, y = 0.3, label = paste0(round(rmssd_group_props$p[1], 0), "%"), size = 2) +
  annotate("text", x = 1.5, y = 0.3, label = paste0(round(rmssd_group_props$p[2], 0), "%"), size = 2) +
  annotate("text", x = 2.5, y = 0.3, label = paste0(round(rmssd_group_props$p[3], 0), "%"), size = 2) +
  annotate("text", x = 5.5, y = 0.3, label = paste0(round(rmssd_group_props$p[4], 0), "%"), size = 2)
ggsave(paste0(image_file, "Fig_E4_RMSSD_Density_Plot.pdf"), fig_E4, height = 70, width = 88, dpi = "retina", units = "mm")

# Plot examples from each within-subject RMSSD group (Extended Data Fig 5) ---------------------------------------------------------------
G1S.data <- subset(long.data.agg, wave_count >= 10 & rmssd_glbcc_times_cert_group == "G1")
G2S.data <- subset(long.data.agg, wave_count >= 10 & rmssd_glbcc_times_cert_group == "G2")
G3S.data <- subset(long.data.agg, wave_count >= 10 & rmssd_glbcc_times_cert_group == "G3")
G4S.data <- subset(long.data.agg, wave_count >= 10 & rmssd_glbcc_times_cert_group == "G4")

set.seed(10)
SG1S.data <- subset(long.data, p_id %in% sample(unique(G1S.data$p_id), 25, replace = FALSE))
SG2S.data <- subset(long.data, p_id %in% sample(unique(G2S.data$p_id), 25, replace = FALSE))
SG3S.data <- subset(long.data, p_id %in% sample(unique(G3S.data$p_id), 25, replace = FALSE))
SG4S.data <- subset(long.data, p_id %in% sample(unique(G4S.data$p_id), 25, replace = FALSE))

fig_E5a <- ggplot(data = SG1S.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  labs(y = "Belief in Climate Change x Certainty", x = "Survey Wave", title = "a") + 
  facet_wrap(~ p_id) +
  plot_theme

fig_E5b <- ggplot(data = SG2S.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  labs(y = "Belief in Climate Change x Certainty", x = "Survey Wave", title = "b") + 
  facet_wrap(~ p_id) + 
  plot_theme

fig_E5c <- ggplot(data = SG3S.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  labs(y = "Belief in Climate Change x Certainty", x = "Survey Wave", title = "c") + 
  facet_wrap(~ p_id) + 
  plot_theme

fig_E5d <- ggplot(data = SG4S.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  labs(y = "Belief in Climate Change x Certainty", x = "Survey Wave", title = "d") + 
  facet_wrap(~ p_id) + 
  plot_theme

fig_E5 <- gridExtra::arrangeGrob(fig_E5a, fig_E5b, fig_E5c, fig_E5d, ncol = 2, nrow = 2)
ggsave(paste0(image_file, "Fig_E5_Example_Tracks_by_RMSSD_Groups.pdf"), fig_E5, height = 180, width = 180, dpi = "retina", units = "mm")

# Plot ideology/party predictions from RMSSD models (Extended Data Fig 6) ---------------------------------------------------------------
fig_E6a_fit <- zelig(rmssd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 10), model = "ls")
fig_E6b_fit <- zelig(rmssd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 10), model = "ls")

set.seed(10)
fig_E6a <- setx(fig_E6a_fit, ideol = levels(long.data.agg$ideol)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = ideol, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 4.5) +
  ggtitle("a") +
  labs(x = "Political Ideology", y = "Within-Subject RMSSD") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Strongly\nliberal", 
                              "2" = "Liberal", 
                              "3" = "Slightly\nliberal",
                              "4" = "Middle",
                              "5" = "Slightly\nconservative",
                              "6" = "Conservative",
                              "7" = "Strongly\nconservative")) +
  theme(axis.text.x = element_text(size = 5)) 

set.seed(10)
fig_E6b <- setx(fig_E6b_fit, party_lean = levels(long.data.agg$party_lean)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = party_lean, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 4.5) +
  ggtitle("b") +
  labs(x = "Political Party", y = "Within-Subject RMSSD") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Democrat", 
                              "2" = "Independent\nlean Democrat", 
                              "3" = "Independent\nlean Republican",
                              "4" = "Republican")) +
  theme(axis.text.x = element_text(size = 5))

fig_E6 <- gridExtra::grid.arrange(fig_E6a, fig_E6b, ncol = 2, nrow = 1)
ggsave(paste0(image_file, "Fig_E6_RMSSD_Political_Predictions.pdf"), fig_E6, height = 90, width = 180, dpi = "retina", units = "mm")

# Plot cross pressure and salience by party  (Extended Data Fig 7) ---------------------------------------------------------------
filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(sal_glbcc, rmssd_glbcc_times_cert, party_lean_rec) %>% 
  group_by(party_lean_rec) %>% 
  summarise(n()) 

filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(grn_sci, rmssd_glbcc_times_cert, party_lean_rec) %>% 
  group_by(party_lean_rec) %>% 
  summarise(n()) 

fig_E7a <- filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(sal_glbcc, rmssd_glbcc_times_cert, party_lean_rec) %>% 
  group_by(party_lean_rec, sal_glbcc) %>% 
  summarise(mu = mean(rmssd_glbcc_times_cert), sd = sd(rmssd_glbcc_times_cert), n = n() - 1, se = sd / sqrt(n)) %>% 
  mutate(p = round(n/sum(n) * 100, 0)) %>% 
  ggplot(., aes(x = factor(sal_glbcc), y = mu, color = factor(party_lean_rec), label = paste0(p, "%"))) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = mu - 1.96 * se, ymax = mu + 1.96 * se), width = 0.2, position = position_dodge(width = 0.4)) + 
  ylim(0, 6) +
  ggtitle("a") +
  labs(x = "Issue Salience", y = "Within-Subject RMSSD", color = "") +
  geom_text(aes(y = 0), position = position_dodge(width = 0.8), size = 2, show.legend = FALSE) +
  scale_x_discrete(labels = c("Never", "Less than\nonce per week", "About once\nper week", "Several times\nper week", "Several\ntimes a day")) +
  scale_color_manual(values = c("Blue", "Red")) +
  plot_theme +
  theme(axis.text.x = element_text(size = 5),
        legend.position = c(0.15, 0.95),
        legend.text.align = 0,
        legend.background = element_blank()) 

fig_E7b <- filter(long.data.agg, wave_count >= 10) %>% 
  drop_na(grn_sci, rmssd_glbcc_times_cert, party_lean_rec) %>% 
  group_by(party_lean_rec, grn_sci) %>% 
  summarise(mu = mean(rmssd_glbcc_times_cert), sd = sd(rmssd_glbcc_times_cert), n = n() - 1, se = sd / sqrt(n)) %>%
  mutate(p = round(n/sum(n) * 100, 0)) %>% 
  ggplot(., aes(x = factor(grn_sci), y = mu, color = party_lean_rec, label = paste0(p, "%"))) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = mu - 1.96 * se, ymax = mu + 1.96 * se), width = 0.2, position = position_dodge(width = 0.4)) + 
  ylim(0, 6) +
  ggtitle("b") +
  labs(x = "Cues from Scientists", y = "Within-Subject RMSSD", color = "") +
  geom_text(aes(y = 0), position = position_dodge(width = 0.5), size = 2, show.legend = FALSE) +
  scale_x_discrete(labels = c("Most scientists believe that\nGHGs ARE NOT causing GCC", "Most scientists believe that\nGHGs ARE causing GCC")) +
  scale_color_manual(values = c("Blue", "Red")) +
  plot_theme +
  theme(axis.text.x = element_text(size = 5),
        legend.position = c(0.85, 0.95), 
        legend.text.align = 1,
        legend.background = element_blank()) 

fig_E7 <- gridExtra::grid.arrange(fig_E7a, fig_E7b, ncol = 2, nrow = 1)
ggsave(paste0(image_file, "Fig_E7_RMSSD_Salience_and_Cross_Pressure_by_Party.pdf"), fig_E7, height = 90, width = 180, dpi = "retina", units = "mm")

# Plot ideology/party predictions from prob change models (Extended Data Fig 8) ---------------------------------------------------------------
fig_E8a_fit <- zelig(ifelse(count_glbcc_change == 0, 0, 1) ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 10), model = "logit")
fig_E8b_fit <- zelig(ifelse(count_glbcc_change == 0, 0, 1) ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 10), model = "logit")

set.seed(10)
fig_E8a <- setx(fig_E8a_fit, ideol = levels(long.data.agg$ideol)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = ideol, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 0.75) +
  ggtitle("a") +
  labs(x = "Political Ideology", y = "Prob(One or More Change in GCC Beliefs)") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Strongly\nliberal", 
                              "2" = "Liberal", 
                              "3" = "Slightly\nliberal",
                              "4" = "Middle",
                              "5" = "Slightly\nconservative",
                              "6" = "Conservative",
                              "7" = "Strongly\nconservative")) +
  theme(axis.text.x = element_text(size = 5))

set.seed(10)
fig_E8b <- setx(fig_E8b_fit, party_lean = levels(long.data.agg$party_lean)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = party_lean, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 0.75) +
  ggtitle("b") +
  labs(x = "Political Party", y = "Prob(One or More Change in GCC Beliefs)") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Democrat", 
                              "2" = "Independent\nlean Democrat", 
                              "3" = "Independent\nlean Republican",
                              "4" = "Republican")) +
  theme(axis.text.x = element_text(size = 5))

fig_E8 <- gridExtra::grid.arrange(fig_E8a, fig_E8b, ncol = 2, nrow = 1)
ggsave(paste0(image_file, "Fig_E8_Prob_Change_Political_Predictions.pdf"), fig_E8, height = 90, width = 180, dpi = "retina", units = "mm")

# Plot variation in beliefs before and after trump  (Extended Data Fig 9) ---------------------------------------------------------------
long.data %>%
  filter(wave_count >= 10 & wave_num %in% 7:18) %>%
  mutate(
    glbcc_times_cert = ifelse(glbcc == 0, glbcc_cert * -1, glbcc_cert),
    trump = ifelse(wave_num > 12, "Post Trump Election", "Pre Trump Election")) %>%
  arrange(p_id) %>%
  select(p_id, glbcc_times_cert, wave_num, trump) %>%
  group_by(p_id, trump) %>%
  summarise(sd_glbcc_times_cert = sd(glbcc_times_cert)) %>%
  add_count(p_id) %>%
  filter(n >= 2) %>%
  left_join(., long.data.agg %>% select(p_id, ideol, party_lean), by = "p_id") %>%
  mutate(trump = factor(trump, levels = c("Pre Trump Election", "Post Trump Election"))) %>%
  drop_na(ideol, sd_glbcc_times_cert) %>%
  group_by(trump) %>% 
  count()

fig_E9a <- long.data %>%
  filter(wave_count >= 10 & wave_num %in% 7:18) %>%
  mutate(
    glbcc_times_cert = ifelse(glbcc == 0, glbcc_cert * -1, glbcc_cert),
    trump = ifelse(wave_num > 12, "Post Trump Election", "Pre Trump Election")) %>%
  arrange(p_id) %>%
  select(p_id, glbcc_times_cert, wave_num, trump) %>%
  group_by(p_id, trump) %>%
  summarise(sd_glbcc_times_cert = sd(glbcc_times_cert)) %>%
  add_count(p_id) %>%
  filter(n >= 2) %>%
  left_join(., long.data.agg %>% select(p_id, ideol, party_lean), by = "p_id") %>%
  mutate(trump = factor(trump, levels = c("Pre Trump Election", "Post Trump Election"))) %>%
  drop_na(ideol, sd_glbcc_times_cert) %>%
  group_by(ideol, trump) %>% 
  summarise(mu = mean(sd_glbcc_times_cert), sd = sd(sd_glbcc_times_cert), n = n() - 1, se = sd / sqrt(n)) %>% 
  ggplot(., aes(x = ideol, y = mu, color = trump)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = mu - 1.96 * se, ymax = mu + 1.96 * se), width = 0.2, position = position_dodge(width = 0.4)) + 
  scale_color_manual(values = c("grey", "black")) +
  ylim(0, 4) +
  ggtitle("a") +
  labs(x = "Political Ideology", y = "Within-Subject Standard Deviation", color = "") +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Strongly\nliberal", 
                              "2" = "Liberal", 
                              "3" = "Slightly\nliberal",
                              "4" = "Middle",
                              "5" = "Slightly\nconservative",
                              "6" = "Conservative",
                              "7" = "Strongly\nconservative")) +
  theme(axis.text.x = element_text(size = 5),
        legend.position = c(0.2, 0.95),
        legend.text.align = 0,
        legend.background = element_blank()) 

fig_E9b <- long.data %>%
  filter(wave_count >= 10 & wave_num %in% 7:18) %>%
  mutate(
    glbcc_times_cert = ifelse(glbcc == 0, glbcc_cert * -1, glbcc_cert),
    trump = ifelse(wave_num > 12, "Post Trump Election", "Pre Trump Election")) %>%
  arrange(p_id) %>%
  select(p_id, glbcc_times_cert, wave_num, trump) %>%
  group_by(p_id, trump) %>%
  summarise(sd_glbcc_times_cert = sd(glbcc_times_cert)) %>%
  add_count(p_id) %>%
  filter(n >= 2) %>%
  left_join(., long.data.agg %>% select(p_id, ideol, party_lean), by = "p_id") %>%
  mutate(trump = factor(trump, levels = c("Pre Trump Election", "Post Trump Election"))) %>%
  drop_na(party_lean, sd_glbcc_times_cert) %>%
  group_by(party_lean, trump) %>% 
  summarise(mu = mean(sd_glbcc_times_cert), sd = sd(sd_glbcc_times_cert), n = n() - 1, se = sd / sqrt(n)) %>% 
  ggplot(., aes(x = party_lean, y = mu, color = trump)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = mu - 1.96 * se, ymax = mu + 1.96 * se), width = 0.2, position = position_dodge(width = 0.4)) + 
  scale_color_manual(values = c("grey", "black")) +
  ylim(0, 4) +
  ggtitle("b") +
  labs(x = "Political Party", y = "Within-Subject Standard Deviation", color = "") +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Democrat", 
                              "2" = "Independent\nlean Democrat", 
                              "3" = "Independent\nlean Republican",
                              "4" = "Republican")) +
  theme(axis.text.x = element_text(size = 5),
        legend.position = c(0.2, 0.95),
        legend.text.align = 0,
        legend.background = element_blank()) 

dem_data <- long.data %>%
  filter(wave_count >= 10 & wave_num %in% 7:18) %>%
  mutate(
    glbcc_times_cert = ifelse(glbcc == 0, glbcc_cert * -1, glbcc_cert),
    trump = ifelse(wave_num > 12, "Post Trump Election", "Pre Trump Election")) %>%
  arrange(p_id) %>%
  select(p_id, glbcc_times_cert, wave_num, trump) %>%
  group_by(p_id, trump) %>%
  summarise(sd_glbcc_times_cert = sd(glbcc_times_cert)) %>%
  add_count(p_id) %>%
  filter(n >= 2) %>%
  left_join(., long.data.agg %>% select(p_id, ideol, party_lean), by = "p_id") %>%
  mutate(trump = factor(trump, levels = c("Pre Trump Election", "Post Trump Election"))) %>%
  drop_na(party_lean, sd_glbcc_times_cert) %>%
  group_by(party_lean, trump) %>% 
  filter(party_lean == 1) %>% 
  select(sd_glbcc_times_cert, trump, party_lean)
t.test(sd_glbcc_times_cert ~ trump, data = dem_data)

rep_data <- long.data %>%
  filter(wave_count >= 10 & wave_num %in% 7:18) %>%
  mutate(
    glbcc_times_cert = ifelse(glbcc == 0, glbcc_cert * -1, glbcc_cert),
    trump = ifelse(wave_num > 12, "Post Trump Election", "Pre Trump Election")) %>%
  arrange(p_id) %>%
  select(p_id, glbcc_times_cert, wave_num, trump) %>%
  group_by(p_id, trump) %>%
  summarise(sd_glbcc_times_cert = sd(glbcc_times_cert)) %>%
  add_count(p_id) %>%
  filter(n >= 2) %>%
  left_join(., long.data.agg %>% select(p_id, ideol, party_lean), by = "p_id") %>%
  mutate(trump = factor(trump, levels = c("Pre Trump Election", "Post Trump Election"))) %>%
  drop_na(party_lean, sd_glbcc_times_cert) %>%
  group_by(party_lean, trump) %>% 
  filter(party_lean == 4) %>% 
  select(sd_glbcc_times_cert, trump, party_lean)
t.test(sd_glbcc_times_cert ~ trump, data = rep_data)

fig_E9 <- gridExtra::grid.arrange(fig_E9a, fig_E9b, ncol = 2, nrow = 1)
ggsave(paste0(image_file, "Fig_E9_SD_PrePost_Trump.pdf"), fig_E9, height = 90, width = 180, dpi = "retina", units = "mm")

# Plot ideology/party predictions from survey variability models (Extended Data Fig 10) ---------------------------------------------------------------
fig_E10a_fit <- zelig(raw_mean_variability ~ age + male + white + college + blue_dot + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 10), model = "ls")
fig_E10b_fit <- zelig(raw_mean_variability ~ age + male + white + college + blue_dot + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 10), model = "ls")

set.seed(10)
fig_E10a <- setx(fig_E10a_fit, ideol = levels(long.data.agg$ideol)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = ideol, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 3) +
  ggtitle("a") +
  labs(x = "Political Ideology", y = "Survey Variability") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Strongly\nliberal", 
                              "2" = "Liberal", 
                              "3" = "Slightly\nliberal",
                              "4" = "Middle",
                              "5" = "Slightly\nconservative",
                              "6" = "Conservative",
                              "7" = "Strongly\nconservative")) +
  theme(axis.text.x = element_text(size = 5))

set.seed(10)
fig_E10b <- setx(fig_E10b_fit, party_lean = levels(long.data.agg$party_lean)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = party_lean, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 3) +
  ggtitle("b") +
  labs(x = "Political Party", y = "Survey Variability") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Democrat", 
                              "2" = "Independent\nlean Democrat", 
                              "3" = "Independent\nlean Republican",
                              "4" = "Republican")) +
  theme(axis.text.x = element_text(size = 5))

fig_E10 <- gridExtra::grid.arrange(fig_E10a, fig_E10b, ncol = 2, nrow = 1)
ggsave(paste0(image_file, "Fig_E10_Survey_Variability_Political_Predictions.pdf"), fig_E10, height = 90, width = 180, dpi = "retina", units = "mm")

# Compare variation across survey variability items (Supplementary Information Fig 1) ---------------------------------------------------------------
fig_S1 <- long.data.agg %>% 
  filter(wave_count >= 10) %>% 
  select(sd_glbcc_cert, sd_cncrn_secur, sd_cncrn_health, sd_cncrn_enrgy, sd_cncrn_trns, sd_cncrn_tax, sd_cncrn_edu, sd_cncrn_econ) %>% 
  gather(variable, value, sd_glbcc_cert:sd_cncrn_econ) %>%
  mutate(variable = factor(variable, levels = rev(c("sd_glbcc_cert", "sd_cncrn_secur", "sd_cncrn_health", "sd_cncrn_enrgy", "sd_cncrn_trns", "sd_cncrn_tax",  "sd_cncrn_edu",  "sd_cncrn_econ")))) %>% 
  ggplot(., aes(x = value, y = variable)) +
  ggridges::geom_density_ridges() +
  labs(x = "Within-Subject SD", y = "") +
  xlim(0, 5) +
  scale_y_discrete(labels = rev(c("Certainty of GCC Beliefs", "Concern about Security", "Concern about Healthcare", "Concern about Energy", "Concern about Transportation",
                              "Concern about Taxes", "Concern about Education", "Concern about the Economy"))) +
  plot_theme
ggsave(paste0(image_file, "Fig_S1_Variability_Items_Density_Plots.pdf"), fig_S1, height = 90, width = 180, dpi = "retina", units = "mm")

# Compare sample to US cross-sections (Supplementary Table 1) ---------------------------------------------------------------
tab_S1a <- lm(mean_glbcc_cert ~ age + male + white + college + party_lean, data = filter(long.data.agg, wave_count >= 10))
tab_S1b <- lm(glbcc_cert ~ age + male + white + college + party_lean, data = us.cross)
tab_S1c <- lm(mean_glbcc_cert ~ age + male + white + college + ideol, data = filter(long.data.agg, wave_count >= 10))
tab_S1d <- lm(glbcc_cert ~ age + male + white + college + ideol, data = us.cross)
stargazer(tab_S1a, tab_S1b, tab_S1c, tab_S1d, type = "text", single.row = TRUE, digits = 2)

# Model variation in beliefs RMSSD (Supplementary Table 2) ---------------------------------------------------------------
tab_S2a <- lm(rmssd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 10))
tab_S2b <- lm(rmssd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 10))
tab_S2c <- lm(rmssd_glbcc_times_cert ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 10))
stargazer(tab_S2a, tab_S2b, tab_S2c, type = "text", single.row = TRUE, digits = 2)

# Model variation in beliefs prob change (Supplementary Table 3) ---------------------------------------------------------------
tab_S3a <- glm(ifelse(count_glbcc_change == 0, 0, 1) ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc, data = filter(long.data.agg, wave_count >= 10), family = "binomial")
tab_S3b <- glm(ifelse(count_glbcc_change == 0, 0, 1) ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 10), family = "binomial")
tab_S3c <- glm(ifelse(count_glbcc_change == 0, 0, 1) ~ age + male + white + college + blue_dot + mean_variability + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 10), family = "binomial")
stargazer(tab_S3a, tab_S3b, tab_S3c, type = "text", single.row = TRUE, digits = 2)

# Model mean variability (Supplementary Table 4) ---------------------------------------------------------------
tab_S4a <- lm(scale(sd_glbcc_times_cert) ~ age + male + white + college + blue_dot + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 10))
tab_S4b <- lm(scale(raw_mean_variability) ~ age + male + white + college + blue_dot + mean_time_glbcc + party_lean, data = filter(long.data.agg, wave_count >= 10))
tab_S4c <- lm(scale(sd_glbcc_times_cert) ~ age + male + white + college + blue_dot + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 10))
tab_S4d <- lm(scale(raw_mean_variability) ~ age + male + white + college + blue_dot + mean_time_glbcc + ideol, data = filter(long.data.agg, wave_count >= 10))
stargazer(tab_S4a, tab_S4b, tab_S4c, tab_S4d, type = "text", single.row = TRUE, digits = 2)

# Compare variation across survey variability items (Supplementary Table 5) ---------------------------------------------------------------
long.data.agg %>% 
  filter(wave_count >= 10) %>% 
  select(sd_glbcc_cert, sd_cncrn_secur, sd_cncrn_health, sd_cncrn_enrgy, sd_cncrn_trns, sd_cncrn_tax, sd_cncrn_edu, sd_cncrn_econ) %>% 
  psych::describe()

# Write source data from plots ---------------------------------------------------------------
fig_1_source <- bind_rows(
  layer_data(fig_1a, i = 3L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_1b, i = 3L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"))
write_csv(fig_1_source, paste0(source_file, "Fig_1_Source_Data.csv"))

fig_2_source <- layer_data(fig_2, i = 1L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL)
write_csv(fig_2_source, paste0(source_file, "Fig_2_Source_Data.csv"))

fig_3_source <- bind_rows(
  layer_data(fig_3a, i = 1L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_3b, i = 1L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"),
  layer_data(fig_3c, i = 1L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "c"),
  layer_data(fig_3d, i = 1L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "d"))
write_csv(fig_3_source, paste0(source_file, "Fig_3_Source_Data.csv"))

fig_4_source <- bind_rows(
  layer_data(fig_4a, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_4b, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"))
write_csv(fig_4_source, paste0(source_file, "Fig_4_Source_Data.csv"))

fig_5_source <- bind_rows(
  layer_data(fig_5a, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_5b, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"))
write_csv(fig_5_source, paste0(source_file, "Fig_5_Source_Data.csv"))

fig_E1_source <- bind_rows(
  layer_data(fig_E1a, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_E1b, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"))
write_csv(fig_E1_source, paste0(source_file, "Fig_E1_Source_Data.csv"))

fig_E2_source <- layer_data(fig_E2, i = 1L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL)
write_csv(fig_E2_source, paste0(source_file, "Fig_E2_Source_Data.csv"))

fig_E3_source <- bind_rows(
  layer_data(fig_E3a, i = 2L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_E3b, i = 2L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"),
  layer_data(fig_E3c, i = 2L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "c"))
write_csv(fig_E3_source, paste0(source_file, "Fig_E3_Source_Data.csv"))
  
fig_E4_source <- layer_data(fig_E4, i = 1L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL)
write_csv(fig_E4_source, paste0(source_file, "Fig_E4_Source_Data.csv"))

fig_E5_source <- bind_rows(
  layer_data(fig_E5a, i = 1L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_E5b, i = 1L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"),
  layer_data(fig_E5c, i = 1L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "c"),
  layer_data(fig_E5d, i = 1L) %>% select(-c(alpha, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "d"))
write_csv(fig_E5_source, paste0(source_file, "Fig_E5_Source_Data.csv"))

fig_E6_source <- bind_rows(
  layer_data(fig_E6a, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_E6b, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"))
write_csv(fig_E6_source, paste0(source_file, "Fig_E6_Source_Data.csv"))

fig_E7_source <- bind_rows(
  layer_data(fig_E7a, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_E7b, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"))
write_csv(fig_E7_source, paste0(source_file, "Fig_E7_Source_Data.csv"))

fig_E8_source <- bind_rows(
  layer_data(fig_E8a, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_E8b, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"))
write_csv(fig_E8_source, paste0(source_file, "Fig_E8_Source_Data.csv"))

fig_E9_source <- bind_rows(
  layer_data(fig_E9a, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_E9b, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"))
write_csv(fig_E9_source, paste0(source_file, "Fig_E9_Source_Data.csv"))

fig_E10_source <- bind_rows(
  layer_data(fig_E10a, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "a"),
  layer_data(fig_E10b, i = 2L) %>% select(-c(alpha, width, linetype, size, group)) %>% rename(subpanel = PANEL) %>% mutate(panel = "b"))
write_csv(fig_E10_source, paste0(source_file, "Fig_E10_Source_Data.csv"))

# # # Estimate Variance Heterogeneity Models ------------------------------------- 
# # # Note: this is not in the manuscript!
# # fit.data <- long.data %>% drop_na(ideol, party_lean)
# # fit.data$ideol <- factor(fit.data$ideol)
# # fit.data$ideol <- relevel(fit.data$ideol, ref = "1")
# # fit.data$party_lean <- factor(fit.data$party_lean)
# # fit.data$party_lean <- relevel(fit.data$party_lean, ref = "1")
# # 
# # vhm1 <- nlme::lme(fixed = glbcc_times_cert ~ 1,
# #              random = list(p_id = pdSymm(form = ~ 1)),
# #              weights = varIdent(form = ~ 1 | ideol), # allow heterogeneity in within-person variance
# #              data = filter(fit.data, wave_count >= 10),
# #              method = 'REML')
# # summary(vhm1)
# # vhm1_sigmas <- sqrt((c(1.0000000, coef(vhm1$modelStruct$varStruct, unconstrained = FALSE)) * vhm1$sigma)^2) # return sigma values
# # vhm1_sigmas <- tibble(ideol = c(6, 3, 5, 4, 2, 7, 1), sigma = vhm1_sigmas) %>% arrange(ideol)
# # 
# # vhm2 <- lme(fixed = glbcc_times_cert ~ 1,
# #              random = list(p_id = pdSymm(form = ~ 1)),
# #              weights = varIdent(form = ~ 1 | party_lean), # allow heterogeneity in within-person variance
# #              data = fit.data,
# #              method = 'REML')
# # summary(vhm2)
# # vhm2_sigmas <- sqrt((c(1.0000000, coef(vhm2$modelStruct$varStruct, unconstrained = FALSE)) * vhm2$sigma)^2) # return sigma values
# # vhm2_sigmas <- tibble(party_lean = c(4, 1, 2, 3), sigma = vhm2_sigmas) %>% arrange(party_lean)