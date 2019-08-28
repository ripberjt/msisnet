# Libraries and Options ---------------------------------------------------------------
library(zeligverse)
library(stargazer)
library(plm)
library(data.table)
library(varian)
library(tidyverse)
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
image_file <- paste0("~/Dropbox/EPSCoR/MSISNet Data/Weather and Climate Change Beliefs/") # update this to save images

# Plot Theme ---------------------------------------------------------------
plot_theme <- theme_bw() + 
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12), 
        plot.title = element_text(size = 20, hjust = 0))

# Data ---------------------------------------------------------------
long.data <- fread(paste0(data_file, "w1_w19_longdata.csv")) # download the data here: http://crcm.ou.edu/epscordata/ - note that wave numbers will change as the survey continues 
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
  'Wave 19 (Winter 2019)'))
long.data$wave_num <- as.numeric(long.data$wave_id)
long.data$p_id <- long.data$userid

long.data <- long.data %>% 
  filter(wave_num %in% 3:18) %>% # exclude start up waves (1 & 2) and most recent wave (19)
  filter(is_statewide == 1) %>% # exclude oversamples in study areas
  drop_na(glbcc, glbcc_cert) %>% # drop if missing climate change beliefs
  group_by(p_id) %>%
  add_count(name = "wave_count") %>% # add wave count variable
  filter(wave_count >= 2) # drop if wave count is less than 2
pdim(long.data) # n = 2115

# Make variables for analysis  ---------------------------------------------------------------
long.data <- long.data %>%
  mutate(glbcc_times_cert = ifelse(glbcc == 0, glbcc_cert * -1, glbcc_cert), # belief x certainty variable
         sd_glbcc_times_cert = sd_id(glbcc_times_cert, p_id), # belief x certainty sd 
         rmssd_glbcc_times_cert = rmssd_id(glbcc_times_cert, p_id), # belief x certainty rmssd
         sd_glbcc = sd_id(glbcc, p_id)) # belief sd

long.data.agg %>%  
  ggplot(., aes(x = sd_glbcc_times_cert, y = rmssd_glbcc_times_cert)) + # compare sd_glbcc_times_cert and rmssd_glbcc_times_cert
  geom_point() + 
  geom_smooth(se = FALSE)

long.data$party_lean <- NA # party lean variable
long.data$party_lean <- ifelse(long.data$party == 1, 1, long.data$party_lean)
long.data$party_lean <- ifelse(long.data$party == 3 & long.data$lean == 1, 2, long.data$party_lean)
long.data$party_lean <- ifelse(long.data$party == 3 & long.data$lean == 2, 3, long.data$party_lean)
long.data$party_lean <- ifelse(long.data$party == 2, 4, long.data$party_lean)

long.data.agg <- long.data %>% 
  group_by(p_id) %>% 
  summarize(wave_count = mean(wave_count),
            sd_glbcc_times_cert = mean(sd_glbcc_times_cert), 
            rmssd_glbcc_times_cert = mean(rmssd_glbcc_times_cert),
            sd_glbcc = mean(sd_glbcc),
            ideol = factor(Mode(ideol, na.rm = TRUE)), 
            party_lean = factor(Mode(party_lean, na.rm = TRUE)),
            age = mean(age, na.rm = TRUE),
            male = factor(ifelse(Mode(gender, na.rm = TRUE) == 1, "M", "F")),
            white = factor(ifelse(Mode(race, na.rm = TRUE) == 1, "W", "NW")),
            college = factor(ifelse(Mode(education, na.rm = TRUE) %in% c("6", "7", "8"), "C", "NC")),
            blue_dot = mean(ifelse(is.na(click_bluedot), 0, click_bluedot))) %>% 
  mutate(age = arm::rescale(age, "full")) %>% # scale by 2 sd
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

long.data.agg %>%
  skimr::skim()

# Plot Aggregate Trends in Beliefs (Fig 1) ---------------------------------------------------------------
glbcc_times_cert_means <- long.data %>%
  filter(wave_count >= 10) %>% 
  group_by(wave_num) %>%
  dplyr::summarise(mean = mean(glbcc_times_cert), 
                   n = n(), 
                   sd = sd(glbcc_times_cert), 
                   se = sd/sqrt(n -1)) %>% 
  mutate(wave_num = factor(wave_num))

fig_1 <- ggplot(data = glbcc_times_cert_means, aes(x = wave_num, y = mean)) +
  geom_line(group = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = mean + 1.96 * se, ymax = mean - 1.96 * se), width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  ylim(-10, 10) +
  scale_x_discrete(labels = 1:16) +
  labs(y = "Believe in Climate Change x Certainty", x = "Survey Wave") +
  plot_theme

ggsave(paste0(image_file, "Figure 1 - Aggregate Trends in Beliefs.png"), fig_1, height = 5, width = 6, dpi = "retina")

# Plot Density of Within-Subject Variation Measures (Fig 2) ---------------------------------------------------------------
sd_group_props <- long.data.agg %>% 
  filter(wave_count >= 10) %>%
  group_by(sd_glbcc_times_cert_group) %>% 
  dplyr::summarise(n = n()) %>% 
  mutate(p = (n/sum(n)) * 100)

fig_2 <- ggplot(filter(long.data.agg, wave_count >= 10), aes(x = sd_glbcc_times_cert)) +
  geom_density(fill = "grey", alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.1), limits = c(0, 0.3)) +
  geom_vline(xintercept = c(1:3), color = "red", linetype = "dashed", size = 0.25) +
  ylab("Density") + xlab("Within-Subject Standard Deviation") +
  plot_theme + 
  annotate("text", x = 0.5, y = 0.3, label = paste0(round(sd_group_props$p[1], 1), "%"), size = 5) +
  annotate("text", x = 1.5, y = 0.3, label = paste0(round(sd_group_props$p[2], 1), "%"), size = 5) +
  annotate("text", x = 2.5, y = 0.3, label = paste0(round(sd_group_props$p[3], 1), "%"), size = 5) +
  annotate("text", x = 5.5, y = 0.3, label = paste0(round(sd_group_props$p[4], 1), "%"), size = 5)
ggsave(paste0(image_file, "Figure 2 - SD Density Plot.png"), fig_2, height = 5, width = 12, dpi = "retina")

# Plot Examples from Each Within-Subject Variation Group (Fig 3) ---------------------------------------------------------------
G1.data <- subset(long.data.agg, wave_count >= 10 & sd_glbcc_times_cert_group == "G1")
G2.data <- subset(long.data.agg, wave_count >= 10 & sd_glbcc_times_cert_group == "G2")
G3.data <- subset(long.data.agg, wave_count >= 10 & sd_glbcc_times_cert_group == "G3")
G4.data <- subset(long.data.agg, wave_count >= 10 & sd_glbcc_times_cert_group == "G4")

set.seed(1)
SG1.data <- subset(long.data, p_id %in% sample(unique(G1.data$p_id), 25, replace = FALSE))
SG2.data <- subset(long.data, p_id %in% sample(unique(G2.data$p_id), 25, replace = FALSE))
SG3.data <- subset(long.data, p_id %in% sample(unique(G3.data$p_id), 25, replace = FALSE))
SG4.data <- subset(long.data, p_id %in% sample(unique(G4.data$p_id), 25, replace = FALSE))

G1 <- ggplot(data = SG1.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  ylab("Believe in Climate Change x Certainty") + xlab("Survey Wave") + 
  ggtitle(expression(paste("(a) Stable Beliefs (", sigma, " < 1)"))) +
  facet_wrap(~ p_id) +
  plot_theme

G2 <- ggplot(data = SG2.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  ylab("Believe in Climate Change x Certainty") + xlab("Survey Wave") + 
  ggtitle(expression(paste("(b) Somewhat Stable Beliefs (", "1 < ", sigma, " < 2)"))) +
  facet_wrap(~ p_id) + 
  plot_theme

G3 <- ggplot(data = SG3.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  ylab("Believe in Climate Change x Certainty") + xlab("Survey Wave") + 
  ggtitle(expression(paste("(c) Somewhat Unstable Beliefs (", "2 < ", sigma, " < 3)"))) +
  facet_wrap(~ p_id) + 
  plot_theme

G4 <- ggplot(data = SG4.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  ylab("Believe in Climate Change x Certainty") + xlab("Survey Wave") + 
  ggtitle(expression(paste("(d) Unstable Beliefs (", sigma, " > 3)"))) +
  facet_wrap(~ p_id) + 
  plot_theme

fig_3 <- gridExtra::arrangeGrob(G1, G2, G3, G4, ncol = 2, nrow = 2)
ggsave(paste0(image_file, "Figure 3 - Example Tracks by SD Group.png"), fig_3, height = 12, width = 16, dpi = "retina")

# Note Proportions that Change Beliefs by Within-Subject Variation Group Group ---------------------------------------------------------------
long.data.agg %>% 
  filter(wave_count >= 10) %>% 
  mutate(change = ifelse(sd_glbcc == 0, 0, 1)) %>% 
  group_by(sd_glbcc_times_cert_group, change) %>% 
  summarise(n = n()) %>% 
  mutate(p = n/sum(n))

long.data.agg %>% 
  filter(wave_count >= 10) %>% 
  mutate(change = ifelse(sd_glbcc == 0, 0, 1)) %>% 
  group_by(rmssd_glbcc_times_cert_group, change) %>% 
  summarise(n = n()) %>% 
  mutate(p = n/sum(n))

# Model Variation in Beliefs (Table 1) ---------------------------------------------------------------
fit1 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 10))
fit2 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 10))
fit3 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 10))
stargazer(fit1, fit2, fit3, type = "text", single.row = TRUE, digits = 2)

# Plot Ideology/Party Predictions from Models (Fig 4) ---------------------------------------------------------------
fit2z <- zelig(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 10), model = "ls")
fit3z <- zelig(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 10), model = "ls")

fig_4a <- setx(fit3z, ideol = levels(long.data.agg$ideol)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = ideol, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 4) +
  ggtitle("(a) Political Ideology") +
  labs(x = "", y = "Within-Subject Standard Deviation") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Strongly\nliberal", 
                              "2" = "Liberal", 
                              "3" = "Slightly\nliberal",
                              "4" = "Middle",
                              "5" = "Slightly\nconservative",
                              "6" = "Conservative",
                              "7" = "Strongly\nconservative")) +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"), 
        axis.text.x = element_text(size = 9)) 

fig_4b <- setx(fit2z, party_lean = levels(long.data.agg$party_lean)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = party_lean, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 4) +
  ggtitle("(b) Political Party") +
  labs(x = "", y = "Within-Subject Standard Deviation") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Democrat", 
                              "2" = "Independent\nlean Democrat", 
                              "3" = "Independent\nlean Republican",
                              "4" = "Republican")) +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        axis.text.x = element_text(size = 9)) 
  
fig_4 <- gridExtra::grid.arrange(fig_4a, fig_4b, ncol = 2, nrow = 1)
ggsave(paste0(image_file, "Figure 4 - SD Political Predictions.png"), fig_4, height = 5, width = 12, dpi = "retina")

# Note Proportions that Change Beliefs by Ideology/Party ---------------------------------------------------------------
long.data.agg$ideol_part <- NA
long.data.agg$ideol_part <- ifelse(long.data.agg$ideol %in% c("1", "2", "3") & long.data.agg$party_lean == "1", "LibDem", long.data.agg$ideol_part)
long.data.agg$ideol_part <- ifelse(long.data.agg$ideol %in% c("5", "6", "7") & long.data.agg$party_lean == "4", "ConRep", long.data.agg$ideol_part)
long.data.agg %>% 
  filter(wave_count >= 10) %>% 
  mutate(change = ifelse(sd_glbcc == 0, 0, 1)) %>% 
  group_by(ideol_part, change) %>% 
  summarise(n = n()) %>% 
  mutate(p = n/sum(n))

# Plot Density of Within-Subject Variation Measures by Wave Count (Supplementary Fig 1) ---------------------------------------------------------------
fig_S1 <- bind_rows(
  long.data %>% filter(wave_count >= 2) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "2+"),
  long.data %>% filter(wave_count >= 3) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "3+"), 
  long.data %>% filter(wave_count >= 4) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "4+"), 
  long.data %>% filter(wave_count >= 5) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "5+"), 
  long.data %>% filter(wave_count >= 6) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "6+"), 
  long.data %>% filter(wave_count >= 7) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "7+"), 
  long.data %>%  filter(wave_count >= 8) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "8+"), 
  long.data %>%  filter(wave_count >= 9) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "9+"), 
  long.data %>%  filter(wave_count >= 10) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "10+"), 
  long.data %>%  filter(wave_count >= 11) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "11+"), 
  long.data %>%  filter(wave_count >= 12) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "12+"), 
  long.data %>%  filter(wave_count >= 13) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "13+"), 
  long.data %>%  filter(wave_count >= 14) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "14+"), 
  long.data %>%  filter(wave_count >= 15) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "15+"), 
  long.data %>%  filter(wave_count >= 16) %>% select(sd_glbcc_times_cert) %>% mutate(`Wave Count` = "16")) %>% 
  mutate(`Wave Count` = factor(`Wave Count`, levels = c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+",
                                                        "11+", "12+", "13+", "14+", "15+", "16"))) %>% 
  ggplot(aes(x = sd_glbcc_times_cert, color = `Wave Count`)) +
  geom_density() +
  ylab("Density") + xlab("Within-Subject Standard Deviation") +
  plot_theme + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10))
ggsave(paste0(image_file, "Figure S1 - SD Density Plot by Wave Count.png"), fig_S1, height = 5, width = 12, dpi = "retina")

# Model Variation in Beliefs by Wave Count (Supplementary Fig 2) ---------------------------------------------------------------
fit1.2 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 2))
fit1.3 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 3))
fit1.4 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 4))
fit1.5 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 5))
fit1.6 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 6))
fit1.7 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 7))
fit1.8 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 8))
fit1.9 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 9))
fit1.10 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 10))
fit1.11 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 11))
fit1.12 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 12))
fit1.13 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 13))
fit1.14 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 14))
fit1.15 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 15))
fit1.16 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 16))
stargazer(fit1.2, fit1.3, fit1.4, fit1.5, fit1.6, fit1.7, fit1.8, fit1.9, fit1.10, fit1.11, fit1.12, fit1.13, fit1.14, fit1.15, fit1.16,
          type = "text", single.row = TRUE, digits = 2)

estimates <- bind_rows(fit1.2$coefficients, fit1.3$coefficients, fit1.4$coefficients, fit1.5$coefficients, fit1.6$coefficients, fit1.7$coefficients,
                       fit1.8$coefficients, fit1.9$coefficients, fit1.10$coefficients, fit1.11$coefficients, fit1.12$coefficients, fit1.13$coefficients,
                       fit1.14$coefficients, fit1.15$coefficients, fit1.16$coefficients)
estimates$`Wave Count` <- c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", "13+", "14+", "15+", "16")
estimates$`Wave Count` <- factor(estimates$`Wave Count`, levels = c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", 
                                                                    "13+", "14+", "15+", "16")) 
estimates <- estimates %>% gather(term, est, `(Intercept)`:blue_dot) 
errors <- bind_rows(sqrt(diag(vcov(fit1.2))), sqrt(diag(vcov(fit1.3))), sqrt(diag(vcov(fit1.4))), sqrt(diag(vcov(fit1.5))), sqrt(diag(vcov(fit1.6))), 
                    sqrt(diag(vcov(fit1.7))), sqrt(diag(vcov(fit1.8))), sqrt(diag(vcov(fit1.9))), sqrt(diag(vcov(fit1.10))), sqrt(diag(vcov(fit1.11))),
                    sqrt(diag(vcov(fit1.12))), sqrt(diag(vcov(fit1.13))), sqrt(diag(vcov(fit1.14))), sqrt(diag(vcov(fit1.15))), sqrt(diag(vcov(fit1.16))))
errors <- errors %>% gather(term, est, `(Intercept)`:blue_dot) 
estimates$se <- errors$est
estimates$term <- factor(estimates$term, levels = c("(Intercept)", "age", "maleM", "whiteW", "collegeC", "blue_dot"), 
                         labels = c("Constant", "Age", "Male", "White", "College", "Survey\nAttention"))

fig_S2a <- estimates %>% 
  ggplot(., aes(y = est, x = term, group = `Wave Count`, color = `Wave Count`)) +
  geom_point(position = position_dodge(width = 0.7), size = 2) +
  geom_linerange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), position = position_dodge(width = 0.7), size = 1.5) +
  labs(x = "Term", y = "Coefficient Estimate", title = "(a)") +
  ylim(-1, 4) +
  plot_theme + 
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        legend.justification = c(1, 0.8), 
        legend.position = c(1, 0.92),
        legend.background = element_rect(fill = "transparent"))

fit2.2 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 2))
fit2.3 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 3))
fit2.4 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 4))
fit2.5 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 5))
fit2.6 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 6))
fit2.7 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 7))
fit2.8 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 8))
fit2.9 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 9))
fit2.10 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 10))
fit2.11 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 11))
fit2.12 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 12))
fit2.13 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 13))
fit2.14 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 14))
fit2.15 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 15))
fit2.16 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 16))
stargazer(fit2.2, fit2.3, fit2.4, fit2.5, fit2.6, fit2.7, fit2.8, fit2.9, fit2.10, fit2.11, fit2.12, fit2.13, fit2.14, fit2.15, fit2.16,
          type = "text", single.row = TRUE, digits = 2)

estimates <- bind_rows(fit2.2$coefficients, fit2.3$coefficients, fit2.4$coefficients, fit2.5$coefficients, fit2.6$coefficients, fit2.7$coefficients,
                       fit2.8$coefficients, fit2.9$coefficients, fit2.10$coefficients, fit2.11$coefficients, fit2.12$coefficients, fit2.13$coefficients,
                       fit2.14$coefficients, fit2.15$coefficients, fit2.16$coefficients)
estimates$`Wave Count` <- c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", "13+", "14+", "15+", "16")
estimates$`Wave Count` <- factor(estimates$`Wave Count`, levels = c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", 
                                                                    "13+", "14+", "15+", "16")) 
estimates <- estimates %>% gather(term, est, `(Intercept)`:party_lean4) 

errors <- bind_rows(sqrt(diag(vcov(fit2.2))), sqrt(diag(vcov(fit2.3))), sqrt(diag(vcov(fit2.4))), sqrt(diag(vcov(fit2.5))), sqrt(diag(vcov(fit2.6))), 
                    sqrt(diag(vcov(fit2.7))), sqrt(diag(vcov(fit2.8))), sqrt(diag(vcov(fit2.9))), sqrt(diag(vcov(fit2.10))), sqrt(diag(vcov(fit2.11))),
                    sqrt(diag(vcov(fit2.12))), sqrt(diag(vcov(fit2.13))), sqrt(diag(vcov(fit2.14))), sqrt(diag(vcov(fit2.15))), sqrt(diag(vcov(fit2.16))))
errors <- errors %>% gather(term, est, `(Intercept)`:party_lean4) 
estimates$se <- errors$est
estimates$term <- factor(estimates$term, levels = c("(Intercept)", "age", "maleM", "whiteW", "collegeC", "blue_dot", "party_lean2", "party_lean3", "party_lean4"),
                         labels = c("Constant", "Age", "Male", "White", "College", "Survey\nAttention", "Lean\nDem", "Lean\nRep", "Rep"))

fig_S2b <- estimates %>% 
  ggplot(., aes(y = est, x = term, group = `Wave Count`, color = `Wave Count`)) +
  geom_point(position = position_dodge(width = 0.7), size = 2) +
  geom_linerange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), position = position_dodge(width = 0.7), size = 1.5) +
  labs(x = "Term", y = "Coefficient Estimate", title = "(b)") +
  ylim(-1, 4) +
  plot_theme + 
  guides(color = FALSE)

fit3.2 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 2))
fit3.3 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 3))
fit3.4 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 4))
fit3.5 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 5))
fit3.6 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 6))
fit3.7 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 7))
fit3.8 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 8))
fit3.9 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 9))
fit3.10 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 10))
fit3.11 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 11))
fit3.12 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 12))
fit3.13 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 13))
fit3.14 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 14))
fit3.15 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 15))
fit3.16 <- lm(sd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 16))
stargazer(fit3.2, fit3.3, fit3.4, fit3.5, fit3.6, fit3.7, fit3.8, fit3.9, fit3.10, fit3.11, fit3.12, fit3.13, fit3.14, fit3.15, fit3.16,
          type = "text", single.row = TRUE, digits = 2)

estimates <- bind_rows(fit3.2$coefficients, fit3.3$coefficients, fit3.4$coefficients, fit3.5$coefficients, fit3.6$coefficients, fit3.7$coefficients,
                       fit3.8$coefficients, fit3.9$coefficients, fit3.10$coefficients, fit3.11$coefficients, fit3.12$coefficients, fit3.13$coefficients,
                       fit3.14$coefficients, fit3.15$coefficients, fit3.16$coefficients)
estimates$`Wave Count` <- c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", "13+", "14+", "15+", "16")
estimates$`Wave Count` <- factor(estimates$`Wave Count`, levels = c("2+", "3+", "4+", "5+", "6+", "7+", "8+", "9+", "10+", "11+", "12+", 
                                                                    "13+", "14+", "15+", "16")) 
estimates <- estimates %>% gather(term, est, `(Intercept)`:ideol7) 

errors <- bind_rows(sqrt(diag(vcov(fit3.2))), sqrt(diag(vcov(fit3.3))), sqrt(diag(vcov(fit3.4))), sqrt(diag(vcov(fit3.5))), sqrt(diag(vcov(fit3.6))), 
                    sqrt(diag(vcov(fit3.7))), sqrt(diag(vcov(fit3.8))), sqrt(diag(vcov(fit3.9))), sqrt(diag(vcov(fit3.10))), sqrt(diag(vcov(fit3.11))),
                    sqrt(diag(vcov(fit3.12))), sqrt(diag(vcov(fit3.13))), sqrt(diag(vcov(fit3.14))), sqrt(diag(vcov(fit3.15))), sqrt(diag(vcov(fit3.16))))
errors <- errors %>% gather(term, est, `(Intercept)`:ideol7) 
estimates$se <- errors$est
estimates$term <- factor(estimates$term, levels = c("(Intercept)", "age", "maleM", "whiteW", "collegeC", "blue_dot", "ideol2", "ideol3", "ideol4", "ideol5", "ideol6", "ideol7"),
                         labels = c("Constant", "Age", "Male", "White", "College", "Survey\nAttention", "Lib", "Slight\nLib", "Middle", "Slight\nCon", "Con", "Strong\nCon"))

fig_S2c <- estimates %>% 
  ggplot(., aes(y = est, x = term, group = `Wave Count`, color = `Wave Count`)) +
  geom_point(position = position_dodge(width = 0.7), size = 2) +
  geom_linerange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), position = position_dodge(width = 0.7), size = 1.5) +
  labs(x = "Term", y = "Coefficient Estimate", title = "(c)") +
  ylim(-1, 4) +
  plot_theme + 
  guides(color = FALSE)

fig_S2 <- gridExtra::grid.arrange(fig_S2a, fig_S2b, fig_S2c, ncol = 1, nrow = 3)
ggsave(paste0(image_file, "Figure S2 - Regression Estimates by Wave Count.png"), fig_S2, height = 12, width = 24, dpi = "retina")

# Plot Density of RMSSD Measures by Wave Count (Supplementary Fig 3) ---------------------------------------------------------------
rmssd_group_props <- long.data.agg %>% 
  filter(wave_count >= 10) %>%
  group_by(rmssd_glbcc_times_cert_group) %>% 
  dplyr::summarise(n = n()) %>% 
  mutate(p = (n/sum(n)) * 100)

fig_S3 <- ggplot(filter(long.data.agg, wave_count >= 10), aes(x = rmssd_glbcc_times_cert)) +
  geom_density(fill = "grey", alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 13, 1), limits = c(0, 13)) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.1), limits = c(0, 0.3)) +
  geom_vline(xintercept = c(1:3), color = "red", linetype = "dashed", size = 0.25) +
  ylab("Density") + xlab("Within-Subject Root Mean Square of Successive Differences") +
  plot_theme + 
  annotate("text", x = 0.5, y = 0.3, label = paste0(round(rmssd_group_props$p[1], 1), "%"), size = 5) +
  annotate("text", x = 1.5, y = 0.3, label = paste0(round(rmssd_group_props$p[2], 1), "%"), size = 5) +
  annotate("text", x = 2.5, y = 0.3, label = paste0(round(rmssd_group_props$p[3], 1), "%"), size = 5) +
  annotate("text", x = 5.5, y = 0.3, label = paste0(round(rmssd_group_props$p[4], 1), "%"), size = 5)
ggsave(paste0(image_file, "Figure S3 - RMSSD Density Plot.png"), fig_S3, height = 5, width = 12, dpi = "retina")

# Plot Examples from Each Within-Subject RMSSD Group (Supplementary Fig 4) ---------------------------------------------------------------
G1S.data <- subset(long.data.agg, wave_count >= 10 & rmssd_glbcc_times_cert_group == "G1")
G2S.data <- subset(long.data.agg, wave_count >= 10 & rmssd_glbcc_times_cert_group == "G2")
G3S.data <- subset(long.data.agg, wave_count >= 10 & rmssd_glbcc_times_cert_group == "G3")
G4S.data <- subset(long.data.agg, wave_count >= 10 & rmssd_glbcc_times_cert_group == "G4")

set.seed(1)
SG1S.data <- subset(long.data, p_id %in% sample(unique(G1S.data$p_id), 25, replace = FALSE))
SG2S.data <- subset(long.data, p_id %in% sample(unique(G2S.data$p_id), 25, replace = FALSE))
SG3S.data <- subset(long.data, p_id %in% sample(unique(G3S.data$p_id), 25, replace = FALSE))
SG4S.data <- subset(long.data, p_id %in% sample(unique(G4S.data$p_id), 25, replace = FALSE))

G1S <- ggplot(data = SG1S.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  ylab("Believe in Climate Change x Certainty") + xlab("Survey Wave") + 
  ggtitle(expression(paste("(a) Stable Beliefs (", RMSSD, " < 1)"))) +
  facet_wrap(~ p_id) +
  plot_theme

G2S <- ggplot(data = SG2S.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  ylab("Believe in Climate Change x Certainty") + xlab("Survey Wave") + 
  ggtitle(expression(paste("(b) Somewhat Stable Beliefs (", "1 < ", RMSSD, " < 2)"))) +
  facet_wrap(~ p_id) + 
  plot_theme

G3S <- ggplot(data = SG3S.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  ylab("Believe in Climate Change x Certainty") + xlab("Survey Wave") + 
  ggtitle(expression(paste("(c) Somewhat Unstable Beliefs (", "2 < ", RMSSD, " < 3)"))) +
  facet_wrap(~ p_id) + 
  plot_theme

G4S <- ggplot(data = SG4S.data, aes(x = wave_num, y = glbcc_times_cert)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.25) +
  ylab("Believe in Climate Change x Certainty") + xlab("Survey Wave") + 
  ggtitle(expression(paste("(d) Unstable Beliefs (", RMSSD, " > 3)"))) +
  facet_wrap(~ p_id) + 
  plot_theme

fig_S3 <- gridExtra::arrangeGrob(G1S, G2S, G3S, G4S, ncol = 2, nrow = 2)
ggsave(paste0(image_file, "Figure S4 - Example Tracks by RMSSD Groups.png"), fig_S3, height = 12, width = 16, dpi = "retina")

# Model Variation in Beliefs RMSSD (Table S1) ---------------------------------------------------------------
fitS1 <- lm(rmssd_glbcc_times_cert ~ age + male + white + college + blue_dot, data = filter(long.data.agg, wave_count >= 10))
fitS2 <- lm(rmssd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 10))
fitS3 <- lm(rmssd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 10))
stargazer(fitS1, fitS2, fitS3, type = "text", single.row = TRUE, digits = 2)

# Plot Ideology/Party Predictions from RMSSD Models (Supplementary Fig 5) ---------------------------------------------------------------
fitS2z <- zelig(rmssd_glbcc_times_cert ~ age + male + white + college + blue_dot + party_lean, data = filter(long.data.agg, wave_count >= 10), model = "ls")
fitS3z <- zelig(rmssd_glbcc_times_cert ~ age + male + white + college + blue_dot + ideol, data = filter(long.data.agg, wave_count >= 10), model = "ls")

fig_S5a <- setx(fitS3z, ideol = levels(long.data.agg$ideol)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = ideol, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 4.5) +
  ggtitle("(a) Political Ideology") +
  labs(x = "", y = "Within-Subject RMSSD") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Strongly\nliberal", 
                              "2" = "Liberal", 
                              "3" = "Slightly\nliberal",
                              "4" = "Middle",
                              "5" = "Slightly\nconservative",
                              "6" = "Conservative",
                              "7" = "Strongly\nconservative")) +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"), 
        axis.text.x = element_text(size = 9)) 

fig_S5b <- setx(fitS2z, party_lean = levels(long.data.agg$party_lean)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer() %>% 
  ggplot(., aes(x = party_lean, y = qi_ci_median)) +
  geom_point() +
  ylim(0, 4.5) +
  ggtitle("(b) Political Party") +
  labs(x = "", y = "Within-Subject RMSSD") +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0.2) +
  plot_theme +
  scale_x_discrete(labels = c("1" = "Democrat", 
                              "2" = "Independent\nlean Democrat", 
                              "3" = "Independent\nlean Republican",
                              "4" = "Republican")) +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        axis.text.x = element_text(size = 9))

fig_S5 <- gridExtra::grid.arrange(fig_S5a, fig_S5b, ncol = 2, nrow = 1)
ggsave(paste0(image_file, "Figure S5 - RMSSD Political Predictions.png"), fig_S5, height = 5, width = 12, dpi = "retina")

# Estimate Variance Heterogeneity Models ------------------------------------- 
# Note: this is not in the manuscript!
fit.data <- long.data %>% drop_na(ideol, party_lean)
fit.data$ideol <- factor(fit.data$ideol)
fit.data$ideol <- relevel(fit.data$ideol, ref = "1")
fit.data$party_lean <- factor(fit.data$party_lean)
fit.data$party_lean <- relevel(fit.data$party_lean, ref = "1")

vhm1 <- nlme::lme(fixed = glbcc_times_cert ~ 1,
             random = list(p_id = pdSymm(form = ~ 1)),
             weights = varIdent(form = ~ 1 | ideol), # allow heterogeneity in within-person variance
             data = filter(fit.data, wave_count >= 10),
             method = 'REML')
summary(vhm1)
vhm1_sigmas <- sqrt((c(1.0000000, coef(vhm1$modelStruct$varStruct, unconstrained = FALSE)) * vhm1$sigma)^2) # return sigma values
vhm1_sigmas <- tibble(ideol = c(6, 3, 5, 4, 2, 7, 1), sigma = vhm1_sigmas) %>% arrange(ideol)

vhm2 <- lme(fixed = glbcc_times_cert ~ 1,
             random = list(p_id = pdSymm(form = ~ 1)),
             weights = varIdent(form = ~ 1 | party_lean), # allow heterogeneity in within-person variance
             data = fit.data,
             method = 'REML')
summary(vhm2)
vhm2_sigmas <- sqrt((c(1.0000000, coef(vhm2$modelStruct$varStruct, unconstrained = FALSE)) * vhm2$sigma)^2) # return sigma values
vhm2_sigmas <- tibble(party_lean = c(4, 1, 2, 3), sigma = vhm2_sigmas) %>% arrange(party_lean)