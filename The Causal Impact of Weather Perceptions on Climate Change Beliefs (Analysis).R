# Libraries ---------------------------------------------------------------
# install.packages("plm", repos = 'http://R-Forge.R-project.org') # make sure to have newest version
library(plm)
library(tidyverse)
library(statar)
library(cowplot)

# Data ---------------------------------------------------------------
data <- data.table::fread("https://www.dropbox.com/s/kltsmrnv1zddlkc/data.csv?dl=1")

base_data <- data %>%
  group_by(p_id) %>%
  slice_min(order_by = wave_num) %>% 
  select(p_id, glbcc_base = glbcc, glbcc_cert_base = glbcc_cert, party_base = party, ideol_base = ideol) %>% 
  data.frame()

data <- data %>% left_join(., base_data, by = "p_id")

data <- data %>% 
  mutate(warm = ifelse(ssn_tmp == 3, 1, 0), 
         cool = ifelse(ssn_tmp == 1, 1, 0),
         dry = ifelse(ssn_precip == 1, 1, 0),
         wet = ifelse(ssn_precip == 3, 1, 0)) %>% 
  mutate(climate_beliefs = ifelse(glbcc == 0, (glbcc_cert * -1), glbcc_cert)) %>% 
  drop_na(warm, cool, dry, wet, climate_beliefs, glbcc_base)

# Describe Data ---------------------------------------------------------------
pdim(data)
data %>% group_by(wave_num, wave_id) %>% count(name = "count")
data %>% distinct(p_id, .keep_all = TRUE) %>% count(n, name = "count") %>% filter(n >= 2) %>% pull(count) %>% sum()
data %>% distinct(p_id, .keep_all = TRUE) %>% count(n, name = "count") %>% filter(n >= 10) %>% pull(count) %>% sum()

# Table 1 ---------------------------------------------------------------
data %>% 
  select(ssn_tmp_15yr_depart, ssn_precip_15yr_depart, warm, cool, dry, wet, climate_beliefs, glbcc_base) %>% 
  drop_na() %>% 
  summarise_all(list(mean = mean, sd = sd)) %>% 
  round(2)

# Figure 1 ---------------------------------------------------------------
a <- ggplot(data, aes(x = ssn_tmp_15yr_depart, y = warm)) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_binmean(col = "blue", alpha = 0.8) +
  theme_classic() +
  labs(x = "Degrees", y = "Proportion") +
  ylim(0, 1)

b <- ggplot(data, aes(x = ssn_tmp_15yr_depart, y = cool)) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_binmean(col = "blue", alpha = 0.8) +
  theme_classic() +
  labs(x = "Degrees", y = "Proportion") +
  ylim(0, 1)

c <- ggplot(data, aes(x = ssn_precip_15yr_depart, y = dry)) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_binmean(col = "blue", alpha = 0.8) +
  theme_classic() +
  labs(x = "Inches", y = "Proportion") +
  ylim(0, 1)

d <- ggplot(data, aes(x = ssn_precip_15yr_depart, y = wet)) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_binmean(col = "blue", alpha = 0.8) +
  theme_classic() +
  labs(x = "Inches", y = "Proportion") +
  ylim(0, 1)

p <- plot_grid(a, b, c, d, labels = c("A", "B", "C", "D"))
# ggsave("~/Dropbox (Univ. of Oklahoma)/EPSCoR/MSISNet Data/Weather Perceptions and Climate Change Beliefs/fig_1.png", p, height = 4, width = 6)

# Table 2 ---------------------------------------------------------------
fit1 <- plm(warm ~ ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data)
fit2 <- plm(cool ~ ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data)
fit3 <- plm(dry ~ ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data)
fit4 <- plm(wet ~ ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data)
fit1_rse <- sqrt(vcovHC(fit1, type = "sss"))
fit2_rse <- sqrt(vcovHC(fit2, type = "sss"))
fit3_rse <- sqrt(vcovHC(fit3, type = "sss"))
fit4_rse <- sqrt(vcovHC(fit4, type = "sss"))
stargazer::stargazer(fit1, fit2, fit3, fit4, type = "text", se = c(fit1_rse, fit2_rse, fit3_rse, fit4_rse))

fit5 <- plm(climate_beliefs ~ warm | ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data)
fit6 <- plm(climate_beliefs ~ cool | ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data)
fit7 <- plm(climate_beliefs ~ dry | ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data)
fit8 <- plm(climate_beliefs ~ wet | ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data)
fit5_rse <- sqrt(vcovHC(fit5, type = "sss"))
fit6_rse <- sqrt(vcovHC(fit6, type = "sss"))
fit7_rse <- sqrt(vcovHC(fit7, type = "sss"))
fit8_rse <- sqrt(vcovHC(fit8, type = "sss"))
stargazer::stargazer(fit5, fit6, fit7, fit8, type = "text", se = c(fit5_rse, fit6_rse, fit7_rse, fit8_rse))
summary(fit5)

# Table 3 ---------------------------------------------------------------
fit9 <- plm(warm ~ ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 0))
fit10 <- plm(cool ~ ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 0))
fit11 <- plm(dry ~ ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 0))
fit12 <- plm(wet ~ ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 0))
fit9_rse <- sqrt(vcovHC(fit9, type = "sss"))
fit10_rse <- sqrt(vcovHC(fit10, type = "sss"))
fit11_rse <- sqrt(vcovHC(fit11, type = "sss"))
fit12_rse <- sqrt(vcovHC(fit12, type = "sss"))
stargazer::stargazer(fit9, fit10, fit11, fit12, type = "text", se = c(fit9_rse, fit10_rse, fit11_rse, fit12_rse))

fit13 <- plm(climate_beliefs ~ warm | ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 0))
fit14 <- plm(climate_beliefs ~ cool | ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 0))
fit15 <- plm(climate_beliefs ~ dry | ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 0))
fit16 <- plm(climate_beliefs ~ wet | ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 0))
fit13_rse <- sqrt(vcovHC(fit13, type = "sss"))
fit14_rse <- sqrt(vcovHC(fit14, type = "sss"))
fit15_rse <- sqrt(vcovHC(fit15, type = "sss"))
fit16_rse <- sqrt(vcovHC(fit16, type = "sss"))
stargazer::stargazer(fit13, fit14, fit15, fit16, type = "text", se = c(fit13_rse, fit14_rse, fit15_rse, fit16_rse))

fit17 <- plm(warm ~ ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 1))
fit18 <- plm(cool ~ ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 1))
fit19 <- plm(dry ~ ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 1))
fit20 <- plm(wet ~ ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 1))
fit17_rse <- sqrt(vcovHC(fit17, type = "sss"))
fit18_rse <- sqrt(vcovHC(fit18, type = "sss"))
fit19_rse <- sqrt(vcovHC(fit19, type = "sss"))
fit20_rse <- sqrt(vcovHC(fit20, type = "sss"))
stargazer::stargazer(fit17, fit18, fit19, fit20, type = "text", se = c(fit17_rse, fit18_rse, fit19_rse, fit20_rse))

fit21 <- plm(climate_beliefs ~ warm | ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 1))
fit22 <- plm(climate_beliefs ~ cool | ssn_tmp_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 1))
fit23 <- plm(climate_beliefs ~ dry | ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 1))
fit24 <- plm(climate_beliefs ~ wet | ssn_precip_15yr_depart, effect = "individual", index = "p_id", model = "within", data = data %>% filter(glbcc_base == 1))
fit21_rse <- sqrt(vcovHC(fit21, type = "sss"))
fit22_rse <- sqrt(vcovHC(fit22, type = "sss"))
fit23_rse <- sqrt(vcovHC(fit23, type = "sss"))
fit24_rse <- sqrt(vcovHC(fit24, type = "sss"))
stargazer::stargazer(fit21, fit22, fit23, fit24, type = "text", se = c(fit21_rse, fit22_rse, fit23_rse, fit24_rse))





