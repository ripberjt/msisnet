#Sean Ernst
#Fall 2018
#Aeroecoloy M-SISnet Survey Data Analysis

#Import packages =========================================================================================

library(ggplot2)
library(dplyr)
library(tidyverse)
library(psych)
library(gridExtra)
library(stargazer)
library(broom)
library(lavaan)
library(DiagrammeR)

cbcol <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#990000", "#000000")
cbcol2 <- c("#999999", "#E69F00", "#F0E442", "#0072B2", "#56B4E9", "#CC79A7", "#009E73", "#D55E00", "#990000", "#000000")

#Open proper directory ===================================================================================
dird="D:\\Documents\\Work Documents\\Aeroecology Project\\Data and Analysis"
setwd(dird)
getwd()
MSISnetraw.data=read.csv("w1_w14_longdata.csv")

msisnet13.data <- MSISnetraw.data %>% 
  select(userid, wave_id, rule:F_rate) %>%
  filter(wave_id == "Wave 13 (Winter 2017)")
  
  
data <- MSISnetraw.data %>%
  select(userid, wave_id, gender, income, age, eco_frag:eco_cncrn, cncrn_natres, glbcc:glbwrm_ok, confirm_county) %>%
  filter(wave_id == "Wave 14 (Spring 2017)")
  data <- merge(data, msisnet13.data, by = "userid", all.x = TRUE)

data <- data %>%
  mutate(log.inc = log(income)) %>%
  mutate(eco_frag = if_else(eco_frag == 1, 4,
                            if_else(eco_frag == 2, 3,
                                    if_else(eco_frag == 3, 2, 1))))
  
#Data Exploration =======================================================================================
    
#proportion of answers for eco_frag
FragData <- data %>% 
  group_by(eco_frag) %>%
  tally() %>% 
  mutate(p = n / sum(n)) %>% 
  drop_na()
pfrag = ggplot(FragData, aes(x = eco_frag, y = p, fill = eco_frag)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "dark green", high = "light green") +
  xlab("Percieved Fragility of Wildlife") +
  ylab("Proportion of Responses") +
  scale_x_continuous(breaks = c(1:4), labels = c("Extremely\nRobust","Relatively\nRobust","Relatively\nFragile","Extremely\nFragile")) + theme_bw() +
  theme(legend.position = "none") #+ ggsave("E_frag.png")

#proportion of answers for eco_wildlife
EcoWData <- data %>% 
  group_by(eco_wildlife) %>%
  tally() %>% 
  mutate(p = n / sum(n)) %>% 
  drop_na()
pEcoW = ggplot(EcoWData, aes(x = factor(eco_wildlife), y = p, fill = factor(eco_wildlife))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = c(1:8), labels = c("Amphibians","Birds","Fish","Mammals","Plants","Reptiles","Invertebrates","Other")) +
  scale_fill_manual(values = cbcol, breaks = c(1:8)) +
  xlab("Response to most vulnerable wildlife") +
  ylab("Proportion of responses") + theme_bw() +
  theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.2)) #+ ggsave("E_wild.png")

#proportion of answers for eco_extwthr
ExtrData <- data %>% 
  group_by(eco_extwthr) %>% 
  tally() %>% 
  mutate(p = n / sum(n)) %>% 
  drop_na()
pExtr = ggplot(ExtrData, aes(x = factor(eco_extwthr), y = p, fill=factor(eco_extwthr))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = c(1:10), labels = c("High\nWinds","Drought","Lightning\nStorms","Cold","Snowstorms","Ice\nStorms","Flash\nFloods","Tornadoes","Wildfires","Heat")) +
  scale_fill_manual(values = cbcol2, breaks = c(1:10)) +
  xlab("Response for Most Impactful Weather") +
  ylab("Proportion of Responses") + theme_bw() +
  theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.3)) #+ ggsave("E_extr.png")

#proportion of answers for eco_see
EseeData <- data %>% 
  group_by(eco_see) %>% 
  tally() %>% 
  mutate(p = n / sum(n)) %>% 
  drop_na()
pEsee = ggplot(EseeData, aes(x = factor(eco_see), y = p, fill = factor(eco_see))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = c(0:2), labels = c("No","Yes","Don't Know")) +  
  scale_fill_manual(values = c("#999999", "#009E73", "#56B4E9"), breaks = c(0:2)) +
  xlab("Did you see a weather event impact wildlife?") +
  ylab("Proportion of Responses") + theme_bw() +
  theme(legend.position = "none") #+ ggsave("E_see.png")

#proportion of answers for eco_cncrn
CncrnData <- data %>% 
  group_by(eco_cncrn) %>% 
  tally() %>% 
  mutate(p = n / sum(n)) %>% 
  drop_na()
pCncrn = ggplot(CncrnData, aes(x = eco_cncrn, y = p, fill = eco_cncrn)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(0:10), labels = c("Not at all\nConcerned","1","2","3","4","5","6","7","8", "9", "Extremely\nConcerned")) +
  scale_fill_gradient(low = "light green", high = "dark green") +
  xlab("Response for Level of Concern (0 to 10)") +
  ylab("Proportion of Responses") + theme_bw() +
  theme(legend.position = "none")  #+ ggsave("E_cncrn.png")
CncrnData %>%
  filter(eco_cncrn < 5) %>%
  summarize(sum(p))
CncrnData %>%
  filter(eco_cncrn > 5) %>%
  summarize(sum(p))

#set New Ecological Paradigm questions to same 1-5 scale (5 NEP, 1 DSP)
nep <- data %>% 
  select(rule:cntrl) %>% 
  mutate(rule = rule * -1 + 6, exagrt = exagrt * -1 + 6, blnc = blnc * -1 + 6, cntrl = cntrl * -1 + 6) %>% 
  # alpha() %>% 
  rowMeans(na.rm = TRUE)
#Density plot of NEP scores
data$nep <- nep
pNEP = ggplot(data, aes(x = nep)) +
  geom_density(color = "dark green", fill = "dark green", alpha = 0.1, size = 1) +
  geom_vline(aes(xintercept = mean(na.omit(nep))), color = "black", linetype = "dashed", size = 2) +
  xlab("Mean Individual Score\n(1 low NEP, 5 high NEP)") +
  ylab("Proportion of Scores") + theme_bw() +
  theme(legend.position = "none") #+ ggsave("NEP_dens.png")

#proportion of answers for Fatalist rating
FData <- data %>% 
  group_by(F_rate) %>% 
  tally() %>% 
  mutate(p = n / sum(n)) %>% 
  drop_na()
pF = ggplot(FData, aes(x = factor(F_rate), y = p, fill = F_rate)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "light grey", high = "black") +
  xlab("Fatalism Score (0 low, 10 high)") +
  ylab("Proportion of Responses")  + theme_bw() +
  theme(legend.position = "none") #+ ggsave("F_rate.png")
  
#proportion of answers for Hierarchy rating
Hdata <- data %>% 
  group_by(H_rate) %>% 
  tally() %>% 
  mutate(p = n / sum(n)) %>% 
  drop_na()
pH = ggplot(Hdata, aes(x = factor(H_rate), y = p, fill = H_rate)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  xlab("Hierarchy Score (0 low, 10 high)") +
  ylab("Proportion of Responses")  + theme_bw() +
  theme(legend.position = "none") #+ ggsave("H_rate.png")

#proportion of answers for Egalitarian rating
EData <- data %>% 
  group_by(E_rate) %>% 
  tally() %>% 
  mutate(p = n / sum(n)) %>% 
  drop_na()
pE = ggplot(EData, aes(x = factor(E_rate), y = p, fill = E_rate)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "light green", high = "dark green") +
  xlab("Egalitarianism Score (0 low, 10 high)") +
  ylab("Proportion of Responses") + theme_bw() +
  theme(legend.position = "none") #+ ggsave("E_rate.png")

#proportion of answers for Individualist rating
IData <- data %>% 
  group_by(I_rate) %>% 
  tally() %>% 
  mutate(p = n / sum(n)) %>% 
  drop_na()
pI = ggplot(IData, aes(x = factor(I_rate), y = p, fill = I_rate)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "violet", high = "purple") +
  xlab("Individualism Score (0 low, 10 high)") +
  ylab("Proportion of Responses") + theme_bw() +
  theme(legend.position = "none") #+ ggsave("I_rate.png")



#NEP vs Cultural theory - looking for covariance
data %>% 
  select(nep, H_rate:F_rate) %>% 
  cor(., use = "pairwise.complete.obs")

#Scatterplot for Wildlife Fragility vs Wildlife Concern
Frag_Cncrn = ggplot(data, aes(x = eco_frag, y = eco_cncrn)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10), labels = c("Not at all\nConcerned","2","4","6","8", "Extremely\nConcerned")) +
  scale_x_continuous(breaks = c(1:4), labels = c("Extremely\nRobust","Relatively\nRobust","Relatively\nFragile","Extremely\nFragile"), limits = c(0.75,4.25)) +
  xlab("Fragility of Wildlife") +
  ylab("Concern for Wildlife") + theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "none")

#Bar plot of Wildlife Concern for Wildlife Concern = Relatively Robust
ef3 <- data %>%
  group_by(eco_frag) %>%
  filter(eco_frag == 3) %>%
  count(eco_cncrn) %>% 
  mutate(p = n / sum(n)) %>% 
  drop_na()
Frag_Cncrnbar = ef3_cncrn = ggplot(ef3, aes(x = eco_cncrn, y = p, fill = eco_cncrn)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(0:10), labels = c("Not at all\nConcerned","1","2","3","4","5","6","7","8", "9", "Extremely\nConcerned")) +
  scale_fill_gradient(low = "light green", high = "dark green") +
  xlab("Response for Level of Concern (0 to 10)") +
  ylab("Proportion of Responses") +
  theme(legend.position = "none")
ef3 %>%
  filter(eco_cncrn < 5) %>%
  summarize(sum(p))
ef3 %>%
  filter(eco_cncrn > 5) %>%
  summarize(sum(p))

#Scatterplot with LM for nep vs eco_cncrn
NEPvCncrn = ggplot(data, aes(x = nep, y = eco_cncrn)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,10), breaks = c(0:10), labels = c("Not at all\nConcerned","1","2","3","4","5","6","7","8", "9", "Extremely\nConcerned")) +
  xlab("NEP Score\n(1 low NEP, 5 high NEP)") +
  ylab("Concern for Wildlife") +
  theme(panel.grid.minor = element_blank(), legend.position = "none") #+ ggsave("NEP_v_cncrn.png")

#Scatterplot with LM for Egalitarianism vs eco_cncrn
EvCncrn = ggplot(data, aes(x = E_rate, y = eco_cncrn)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,10), breaks = c(0:10), labels = c("Not at all\nConcerned","1","2","3","4","5","6","7","8", "9", "Extremely\nConcerned")) +
  xlab("Egalitarianism score\n(1 low, 10 high)") +
  ylab("Concern for Wildlife") +
  theme(panel.grid.minor = element_blank(), legend.position = "none") #+ ggsave("Erate_v_cncrn.png")

#Scatterplot with LM for nep vs eco_frag
NEPvFrag = ggplot(data, aes(x = nep, y = eco_frag)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(1,4), breaks = c(1:4), labels = c("Extremely\nRobust","Relatively\nRobust","Relatively\nFragile","Extremely\nFragile")) +
  xlab("NEP Score\n(1 low NEP, 5 high NEP)") +
  ylab("Percieved Fragility of Nature") +
  theme(panel.grid.minor = element_blank(), legend.position = "none") #+ ggsave("ENEP_v_frag.png")

#Boxplot comparing eco_cncrn to eco_see responses (0 not seen impacts, 1 seen, 2 not sure)
CncrnvSee = ggplot(data, aes(x = factor(eco_see), y = eco_cncrn, fill = factor(eco_see))) +
  geom_boxplot() +
  scale_x_discrete(breaks = c(0:2, NA), labels = c("No","Yes","Not Sure", "N/A")) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#56B4E9", "#999999"), na.value = "Grey", breaks = c(0:2)) +
  xlab("Did you see an impact to wildlife?") +
  ylab("Score for Wildlife Concern") + theme_bw() +
  theme(legend.position = "none") #+ ggsave("cncrn_v_see.png")


#Jeremy plot - heatmap of responses
wild.wthr <- data %>% group_by(eco_wildlife,eco_extwthr) %>% tally() 

wild.wthr$eco_wildlife<-recode(wild.wthr$eco_wildlife, '1'="Amphibians",'2'="Birds",'3'="Fish",'4'="Mammals",'5'="Plants",
                               '6'="Reptiles",'7'="Invertebrates",'8'="Other", .default="No response")
wild.wthr$eco_extwthr<-recode(wild.wthr$eco_extwthr, '1'="High Winds",'2'="Drought",'3'="Lightning",'4'="Cold",'5'="Snowstorms",
                              '6'="Ice Storms",'7'="Flash Floods",'8'="Tornadoes", '9'="Wildfires",'10'="Heat",.default="No response")
wild.wthr[is.na(wild.wthr)] <- "No response"
library(reshape2)
# converting data to long form for ggplot2 use

wild.wthr<-wild.wthr[order(-wild.wthr$n),]
ww.resp <- subset(wild.wthr, eco_wildlife!="Other")
ww.resp <- subset(ww.resp, eco_wildlife!="No response")
ww.resp <- subset(ww.resp, eco_extwthr!="No response")

WxE.plot <- ggplot(ww.resp, aes(reorder(eco_extwthr, -n), reorder(eco_wildlife, n))) +
  geom_tile(aes(fill = sqrt(n)),colour = "black") + scale_fill_gradient(low = "white", high = "black")+ #, name=bquote(n^0.5)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35)) + 
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10, title=expression(sqrt(n))))

heatmap <- WxE.plot + labs(x = "\nMost Impactful Weather Extreme", y="Group Most Impacted\n")


data$race <- car::recode(data$race,"3:7 = 3")
table(data$race)

# Multiple Regression Models =====================================================================
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean_df <- data %>%
  select(nep,H_rate,I_rate,E_rate,F_rate,eco_see,gender,age,race,hispanic,log.inc) %>%
  mutate(eco_see = getmode(eco_see),gender = getmode(gender),race = getmode(race),hispanic = getmode(hispanic)) %>%
  summarize_all(mean,na.rm=TRUE)

ecolm <- lm(data = data, eco_cncrn ~ eco_frag)
summary(ecolm)
augment(ecolm, newdata = data.frame(eco_frag = 1:4, eco_cncrn = mean(data$eco_cncrn, na.rm = T)))

#Comparison of linear models for eco_frag vs the scales of interest
fit1 <- lm(eco_frag ~ nep + H_rate + I_rate + E_rate + F_rate + factor(eco_see) + factor(gender) + age + factor(race) + factor(hispanic) + log.inc, data = data)
summary(fit1)

nep1 <- augment(fit1, newdata = data.frame(nep = 1:5, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Hie1 <- augment(fit1, newdata = data.frame(H_rate = 0:10, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Ind1 <- augment(fit1, newdata = data.frame(I_rate = 0:10, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Ega1 <- augment(fit1, newdata = data.frame(E_rate = 0:10, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Fat1 <- augment(fit1, newdata = data.frame(F_rate = 0:10, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
See1 <- augment(fit1, newdata = data.frame(eco_see = 0:2, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
#Gen1 <- augment(fit1, newdata = data.frame(gender = 0:1, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Age1 <- augment(fit1, newdata = data.frame(age = 18:99, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
#Rac1 <- augment(fit1, newdata = data.frame(race = 1:3, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
His1 <- augment(fit1, newdata = data.frame(hispanic = 0:1, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Inc1 <- augment(fit1, newdata = data.frame(log.inc = 9:14, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)



#Comparison of linear models for eco_cncrn vs the scales of interest
fit2 <- lm(eco_cncrn ~ nep + H_rate + I_rate + E_rate + F_rate + factor(eco_see) + factor(gender) + age + factor(race) + factor(hispanic) + log.inc, data = data)
summary(fit2)

nep2 <- augment(fit2, newdata = data.frame(nep = 1:5,  mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Hie2 <- augment(fit2, newdata = data.frame(H_rate = 0:10, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Ind2 <- augment(fit2, newdata = data.frame(I_rate = 0:10, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Ega2 <- augment(fit2, newdata = data.frame(E_rate = 0:10, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Fat2 <- augment(fit2, newdata = data.frame(F_rate = 0:10, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
See2 <- augment(fit2, newdata = data.frame(eco_see = 0:2, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Gen2 <- augment(fit2, newdata = data.frame(gender = 0:1, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Age2 <- augment(fit1, newdata = data.frame(age = 18:99, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
#Rac2 <- augment(fit2, newdata = data.frame(race = 1:3, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
#His2 <- augment(fit2, newdata = data.frame(hispanic = 0:1, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
Inc2 <- augment(fit2, newdata = data.frame(log.inc = 9:14, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)


intermod1 <- lm(data = data, eco_cncrn ~ factor(eco_see) * eco_frag + factor(eco_see) * E_rate + factor(eco_see) * nep)
summary(intermod1)
intermod2 <- lm(data = data, eco_frag ~ factor(eco_see) * eco_cncrn + factor(eco_see) * E_rate + factor(eco_see) * nep)
summary(intermod2)
mean_df <- data %>%
  select(nep,E_rate,eco_see,eco_cncrn,eco_frag) %>%
  mutate(eco_see = getmode(eco_see)) %>%
  summarize_all(mean,na.rm=TRUE)

int0 <- augment(intermod2, newdata = data.frame(eco_see = 0:1, E_rate = 0, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
int1 <- augment(intermod2, newdata = data.frame(eco_see = 0:1, E_rate = 1, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
int2 <- augment(intermod2, newdata = data.frame(eco_see = 0:1, E_rate = 2, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
int3 <- augment(intermod2, newdata = data.frame(eco_see = 0:1, E_rate = 3, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
int4 <- augment(intermod2, newdata = data.frame(eco_see = 0:1, E_rate = 4, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
int5 <- augment(intermod2, newdata = data.frame(eco_see = 0:1, E_rate = 5, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
int6 <- augment(intermod2, newdata = data.frame(eco_see = 0:1, E_rate = 6, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
int7 <- augment(intermod2, newdata = data.frame(eco_see = 0:1, E_rate = 7, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
int8 <- augment(intermod2, newdata = data.frame(eco_see = 0:1, E_rate = 8, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
int9 <- augment(intermod2, newdata = data.frame(eco_see = 0:1, E_rate = 9, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
int10 <- augment(intermod2, newdata = data.frame(eco_see = 0:1, E_rate = 10, mean_df)) %>% mutate(upper = .fitted + 1.96 * .se.fit,lower = .fitted - 1.96 * .se.fit)
full_join(int0, int1) %>%
  full_join(., int2) %>%
  full_join(., int3) %>%
  full_join(., int4) %>%
  full_join(., int5) %>%
  full_join(., int6) %>%
  full_join(., int7) %>%
  full_join(., int8) %>%
  full_join(., int9) %>%
  full_join(., int10) -> int.df

H1 <- ggplot(Ega2, aes(x = E_rate, y = .fitted)) +
  geom_line(size=1.5, color = "#488915") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .5, fill = "#488915") +
  ylab("Concern for Nature") +
  xlab("Egalitarianism Score") +
  scale_y_continuous(breaks = c(0:10), labels = c("Not at All\nConcerned","1","2","3","4","5","6","7","8","9","Extremely\nConcerned"), limits = c(0,10)) +
  scale_x_continuous(breaks = c(0:10)) +
  theme_bw()
H2 <- ggplot(Ega1, aes(x = E_rate, y = .fitted)) +
  geom_line(size=1.5, color = "dodgerblue2") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .5, fill = "dodgerblue2") +
  ylab("Fragility of Nature") +
  xlab("Egalitarianism Score") +
  scale_x_continuous(breaks = c(0:10)) +
  scale_y_continuous(breaks = c(1:4), labels = c("Extremely\nRobust","Relatively\nRobust","Relatively\nFragile","Extremely\nFragile"), limits = c(1,4)) +
  theme_bw()
H3 <- ggplot(Ind2, aes(x = I_rate, y = .fitted)) +
  geom_line(size=1.5, color = "#488915") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .5, fill = "#488915") +
  ylab("Concern for Nature") +
  xlab("Individualism Score") +
  scale_y_continuous(breaks = c(0:10), labels = c("Not at All\nConcerned","1","2","3","4","5","6","7","8","9","Extremely\nConcerned"), limits = c(0,10)) +
  scale_x_continuous(breaks = c(0:10)) +
  theme_bw()
H4 <- ggplot(Ind1, aes(x = I_rate, y = .fitted)) +
  geom_line(size=1.5, color = "dodgerblue2") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .5, fill = "dodgerblue2") +
  ylab("Fragility of Nature") +
  xlab("Individualism Score") +
  scale_y_continuous(breaks = c(1:4), labels = c("Extremely\nRobust","Relatively\nRobust","Relatively\nFragile","Extremely\nFragile"), limits = c(1,4)) +
  scale_x_continuous(breaks = c(0:10)) +
  theme_bw()
H5 <- ggplot(Hie2, aes(x = H_rate, y = .fitted)) +
  geom_line(size=1.5, color = "#488915") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .5, fill = "#488915") +
  ylab("Concern for Nature") +
  xlab("Hierarchy Score") +
  scale_y_continuous(breaks = c(0:10), labels = c("Not at All\nConcerned","1","2","3","4","5","6","7","8","9","Extremely\nConcerned"), limits = c(0,10)) +
  scale_x_continuous(breaks = c(0:10)) +
  theme_bw()
H6 <- ggplot(Hie1, aes(x = H_rate, y = .fitted)) +
  geom_line(size=1.5, color = "dodgerblue2") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .5, fill = "dodgerblue2") +
  ylab("Fragility of Nature") +
  xlab("Hierarchy Score") +
  scale_y_continuous(breaks = c(1:4), labels = c("Extremely\nRobust","Relatively\nRobust","Relatively\nFragile","Extremely\nFragile"), limits = c(1,4)) +
  scale_x_continuous(breaks = c(0:10)) +
  theme_bw()
H7 <- ggplot(Fat2, aes(x = F_rate, y = .fitted)) +
  geom_line(size=1.5, color = "#488915") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .5, fill = "#488915") +
  ylab("Concern for Nature") +
  xlab("Fatalism Score") +
  scale_y_continuous(breaks = c(0:10), labels = c("Not at All\nConcerned","1","2","3","4","5","6","7","8","9","Extremely\nConcerned"), limits = c(0,10)) +
  scale_x_continuous(breaks = c(0:10)) +
  theme_bw()
H8 <- ggplot(Fat1, aes(x = F_rate, y = .fitted)) +
  geom_line(size=1.5, color = "dodgerblue2") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .5, fill = "dodgerblue2") +
  ylab("Fragility of Nature") +
  xlab("Fatalism Score") +
  scale_y_continuous(breaks = c(1:4), labels = c("Extremely\nRobust","Relatively\nRobust","Relatively\nFragile","Extremely\nFragile"), limits = c(1,4)) +
  scale_x_continuous(breaks = c(0:10)) +
  theme_bw()
H9 <- ggplot(nep2, aes(x = nep, y = .fitted)) +
  geom_line(size=1.5, color = "#488915") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .5, fill = "#488915") +
  ylab("Concern for Nature") +
  xlab("NEP Score") +
  scale_y_continuous(breaks = c(0:10), labels = c("Not at All\nConcerned","1","2","3","4","5","6","7","8","9","Extremely\nConcerned"), limits = c(0,10)) +
  scale_x_continuous(breaks = c(1:5)) +
  theme_bw()
H10 <- ggplot(nep1, aes(x = nep, y = .fitted)) +
  geom_line(size=1.5, color = "dodgerblue2") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .5, fill = "dodgerblue2") +
  ylab("Fragility of Nature") +
  xlab("NEP Score") +
  scale_y_continuous(breaks = c(1:4), labels = c("Extremely\nRobust","Relatively\nRobust","Relatively\nFragile","Extremely\nFragile"), limits = c(1,4)) +
  scale_x_continuous(breaks = c(1:5)) +
  theme_bw()
H11 <- ggplot(See2, aes(x = as.factor(eco_see), y = .fitted)) +
  geom_point(color = "#488915", size = 2) +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.2, size = 1, col = "#488915") +
  ylab("Concern for Nature") +
  xlab("Experienced Wildlife Impact") +
  scale_x_discrete(labels = c("No","Yes","Don't\nKnow")) +
  scale_y_continuous(breaks = c(0:10), labels = c("Not at All\nConcerned","1","2","3","4","5","6","7","8","9","Extremely\nConcerned"), limits = c(0,10)) +
  theme_bw()
H12 <- ggplot(See1, aes(x = as.factor(eco_see), y = .fitted)) +
  geom_point(color = "dodgerblue2", size = 2) +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.2, size = 1, col = "dodgerblue2") +
  ylab("Fragility of Nature") +
  xlab("Experienced Wildlife Impact") +
  scale_x_discrete(labels = c("No","Yes","Don't\nKnow")) +
  scale_y_continuous(breaks = c(1:4), labels = c("Extremely\nRobust","Relatively\nRobust","Relatively\nFragile","Extremely\nFragile"), limits = c(1,4)) +
  theme_bw()

col_scale<-colorRampPalette(c("#7c5100","#38e702"))(11)
int <- ggplot(int.df, aes(x = eco_see, y = .fitted, color = as.factor(E_rate))) +
  geom_line(size = 1) +
  ylab("Fragility of Nature") +
  xlab("Experienced Wildlife Impact") +
  scale_color_manual(values = c(col_scale[1], col_scale[2], col_scale[3],
                                col_scale[4], col_scale[5], col_scale[6], col_scale[7],
                                col_scale[8], col_scale[9], col_scale[10], col_scale[11]),
                     labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                     name = "Egalitarianism") +
  scale_x_continuous(breaks = c(0:1), labels = c("No","Yes")) +
  scale_y_continuous(breaks = c(1:4), labels = c("Extremely\nRobust","Relatively\nRobust","Relatively\nFragile","Extremely\nFragile"), limits = c(1,4)) +
  theme_bw()

stargazer(ecolm,fit1,fit2,type = "text")


#SEM models =================================================================================================

natmod <- '
#Regressions
nep ~ age + gender + hispanic + log.inc + I_rate + E_rate
eco_frag ~ age + gender + hispanic + log.inc + I_rate + E_rate
eco_cncrn ~ nep + eco_frag + age + gender + hispanic + log.inc + I_rate + E_rate
#Correlations
nep ~~ eco_frag
'
fitmod <- sem(natmod, data=data, std.lv=TRUE)
summary(fitmod,  fit.measures=T, standardized=T)

modpic <- grViz("
digraph boxes_and_circles {
                
                graph[rankdir = LR]
                
                # add node statements
                node [shape = box]
                
                a [label = 'Demographics']
                b [label = 'Cultural Theory']
                
                c [label = 'NEP Score']
                d [label = 'Fragility\nof Nature']
                e [label = 'Concern for\nNature']
                
                # add edge statements
                
                a->c 
                b->c 
                a->d 
                b->d 
                
                c->e 
                d->e

                c->d [dir = both]
                
                #ordering nodes
                
                subgraph {
                rank = same; a; b};

                subgraph {
                rank = same; c; d};
                
                graph [nodesep = 0.5]
                }
                ")

modpic


modpic <- grViz("
digraph boxes_and_circles {
              
              graph[rankdir = LR]
              
              # add node statements
              node [shape = box]
              
              a [label = 'Age']
              b [label = 'Gender']
              c [label = 'Ethnicity']
              d [label = 'Income']
              e [label = 'Individualism']
              f [label = 'Egalitarianism']

              g [label = 'NEP Score']
              h [label = 'Fragility\nof Nature']
              i [label = 'Concern for\nNature']

              # add edge statements
              
              a->g [color = orangered, label = '-0.07']
              b->g [color = orangered, label = '-0.07']
              d->g [color = orangered, label = '-0.06']
              e->g [color = orangered, label = '-0.08']
              f->g [color = dodgerblue, label = '0.22']

              c->h [color = dodgerblue, label = '0.05']
              e->h [color = orangered, label = '-0.09']
              f->h [color = dodgerblue, label = '0.12']

              g->i [color = dodgerblue, label = '0.30']
              h->i [color = dodgerblue, label = '0.15']
              b->i [color = orangered, label = '-0.07']
              d->i [color = orangered, label = '-0.06']
              f->i [color = dodgerblue, label = '0.16']


              h->g [dir = both, color = dodgerblue, label ='0.17']
              
              #ordering nodes
              
              subgraph {
              rank = same; a; b; c; d; e; f};
              
              subgraph {
              rank = same; g; h};
              
              graph [nodesep = 0.5]
              }
              ")

modpic


#Graphics construction ======================================================================================

windows(width = 8, height = 5)
F1 <- grid.arrange(pfrag,Frag_Cncrn,pCncrn, layout_matrix = rbind(c(1,2),c(3)))
ggsave("fig1.png",F1)

windows(width = 8, height = 5)
F2 <- grid.arrange(pEcoW,pExtr, nrow = 1)
ggsave("fig2.png",F2)

windows(height = 8, width = 8)
F3 <- grid.arrange(pNEP,pF,pH,pEsee,pE,pI, layout_matrix = rbind(c(1,4), c(2, 3), c(5, 6)))
ggsave("fig3.png",F3)

windows(width = 8, height = 4)
H1n2 <- grid.arrange(H1,H2, nrow = 1)
ggsave("H1and2.png",H1n2)

windows(width = 8, height = 4)
H3n4 <- grid.arrange(H3,H4, nrow = 1)
ggsave("H3and4.png",H3n4)

windows(width = 8, height = 4)
H5n6 <- grid.arrange(H5,H6, nrow = 1)
ggsave("H5and6.png",H5n6)

windows(width = 8, height = 4)
H7n8 <- grid.arrange(H7,H8, nrow = 1)
ggsave("H7and8.png",H7n8)

windows(width = 8, height = 4)
H9n10 <- grid.arrange(H9,H10, nrow = 1) 
ggsave("H9and10.png",H9n10)

windows(width = 8, height = 4)
H11n12 <- grid.arrange(H11,H12, nrow = 1) 
ggsave("H11and12.png",H11n12)

windows(width = 6, height = 4)
int <- grid.arrange(int)
ggsave("Intplot.png",int)

windows(width = 6, height = 4)
htm <- grid.arrange(heatmap)
ggsave("Heatplot.png",htm)

#Table of descriptive statistics ============================================================================
Datatable <- data %>%
  select(userid, wave_id.x, age, gender, hispanic, race, income, eco_frag, eco_wildlife, eco_extwthr, eco_see, eco_cncrn, nep, F_rate, H_rate, I_rate, E_rate)
stargazer(Datatable, omit.summary.stat = c("p25","p75"),  column.sep.width = "10pt", type = "text", out="D:/Documents/Work Documents/Aeroecology Project/DesStatsTable.html")

stargazer(fit1,fit2,column.sep.width = "10pt", type = "text", out="D:/Documents/Work Documents/Aeroecology Project/RegStatsTable.html")

#Edited code from Jeremy ====================================================================================

#Setup for creating maps =====================================================================================
library(maps)
library(dplyr)
#function to split a string and return a 2 column dataframe
strtodf<-function (list){
  list <- as.character(list)
  slist<-strsplit(list, ",")
  y<-sapply(slist, FUN= function(x) {x[2]})
  df<-data.frame(county=y, stringsAsFactors = FALSE)
  return(df)
}

maplist<-map("county", 'oklahoma', namesonly = TRUE, plot=FALSE)
OKlist<-strtodf(maplist)  #convert to dataframe
OKlist$row<-as.numeric(rownames(OKlist))

### Responses per county
cnty.tots <- data %>% group_by(confirm_county) %>% tally() 
cnty.tots$confirm_county <- tolower(cnty.tots$confirm_county)
OKlist$total <- cnty.tots$n[match(OKlist$county, cnty.tots$confirm_county)]


#Proportion of respondents indicating Drought was most impactful type of event ===============================
cnty.evnts <- data %>% group_by(confirm_county,eco_extwthr) %>% tally() 
cnty.evnts <- subset(cnty.evnts, (!is.na(cnty.evnts$confirm_county)))
cnty.evnts <- subset(cnty.evnts, eco_extwthr == 2)
cnty.evnts$confirm_county <- tolower(cnty.evnts$confirm_county)
OKlist$drought.cncrn <- cnty.evnts$n[match(OKlist$county, cnty.evnts$confirm_county)]
OKlist$drought.cncrn[is.na(OKlist$drought.cncrn)] <- 0

OKlist$drought.ppn <- (OKlist$drought.cncrn/OKlist$total)
OKlist$drought.col <- as.numeric(cut(OKlist$drought.ppn, c(-1, -0.01, 0, 0.2, 0.4, 0.6, 0.8, 1.0)))

OKlist<-OKlist[order(OKlist$row),]
colors = c("#FFFFFF","#e5eefc", "#E5E100", "#C8AA00", "#903C00", "#740501","#440800")
lnames = c("No Data","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
map("county", 'oklahoma', fill=TRUE, col=colors[OKlist$drought.col])
title(main="Drought as the Most Impactful Event", font.main=2, cex.main=1.2, col.main="brown")
legend("left", lnames, fill = colors, bty="n", cex=0.95, text.font=12, y.intersp=1, title="Response Percentage")


#Proportion of respondents indicating Wildfires was most impactful type of event =============================
cnty.evnts <- data %>% group_by(confirm_county,eco_extwthr) %>% tally()
cnty.evnts <- subset(cnty.evnts, (!is.na(cnty.evnts$confirm_county)))
cnty.evnts <- subset(cnty.evnts, eco_extwthr == 9)
cnty.evnts$confirm_county <- tolower(cnty.evnts$confirm_county)
OKlist$wildfire.cncrn <- cnty.evnts$n[match(OKlist$county, cnty.evnts$confirm_county)]
OKlist$wildfire.cncrn[is.na(OKlist$wildfire.cncrn)] <- 0

OKlist$wildfire.ppn <- (OKlist$wildfire.cncrn/OKlist$total)
OKlist$wildfire.col <- as.numeric(cut(OKlist$wildfire.ppn, c(-1, -0.01, 0, 0.2, 0.4, 0.6, 0.8, 1.0)))

OKlist<-OKlist[order(OKlist$row),]
colors = c("#FFFFFF","#e5eefc", "#E5E100", "#C8AA00", "#903C00", "#740501","#440800")
lnames = c("No Data","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
map("county", 'oklahoma', fill=TRUE, col=colors[OKlist$wildfire.col])
title(main="Wildfire as the Most Impactful Event", font.main=2, cex.main=2, col.main="brown")
legend("left", lnames, fill = colors, bty="n", cex=0.95, text.font=12, y.intersp=1, title="Response Percentage")


#Proportion of respondents indicating Extreme Heat was most impactful type of event ==========================
cnty.evnts <- data %>% group_by(confirm_county,eco_extwthr) %>% tally() 
cnty.evnts <- subset(cnty.evnts, (!is.na(cnty.evnts$confirm_county)))
cnty.evnts <- subset(cnty.evnts, eco_extwthr == 10)
cnty.evnts$confirm_county <- tolower(cnty.evnts$confirm_county)
OKlist$heat.cncrn <- cnty.evnts$n[match(OKlist$county, cnty.evnts$confirm_county)]
OKlist$heat.cncrn[is.na(OKlist$heat.cncrn)] <- 0

OKlist$heat.ppn <- (OKlist$heat.cncrn/OKlist$total)
OKlist$heat.col <- as.numeric(cut(OKlist$heat.ppn, c(-1,-0.01,0, 0.1, 0.2, 0.3, 0.4, 1.0)))

OKlist<-OKlist[order(OKlist$row),]
colors = c("#FFFFFF","#e5eefc", "#E5E100", "#C8AA00", "#903C00", "#740501","#440800")
lnames = c("No Data","0%","1-10%", "11-20%", "21-30%", "31-40%", ">40%")
map("county", 'oklahoma', fill=TRUE, col=colors[OKlist$heat.col])
title(main="Heat as the Most Impactful Event", font.main=2, cex.main=1.2, col.main="maroon4")
legend("left", lnames, fill = colors, bty="n", cex=0.95, text.font=12, y.intersp=1, title="Response Percentage")


#Proportion of respondents indicating Flash Flooding was most impactful type of event ========================
cnty.evnts <- data %>% group_by(confirm_county,eco_extwthr) %>% tally() 
cnty.evnts <- subset(cnty.evnts, (!is.na(cnty.evnts$confirm_county)))
cnty.evnts <- subset(cnty.evnts, eco_extwthr == 7)
cnty.evnts$confirm_county <- tolower(cnty.evnts$confirm_county)
OKlist$flood.cncrn <- cnty.evnts$n[match(OKlist$county, cnty.evnts$confirm_county)]
OKlist$flood.cncrn[is.na(OKlist$flood.cncrn)] <- 0

OKlist$flood.ppn <- (OKlist$flood.cncrn/OKlist$total)
OKlist$flood.col <- as.numeric(cut(OKlist$flood.ppn, c(-1,-0.01,0, 0.1, 0.2, 0.3, 0.4, 1.0)))

OKlist<-OKlist[order(OKlist$row),]
colors = c("#FFFFFF","#e5eefc", "#E5E100", "#C8AA00", "#903C00", "#740501","#440800")
lnames = c("No Data","0%","1-10%", "11-20%", "21-30%", "31-40%", ">40%")
map("county", 'oklahoma', fill=TRUE, col=colors[OKlist$flood.col])
title(main="Flash Floods as the Most Impactful Event", font.main=2, cex.main=2, col.main="orange")
legend("left", lnames, fill = colors, bty="n", cex=0.95, text.font=12, y.intersp=1, title="Response Percentage")


#Proportion of respondents indicating Ice Storms was most impactful type of event ============================
cnty.evnts <- data %>% group_by(confirm_county,eco_extwthr) %>% tally()
cnty.evnts <- subset(cnty.evnts, (!is.na(cnty.evnts$confirm_county)))
cnty.evnts <- subset(cnty.evnts, eco_extwthr == 6)
cnty.evnts$confirm_county <- tolower(cnty.evnts$confirm_county)
OKlist$ice.cncrn <- cnty.evnts$n[match(OKlist$county, cnty.evnts$confirm_county)]
OKlist$ice.cncrn[is.na(OKlist$ice.cncrn)] <- 0

OKlist$ice.ppn <- (OKlist$ice.cncrn/OKlist$total)
#OKlist$ice.ppn[is.na(OKlist$ice.ppn)] <- -1
OKlist$ice.col <- as.numeric(cut(OKlist$ice.ppn, c(-1, -0.01, 0, 0.2, 0.4, 0.6, 0.8, 1.0)))

OKlist<-OKlist[order(OKlist$row),]
colors = c("#FFFFFF","#e5eefc", "#E5E100", "#C8AA00", "#903C00", "#740501","#440800")
lnames = c("No Data","0%","1-20%", "21-40%", "41-60%", "61-80%", "81-100%")
map("county", 'oklahoma', fill=TRUE, col=colors[OKlist$ice.col])
title(main="Ice Storms as the Most Impactful Event", font.main=2, cex.main=2, col.main="blue")
legend("left", lnames, fill = colors, bty="n", cex=0.95, text.font=12, y.intersp=1, title="Response Percentage")


#Perceptions of wildlife impacts by county ===================================================================
### Proportion of respondents indicating bird are most impacted
cnty.evnts <- data %>% group_by(confirm_county,eco_wildlife) %>% tally()
cnty.evnts <- subset(cnty.evnts, (!is.na(cnty.evnts$confirm_county)))
cnty.evnts <- subset(cnty.evnts, eco_wildlife == 2) 
cnty.evnts$confirm_county <- tolower(cnty.evnts$confirm_county)
OKlist$bird.cncrn <- cnty.evnts$n[match(OKlist$county, cnty.evnts$confirm_county)]
OKlist$bird.cncrn[is.na(OKlist$bird.cncrn)] <- 0

OKlist$bird.ppn <- (OKlist$bird.cncrn/OKlist$total)
#OKlist$bird.ppn[is.na(OKlist$bird.ppn)] <- -1
OKlist$bird.col <- as.numeric(cut(OKlist$bird.ppn, c(-1,-0.01,0, 0.1, 0.2, 0.3, 0.4, 1.0)))

OKlist<-OKlist[order(OKlist$row),]
colors = c("#FFFFFF","#e5eefc","#E5E100", "#C8AA00", "#AC7300", "#903C00", "#740501")
lnames = c("No Data","0%","1-10%", "11-20%", "21-30%", "31-40%", ">40%")
map("county", 'oklahoma', fill=TRUE, col=colors[OKlist$bird.col])
title(main="Birds as the Most Impacted Group", font.main=2, cex.main=2, col.main="orange")
legend("left", lnames, fill = colors, bty="n", cex=0.95, text.font=12, y.intersp=1, title="Response Percentage")


# Map eco_cncrn by county ====================================================================================

cnty.cncrn <- data %>%  group_by(confirm_county) %>%
  dplyr::summarize(Mean = mean(eco_cncrn, na.rm=TRUE)) 
cnty.cncrn$confirm_county <- tolower(cnty.cncrn$confirm_county)

OKlist$meanCncrn <- cnty.cncrn$Mean[match(OKlist$county, cnty.cncrn$confirm_county)]
OKlist$conLvl <- as.numeric(cut(OKlist$meanCncrn, c(-1, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 10)))
colors = c("#FFFFFF","#DCF2CA", "#B3D3A5", "#8BB580", "#62965B", "#3A7836","#125A11")
lnames = c("No Data","2.5-3.5", "3.6-4.5", "4.6-5.5", "5.6-6.5", "6.6-7.5",">7.5")
map("county", 'oklahoma', fill=TRUE, col=colors[OKlist$conLvl])
title(main="Concern about Ecological Impacts of Weather", font.main=2, cex.main=1.2, col.main="dark green")
legend("left", lnames, fill = colors, bty="n", cex=0.9, text.font=12, y.intersp=1, title="Level of Concern \n (Out of 10)")


#Wildlife by Event heatmap ===================================================================================
wild.wthr <- data %>% group_by(eco_wildlife,eco_extwthr) %>% tally() 

wild.wthr$eco_wildlife<-recode(wild.wthr$eco_wildlife, '1'="Amphibians",'2'="Birds",'3'="Fish",'4'="Mammals",'5'="Plants",
                               '6'="Reptiles",'7'="Invertebrates",'8'="Other", .default="No response")
wild.wthr$eco_extwthr<-recode(wild.wthr$eco_extwthr, '1'="High Winds",'2'="Drought",'3'="Lightning",'4'="Cold",'5'="Snowstorms",
                              '6'="Ice Storms",'7'="Flash Floods",'8'="Tornadoes", '9'="Wildfires",'10'="Heat",.default="No response")
wild.wthr[is.na(wild.wthr)] <- "No response"
library(reshape2)
# converting data to long form for ggplot2 use

wild.wthr<-wild.wthr[order(-wild.wthr$n),]
ww.resp <- subset(wild.wthr, eco_wildlife!="Other")
ww.resp <- subset(ww.resp, eco_wildlife!="No response")
ww.resp <- subset(ww.resp, eco_extwthr!="No response")

WxE.plot <- ggplot(ww.resp, aes(reorder(eco_extwthr, -n), reorder(eco_wildlife, n))) +
  geom_tile(aes(fill = sqrt(n)),colour = "black") + scale_fill_gradient(low = "white", high = "black")+ #, name=bquote(n^0.5)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35), text = element_text(size=15)) + 
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10, title=expression(sqrt(n))))

WxE.plot + labs(x = "\nMost Impactful Weather Extreme", y="Group Most Impacted\n")

### Without Plants, Other, or No response
ww.animals <- subset(wild.wthr, eco_wildlife!="Plants")
ww.animals <- subset(ww.animals, eco_wildlife!="Other")
ww.animals <- subset(ww.animals, eco_wildlife!="No response")
ww.animals <- subset(ww.animals, eco_extwthr!="No response")

WxE.plot2 <- ggplot(ww.animals, aes(reorder(eco_extwthr, -n), reorder(eco_wildlife, n))) +
  geom_tile(aes(fill = sqrt(n)),colour = "black") + scale_fill_gradient(low = "white", high = "dark red", name=bquote(n^0.5)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35), text = element_text(size=20))

WxE.plot2 + labs(title="Heatmap of Responses",x = "Most Impactful Weather Extreme", y="Group Most Impacted")
