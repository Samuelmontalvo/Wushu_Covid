library(readxl)
Wushu <- read_excel("Wushu_Covid_open.xlsx", 
                               sheet = "Jump_Data")
View(Wushu)
attach(Wushu)
library(dplyr)

# Convert Period and Sex from characters to factor variables
Wushu$Period <- factor(Wushu$Period, 
                       levels = c("Pre","Post","Post+2","Post+4"))
Wushu$Sex <- as.factor(Wushu$Sexk, levels = c("Male", "Female"))

#Reorder Variables
Wushu %>%
  mutate(Period =  factor(Period,
                          levels = c("Pre","Post","Post+2","Post+4"))) %>%
  arrange(Period)  

library(rstatix) #package needed for normality and repeated measures anova

#Normality test (Shapiro-Wilk)
Wushu %>% group_by(Period) %>% shapiro_test(JH)

# Repeated Measures Anova (within subjects) 
res.aov <- anova_test(data = Wushu, dv = JH, wid = ID, within = Period)

#the Greenhouse-Geisser sphericity correction is automatically applied- 
#-through the Mauchly's Test for Sphericity 
get_anova_table(res.aov)

#Post-hoc
# pairwise comparisons
pwc <- Wushu %>%
  pairwise_t_test(JH ~ Period, paired = TRUE,
    p.adjust.method = "holm")
pwc

#Plot
library(ggplot2)
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Period")
ggplot(Wushu, aes(x = Period,  y = JH, color = Period)) + 
  geom_boxplot() + theme_classic() +
  labs(x = "Period", y = "Vertical Jump Height (cm)") +
  stat_pvalue_manual(pwc,hide.ns = TRUE) 

#Save Plot
ggsave("VJH.png")

# Descriptives
library(psych)
Wushu %>% describeBy(Period)