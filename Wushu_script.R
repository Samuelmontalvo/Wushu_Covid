library(readxl)
Wushu <- read_excel("Wushu_Covid_open.xlsx", 
                               sheet = "Jump_Data")
View(Wushu )
attach(Wushu)
library(dplyr)

# Convert Period and Sex from characters to factor variables
Wushu$Period <- factor(Wushu$Period, 
                       levels = c("Pre","Post","Post+2","Post+4"))
Wushu$Period <- ordered(Wushu$Period, 
                       levels = c("Pre","Post","Post+2","Post+4"))
Wushu$Sex <- as.factor(Wushu$Sex)

# Descriptives
library(psych)
Wushu%>% describeBy(Period) 


#Statistical Analyzes and Plots
library(rstatix) #needed for normality and Repeated Measures Anova
#Normality test
Wushu %>% group_by(Period) %>% shapiro_test(JH)

# Repeated Measures Anova (within subjects) 
res.aov <- anova_test(data = Wushu, dv = JH, 
                      wid = ID, within = Period)

# Greenhouse-Geisser sphericity correction is automatically applied- 
#-through the Mauchly's Test for Sphericity 
get_anova_table(res.aov)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Wushu %>%
  pairwise_t_test(JH ~ Period, paired = TRUE, 
                  p.adjust.method = "none")
pwc

#Plots
library(ggplot2)
library(ggpubr)
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Period")
# Boxplot of Vertical Jump Height
p_a <- ggplot(Wushu, aes(x = Period,  y = JH, color = Period)) + 
  geom_boxplot() + theme_classic() +
  labs(y = "Vertical Jump Height (cm)", tag = "A)") +
  stat_pvalue_manual(pwc,size = 2.8, hide.ns = TRUE) 
#Save Plot
ggsave("VJH.png")



# Replication of the RM test and plots for Peak Velocity

#Normality Test
Wushu %>% group_by(Period) %>% shapiro_test(peak_velocity)
# Repeated Measures Anova (within subjects) 
res.aov <- anova_test(data = Wushu, dv = peak_velocity, 
                      wid = ID, within = Period)
get_anova_table(res.aov)

#Post-hoc pairwise comparisons
pwc <- Wushu %>%
  pairwise_t_test(peak_velocity ~ Period, paired = TRUE, 
                  p.adjust.method = "none")
pwc

# Boxplot 
pwc <- pwc %>% add_xy_position(x = "Period")
p_b <- ggplot(Wushu, aes(x = Period,  y = peak_velocity, color = Period)) + 
  geom_boxplot() + theme_classic() +
  labs(y = "Peak Velocity (m/s)", tag = "B)") +
  stat_pvalue_manual(pwc,size = 2.8,hide.ns = TRUE) 
#Save Plot
ggsave("Peak_Velocity.png")


#Replication of Test for Peak Propulsive Power

#Normality Test
Wushu %>% group_by(Period) %>% shapiro_test(power_max)
# Repeated Measures Anova (within subjects)
res.aov <- anova_test(data = Wushu, dv = power_max, 
                      wid = ID, within = Period)
get_anova_table(res.aov)

#Post-hoc pairwise comparisons
pwc <- Wushu %>%
  pairwise_t_test(power_max ~ Period, paired = TRUE, 
                  p.adjust.method = "none")
pwc

# Boxplot 
pwc <- pwc %>% add_xy_position(x = "Period")
p_c <- ggplot(Wushu, aes(x = Period,  y = power_max, color = Period)) + 
  geom_boxplot() + theme_classic() +
  labs(y = "Peak Propulsive Power (w)", tag = "C)") +
  stat_pvalue_manual(pwc,size = 2.8,hide.ns = TRUE) 
#Save Plot
ggsave("Peak_Propulsive_Power.png")


# RSImod

#Normality Test
Wushu %>% group_by(Period) %>% shapiro_test(RSImodified)
# Repeated Measures Anova (within subjects)
res.aov <- anova_test(data = Wushu, dv = RSImodified, 
                      wid = ID, within = Period)
get_anova_table(res.aov)

#Post-hoc pairwise comparisons
pwc <- Wushu %>%
  pairwise_t_test(RSImodified ~ Period, paired = TRUE, 
                  p.adjust.method = "none")
pwc

# Boxplot 
pwc <- pwc %>% add_xy_position(x = "Period")
p_d <- ggplot(Wushu, aes(x = Period,  y = RSImodified, color = Period)) + 
  geom_boxplot() + theme_classic() +
  labs( y = "RSImod", tag = "D)") +
  stat_pvalue_manual(pwc,size = 2.8,hide.ns = TRUE) 
#Save Plot
ggsave("RSImod.png")

ggarrange(p_a, p_b, p_c, p_d, widths = c(2,2))
ggsave("all_plots.png")


# Reliability
Reliability <- read_excel("Wushu_Covid_open.xlsx", 
                    sheet = "Reliability")
View(Reliability)
attach(Reliability).
Reliability$Period <- as.factor(Reliability$Period)
#ICC PRE
ICC_pre <- Reliability %>% filter(Period == "Pre") %>%
  subset(select = c("JH_1","JH_2","JH_3"))
ICC(ICC_pre)
#ICC post
ICC_post <- Reliability %>% filter(Period == "Post") %>%
  subset(select = c("JH_1","JH_2","JH_3")) 
ICC(ICC_post)
#ICC post+2
ICC_post2 <- Reliability %>% filter(Period == "Post+2") %>%
  subset(select = c("JH_1","JH_2","JH_3")) 
ICC(ICC_post2)
#ICC post+2
ICC_post4 <- Reliability %>% filter(Period == "Post+4") %>%
  subset(select = c("JH_1","JH_2","JH_3")) 
ICC(ICC_post4)