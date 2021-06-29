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

library(rcompanion)
groupwiseMean(data = Wushu, var = "relative_net_impulse", group = "Period",
              conf   = 0.95, digits = 3, R = 1000, traditional = FALSE, 
              bca = TRUE)

#Statistical Analyzes and Plots
library(rstatix) #needed for normality and Repeated Measures Anova
#Normality test
Wushu %>% group_by(Period) %>% shapiro_test(JH)

# Repeated Measures Anova (within subjects) 
res.aov <- anova_test(data = Wushu, dv = JH, 
                      wid = ID, within = Period, effect.size = "pes")

# Greenhouse-Geisser sphericity correction is automatically applied- 
#-through the Mauchly's Test for Sphericity 
get_anova_table(res.aov)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Wushu %>%
  pairwise_t_test(JH ~ Period, paired = TRUE, 
                  p.adjust.method = "none")
pwc

# Effect size Cohen's D with Hedge's g correction for small sample size
Wushu  %>% cohens_d(JH ~ Period, 
                       paired = TRUE, hedges.correction = TRUE)

#Plots
library(ggplot2)
library(ggpubr)
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Period")
# Boxplot of Vertical Jump Height
p_a <- ggboxplot(Wushu, x = "Period", y = "JH", color = "Period",
                 palette = get_palette("Set1", 4),
                 title = "A)", ylab = "Vertical Jump Height (cm)") + 
  stat_pvalue_manual(pwc,size = 2.8,hide.ns = TRUE) 
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

# Effect size Cohen's D with Hedge's g correction for small sample size
library(rstatix)
Wushu  %>% cohens_d(peak_velocity ~ Period, 
                    paired = TRUE, hedges.correction = TRUE)

# Boxplot 
pwc <- pwc %>% add_xy_position(x = "Period")
p_b <- ggboxplot(Wushu, x = "Period", y = "peak_velocity", color = "Period",
                 palette = get_palette("Set1", 4),
                 title = "B)", ylab = "Peak Velocity (m/s)") + 
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

# Effect size Cohen's D with Hedge's g correction for small sample size
library(rstatix)
Wushu  %>% cohens_d(power_max ~ Period, 
                    paired = TRUE, hedges.correction = TRUE)

# Boxplot 
pwc <- pwc %>% add_xy_position(x = "Period")
p_c <- ggboxplot(Wushu, x = "Period", y = "power_max", color = "Period",
                 palette = get_palette("Set1", 4),
                 title = "C)", ylab = "Peak Propulsive Power (w)") + 
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

# Effect size Cohen's D with Hedge's g correction for small sample size
library(rstatix)
Wushu  %>% cohens_d(RSImodified ~ Period, 
                    paired = TRUE, hedges.correction = TRUE)

# Boxplot 
pwc <- pwc %>% add_xy_position(x = "Period")
p_d <- ggboxplot(Wushu, x = "Period", y = "RSImodified", color = "Period",
                 palette = get_palette("Set1", 4),
                 title = "B)", ylab = "Reactive Strength Index modified") + 
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



# Anthroprometics data
library(readxl)
Anthro <- read_excel("Wushu_Covid_open.xlsx", 
                    sheet = "Anthroprometics")
View(Anthro )
attach(Anthro)

# Convert Period and Sex from characters to factor variables
Anthro$Period <- factor(Anthro$Period, 
                       levels = c("Pre","Post","Post+4"))
Anthro$Period <- ordered(Anthro$Period, 
                        levels = c("Pre","Post","Post+4"))
Anthro$Sex <- as.factor(Anthro$Sex)

# Descriptives
library(psych)
Anthro%>% describeBy(Period) 

#Normality Test
Anthro %>% group_by(Period) %>% shapiro_test(FM)
# Repeated Measures Anova (within subjects)
res.aov <- anova_test(data = Anthro, dv = FM, 
                      wid = ID, within = Period)
get_anova_table(res.aov)

#Post-hoc pairwise comparisons
pwc <- Anthro %>%
  pairwise_t_test(FM ~ Period, paired = TRUE, 
                  p.adjust.method = "none")
pwc

# Effect size Cohen's D with Hedge's g correction for small sample size
library(rstatix)
Anthro  %>% cohens_d(FM ~ Period, 
                    paired = TRUE, hedges.correction = TRUE)

# Boxplot 
library(ggplot2)
library(ggpubr)
pwc <- pwc %>% add_xy_position(x = "Period")
p_FM <- ggboxplot(Anthro, x = "Period", y = "FM", color = "Period",
                  palette = get_palette("Set1", 3),
                 title = "A)", ylab = "Fat-Mass (kg)") + 
  stat_pvalue_manual(pwc,size = 2.8,hide.ns = TRUE) 
ggsave("Fat-free-mass.png")
#Save Plot

# Fat-Free Mass

#Normality Test
Anthro %>% group_by(Period) %>% shapiro_test(FFM)
# Repeated Measures Anova (within subjects)
res.aov <- anova_test(data = Anthro, dv = FFM, 
                      wid = ID, within = Period)
get_anova_table(res.aov)

#Post-hoc pairwise comparisons
pwc <- Anthro %>%
  pairwise_t_test(FFM ~ Period, paired = TRUE, 
                  p.adjust.method = "none")
pwc

# Effect size Cohen's D with Hedge's g correction for small sample size
library(rstatix)
Anthro  %>% cohens_d(FFM ~ Period, 
                     paired = TRUE, hedges.correction = TRUE)

# Boxplot 
library(ggpubr)
pwc <- pwc %>% add_xy_position(x = "Period")
p_FFM <- ggboxplot(Anthro, x = "Period", y = "FFM", color = "Period",
                   palette = get_palette("Set1", 3),
                   title = "B)", ylab = "Fat-Free Mass (kg)") +
  stat_pvalue_manual(pwc,size = 2.8,hide.ns = TRUE) 
ggsave("Fat-Free-Mass.png")
#Save Plot

#Body Fat %
#Normality Test
Anthro %>% group_by(Period) %>% shapiro_test(BF)
# Repeated Measures Anova (within subjects)
res.aov <- anova_test(data = Anthro, dv = BF, 
                      wid = ID, within = Period)
get_anova_table(res.aov)

#Post-hoc pairwise comparisons
pwc <- Anthro %>%
  pairwise_t_test(BF ~ Period, paired = TRUE, 
                  p.adjust.method = "none")
pwc

# Effect size Cohen's D with Hedge's g correction for small sample size
library(rstatix)
Anthro  %>% cohens_d(BF ~ Period, 
                     paired = TRUE, hedges.correction = TRUE)

# Boxplot 
pwc <- pwc %>% add_xy_position(x = "Period")
p_BF <- ggboxplot(Anthro, x = "Period", y = "BF", color = "Period",
                  palette = get_palette("Set1", 3),
                  title = "C)", ylab = "Body Fat (%)") + 
  stat_pvalue_manual(pwc,size = 2.8,hide.ns = TRUE) 
#Save Plot
ggsave("Bodyfat.png")


ggarrange(p_FM, p_FFM, p_BF, ncol = 1,nrow = 3)
ggsave("all_plots_bodycomp.png")