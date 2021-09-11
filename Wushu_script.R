
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
                      wid = ID...5, within = Period)
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
                 title = "A)", ylab = "Fat-Mass (kg)")  +
  stat_pvalue_manual(pwc,size = 2.8, bracket.shorte = 0.05, tip.length = 0.01,
                     hide.ns = TRUE) 
ggsave("Fat-free-mass.png")
#Save Plot

# Fat-Free Mass

#Normality Test
Anthro %>% group_by(Period) %>% shapiro_test(FFM)
# Repeated Measures Anova (within subjects)
res.aov <- anova_test(data = Anthro, dv = FFM, 
                      wid = ID..5, within = Period)
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
  stat_pvalue_manual(pwc,size = 2.8, bracket.shorte = 0.05, tip.length = 0.01,
                      hide.ns = TRUE) 
ggsave("Fat-Free-Mass.png")
#Save Plot

#Body Fat %
#Normality Test
Anthro %>% group_by(Period) %>% shapiro_test(BF)
# Repeated Measures Anova (within subjects)
res.aov <- anova_test(data = Anthro, dv = BF, 
                      wid = ID...5, within = Period)
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
  stat_pvalue_manual(pwc,size = 2.8, bracket.shorte = 0.05, tip.length = 0.01,
                     hide.ns = TRUE) 
#Save Plot
ggsave("Bodyfat.png")


ggarrange(p_FM, p_FFM, p_BF, ncol = 1,nrow = 3)
ggsave("all_plots_bodycomp.png")





library(readxl)
RMC <- read_excel("Wushu_Covid_open.xlsx", 
                               sheet = "RMC")
View(RMC )
df <- RMC 
View(df)
attach(df)

library(rmcorr)

ID <- as.factor(ID)

repeated <- rmcorr(participant = ID,
                   measure1 = FFM, measure2 = JH,
                   dataset = df,
                   CI.level = 0.95, CIs = c("bootstrap"),
                   nreps = 1000, bstrap.out = F)
repeated

library(lme4)
library(ggplot2)
##regression line for all RMC
null.vol <- lmer(JH ~ FFM + (1| ID), data = df, REML = FALSE)

Wu <- ggplot(df, ggplot2::aes(x = FFM, y = JH, group = factor(ID),
                              color = factor(ID))) + 
  geom_point(ggplot2::aes(colour = factor(ID))) +
  geom_line(ggplot2::aes(y = repeated$model$fitted.values), linetype = 1) +
  xlab("Fat-Free Mass (kg)") +
  ylab("Vertical Jump Height (cm)") +
  theme_classic() +
  annotate("text", label = "RMC = 0.470 [0.169,0.809], p=0.013", x = 44, y = 50) +
  geom_abline(intercept = fixef(null.vol)[1], slope = fixef(null.vol)[2],
              colour = "black", size = 1, linetype = 2)  
Wu


library(ggpubr)
pre_period <- subset(df, Period=="Pre")
pre <- ggscatter(pre_period, x = "FFM", y = "JH",
                 add = "reg.line", fullrange = TRUE,                                # Add regression line
                 conf.int = TRUE,                                  # Add confidence interval
                 add.params = list(color = "blue",
                                   fill = "lightgray"))+ labs(y = "Vertical Jump Height (cm)",
                                                              x = "Fat-Free Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 40, label.y = 50,p.accuracy = 0.001, r.accuracy = 0.01)  # Add correlation coefficient
pre

post_period <- subset(df, Period=="Post")
post <- ggscatter(post_period, x = "FFM", y = "JH",
                  add = "reg.line", fullrange = TRUE,                                # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "blue",
                                    fill = "lightgray"))+ labs(y = "Vertical Jump Height (cm)",
                                                               x = "Fat-Free Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 38, label.y = 48, p.accuracy = 0.001, r.accuracy = 0.01)  # Add correlation coefficient
post

post4_period <- subset(df, Period=="Post+4")
post4 <- ggscatter(post4_period, x = "FFM", y = "JH",
                   add = "reg.line", fullrange = TRUE,                                # Add regression line
                   conf.int = TRUE,                                  # Add confidence interval
                   add.params = list(color = "blue",
                                     fill = "lightgray"))+ labs(y = "Vertical Jump Height (cm)",
                                                                x = "Fat-Free Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 38, label.y = 50, p.accuracy = 0.001, r.accuracy = 0.01)    # Add correlation coefficient
post4


ggarrange(Wu,                                                 # First row with scatter plot
          ggarrange(pre, post, post4, ncol = 3, labels = c("B", "C", "D"),
                    label.y = 1.1), # Second row with box and dot plots
          nrow = 2,
          labels = "A")
ggsave("RMR_FFMvsJH.png")


#Repeated measures correlation for FFM and Peak Propulsive Power

repeated <- rmcorr(participant = ID,
                   measure1 = FFM, measure2 = power_max,
                   dataset = df,
                   CI.level = 0.95, CIs = c("bootstrap"),
                   nreps = 1000, bstrap.out = F)
repeated

##regression line for all RMC
null.vol <- lmer(power_max ~ FFM + (1| ID), data = df, REML = FALSE)

Wu <- ggplot(df, ggplot2::aes(x = FFM, y = power_max, group = factor(ID),
                              color = factor(ID))) +
  geom_point(ggplot2::aes(colour = factor(ID))) +
  geom_line(ggplot2::aes(y = repeated$model$fitted.values), linetype = 1) +
  xlab("Fat-Free Mass (kg)") +
  ylab("Peak Propulsive Power (w)") +
  theme_classic() +
  annotate("text", label = "RMC = 0.471 [0.276,0.675], p=0.012", x = 45, y = 4500) +
  geom_abline(intercept = fixef(null.vol)[1], slope = fixef(null.vol)[2],
              colour = "black", size = 1, linetype = 2)
Wu


library(ggpubr)
pre_period <- subset(df, Period=="Pre")
pre <- ggscatter(pre_period, x = "FFM", y = "power_max",
                 add = "reg.line", fullrange = TRUE,                                # Add regression line
                 conf.int = TRUE,                                  # Add confidence interval
                 add.params = list(color = "blue",
                                   fill = "lightgray"))+ labs(y = "Peak Propulsive Power (w)",
                                                              x = "Fat-Free Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 40, label.y = 4800,p.accuracy = 0.001, r.accuracy = 0.01)  # Add correlation coefficient
pre

post_period <- subset(df, Period=="Post")
post <- ggscatter(post_period, x = "FFM", y = "power_max",
                  add = "reg.line", fullrange = TRUE,                                # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "blue",
                                    fill = "lightgray"))+ labs(y = "Peak Propulsive Power (w)",
                                                               x = "Fat-Free Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 38, label.y = 4700, p.accuracy = 0.001, r.accuracy = 0.01)  # Add correlation coefficient
post

post4_period <- subset(df, Period=="Post+4")
post4 <- ggscatter(post4_period, x = "FFM", y = "power_max",
                   add = "reg.line", fullrange = TRUE,                                # Add regression line
                   conf.int = TRUE,                                  # Add confidence interval
                   add.params = list(color = "blue",
                                     fill = "lightgray"))+ labs(y = "VPeak Propulsive Power (w)",
                                                                x = "Fat-Free Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 38, label.y = 4700, p.accuracy = 0.001, r.accuracy = 0.01)    # Add correlation coefficient
post4


ggarrange(Wu,                                                 # First row with scatter plot
          ggarrange(pre, post, post4, ncol = 3, labels = c("B", "C", "D"),
                    label.y = 1.1), # Second row with box and dot plots
          nrow = 2,
          labels = "A")
ggsave("RMR_FFMvsPPP.png")




#Repeated measures correlation for FFM and Peak Velocity

repeated <- rmcorr(participant = ID,
                   measure1 = FFM, measure2 = peak_velocity,
                   dataset = df,
                   CI.level = 0.95, CIs = c("bootstrap"),
                   nreps = 1000, bstrap.out = F)
repeated

##regression line for all RMC
null.vol <- lmer(peak_velocity ~ FFM + (1| ID), data = df, REML = FALSE)


Wu <- ggplot(df, ggplot2::aes(x = FFM, y = peak_velocity, group = factor(ID),
                              color = factor(ID))) +
  geom_point(ggplot2::aes(colour = factor(ID))) +
  geom_line(ggplot2::aes(y = repeated$model$fitted.values), linetype = 1) +
  xlab("Fat-Free Mass (kg)") +
  ylab("Peak Velocity (m/s)") +
  theme_classic() +
  annotate("text", label = "RMC = 0.471 [0.276,0.675], p=0.012", x = 45, y = 3.5) +
  geom_abline(intercept = fixef(null.vol)[1], slope = fixef(null.vol)[2],
              colour = "black", size = 1, linetype = 2)
Wu


pre_period <- subset(df, Period=="Pre")
pre <- ggscatter(pre_period, x = "FFM", y = "peak_velocity",
                 add = "reg.line", fullrange = TRUE,                                # Add regression line
                 conf.int = TRUE,                                  # Add confidence interval
                 add.params = list(color = "blue",
                                   fill = "lightgray"))+ labs(y = "Peak Velocity (m/s)",
                                                              x = "Fat-Free Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 40, label.y = 3.5,p.accuracy = 0.001, r.accuracy = 0.01)  # Add correlation coefficient
pre

post_period <- subset(df, Period=="Post")
post <- ggscatter(post_period, x = "FFM", y = "peak_velocity",
                  add = "reg.line", fullrange = TRUE,                                # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "blue",
                                    fill = "lightgray"))+ labs(y = "Peak Velocity (m/s)",
                                                               x = "Fat-Free Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 38, label.y = 3.5, p.accuracy = 0.001, r.accuracy = 0.01)  # Add correlation coefficient
post

post4_period <- subset(df, Period=="Post+4")
post4 <- ggscatter(post4_period, x = "FFM", y = "peak_velocity",
                   add = "reg.line", fullrange = TRUE,                                # Add regression line
                   conf.int = TRUE,                                  # Add confidence interval
                   add.params = list(color = "blue",
                                     fill = "lightgray"))+ labs(y = "Peak Velocity (m/s)",
                                                                x = "Fat-Free Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 38, label.y = 3.5, p.accuracy = 0.001, r.accuracy = 0.01)    # Add correlation coefficient
post4


ggarrange(Wu,                                                 # First row with scatter plot
          ggarrange(pre, post, post4, ncol = 3, labels = c("B", "C", "D"),
                    label.y = 1.1), # Second row with box and dot plots
          nrow = 2,
          labels = "A")
ggsave("RMR_FFMvsPV.png")


# Body fat and peak velocity
repeated <- rmcorr(participant = ID,
                   measure1 = BF, measure2 = peak_velocity,
                   dataset = df,
                   CI.level = 0.95, CIs = c("bootstrap"),
                   nreps = 1000, bstrap.out = F)
repeated
##regression line for all RMC
null.vol <- lmer(peak_velocity ~ BF + (1| ID), data = df, REML = FALSE)


Wu <- ggplot(df, ggplot2::aes(x = BF, y = peak_velocity, group = factor(ID),
                              color = factor(ID))) +
  geom_point(ggplot2::aes(colour = factor(ID))) +
  geom_line(ggplot2::aes(y = repeated$model$fitted.values), linetype = 1) +
  xlab("Body Fat (%)") +
  ylab("Peak Velocity (m/s)") +
  theme_classic() +
  annotate("text", label = "RMC = 0.471 [0.276,0.675], p=0.012", x = 12, y = 3.5) +
  geom_abline(intercept = fixef(null.vol)[1], slope = fixef(null.vol)[2],
              colour = "black", size = 1, linetype = 2)
Wu


pre_period <- subset(df, Period=="Pre")
pre <- ggscatter(pre_period, x = "BF", y = "peak_velocity",
                 add = "reg.line", fullrange = TRUE,                                # Add regression line
                 conf.int = TRUE,                                  # Add confidence interval
                 add.params = list(color = "blue",
                                   fill = "lightgray"))+ labs(y = "Peak Velocity (m/s)",
                                                              x = "Body Fat (%)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 7, label.y = 3.5,p.accuracy = 0.001, r.accuracy = 0.01)  # Add correlation coefficient
pre

post_period <- subset(df, Period=="Post")
post <- ggscatter(post_period, x = "BF", y = "peak_velocity",
                  add = "reg.line", fullrange = TRUE,                                # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "blue",
                                    fill = "lightgray"))+ labs(y = "Peak Velocity (m/s)",
                                                               x = "Body Fat (%)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 12, label.y = 3.5, p.accuracy = 0.001, r.accuracy = 0.01)  # Add correlation coefficient
post

post4_period <- subset(df, Period=="Post+4")
post4 <- ggscatter(post4_period, x = "BF", y = "peak_velocity",
                   add = "reg.line", fullrange = TRUE,                                # Add regression line
                   conf.int = TRUE,                                  # Add confidence interval
                   add.params = list(color = "blue",
                                     fill = "lightgray"))+ labs(y = "Peak Velocity (m/s)",
                                                                x = "Body Fat (%)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 12, label.y = 3.5, p.accuracy = 0.001, r.accuracy = 0.01)    # Add correlation coefficient
post4


ggarrange(Wu,                                                 # First row with scatter plot
          ggarrange(pre, post, post4, ncol = 3, labels = c("B", "C", "D"),
                    label.y = 1.1), # Second row with box and dot plots
          nrow = 2,
          labels = "A")
ggsave("RMR_BFvsPV.png")



#Repeated measures correlation for FFM and Peak Velocity

repeated <- rmcorr(participant = ID,
                   measure1 = FM, measure2 = RSIm,
                   dataset = df,
                   CI.level = 0.95, CIs = c("bootstrap"),
                   nreps = 1000, bstrap.out = F)
repeated

##regression line for all RMC
null.vol <- lmer(peak_velocity ~ FM + (1| ID), data = df, REML = FALSE)


Wu <- ggplot(df, ggplot2::aes(x = FM, y = peak_velocity, group = factor(ID),
                              color = factor(ID))) +
  geom_point(ggplot2::aes(colour = factor(ID))) +
  geom_line(ggplot2::aes(y = repeated$model$fitted.values), linetype = 1) +
  xlab("Fat Mass (kg)") +
  ylab("Peak Velocity (m/s)") +
  theme_classic() +
  annotate("text", label = "RMC = 0.471 [0.276,0.675], p=0.012", x = 8, y = 3.5) +
  geom_abline(intercept = fixef(null.vol)[1], slope = fixef(null.vol)[2],
              colour = "black", size = 1, linetype = 2)
Wu


pre_period <- subset(df, Period=="Pre")
pre <- ggscatter(pre_period, x = "FM", y = "peak_velocity",
                 add = "reg.line", fullrange = TRUE,                                # Add regression line
                 conf.int = TRUE,                                  # Add confidence interval
                 add.params = list(color = "blue",
                                   fill = "lightgray"))+ labs(y = "Peak Velocity (m/s)",
                                                              x = "Fat Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 7, label.y = 3.5,p.accuracy = 0.001, r.accuracy = 0.01)  # Add correlation coefficient
pre

post_period <- subset(df, Period=="Post")
post <- ggscatter(post_period, x = "FM", y = "peak_velocity",
                  add = "reg.line", fullrange = TRUE,                                # Add regression line
                  conf.int = TRUE,                                  # Add confidence interval
                  add.params = list(color = "blue",
                                    fill = "lightgray"))+ labs(y = "Peak Velocity (m/s)",
                                                               x = "Fat Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 7, label.y = 3.5, p.accuracy = 0.001, r.accuracy = 0.01)  # Add correlation coefficient
post

post4_period <- subset(df, Period=="Post+4")
post4 <- ggscatter(post4_period, x = "FM", y = "peak_velocity",
                   add = "reg.line", fullrange = TRUE,                                # Add regression line
                   conf.int = TRUE,                                  # Add confidence interval
                   add.params = list(color = "blue",
                                     fill = "lightgray"))+ labs(y = "Peak Velocity (m/s)",
                                                                x = "Fat Mass (kg)") +
  stat_cor(method = "pearson",aes(label = paste(..r.label..,..p.label.., sep = "~`,`~")),
           label.x = 7, label.y = 3.5, p.accuracy = 0.001, r.accuracy = 0.01)    # Add correlation coefficient
post4


ggarrange(Wu,                                                 # First row with scatter plot
          ggarrange(pre, post, post4, ncol = 3, labels = c("B", "C", "D"),
                    label.y = 1.1), # Second row with box and dot plots
          nrow = 2,
          labels = "A")
ggsave("RMR_FMvsPV.png")