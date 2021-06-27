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

#Normality test (Shapiro-wilk)
library(rstatix)
Wushu %>% group_by(Period) %>% shapiro_test(JH)

# Repeated Measures Anova (within subjects)



#Plot
library(ggplot2)

# Descriptives
library(psych)
Wushu %>% describeBy(Period)