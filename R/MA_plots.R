# CINAR BRP workshop report figures
# April 2025
# J.Jesse

library(here)
library(tidyverse)

###########################################
#########  MID-ATLANTIC STOCKS ############
###########################################

##### Read in Data #####
MA <- read.csv(here('data/MA_table.csv'))
str(MA)  
head(MA)

#### data cleaning ####
#Estimation method

unique(MA$Reference.Point.Estimation.Method)
MA <- MA %>%
  mutate(Reference.Point.Estimation.Method = if_else(str_starts(Reference.Point.Estimation.Method, "Used"), "MSE", Reference.Point.Estimation.Method))%>%
  mutate(Reference.Point.Estimation.Method = if_else(str_starts(Reference.Point.Estimation.Method, "Overfishing"), "Empirical", Reference.Point.Estimation.Method))

# Recruitment assumption
unique(MA$Recruitment.Assumption)
MA <- MA %>%
  mutate(Recruitment.Assumption = if_else(str_starts(Recruitment.Assumption, "Mean"), "Long-term average", Recruitment.Assumption))%>%
  mutate(Recruitment.Assumption = if_else(str_detect(Recruitment.Assumption, "Average recruitment over full"), "Long-term average", Recruitment.Assumption))%>%
  mutate(Recruitment.Assumption = if_else(str_starts(Recruitment.Assumption, "Median"), "Long-term median", Recruitment.Assumption))%>%
  mutate(Recruitment.Assumption = if_else(str_detect(Recruitment.Assumption, "2000-2019"), "Recent resampling", Recruitment.Assumption))%>%
  mutate(Recruitment.Assumption = if_else(str_detect(Recruitment.Assumption, "1971-2020"), "Long-term resampling", Recruitment.Assumption))%>%
  mutate(Recruitment.Assumption = if_else(str_detect(Recruitment.Assumption, "1975-2016"), "Long-term resampling", Recruitment.Assumption))%>%
  mutate(Recruitment.Assumption = if_else(str_starts(Recruitment.Assumption, "Average recent"), "Recent average", Recruitment.Assumption))%>%
  mutate(Recruitment.Assumption = if_else(str_starts(Recruitment.Assumption, "Conducted"), "MSE with SRR", Recruitment.Assumption))%>%
  mutate(Recruitment.Assumption = if_else(Recruitment.Assumption== "", "MSE with SRR", Recruitment.Assumption))%>%
  mutate(Recruitment.Assumption = if_else(str_starts(Recruitment.Assumption, "Survival"), "Survival SRR", Recruitment.Assumption))
  

#stock assessment method
unique(MA$Assessment.model)
MA <- MA %>%
  mutate(Assessment.model = if_else(str_detect(Assessment.model, "Swept"), "Swept Area Biomass", Assessment.model))%>%
  mutate(Assessment.model = if_else(str_starts(Assessment.model, "Stock"), "Stock Synthesis", Assessment.model))


# B/Bmsy and F/Fmsy
MA_filter <- MA %>%
  filter(Bcurrent.BMSY!= "Unknown")
MA_filter$Bcurrent.BMSY <- as.numeric(gsub("%", "", MA_filter$Bcurrent.BMSY)) / 100
MA_filter$Fcurrent.FMSY <- as.numeric(gsub("%", "", MA_filter$Fcurrent.FMSY)) / 100

#### Plots ####
#estimation method
MA$Reference.Point.Estimation.Method <- str_wrap(MA$Reference.Point.Estimation.Method, width = 15)
ggplot(MA)+geom_bar(aes(x=Reference.Point.Estimation.Method))+
  theme_minimal()+
  labs(x="Reference Point Estimation Method", title = "Mid-Atlantic")+
  theme(text = element_text(size = 16))

#recruitment assumption
MA$Recruitment.Assumption <- str_wrap(MA$Recruitment.Assumption, width = 15)
ggplot(MA)+geom_bar(aes(x=Recruitment.Assumption))+
  theme_minimal()+
  labs(x="Recruitment Assumption", title = "Mid-Atlantic")+
  theme(text = element_text(size = 16))

#assessment model
ggplot(MA)+geom_bar(aes(x=Assessment.model))+
  theme_minimal()+
  labs(x="Assessment Model", title = "Mid-Atlantic")+
  theme(text = element_text(size = 16))

# kobe plot
#longfin squid does not have b/bmsy
MA_filter$label_wrapped <- str_wrap(MA_filter$Stock, width = 40)  # Adjust width to your preference

ggplot(MA_filter, aes(x = Bcurrent.BMSY, y = Fcurrent.FMSY)) +
  # Quadrants
  geom_rect(aes(xmin = 1, xmax = Inf, ymin = 0, ymax = 1), fill = "green", alpha = 0.2) +   # Healthy (green)
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = "yellow", alpha = 0.2) +     # Overfished
  geom_rect(aes(xmin = 1, xmax = Inf, ymin = 1, ymax = Inf), fill = "orange", alpha = 0.2) + # Overfishing
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 1, ymax = Inf), fill = "red", alpha = 0.2) +      # Overfished + Overfishing
  
  # Reference lines
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  
  # Points
  geom_point(size = 3, color = "black", fill = "blue", shape = 21) +
  geom_text_repel(aes(label = label_wrapped), vjust = -1, size = 4) +
  
  # Labels and theme
  labs(
    x = expression(B / B[MSY]),
    y = expression(F / F[MSY]),
    title = ""
  ) +
  xlim(0, max(MA_filter$Bcurrent.BMSY, 2)) +
  ylim(0, max(MA_filter$Fcurrent.FMSY, 2)) +
  theme_minimal(base_size = 16)

