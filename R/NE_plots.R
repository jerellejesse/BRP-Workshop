# CINAR BRP workshop report figures
# April 2025
# J.Jesse

library(here)
library(tidyverse)
library(ggrepel)
library(stringr)

##########################################
#########  NEW ENGLAND STOCKS ############
##########################################

##### Read in Data #####
NE <- read.csv(here("data/NE_table.csv"))
str(NE)
head(NE)
               
#### data cleaning ####
#Estimation method
unique(NE$Reference.point.Estimation.Method)

NE <- NE %>%
  mutate(Reference.point.Estimation.Method = if_else(str_starts(Reference.point.Estimation.Method, "Empirical"), "Empirical", Reference.point.Estimation.Method))%>%
  mutate(Reference.point.Estimation.Method = if_else(str_starts(Reference.point.Estimation.Method, "Exploitation"), "Empirical", Reference.point.Estimation.Method))%>%
  mutate(Reference.point.Estimation.Method = if_else(Reference.point.Estimation.Method== "", "None", Reference.point.Estimation.Method))

# Recruitment assumption
unique(NE$Recruitment.Assumption)
NE <- NE %>%
  mutate(Recruitment.Assumption = if_else(str_starts(Recruitment.Assumption, "recent"), str_replace(Recruitment.Assumption,"^recent", "Recent"), Recruitment.Assumption ))%>%
  mutate(Recruitment.Assumption = if_else(str_starts(Recruitment.Assumption, "Autoregressive"), "Autoregressive long-term average", Recruitment.Assumption ))%>%
  mutate(Recruitment.Assumption = if_else(Recruitment.Assumption== "", "None", Recruitment.Assumption))

#stock assessment model
unique(NE$Assessment.model)
NE <- NE %>%
  mutate(Assessment.model = if_else(str_starts(Assessment.model, "Index"), "Index-based", Assessment.model))%>%
  mutate(Assessment.model = if_else(str_starts(Assessment.model, "CASA"), "CASA", Assessment.model))%>%
  mutate(Assessment.model = if_else(Assessment.model== "", "None", Assessment.model))

# B/Bmsy and F/Fmsy
NE_filter <- NE %>%
  filter(Bcurrent.BMSY!= "Unknown")%>%
  filter(Bcurrent.BMSY != "TBD")
NE_filter$Bcurrent.BMSY <- as.numeric(gsub("%", "", NE_filter$Bcurrent.BMSY)) / 100
NE_filter$Fcurrent.FMSY <- as.numeric(gsub("%", "", NE_filter$Fcurrent.FMSY)) / 100


#### Plots ####
#estimation method
ggplot(NE)+geom_bar(aes(x=Reference.Point.Estimation.Method))+
  theme_minimal()+
  labs(x="Reference Point Estimation Method", title = "New England")+
  theme(text = element_text(size = 16))

#recruitment assumption
NE$Recruitment.Assumption <- str_wrap(NE$Recruitment.Assumption, width = 20)
ggplot(NE)+geom_bar(aes(x=Recruitment.Assumption))+
  theme_minimal()+
  labs(x="Recruitment Assumption", title = "New England")+
  theme(text = element_text(size = 16))

#assessment model
NE$Assessment.model <- str_wrap(NE$Assessment.model, width=15)
ggplot(NE)+geom_bar(aes(x=Assessment.model))+
  theme_minimal()+
  labs(x="Assessment Model", title = "New England")+
  theme(text = element_text(size = 16))


#kobe plot
#need to add cod and pollock has two numbers?
#skates only have F/Fmsy
NE_filter$label_wrapped <- str_wrap(NE_filter$Stock, width = 40)  # Adjust width to your preference

ggplot(NE_filter, aes(x = Bcurrent.BMSY, y = Fcurrent.FMSY)) +
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
  xlim(0, max(NE_filter$Bcurrent.BMSY, 2)) +
  ylim(0, max(NE_filter$Fcurrent.FMSY, 2)) +
  theme_minimal(base_size = 16)
