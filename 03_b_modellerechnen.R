library(tidyverse)
library(nlme)
library(psych)
library(readr)
library(janitor)
setwd("~/Uni Basel/3. Semester/Masterarbeit/B")
df <- readr::read_delim("LMM_Gesamtdaten_longformat.csv", delim = ";", na = "#NULL!" ) %>%
  clean_names()

df <- df %>%
  rename(id = messzeitpunkt, time = index1) %>%
  mutate(
    groupFct = factor(gruppe,
                      levels = c(0, 1),
                      labels = c("Kontrollgruppe", "Interventionsgruppe"))
  )

dfPcpts <- df %>%
  filter(!is.na(gruppe))

df_cleaned <- dfPcpts %>%
  filter(time %in% c(2,3), id != 29) %>%  # nur time 2 oder 3, bestehende ID-Filter
  group_by(id) %>%
  filter(any(!is.na(bai))) %>%           # behalten, wenn mindestens ein Wert vorhanden
  ungroup()

df_cleaned <- df_cleaned %>%
  mutate(time = factor(time)) %>%
  rename(group = groupFct)


########## Unconditional Means Model A ##########
model.a.ML <- lme(fixed = bai ~ 1, data = df_cleaned,
                  random= ~ 1 | id, method = "ML", na.action = "na.exclude")

summary(model.a.ML)
VarCorr(model.a.ML)

Variance <- VarCorr(model.a.ML)
VarIntercept <- as.numeric(Variance[[1]][1])  
VarResidual <- as.numeric(Variance[[2]][1])

# Ca. 46% der Varianz lässt sich durch interindividuelle Unterschiede erklären.
ICC <- VarIntercept / (VarIntercept + VarResidual)
ICC


########## Model B ##########

model.b.ML <- lme(fixed = bai ~ 1 + time, data = df_cleaned, random= ~ 1 | id, method = "ML", na.action = "na.exclude")
summary(model.b.ML)
VarCorr(model.b.ML)


########## Model C ##########

model.c.ML <- lme(fixed = bai ~ group * time, data = df_cleaned,
                  random= ~ 1 | id, method = "ML", na.action = "na.exclude")
summary(model.c.ML)
VarCorr(model.c.ML)

anova(model.a.ML, model.c.ML)




library(ggplot2)

# neues Datenset für Vorhersagen
newdat <- expand.grid(
  group = unique(df_cleaned$group),
  time  = unique(df_cleaned$time)
)

# Vorhersage nur auf Basis der Fixeffekte
newdat$pred <- predict(model.c.ML, newdat, level = 0)
df_cleaned$pred_ind <- predict(model.c.ML, level = 1)

ggplot(df_cleaned, aes(x = time, y = bai, color = group)) +
  geom_jitter(alpha = 0.6, width = 0.05) +
  scale_color_manual(values = c(
    "Kontrollgruppe" = "#4C78A8",
    "Interventionsgruppe" = "#F58518"
  )) +
  # Fixeffekt-Linien
  labs(
    x = "Messzeitpunkte",
    y = "BAI-Werte",
    color = "Gruppe"
  ) +
  geom_line(data = newdat,
            aes(x = time, y = pred, group = group),
            size = 1.2) +
  # Random Effect Linien pro Person, Farbe nach Gruppe
  geom_line(aes(y = pred_ind, group = id, color = group),
            alpha = 0.2, size = 0.5) +
  theme_minimal(base_size = 15) + 
  theme(
    legend.position = "top",
    strip.text = element_text(size = 16, face = "bold")   # Facet-Titelgröße
  )

ggsave(
  dpi = 300,
  width = 9,
  height = 6,
  filename = "03visuals/modell-c-plotted.png"
)

ggplot(df_cleaned, aes(x = time, y = bai, color = group)) +
  geom_jitter(alpha = .6, width = .1) +
  geom_line(data = newdat,
            aes(x = time, y = pred, group = group),
            size = 1.2) +
  theme_minimal(base_size = 15)



ggplot(newdat, aes(x = time, y = pred, color = group, group = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  ylim(0,20) +
  labs(
    x = "Zeit",
    y = "BAI (vorhergesagt)",
    color = "Gruppe"
  ) +
  theme_minimal(base_size=15)
