library(tidyverse)
library(readr)
library(ggplot2)
library(janitor)
library(lme4)


#### MIT 38 Fällen

# Daten einlesen
df <- readr::read_delim("1_Gesamtdaten_longFormat_N38.csv") %>%
  clean_names()

# Daten vorbereiten
df_midPost <- df %>%
  select(v_pn, zeit, bai, gruppe) %>%
  filter(zeit != 1) %>%
  mutate(
    bai = as.numeric(bai),
    zeit = factor(
      zeit,
      levels = c(2, 3),
      labels = c("T2", "T3")
    ),
    gruppe = factor(
      gruppe,
      levels = c(0, 1),
      labels = c("Kontrollgruppe", "Experimentalgruppe")
    )
  )

# LMM spezifizieren
model1 <- lmer(bai ~ zeit * gruppe + (1 | v_pn), data = df_midPost)
summary(model1)

# KI rechnen
set.seed(9)
confint.merMod(model1, method = 'boot', boot.type = 'basic', oldNames = F, nsim = 1000)

# Testvoraussetzungen
library(performance)

check_model(model1, 
            check = c("linearity", "homogeneity", "qq", "outliers"))

check_model(model1, check = "reqq")
check_model(model1, check = "pp_check")

# 1. Assign visual plot to a variable name:
assumptPlot1 <- check_model(model1, check =
                              c("linearity", "homogeneity", "qq", "outliers"))
assumptPlot2 <- check_model(model1, check = "reqq")
assumptPlot3 <- check_model(model1, check = "pp_check")
# 2. Use the plot command "again"
assumptPlot1Save <- plot(assumptPlot1)
assumptPlot2Save <- plot(assumptPlot2)
assumptPlot3Save <- plot(assumptPlot3)
# 3. Save the plot
ggsave(filename="assumptPlot1.png", plot = assumptPlot1Save, device = "png", width=10, height=10, units="in", dpi=300)
ggsave(filename="assumptPlot2.png", plot = assumptPlot2Save, device = "png", width=10, height=10, units="in", dpi=300)
ggsave(filename="assumptPlot3.png", plot = assumptPlot3Save, device = "png", width=10, height=10, units="in", dpi=300)


#### MIT 31 Fällen

# Daten einlesen
df2 <- readr::read_delim("1_Gesamtdaten_longFormat_N31.csv") %>%
  clean_names()

# Daten vorbereiten
df2_midPost <- df2 %>%
  select(v_pn, zeit, bai, gruppe) %>%
  filter(zeit != 1) %>%
  mutate(
    bai = as.numeric(bai),
    zeit = factor(
      zeit,
      levels = c(2, 3),
      labels = c("T2", "T3")
    ),
    gruppe = factor(
      gruppe,
      levels = c(0, 1),
      labels = c("Kontrollgruppe", "Experimentalgruppe")
    )
  )

# LMM spezifizieren
model2 <- lmer(bai ~ zeit * gruppe + (1 | v_pn), data = df2_midPost)
summary(model2)

# KI rechnen
set.seed(9)
confint.merMod(model2, method = 'boot', boot.type = 'basic', oldNames = F, nsim = 1000)

# Testvoraussetzungen
library(performance)

check_model(model2, 
            check = c("linearity", "homogeneity", "qq", "outliers"))

check_model(model2, check = "reqq")
check_model(model2, check = "pp_check")

# 1. Assign visual plot to a variable name:
assumptPlot1 <- check_model(model2, check =
                              c("linearity", "homogeneity", "qq", "outliers"))
assumptPlot2 <- check_model(model2, check = "reqq")
assumptPlot3 <- check_model(model2, check = "pp_check")
# 2. Use the plot command "again"
assumptPlot1Save <- plot(assumptPlot1)
assumptPlot2Save <- plot(assumptPlot2)
assumptPlot3Save <- plot(assumptPlot3)
# 3. Save the plot
ggsave(filename="assumptPlot1_mod2.png", plot = assumptPlot1Save, device = "png", width=10, height=10, units="in", dpi=300)
ggsave(filename="assumptPlot2_mod2.png", plot = assumptPlot2Save, device = "png", width=10, height=10, units="in", dpi=300)
ggsave(filename="assumptPlot3_mod2.png", plot = assumptPlot3Save, device = "png", width=10, height=10, units="in", dpi=300)




