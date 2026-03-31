library(tidyverse)
library(nlme)
library(ggplot2)
library(psych)

df <- readr::read_delim("../01data/LMM_Gesamtdaten_longformat.csv", delim = ";", na = "#NULL!" ) %>%
  clean_names()

df <- df %>%
  rename(id = messzeitpunkt, time = index1) %>%
  mutate(
    groupFct = factor(gruppe,
                      levels = c(0, 1),
                      labels = c("Kontrollgruppe", "Experimentalgruppe"))
  )

dfPcpts <- df %>%
  filter(!is.na(gruppe))


# Für T2-t3
# Ohne ausreißer
dfPcptsFilterd <- dfPcpts %>%
  filter(time %in% c(2,3), id != 29) %>%  # nur time 2 oder 3, bestehende ID-Filter
  group_by(id) %>%
  filter(any(!is.na(bai))) %>%           # behalten, wenn mindestens ein Wert vorhanden
  ungroup()


dfPcptsFilterd

unique(dfPcptsFilterd$id)

dfPcptsFilterd %>%
  group_by(groupFct) %>%
  summarise(n_ids = n_distinct(id))



ggplot(data = dfPcptsFilterd,aes(x = factor(time),y = bai , color = groupFct)) +
  geom_point(size = 3.5) +
  labs(
    x = "Messzeitpunkte (T2, T3)",
    y = "BAI-Werte",
    #title = "Empirical Growth Plots (zufällig gezogen)",
  ) +
  facet_wrap( ~ id) +
  geom_smooth(
    aes(group = id), # Wichtig, um separate Linien pro ID zu erhalten
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    lwd = 1,
    color = "grey30" # Hier die Farbe der Regressionsgeraden festlegen
  ) +
  coord_cartesian(ylim = c(0, 40)) +
  #scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_color_manual(
    values = c(
      #"Kontrollgruppe" = "#9BBB59",
      #"Interventionsgruppe" = "#C0504D"
      "Kontrollgruppe" = "#9BBB59",
      "Experimentalgruppe" = "#C0504D"
    ),
    name = "Gruppe"   # Legendentitel
  ) +
  theme_minimal(base_size = 15) +
  #theme_linedraw() +
  theme(
    axis.title.x = element_text(size = 14),  # überschreibt nur x-Achse
    legend.position = "bottom"               # neue Position
  )

ggsave(
  dpi = 300,
  width = 12,
  height = 9,
  filename = "new-indidivudal-growth-plot-t2t3.png"
)