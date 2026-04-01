library(tidyverse)
library(readr)
library(ggplot2)
library(janitor)
library(haven)

# Daten einlesen
df <- haven::read_sav("01data/1_Gesamtdaten_longFormat_N38.sav") %>%
  clean_names()


df <- df %>%
  rename(messzeitpunkt = id) %>%
  mutate(
    groupFct = factor(gruppe,
                      levels = c(0, 1),
                      labels = c("Kontrollgruppe", "Experimentalgruppe"))
  )

# Nur Teilnehmer*innen
df_participated <- df %>%
  filter(!is.na(groupFct)) %>%
  mutate(
    geschlecht = factor(
      geschlecht,
      levels = c(1, 2),
      labels = c("männlich", "weiblich")
    ),
    zeit = factor(
      zeit,
      levels = c(1, 2, 3),
      labels = c("T1", "T2", "T3")
    )
  )

plot(df_participated$rti, df_participated$bai)

ggplot(df_participated, aes(x = alter, y = bai, color = geschlecht)) +
  geom_jitter(alpha = 0.7, size = 3, width = 0.2, height = 0) +
  ylim(0, 40) +
  xlim(19,60) +
  facet_wrap(~factor(zeit), nrow = 1) +
  scale_color_manual(values = c(
    "männlich" = "#0000FF",
    "weiblich" = "black"
  )) +
  labs(color = "Geschlecht") +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "bottom"
  )

ggsave(
  dpi = 300,
  width = 11,
  height = 6,
  filename = "03visuals/new-alter-x-bai-v2.png"
)





# Abbildung 3

ggplot(df_participated, aes(x = rti, y = bai)) +
  geom_point(alpha = 0.6, size = 3, width = 0.2, height = 0) +
  facet_wrap(~factor(zeit), nrow = 3) +
  labs(color = "Geschlecht") +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "bottom"
  )

ggsave(
  dpi = 300,
  width = 9,
  height = 6.5,
  filename = "03visuals/new-rti_x_bai.png"
)

# Alternative Darstellung
ggplot(df_participated, aes(x = rti, y = bai, color = zeit)) +
  geom_point(alpha = 0.7, size = 3, width = 0.2, height = 0) +
  #facet_wrap(~factor(zeit), nrow = 1) +
  scale_color_manual(values = c(
    "T1" = "#0000FF",
    "T2" = "black",
    "T3" = "orange"
  )) +
  labs(color = "Geschlecht") +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "bottom"
  )

ggsave(
  dpi = 300,
  width = 11,
  height = 6,
  filename = "03visuals/new-alter-x-bai-v2.png"
)