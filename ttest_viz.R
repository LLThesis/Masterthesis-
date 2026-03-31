library(tidyverse)
library(ggplot2)
library(haven)

df <- read_sav("1_Gesamtdaten_longFormat_N38.sav")
#df_wide <- read_sav("1_Gesamtdaten_wideFormat_N38.sav")

filtered <- df %>%
  select(VPn, Gruppe, Zeit, BAI)

filtered <- filtered %>%
  filter(
    Gruppe != 0, 
    Zeit != 1
  ) 

data <- filtered %>%
  mutate(
    Zeit = factor(
      Zeit,
      levels = c(2, 3),
      labels = c("T2", "T3")
    )
  )

data %>%
  group_by(Zeit) %>%
  summarise(mean = mean(BAI, na.rm =T))

data_wide <- data %>%
  pivot_wider(
    id_cols = VPn,
    names_from = Zeit,
    values_from = BAI
  )

data_wide %>%
  summarise(
    meanT2 = mean(T2, na.rm =T), 
    meanT3 = mean(T3, na.rm =T),
    medianT2 = median(T3, na.rm =T), 
    medianT3 = median(T3, na.rm =T)
    )

t.test(data_wide$T2, data_wide$T3, paired = TRUE)

# Plot: Boxplot gesamt + gepaarte Linien
ggplot(data, aes(x = Zeit, y = BAI, color = Zeit, group = VPn)) +
  # Boxplots für T2 und T3
  geom_boxplot(aes(group = Zeit), alpha = 0.2, width = 0.3, fill = NA) +
  geom_line(alpha = 0.5, size = 0.8, color ="grey") +
  geom_point(size = 2) +
  scale_color_manual(values = c("T2" = "black", "T3" = "#0000FF")) +
  xlab("Messzeitpunkt") +
  ylab("BAI") +
  theme_minimal(base_size = 15) +
  theme(
    #legend.position = "none",
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.position = "bottom"
  )

ggsave(
  dpi = 300,
  width = 9,
  height = 7,
  filename = "ttestPlot.png"
)