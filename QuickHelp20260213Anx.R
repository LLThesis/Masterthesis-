library(tidyverse)
library(psych)
library(lme4)
library(lmerTest)
library(clipr)

library(haven)

data1 <- haven::read_sav(file="1_Gesamtdaten_wideFormat_N38.sav")
data1 <- haven::zap_formats(data1)
data1 <- haven::zap_labels(data1)
data1 <- haven::zap_label(data1)
data1 <- haven::zap_widths(data1)
# Remove: NA in column "Gruppe", NA in BAI zu T2 UND T3, und BAI > 63
idx1 <- is.na(data1$Gruppe)
idx2 <- apply(data1[,c("BAI_T2", "BAI_T3")], 1, function(x) all(is.na(x)))
idx3 <- !is.na(data1$BAI_T2) & data1$BAI_T2 > 63
# idx4 = remove when BAI_T2 or BAI_T3 is missing
idx4 <- is.na(data1$BAI_T2) | is.na(data1$BAI_T3)
idx1234 <- idx1 | idx2 | idx3 | idx4
data <- data1[!idx1234,]
dim(data)

# Show all column names
cbind(colnames(data))

colnames(data)[c(3, 6:9)] <- c("geschlecht", "group", "bai_pre", "bai_mid", "bai_post")

data$group <- factor(
    data$group,
    levels = c(0, 1),
    labels = c("Control group", "Experimental group")
)
data$geschlecht <- ifelse(data$geschlecht == 2, "Weiblich", "Männlich")
# # Rename the BAI pre-intervention for clarity
# names(data)[names(data) == "bai_tot_pre"] <- "bai_pre"

table(data$group)

# ---------------------------------------------------------------
# Show trajectories of each individual across pre, mid, and post.

# Neu (2026-02-14)
dataIndiv <- data.frame(pre=data$bai_pre, mid=data$bai_mid, post=data$bai_post,
                        group=data$group, vpn=data$VPn)
# hlh = high at pre, low at mid, high at post
hlh <- function(vec=NULL) {
    if(any(is.na(vec))) {
        return(NA)
    } else {
        return(vec[1] > vec[2] & vec[2] < vec[3])
    }
}
# lhl = low at pre, high at mid, low at post
lhl <- function(vec=NULL) {
    if(any(is.na(vec))) {
        return(NA)
    } else {
        return(vec[1] < vec[2] & vec[2] > vec[3])
    }
}
# Execute the function hlh and lhl
dataIndiv$hlh <- apply(dataIndiv, 1, FUN=hlh)
dataIndiv$lhl <- apply(dataIndiv, 1, FUN=lhl)
# See the result, separated by group
(dataIndivExp <- dataIndiv[dataIndiv$group=="Experimental group",])
table(dataIndivExp$hlh)
table(dataIndivExp$lhl)
# Result for experimental group: 6 of 21 show hlh, 3 show lhl, 11 show something else than hlh or lhl, 1 missing due to no data at pre.
(dataIndivCon <- dataIndiv[dataIndiv$group=="Control group",])
table(dataIndivCon$hlh)
table(dataIndivCon$lhl)
# Result for control group: 6 of 9 show hlh, 1 shows lhl, the remaining 3 show something else than hlh or lhl.

dataPlotIndiv <- data.frame(BAI=c(data$bai_pre, data$bai_mid, data$bai_post),
                            time=rep(0:2, each=nrow(data)),
                            group=factor(rep(data$group, times=3)),
                            VPN=factor(rep(data$VPn, times=3)))

dataPlotIndiv <- dataPlotIndiv %>%
  mutate(
    gruppe = factor(
      group,
      levels = c("Experimental group","Control group"),
      labels = c("Experimentalgruppe", "Kontrollgruppe")
    )
  )

T1T2Data <- dataPlotIndiv %>%
  filter(time != 0) %>%


(indivTrajec <- 
ggplot(data=T1T2Data, aes(x=time, y=BAI, gruppe=VPN, color=VPN)) +
    geom_line() +
    scale_x_continuous(breaks = c(1,2), labels=c("T2", "T3")) +
    xlab(label="Messzeitpunkt") +
    ylab(label="BAI") +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=16),
        axis.title.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title.y = element_text(size=16),
        panel.border = element_rect(color="grey", fill=NA),
        strip.text.x = element_text(size = 16),
) +
    facet_wrap(~gruppe)) +
  theme_minimal(base_size=15)

# Save plot as png
#ggsave(filename="indivTrajec.png", plot = indivTrajec, path = "./", device = "png", width=9, height=7, units="in", dpi=300)

ggsave(
  dpi = 300,
  width = 9,
  height = 7,
  filename = "indivTrajec.png"
)








meanData <- dataPlotIndiv %>%
  group_by(gruppe, time) %>%
  summarise(
    mean_BAI = mean(BAI, na.rm = TRUE),
    .groups = "drop"
  )

(trajPlot <-
  ggplot(meanData, aes(x = time, y = mean_BAI, color = gruppe, group = gruppe)) +
  
  # Punkte für Mittelwerte
  geom_point(size = 3) +
  
  # T1 -> T2 gestrichelte Linie
  geom_line(
    data = subset(meanData, time %in% c(0,1)),
    linetype = "dashed",
    linewidth = 1
  ) +
  
  # T2 -> T3 durchgezogene Linie
  geom_line(
    data = subset(meanData, time %in% c(1,2)),
    linetype = "solid",
    linewidth = 1
  ) +
  
  scale_x_continuous(
    breaks = c(0,1,2),
    labels = c("T1","T2","T3")
  ) +
    scale_color_manual(
      name = "Gruppe",
      values = c(
        "Experimentalgruppe" = "#C0504D",
        "Kontrollgruppe" = "#9BBB59"
      )
    ) +
  
  xlab("Messzeitpunkt") +
  ylab("BAI")) +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.border = element_rect(color = "grey", fill = NA),
    strip.text.x = element_text(size = 16),
    legend.title = element_blank()
  ) +
  theme_minimal(base_size = 15)

ggsave(
  dpi = 300,
  width = 9,
  height = 7,
  filename = "GruppenMittelwerte.png"
)








# ---------------------------------------------------------------

# ---------------------------------------------------------------
# Show trajectories of both groups across pre, mid, and post.

dataPlot <- data %>%
    group_by(group) %>%
    dplyr::summarise(baiPre=mean(bai_pre, na.rm=TRUE),
                     baiMid=mean(bai_mid, na.rm=TRUE),
                     baiPost=mean(bai_post, na.rm=TRUE))

# Long format for plotting
dataPlot_long <- dataPlot %>%
    pivot_longer(cols = c(baiPre, baiMid, baiPost),
                 names_to = "Time",
                 values_to = "BAI")
dataPlot_long$Time <- factor(dataPlot_long$Time,
                             levels=c("baiPre", "baiMid", "baiPost"),
                             labels=c("pre", "mid", "post"))

(groupTrajec <- 
ggplot(data=dataPlot_long, aes(x=Time, y=BAI, color=group, group=group)) +
    geom_line() +
    # expand_limits(y=c(0, 12)) +
    xlab(label="Measurement timepoint") +
    ylab(label="BAI score") +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=16),
        axis.title.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title.y = element_text(size=16),
        panel.border = element_rect(color="grey", fill=NA),
        legend.text = element_text(size=14),
        legend.position = "top",
        legend.title = element_blank()))

# Save plot as png
ggsave(filename="groupTrajec.png", plot = groupTrajec, path = "/Users/mmiche/Desktop/TeachingClass/FS2026MA_OCD/LauraLauber/Erstversion/", device = "png", width=9, height=7, units="in", dpi=300)
# ---------------------------------------------------------------

# Mean, sd, etc., also correlations between pre, mid, and post

# Control group
psych::describe(x=data[data$group=="Control group",c("bai_pre", "bai_mid", "bai_post")])

# Correlation
cor(data[data$group=="Control group",c("bai_pre", "bai_mid", "bai_post")], use = "complete.obs")
# ---------------------

# Experimental group
psych::describe(x=data[data$group=="Experimental group",c("bai_pre", "bai_mid", "bai_post")])

# Correlation
cor(data[data$group=="Experimental group",c("bai_pre", "bai_mid", "bai_post")], use = "complete.obs")
# ---------------------------------------------------------------

# Linear mixed effects model (LMM)

# Add an ID column
data$subject <- 1:nrow(data)

# Long format for LME
data_long <- data %>%
    pivot_longer(cols = c(bai_mid, bai_post),
                 names_to = "Time",
                 values_to = "BAI") %>%
    mutate(
        Time = factor(Time, levels = c("bai_mid", "bai_post"), labels = c("Mid", "Post")),
        Group = factor(group),
        Subject = factor(subject)
    )

# LMM
model1 <- lmer(BAI ~ Time * Group + (1 | Subject), data = data_long)
summary(model1)
colMeans(data[data$group=="Experimental group",c("bai_mid", "bai_post")])
colMeans(data[data$group=="Control group",c("bai_mid", "bai_post")])
# Just show the fixed effects results:
(modelout <- coefficients(summary(model1)))

clipr::write_clip(content=modelout)

# p-value for the one-sided hypothesis
pt(q=-0.4342149, df=29, lower.tail = TRUE)
# p-value for the two-sided hypothesis
pt(q=-0.4342149, df=29, lower.tail = TRUE)*2



library(nlme)

model.a.ML <- nlme::lme(fixed = BAI ~ Time * Group, data = data_long,
                  random= ~ 1 | Subject, method = "REML", na.action = "na.exclude")
summary(model.a.ML)
VarCorr(model.a.ML)

Variance <- VarCorr(model.a.ML)
VarIntercept <- as.numeric(Variance[[1]][1])  
VarResidual <- as.numeric(Variance[[2]][1])

# Ca. 46% der Varianz lässt sich durch interindividuelle Unterschiede erklären.
ICC <- VarIntercept / (VarIntercept + VarResidual)
ICC

# ----------------------------------
# Visualize the most important component which is contained in the correlation coefficient, namely the pairwise distances of each value to its respective mean value:
# install.packages("correlatio")
library(correlatio)
# help(package="correlatio")

# Correlation between mid and post in the control group:
cor(dataIndivCon[,c("mid", "post")])
datMidPostCon <- dataIndivCon[,c("mid", "post")]
datMidPostCon$mid <- as.numeric(datMidPostCon$mid)
datMidPostCon$post <- as.numeric(datMidPostCon$post)
controlGroupResult <- correlatio::corrio(data=datMidPostCon)
# Resultat-Tabelle
controlGroupResult$dat
# Visualisierung
controlGroupResult$plot2

# # Save plot as png
# ggsave(filename="control10.png", plot = controlGroupResult$plot2, path = "/Users/mmiche/Desktop/TeachingClass/FS2026MA_OCD/CamilleRérat/Erstversion/plots/", device = "png", width=9, height=7, units="in", dpi=300)

# Correlation between mid and post in the experimental group:
cor(dataIndivExp[,c("mid", "post")])
datMidPostExp <- dataIndivExp[,c("mid", "post")]
datMidPostExp$mid <- as.numeric(datMidPostExp$mid)
datMidPostExp$post <- as.numeric(datMidPostExp$post)
experimentalGroupResult <- correlatio::corrio(data=datMidPostExp)
# Resultat-Tabelle
experimentalGroupResult$dat
# Visualisierung
experimentalGroupResult$plot2

# # Save plot as png
# ggsave(filename="experimental21.png", plot = experimentalGroupResult$plot2, path = "/Users/mmiche/Desktop/TeachingClass/FS2026MA_OCD/CamilleRérat/Erstversion/plots/", device = "png", width=9, height=7, units="in", dpi=300)

# ----------------------------------

# Add-on 2026-02-17, visualize part of what is analyzed in the dependent t-test

# >>> First, run script lines 1-20, then continue below ... <<<

###############################################################################
#                        HYPOTHESIS 1
################################################################################
# H1: Participants in the experimental group will show a significant reduction 
# in depressive symptoms (BDI) after the CBM-I training.

# Prepare sample for the analysis
table(data$group)
exp_data <- subset(data, group == "Experimental group")
colSums(is.na(exp_data[, c("bai_mid", "bai_post")])) # no missing data

colMeans(exp_data[,c("bai_mid", "bai_post")])
colMeans(data[data$group=="Control group",c("bai_mid", "bai_post")])

# Small sample size and normal distribution of the difference scores: paired 
# t-test can be implemented. Because of the exploratory nature of the analysis, 
# the t-test will be two-tailed
t_res <- t.test(
    exp_data$bai_post,
    exp_data$bai_mid,
    paired = TRUE,
    alternative = "two.sided"
)
t_res

# Visualization

library(rstatix)
# First, the data must be re-formatted:
dl <- tidyr::pivot_longer(data=exp_data[,c("id_pre", "bai_mid", "bai_post")], !id_pre, names_to = "group", values_to = "BAI")
dl$group <- factor(dl$group, levels=c("bai_mid", "bai_post"), labels = c("mid", "post"))
library(ggpubr)
depttestPlot <- 
    ggpaired(
        dl, x = "group", y = "BAI", color = "group", palette = "jco", 
        line.color = "gray", line.size = 0.4, ylim = c(0, 35)) +
    labs(x="Study timepoint", y="BAI score", title = "Experimental group")

# # Save plot as png
# ggsave(filename="depttestPlot.png", plot = depttestPlot, path = "/Users/mmiche/Desktop/TeachingClass/FS2026MA_OCD/CamilleRérat/Erstversion/", device = "png", width=8, height=7, units="in", dpi=300)
# --------------------------------------------------------------------