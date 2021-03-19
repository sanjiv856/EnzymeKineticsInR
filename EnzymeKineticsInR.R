library(tidyverse)
library(grid)
library(gridExtra)
library(propagate)
library(broom)

# Loading data

########## Protein Concentration ##########

ProteinConc = read_csv("./Protein/data/ProteinConcentrationOptimization.csv")
# ProteinConc = read_csv("ProteinConcentrationOptimization.csv")
df1 <- pivot_longer(ProteinConc, cols = "No Protein":"1400 nM", names_to = "Concentrations", 
                    values_to = "OD")

df1$Concentrations <- factor(df1$Concentrations,
                             levels = c("No Protein", "No NADH", "10 nM", "20 nM", "100 nM", "200 nM",
                                        "400 nM", "600 nM", "800 nM", "1000 nM", "1200 nM", "1400 nM"))

ProteinConcPlot <- ggplot(df1, aes(Time, OD, color= Concentrations, shape = Concentrations)) +
  # theme_bw() +
  geom_point(size = 2, stroke = 2) +
  scale_fill_viridis_d(option = "plasma") +
  # theme_classic() +
  # scale_colour_pander() +
  scale_shape_manual(name = "Concentrations", values=1:nlevels(as.factor(df1$Concentrations))) +
  # scale_color_manual(name = "Concentrations", values=1:nlevels(as.factor(df1$Concentrations))) +
  labs(x = "Time (minutes)", y = expression(paste("Absorbance (OD"["340"], ")", sep = "")),  
       title = "", tag = "A") +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 0.7, 0.1)) +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(size = 0.7, color = "black"),
        text = element_text(size = 24), 
        axis.text = element_text(angle=0, vjust=0.5, size=18, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_rect(fill = "grey85",
                                        size = 0.5, linetype = "solid"), 
        axis.ticks = element_line(size = 0.5, colour = "black"))

# ggsave(filename = "Protein/Figures/ProteinConcPlot.jpg", width = 30, height = 20, units = "cm", dpi = 600)

########## pH Optimization ##########

pHData = read_csv("./Protein/data/pHOptimization.csv")
# pHData = read_csv("pHOptimization.csv")

dfpHData <- pivot_longer(pHData, cols = "No Protein":"pH 8.5", names_to = "pH", 
                         values_to = "OD")

dfpHData$pH <- factor(dfpHData$pH,
                      levels = c("No Protein", "No NADH", "pH 3.0", "pH 4.0", 
                                 "pH 5.0", "pH 6.0", "pH 6.5", "pH 7.0", "pH 7.5", "pH 8.0", "pH 8.5"))

# pHPlot = ggplot(dfpHData, aes(Time, OD, color= pH, shape = pH)) +
#   theme_bw() +
#   geom_point(size = 1.5) +
#   theme_classic() +
#   scale_shape_manual(values=1:nlevels(as.factor(dfpHData$pH))) +
#   labs(x = "Time (minutes)", y = expression("OD"[340]), 
#        title = "pH optimization", tag = "B") +
#   theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
#         plot.title = element_text(hjust = 0.5),
#         axis.line = element_line(size = 0.7, color = "black"),
#         text = element_text(size = 16), 
#         axis.text = element_text(angle=0, vjust=0.5, size=16, face = "bold")) 

pHDataPlot <- ggplot(dfpHData, aes(Time, OD, color = pH, shape = pH)) +
  # theme_bw() +
  geom_point(size = 2, stroke = 1.5) +
  scale_fill_viridis_d(option = "plasma") +
  # theme_classic() +
  # scale_colour_pander() + 
  scale_shape_manual(name = "pH", values=1:nlevels(as.factor(dfpHData$pH))) +
  scale_color_manual(name = "pH", values= c(1, 2, 3, 4, 10, 6, 7, 8, 9, 11, 12)) +
  labs(x = "Time (minutes)", y = expression(paste("Absorbance (OD"["340"], ")", sep = "")),  
       title = "", tag = "B") +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 0.7, 0.1)) +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(size = 0.7, color = "black"),
        text = element_text(size = 24), 
        axis.text = element_text(angle=0, vjust=0.5, size=18, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_rect(fill = "grey85",
                                        size = 0.5, linetype = "solid"), 
        axis.ticks = element_line(size = 0.5, colour = "black"))

# ggsave(filename = "Protein/Figures/pHPlot.jpg", width = 30, height = 20, units = "cm", dpi = 600)

########## Temperature Optimization ##########

TempData = read_csv("./Protein/data/TempOptimization.csv")
# TempData = read_csv("TempOptimization.csv")       


dfTempData <- pivot_longer(TempData, cols = "No Protein":"80 C", names_to = "Temperature", 
                           values_to = "OD")

dfTempData$Temperature <- factor(dfTempData$Temperature,
                                 levels = c("No Protein", "No NADH", "25 C", "30 C", 
                                            "37 C", "40 C", "45 C", "50 C", "55 C", "60 C", "65 C", "70 C", "80 C"))

# TempDataPlot = ggplot(dfTempData, aes(Time, OD, color = Temperature, shape = Temperature)) +
#   theme_bw() +
#   geom_point(size = 1.5) +
#   theme_classic() +
#   scale_shape_manual(name = "Temperature (°C)", values=1:nlevels(as.factor(dfTempData$Temperature))) +
#   scale_color_manual(name = "Temperature (°C)", values=1:nlevels(as.factor(dfTempData$Temperature))) +
#   labs(x = "Time (minutes)", y = expression("OD"[340]), 
#        title = "Temperature optimization", tag = "C") +
#   theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
#         plot.title = element_text(hjust = 0.5),
#         axis.line = element_line(size = 0.7, color = "black"),
#         text = element_text(size = 16), 
#         axis.text = element_text(angle=0, vjust=0.5, size=16, face = "bold"))

TempDataPlot <- ggplot(dfTempData, aes(Time, OD, color = Temperature, shape = Temperature)) +
  # theme_bw() +
  geom_point(size = 2, stroke = 1.5) +
  scale_fill_viridis_d(option = "plasma") +
  # theme_classic() +
  # scale_colour_pander() + c(1, 2, 3, 4, 10, 6, 7, 8, 9)
  scale_shape_manual(name = "Temperature (°C)", values=1:nlevels(as.factor(dfTempData$Temperature))) +
  scale_color_manual(name = "Temperature (°C)", values=c(1, 2, 3, 4, 10, 6, 7, 8, 9, 11, 12, 14, 16)) +
  labs(x = "Time (minutes)", y = expression(paste("Absorbance (OD"["340"], ")", sep = "")),  
       title = "", tag = "C") +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 0.7, 0.1)) +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(size = 0.7, color = "black"),
        text = element_text(size = 24), 
        axis.text = element_text(angle=0, vjust=0.5, size=18, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_rect(fill = "grey85",
                                        size = 0.5, linetype = "solid"), 
        axis.ticks = element_line(size = 0.5, colour = "black"))
# guides(shape=guide_legend("my title"))
# guides(shape=guide_legend("my title"))
# , shape = "Temperature (°C)"

# ggsave(filename = "Protein/Figures/TempDataPlot.jpg", width = 30, height = 20, units = "cm", dpi = 600)

########## Substrate Concentration ##########

SubstrateConc = read_csv("./Protein/data/SubstrateOptimization.csv")

# SubstrateConc = read_csv("SubstrateOptimization.csv")

dfSubstrateConc <- pivot_longer(SubstrateConc, cols = "No Protein":"1600", names_to = "Concentrations", 
                                values_to = "OD")

dfSubstrateConc$Concentrations <- factor(dfSubstrateConc$Concentrations,
                                         levels = c("No Protein", "No NADH", "25", "50", 
                                                    "100", "200", "400", "800", "1600"))

SubstrateConcPlot = ggplot(dfSubstrateConc, aes(Time, OD,  color = Concentrations, shape = Concentrations)) +
  geom_point(size = 2, stroke = 1.5) +
  scale_fill_viridis_d(option = "plasma") +
  # theme_classic() +
  # scale_colour_pander() + c(1, 2, 3, 4, 10, 6, 7, 8, 9)
  scale_shape_manual(name = "Substrate (μM)", values=1:nlevels(as.factor(dfSubstrateConc$Concentrations))) +
  scale_color_manual(name = "Substrate (μM)", values=c(1, 2, 3, 4, 10, 6, 7, 8, 9)) +
  labs(x = "Time (minutes)", y = expression(paste("Absorbance (OD"["340"], ")", sep = "")),  
       title = "", tag = "D") +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 0.7, 0.1)) +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(size = 0.7, color = "black"),
        text = element_text(size = 24), 
        axis.text = element_text(angle=0, vjust=0.5, size=18, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_rect(fill = "grey85",
                                        size = 0.5, linetype = "solid"), 
        axis.ticks = element_line(size = 0.5, colour = "black"))
# theme_bw() +
# geom_point(size = 1.5) +
# theme_classic() +
# scale_shape_manual(name = "Substrate (μM)", values=1:nlevels(as.factor(dfSubstrateConc$Concentrations))) + 
# scale_color_manual(name = "Substrate (μM)", values=1:nlevels(as.factor(dfSubstrateConc$Concentrations))) +
# labs(x = "Time (minutes)", y = expression("OD"[340]), 
#      title = "Substrate concentration", tag = "D") +
# theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
#       plot.title = element_text(hjust = 0.5),
#       axis.line = element_line(size = 0.7, color = "black"),
#       text = element_text(size = 16), 
#       axis.text = element_text(angle=0, vjust=0.5, size=16, face = "bold")) 

# ggsave(filename = "Protein/Figures/SubstrateConcPlot.jpg", width = 30, height = 20, units = "cm", dpi = 600)

allplots <- grid.arrange(ProteinConcPlot, pHDataPlot, TempDataPlot, SubstrateConcPlot, nrow = 2)

ggsave("Protein/Figures/AllPlots.jpg", allplots, width = 60, height = 40, units = "cm", dpi = 600)

########### SLOPE ###########

df = read_csv("./Protein/data/SlopeNoSignTest.csv")

ggplot(df) +
  geom_point(aes(x=S, y = v)) +
  theme_bw() +
  xlab("Substrate (µM)") +
  ylab("Velocity (nmol/min)")

MMformula <-formula(v ~ Vmax * S/(Km+S))
MMmodel <- nls(MMformula, df, start = list(Vmax = 0.01, Km = 800))
summary(MMmodel)

pred <- predictNLS(MMmodel, newdata = data.frame(S = df$S))
pred$summary
# Add the predicted values to the dataframe and plot
df2 <- cbind(df, pred$summary)
ggplot(df2) + geom_point(aes(x = S, y = v), alpha = 0.6) +
  geom_line(aes(x=S, y = Prop.Mean.1), color = "red") +
  geom_line(aes(x=S, y=`Prop.97.5%`), linetype = 3) +
  geom_line(aes(x=S, y=`Prop.2.5%`), linetype = 3) +
  xlab("Substrate (µM)") +
  ylab("Velocity (µmol/min)")+
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(size = 0.7, color = "black"),
        text = element_text(size = 16), 
        axis.text = element_text(angle=0, vjust=0.5, size=16, face = "bold"), 
        panel.background = element_rect(fill = "grey85",
                                        size = 0.5, linetype = "solid"), 
        axis.ticks = element_line(size = 0.5, colour = "black")) +
  geom_text(aes(1250,0.0014, label = paste("Km =", round(coef(MMmodel)[2], 3), "µM")), size = 6) +
  geom_text(aes(1250,0.0010, label = paste("Vmax =", round(coef(MMmodel)[1], 3), "µmol/min")), size = 6) +
  geom_ribbon(aes(x = S, ymin = `Prop.2.5%`, ymax = `Prop.97.5%`), alpha = 0.2)

ggsave(filename = "Protein/Figures/ProteinSubstrateVmaxKm.jpg", width = 20, height = 20, units = "cm", dpi = 600)

##############################################################
# trying second one 
##############################################################
SubstrateConc20min <- SubstrateConc[0:20,]
tidy(lm(SubstrateConc20min$`25` ~ SubstrateConc20min$Time))$estimate[2]
tidy(lm(SubstrateConc20min$`50` ~ SubstrateConc20min$Time))$estimate[2]
tidy(lm(SubstrateConc20min$`100` ~ SubstrateConc20min$Time))$estimate[2]
tidy(lm(SubstrateConc20min$`200` ~ SubstrateConc20min$Time))$estimate[2]
tidy(lm(SubstrateConc20min$`400` ~ SubstrateConc20min$Time))$estimate[2]
tidy(lm(SubstrateConc20min$`800` ~ SubstrateConc20min$Time))$estimate[2]
tidy(lm(SubstrateConc20min$`1600` ~ SubstrateConc20min$Time))$estimate[2]

df = read_csv("./Protein/data/SlopeNoSignTest_2.csv")

# ggplot(df) +
#   geom_point(aes(x=S, y = v)) +
#   theme_bw() +
#   xlab("Substrate (µM)") +
#   ylab("Velocity (nmol/min)")

MMformula <-formula(v ~ Vmax * S/(Km+S))
MMmodel <- nls(MMformula, df, start = list(Vmax = 0.01, Km = 800))
summary(MMmodel)

pred <- predictNLS(MMmodel, newdata = data.frame(S = df$S))
pred$summary
# Add the predicted values to the dataframe and plot
df2 <- cbind(df, pred$summary)
ggplot(df2) + geom_point(aes(x = S, y = v), alpha = 0.6) +
  geom_line(aes(x=S, y = Prop.Mean.1), color = "red") +
  geom_line(aes(x=S, y=`Prop.97.5%`), linetype = 3) +
  geom_line(aes(x=S, y=`Prop.2.5%`), linetype = 3) +
  xlab("Substrate (µM)") +
  ylab("Velocity (µmol/min)")+
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(size = 0.7, color = "black"),
        text = element_text(size = 16), 
        axis.text = element_text(angle=0, vjust=0.5, size=16, face = "bold"), 
        panel.background = element_rect(fill = "grey85",
                                        size = 0.5, linetype = "solid"), 
        axis.ticks = element_line(size = 0.5, colour = "black")) +
  geom_text(aes(1250,0.0014, label = paste("Km =", round(coef(MMmodel)[2], 3), "µM")), size = 6) +
  geom_text(aes(1250,0.0010, label = paste("Vmax =", round(coef(MMmodel)[1], 3), "µmol/min")), size = 6) +
  geom_ribbon(aes(x = S, ymin = `Prop.2.5%`, ymax = `Prop.97.5%`), alpha = 0.2)

ggsave(filename = "Protein/Figures/ProteinSubstrate_2_VmaxKm.jpg", width = 20, height = 20, units = "cm", dpi = 600)

##############################################################
# DONE TILL HERE


##############################################################
# for initial 5 minutes
##############################################################
# Ctrl + Alt + Shift + M to replace them all together 
SubstrateConc5min <- SubstrateConc[0:5,]
# Take the slope i.e. other than intercept, paste in excel as velocity against respective concentration
# lm(PecitnConc20min$`No Protein` ~ PecitnConc20min$Time)
tidy(lm(SubstrateConc5min$`No Protein` ~ SubstrateConc5min$Time))$estimate[2]
tidy(lm(SubstrateConc5min$`No NADH` ~ SubstrateConc5min$Time))$estimate[2]
tidy(lm(SubstrateConc5min$`25` ~ SubstrateConc5min$Time))$estimate[2]
tidy(lm(SubstrateConc5min$`50` ~ SubstrateConc5min$Time))$estimate[2]
tidy(lm(SubstrateConc5min$`100` ~ SubstrateConc5min$Time))$estimate[2]
tidy(lm(SubstrateConc5min$`200` ~ SubstrateConc5min$Time))$estimate[2]
tidy(lm(SubstrateConc5min$`400` ~ SubstrateConc5min$Time))$estimate[2]
tidy(lm(SubstrateConc5min$`800` ~ SubstrateConc5min$Time))$estimate[2]
tidy(lm(SubstrateConc5min$`1600` ~ SubstrateConc5min$Time))$estimate[2]

df5min = read_csv("./Protein/data/SlopeNoSignTest5min.csv")

ggplot(df5min) +
  geom_point(aes(x=S, y = v)) +
  theme_bw() +
  xlab("Substrate (µM)") +
  ylab("Velocity (nmol/min)")

MMformula <-formula(v ~ Vmax * S/(Km+S))
MMmodel <- nls(MMformula, df5min, start = list(Vmax = 0.01, Km = 800))
summary(MMmodel)

pred <- predictNLS(MMmodel, newdata = data.frame(S = df5min$S))
pred$summary
# Add the predicted values to the dataframe and plot
df2 <- cbind(df5min, pred$summary)
ggplot(df2) + geom_point(aes(x = S, y = v), alpha = 0.6) +
  geom_line(aes(x=S, y = Prop.Mean.1), color = "red") +
  geom_line(aes(x=S, y=`Prop.97.5%`), linetype = 3) +
  geom_line(aes(x=S, y=`Prop.2.5%`), linetype = 3) +
  xlab("Substrate (µM)") +
  ylab("Velocity (µmol/min)")+
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(size = 0.7, color = "black"),
        text = element_text(size = 16), 
        axis.text = element_text(angle=0, vjust=0.5, size=16, face = "bold"), 
        panel.background = element_rect(fill = "grey85",
                                        size = 0.5, linetype = "solid"), 
        axis.ticks = element_line(size = 0.5, colour = "black")) +
  geom_text(aes(1250,0.002, label = paste("Km =", round(coef(MMmodel)[2], 3), "µM")), size = 6) +
  geom_text(aes(1250,0.0010, label = paste("Vmax =", round(coef(MMmodel)[1], 3), "µmol/min")), size = 6) +
  geom_ribbon(aes(x = S, ymin = `Prop.2.5%`, ymax = `Prop.97.5%`), alpha = 0.2)

ggsave(filename = "Protein/Figures/ProteinSubstrate5minVmaxKm.jpg", width = 20, height = 20, units = "cm", dpi = 600)
