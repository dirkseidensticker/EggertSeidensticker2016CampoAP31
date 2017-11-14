###########################################################
# Fig. 8.2.1 Chemical analysis of soil samples from Campo #
###########################################################

library(ggplot2)
library(gridExtra)
library(sqldf)

df <- read.csv("data/GeochemieDaten.csv", dec=",")

df_cam_ff <- sqldf('select * from df where FdSt Like "%CAM%" AND lab LIKE "%Frankfurt%"')

# spliting data after types of features
df_cam_ff_graves <- sqldf('select * from df where FdSt Like "%CAM%" AND lab LIKE "%Frankfurt%" AND Bef LIKE "grave"')

df_cam_ff_bedrock <- sqldf('select * from df where FdSt Like "%CAM%" AND lab LIKE "%Frankfurt%" AND Bef LIKE "bedrock"')

df_cam_ff_pit <- sqldf('select * from df where FdSt Like "%CAM%" AND lab LIKE "%Frankfurt%" AND Bef LIKE "pit"')

# P
# -

graves = data.frame(x1 = df_cam_ff_graves$P_p)
graves$TYPE <- '1 graves'
bedrock = data.frame(x1 = df_cam_ff_bedrock$P_p)
bedrock$TYPE <- '3 bedrock'
pits = data.frame(x1 = df_cam_ff_pit$P_p)
pits$TYPE <- '2 pits'

all_p <- rbind(graves, bedrock, pits)

# pH
# --

graves = data.frame(x1 = df_cam_ff_graves$pH)
graves$TYPE <- '1 graves'
bedrock = data.frame(x1 = df_cam_ff_bedrock$pH)
bedrock$TYPE <- '3 bedrock'
pits = data.frame(x1 = df_cam_ff_pit$pH)
pits$TYPE <- '2 pits'

all_ph <- rbind(graves, bedrock, pits)

# C
# -

graves = data.frame(x1 = df_cam_ff_graves$Cges_p)
graves$TYPE <- '1 graves'
bedrock = data.frame(x1 = df_cam_ff_bedrock$Cges_p)
bedrock$TYPE <- '3 bedrock'
pits = data.frame(x1 = df_cam_ff_pit$Cges_p)
pits$TYPE <- '2 pits'

all_C <- rbind(graves, bedrock, pits)

# da die Sortierung sonst anders ist, als in den box-plot - hier eine neue Spalte erzeugen und Namen ändern

df_cam_ff$Bef2 <- df_cam_ff$Bef

df_cam_ff$Bef2 <- gsub("grave", "1 gave", df_cam_ff$Bef2)
df_cam_ff$Bef2 <- gsub("pit", "2 pit", df_cam_ff$Bef2)
df_cam_ff$Bef2 <- gsub("bedrock", "3 bedrock", df_cam_ff$Bef2)

# Plot
# ----

plot1 <- ggplot(all_p,aes(factor(TYPE), x1)) + 
  geom_boxplot(aes(fill = factor(TYPE))) + 
  theme_bw() + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") + 
  ylab("P (%)") + 
  ggtitle("Phosphor (%)\n") + 
  scale_x_discrete(labels=c("Grave", "Pit", "Control Group"))

plot2 <- ggplot(all_ph,aes(factor(TYPE), x1)) + 
  geom_boxplot(aes(fill = factor(TYPE))) + 
  theme_bw() + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") + 
  ylab("pH") + 
  ggtitle("pH\n") + 
  scale_x_discrete(labels=c("Grave", "Pit", "Control Group"))

plot3 <- ggplot(all_C,aes(factor(TYPE), x1)) + 
  geom_boxplot(aes(fill = factor(TYPE))) + 
  theme_bw() + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("") + 
  ylab("C (%)") + 
  ggtitle("Total Carbon (%)\n") + 
  scale_x_discrete(labels=c("Grave", "Pit", "Control Group"))

plot4 <- ggplot(df_cam_ff) + 
  geom_smooth(aes(x = T_cm, y = P_p), method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), fill = "grey80", colour="red", linetype = 2) + 
  geom_point(aes(x = T_cm, y = P_p, fill = Bef2), size = 5, pch = 21) + 
  theme_bw() + 
  xlab("Depth (m)") + 
  ylab("P (%)") + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot5 <- ggplot(df_cam_ff) + 
  geom_smooth(aes(x = T_cm, y = pH), method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), fill = "grey80", colour="red", linetype = 2) + 
  geom_point(aes(x = T_cm, y = pH, fill = Bef2), size = 5, pch = 21) + 
  theme_bw() + 
  xlab("Depth (m)") + 
  ylab("pH") + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot6 <- ggplot(df_cam_ff) + 
  geom_smooth(aes(x = T_cm, y = Cges_p), method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), fill = "grey80", colour="red", linetype = 2) + 
  geom_point(aes(x = T_cm, y = Cges_p, fill = Bef2), size = 5, pch = 21) + 
  theme_bw() + 
  xlab("Depth (m)") + 
  ylab("C (%)") + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())

g <- arrangeGrob(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3, nrow = 2)
ggsave(file = "output/FigApp2-1_Geochem_FF.pdf", g, width = 12, height = 8)
