#############################################################
# Fig. 8.2.2 Phosphor Analysis from Nkang (Mbida 1995/1996) #
#############################################################

library(ggplot2)

df_f9 <- read.csv("data/MbidaMindzie19951996/Appendix_F9_P-Gehalt.csv", dec=",")
df_f9$feature <- '1_F9'

# Anzahl Proben
tapply(df_f9$ID, list(df_f9$sample), length)

df_f13 <- read.csv("data/MbidaMindzie19951996/Appendix_F13_P-Gehalt.csv", dec=",")
df_f13$feature <- '2_F13'

df_f14 <- read.csv("data/MbidaMindzie19951996/Appendix_F14_P-Gehalt.csv", dec=",")
df_f14$feature <- '3_F14'

df_nkang <- rbind(df_f9, df_f13, df_f14)

df_nkang$T_cm <- df_nkang$T_cm / 100

# siehe http://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels
feature_names <- list(
  '1_F9'="Pit F9",
  '2_F13'="Pit F13",
  '3_F14'="Pit F14"
)
feature_labeller <- function(variable,value){
  return(feature_names[value])
}

# damit geom_smooth keine Funktion für die Proben aus dem Anstehenden ausgibt, diese aus dem Datensatz entfernen, der später genutzt wird:
df_nkang_pit <- sqldf('select * from df_nkang where sample Like "%pit%"')

# Plot
# ----
ggplot(df_nkang) + 
  geom_smooth(data = df_nkang_pit, aes(x = T_cm, y = P_mg.g, shape = sample), method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), fill = "grey80", colour="red", linetype = 2) + 
  geom_point(aes(x = T_cm, y = P_mg.g, fill = sample, pch = sample), size = 5) + 
  theme_bw() + 
  xlab("Depth (m)") + 
  ylab("P (mg/g)") + 
  theme(legend.position="bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.key = element_blank()) + 
  facet_grid(. ~ feature, scales = "free", labeller = feature_labeller) + 
  scale_shape_manual(values=c(23, 21), name="Sample", breaks=c("bedrock", "pit"), labels=c("Control Group", "Pit")) +   
  scale_fill_manual(values=c("#7294C8", "#00BA38"), name="Sample", labels=c("Control Group", "Pit"))
ggsave(file = "output/FigApp2-2_Nkang_P-values_Mbida19951996.pdf", width = 12, height = 5)