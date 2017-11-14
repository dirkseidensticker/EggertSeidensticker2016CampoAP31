#######################################
# Tab 8.2.1 Datensatz labor Frankfurt #
#######################################

library(sqldf)
library(xlsx)

df <- read.csv("data/GeochemieDaten.csv", dec=",")

df_cam_ff <- sqldf('select ProbenNr AS "Lab. No.",
              FdSt AS Site, 
              Komplex AS Feature,
              Bef AS "Sample Location",
              T_cm AS "Depth (m)",
              P_p AS "Phosphor (%)",
              pH AS "pH",
              Cges_P AS "Total Carbon (%)",
              Corg_P AS "Organic Carbon (%)",
              orgSub AS "Organic Substances (%)"
        from df 
        where FdSt Like "%CAM%" 
              AND lab LIKE "%Frankfurt%"')

# Fpl-Name ändern
df_cam_ff$Site[df_cam_ff$Feature == "07/1"] <- "Campo-Center"
df_cam_ff$Site[df_cam_ff$Feature == "07/2"] <- "Campo-Center"

df_cam_ff$Site <- gsub("CAM", "Campo Chruch", df_cam_ff$Site)

# Tabelle schreiben
write.xlsx(df_cam_ff, "output/TabApp2-1_Geochem_FF.xlsx", col.names = TRUE, row.names = FALSE)
