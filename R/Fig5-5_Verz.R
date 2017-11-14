#########################
# Fig. 5.5 Verzierungen #
#########################

library(ggplot2)
library(ODB)
library(scales)
library(sqldf)

connection <- odb.open("data/CampoDB.odb")
scaleCol <- colorRampPalette(c("white", "red"), space = "rgb")(256)

sql <- 'SELECT
          "t_Obj"."ObjID",
          "t_Obj"."feature"
        FROM "t_Obj_pottery", "t_Obj"
        WHERE "t_Obj_pottery"."ObjID" = "t_Obj"."ObjID"'
df <- odb.read(connection, sql)

df_a <- read.csv("data/t_Obj_VerzPos.csv")
df_a <- sqldf('select * from df_a where Verz != ""')
df_merge <- merge(df, df_a, by = "ObjID")
write.csv(df_merge, file = "data/processed/ch5_VerzPos-Matrix_merge.csv")

sql <- 'SELECT
          "t_Obj"."ObjID",
          "t_Obj"."feature",
          "t_Obj"."Indiv",
          "t_Obj"."GE",
          "t_Obj"."box",
          "t_Obj_pottery"."form_vessel"
        FROM "t_Obj_pottery", "t_Obj"
        WHERE "t_Obj_pottery"."ObjID" = "t_Obj"."ObjID"'

df <- odb.read(connection, sql)

# nur GE mit bestimmter Gefäßform:
# df <- sqldf('select * from df where form_vessel != "NA"')
write.csv(df, file = "data/processed/ch5_VerzPos-Matrix_Suppl.csv")

# CA
df_a <- read.csv("data/t_Obj_VerzPos.csv")
df_a <- sqldf('select * from df_a where Verz != ""')

# Tabellen verbinden
df_merge <- merge(df, df_a, by = "ObjID")

df_pivot <- tapply(df_merge$ObjID, list(df_merge$Verz, df_merge$form_vessel), length)
df_pivot[is.na(df_pivot)] <- 0

# tapply() produziert eine Matrix mit 0er-Zeilen, daher Kreuztabelle in Pandas
write.csv(df_merge, file = "data/processed/ch5_VerzPos-Matrix_df_merge.csv")
df_pivot <- read.csv("data/processed/ch5_VerzPos-Matrix_pivot.csv", row.names=1)

# Anzahl Verzierungen: 
length(unique(df_a$Verz))

a <- data.frame(rowSums(df_pivot, na.rm = TRUE, dims = 1))
a <- data.matrix(a)

a <- data.frame(a)      # wieder in Dataframe umwandeln
names(a)[1] <- "Anzahl"
a$pr <- a$Anzahl / sum(a$Anzahl) *100
a <- a[order(-a$Anzahl), , drop = FALSE]

df_1 <- sqldf('select * from df_merge where feature = "07/1" OR feature = "07/2"')
df_2 <- sqldf('select * from df_merge where feature != "07/1" AND feature != "07/2" AND feature != "07/11" AND feature != "07/13" AND feature != "07/95" AND feature != "07/106" AND feature != "07/109" AND feature != "07/110"')
df_3 <- sqldf('select * from df_merge where feature = "07/13"')
df_4 <- sqldf('select * from df_merge where feature = "07/95" OR feature = "07/106" OR feature = "07/109" OR feature = "07/110"')
df_5 <- sqldf('select * from df_merge where feature = "07/11"')

df_1$site <- "02_Burials_Center"
df_2$site <- "03_Burials_Church"
df_3$site <- "04_Pit13_Church"
df_4$site <- "05_Survey"
df_5$site <- "01_Pit11_Church"

df_bind <- rbind(df_1, df_2, df_3, df_4, df_5)

df_bind_a <- tapply(df_bind$ObjID, list(df_bind$Verz, df_bind$site), length)

# barplot(df_bind_a, horiz = TRUE, beside=TRUE)

# Liste mit Werten für ggplot in Pandas
write.csv(df_bind, file = "data/processed/ch5_Verz_Sites.csv")
df_pivot <- read.csv("data/processed/ch5_Verz_Sites_pivot.csv", row.names=1)

df_pivot <- read.csv("data/processed/ch5_Verz_Sites_pivot_Prozent.csv")
#df_pivot$Verz2 <- factor(df_pivot$Verz, as.character(df_pivot$Verz))

# Sortierung im Plot von oben nach unten; siehe: http://stackoverflow.com/questions/7299440/order-of-legend-entries-in-ggplot2-barplots-with-coord-flip
df_pivot$Verz <- factor(
  df_pivot$Verz, 
  levels=rev(sort(unique(df_pivot$Verz))), 
  ordered=TRUE
)

ggplot(df_pivot, aes(x = Verz, y = Prozent)) + 
geom_bar(stat = "identity", colour="black", fill="grey", width=.8, size = 0.25) + 
facet_grid(. ~ site) + 
coord_flip() + 
theme_bw() +
theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
scale_x_discrete(name = "Decoration Elements") + 
scale_y_continuous(labels = percent_format())
ggsave(file="output/Fig5-5_Verz.pdf", width = 10, height = 6)