####################
# Fig. 5.1 Keramik #
####################

library(ggplot2)
library(grid)
library(gridExtra)
library(ODB)
library(sqldf)

connection <- odb.open("data/CampoDB.odb")

sql <- 'SELECT
          "t_Obj"."ObjID",
          "t_Obj"."feature",
          "t_Obj"."material",
          "t_Obj"."GE",
          "t_Obj"."n",
          "t_Obj"."type",
          "t_Obj"."weight",
          "t_Obj_pottery"."size"
        FROM "t_Obj_pottery", "t_Obj"
        WHERE "t_Obj_pottery"."ObjID" = "t_Obj"."ObjID"'

df <- odb.read(connection, sql)

# Gäber an der Kirche
# -------------------
a1 <- sqldf('select * from df where weight != ""
            AND feature NOT LIKE "%07/11%" 
            AND feature NOT LIKE "%07/13%"
            AND feature NOT LIKE "%07/95%"
            AND feature NOT LIKE "%05/101%"
            AND feature NOT LIKE "%07/106%"
            AND feature NOT LIKE "%07/109%"
            AND feature NOT LIKE "%07/14"')

a1_pivot <- tapply(a1$weight, list(a1$type), sum, na.rm = TRUE)
a1_pivot <- data.frame(a1_pivot)
names(a1_pivot)[1] <- "Gewicht"
a1_pivot$GewProzent <- a1_pivot$Gewicht / sum(a1_pivot$Gewicht) * 100

# Daten vorbereiten
a1_pivot$Type <- row.names(a1_pivot)
a1_pivot <- a1_pivot[order(a1_pivot$Type),]
a1_pivot$Type <- as.character(a1_pivot$Type)
a1_pivot$Type <- factor(a1_pivot$Type, levels = unique(a1_pivot$Type), ordered = TRUE)

levels(a1_pivot$Type)[levels(a1_pivot$Type)=="B"] <- "Bases"
levels(a1_pivot$Type)[levels(a1_pivot$Type)=="R"] <- "Rims"
levels(a1_pivot$Type)[levels(a1_pivot$Type)=="W"] <- "Walls"
levels(a1_pivot$Type)[levels(a1_pivot$Type)=="G"] <- "Vessels"

# Gruben 11 & 13
# --------------
a2 <- sqldf('select * from df where weight != ""
            AND feature LIKE "%07/11" 
            OR feature  LIKE "%07/13"')

# a2_pivot <- tapply(a2$weight, list(a2$type), sum, na.rm = TRUE)
a2_pivot <- tapply(a2$ObjID, list(a2$type), length)
a2_pivot <- data.frame(a2_pivot)
names(a2_pivot)[1] <- "Gewicht"
a2_pivot$GewProzent <- a2_pivot$Gewicht / sum(a2_pivot$Gewicht) * 100

# Daten vorbereiten
a2_pivot$Type <- row.names(a2_pivot)
a2_pivot <- a2_pivot[order(a2_pivot$Type),]
a2_pivot$Type <- as.character(a2_pivot$Type)
a2_pivot$Type <- factor(a2_pivot$Type, levels = unique(a2_pivot$Type), ordered = TRUE)

levels(a2_pivot$Type)[levels(a2_pivot$Type)=="B"] <- "Bases"
levels(a2_pivot$Type)[levels(a2_pivot$Type)=="R"] <- "Rims"
levels(a2_pivot$Type)[levels(a2_pivot$Type)=="W"] <- "Walls"
levels(a2_pivot$Type)[levels(a2_pivot$Type)=="G"] <- "Vessels"

# Gewicht je Befund
# -----------------

b1 <- sqldf('select * from df where feature not like "05%"')

b1_pivot <- tapply(b1$weight, list(b1$feature, b1$type), sum)
b <- data.frame(rowSums (b1_pivot, na.rm = TRUE, dims = 1) / 1000)
names(b)[1] <- "Gewicht_kg"
b <- b[order(-b$Gewicht_kg), , drop = FALSE]
b$Feature <- row.names(b)
b <- data.frame(b)
b$Feature <- as.character(b$Feature)
b$Feature <- factor(b$Feature, levels = unique(b$Feature), ordered = TRUE)

p1 <- ggplot(data = a1_pivot, aes(x = factor(1), y = GewProzent, fill = factor(Type))) + 
            geom_bar(stat="identity", width = 1, colour = "white") + 
            coord_polar(theta = "y") + 
            theme_bw() + 
            theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) + 
            geom_text(aes(y = GewProzent/2 + c(0, cumsum(GewProzent)[-length(GewProzent)]), label = paste(round(GewProzent), "%")), size=4) + 
            scale_fill_grey() + 
            ggtitle("Burials [Weight]")

p2 <- ggplot(data = a2_pivot, aes(x = factor(1), y = GewProzent, fill = factor(Type))) + 
            geom_bar(stat="identity", width = 1, colour = "white") + 
            coord_polar(theta = "y") + 
            theme_bw() + 
            theme(legend.position="bottom", axis.title.x = element_blank(), axis.title.y = element_blank()) + 
            geom_text(aes(y = GewProzent/2 + c(0, cumsum(GewProzent)[-length(GewProzent)]), label = paste(round(GewProzent), "%")), size=4) + 
            scale_fill_grey() + 
            ggtitle("Pits [Quantitiy]")

p3 <- ggplot(b, aes(x = Feature, y = Gewicht_kg)) + 
            geom_bar(stat = "identity", colour="black", fill="grey", width=.6) + 
            coord_flip() +  
            theme_bw() + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            scale_y_continuous(name = 'Weight [kg]')

# http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization

# Move to a new page
grid.newpage()

# Create layout : nrow = 2, ncol = 2
pushViewport(viewport(layout = grid.layout(2, 2)))

# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

# Arrange the plots
print(p1, vp = define_region(1, 1))
print(p2, vp = define_region(2, 1))
print(p3, vp = define_region(1:2, 2))

dev.print(device = pdf, "output/Fig5-1_Keramik.pdf", width = 10, height = 10)


