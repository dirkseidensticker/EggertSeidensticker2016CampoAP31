###########################
# Fig. 5.2 Fragmentierung #
###########################

library(ODB)

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

# Gräber
df_a <- sqldf('select * from df where feature NOT LIKE "%07/11%" 
              AND feature NOT LIKE "%07/13%"
              AND feature NOT LIKE "%07/95%"
              AND feature NOT LIKE "%05/101%"
              AND feature NOT LIKE "%07/106%"
              AND feature NOT LIKE "%07/109%"')
df_a_pivot <- tapply(df_a$n, list(df_a$feature, df_a$size), length)
a = colSums (df_a_pivot, na.rm = TRUE, dims = 1)

# Gruben
df_b <- sqldf('select * from df where feature LIKE "%07/11" 
              OR feature LIKE "%07/13%"')
df_b_pivot <- tapply(df_b$n, list(df_b$feature, df_b$size), length)
b = colSums (df_b_pivot, na.rm = TRUE, dims = 1)

# Survey
df_d <- sqldf('select * from df where feature LIKE "%05/101%"
              OR feature LIKE "%07/106%"
              OR feature LIKE "%07/109%"')
df_d_pivot <- tapply(df_d$n, list(df_d$feature, df_d$size), length)
d = colSums (df_d_pivot, na.rm = TRUE, dims = 1)

a <- data.frame(a)      # wieder in Dataframe umwandeln
names(a)[1] <- "Anzahl"
a$Prozent <- a$Anzahl / sum(a$Anzahl) *100
a

b <- data.frame(b)
names(b)[1] <- "Anzahl"
b$Prozent <- b$Anzahl / sum(b$Anzahl) *100
b

a$Type <- 'Burials'
a$Size <- row.names(a)
b$Type <- 'Pits'
b$Size <- row.names(b)
c <- rbind(a, b)
c

a$Size <- as.character(a$Size)
a$Size <- factor(a$Size, levels=unique(a$Size), ordered=TRUE)
b$Size <- as.character(b$Size)
b$Size <- factor(b$Size, levels=unique(b$Size), ordered=TRUE)

# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# 
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Plot
# ----

p1 <- ggplot(a, aes(x = Size, y = Anzahl)) + 
            geom_bar(stat = "identity", fill = "grey", colour = "black", size = .25) + 
            scale_x_discrete(name = "Size Classes") +
            scale_y_continuous(name = 'Frequency') +
            ggtitle("Burials\n") + 
            theme_bw() + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p2 <- ggplot(b, aes(x = Size, y = Anzahl)) + 
            geom_bar(stat = "identity", fill = "grey", colour = "black", size = .25) + 
            scale_x_discrete(name = "Size Classes") +
            scale_y_continuous(name = 'Frequency') +
            ggtitle("Pits\n") +
            theme_bw() + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

multiplot(p1, p2, cols=2)
dev.print(device = pdf, "output/Fig5-2_Fragmentierung.pdf", width = 10, height = 6)