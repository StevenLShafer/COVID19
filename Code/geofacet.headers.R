ggObject <- ggplot(Cases_Long[Cases_Long$Date >= today - 58 & Cases_Long$Date < today,], aes(Date, Cases)) +
  geom_line() +
  scale_x_date(
    breaks = c(today - 0:4 * 14)-1,
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    breaks = c(0,25,50,75, 100)
  ) +
  coord_cartesian(ylim = c(0,100)) +
  facet_geo(~ State, grid = "us_state_grid4") +
  labs(
    y = "Percent of Peak",
    title = "Daily Cases as a Percent of Peak Cases") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x=element_text(angle=70, hjust=1, size=7),
    axis.text.y=element_text(size=6),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
  ) 
print(ggObject)

p <-  ggObject
headerColors <- stateColors[as.numeric(States$signCase)]

g <- ggplot_gtable(ggplot_build(p))
strips <- which(grepl('strip', g$layout$name))
g$layout$name[strips]
panels <- which(grepl('panel', g$layout$name))
g$layout$name[panels]
for (i in strips)
  {
  #  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  label <- g$grobs[[i]]$grobs[[1]]$children[[2]]$children$GRID$label
  s <- which(States$Abbreviation == label)
  if (length(s) == 1)
  {
    cat("Label",i,label,s,"\n")
    g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- headerColors[s]
  }
  else(
    g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- NA
  )
}  

g$grobs[[3]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[7]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[8]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[10]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[11]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[16]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[17]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[18]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[19]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[24]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[25]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[26]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[27]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[34]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[35]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[41]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[42]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[49]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[50]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[51]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[57]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[58]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[59]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[60]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[65]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[66]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[67]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[72]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[74]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[79]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[80]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[81]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[84]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[86]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[87]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[88]]$children[[1]]$children[[1]]$gp$fill <- "white"
g$grobs[[89]]$children[[1]]$children[[1]]$gp$fill <- "white"

grid.draw(g)
