# Four Quadrant Graph
fourQPlot <- function(
  DATA,
  colX,
  colY,
  title,
  labelX,
  labelY,
  maxX,
  maxY,
  colors,
  scale, 
  caption = NULL
)
{
  
  # DATA <- States
  # colX <- "slopeCases"
  # colY <- "slopeDeaths"
  # title <- "Change in cases vs. change in deaths over last 14 days"
  # labelX <- "Change in cases (%/day)"
  # labelY <- "Change in deaths (%/day)"
  # max <- 6
  # colors <- c("red","forestgreen","blue","magenta")
  
  DATA$X <- plim(DATA[,colX], -maxX, maxX)
  DATA$Y <- plim(DATA[,colY], -maxY, maxY)
  DATA$scale <- scale
    
  DATA$Quadrant <- 1
  DATA$Quadrant[DATA$X < 0 & DATA$Y < 0] <- 2
  DATA$Quadrant[DATA$X > 0 & DATA$Y < 0] <- 3
  DATA$Quadrant[DATA$X < 0 & DATA$Y > 0] <- 4

  ggObject <- ggplot(
    DATA,
    aes(
      x = X, 
      y = Y, 
      label=Abbreviation, 
      size = scale, 
      color = as.factor(Quadrant)
      )
    ) +
    geom_text(show.legend=FALSE) +
    labs(
      title = paste("Change in", title, "as of", today),
      y = paste("Change in", labelY),
      x = paste("Change in", labelX),
      caption = caption
    ) +
    coord_cartesian(xlim = c(-maxX, maxX), ylim = c(-maxY,maxY)) +
    scale_color_manual(values = colors) +
    annotate(
      "segment",
      x = 0,
      xend = 0,
      y = -maxY,
      yend = maxY,
      color = "black"
    ) +
    annotate(
      "segment",
      y = 0,
      yend = 0,
      x = -maxX,
      xend = maxX,
      color = "black"
    )
  
  nextSlide(ggObject, paste("Change in", title))
  
  emailText <<- paste(
    emailText,
    email.list.start,
    paste(
      "The four quadrant map for change in",
      title,
      "."),
    sls_trim(
      add_ggplot(
        plot_object = ggObject,
        width = 7.2,
        height = 4,
        alt = NULL,
        align = "left",
        float = "none"
      )
    ),
    email.list.end
  )
}

