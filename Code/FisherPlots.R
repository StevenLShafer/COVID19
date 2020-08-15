# Fisher Plots
internationalFisherPlot <- function (X, title, ylabel, emailTitle, N, OneIn = FALSE)
{ 
  X <- X[order(X$Y, decreasing = TRUE),]
  X <- X[1:65,]
  X$Rank <- 1:nrow(X)
  ggObject <- ggplot(X, aes(x = Rank, y = Y)) +
    geom_point() +
    geom_point(data=X[X$Abbreviation == "USA",], color = "red") +
    geom_text(aes(y = Y, label = Abbreviation), angle = 90, size = 2.6, hjust = -0.3) +
    geom_text(
      data=X[X$Abbreviation == "USA",], 
      aes(y = Y, label = Abbreviation), 
      color = "red", 
      angle = 90, size = 2.6, hjust = -0.3) +
    labs(
      title = title,
      y = ylabel,
      x = "Rank",
      caption = "Excludes countries with population < 5,000,000"
    ) +
    scale_x_continuous(
      limits = c(1, nrow(X)),
      breaks = c(0:13*5 + 1)
    )
  if (OneIn)
  {
    breaks <- pretty(X$Y)
    labels <- paste("1 in", prettyNum(round(1000000 / breaks, 0), big.mark = ",", scientific = FALSE))
    labels[labels== "1 in Inf"] <- "None"
    ggObject <- ggObject + scale_y_continuous(
      limits = c(0, max(X$Y) * 1.1),
      breaks = breaks, 
      labels = labels
    ) 
  } else {
    ggObject <- ggObject + scale_y_continuous(
      limits = c(0, max(X$Y) * 1.1),
      labels = scales::number_format(big.mark = ",", decimal.mark = '.')
    )
  }
  # +
  #   coord_cartesian(
  #     xlim = c(1, nrow(X))
  #   )
  nextSlide(ggObject, title)
  if (emailTitle != "") emailText <<- textRanksInternational(X, emailTitle, N)
}

stateFisherPlot <- function (X, title, ylabel, emailTitle, N, OneIn = FALSE)
{
  X <- X[order(X$Y, decreasing = TRUE),]
  X$Rank <- 1:nrow(X)
  pMasks <- p <- signif(wilcox.test(X$Rank[X$Masks == "Yes"], X$Rank[X$Masks == "No"])$p.value, 2)
  pGov <- signif(wilcox.test(X$Rank[X$Governor == "Republican"], X$Rank[X$Governor == "Democratic"])$p.value, 2)
  ggObject <- ggplot(X, aes(x = Rank, y = Y, color = Governor, shape = Masks)) +
    geom_point() +
    geom_text(aes(y = Y, label = Abbreviation), angle = 90, size = 2.6, hjust = -0.3) +
    scale_color_manual(values = c("blue","red")) +
    scale_shape_manual(values = c(0, 15)) +
    labs(
      title = title,
      y = ylabel,
      x = "Rank",
      caption = paste0
      (
        "p masks: ", 
        signif(pMasks,2), 
        ", p governor: ",
        signif(pGov,2),
        ". NB: association != causation."
      )
    ) +
    scale_x_continuous(
      limits = c(1, nrow(X)),
      breaks = c(0:10*5 + 1)
    )
  if (OneIn)
  {
    breaks <- pretty(c(0, max(X$Y)*1.1))
    labels <- paste("1 in", prettyNum(round(1000000 / breaks, 0), big.mark = ",", scientific = FALSE))
    labels[labels== "1 in Inf"] <- "None"
    ggObject <- ggObject + scale_y_continuous(
      limits = c(0, max(X$Y) * 1.1),
      breaks = breaks, 
      labels = labels
    ) 
  } else {
    ggObject <- ggObject + scale_y_continuous(
      limits = c(min(0, X$Y)*1.1, max(X$Y) * 1.1),
      labels = scales::number_format(big.mark = ",", decimal.mark = '.')
    )
    if (min(X$Y) < 0)
    {
      ggObject <- ggObject +
        annotate(
          "segment",
          y = 0,
          yend = 0,
          x = 1,
          xend = 51,
        )
    }
  }
  # +
  #   coord_cartesian(
  #     xlim = c(1, nrow(X))
  #   )
  nextSlide(ggObject, title)
  if (emailTitle != "") emailText <<- textRanksStates(X, emailTitle, N)
}
