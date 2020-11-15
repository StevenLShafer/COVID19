# Helper Functions

nextSlide <- function (ggObject, Title)
{
  suppressWarnings(
    print(ggObject)
  )
  
  pptx <- add_slide(pptx, layout = "Title and Content", master = master)
  pptx <- ph_with(pptx, value = Title, location = ph_location_type("title"))
  suppressWarnings(
    pptx <<- ph_with(pptx, value = dml(ggobj = ggObject), location = ph_location_type("body"))
  )
  pptx <<- ph_with(pptx, value = slideNumber, location = ph_location_type("sldNum"))
  slideNumber <<- slideNumber + 1
}

closest <- function(a, b)
{
  which(abs(a-b) == min(abs(a-b), na.rm=TRUE))[1]
}

Gompertz_fn <- function(par, N)
{
  # par <- intercept, peak, k
  return(par[1] + (par[2] - par[1]) * (1-exp(-par[3] * 0:(N-1))))
}

Gompertz_obj <- function(par, Y, weight)
{
  return(
    sum(
      (Y-Gompertz_fn(par, length(Y)))^2 * (1:length(Y))^weight
    )
  )
}

Gompertz_fit <-  function(Y, maxCases, weight = 1)
{
  Y1 <- tail(Y,5)
  X1 <- 1:5
  slope <- lm(Y1 ~ X1)$coefficients[2]
  return(
    optim(
      c(Y[1],Y[1] + 2, slope),
      Gompertz_obj,
      Y = Y,
      weight = weight,
      method = "L-BFGS-B",
      lower = c(Y[1] - 1, Y[1] - 1, 0.01),
      upper = c(Y[1] + 1, maxCases, 0.693)
    )$par
  )
}
