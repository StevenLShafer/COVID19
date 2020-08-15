library(curl)
library(RPushbullet)

testfn <- function()
{
browser()
pbPost(
#  channel = "",
  devices = "Phone",
  type = "note", 
  title = "Message from R", 
  body = "The most recent COVID files have been downloaded.",
  apikey = "o.Jb1UN5cEOOZnaZ7Tp3rsxf4vShe82xXy",
  debug=TRUE
)
}
testfn()
