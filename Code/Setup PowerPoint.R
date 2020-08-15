# Set up PowerPoint
timestamp <- format(Sys.time(), format = "%Y-%m-%d")
pptx <- read_pptx("Template.new.pptx")
master <- "Office Theme"
slideNumber <- 1
pptxfileName <- paste0("Steve's COVID Analysis.", timestamp, ".pptx")
if (file.exists(pptxfileName))
  file.remove(pptxfileName)

while(file.exists(pptxfileName))
{
  cat("Close the open PowerPoint File\n")
  file.remove(pptxfileName)
}

slideNumber <- slideNumber + 2

