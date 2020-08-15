setwd("g:/projects/covid/powerpoints")
FILES <- dir()
for (date in as.Date("2020-01-22"):as.Date("2020-08-13"))
{
  X <- as.character(as.Date(date))
  OLD <- FILES[grep(X, FILES)]
  if (length(OLD) > 0)
  {
    
    NEWDIR <- paste0("g:/projects/covid/updates/",X)
    if (!file.exists(NEWDIR)) dir.create(NEWDIR)
    NEW <- paste0("g:/projects/covid/updates/",X,"/",OLD)
    file.rename(from = OLD, to = NEW)
  }
}
