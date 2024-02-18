library(tidyverse)
library(jsonlite)
mylist = fromJSON("https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en")

#convert to DF and remove first element
mydf=bind_rows(mylist$result[-1])

#class of the date column (character)
class(mydf$date)

#list the first n rows of the mydf
head(mydf)
