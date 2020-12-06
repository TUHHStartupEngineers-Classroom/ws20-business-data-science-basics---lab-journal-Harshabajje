#install.packages(c("httr", "jsonlite"))

library(httr)
library(jsonlite)

# In our example, I will be working with the Open Notify API, which opens up data on various NASA projects.

res = GET("http://api.open-notify.org/astros.json")

res

data = fromJSON(rawToChar(res$content))
names(data)

data$people