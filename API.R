install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

apidata <- GET("https://datacatalogapi.worldbank.org/ddhxext/ResourceDownload?resource_unique_id=0066476&version_id=")
raw <- rawToChar(apidata$content)
raw


data <- fromJSON(rawToChar(apidata$content))
View(data)
