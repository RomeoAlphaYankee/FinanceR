# Load the XLConnect package
library(XLConnect)

# Set the local file path
sheet <- file.path("~", "DataFile", "R_Files", "data", "example.xlsx")

# or download sample data
# download.file("https://miraisolutions.files.wordpress.com/2013/01/example.xlsx", destfile = "example.xlsx")

# Load the workbook
wb <- loadWorkbook("example.xlsx")

# Find the tab names
getSheets(wb)

# Read the data on the sheet named "data"
data <- readWorksheet(wb, sheet = "data")

# Explore the worksheet
str(data)
dim(data)
class(data)

head(data, 10)

# Lots of empty cells, in addition to missing data represented as '999999'

# Set the missing value identifier
setMissingValue(wb, value = 999999)

data <- readWorksheet(wb, sheet = "data", startRow = 7, startCol = 7,
                      endRow = 47, endCol = 25,
                      useCachedValues = TRUE)

head(data)

tail(data)

# Edit the data
data$complex <- NULL

# Explore the data
library(tidyverse)
data %>% group_by(cyl) %>% summarize(mean(wt))

data %>% group_by(cyl) %>% summarize(mean(mpg))

# Plot the data
data %>%  group_by(cyl) %>% ggplot(aes(x = wt, y = mpg)) + geom_point(aes(color = cyl))

# manipulate the data
data$mpg_wt <- data$mpg / data$wt
data$mpg_hp <- data$mpg / data$hp

data %>% ggplot(aes(x = mpg_wt, y = mpg_hp)) + geom_point(aes(color = cyl))

# Create a new sheet with the cleaned and edited data
createSheet(wb, "sheet2")
getSheets(wb)

renameSheet(wb, "sheet2", "clean")
getSheets(wb)

writeWorksheet(wb, data, "clean")
getSheets(wb)

# Save the workbook
saveWorkbook(wb)

# Alternatively, write R data object to a new Excel workbook
writeWorksheetToFile("~/DataFile/sample.xlsx", returns3, "sheet1")
