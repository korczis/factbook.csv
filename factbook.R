library(data.table)
library(dplyr)

# Load codes for country names
loadCodes <- function(path = './codes.csv') {
  dt <- data.table(read.csv(path))
  
  split(dt, dt$Name)
}

# Load categories
loadCategories <- function(path = './categories.csv') {
  dt <- data.table(read.csv(path))
  
  lapply(split(dt, dt$Num), function(x) {
    name.sanitized <- as.character(x$Name)
    name.sanitized <- gsub("[^[:alnum:]]", ".", name.sanitized)
    name.sanitized <- gsub("[\\.]+", ".", name.sanitized)
    name.sanitized <- gsub("\\.$", "", name.sanitized)
    name.sanitized <- tolower(name.sanitized)

    cbind(x, NameSanitized=name.sanitized)
  })
}

## Load and merge data
loadData <- function (dir = './data') {
  # Create empty list
  res <- list()
  
  # Iterate through all files and load them into list
  for(file in dir(dir)) {
    # Construct full path
    path <- file.path(dir, file)
    
    # Get just filename without extension
    k <- gsub("c", "", gsub(".csv", "", file))
    
    # Load CSV
    csv <- read.csv(path)
    
    # Add to list
    res[[k]] = data.table(csv)
  }
  
  # Return result
  res
}

loadFactbook <- function(dir = "./data") {
  # Load codes
  codes <- loadCodes(file.path(dir, "..", "codes.csv"))
  categories <- loadCategories(file.path(dir, "..", "categories.csv"))
  data <- loadData(dir)
  
  # Create data table with country names
  res <- data.table(Name = as.character(names(codes)))
  setkey(res, Name)

  for(category in categories) {
    dataset = data[[as.character(category$Num)]]
    setkey(dataset, Name)
    
    raw <- dataset[[3]]
    # indices <- !is.na(raw)
    
    # Convert factor values to numeric values
    if(class(raw) == "factor") {
      tmp <- as.character(raw)
      indices <- !is.na(tmp)
      tmp[indices] <- sapply(tmp[indices], function(x) {
        # gsub("[^[:alnum:] ]", "", str)
        x <- gsub("\\$", "", x)
        x <- gsub(",", "", x)
      })
      dataset[[3]] <- as.numeric(tmp)
    }

    names(dataset)[names(dataset) == "Value"] <- category$NameSanitized
    
    res <- merge(res, dataset[, c("Name", category$NameSanitized), with=FALSE], all = TRUE)
  }
  
  # Return result
  res
}

# Export factbook in one big CSV
exportFactbook <- function(data, file="factbook.csv") {
  write.table(data, file = file, sep = ",", col.names = NA, qmethod = "double")
}
