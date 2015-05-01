library("XML")

#loading files
current_files <- dir()
file_vec <- vector()
for (i in 1:length(current_files)) {
  file <- current_files[i]
  if (grepl(".xml", file)) {
    if (!(file %in% file_vec)) {
      file_vec <- c(file_vec, file)
    }
  }
}
ser_list <- list()
for (file in 1:length(file_vec)) {
  doc <- xmlTreeParse(file_vec[file])
  root <- xmlRoot(doc)
  #chart type
  chart_type <- names(doc$doc$children$chartSpace["chart"]$chart["plotArea"]$plotArea[2])
  
  #fetching series
  ser = xpathApply(root, "//c:ser/c:tx/c:strRef/c:strCache/c:pt/c:v")
  
  ser_vec = vector()
  for (i in 1:length(ser)) {
    ser_item <- toString.XMLNode(ser[[i]])
    ser_item <- gsub("<.*?>","", ser_item)
    ser_item <- gsub(" $","", ser_item, perl=T)
    ser_vec <- c(ser_vec, ser_item)
    
  }
  
  #fetching categories
  cat0 <- xpathApply(root, "//c:cat")
  cat_val <- names(cat0[[1]])
  if (cat_val == "c:strRef") {
    cat <- xpathApply(root, "//c:cat/c:strRef/c:strCache/c:pt/c:v")
  } else {
    cat <- xpathApply(root, "//c:cat/c:numRef/c:numCache/c:pt/c:v") 
  }
  
  
  
  cat_vec = vector()
  for (i in 1:length(cat)) {
    cat_item <- toString.XMLNode(cat[[i]])
    cat_item <- gsub("<.*?>","", cat_item)
    cat_item <- gsub(" $","", cat_item, perl=T)
    if (!(cat_item %in% cat_vec)) {
      cat_vec <- c(cat_vec, cat_item)
    } 
  }
  
  #fetching values
  value = xpathApply(root, "//c:ser/c:val/c:numRef/c:numCache/c:pt/c:v")
  
  val_vec = vector()
  for (i in 1:length(value)) {
    value_item <- toString.XMLNode(value[[i]])
    value_item <- gsub("<.*?>","", value_item)
    value_item <- gsub(" $","", value_item, perl=T)
    value_item <- as.numeric(value_item)
    val_vec <- c(val_vec, value_item)
  }
  
  #creating matrix
  final <- matrix(val_vec,nrow=length(cat_vec),ncol=length(ser_vec))
  
  rownames(final) <- cat_vec
  colnames(final) <- ser_vec
  
  #export as a csv file
  filename <- paste("done_", "chart", file, sep="")
  filename <- paste(filename, ".csv", sep="")
  write.csv(final, filename)
  
}



