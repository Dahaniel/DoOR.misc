#' showDuplicates
#' 
#' look for duplicated odor identifiers and show them
#'
#' @param data 
#' @param check.column 
#' @param gsub 
#'
showDuplicates <- function(data, check.column = 'InChIKey', gsub = c('\\s|-|\\+|,|\\(|\\)|\\*|#')) {
  if (any(duplicated(na.omit(tolower(gsub(gsub,'',data[,check.column])), na.rm = T))))
  {
    duplicates <- na.omit(tolower(gsub(gsub,'',data[,check.column]))[which(duplicated(tolower(gsub(gsub,'',data[,check.column])), na.rm = T))])
    duplicates <- data[which(tolower(gsub(gsub,'',data[,check.column])) %in% duplicates),]
    duplicates <- duplicates[order(duplicates[,check.column]),]
    return(duplicates)
  } else message('No duplicates found!')
}