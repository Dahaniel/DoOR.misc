#' getIdentifierDoOR
#' 
#' lookup chemical identifiers using the webchem package
#'
#' @param query 
#' @param representation 
#' @param first 
#' @param verbose 
#' @param ... 
getIdentifierDoOR <- function(query, representation = 'stdinchikey', first = T, verbose = T, ...) {
  if(representation != 'cid')
  {
    result <- sapply(query, function(x) cir_query(identifier = x , representation = representation, first = first, verbose = verbose, ...))
    if (representation == 'stdinchikey') result <- gsub('InChIKey=','',result, ignore.case = T)
    #if (representation == 'stdinchi') result <- gsub('InChI=','',result) 
  } else {  
    result <- sapply(query, function(x) get_cid(query = x, first = first, verbose = verbose, ...))
  }
  return(result)
}