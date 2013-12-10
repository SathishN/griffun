###############################################################################
#
# Copyright (C) 2013  Drew Griffith
#
# For more information please visit my blog at http://drewgriffith15.tumblr.com/
###############################################################################
#' Formatting R code snippets 
#'
#' Using code borrowed from Distantobserver who wrote a 
#' function that replaces the copy-and-paste part of the PrettyR webtool
#'
#' @param file filepath of R script
#' @keywords PrettyR Formatting
#' @export
#' @examples
#' ## NULL
prettyR  <- function (file) {
  require (RCurl)
  require (XML)
  Rcode <- readLines (file)
  Rcode <- paste (Rcode, collapse ="\n")
  # assemble the parameters for the http POST to the Pretty R web site
  URL <- "http://www.inside-r.org/pretty-r/tool"
  parameters <- list (
    op = "edit-submit", 
    form_id = "pretty_r_tool_form", 
    code_input = Rcode
 )
  # send the http POST request
  rawHTML <- postForm(URL, .params = parameters)
  parsedHTML <- htmlParse(rawHTML)
  
  # find the node
  prettified <- getNodeSet(parsedHTML, "//div[@class='form-item']/textarea")[[1]]
  prettified <- xmlValue(prettified[[1]])
  return (prettified)
}