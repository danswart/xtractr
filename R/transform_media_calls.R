#' @title Transforms All Media Calls in Rmd File to Rmarkdown \code{![]()} Syntax
#' @description Prints all media calls found in Rmd file into the console as originally found and in Rmarkdown \code{![]()} format to be available for copy and paste operations.
#'
#' @param rmd_file  An .Rmd path/filename provided as a character string.
#'
#' @return  Character vector prints in the console
#'
#' @examples
#' rmd_file = system.file("extdata", "sample-slides.rmd", package = "xtractr")
#' transform_media_calls(rmd_file)
#' @export
transform_media_calls <- function(rmd_file) {

  # Extract media calls from the Rmarkdown file:
  media_calls <- xtractr::get_media_calls(rmd_file)

  # Remove any 'url(' characters if present and format as ![](path)
  transformed_calls <- gsub("url\\((.*?)\\)", "![](\\1)", media_calls)

  # Remove 'url(' characters at the beginning of the string if present
  transformed_calls <- gsub("^url\\((.*?)$", "![](\\1)", transformed_calls)

  # Add ![]() format to media calls that don't have 'url(' prefix
  transformed_calls[!grepl("!\\[.*?\\]\\(.*?\\)", transformed_calls)] <-
    paste0("![](", transformed_calls[!grepl("!\\[.*?\\]\\(.*?\\)", transformed_calls)], ")")

  # Return the transformed media calls to console for further copy-paste operations
  return(transformed_calls)
}

