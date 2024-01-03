#' @title Transforms All Media Calls in Rmd File to HTML Syntax
#' @description Prints all media calls found in Rmd file into the console as originally found and in HTML \code{img src="path/filename" height=>} format to be available for copy and paste operations.  Media calls in slides shows that are in any format are transformed and put into a format suitable to be copied into a bookdown document.
#'
#' @param rmd_file  An .Rmd path/filename provided as a character string.
#'
#' @return  Character vector prints in the console
#'
#' @examples
#' rmd_file = system.file("extdata", "sample-slides.rmd", package = "xtractr")
#' transform_media_calls(rmd_file)
#'
#' @export
transform_media_calls <- function(rmd_file) {
  # Extract media calls from the Rmarkdown file:
  media_calls <- xtractr::get_media_calls(rmd_file)

  # Transform media calls from 'url(...)' format to '<img src="..." height=200>'
  transformed_calls1 <- gsub("url\\((.*?)\\)", "<img src=\"\\1\" height=200>", media_calls)

  # Transform media calls from '![](...)' format to '<img src="..." height=200>'
  transformed_calls2 <- gsub("!\\[.*?\\]\\((.*?)\\)", "<img src=\"\\1\" height=200>", transformed_calls1)

  # Handle any remaining calls that are plain paths (e.g., 'path/filename')
  # Assuming these are direct links to images or other media
  transformed_calls3 <- ifelse(grepl("<img src=", transformed_calls2),
                               transformed_calls2,
                               paste0("<img src=\"", transformed_calls2, "\" height=200>"))

  # Print transformed_calls3 to console
  cat(transformed_calls3, sep = "\n")
}
