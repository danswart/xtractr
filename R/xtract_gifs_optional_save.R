#' @title Extract GIF Media Calls Found in Rmd File, Transform to Rmarkdown \code{![]()} format and Write to Console; Optionally Write to Disk
#' @description All GIF media calls found in the specified Rmd file are transformed into Rmarkdown \code{![]()} syntax. The list is sent to the console as originally found and in the Rmarkdown \code{![]()} format. If `write_to_file` is `TRUE` (default = FALSE) and 'output_file' argument has a text value, the list is saved to a new file with the specified `output_file` name. If `write_to_file` is `FALSE`, no file is written.
#'
#' @param rmd_file   An .Rmd path/filename provided as a character string.
#' @param output_file   An .Rmd path/filename provided as a character string. File is saved in the current working directory.
#' @param write_to_file   A logical value indicating whether to write the result to a file (default = FALSE).
#'
#' @return  Character vector object named 'gifs-only'.
#'
#' @examples
#' rmd_file <- system.file("extdata", "sample-slides.rmd", package = "xtractr")
#' # Only print to console, no file is saved
#' xtract_gifs_optional_save(rmd_file)
#'#' # Save to a file with a custom filename "custom-output.rmd"
#'\dontrun{
#'xtract_gifs_optional_save(rmd_file, output_file = "custom-output.rmd", write_to_file = TRUE)
#'}
#' @export
xtract_gifs_optional_save <- function(rmd_file, output_file, write_to_file = FALSE) {

  # Get transformed media calls from the Rmarkdown file:
  transformed_calls <- xtractr::transform_media_calls(rmd_file)

  # Filter out .gif files and store them in gifs_only
  gifs_only <- transformed_calls[grepl("\\.gif", transformed_calls)]

  if (write_to_file) {
    # Write the gifs_only content to the output file if write_to_file is TRUE
    writeLines(gifs_only, con = output_file)
  }

  # Print the gifs_only content to the console
  cat(gifs_only, sep = "\n")

  # Return the transformed media calls
  return(gifs_only)
}
