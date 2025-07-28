#' @title Convert Rmd Slide Show to Bookdown Compatible Text for Further Editing
#' @description Rmd Slide Show file is transformed to bookdown compatible text with no more than two spaces between words and limited to 80 characters per line.  Output appears in console for further 'copy and paste' operation.  Optionally, the transformed text can be stored in new Rmd file in the current working directory.  User supplies arguments for Rmd path/filename containing the slide deck, and the (optional) output path/filename.  Default values are "sample-slides.Rmd" and "sample-slides-to-bookdown.Rmd".  CAUTION - WHEN 'save_to_file' argument = TRUE, TWO NEW INTERIM FILES ARE SAVED IN THE CURRENT DIRECTORY:  rmd_file.pdf and rmd_file.html.
#'
#' Confirmation message of saved file appears in the console.
#'
#' @param rmd_file   An .Rmd path/filename provided as a character string.  Default value = "sample-slides.Rmd"
#' @param txt_output_file An Rmd path/filename provided as a character string.  Default value = "sample-slides-to-bookdown.Rmd"
#'
#'@param save_to_file A logical vector,provided as a character string, indicating user request to write a new file to current working directory.  Default value = "FALSE"
#'
#' @return  An R object named 'modified_text' which is (optionally) written to a new Rmd file, suitable for editing in bookdown.  New file is created in current working directory.
#'
#' @import renderthis
#' @import tools
#' @import pdftools
#'
#' @examples
#'
#' \dontrun{
#'    slides_to_bookdown(rmd_file) # prints modified text to console
#'    slides_to_bookdown("sample-slides.rmd", "sample-slides-to-bookdown.rmd", save_to_file = TRUE)
#'}
#'
#' @export
slides_to_bookdown <- function(rmd_file = "sample-slides.rmd", txt_output_file = "sample-slides-to-bookdown.rmd", save_to_file = FALSE) {
  # Generate the PDF output file
  pdf_output_file <- sub("\\.Rmd", ".pdf", rmd_file, ignore.case = TRUE)

  # Convert RMD to PDF
  renderthis::to_pdf(rmd_file, pdf_output_file, complex_slides = TRUE, partial_slides = TRUE)

  # Read text from PDF
  pdf_text <- pdftools::pdf_text(pdf_output_file)
  pdf_text <- paste(pdf_text, collapse = " ")

  # Replace multiple spaces with a single space
  modified_text <- gsub("\\s{2,}", " ", pdf_text)

  # Save to file if specified
  if (save_to_file) {
    writeLines(modified_text, txt_output_file)
    cat("Slide file has been converted into text and stored in an Rmd file with no more than two spaces between words in", txt_output_file, "\n")
  } else {
    cat(modified_text)
  }

  return(invisible(NULL))
}
