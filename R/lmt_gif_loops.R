#' @title Limit GIF Loops
#'
#' @description This function takes the path/filename of a GIF, limits the number of loops, and saves the modified GIF to a new file specified by user.
#'
#' @param input_gif Path/filename of the input GIF.
#' @param output_gif Path/filename where the limited-loop GIF will be saved.
#' @param loop_limit (optional) The number of loops to limit the GIF to (default is 3).
#'
#' @return None (the function saves the modified GIF to a new output file).
#'
#' @import magick
#'
#' @examples
#' \dontrun{
#' # Limit the loops to 2 and save the result to a new file
#' lmt_gif_loops("gif/red-raincoat-in-crowd.gif", "gif/ltd-red-raincoat-in-crowd.gif", loop_limit = 2)
#' }
#'
#' @export
lmt_gif_loops <- function(input_gif, output_gif, loop_limit = 3) {
  # Read the input GIF
  gif <- magick::image_read(input_gif)

  # Limit the number of loops
  gif_limited <- magick::image_animate(gif, loop = loop_limit)

  # Write the limited-loop GIF to the specified output file
  magick::image_write(gif_limited, output_gif)
}
