

#' resize a image to match maximum width and heigh and keep ratio
#'
#' @param img_path A file path to the input image file
#' @param max_width A desired maximum image width, default 400 pix.
#' @param max_height A desired maximum image height, default 300 pix.
#'
#' @return a temporary file path to resized image.
#' @export
#'
#' @examples
#' # img_path <- './image/example.jpg'
#' # resized_img_path <- resize_image(img_path, max_width=640, max_heigh=480)

resize_image <- function(img_path,max_width=400,
                         max_height=300) {
  # resize he image to max_width* max_height size load image
  img_content <- magick::image_read(img_path)
  # Read image info
  info <- magick::image_info(img_content)

  # Calculate scaling factor (if needed)
  scale_factor <- ifelse(max(info$width, info$height) > max_width,
                         min(max_width / info$width, max_width / info$height),
                         1)

  # Generate resized image (if scaling needed)
  if (scale_factor != 1) {
    # Use appropriate library (e.g., magick, jpeg) for resizing
    resized_img_content <-
      img_content |>
      magick::image_resize(scale_factor * info$width)

    result <- list(src = resized_img_content,
                   contentType = 'image/jpep', # Adjust based on image format
                   width = paste0(scale_factor * info$width, "px"),
                   height = paste0(scale_factor * info$height, "px"),
                   alt = "This is an resized image")
  } else {
    # Return original image details if no scaling needed
    result <- list(
      src = img_content,
      contentType = 'image/jpep',
      # Adjust based on image format
      width = paste0(info$width, 'px'),
      height = paste0(info$height, 'px'),
      alt = "This is an original image"
    )
  }
  tmpfile <- result$src |> magick::image_write(tempfile(fileext = 'jpg'), format = 'jpg')
  return(tmpfile)
}
