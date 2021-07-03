#' separate_single_artefacts
#'
#' A function to separate single artefacts from an image containing
#' multiple artefacts.
#'
#' @param inpath Pathname to the folder containing the JPEG
#' images with multiple artefacts on it.
#' @param outpath Pathname to the folder where the JPEGs of
#' the singled-out artefacts from this function should be stored.
#' @param thres threshold, by default: 50%. Less contrasted images
#' and images with objects showing light hues need to have
#' a higher threshold (~ 80%). Very contrasted images could have a
#' lesser threshold (~ 20%)
#' @param min.area the minimal area, by default: 20 px. If the area
#' of the object is under this threshold, the object will be discarded.
#'
#' @return If return_combined_outlines = TRUE, returns the combined
#' Coo objects in a single Opn file. If return_combined_outlines = FALSE,
#' returns a list of coordinate matrices of each open outline.
#'
#' @export
separate_single_artefacts <- function(inpath, outpath,
                                      thres = "50%",
                                      min.area = 20) {

  files_to_use_names <- list.files(inpath,
                                   full.names = FALSE,
                                   pattern = c(".jpg", ".jpeg", ".JPG", ".JPEG"))
  inpath <- list.files(inpath,
                       full.names = TRUE,
                       pattern = c(".jpg", ".jpeg", ".JPG", ".JPEG"))

  pb <- utils::txtProgressBar(min = 0, max = length(inpath), style = 3)
  for (current_masked_file in 1:length(inpath)) {
    # current_masked_file <- 1
    # x_raw <- imager::load.image(inpath[current_masked_file])
    x_raw <- magick::image_read(inpath[current_masked_file])
    # plot(x_raw)
    test <- magick::image_convert(x_raw, colorspace = "Gray")
    # plot(test)
    x_thres <- magick::image_threshold(test,
                                       type = "black",
                                       threshold = thres)
    # plot(x_thres)
    x_grayscale <- imager::magick2cimg(x_thres)
    # x_grayscale <- imager::grayscale(x_raw)
    # plot(x_grayscale)
    # because some images had to be pre-processed in GIMP with the thresholding tool, this step might result in errors
    x_threshold <- x_grayscale
    # plot(imager::threshold(x_threshold, thr = "20%"))
    try(x_threshold <- imager::threshold(x_threshold))
    # plot(x_threshold)
    x_clean <- imager::clean(x_threshold, 1)
    # plot(x_clean)
    x_clean_cimg <- imager::as.cimg(x_clean)

    x_clean_img <- EBImage::Image(x_clean_cimg)
    x_clean_img_filled <- EBImage::fillHull(1 - x_clean_img)
    # plot(x_clean_img_filled)

    bin_image <- round(x_clean_img_filled / max(x_clean_img_filled), 0)
    # plot(bin_image)

    bin_image_labeled <- EBImage::bwlabel(bin_image)
    # EBImage::computeFeatures.shape(bin_image_labeled)
    # plot(bin_image_labeled)

    # important step to be able to work with images that are already binary and images that are still in grayscale
    if (EBImage::numberOfFrames(bin_image_labeled) == 1) {
      bin_image_labeled_filled_frame <- try(EBImage::getFrame(bin_image_labeled, 1),
                                            silent = TRUE
      )
    } else {
      bin_image_labeled_filled_frame <- try(EBImage::getFrame(bin_image_labeled, 3),
                                            silent = TRUE
      )
    }
    # plot(bin_image_labeled_filled_frame)
    features <- EBImage::computeFeatures.shape(bin_image_labeled_filled_frame)
    print(paste0("nb of object before rm small ones: ", nrow(features)))
    features <- features[features[, "s.area"] > min.area,]
    print(paste0("nb of object after rm small ones: ", nrow(features)))
    all_objects <- row.names(features)

    for (object_counter in all_objects) {
      # object_counter <- 1
      # single out individual artefacts (starting with the largest)
      current_object <- EBImage::rmObjects(
        bin_image_labeled_filled_frame,
        all_objects[all_objects != object_counter]
      )

      # inverts the image (black artefact on white background)
      current_object_inverted <- 1 - current_object

      # save it as .jpg with a pseudo-number (does not represent the number which the artefact might have on the page)
      out.name <- unlist(strsplit(files_to_use_names[current_masked_file],
                                  split = "[.]"))
      # remove device (last element)
      image.name <- paste0(paste0(head(out.name, -1), collapse = "."),
                           ".out", object_counter,
                           ".jpg")
      EBImage::writeImage(
        current_object_inverted,
        file.path(outpath, image.name)
      )
    }

    gc()
    utils::setTxtProgressBar(pb, current_masked_file)
  }
  close(pb)
}
