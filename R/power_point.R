################################################################################
#' Creates a temporary Power Point object
#'
#' @return A temporary Power Point file
#' @export
create_powerpoint <- function() {
  return(pptx())
}

################################################################################
#' Saves a temporary Power Point file
#' @param doc a Temporary Power Point file created by create_powerpoint()
#' @param path the path to the location where the file will be saved
#' 
#' @return Nothing
#' @export
save_powerpoint <- function(doc, path) {
  writeDoc(doc, path)
}

################################################################################
#' Add a title slide to a temporary Power Point file
#' @param doc a Temporary Power Point file created by create_powerpoint()
#' @param title The title of the slide
#' @param subtitle OPTIONAL a subtitle for the slide
#' @param add_date A boolean indicating whether or not to add today's dat to the slide
#' @param footer OPTIONAL a footer for the slide
#' @param page_number OPTIONAL the page number
#' 
#' @return the temporary Power Point file with a title slided added
#' @export
add_title_slide <- function(doc, 
                            title,
                            subtitle=NULL,
                            add_date=FALSE,
                            footer=NULL,
                            page_number=NULL) {
  doc <- addSlide(doc, "Title Slide")
  doc <- addTitle(doc, title)
  if (!is.null(subtitle)) {
    doc <- addSubtitle(doc, subtitle)
  }
  if (add_date) {
    doc <- addDate(doc)
  }
  if (!is.null(footer)) {
    doc <- addFooter(doc, footer)
  }
  if (!is.null(page_number)) {
    doc <- addPageNumber(doc, page_number)
  }
  return(doc)
}

################################################################################
#' Add a title slide to a temporary Power Point file
#' @param doc a Temporary Power Point file created by create_powerpoint()
#' @param title The title of the slide
#' @param ggplot_object A ggplot object
#' @param paragraph Some text describing the ggplot_image
#' @param footer OPTIONAL a footer for the slide
#' @param page_number OPTIONAL the page number
#' @param invert A boolean if true swap the positions of the plot and the description
#' 
#' @return the temporary Power Point file with a title slided added
#' @export
add_content_slide <- function(doc,
                              title,
                              ggplot_object,
                              paragraph,
                              footer=NULL,
                              page_number=NULL,
                              invert=FALSE) {
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, title)
  if (invert) {
    doc <- addParagraph(doc, paragraph)
    doc <- addPlot(doc, fun = print, x = ggplot_object)
  } else {
    doc <- addPlot(doc, fun = print, x = ggplot_object)
    doc <- addParagraph(doc, paragraph)
  }
  if (!is.null(footer)) {
    doc <- addFooter(doc, footer)
  }
  if (!is.null(page_number)) {
    doc <- addPageNumber(doc, page_number)
  }
  return(doc)
}