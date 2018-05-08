################################################################################
#' Creates a temporary Power Point object
#' @param template_path The path to a pptx to use as a template
#'
#' @return A temporary Power Point file
#' @export
create_powerpoint <- function(template_path=NULL) {
  if (!is.null(template_path)) {
    return(pptx(template = template_path))
  } else {
    return(pptx())
  }
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
#' @return the temporary Power Point file with a title slide added
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
#' Add a text only slide temporary Power Point file
#' @param doc a Temporary Power Point file created by create_powerpoint()
#' @param title The title of the slide
#' @param paragraph Some text
#' @param footer OPTIONAL a footer for the slide
#' @param page_number OPTIONAL the page number
#' 
#' @return the temporary Power Point file with a text slide added
#' @export
add_text_slide <- function(doc,
                           title,
                           paragraph,
                           footer=NULL,
                           page_number=NULL) {
  doc <- addSlide(doc, "Title and Content")
  doc <- addTitle(doc, title)
  doc <- addParagraph(doc, paragraph)
  if (!is.null(footer)) {
    doc <- addFooter(doc, footer)
  }
  if (!is.null(page_number)) {
    doc <- addPageNumber(doc, page_number)
  }
  return(doc)
}

################################################################################
#' Add a slide with a data table to a temporary Power Point file
#' @param doc a Temporary Power Point file created by create_powerpoint()
#' @param title The title of the slide
#' @param df_table A dataframe to be added as a table
#' @param paragraph OPTIONAL Some text describing the table
#' @param footer OPTIONAL a footer for the slide
#' @param page_number OPTIONAL the page number
#' @param invert A boolean if true swap the positions of the plot and the description
#' 
#' @return the temporary Power Point file with a table slide added
#' @export
add_table_slide <- function(doc,
                            title,
                            df_table,
                            paragraph=NULL,
                            footer=NULL,
                            page_number=NULL,
                            invert=FALSE) {
  # if we have not been given a paragraph, assume that we're only going to add
  # table with no explantory text
  if (!is.null(paragraph)) {
    doc <- addSlide(doc, "Two Content")
  } else {
    doc <- addSlide(doc, "Title and Content")
  }
  
  doc <- addTitle(doc, title)
  if (invert) {
    if (!is.null(paragraph)) {
      doc <- addParagraph(doc, paragraph)
    }
    doc <- addFlexTable(doc, FlexTable(df_table))
  } else {
    doc <- addFlexTable(doc, FlexTable(df_table))
    if (!is.null(paragraph)) {
      doc <- addParagraph(doc, paragraph)
    }
  }
  if (!is.null(footer)) {
    doc <- addFooter(doc, footer)
  }
  if (!is.null(page_number)) {
    doc <- addPageNumber(doc, page_number)
  }
}

################################################################################
#' Add a title slide to a temporary Power Point file
#' @param doc a Temporary Power Point file created by create_powerpoint()
#' @param title The title of the slide
#' @param ggplot_object A ggplot object
#' @param paragraph OPTIONAL Some text describing the ggplot_image
#' @param footer OPTIONAL a footer for the slide
#' @param page_number OPTIONAL the page number
#' @param invert A boolean if true swap the positions of the plot and the description
#' 
#' @return the temporary Power Point file with a title slide added
#' @export
add_plot_slide <- function(doc,
                           title,
                           ggplot_object,
                           paragraph=NULL,
                           footer=NULL,
                           page_number=NULL,
                           invert=FALSE) {
  # if we have not been given a paragraph, assume that we're only going to add
  # plot with no explantory text
  if (!is.null(paragraph)) {
    doc <- addSlide(doc, "Two Content")
  } else {
    doc <- addSlide(doc, "Title and Content")
  }
  doc <- addTitle(doc, title)
  if (invert) {
    if (!is.null(paragraph)) {
      doc <- addParagraph(doc, paragraph)
    }
    doc <- addPlot(doc, fun = print, x = ggplot_object)
  } else {
    if (!is.null(paragraph)) {
      doc <- addParagraph(doc, paragraph)
    }
    doc <- addPlot(doc, fun = print, x = ggplot_object)
  }
  if (!is.null(footer)) {
    doc <- addFooter(doc, footer)
  }
  if (!is.null(page_number)) {
    doc <- addPageNumber(doc, page_number)
  }
  return(doc)
}