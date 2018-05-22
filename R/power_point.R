################################################################################
#' Creates a temporary Power Point object
#' @param template_path The path to a pptx to use as a template
#' @param title The title of the document in the file metadata
#' 
#' @return A temporary Power Point file
#' @export
create_powerpoint <- function(title, template_path=NULL) {
  if (!is.null(template_path)) {
    return(read_pptx(title = title, template = template_path))
  } else {
    return(read_pptx(title = title))
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
  print(doc, target = path) 
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
                            page_number=NULL,
                            template_override = NULL) {
  template_name <- if(is.null(template_override)) {"Title Slide"} else {template_override}
  
  doc <- addSlide(doc, template_name)
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
#' Add an empty generic slide, a user can insert  user reportRs functions
#' @param doc a Temporary Power Point file created by create_powerpoint()
#' @param title The title of the slide
#' @param cols The number of columns to add must be 1 or 2
#' @param footer OPTIONAL a footer for the slide
#' @param page_number OPTIONAL the page number
#' 
#' @return the temporary Power Point file with a text slide added
#' @export
add_generic_slide <- function(doc,
                              layout) {
  if (cols == 1) {
    template_name <- "Title and Content"
  } else if (cols == 2) {
    template_name <- "Two Content"
  } else {
    stop("In add_generic_slide number of columns must be 1 or 2")
  }
  
  template_name <- if (is.null(template_override)) {template_name} else {template_override}
  
  doc <- addSlide(doc, template_name)
  doc <- addTitle(doc, title)
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
#' @param cols The number of columns to add must be 1 or 2
#' @param paragraph Some text
#' @param footer OPTIONAL a footer for the slide
#' @param page_number OPTIONAL the page number
#' 
#' @return the temporary Power Point file with a text slide added
#' @export
add_text_slide <- function(doc,
                           title,
                           cols=1,
                           paragraph,
                           footer=NULL,
                           page_number=NULL) {
  if (cols == 1) {
    doc <- addSlide(doc, "Title and Content")
  } else if (cols == 2) {
    doc <- addSlide(doc, "Two Content")
  } else {
    stop("In add_text_slide number of columns must be 1 or 2")
  }
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
#' @param cols The number of columns to add must be 1 or 2
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
                            cols=1,
                            df_table,
                            paragraph=NULL,
                            footer=NULL,
                            page_number=NULL,
                            invert=FALSE) {
  # if we have not been given a paragraph, assume that we're only going to add
  # table with no explantory text
  if (cols == 1) {
    doc <- addSlide(doc, "Title and Content")
  } else if (cols == 2) {
    doc <- addSlide(doc, "Two Content")
  } else {
    stop("In add_table_slide number of columns must be 1 or 2")
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
#' @param cols The number of columns to add must be 1 or 2
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
                           cols=1,
                           ggplot_object,
                           paragraph=NULL,
                           footer=NULL,
                           page_number=NULL,
                           invert=FALSE) {
  # if we have not been given a paragraph, assume that we're only going to add
  # plot with no explantory text
  if (cols == 1) {
    doc <- addSlide(doc, "Title and Content")
  } else if (cols == 2) {
    doc <- addSlide(doc, "Two Content")
  } else {
    stop("In add_plot_slide number of columns must be 1 or 2")
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

################################################################################
#' Inserts a list into the current slide
#' @param doc a Temporary Power Point file created by create_powerpoint()
#' @param list_of_items A vector of items to incldue in list
#' @param numbered A boolean if true the list is numbered
#' 
#' @return the temporary Power Point file with a title slide added
#' @export
insert_list <- function(doc, 
                        list_of_items,
                        append = FALSE,
                        numbered = FALSE) {
  if (numbered) {
    list_type = 'ordered'
  } else{ 
    list_type = 'unordered'
  }
  
  doc <- addParagraph(doc, 
                      value = list_of_items,
                      append = append,
                      par.properties =  parProperties(list.style = list_type))  
  
  return(doc)
}

################################################################################
#' Inserts a plot directly into the current slide
#' @param doc a Temporary Power Point file created by create_powerpoint()
#' @param ggplot_object A vector of items to incldue in list
#' @param vector plot as vector graphic or png
#' @param font_size rescales some aspects of the plot
#' 
#' @return the temporary Power Point file with a title slide added
#' @export
insert_plot <- function(doc, ggplot_object, vector=TRUE, font_size=6) {
  doc <- addPlot(doc, fun = print, x = ggplot_object, vector.graphic = vector, pointsize = font_size)
  return(doc)
}

################################################################################
#' Inserts a dataframe into the current slide as a table
#' @param doc a Temporary Power Point file created by create_powerpoint()
#' @param df_table A data frame to be inserted as a table
#' 
#' @return the temporary Power Point file with a title slide added
#' @export
insert_table <- function(doc, df_table) {
  doc <- addFlexTable(doc, FlexTable(df_table))
  return(doc)
}