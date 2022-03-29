#' Internal function copied from ggplot2 (not exported there).
#'
#' This function generates a unique (within-session) name for a grob, based on
#' the grob's class.
#'
#' @param prefix The prefix part of the name.
#' @param grob A grob object or NULL.
#' @importFrom grid grobName
ggname <- function (prefix, grob) {
	grob$name <- grid::grobName(grob, prefix)
	grob
}
