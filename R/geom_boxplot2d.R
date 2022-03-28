#' Internal function copied from ggplot2 (not exported there).
#' @importFrom grid grobName
ggname <- function (prefix, grob) {
	grob$name <- grobName(grob, prefix)
	grob
}

#' Create a ggplot2 geom for two dimensional box and whisker plots.
#'
#' @rdname boxplot2d
#' @format NULL
#' @usage NULL
#' @export
#' @import dplyr
#' @importFrom ggplot2 aes ggproto ggproto_parent GeomPolygon Geom draw_key_polygon
#' @importFrom grid nullGrob polygonGrob gpar segmentsGrob pointsGrob grobTree
GeomBoxplot2d <- ggproto(
	"GeomBoxplot2d",
	ggplot2::Geom,
	setup_data = function(self, data, params) {
		data <- ggplot2::ggproto_parent(ggplot2::GeomPolygon, self)$setup_data(data, params)
		data
	},
	draw_group = function(data, panel_scales, coord) {
		n <- nrow(data)
		if (n <= 2) return(grid::nullGrob())

		coords <- coord$transform(data, panel_scales)
		first_row <- coords[1,,drop=FALSE] # Use the first row for other aethestics

		# Outer box (1.5 times the IQR)
		outer_box <- data[,c('x', 'y')] %>%
			summarize(xmin = max(quantile(x, .25) - 1.5 * IQR(x), min(x)),
					  ymin = max(quantile(y, .25) - 1.5 * IQR(y), min(y)),
					  xmax = min(quantile(x, .75) + 1.5 * IQR(x), max(x)),
					  ymax = min(quantile(y, .75) + 1.5 * IQR(y), max(y)) )
		outer_box <- as.numeric(outer_box[1,,drop = TRUE])
		outer_box <- as.data.frame(rbind(outer_box[c(1,2)],
										 outer_box[c(1, 4)],
										 outer_box[c(3,4)],
										 outer_box[c(3,2)]))
		names(outer_box) <- c('x', 'y')
		outer_coords <- coord$transform(outer_box, panel_scales)
		outer_grob <- grid::polygonGrob(
			x = outer_coords$x,
			y = outer_coords$y,
			gp = grid::gpar(
				alpha = 0.2,
				col = first_row$colour,
				fill = first_row$fill,
				lwd = first_row$size * .pt,
				lty = first_row$linetype
			)
		)

		# Inner box (IQR)
		inner_box <- data[,c('x', 'y')] %>%
			dplyr::summarize(xmin = quantile(x, .25),
					  ymin = quantile(y, .25),
					  xmax = quantile(x, .75),
					  ymax = quantile(y, .75))
		inner_box <- as.numeric(inner_box[1,,drop = TRUE])
		inner_box <- as.data.frame(rbind(inner_box[c(1,2)],
										 inner_box[c(1, 4)],
										 inner_box[c(3,4)],
										 inner_box[c(3,2)]))
		names(inner_box) <- c('x', 'y')
		inner_coords <- coord$transform(inner_box, panel_scales)
		inner_grob <- grid::polygonGrob(
			x = inner_coords$x,
			y = inner_coords$y,
			gp = grid::gpar(
				alpha = 0.5,
				col = first_row$colour,
				fill = first_row$fill,
				lwd = first_row$size * .pt,
				lty = first_row$linetype
			)
		)

		# Median
		median_x_grob <- grid::segmentsGrob(
			x0 = median(coords$x),
			y0 = max(quantile(coords$y, .25) - 1.5 * IQR(coords$y), min(coords$y)),
			x1 = median(coords$x),
			y1 = min(quantile(coords$y, .75) + 1.5 * IQR(coords$y), max(coords$y)),
			gp = grid::gpar(
				alpha = 1,
				col = first_row$colour,
				fill = first_row$fill,
				lwd = first_row$size * .pt,
				lty = first_row$linetype
			)
		)

		median_y_grob <- grid::segmentsGrob(
			x0 = max(quantile(coords$x, .25) - 1.5 * IQR(coords$x), min(coords$x)),
			y0 = median(coords$y),
			x1 = min(quantile(coords$x, .75) + 1.5 * IQR(coords$x), max(coords$x)),
			y1 = median(coords$y),
			gp = grid::gpar(
				alpha = 1,
				col = first_row$colour,
				fill = first_row$colour,
				lwd = first_row$size * .pt,
				lty = first_row$linetype
			)
		)

		# Outliers
		tree <- NULL
		outliers <- data %>% filter(x > max(outer_box$x) |
									x < min(outer_box$x) |
									y > max(outer_box$y) |
									y < min(outer_box$y))
		if(nrow(outliers) > 0) {
			outliers_coords <- coord$transform(outliers, panel_scales)
			stroke_size <- outliers_coords$stroke
			stroke_size[is.na(stroke_size)] <- 0
			points_grob <- grid::pointsGrob(
				x = outliers_coords$x,
				y = outliers_coords$y,
				pch = outliers_coords$shape,
				gp = grid::gpar(
					col = first_row$colour,
					fill = first_row$fill,
					fontsize = first_row$size * .pt + stroke_size * .stroke / 2,
					lwd = first_row$size * .pt,
					lty = first_row$linetype,
					pch = first_row$shape
				)
			)
			tree <- grobTree(inner_grob, outer_grob, median_x_grob, median_y_grob, points_grob)
		} else {
			tree <- grobTree(inner_grob, outer_grob, median_x_grob, median_y_grob)
		}

		ggname("geom_boxplot2d", tree)
	},
	draw_key = ggplot2::draw_key_polygon,
	required_aes = c("x", "y"),
	default_aes = aes(
		colour = "grey20",
		fill = "grey50",
		size = 0.75,
		linetype = 1,
		alpha = 1,
		shape = 16,
		stroke_size = 4
	)
)

#' A two-dimensional box and whiskers plot
#'
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_boxplot
#' @seealso [geom_boxplot()]
#' @references Persaud, N., & Yates, B. T. (complete 9-chapter book manuscript
#' submitted for hopefully final review 2022 March 25).  Cost-inclusive
#' evaluation: Planning it, doing it, using it. Book manuscript already under
#' contract. New York, NY: Guilford Press.
#' @export
#' @importFrom ggplot2 layer
#' @examples
#' data(mtcars)
#' ggplot(mtcars, aes(x = wt, y = mpg, group = factor(cyl))) + geom_boxplot2d()
geom_boxplot2d <-function(mapping = NULL,
						  data = NULL,
						  stat = "identity",
						  position = "identity",
						  na.rm = FALSE,
						  show.legend = NA,
						  inherit.aes = TRUE,
						  ...) {
	ggplot2::layer(geom = GeomBoxplot2d,
		  mapping = mapping,
		  data = data,
		  stat = stat,
		  position = position,
		  show.legend = show.legend,
		  inherit.aes = inherit.aes,
		  params = list(na.rm = na.rm, ...)
	)
}

