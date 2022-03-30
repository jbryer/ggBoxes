#' Create a ggplot2 geom for two dimensional error boxes.
#'
#' @rdname error2d
#' @format NULL
#' @usage NULL
#' @export
#' @import dplyr
#' @importFrom ggplot2 aes ggproto ggproto_parent GeomPolygon Geom draw_key_polygon
#' @importFrom grid nullGrob polygonGrob gpar segmentsGrob pointsGrob grobTree
GeomErrorBox2d <- ggproto(
	"GeomErrorBox2d",
	ggplot2::Geom,
	extra_params = c("conf_levels", "na.rm"),
	setup_data = function(self, data, params) {
		data <- ggplot2::ggproto_parent(ggplot2::GeomPolygon, self)$setup_data(data, params)
		data
	},
	draw_group = function(data, panel_params, coord, conf_levels, alpha, na.rm) {
		n <- nrow(data)
		if (n <= 2) return(grid::nullGrob())

		conf_levels <- conf_levels[order(conf_levels)]

		coords <- coord$transform(data, panel_params)
		first_row <- coords[1,,drop=FALSE] # Use the first row for other aethestics

		alphas <- seq(0, alpha, length(conf_levels) + 1)[-1] # Drop zero alpha
		grobs <- list()

		tab <- data[,c('x','y')] |>
			dplyr::summarise(n = dplyr::n(),
							 x_mean = mean(x, na.rm = na.rm),
							 x_se = sd(x, na.rm = na.rm) / sqrt(dplyr::n()),
							 y_mean  = mean(y, na.rm = na.rm),
							 y_se = sd(y, na.rm = na.rm) / sqrt(dplyr::n()))


		for(i in seq_len(length(conf_levels))) {
			cl <- (1 - conf_levels[i]) / 2
			cv <- abs(qnorm(cl))

			se_x <- sd(data[,'x',drop=TRUE], na.rm = na.rm) / sqrt(nrow(data))
			se_y <- sd(data[,'y',drop=TRUE], na.rm = na.rm) / sqrt(nrow(data))

			box <- data[,c('x', 'y')] |>
				dplyr::summarize(
					xmin = mean(x, na.rm = na.rm) - cv * se_x,
					ymin = mean(y, na.rm = na.rm) - cv * se_y,
					xmax = mean(x, na.rm = na.rm) + cv * se_x,
					ymax = mean(y, na.rm = na.rm) + cv * se_y)
			box <- as.numeric(box[1,,drop = TRUE])
			box <- as.data.frame(rbind(box[c(1,2)],
									   box[c(1,4)],
									   box[c(3,4)],
									   box[c(3,2)]))
			names(box) <- c('x', 'y')
			coords <- coord$transform(box, panel_params)
			grobs[[length(grobs)+1]] <- grid::polygonGrob(
				x = coords$x,
				y = coords$y,
				gp = grid::gpar(
					alpha = 0.2,
					col = first_row$colour,
					fill = first_row$fill,
					lwd = first_row$size * .pt,
					lty = first_row$linetype
				)
			)
		}

		# Mean
		# # TODO: use hline and vline
		grobs[[length(grobs)+1]] <- grid::segmentsGrob(
			x0 = mean(coords$x),
			y0 = min(coords$y),
			x1 = mean(coords$x),
			y1 = max(coords$y),
			gp = grid::gpar(
				alpha = 1,
				col = first_row$colour,
				fill = first_row$fill,
				lwd = first_row$size * .pt,
				lty = first_row$linetype
			)
		)

		grobs[[length(grobs)+1]] <- grid::segmentsGrob(
			x0 = min(coords$x),
			y0 = mean(coords$y),
			x1 = max(coords$x),
			y1 = mean(coords$y),
			gp = grid::gpar(
				alpha = 1,
				col = first_row$colour,
				fill = first_row$colour,
				lwd = first_row$size * .pt,
				lty = first_row$linetype
			)
		)

		tree <- do.call(grid::grobTree, grobs)

		ggname("geom_errorbox2d", tree)
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
		stroke_size = 4,
		na.rm = TRUE
	)
)

#' A two-dimensional error box plot.
#'
#'
#' @inheritParams ggplot2::layer
#' @seealso [geom_boxplot2d()]
#' @export
#' @importFrom ggplot2 layer
#' @examples
#' data(mtcars)
#' ggplot(mtcars, aes(x = wt, y = mpg, group = factor(cyl))) + geom_errorbox2d()
geom_errorbox2d <-function(mapping = NULL,
						data = NULL,
						stat = "identity",
						position = "identity",
						na.rm = TRUE,
						show.legend = NA,
						inherit.aes = TRUE,
						conf_levels = c(0.9, 0.95, 0.99),
						alpha = 1.0,
						...) {
	ggplot2::layer(
		geom = GeomErrorBox2d,
		 mapping = mapping,
		 data = data,
		 stat = stat,
		 position = position,
		 show.legend = show.legend,
		 inherit.aes = inherit.aes,
		 params = list(na.rm = na.rm,
		 			   conf_levels = conf_levels,
		 			   alpha = alpha,
		 			   ...)
	)
}

