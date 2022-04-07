library(devtools)

usethis::use_tidy_description()
document()
install(build_vignettes = TRUE)
test()
check()



##### Package setup stuff
usethis::use_package('grid')
usethis::use_package('vdiffr', type = 'Suggests')
usethis::use_gpl3_license()
usethis::use_testthat()
usethis::use_test('geom_boxplot2d')
usethis::use_vignette('boxplot2d')


##### Create the sticker
library(hexSticker)
library(ggBoxes)

data(mtcars)
p <- ggplot(mtcars, aes(x = wt, y = mpg, group = factor(cyl),
				   color = factor(cyl), fill = factor(cyl))) +
	geom_boxplot2d(size = .5) +
	theme_minimal() +
	theme(legend.position = 'none',
		  axis.text = element_blank(),
		  axis.title = element_blank(),
		  panel.grid = element_blank())


hexSticker::sticker(p,
					filename = 'man/figures/ggBoxes.png',
					p_size = 18,
					package = 'ggBoxes',
					url = "github.com/jbryer/ggBoxes",
					u_size = 5.5,
					s_width = 1.5, s_height = 1.2,
					s_x = .9, s_y = 0.9,
					p_x = 1, p_y = 1.59,
					p_color = "#000000",
					h_fill = '#FFFFFF',
					h_color = '#CC79A7',
					white_around_sticker = FALSE)


