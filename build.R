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


gd <- mtcars |>
	group_by(cyl) |>
	summarise(n = n(),
			  wt_mean = mean(wt),
			  wt_se = sd(wt) / sqrt(n()),
			  mpg_mean  = mean(mpg),
			  mpg_se = sd(mpg) / sqrt(n()))
ggplot(mtcars, aes(x = wt, y = mpg, group = factor(cyl), color = factor(cyl), fill = factor(cyl))) +
	geom_boxplot2d() +
	# geom_point(alpha = 0) +
	geom_errorbar(data = gd,
				  aes(x = wt_mean,
				  	xmin = wt_mean - 1.96 * wt_se,
				  	xmax = wt_mean + 1.96 * wt_se,
				  	y = mpg_mean),
				  size = 1, width = 0.5, linetype = 6) +
	geom_errorbar(data = gd,
				  aes(x = wt_mean,
				  	ymin = mpg_mean - 1.96 * mpg_se,
				  	ymax = mpg_mean + 1.96 * mpg_se,
				  	y = mpg_mean),
				  size = 1, width = 0.5, linetype = 6) +
	geom_rug() +
	geom_point(data = gd,
			   aes(x = wt_mean, y = mpg_mean),
			   size = 5, shape = 15, alpha = 1) +
	theme(legend.position = 'bottom')

data(iris)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species, fill = Species)) +
	geom_boxplot2d() + geom_point(alpha = 0.2)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species, fill = Species)) +
	geom_boxplot2d()
