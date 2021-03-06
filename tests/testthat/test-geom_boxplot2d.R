test_that("mtcars_boxplot2d", {
	data(mtcars)
	p <- ggplot(mtcars, aes(x = wt, y = mpg, group = factor(cyl), color = factor(cyl), fill = factor(cyl))) +
		geom_boxplot2d() +
		theme(legend.position = 'bottom')
	vdiffr::expect_doppelganger("mtcars boxplot2d", p)
})
