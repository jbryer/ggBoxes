test_that("mtcars_errorbox2d", {
	data(mtcars)
	p <- ggplot(mtcars, aes(x = wt, y = mpg, group = factor(cyl), color = factor(cyl), fill = factor(cyl))) +
		geom_errorbox2d() +
		theme(legend.position = 'bottom')
	vdiffr::expect_doppelganger("mtcars errorbox2d", p)
})
