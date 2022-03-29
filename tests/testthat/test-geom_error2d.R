test_that("mtcars_error2d", {
	data(mtcars)
	p <- ggplot(mtcars, aes(x = wt, y = mpg, group = factor(cyl), color = factor(cyl), fill = factor(cyl))) +
		geom_error2d() +
		theme(legend.position = 'bottom')
	vdiffr::expect_doppelganger("mtcars error2d", p)
})
