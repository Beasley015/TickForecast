df.sum.proc <- df.model.names %>%
	ungroup() %>%
	mutate(year = year(time)) %>%
	group_by(lifeStage, model, species, plotID, siteID, ua) %>%
	mutate(max = if_else(observed >= 0.1 * max(observed), 1, 0)) %>%
	filter(max == 1) %>%
	filter(ua == "+ Process", observed > 0) %>%
	ungroup() %>%
	mutate(
		model = if_else(mice == "No mice" & weather == "No weather", "S", "x"),
		model = if_else(mice == "No mice" & weather == "Weather", "W", model),
		model = if_else(mice == "Mice" & weather == "No weather", "M", model),
		model = if_else(mice == "Mice" & weather == "Weather", "WM", model)
	)


w <- 0.7

gg <- df.model.names %>%
	# mutate(model = if_else(mice == "No mice" & weather == "No weather", "S", "x"),
	#        model = if_else(mice == "No mice" & weather == "Weather", "W", model),
	#        model = if_else(mice == "Mice" & weather == "No weather", "M", model),
	#        model = if_else(mice == "Mice" & weather == "Weather", "WM", model),
	#        model = factor(model, levels = c("S", "M", "W", "WM"))) %>%
	filter(
		lifeStage == "Nymph",
		observed >= 0,
		ua == "+ Process",
		siteID %in% both.plots
	) %>%
	group_by(model, lifeStage, species, siteID, parameters, ua) %>%
	summarise(
		lower95 = quantile(rmse, 0.025),
		lower75 = quantile(rmse, 0.125),
		median = median(rmse),
		mean = mean(rmse),
		upper75 = quantile(rmse, 0.875),
		upper95 = quantile(rmse, 0.975)
	) %>%
	ggplot() +
	aes(x = model, y = median, color = parameters) +
	geom_linerange(
		aes(ymin = lower95, ymax = upper95),
		size = 0.5,
		position = position_dodge(w)
	) +
	geom_linerange(
		aes(ymin = lower75, ymax = upper75),
		size = 2,
		position = position_dodge(w)
	) +
	geom_point(size = 3.5, position = position_dodge(w)) +
	scale_color_manual(values = met.brewer("Egypt")) +
	labs(x = "Model", y = "RMSE", color = "Parameter DA") +
	scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
	facet_grid(species ~ siteID) +
	theme_pubclean() +
	theme(legend.position = "bottom")
gg
save_gg(
	dest = "Both_nonZero.jpeg",
	gg = gg,
	path = file.path(dir.plot, "ScoreDist")
)


mean.scores <- df.sum.proc %>%
	group_by(model, lifeStage, species, siteID, parameters) %>%
	summarise(mu.rmse = mean(crps))

mean.scores %>%
	filter(lifeStage == "Nymph")


null.sum <- null.crps %>%
	select(lifeStage, crps, model, siteID, species)

bind_rows(null.sum, df.sum.proc) %>%
	group_by(model, lifeStage, species, siteID, parameters) %>%
	summarise(mu.rmse = mean(crps)) %>%
	pivot_wider(names_from = c(species, model), values_from = mu.rmse) %>%
	view()


group_by(lifeStage, species, siteID) %>%
	summarise(mu.rmse = mean(rmse))


## combine process model with null, take difference for each model and forecast
## get the average for each model/species and put into a table

process.scores <- df.sum.proc %>%
	select(lifeStage, time, model, ua, siteID, species, crps, parameters, doy)

null.select <- null.crps %>%
	select(-ua, -model, -doy, -crps)

score.join <- left_join(
	process.scores,
	null.select,
	by = c("time", "lifeStage", "siteID", "species")
)

skill <- score.join %>%
	mutate(skill = score - crps)

skill.summary <- skill %>%
	group_by(lifeStage, time, model, ua, siteID, species, parameters) %>%
	summarise(
		mean.skill = mean(skill),
		sd.skill = sd(skill),
		lower.skill = quantile(skill, 0.025),
		upper.skill = quantile(skill, 0.975)
	)
