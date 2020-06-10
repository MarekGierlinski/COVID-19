read_data <- drake_plan(
  url_covid = get_url(),
  covid_raw = read_covid(url_covid),
  covid = process_covid(covid_raw, gdp),
  covid_sel = covid %>% filter(country %in% countries_sel) %>% filter(cases > 0) %>% mutate(country = factor(country, levels=countries_sel)),
  covid_europe = covid %>% filter(id %in% europe),
  ons = read_ons(),
  gdp = read_gdp("data/UNdata_Export_20200427_142511000.csv"),
  print_date = print(paste("Last date in file:", covid %>% pull(date) %>% max()))
)

plots <- drake_plan(
  #fig_cases = basic_plot(covid_sel),
  #fig_deaths = basic_plot(covid_sel %>% filter(deaths>0), y="deaths"),
  #fig_cases_pop = basic_plot(covid_sel, y="cases_pop"),
  #fig_deaths_pop = basic_plot(covid_sel %>% filter(deaths>0), y="deaths_pop"),
  #fig_shifted_cases = plot_shifted(covid_sel, what="cases"),
  #fig_shifted_deaths = plot_shifted(covid_sel, what="deaths", val.min=10),
  fig_shifted_cases_pop = plot_shifted(covid_sel, what="cases_pop", val.min=1, val.max=20),
  fig_shifted_deaths_pop = plot_shifted(covid_sel, what="deaths_pop", val.min=0.2, val.max=5),
  #fig_doubling_cases = plot_doubling_times(covid_sel, what="cases", val.min=100),
  #fig_doubling_deaths = plot_doubling_times(covid, what="deaths", val.min=10),
  #fig_uk = plot_country(covid, cntry="United Kingdom"),
  #fig_us = plot_country(covid, cntry="United States"),
  fig_new_cases = plot_daily_cases(covid, what="new_cases", ncol=8),
  fig_new_deaths = plot_daily_cases(covid, what="new_deaths", ncol=8, val.min = 1),
  #fig_italy_fit = plot_country_fit(covid, "Italy", val.max=2000),
  #fig_spain_fit = plot_country_fit(covid, "Spain", val.max=1000),
  #fig_diff_italy = plot_derivative(covid, cntry="Italy"),
  #fig_diff_uk = plot_derivative(covid, cntry="United Kingdom"),
  fig_ratio = plot_death_ratio(covid, mortality=NULL),
  fig_cases_deaths = plot_cases_diff_deaths(covid),
  fig_cases_deaths_pop = plot_cases_diff_deaths(covid, pop=TRUE),
  fig_cases_deaths_both = plot_grid(fig_cases_deaths, fig_cases_deaths_pop, nrow=1),
  fig_eu = plot_cases_diff_deaths(covid_europe, pop=TRUE, x.min=0.1),
  
  fig_daily_deaths = plot_daily(covid, countries_day, what="deaths", span=0.5),
  fig_daily_deaths_fixed = plot_daily(covid, countries_day, what="deaths", span=0.5, scls="fixed", ymax=30),
  fig_daily_cases = plot_daily(covid, countries_day, what="cases", span=0.5),
  fig_daily_cases_fixed = plot_daily(covid, countries_day, what="cases", span=0.5, scls="fixed", ymax=200),
  fig_shift_cases = plot_shifts(covid, countries_day),
  
  fig_daily_deaths_2 = plot_daily(covid, countries_2, what="deaths", span=0.5, cut.day=0),
  fig_daily_cases_2 = plot_daily(covid, countries_2, what="cases", span=0.5, cut.day=0),
  
  fig_daily_deaths_3 = plot_daily(covid, countries_3, what="deaths", span=0.4, cut.day=0),
  fig_daily_cases_3 = plot_daily(covid, countries_3, what="cases", span=0.4, cut.day=0),
  
  fig_daily_fits_cases = plot_daily_fits(covid, countries_day, what="cases", span=0.5),
  fig_daily_fits_deaths = plot_daily_fits(covid, countries_day, what="deaths", span=0.5),
  
  fig_recent_daily_deaths = plot_recent_daily(covid, what="new_deaths_pop", n=7, top.n=10),
  fig_recent_daily_cases = plot_recent_daily(covid, what="new_cases_pop", n=7, top.n=10),
  
  fig_uk_korea_excess = plot_death_excess(covid, cntry="United Kingdom", cntry_short="the UK", base_country = "South Korea", val.min=0.05, val.max=0.2),
  fig_uk_germany_excess = plot_death_excess(covid, cntry="United Kingdom", cntry_short="UK", base_country = "Germany", val.min=0.05, val.max=0.5),
  
  fig_deaths_population = plot_deaths_gdppop(covid, what="pop"),
  fig_deaths_gdp = plot_deaths_gdppop(covid, what="gdp"),
  fig_deaths_vs_cases = plot_cases_deaths(covid, min.deaths=1000),
  
  fig_week_days_deaths = plot_week_days(covid, countries_day, what="deaths"),
  fig_week_days_cases = plot_week_days(covid, countries_day, what="cases"),
  
  fig_week_days_deaths_tot = plot_week_days_total(covid),
  
  fig_map_eur_deaths = plot_map_europe(covid, what="deaths", brks=0:5*10000),
  fig_map_eur_cases = plot_map_europe(covid, what="cases", brks=0:5*100000),
  fig_map_eur_deaths_pop = plot_map_europe(covid, what="deaths_pop",brks=seq(0,10,2)*100),
  fig_map_eur_cases_pop = plot_map_europe(covid, what="cases_pop", brks=0:10*1000),
  
  fig_heatmap_deaths = plot_smooth_heatmap(covid, what="deaths", min.val=50, brks=c(0,1,2,5,10,20,50)),
  fig_heatmap_cases = plot_smooth_heatmap(covid, what="cases", min.val=1000, brks=c(0,10,20,50,100,200,500))

)

figs <- plots %>% 
  select(-command, name = target) %>% 
  mutate(
    obj = rlang::syms(name),
    filename = paste0("fig/", str_remove(name, "fig_"), ".png"),
    width = 6,
    height = 4
  ) %>% 
  mutate(height = if_else(str_detect(name, "fig_cases_deaths"), 11, height)) %>% 
  mutate(height = if_else(str_detect(name, "excess"), 6, height)) %>% 
  mutate(height = if_else(str_detect(name, "ratio"), 8, height)) %>%
  mutate(height = if_else(str_detect(name, "new"), 10, height)) %>% 
  mutate(height = if_else(str_detect(name, "heatmap"), 6, height))

save_figures <- drake_plan(
  figures = target(
    command = annotate_save(filename, obj, url_covid, width=width, height=height),
    transform = map(.data = !!figs, .id=eval(obj))
  )
)

ons_figures <- drake_plan(
  fig_ons_deaths = plot_ons_deaths(ons, max(ons$week)),
  ggsave("fig/ons_deaths.png", fig_ons_deaths, device="png", width=6, height=3)
)

animations <-  drake_plan(
  anim_cases_deaths = plot_cases_deaths_anim(covid),
  save_cases_deaths = anim_save("anim_cases_deaths.gif", animation=anim_cases_deaths, path="fig")
)


testing <- drake_plan(
  url_testing = get_url_testing(),
  testing_data = read_testing(url_testing),
  
  fig_testing_people = plot_testing(testing_data, "people_tested"),
  fig_testing_positives = plot_testing(testing_data, "positives"),
  
  save_fig_testing_people = ggsave("fig/testing_people.png", fig_testing_people, device="png", width=7, height=3),
  save_fig_testing_positives = ggsave("fig/testing_positives.png", fig_testing_positives, device="png", width=7, height=3)
)

knit_report <- drake_plan(
  report = rmarkdown::render(
    input = knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  ) 
)

plan <- bind_rows(
  read_data,
  plots,
  save_figures,
  ons_figures,
  testing,
  knit_report
  #animations
)
