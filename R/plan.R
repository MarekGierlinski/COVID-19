ecdc_data <- drake_plan(
  url_covid = get_url_ecdc(),
  covid_raw = read_covid(url_covid),
  covid = process_covid(covid_raw, gdp),
  covid_sel = covid %>% filter(country %in% countries_sel) %>% filter(cases > 0) %>% mutate(country = factor(country, levels=countries_sel)),
  covid_europe = covid %>% filter(id %in% europe),
  gdp = read_gdp("data/UNdata_Export_20200427_142511000.csv"),
  print_date = print(paste("Last date in ECDC file:", covid %>% pull(date) %>% max()))
)

plots <- drake_plan(
  fig_shifted_cases_pop = plot_shifted(covid_sel, what="cases_pop", val.min=1, val.max=20),
  fig_shifted_deaths_pop = plot_shifted(covid_sel, what="deaths_pop", val.min=0.2, val.max=5),
  fig_new_cases = plot_daily_cases(covid, what="new_cases", ncol=8),
  fig_new_deaths = plot_daily_cases(covid, what="new_deaths", ncol=8, val.min = 1),
  fig_ratio = plot_death_ratio(covid, mortality=NULL),
  fig_cases_deaths = plot_cases_diff_deaths(covid),
  fig_cases_deaths_pop = plot_cases_diff_deaths(covid, pop=TRUE),
  fig_cases_deaths_both = plot_grid(fig_cases_deaths, fig_cases_deaths_pop, nrow=1),
  fig_eu = plot_cases_diff_deaths(covid_europe, pop=TRUE, x.min=0.1),
  
  fig_daily_deaths = plot_daily(covid, countries_day, what="deaths", span=0.3),
  fig_daily_deaths_fixed = plot_daily(covid, countries_day, what="deaths", span=0.3, scls="fixed", ymax=30),
  fig_daily_cases = plot_daily(covid, countries_day, what="cases", span=0.3),
  fig_daily_cases_fixed = plot_daily(covid, countries_day, what="cases", span=0.3, scls="fixed", ymax=200),
  fig_shift_cases = plot_shifts(covid, countries_day),
  
  fig_daily_deaths_2 = plot_daily(covid, countries_2, what="deaths", span=0.3, cut.day=0),
  fig_daily_cases_2 = plot_daily(covid, countries_2, what="cases", span=0.3, cut.day=0),
  
  fig_daily_deaths_3 = plot_daily(covid, countries_3, what="deaths", span=0.3, cut.day=0),
  fig_daily_cases_3 = plot_daily(covid, countries_3, what="cases", span=0.3, cut.day=0),
  
  fig_daily_fits_cases = plot_daily_fits(covid, countries_day, what="cases", span=0.3),
  fig_daily_fits_deaths = plot_daily_fits(covid, countries_day, what="deaths", span=0.3),
  
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
  fig_heatmap_cases = plot_smooth_heatmap(covid, what="cases", min.val=1000, brks=c(0,10,20,50,100,200,500)),
  
  fig_continents_cases = plot_continents(covid, "new_cases", brks=seq(0, 1e6, 50000)),
  fig_continents_deaths = plot_continents(covid, "new_deaths", brks=seq(0, 1e5, 1000)),
  
  fig_global = plot_global(covid, span=0.3),
  fig_eu_uk_us = plot_eu_uk_us(covid)
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

animations <-  drake_plan(
  anim_cases_deaths = plot_cases_deaths_anim(covid),
  save_cases_deaths = anim_save("anim_cases_deaths.gif", animation=anim_cases_deaths, path="fig")
)


testing <- drake_plan(
  url_testing = get_url_testing(),
  testing_data = read_testing(url_testing),
  
  #fig_testing_people = plot_testing(testing_data, "tests_processed"),
  fig_testing_positives = plot_testing(testing_data, "positives"),
  
  #save_fig_testing_people = ggsave("fig/testing_people.png", fig_testing_people, device="png", width=7, height=3),
  save_fig_testing_positives = ggsave("fig/testing_positives.png", fig_testing_positives, device="png", width=7, height=3)
)

excess <- drake_plan(
  url_excess = get_url_excess(),
  exc = read_excess(url_excess),
  print_exc_date = print(paste("Last date in excess file:", exc %>% drop_na() %>%  pull(date) %>% max())),
  
  fig_exc_countries = plot_excess_details(exc, ncol=4, y.scale=2.9),
  fig_exc_uk = plot_excess_details(exc, "UK", by.region=TRUE, ncol=4, y.scale=4),
  #fig_exc_prop_countries = plot_excess_prop(exc, ncol=3),
  fig_exc_prop_uk = plot_excess_prop(exc, "UK", by.region=TRUE, ncol=4),
  fig_excess_deaths_uk = plot_exc_total_deaths(exc, "UK"),
  fig_excess_weekly_deaths_uk = plot_exc_weekly_deaths(exc, "UK"),
  exc_deaths_2010 = excess_deaths(exc, "UK", year1=2010),
  exc_deaths_2015 = excess_deaths(exc, "UK", year1=2015),
  last_week_uk = last_week(exc, "UK"),
  last_date_uk = last_date(exc, "UK"),
  uk_exc_nations = UK_nation_excess(exc),
  fig_exc_uk_nations = plot_uk_nation_excess(uk_exc_nations),
  
  save_fig_exc_countries = annotate_save("fig/exc_countries.png", fig_exc_countries, url_excess, width=10, height=8),
  save_fig_exc_uk = annotate_save("fig/exc_uk.png", fig_exc_uk, url_excess, width=10, height=8),
  save_fig_exc_deaths_uk = annotate_save("fig/exc_uk_deaths.png", fig_excess_deaths_uk, url_excess, width=6, height=4),
  save_fig_excess_weekly_deaths_uk = annotate_save("fig/exc_uk_weekly_deaths.png", fig_excess_weekly_deaths_uk, url_excess, width=8, height=4),
  #save_fig_exc_prop_countries = annotate_save("fig/exc_prop_countries.png", fig_exc_prop_countries, url_excess, width=10, height=8),
  save_fig_exc_prop_uk = annotate_save("fig/exc_prop_uk.png", fig_exc_prop_uk, url_excess, width=10, height=8),
  save_fig_exc_uk_nations = annotate_save("fig/exc_uk_nations.png", fig_exc_uk_nations, url_excess, width=6, height=4)
)

staging <- drake_plan(
  stag = read_staging_data(),
  fig_stag_cumul = plot_staging_cumul(stag),
  fig_stag_hospital = plot_staging_hospitals(stag),
  
  save_fig_stag_cumul = annotate_save("fig/stag_cumul.png", fig_stag_cumul, "https://coronavirus-staging.data.gov.uk", width=8, height=4),
  save_fig_stag_hospital = annotate_save("fig/stag_hospital.png", fig_stag_hospital, "https://coronavirus-staging.data.gov.uk", width=7, height=6)
)


knit_report <- drake_plan(
  report = rmarkdown::render(
    input = knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  ) 
)

plan <- bind_rows(
  ecdc_data,
  plots,
  save_figures,
  testing,
  excess,
  staging,
  knit_report
)
