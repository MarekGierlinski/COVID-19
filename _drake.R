suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(glue)
  library(broom)
  library(drake)
  library(cowplot)
})

source("R/func.R")

if(!dir.exists("fig")) dir.create("fig")

read_data <- drake_plan(
  population = read_population("API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv"),
  url_covid = get_url(),
  covid_raw = read_covid(url_covid),
  covid = process_covid(covid_raw, population),
  covid_sel = covid %>% filter(country %in% countries_sel) %>% filter(cases > 0) %>% mutate(country = factor(country, levels=countries_sel))
)

plots <- drake_plan(
  fig_cases = basic_plot(covid_sel),
  fig_deaths = basic_plot(covid_sel %>% filter(deaths>0), y="deaths"),
  fig_cases_pop = basic_plot(covid_sel, y="cases_pop"),
  fig_deaths_pop = basic_plot(covid_sel %>% filter(deaths>0), y="deaths_pop"),
  fig_shifted_cases = plot_shifted(covid_sel, what="cases"),
  fig_shifted_deaths = plot_shifted(covid_sel, what="deaths", val.min=10),
  fig_shifted_cases_pop = plot_shifted(covid_sel, what="cases_pop", val.min=0.1, val.max=2),
  fig_shifted_deaths_pop = plot_shifted(covid_sel, what="deaths_pop", val.min=0.02, val.max=0.5),
  #fig_doubling_cases = plot_doubling_times(covid_sel, what="cases", val.min=100),
  #fig_doubling_deaths = plot_doubling_times(covid, what="deaths", val.min=10),
  fig_uk = plot_country(covid, cntry="United Kingdom"),
  fig_us = plot_country(covid, cntry="United States"),
  fig_new_cases = plot_daily_cases(covid, what="new_cases"),
  fig_new_deaths = plot_daily_cases(covid, what="new_deaths", ncol=5, val.min = 1),
  fig_italy_fit = plot_country_fit(covid, "Italy", val.max=2000),
  fig_spain_fit = plot_country_fit(covid, "Spain", val.max=1000),
  fig_diff_italy = plot_derivative(covid, cntry="Italy"),
  fig_diff_uk = plot_derivative(covid, cntry="United Kingdom"),
  fig_ratio = plot_death_ratio(covid, mortality=0.034),
  fig_cases_deaths = plot_cases_diff_deaths(covid),
  fig_cases_deaths_pop = plot_cases_diff_deaths(covid, pop=TRUE),
  
  fig_daily_deaths = plot_daily_deaths(covid, countries_sel),

  fig_japan = plot_grid(
    plot_country_1(covid, cntry="Japan", "cases_pop", val.min=0.1, val.max=10, shft=13) + ggtitle("Japan"),
    plot_country_1(covid, cntry="Japan", "deaths_pop", val.min=0.007, val.max=0.5, shft=6),
    nrow=1
  )
)

figs <- plots %>% 
  select(-command, name = target) %>% 
  mutate(
    obj = rlang::syms(name),
    filename = paste0("fig/", str_remove(name, "fig_"), ".png"),
    width = 6,
    height = 4
  ) %>% 
  mutate(height = if_else(str_detect(name, "fig_cases_deaths"), 8, height))

save_figures <- drake_plan(
  figures = target(
    command = annotate_save(filename, obj, url_covid, width=width, height=height),
    transform = map(.data = !!figs)
  )
)

plan <- bind_rows(
  read_data,
  plots,
  save_figures
)

cfg <- drake_config(plan)