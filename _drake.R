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
  covid_sel = covid %>% filter(country %in% countries_eu) %>% filter(cases > 0) %>% mutate(country = factor(country, levels=countries_eu))
)

plots <- drake_plan(
  plot_cases = basic_plot(covid_sel),
  plot_deaths = basic_plot(covid_sel %>% filter(deaths>0), y="deaths"),
  plot_cases_pop = basic_plot(covid_sel, y="cases_pop"),
  plot_deaths_pop = basic_plot(covid_sel %>% filter(deaths>0), y="deaths_pop"),
  plot_shifted_cases = plot_shifted(covid_sel, what="cases"),
  plot_shifted_deaths = plot_shifted(covid_sel, what="deaths", val.min=10),
  plot_shifted_cases_pop = plot_shifted(covid_sel, what="cases_pop", val.min=0.1, val.max=10),
  plot_shifted_deaths_pop = plot_shifted(covid_sel, what="deaths_pop", val.min=0.02, val.max=1),
  plot_doubling_cases = plot_doubling_times(covid_sel, what="cases", val.min=100),
  plot_doubling_deaths = plot_doubling_times(covid, what="deaths", val.min=10),
  plot_uk = plot_country(covid, cntry="United Kingdom"),
  plot_us = plot_country(covid, cntry="United States"),
  plot_new_cases = plot_daily_cases(covid, what="new_cases"),
  plot_new_deaths = plot_daily_cases(covid, what="new_deaths", ncol=5, val.min = 1),
  plot_italy_fit = plot_country_fit(covid, "Italy", val.max=2000),
  plot_spain_fit = plot_country_fit(covid, "Spain", val.max=1000),
  plot_diff_italy = plot_derivative(covid, cntry="Italy"),
  plot_diff_uk = plot_derivative(covid, cntry="United Kingdom"),
  plot_ratio = plot_death_ratio(covid, mortality=0.034),
  plot_cases_deaths = plot_cases_diff_deaths(covid),
  plot_cases_deaths_pop = plot_cases_diff_deaths(covid, pop=TRUE),
  
  plot_daily_italy = plot_daily(covid, cntry="Italy"),
  plot_daily_spain = plot_daily(covid, cntry="Spain"),
  plot_daily_uk = plot_daily(covid, cntry="United Kingdom"),
  plot_daily_us = plot_daily(covid, cntry="United States"),
  
  plot_daily_cases_china = plot_daily(covid, "China", what="new_cases_pop", span=0.5),
  
  plot_japan = plot_grid(
    plot_country_1(covid, cntry="Japan", "cases_pop", val.min=0.1, val.max=10, shft=13) + ggtitle("Japan"),
    plot_country_1(covid, cntry="Japan", "deaths_pop", val.min=0.007, val.max=0.5, shft=6),
    nrow=1
  )
)

figs <- plots %>% 
  select(-command, name = target) %>% 
  mutate(
    obj = rlang::syms(name),
    filename = paste0("fig/", str_remove(name, "plot_"), ".png"),
    width = 6,
    height = 4
  ) %>% 
  mutate(height = if_else(str_detect(name, "plot_cases_deaths"), 8, height))

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