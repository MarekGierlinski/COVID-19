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
  url_covid = get_url(),
  covid_raw = read_covid(url_covid),
  covid = process_covid(covid_raw),
  covid_sel = covid %>% filter(country %in% countries_eu) %>% filter(cases > 0) %>% mutate(country = factor(country, levels=countries_eu))
)

plots <- drake_plan(
  plot_cases = basic_plot(covid_sel),
  plot_deaths = basic_plot(covid_sel %>% filter(deaths>0), y="deaths"),
  plot_shifted_cases = plot_shifted(covid_sel, what="cases"),
  plot_shifted_deaths = plot_shifted(covid_sel, what="deaths", val.min=10),
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
  
  plot_aus = plot_country(covid, cntry="Australia")
 )

figs <- plots %>% 
  select(-command, name = target) %>% 
  mutate(
    obj = rlang::syms(name),
    filename = paste0("fig/", str_remove(name, "plot_"), ".png")
  )

save_figures <- drake_plan(
  figures = target(
    #command = ggsave(filename, obj, device="png", width=6, height=3),
    command = annotate_save(filename, obj, url_covid),
    transform = map(.data = !!figs)
  )
)

plan <- bind_rows(
  read_data,
  plots,
  save_figures
)

cfg <- drake_config(plan)