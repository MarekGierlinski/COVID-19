suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(glue)
  library(broom)
  library(drake)
  library(cowplot)
  library(ggrepel)
})

source("R/func.R")

if(!dir.exists("fig")) dir.create("fig")

read_data <- drake_plan(
  url_covid = get_url(),
  covid_raw = read_covid(url_covid),
  covid = process_covid(covid_raw),
  covid_sel = covid %>% filter(country %in% countries_sel) %>% filter(cases > 0) %>% mutate(country = factor(country, levels=countries_sel)),
  covid_europe = covid %>% filter(id %in% europe),
  ons = read_ons()
)

plots <- drake_plan(
  fig_cases = basic_plot(covid_sel),
  fig_deaths = basic_plot(covid_sel %>% filter(deaths>0), y="deaths"),
  fig_cases_pop = basic_plot(covid_sel, y="cases_pop"),
  fig_deaths_pop = basic_plot(covid_sel %>% filter(deaths>0), y="deaths_pop"),
  fig_shifted_cases = plot_shifted(covid_sel, what="cases"),
  fig_shifted_deaths = plot_shifted(covid_sel, what="deaths", val.min=10),
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
  fig_diff_uk = plot_derivative(covid, cntry="United Kingdom"),
  fig_ratio = plot_death_ratio(covid, mortality=NULL),
  fig_cases_deaths = plot_cases_diff_deaths(covid),
  fig_cases_deaths_pop = plot_cases_diff_deaths(covid, pop=TRUE),
  fig_eu = plot_cases_diff_deaths(covid_europe, pop=TRUE, x.min=0.1),
  
  fig_daily_deaths = plot_daily(covid, countries_day, what="deaths", span=0.6),
  fig_daily_deaths_fixed = plot_daily(covid, countries_day, what="deaths", span=0.6, scls="fixed"),
  fig_daily_cases = plot_daily(covid, countries_day, what="cases", span=0.6),
  fig_daily_cases_fixed = plot_daily(covid, countries_day, what="cases", span=0.6, scls="fixed"),
  fig_shift_cases = plot_shifts(covid, countries_day),
  
  fig_daily_fits_cases = plot_daily_fits(covid, countries_day, what="cases", span=0.6),
  fig_daily_fits_deaths = plot_daily_fits(covid, countries_day, what="deaths", span=0.6),
  
  fig_japan = plot_grid(
    plot_country_1(covid, cntry="Japan", "cases_pop", val.min=1, val.max=100, shft=13) + ggtitle("Japan"),
    plot_country_1(covid, cntry="Japan", "deaths_pop", val.min=0.07, val.max=5, shft=6),
    nrow=1
  ),
  
  fig_uk_korea_excess = plot_death_excess(covid, cntry="United Kingdom", cntry_short="the UK", base_country = "South Korea", val.min=0.05, val.max=0.2),
  fig_uk_germany_excess = plot_death_excess(covid, cntry="United Kingdom", cntry_short="UK", base_country = "Germany", val.min=0.05, val.max=0.5),
  
  fig_deaths_population = plot_deaths_population(covid)
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
  mutate(height = if_else(str_detect(name, "new"), 10, height))

save_figures <- drake_plan(
  figures = target(
    command = annotate_save(filename, obj, url_covid, width=width, height=height),
    transform = map(.data = !!figs)
  )
)

ons_figures <- drake_plan(
  fig_ons_deaths = plot_ons_deaths(ons, max(ons$week)),
  ggsave("fig/ons_deaths.png", fig_ons_deaths, device="png", width=6, height=3)
)

plan <- bind_rows(
  read_data,
  plots,
  save_figures,
  ons_figures
)

cfg <- drake_config(plan)