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
  covid_raw = read_covid(),
  covid = process_covid(covid_raw),
  covid_sel = covid %>% filter(country %in% countries_eu) %>% filter(cases > 0) %>% mutate(country = factor(country, levels=countries_eu))
)

plots <- drake_plan(
  plot_cases = basic_plot(covid_sel),
  plot_deaths = basic_plot(covid_sel %>% filter(deaths>0), y="deaths"),
  plot_shifted_cases = plot_shifted(covid_sel, what="cases"),
  plot_shifted_deaths = plot_shifted(covid_sel, what="deaths", val.min=10),
  plot_doubling_cases = plot_doubling_times(covid, what="cases", val.min=100),
  plot_doubling_deaths = plot_doubling_times(covid, what="deaths", val.min=10),
  plot_uk_cases = plot_country(covid, cntry="United Kingdom", what="cases", val.min=100),
  plot_uk_deaths = plot_country(covid, cntry="United Kingdom", what="deaths", val.min=10),
  plot_us_cases = plot_country(covid, cntry="United States", what="cases", val.min=100),
  plot_us_deaths = plot_country(covid, cntry="United States", what="deaths", val.min=10),
  plot_new_cases = plot_daily_cases(covid, what="new_cases"),
  plot_new_deaths = plot_daily_cases(covid, what="new_deaths", ncol=5, val.min = 1),
  plot_italy_fit = plot_country_fit(covid, "Italy", val.max=2000),
  plot_spain_fit = plot_country_fit(covid, "Spain", val.max=1000),
  plot_diff_italy = plot_derivative(covid, cntry="Italy"),
  plot_diff_uk = plot_derivative(covid, cntry="United Kingdom"),
  plot_diff_italy_deaths = plot_derivative(covid, cntry="Italy", what="deaths", val.min=10, span=1.3)
)

figs <- plots %>% 
  select(-command, name = target) %>% 
  mutate(
    obj = rlang::syms(name),
    filename = paste0("fig/", str_remove(name, "plot_"), ".png")
  )

save_figures <- drake_plan(
  figures = target(
    command = ggsave(filename, obj, device="png", width=6, height=3),
    transform = map(.data = !!figs)
  )
)

plan <- bind_rows(
  read_data,
  plots,
  save_figures
)

cfg <- drake_config(plan)