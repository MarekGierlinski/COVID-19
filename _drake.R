suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(glue)
  library(broom)
  library(drake)
})

source("R/func.R")

if(!dir.exists("fig")) dir.create("fig")

read_data <- drake_plan(
  covid_raw = read_covid(),
  covid = process_covid(covid_raw),
  covid_sel = covid %>% filter(country %in% countries_eu) %>% filter(cases > 0) %>% mutate(country = factor(country, levels=countries_eu))
)

find_shifts <- drake_plan(
  shifts_cases = linear_shifts(covid, "cases"),
  shifts_deaths = linear_shifts(covid, "deaths",  val.min=10)
)

plots <- drake_plan(
  plot_cases = basic_plot(covid_sel),
  plot_shifted_cases = plot_shifted(covid_sel, what="cases"),
  plot_shifted_deaths = plot_shifted(covid_sel, what="deaths", val.min=10),
  plot_doubling_cases = plot_doubling_times(covid, what="cases", val.min=100, lab="Reported cases"),
  plot_doubling_deaths = plot_doubling_times(covid, what="deaths", val.min=10, lab="Reported deaths"),
  plot_uk = plot_country(covid, cntry="United Kingdom", what="cases", val.min=100, ylab="Reported cases"),
  plot_us = plot_country(covid, cntry="United States", what="cases", val.min=100, ylab="Reported cases")
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
  find_shifts,
  plots,
  save_figures
)

cfg <- drake_config(plan)