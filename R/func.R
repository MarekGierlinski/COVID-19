countries_eu <- c("Italy", "United Kingdom", "France", "Germany", "Netherlands", "Sweden", "Switzerland", "Spain")
shapes <- c(15:18, 0:14)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey30", "grey70", "black")


read_covid <- function() {
  yesterday <- Sys.Date() - 1
  url <- glue("https://ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{yesterday}.xls")
  tmp <- tempfile()
  download.file(url, tmp, mode="wb")
  read_excel(tmp)
}

process_covid <- function(cvd) {
  cvd %>% 
    mutate(CountryExp = recode(CountryExp, `United kingdom` = "United Kingdom")) %>% # spelling mistake
    mutate(date = as.Date(DateRep, format="%d/%m/%Y")) %>% 
    rename(country = CountryExp, new_cases = NewConfCases) %>% 
    group_by(country) %>% 
    arrange(date) %>% 
    mutate(tot_cases = sum(new_cases), cases = cumsum(new_cases)) %>% 
    mutate(deaths = cumsum(NewDeaths)) %>% 
    ungroup() %>% 
    filter(tot_cases > 200 & country != "Cases on an international conveyance Japan") %>% 
    mutate(country = recode(country, "United States of America" = "United States"))
}

basic_plot <- function(d, x="date", y="cases", xlab="Date", ylab="Cases", palette=cbPalette, shps=shapes, point.size=1) {
  brks <- rep(c(1, 2, 5), 5) * 10^sort(rep(0:4,3))
  d %>% 
    ggplot(aes_string(x=x, y=y, colour="country", shape="country", group="country")) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank()
    ) +
    geom_line() +
    geom_point(size=point.size) +
    scale_colour_manual(values=palette) +
    scale_shape_manual(values=shps) +
    scale_y_log10(breaks=brks) +
    labs(x=xlab, y=ylab)
}

shift_days <- function(d, shifts, what="cases", val.min=100) {
  d %>% 
    filter(!!sym(what) > val.min) %>% 
    left_join(shifts, by="country") %>% 
    mutate(days = as.integer(date) - shift - ref)
}

plot_shifted <- function(cvd, what="cases", val.min=100) {
  shifts <- linear_shifts(cvd, what=what, val.min=val.min)
  cvd %>% 
    mutate(value = !!sym(what)) %>% 
    filter(value > val.min) %>% 
    left_join(shifts, by="country") %>% 
    mutate(days = as.integer(date) - shift - ref) %>% 
    basic_plot(x="days", y="value", xlab="Normalized day", ylab=what)
}

linear_shifts <- function(cvd, what="cases", base_country = "Italy", val.min=100) {
  dat <- cvd %>%
    mutate(value = !!sym(what)) %>% 
    filter(value > val.min & value < 10000) %>%
    mutate(lval = log2(value), day = as.integer(date)) %>% 
    select(country, lval, day)
  
  finv <- lm(day ~ lval, data=filter(dat, country==base_country))
  ref_day0 <- finv$coefficients[1]
  ref_slope <- finv$coefficients[2]
  ref_10000 <- predict(finv, data.frame(lval = log2(10000)))
  
  shifts <- dat %>% 
    group_by(country) %>% 
    summarise(shift = mean(day - ref_slope * lval) - ref_day0) %>% 
    mutate(date10000 = as.Date(ref_10000 + shift, origin=as.Date("1970/01/01"))) %>% 
    arrange(date10000) %>% 
    mutate(ref = ref_day0)
}

get_doubling_times <- function(cvd, what="cases", val.min=100) {
  cvd %>% 
    mutate(value = !!sym(what)) %>% 
    filter(value > val.min & cases < 10000 & country != "China") %>% 
    mutate(lval = log2(value), day = as.integer(date)) %>% 
    group_by(country) %>%
    group_split() %>% 
    map_dfr(function(w) {
      if(nrow(w) < 5) return(NULL)
      fit <-  lm(day ~ lval, data=w)
      coef <- summary(fit)$coefficients
      conf <- confint(fit)
      tibble(
        country = first(w$country),
        slope = coef[2, 1],
        lo = conf[2, 1],
        up = conf[2, 2]
      )
    }) %>% 
    arrange(slope) %>% 
    mutate(country = as_factor(country))
}

plot_doubling_times <- function(cvd, what="cases", val.min=100, lab="Reported cases") {
  get_doubling_times(cvd, what, val.min=val.min) %>% 
    ggplot(aes(x=country, y=slope, ymin=lo, ymax=up)) +
    theme_bw() +
    geom_errorbar(width=0.3) +
    geom_point() +
    coord_flip() +
    labs(x=NULL, y=glue::glue("{lab} doubling time (day)"))
}


plot_country <- function(cvd, cntry="United Kingdom",
                         what="cases", val.min=100, ylab="Reported cases") {
  shifts <- linear_shifts(cvd, what=what, val.min=val.min)
  d <- cvd %>%
    shift_days(shifts, what = what, val.min = val.min)
  d_eu <- d %>% filter(country %in% countries_eu)
  d_sel <- d %>% filter(country == cntry)
  basic_plot(d_eu, x="days", y=what, xlab="Normalized day", ylab=ylab, palette=rep("grey",10), shps=rep(1, 10)) +
    theme(legend.position = "none") +
    geom_point(data=d_sel, shape=21, fill="royalblue", colour="black", size=2.5) +
    geom_line(data=d_sel, colour="black")
}