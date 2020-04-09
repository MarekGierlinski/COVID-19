countries_sel <- c("Italy", "Spain",  "France", "Germany", "United Kingdom", "Switzerland", "Netherlands",  "Norway", "Belgium",  "Sweden",  "Austria", "Portugal", "Turkey")
countries_sel <- c("Italy", "Spain",  "France", "Germany", "United Kingdom", "United States")


shapes <- c(15:18, 0:14)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey20", "grey40", "grey60", "grey80", "grey90", "black")

get_url <- function() {
  yesterday <- Sys.Date() - 1
  urlc <- glue("https://ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{yesterday}.xlsx")
  stopifnot(RCurl::url.exists(urlc))
  urlc
}

read_covid <- function(urlc) {
  tmp <- tempfile()
  download.file(urlc, tmp, mode="wb")
  readxl::read_excel(tmp)
}

# Read and process data from World Bank
# https://data.worldbank.org/indicator/SP.POP.TOTL
read_population <- function(file) {
  read_csv(file, skip=4, col_types = cols()) %>% 
    select(
      country = `Country Name`,
      id = `Country Code`,
      population = `2018`
    ) %>% 
    mutate(country = recode(country,
      "Egypt, Arab Rep." = "Egypt",
      "Iran, Islamic Rep." = "Iran",
      "Russian Federation" = "Russia",
      "Slovak Republic" = "Slovakia",
      "Korea, Rep." = "South Korea"
    ))
}

process_covid <- function(cvd, pop) {
  cvd %>% 
    rename(
      country = `countriesAndTerritories`,
      new_cases = cases,
      new_deaths = deaths,
      id = geoId
    ) %>%
    mutate(country = str_replace_all(country, "_", " ")) %>% 
    mutate(country = recode(country,
      'United States of America' = "United States",
      "CANADA" = "Canada"
    )) %>%
    mutate(date = as.Date(dateRep, format="%d/%m/%Y")) %>% 
    group_by(country) %>% 
    arrange(date) %>% 
    mutate(tot_cases = sum(new_cases), cases = cumsum(new_cases)) %>% 
    mutate(deaths = cumsum(new_deaths)) %>% 
    ungroup() %>% 
    filter(
      tot_cases > 100 &
      country != "Cases on an international conveyance Japan"
    ) %>% 
    left_join(pop, by="country") %>% 
    mutate(
      cases_pop = 1e5 * cases / population,
      deaths_pop = 1e5 * deaths/ population,
      new_cases_pop = 1e5 * new_cases / population,
      new_deaths_pop = 1e5 * new_deaths/ population
    )
}

basic_plot <- function(d, x="date", y="cases", xlab="Date", ylab=NULL, palette=cbPalette, shps=shapes, point.size=1, shft=0) {
  brks <- c(1, 2, 5) * 10^sort(rep(-4:6,3))
  labs <- sprintf("%f", brks) %>% str_remove("0+$") %>% str_remove("\\.$") %>% prettyNum(big.mark = ",") %>% str_remove("^\\s+")
  if(is.null(ylab)) {
    yl <- y
    if(str_detect(yl, "_pop")) yl <- paste(str_remove(yl, "_pop"), "per 100,000")
    ylab = glue("Reported {yl}")
  }
  d[[x]] <- d[[x]] - shft
  d %>% 
    ggplot(aes_string(x=x, y=y, colour="country", shape="country", group="country")) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      legend.key.size = unit(0.4, "cm"),
      legend.title=element_blank()
    ) +
    geom_line() +
    geom_point(size=point.size) +
    scale_colour_manual(values=palette) +
    scale_shape_manual(values=shps) +
    scale_y_log10(breaks=brks, labels=labs) +
    labs(x=xlab, y=ylab)
}

shift_days <- function(d, shifts, what="cases", val.min=100) {
  d %>% 
    filter(!!sym(what) >= val.min) %>% 
    left_join(shifts, by="country") %>% 
    mutate(days = as.integer(date) - shift - ref)
}

plot_shifted <- function(cvd, what="cases", val.min=100, val.max=1000, base_country="Italy") {
  shifts <- linear_shifts(cvd, what=what, val.min=val.min, val.max=val.max, base_country = base_country)
  cvd %>% 
    filter(!!sym(what) >= val.min) %>% 
    left_join(shifts, by="country") %>% 
    mutate(days = as.integer(date) - shift - ref) %>% 
    basic_plot(x="days", y=what, xlab="Relative day") +
      geom_hline(yintercept=c(val.min, val.max), colour="royalblue", linetype="dashed")
}

linear_shifts <- function(cvd, what="cases", base_country = "Italy", val.min=100, val.max=1000) {
  dat <- cvd %>%
    mutate(value = !!sym(what)) %>% 
    filter(value >= val.min & value <= val.max) %>%
    mutate(lval = log2(value), day = as.integer(date)) %>% 
    select(country, lval, day)
  
  finv <- lm(day ~ lval, data=filter(dat, country==base_country))
  ref_day0 <- finv$coefficients[1]
  ref_day_vmin <- predict(finv, data.frame(lval=log2(val.min)))
  ref_slope <- finv$coefficients[2]

  shifts <- dat %>% 
    group_by(country) %>% 
    summarise(shift = mean(day - ref_slope * lval) - ref_day0) %>% 
    arrange(shift) %>% 
    mutate(ref = ref_day_vmin)
}

get_doubling_times <- function(cvd, what="cases", val.min=100, val.max=10000) {
  cvd %>% 
    mutate(value = !!sym(what)) %>% 
    filter(value >= val.min & value < val.max & country != "China") %>% 
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
    mutate(country = factor(country, levels=as.character(country)))
}

plot_doubling_times <- function(cvd, what="cases", val.min=100, val.max=1000) {
  get_doubling_times(cvd, what, val.min=val.min, val.max=val.max) %>% 
    ggplot(aes(x=country, y=slope, ymin=lo, ymax=up)) +
    theme_bw() +
    geom_errorbar(width=0.3) +
    geom_point() +
    coord_flip() +
    labs(x=NULL, y=glue::glue("Reported {what} doubling time (day)"))
}


plot_country_1 <- function(cvd, cntry="United Kingdom",
                         what="cases", val.min=100, val.max=1000, title="", shft=0) {
  shifts <- linear_shifts(cvd, what=what, val.min=val.min, val.max=val.max)
  d <- cvd %>%
    shift_days(shifts, what = what, val.min = val.min)
  d_eu <- d %>% filter(country %in% countries_sel)
  d_sel <- d %>% filter(country == cntry)
  basic_plot(d_eu, x="days", y=what, xlab="Relative day",
             palette=rep("grey",100), shps=rep(1, 100), shft=shft) +
    ggtitle(title) +
    theme(legend.position = "none") +
    geom_point(data=d_sel, shape=21, fill="royalblue", colour="black", size=2)
    #geom_line(data=d_sel, colour="black")
}

plot_country_fit <- function(cvd, cntry="Italy",
                         what="cases", val.min=100, val.max=10000) {
  shifts <- linear_shifts(cvd, what=what, val.min=val.min)
  d <- cvd %>%
    filter(country == cntry) %>% 
    shift_days(shifts, what = what, val.min = val.min) %>%
    mutate(value = !!sym(what), lval = log10(value)) 
  df <- d %>% filter(value >= val.min & value <= val.max)
  fit <- lm(lval ~ days, data=df)
  x <- d$days
  pred <- predict(fit, tibble(days=x), interval="confidence", level=0.95) %>% 
    as_tibble() %>% 
    mutate(days = x)

  ggplot(d, aes(x=days, y=lval)) +
    theme_bw() +
    geom_ribbon(data = pred, aes(x=days, y=NULL, ymin=lwr, ymax=upr), fill="grey70", alpha=0.3) +
    geom_line(data = pred, aes(x=days, y=fit, group=1), colour="grey30") +
    geom_point(size=1.5)+
    labs(x="Relative day", y=glue::glue("log10 {what}"), title=cntry)
}

plot_daily_cases <- function(cvd, what="new_cases", val.min=5, point.size=0.4, text.size=7, ncol=9, min.date=as.Date("2020-01-10")) {
  swhat <- str_remove(what, "new_")
  cvd %>% 
    filter(date > min.date) %>% 
    mutate(value = !!sym(what)) %>% 
    filter(value > val.min) %>% 
  ggplot(aes(x=date, y=value, group=country)) +
    #geom_line(colour="grey70") +
    geom_point(size=point.size) +
    geom_smooth(method="loess", formula="y ~ x", span=1.5, se=FALSE, colour="red", alpha=0.2, size=0.5) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
      axis.text.x = element_text(size = text.size-2),
      text = element_text(size = text.size),
      panel.spacing=unit(0, "cm")
    ) +
    scale_y_log10(limits=c(val.min,10^4)) +
    scale_x_date(guide = guide_axis(check.overlap = TRUE), breaks=as.Date(c("2020-02-01", "2020-03-01")), labels=c("1 Feb", "1 Mar")) +
    facet_wrap(~ country, scales="fixed", ncol=ncol) +
    labs(x = NULL, y = glue::glue("New reported daily {swhat}"))
}


plot_derivative_1 <- function(cvd, cntry="United Kingdom",
                         what="cases", val.min=100, span=2, title="") {
  shifts <- linear_shifts(cvd, what=what, val.min=val.min)
  d <- cvd %>%
    filter(country == cntry) %>% 
    shift_days(shifts, what = what, val.min = val.min) %>%
    mutate(value = !!sym(what), lval = log2(value))
  fit <- loess(lval ~ days, data=d, span=span)
  xs <- seq(min(d$days), max(d$days), 0.1)
  pred_loess <- predict(fit, tibble(days=xs), se=TRUE) 
  pred <- tibble(
    x = xs,
    y = pred_loess$fit,
    se = pred_loess$se.fit,
    y_lo = y - se,
    y_up = y + se
  )
  dif <- tibble(
    x = rowMeans(embed(xs, 2)),
    y = diff(pred$y) / diff(pred$x),
    dbl = 1 / y
  )

  xlim <- c(min(xs), max(xs))
  g1 <- ggplot(d) +
    theme_bw() +
    geom_point(aes(x=days, y=lval / log2(10))) +
    geom_line(data=pred, aes(x=x, y=y / log2(10))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(x=NULL, y=glue::glue("log10 {what}"), title=title) +
    xlim(xlim)
  g2 <- ggplot(dif, aes(x=x, y=dbl)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    geom_line() +
    labs(x = "Relative day", y = "Doubling time") +
    xlim(xlim)
  plot_grid(g1, g2, ncol=1, rel_heights = c(1,0.6), align="v")
  
}

plot_derivative <- function(cvd, cntry="United Kingdom", span=2) {
  g1 <- plot_derivative_1(cvd, cntry, "cases", val.min=100, span=span, title=cntry)
  g2 <- plot_derivative_1(cvd, cntry, "deaths", val.min=10, span=span)
  plot_grid(g1, g2, nrow=1)
}


plot_country <- function(cvd, cntry="United Kingdom") {
  g1 <- plot_country_1(cvd, cntry, "cases_pop", val.min=0.1, val.max=10, title=cntry)
  g2 <- plot_country_1(cvd, cntry, "deaths_pop", val.min=0.02, val.max=1)
  plot_grid(g1, g2, nrow=1)
}

plot_death_ratio <- function(cvd, text.size=8, mortality=0.034, min.cases=100, min.deaths=30) {
  d <- cvd %>% 
    group_by(country) %>% 
    summarise(deaths = sum(new_deaths), cases = sum(new_cases)) %>% 
    filter(deaths >= min.deaths & cases >= min.cases) %>% 
    mutate(ratio = deaths / cases) %>% 
    arrange(ratio) %>% 
    mutate(country = as_factor(country)) %>% 
    filter(deaths > 0) %>% 
    mutate(rate = map2(deaths, cases, ~prop.test(.x, .y, conf.level=0.95) %>% broom::tidy())) %>%
    unnest(rate)
  
  g <- ggplot(d, aes(x=country, y=estimate, ymin=conf.low, ymax=conf.high)) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      text = element_text(size = text.size)
    ) +
    geom_errorbar(width=0.3) +
    geom_point() +
    coord_flip() +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(d$conf.high) * 1.03)) +
    labs(x=NULL, y="Reported deaths / reported cases") +
    geom_hline(yintercept = mortality, colour="red", linetype="dashed")
  g
}


annotate_save <- function(filename, g, lab, width=6, height=3) {
  ann <- ggplot() +
    xlim(0, 1) +
    annotate("text", label=glue::glue("Source: {lab}"), hjust=0, x=0, y=0, size=2, colour="grey50") +
    theme_nothing() +
    theme(plot.margin = unit(c(-5,0,-5,-5), "mm"))
  plt <- plot_grid(g ,ann, ncol=1, rel_heights = c(20, 1))
  ggsave(filename, plt, device="png", width=width, height=height)
}


plot_cases_diff_deaths <- function(cvd, pop=FALSE) {
  brkmin <- 0
  xmin <- 0.05
  xlab <- "Reported deaths and cases"
  if(pop) {
    cvd <- mutate(cvd, new_cases = new_cases_pop, new_deaths = new_deaths_pop)
    xmin <- 0.0005
    xlab <- paste(xlab, "per 100,000")
    brkmin <- -3
  }
  
  brks <- c(1) * 10^sort(rep(brkmin:6,3))
  labs <- sprintf("%f", brks) %>% str_remove("0+$") %>% str_remove("\\.$") %>% prettyNum(big.mark = ",") %>% str_remove("^\\s+")
  
  d <- cvd %>%
    group_by(country) %>%
    summarise(cases_tot = sum(new_cases), deaths_tot = sum(new_deaths)) %>%
    filter(deaths_tot > 0) %>% 
    arrange(deaths_tot) %>%
    mutate(country=as_factor(country), lab=paste0(as.character(country), "  "))
  ggplot(d, aes(y=country)) +
    theme_bw() +
    geom_segment(aes(x=deaths_tot, xend=cases_tot, y=country, yend=country), colour="grey50") +
    geom_point(aes(x = cases_tot), shape=21, fill="blue", size=1.5, colour="grey50") +
    geom_point(aes(x = deaths_tot), shape=21, fill="red", size=1.5, colour="grey50") +
    geom_text(aes(x=deaths_tot, y=country, label=lab), hjust=1, size=2.5) +
    scale_x_log10(breaks=brks, labels=labs, limits=c(xmin, max(d$cases_tot)*1.05)) +
    scale_y_discrete(expand=c(0,1)) +
    labs(x=xlab, y=NULL) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      text = element_text(size=8),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}


plot_two_countries <- function(cvd, cntry1 = "Italy", cntry2 = "United Kingdom", what="cases", val.min=100, shift=0) {
  d <- cvd %>% 
    mutate(value = !!sym(what), lval = log10(value), day = as.integer(date)) %>% 
    filter(country %in% c(cntry1, cntry2) & value >= val.min) %>% 
    select(country, day, lval) %>% 
    mutate(day = as.numeric(day)) %>% 
    mutate(day = if_else(country == cntry2, day - shift, day))
  x <- seq(min(d$day), max(d$day), 0.1)
  f <- d %>% 
    group_by(country) %>% 
    group_split() %>% 
    map_dfr(function(w) {
      f <- lm(lval ~ poly(day, 2), data=w)
      tibble(
        country = w[1, ]$country,
        x = x,
        y = predict(f, tibble(day = x))
      )
    })
  ggplot() +
    theme_bw() +
    geom_point(data=d, aes(x=day, y=lval, colour=country)) +
    geom_line(data=f, aes(x=x, y=y, colour=country))
}


plot_daily <- function(cvd, countries, what="deaths", date_min="2020-03-01", ncol=2) {
  if(what=="deaths") {
    cvd$y <- cvd$new_deaths_pop
  } else {
    cvd$y <- cvd$new_cases_pop
  }
  d <- cvd %>%
    filter(country %in% countries) %>%
    filter(date >= as.Date(date_min))
  bl <- d %>%
    group_by(country) %>%
    summarise(maxy = max(y) * 1.1, date=as.Date(date_min))
  
  ggplot() +
    geom_blank(data=bl, aes(x=date, y=maxy)) +
    geom_segment(data=d, aes(x=date, xend=date, y=0, yend=y), colour="grey70") +
    geom_point(data=d, aes(x=date, y=y)) +
    scale_y_continuous(expand=c(0,0)) +
    facet_wrap(~country, ncol=ncol, scales ="free_y") +
    theme_bw() +
    theme(
      panel.grid = element_blank()
    ) +
    labs(x=NULL, y=glue("Daily {what} per 100,000"))
}

plot_heatmap <- function(cvd, what="new_cases") {
  brks <- c(1, 2, 5) * 10^sort(rep(0:4,3))
  labs <- sprintf("%f", brks) %>% str_remove("0+$") %>% str_remove("\\.$")
  lbrks <- log10(brks)
  
  cvd %>%
    mutate(value = !!sym(what), lval = log10(value)) %>% 
    filter(value > 5) %>%
  ggplot(aes(x=date, y=country, fill=lval)) +
    theme_bw() +
    geom_tile() +
    scale_fill_viridis_c(breaks=lbrks, labels=labs, option="cividis")
}

plot_death_excess <- function(cvd, cntry="United Kingdom", base_country="South Korea", val.min=0.005, val.max=0.02, cntry_short=NULL) {
  if(is.null(cntry_short)) cntry_short <- cntry
  
  shifts <- linear_shifts(cvd, what="deaths_pop", val.min=val.min, val.max=val.max, base_country = base_country)
  cvd_sel <- cvd %>% 
    filter(country %in% c(cntry, base_country)) %>% 
    filter(deaths_pop >= val.min) %>% 
    left_join(shifts, by="country") %>% 
    mutate(days = as.integer(date) - shift - ref) %>% 
    select(country, date, days, deaths, deaths_pop, population)
  
  fit <- loess(log10(deaths_pop) ~ days, data=cvd_sel %>% filter(country == base_country))
  
  cvd_cntry <- cvd_sel %>% filter(country == cntry)
  cvd_pred <- cvd_cntry %>% 
    mutate(deaths_pred = 10^predict(fit, cvd_cntry) * population / 1e5) %>% 
    drop_na() %>% 
    mutate(deaths_ex = as.integer(deaths - deaths_pred)) %>% 
    filter(deaths_ex >= 0)
    
  
  xlims <- c(0, max(cvd_sel$days))  
  g1 <- basic_plot(cvd_sel, x="days", y="deaths_pop",xlab=NULL,  ylab="Cumulative deaths per 100,000") +
    scale_x_continuous(limits=xlims) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      plot.margin = margin(0, 0, -2, 0, "pt")
    )
  
  g2 <- ggplot(cvd_pred, aes(x=days, y=deaths_ex)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    geom_segment(aes(xend=days, yend=0), colour="grey80") +
    geom_point() +
    #scale_y_log10() +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(cvd_pred$deaths_ex)*1.05)) +
    scale_x_continuous(limits = xlims) +
    labs(x="Relative day", y=glue("Excess deaths in {cntry_short}"))
  
  plot_grid(g1, g2, align="v", ncol=1, rel_heights = c(1.5,1))
}