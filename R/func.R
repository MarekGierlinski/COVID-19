countries_sel <- c("Italy", "Spain",  "France", "Germany", "United Kingdom", "Switzerland", "Netherlands",  "Norway", "Belgium",  "Sweden",  "Austria", "Portugal", "Turkey")
countries_sel <- c("Italy", "Spain",  "France", "Germany", "United Kingdom", "United States")
countries_day <- c(countries_sel, "Belgium", "Netherlands", "Ireland", "Switzerland", "Canada", "Sweden")
countries_2 <- c("Argentina", "Brazil", "Chile", "Colombia", "Mexico", "Egypt", "Iran", "Russia", "Pakistan", "India", "Bangladesh", "Indonesia")

europe <- "AL-AD-AT-BY-BE-BA-BG-HR-CZ-DK-EE-FI-FR-DE-EL-HU-IS-IE-IT-XK-LV-LT-LU-MT-NL-MD-ME-NO-PL-PT-RO-SM-ES-RS-SK-SI-SE-CH-UA-TR-UK" %>% str_split("-") %>% unlist()

shapes <- c(15:18, 0:14)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey20", "grey40", "grey60", "grey80", "grey90", "black")
wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

get_url <- function() {
  today <- Sys.Date()
  urlc <- glue("https://ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{today}.xlsx")
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

# http://data.un.org/Data.aspx?q=GDP+per+capita&d=SNAAMA&f=grID%3a101%3bcurrID%3aUSD%3bpcFlag%3a1
read_gdp <- function(file) {
  read_csv(file, col_types = cols()) %>% 
    set_names(c("country", "year", "item", "gdp")) %>% 
    select(country, gdp)
}

get_one_ons <- function(file, category) {
  read_tsv(file, col_types = cols()) %>%
    mutate(age = as_factor(age)) %>% 
    pivot_longer(-age, names_to = "week", values_to = "deaths") %>% 
    mutate(category = category, week=as.integer(week))
}

read_ons <- function() {
  bind_rows(
     get_one_ons("data/ons_all_deaths.txt", "all"),
     get_one_ons("data/ons_covid_deaths.txt", "covid")
  )
}

process_covid <- function(cvd, gdp) {
  cvd %>% 
    rename(
      country = `countriesAndTerritories`,
      new_cases = cases,
      new_deaths = deaths,
      id = geoId,
      population = popData2018
    ) %>%
    mutate(country = str_replace_all(country, "_", " ")) %>% 
    mutate(country = recode(country,
      'United States of America' = "United States",
      "CANADA" = "Canada"
    )) %>%
    mutate(
      date = as.Date(dateRep, format="%d/%m/%Y"),
      day_of_week = weekdays(date) %>% factor(levels=wd)
    ) %>% 
    group_by(country) %>% 
    arrange(date) %>% 
    mutate(
      tot_cases = sum(new_cases),
      cases = cumsum(new_cases),
      deaths = cumsum(new_deaths)
    ) %>% 
    ungroup() %>% 
    filter(
      tot_cases > 100 &
      country != "Cases on an international conveyance Japan"
    ) %>% 
    mutate(
      cases_pop = 1e6 * cases / population,
      deaths_pop = 1e6 * deaths/ population,
      new_cases_pop = 1e6 * new_cases / population,
      new_deaths_pop = 1e6 * new_deaths/ population
    ) %>% 
    left_join(gdp, by="country")
}

basic_plot <- function(d, x="date", y="cases", xlab="Date", ylab=NULL, palette=cbPalette, shps=shapes, point.size=1, shft=0, logy=TRUE) {
  brks <- c(1, 2, 5) * 10^sort(rep(-4:6,3))
  labs <- sprintf("%f", brks) %>% str_remove("0+$") %>% str_remove("\\.$") %>% prettyNum(big.mark = ",") %>% str_remove("^\\s+")
  if(is.null(ylab)) {
    yl <- y
    if(str_detect(yl, "_pop")) yl <- paste(str_remove(yl, "_pop"), "per million")
    ylab = glue("Reported {yl}")
  }
  d[[x]] <- d[[x]] - shft
  g <- d %>% 
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
    labs(x=xlab, y=ylab)
  if(logy) {
    g <- g + scale_y_log10(breaks=brks, labels=labs)
  } else {
    g <- g + scale_y_continuous(expand=c(0,0), limits=c(0, max(d[[y]])*1.03))
  }
  g
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
    #geom_smooth(method="loess", formula="y ~ x", span=1.5, se=FALSE, colour="red", alpha=0.2, size=0.5) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
      axis.text.x = element_text(size = text.size-2),
      text = element_text(size = text.size),
      panel.spacing=unit(0, "cm")
    ) +
    scale_y_log10(limits=c(val.min,40000)) +
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
  g1 <- plot_country_1(cvd, cntry, "cases_pop", val.min=1, val.max=100, title=cntry)
  g2 <- plot_country_1(cvd, cntry, "deaths_pop", val.min=0.2, val.max=10)
  plot_grid(g1, g2, nrow=1)
}

plot_death_ratio <- function(cvd, text.size=8, mortality=NULL, min.cases=100, min.deaths=30) {
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
    labs(x=NULL, y="Reported deaths / reported cases")
  if(!is.null(mortality)) g <- g + geom_hline(yintercept = mortality, colour="red", linetype="dashed")
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


plot_cases_diff_deaths <- function(cvd, pop=FALSE, x.min=NULL) {
  brkmin <- 0
  xmin <- 0.05
  xlab <- "Reported deaths and cases"
  if(pop) {
    cvd <- mutate(cvd, new_cases = new_cases_pop, new_deaths = new_deaths_pop)
    xmin <- 0.0005
    xlab <- paste(xlab, "per million")
    brkmin <- -3
  }
  if(!is.null(x.min)) xmin <- x.min
  
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

make_day_shifts <- function(cvd, cntrs, what, val.min, val.max, base_country) {
  shifts <- linear_shifts(cvd, what=glue("cases_pop"), val.min=val.min, val.max=val.max, base_country = base_country)
  
  d <- cvd %>%
    mutate(y = !!sym(glue("new_{what}_pop"))) %>% 
    filter(country %in% cntrs) %>%
    left_join(shifts, by="country") %>% 
    mutate(day = as.integer(date) - shift - ref) %>% 
    select(country, day, y) %>% 
    filter(day >= 0)
  
}

plot_daily <- function(cvd, cntrs, what="cases", val.min=1, val.max=20,
                       ncol=4, ymax=NA, span=0.75, base_country="Italy",
                       scls="free_y", n.iter=3, step=0.5, cut.day=3) {
  
  d <- make_day_shifts(cvd, cntrs, what, val.min, val.max, base_country) %>% 
    filter(y >=0)
  bl <- d %>%
    group_by(country) %>%
    summarise(maxy = max(y) * 1.05, day=0)
  if(!is.na(ymax)) bl$maxy <- ymax
  
  sm <- make_country_fits(d, cntrs, n.iter, span, step, cut.day)
  
  ggplot() +
    geom_blank(data=bl, aes(x=day, y=maxy)) +
    geom_segment(data=d, aes(x=day, xend=day, y=0, yend=y), colour="grey90", size=0.3) +
    geom_point(data=d, aes(x=day, y=y), size=0.2) +
    geom_line(data=sm, aes(x=day, y=y, group=country), size=0.9, alpha=0.8, colour=cbPalette[3]) +
    #stat_smooth(geom="line", data=d, aes(x=day, y=y), method="loess", span=span, se=FALSE, alpha=0.8, colour=cbPalette[3]) +
    #scale_y_continuous(expand=c(0,0), limits=c(0, ymax)) +
    coord_cartesian(expand=FALSE, ylim=c(0, ymax)) +
    facet_wrap(~country, ncol=ncol, scales =scls) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      text = element_text(size=8),
      strip.text.x = element_text(margin = margin(0.5,1,0.5,1, "mm")),
    ) +
    labs(x="Relative day", y=glue("Daily {what} per million"))
}

fit_loess_outliers <- function(w, n.iter=3, span=0.75) {
  for(i in 1:n.iter) {
    fit <- loess(y ~ day, data=w, span=span)
    w$diff <-  abs(w$y - predict(fit, w))
    imx <- which.max(w$diff)
    w <- w[-imx, ]
  }
  return(fit)
}

make_country_fits <- function(d, cntrs, n.iter=3, span=0.75, step=0.5, cut.day=3) {
  d %>% group_split(country) %>% 
    map_dfr(function(w) {
      fit <- fit_loess_outliers(w, n.iter, span)
      x <- seq(min(w$day), max(w$day), step)
      tibble(
        country = w$country[1],
        day = x,
        y = predict(fit, data.frame(day=x))
      )
    }) %>% 
    mutate(
      y = if_else(day < cut.day, 0, y),
      y = if_else(y < 0, 0, y),
      country = factor(country, levels=cntrs)
    )
}

plot_daily_fits <- function(cvd, countries, what="cases", val.min=1, val.max=20,
                            span=0.75, base_country="Italy", step=0.5, n.iter=3) {
  d <- make_day_shifts(cvd, countries, what, val.min, val.max, base_country)
  s <- make_country_fits(d, countries, n.iter, span, step)
  p <- s %>%
    group_by(country) %>%
    summarise(x=max(day), y=last(y))
  
  ggplot(s, aes(x=day, y=y, colour=country)) +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.position = "none") +
    geom_line()  +
    scale_color_manual(values=cbPalette) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(s$y)*1.05)) +
    labs(x="Relative day", y=glue("Daily {what} per million")) +
    geom_point(data=p, aes(x=x, y=y, colour=country), size=1) +
    geom_text_repel(data=p, aes(x=x, y=y, label=country), size=2, nudge_x=0, min.segment.length = 0.5, hjust=0.5, segment.alpha = 0.3)
}

plot_shifts <- function(cvd, countries, what="cases_pop", val.min=1, val.max=20, base_country="Italy") {
  linear_shifts(cvd, what=what, val.min=val.min, val.max=val.max, base_country = base_country) %>% 
    filter(country %in% countries) %>%
    arrange(shift) %>% 
    mutate(country = as_factor(country)) %>% 
  ggplot(aes(x=shift, y=country)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank()) +
    geom_segment(aes(xend=0, yend=country), colour="grey60") +
    geom_point() +
    labs(x="Delay (days)", y=NULL)
  
}

plot_heatmap <- function(cvd, what="new_cases", min.val=5) {
  brks <- c(1) * 10^sort(rep(-4:5,3))
  labs <- sprintf("%f", brks) %>% str_remove("0+$") %>% str_remove("\\.$")
  lbrks <- log10(brks)
  
  shifts <- linear_shifts(cvd, what="cases_pop", val.min=1, val.max=20, base_country = "Italy")
  
  cvd %>%
    left_join(shifts, by="country") %>% 
    arrange(shift) %>% 
    mutate(country = as_factor(country)) %>% 
    mutate(value = !!sym(what), lval = log10(value)) %>% 
    filter(value > min.val) %>%
  ggplot(aes(x=date, y=country, fill=lval)) +
    theme_bw() +
    geom_tile() +
    scale_fill_viridis_c(breaks=lbrks, labels=labs, option="cividis") +
    labs(x="Date", y=NULL, fill=what)
}

plot_death_excess <- function(cvd, cntry="United Kingdom", base_country="South Korea", val.min=0.05, val.max=0.2, cntry_short=NULL) {
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
    mutate(deaths_pred = 10^predict(fit, cvd_cntry) * population / 1e6) %>% 
    drop_na() %>% 
    mutate(
      deaths_ex = as.integer(deaths - deaths_pred),
      deaths_rat = deaths / deaths_pred
    ) %>% 
    filter(deaths_ex >= 0)
    
  
  xlims <- c(0, max(cvd_sel$days))  
  g1 <- basic_plot(cvd_sel, x="days", y="deaths_pop",xlab=NULL,  ylab="Cumulative deaths per million", logy=TRUE) +
    scale_x_continuous(limits=xlims) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      plot.margin = margin(0, 0, 1, 0, "pt")
    )
  
  g2 <- ggplot(cvd_pred, aes(x=days, y=deaths_ex)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    geom_segment(aes(xend=days, yend=0), colour="grey80") +
    geom_point() +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(cvd_pred$deaths_ex)*1.05)) +
    scale_x_continuous(limits = xlims) +
    labs(x="Relative day", y=glue("Excess deaths in {cntry_short}"))
  
  plot_grid(g1, g2, align="v", ncol=1, rel_heights = c(1.5,1))
}


plot_ons_deaths <- function(ons, wk) {
  d <- ons %>% 
    filter(week == wk)
  d %>% group_by(category) %>% summarise(deaths = sum(deaths)) %>% print()
  ggplot(d, aes(x=age, y=deaths, fill=category)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    geom_col(position="identity") +
    labs(x="Age group", y="Deaths", title=glue("Week {wk}")) +
    scale_fill_manual(values=cbPalette[2:3]) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(d$deaths)*1.05))
}

plot_deaths_gdppop <- function(cvd, what="pop") {
  brks <- c(1) * 10^sort(rep(-4:6,3))
  labs <- sprintf("%f", brks) %>% str_remove("0+$") %>% str_remove("\\.$") %>% prettyNum(big.mark = ",") %>% str_remove("^\\s+")
  xlab <- ifelse(what == "pop", "Population (million)", "GDP per capita (USD)")
  d <- cvd %>%
    group_by(country) %>%
    summarise(deaths = sum(new_deaths), pop = first(population)/1e6, rat = deaths / pop, gdp = first(gdp)) %>%
    filter(deaths > 10)
  g <- ggplot(d, aes_string(x=what, y="rat", label="country"))
  if(what == "gdp") g <-g + geom_smooth(method="lm", colour="lightskyblue1", alpha=0.1)
  g <- g + theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_x_log10(breaks=brks, labels=labs) +
    scale_y_log10(breaks=brks, labels=labs) +
    geom_text_repel(size=1.5, segment.color = "grey70", segment.alpha = 0.4) +
    geom_point(size=0.8) +
    scale_colour_viridis_c(option="cividis") +
    labs(x=xlab, y="Deaths per million")
  
  g
}


g_cases_deaths <- function(d, repel=FALSE, min.deaths=0) {
  brks <- c(1) * 10^sort(rep(-4:9,3))
  labs <- sprintf("%f", brks) %>% str_remove("0+$") %>% str_remove("\\.$") %>% prettyNum(big.mark = ",") %>% str_remove("^\\s+")
  d <- d %>% 
    filter(deaths > 0)
  g <- ggplot(d, aes(x=cases, y=deaths, colour=population)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    #geom_abline(slope=1, intercept=log10(c(0.01, 0.05, 0.1, 0.2)), colour="red", alpha=0.1) +
    scale_x_log10(breaks=brks, labels=labs, limits=c(50,4e6)) +
    scale_y_log10(breaks=brks, labels=labs) +
    labs(x="Reported cases", y="Reported deaths", colour="Population") +
    scale_colour_viridis_c(labels=scales::trans_format('log10', scales::math_format(10^.x)), breaks=brks, trans="log10")
  if(repel) {
    dd <- d %>% filter(deaths >= min.deaths)
    g <- g + geom_text_repel(data=dd, aes(label=country), size=2, segment.color="grey90", colour="grey40")
  } else {
    g <- g + geom_text(aes(label=paste0("  ", country)), size=2.4, colour="grey80", hjust=0)
  }
  g + geom_point(size=0.8)

}

plot_cases_deaths <- function(cvd, min.deaths=1000) {
  cvd %>%
    group_by(country) %>% 
    summarise(deaths = max(deaths), cases = max(cases), population = first(population)) %>% 
    g_cases_deaths(repel=TRUE, min.deaths) +
    theme(panel.grid = element_blank())
}

plot_cases_deaths_anim <- function(cvd) {
  g <- g_cases_deaths(cvd) + 
    ggtitle("{frame_time}") +
    transition_time(date) +
    ease_aes('cubic-in-out')
  animate(g, height = 1200, width = 1200, res = 250, fps=8)
}

plot_week_days <- function(cvd, cntrs, what="deaths") {
  w <- glue("new_{what}")
  d <- cvd %>% 
    filter(country %in% cntrs) %>% 
    mutate(value = !!sym(w)) %>% 
    group_by(country, day_of_week) %>% 
    summarise(y = sum(value)) %>% 
    group_by(country) %>% 
    mutate(prop = y / sum(y))
  chi <- d %>% 
    ungroup() %>% 
    group_split(country) %>% 
    map_dfr(function(w) {
      chisq.test(w$y) %>%
        tidy() %>%
        mutate(country = first(w$country))
    }) 
  
  ggplot(d, aes(x=day_of_week, y=prop)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_col(fill="grey70", colour="grey30", width=0.8, size=0.3) +
    facet_wrap(~ country) +
    scale_x_discrete(labels=c("M", "T", "W", "T", "F", "S", "S")) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(d$prop)*1.05)) +
    labs(x=NULL, y=glue("Proportion of reported {what}")) +
    geom_hline(yintercept = 1/7, colour="red", linetype="dashed", alpha=0.5)
}


plot_week_days_total <- function(cvd) {
  d <- cvd %>%
    group_by(day_of_week, country) %>%
    summarise(n = sum(new_deaths)) %>%
    ungroup() %>%
    filter(n > 0) %>% 
    group_by(country) %>%
    mutate(p = n / sum(n), tot=sum(n)) %>%
    filter(tot > 100) %>% 
    ungroup()
  ggplot(data=d, aes(x=day_of_week, y=p)) + 
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_boxplot(outlier.shape = NA, fill=cbPalette[3], alpha=0.3) +
    geom_beeswarm(cex=1, size=1, colour="grey50") +
    labs(x=NULL, y="Proportion of deaths")
}


plot_map_europe <- function(cvd, what="deaths", val.max=NULL, brks=NULL) {
  val <- glue("new_{what}")
  brks <- sort(rep(brks,3))
  labs <- sprintf("%f", brks) %>% str_remove("0+$") %>% str_remove("\\.$") %>% prettyNum(big.mark = ",") %>% str_remove("^\\s+")
  covid_tot <- cvd %>%
    group_by(countryterritoryCode) %>%
    summarise(tot = sum(!!sym(val)))
  eur_map <- ne_countries(scale=50, continent="Europe", returnclass = "sf") %>% 
    left_join(covid_tot, by=c("iso_a3" = "countryterritoryCode")) %>%
    filter(!(admin %in% c("San Marino", "Andorra"))) 
    #filter(!is.na(tot))
  w <- str_remove(what, "_pop")
  leg <- ifelse(str_detect(what, "pop"), glue("{w} per million"), w)
  
  g <- ggplot(eur_map) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    geom_sf(aes(fill=tot), size=0.2) +
    labs(fill=leg) +
    xlim(-23, 45) +
    ylim(36, 70)
  if(is.null(val.max)) {
    g <- g + scale_fill_viridis_c(option="cividis", breaks=brks, labels=labs)
  } else {
    g <- g + scale_fill_viridis_c(option="cividis", breaks=brks, labels=labs, limits=c(0, val.max))
  }
  g
}



read_scotland <- function(file) {
  pop <- 5.463300
  d <- read_csv(file, skip=3, col_types="cc", col_names=c("Date", "Count")) %>% 
    mutate(count = str_remove(Count, ",") %>% as.numeric()) %>% 
    filter(str_detect(Date, "/2020")) %>% 
    mutate(date = as.Date(Date, "%m/%d/%Y")) 
  tibble(
    country = "Scotland",
    date = d[2:nrow(d), ] %>% pull(date),
    deaths = diff(d$count),
    deaths_pop = deaths / pop,
    cumdeaths_pop = cumsum(deaths_pop)
  )
}

read_england <- function(file) {
  pop <- 56.286961
  d <- read_tsv(file) %>% select(-(1:3))
  tibble(
    country = "England",
    date = as.Date(names(d), "%d-%b-%y"),
    deaths = d[1, ] %>% as.numeric(),
    deaths_pop = deaths / pop,
    cumdeaths_pop = cumsum(deaths_pop)
  )
}

plot_england_scotland <- function() {
  d <- bind_rows(
    read_scotland("Scotland 2020-05-20.csv"),
    read_england("England 2020-05-20.txt")
  ) 

  ggplot(d, aes(x=date, y=cumdeaths_pop, colour=country)) +
    theme_bw() +
    geom_line() +
    geom_point() +scale_colour_manual(values=cbPalette) 
    #scale_y_log10()
}