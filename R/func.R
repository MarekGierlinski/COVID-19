shapes <- c(15:18, 0:14)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey20", "grey40", "grey60", "grey80", "grey90", "black")
wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

ukPalette <- c(
  England = rgb(255, 90, 90, max=255),
  Scotland = rgb(110, 110, 255, max=255),
  Wales = rgb(255, 214, 0, max=255),
  `Northern Ireland` = rgb(20, 185, 90, max=255)
)

if(!dir.exists("fig")) dir.create("fig")


get_url_ecdc <- function() {
  today <- Sys.Date()
  urlc <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  #urlc <- glue("https://ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{today}.xlsx")
  stopifnot(url.exists(urlc))
  urlc
}

read_covid <- function(urlc) {
  #tmp <- tempfile()
  #download.file(urlc, tmp, mode="wb")
  #readxl::read_excel(tmp)
  read_csv(urlc)
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


read_staging_data <- function() {
  u <- 'https://api.coronavirus-staging.data.gov.uk/v1/data?filters=areaType=nation&structure={"date":"date","newDeathsByPublishDate":"newDeathsByPublishDate","newAdmissions":"newAdmissions","newCasesByPublishDate":"newCasesByPublishDate","hospitalCases":"hospitalCases","covidOccupiedMVBeds":"covidOccupiedMVBeds","areaName":"areaName"}&format=csv'
  read_csv(u) %>% 
    rename(
      new_cases = newCasesByPublishDate,
      new_deaths = newDeathsByPublishDate,
      hospital_cases = hospitalCases,
      new_admissions = newAdmissions,
      mv_beds = covidOccupiedMVBeds,
      nation = areaName
    ) %>% 
    drop_na()
}


# 2018 population, source: Wikipedia
uk_pop <- tibble::tribble(
  ~nation, ~population,
  "England", 55977178,
  "Scotland", 5438100,
  "Wales", 3138631,
  "Northern Ireland", 1881641,
  "UK", 66435550	
)


# scrape web page to find URL
get_url_testing <- function() {
  urlc <- read_html("https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public") %>% 
    html_text() %>%
    str_extract("https[\\w\\.\\-:/]+?testing[_-]time[_-]series[\\w]*?.csv")
  stopifnot(url.exists(urlc))
  urlc
}

read_testing <- function(urlc) {
  read_csv(urlc, col_types = cols(), na = c("NA", "Unavailable")) %>% 
    mutate(date = as.Date(`Date of activity`, format="%d/%m/%Y")) %>% 
    rename(
      nation = Nation,
      pillar = Pillar,
      tests = "Daily number of tests processed",
      positives_old = "Daily number of positive cases (old methodology)",
      positives_new = "Daily number of positive cases (new methodology)",
      tests_processed = "Daily In-person (tests processed)",
      tests_sent = "Daily Delivery (tests sent out)"
    ) %>% 
    mutate(positives = if_else(is.na(positives_old), positives_new, positives_old)) %>% 
    select(-starts_with("Cumul")) %>% 
    left_join(uk_pop, by="nation")
}

get_url_excess <- function() {
  urlc <- "https://github.com/Financial-Times/coronavirus-excess-mortality-data/raw/master/data/ft_excess_deaths.csv"
  stopifnot(url.exists(urlc))
  urlc
}

read_excess <- function(urlc) {
  read_csv(urlc, col_types = cols())
}


process_covid <- function(cvd, gdp) {
  cvd %>% 
    rename(
      country = countriesAndTerritories,
      code = countryterritoryCode,
      new_cases = cases,
      new_deaths = deaths,
      id = geoId,
      population = popData2019,
      continent = continentExp
    ) %>%
    mutate(
      country = str_replace_all(country, "_", " ")) %>% 
    mutate(
      country = recode(country,
        'United States of America' = "United States",
        "CANADA" = "Canada"
      ),
      code = recode(code,
        "XKX" = "KOS"
      )
    ) %>%
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


rollmean_boundary <- function(x, k) {
  n <- floor(k / 2)
  if(n == k/2) stop("k needs to be odd.")
  xx = c(rep(x[1], n), x, rep(x[length(x)], n))
  zoo::rollmean(xx, k, na.rm=TRUE)
}

rollmean_outliers <- function(x, n.iter=3, k=7) {
  for(i in 1:n.iter) {
    m <- rollmean_boundary(x, k)
    diff <-  abs(x - m)
    x[which.max(diff)] <- NA
  }
  return(m)
}

make_country_fits_ <- function(d, cntrs, n.iter=3, span=0.75, step=0.5, cut.day=3) {
  d %>% group_split(country) %>% 
    map_dfr(function(w) {
      w %>% 
        mutate(y = if_else(y == 0, as.numeric(NA), y)) %>% 
        mutate(y = rollmean_boundary(y, k=7))
    }) %>% 
    mutate(
      y = if_else(day < cut.day, 0, y),
      y = if_else(y < 0, 0, y),
      country = factor(country, levels=cntrs)
    )
}

make_country_fits <- function(d, cntrs, n.iter=3, span=0.75, step=0.5, cut.day=3) {
  d %>% group_split(country) %>% 
    map_dfr(function(w) {
      w[w$y == 0, "y"] <- NA
      fit <- fit_loess_outliers(w, n.iter, span)
      x <- seq(min(w$day), max(w$day), step)
      f <- predict(fit, data.frame(day=x), se=TRUE)
      tibble(
        country = w$country[1],
        day = x,
        y = f$fit,
        y_lo = f$fit - qt(0.975, f$df) * f$se,
        y_up = f$fit + qt(0.975, f$df) * f$se
      )
    }) %>% 
    mutate(
      y = if_else(day < cut.day, 0, y),
      y = if_else(y < 0, 0, y),
      y_lo = if_else(y_lo < 0, 0, y_lo),
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
    #geom_ribbon(aes(ymin=y_lo, ymax=y_up, y=NULL, fill=country), alpha=0.3, colour=NA) +
    geom_line()  +
    scale_color_manual(values=cbPalette) +
    scale_fill_manual(values=cbPalette) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(s$y, na.rm=TRUE)*1.05)) +
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

plot_smooth_heatmap <- function(cvd, what="deaths", min.val=10, min.pop=1e6,
                                min.date="2020-03-01", span=0.3, brks=NULL) {
  w <- glue("new_{what}_pop")
  x <- cvd %>% 
    filter(population >= min.pop & date >= min.date) %>% 
    mutate(y = !!sym(w), day = as.integer(date))
  
  cd <- x %>% 
    group_by(country) %>% 
    summarise(tot = sum(y), cg = sum(y * day) / sum(y), mx = max(y)) %>% 
    filter(tot > min.val) %>% 
    arrange(cg) %>% 
    mutate(country = as_factor(country))
  
  d <- x %>% 
    filter(country %in% cd$country) %>% 
    mutate(country = factor(country, levels = cd$country))
  
  df <- make_country_fits(d, cd$country, span=span) %>% 
    mutate(date = as.Date(day, origin="1970-01-01"))
  ggplot(df, aes(x=date, y=country, fill=y)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border =  element_blank()
    ) +
    geom_tile() +
    #scale_fill_viridis_c(breaks=lbrks, labels=labs, option="cividis") +
    scale_fill_viridis_c(option="magma", trans="log1p", breaks=brks) +
    scale_x_date(expand = c(0,0)) +
    labs(x="Date", y=NULL, fill=glue("{what} per million"))
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
    filter(!is.na(y)) %>% 
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


plot_map_europe <- function(cvd, what, brks) {
  when <- max(cvd$date) - 1
  d <- cvd %>% filter(!(country %in% c("San Marino", "Andorra", "Luxembourg"))) 
  plot_map(d, what, when, brks, "Europe", val.max=NULL, x.lim=c(-23, 45), y.lim=c(36, 70))
}



plot_map <- function(cvd, what, when, brks, cont, val.max, x.lim=NULL, y.lim=NULL) {
  brks <- sort(rep(brks,3))
  labs <- sprintf("%f", brks) %>% str_remove("0+$") %>% str_remove("\\.$") %>% prettyNum(big.mark = ",") %>% str_remove("^\\s+")
  covid_tot <- cvd %>%
    mutate(val = !!sym(what))
  if(!is.null(when)) covid_tot <- covid_tot %>% filter(date == when)
  cvd_map <- ne_countries(scale=50, continent=cont, returnclass = "sf") %>% 
    left_join(covid_tot, by=c("su_a3" = "code")) %>%
    filter(!is.na(date))
  w <- str_remove(what, "_pop")
  leg <- ifelse(str_detect(what, "pop"), glue("{w} per million"), w)
  
  g <- ggplot(cvd_map) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    geom_sf(aes(fill=val), size=0.2) +
    labs(fill=leg) +
    xlim(x.lim) +
    ylim(y.lim)
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


plot_recent_daily_ <- function(cvd, what="new_deaths_pop", n=7, min.pop = 1e6, top.n=30) {
  s <- what %>% str_remove("new_") %>% str_remove("_pop")
  d <- cvd %>%
    filter(date > max(date) - n & population >= min.pop) %>% 
    mutate(val = !!sym(what)) %>% 
    group_by(country) %>% 
    summarise(M = mean(val, na.rm=TRUE), S = sd(val, na.rm=TRUE), n = n()) %>% 
    arrange(-M) %>% 
    mutate(country = as_factor(country) %>% fct_rev()) %>% 
    mutate(SE = S / sqrt(n), tc = qt(0.975, df = n - 1), M_lo = M - SE*tc, M_up = M + SE*tc) %>% 
    mutate(M_lo = if_else(M_lo < 0, 0, M_lo)) %>% 
    head(top.n)
  ggplot(d, aes(x=country)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_segment(aes(xend=country, y = M_lo, yend = M_up), colour="grey60") +
    geom_point(aes(y = M)) +
    coord_flip() +
    labs(y = glue("Reported {s} per million per day (mean and 95% CI over last {n} days)"), x=NULL) +
    scale_y_continuous(expand = c(0,0), limits=c(0, max(d$M_up) * 1.05))
}

plot_recent_daily <- function(cvd, what="new_deaths_pop", n=7, min.pop = 1e6, top.n=30) {
  s <- what %>% str_remove("new_") %>% str_remove("_pop")
  d <- cvd %>%
    filter(date > max(date) - n & population >= min.pop) %>% 
    mutate(delta = as.character(max(date) - date + 1)) %>% 
    mutate(val = !!sym(what))
  md <- d %>% 
    group_by(country) %>% 
    summarise(M = median(val, na.rm=TRUE)) %>% 
    arrange(-M) %>%
    head(top.n)
  ct <- md$country %>% as_factor()
  
  md %>% 
    left_join(d, by="country") %>% 
    mutate(country = factor(country, levels = ct) %>% fct_rev()) %>% 
  ggplot(aes(x=country, y=val, fill=as.character(date))) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_boxplot(outlier.shape = NA, colour="grey70", fill=NA) +
    geom_beeswarm(shape=21, cex=1.5) +
    coord_flip() +
    labs(y = glue("Reported {s} per million per day (last {n} days)"), x=NULL, fill="Date") +
    scale_fill_viridis_d()
}


plot_testing <- function(d, what="positives") {
  d <- d %>% 
    filter(!(nation %in% c("UK", "Wales"))) %>% 
    mutate(val = !!sym(what), val_pop = 1e6 * val / population)
  mx <- max(d$val_pop, na.rm=TRUE) * 1.05
  ggplot(d, aes(x=date, y=val_pop)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_segment(aes(xend=date, yend=0), colour="grey80") +
    geom_point() +
    geom_smooth(method="loess", span=0.5, size=0.9, alpha=0.8, colour=cbPalette[3], se=FALSE) +
    facet_wrap(~nation, ncol=3) +
    scale_y_continuous(expand=c(0,0), limits=c(0,mx)) +
    labs(x=NULL, y=NULL, title=glue("Daily {what} per million") %>% str_replace("_", " "))
}

prep_country_region <- function(d, ctry, by.region) {
  if(is.null(ctry)) ctry <- unique(d$country)
  
  if(by.region) {
    d <- d %>% 
      mutate(group = region) %>% 
      filter(region != country)
  } else {
    d <- d %>% 
      mutate(group = country) %>% 
      filter(region == country)
  }
  
  d <- d %>% 
    filter(country %in% ctry & date > "2015-01-01")
}

plot_excess_details <- function(d, ctry=NULL, by.region=FALSE, ncol=1, y.scale=3) {
  d <- prep_country_region(d, ctry, by.region)
  bkg <- d %>% filter(date < "2020-01-01")
  fg <- d %>% filter(date >= "2020-01-01")
  xmx <- max(fg$week) + 1
  ymx <- max(d$deaths) * 1.05
  
  bl <- d %>%
    group_by(group) %>%
    summarise(maxy = median(expected_deaths) * y.scale, week=1)
  
  ggplot() +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_blank(data=bl, aes(x=week, y=maxy)) +
    geom_blank(data=bl, aes(x=week, y=0)) +
    geom_beeswarm(data=bkg, aes(x=week, y=deaths), colour="grey70", size=0.5, cex=0.6) +
    #geom_step(data=fg, aes(x=week-0.5, y=deaths), size=0.7) +
    geom_line(data=fg, aes(x=week, y=deaths), colour="grey90") +
    geom_point(data=fg, aes(x=week, y=deaths), size=1.5, shape=22, fill=cbPalette[3]) +
    facet_wrap(~ group, ncol=ncol, scales = "free_y") +
    scale_x_continuous(breaks=c(1,10,20,30,40,50), limits=c(0, xmx)) +
    scale_y_continuous(expand=c(0,0)) +
    labs(x="Week", y="Deaths")
}

plot_excess_prop <- function(x, ctry=NULL, by.region=FALSE, ncol=1, y.scale=3) {
  d <- prep_country_region(x, ctry, by.region) %>% 
    filter(week <= max(week[year==2020])) %>%
    group_split(week, group) %>%
    map_dfr(function(w) {
      if(nrow(w) > 1 & nrow(w[w$year==2020, ]) == 1) {
        d2020 <- w[w$year==2020, ]$deaths
        w <- w %>% 
          mutate(prop = d2020 / deaths) %>% 
          filter(year < 2020)
        w$pval = t.test(w$prop, mu = 1, alternative = "greater")$p.value
        w
      }
    }) %>% 
    group_by(group) %>% 
    mutate(p.adj = p.adjust(pval, method="BH"), sig = p.adj < 0.05)

  ymx <- max(d$prop) * 1.05
  
  bl <- d %>%
    group_by(group) %>%
    summarise(maxy = max(prop)*1.05, week=1)
  
  ggplot(d) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    ) +
    #geom_blank(data=bl, aes(x=week, y=maxy)) +
    #geom_blank(data=bl, aes(x=week, y=0)) +
    geom_beeswarm(aes(x=week, y=prop, colour=sig), size=1, cex=0.5) +
    scale_colour_manual(values=c("black", "red"))+
    facet_wrap(~ group, ncol=ncol, scales = "fixed") +
    scale_x_continuous(breaks=c(1,5,10,15,20,25,30,35,40,45,50)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, ymx)) +
    labs(x="Week", y="Excess deaths ratio")
}


last_week <- function(x, ctry="UK") {
  x %>%
    filter(country == ctry & region == ctry & year == 2020) %>%
    pull(week) %>%
    max()
}

last_date <- function(x, ctry="UK") {
  x %>%
    filter(country == ctry & region == ctry & year == 2020) %>% 
    pull(date) %>%
    max()
}

annual_deaths <- function(x, ctry = "UK", reg = NULL) {
  if(is.null(reg)) reg <- ctry
  d <- x %>%
    filter(country == ctry & region == reg)
  week.end <- last_week(x, ctry)
  d <- d %>%
    filter(week <= week.end) %>%
    group_by(year) %>%
    summarise(tot = sum(deaths), n = n())
}

plot_exc_total_deaths <- function(x, ctry="UK") {
  d <- annual_deaths(x, ctry)
  
  week.end <- x %>%
    filter(country == ctry & region == ctry) %>% 
    filter(year == 2020) %>% pull(week) %>% max()
  
  ggplot(d, aes(x=year, y=tot/1000)) + 
    theme_bw() +
    geom_col() +
    scale_x_continuous(breaks=seq(2010,2020,2)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(d$tot/1000)*1.05)) +
    labs(x="Year", y=glue("Deaths (thousands) weeks 1-{week.end}"))
}

plot_exc_weekly_deaths <- function(x, ctry = "uk") {
  x %>% 
    filter(country == ctry & region == ctry) %>%
  ggplot(aes(x=date, y=deaths)) + 
    theme_bw() +
    geom_step() +
    labs(x = "Date", y="Weekly deaths")
}

excess_deaths <- function(x, ctry, reg=NULL, year1=2010, year2=2019) {
  d <- annual_deaths(x, ctry, reg)
  base <- d %>% filter(year >= year1, year <= year2)
  M <- mean(base$tot)
  SE <- sd(base$tot) / sqrt(nrow(base))
  tc <- qt(0.975, nrow(base) - 1)
  CI <- SE * tc
  D <- d[d$year == 2020, ]$tot - M
  
  list(
    diff = signif(D, 2),
    se = signif(SE, 2),
    ci = signif(CI, 2)
  )
}


plot_continents <- function(cvd, what, brks, bw=7) {
  labs <- sprintf("%f", brks) %>% str_remove("0+$") %>% str_remove("\\.$") %>% prettyNum(big.mark = ",") %>% str_remove("^\\s+")
  cvd %>%
    filter(date > as.Date("2020-01-01")) %>%
    group_by(continent, date) %>% 
    summarise(tot = sum(!!sym(what))) %>%
    mutate(stot = ksmooth(date, tot, x.points=date, bandwidth=bw)$y) %>%
    mutate(continent = factor(continent, level=c("Asia", "Europe", "America", "Africa", "Oceania")) %>% fct_rev) %>%
  ggplot(aes(x=date, y=stot, fill=continent)) +
    theme_bw() +
    geom_area(colour="black") +
    scale_fill_brewer(palette="YlGnBu") +
    labs(x=NULL, y=paste("Daily", str_remove(what, "new_")), fill="Continent") +
    scale_y_continuous(breaks=brks, labels=labs)
}

UK_nation_excess <- function(x) {
  map_dfr(uk_pop$nation, function(r) {
    ad <- annual_deaths(x, "UK", r)
    current <- ad[ad$year=="2020", ]$tot
    ad %>%
      filter(year >= 2015, year <= 2019) %>% 
      mutate(diff = current - tot, nation = r)
  }) %>% 
    left_join(uk_pop, by="nation") %>% 
    mutate(diff_pop = 1e6 * diff / population)
}

plot_uk_nation_excess <- function(x) {
  x <- x %>% 
    mutate(year = as_factor(year), nation = factor(nation, levels=uk_pop$nation)) %>% 
    filter(nation != "UK")
  ggplot(x, aes(x=nation, y=diff_pop, group=nation)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_boxplot(outlier.shape = NA, colour="grey80") +
    geom_beeswarm() +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(x$diff_pop)*1.05)) +
    scale_colour_viridis_d() +
    labs(x=NULL, y="2020 excess deaths per million vs 2015-2019")
}


make_eu <- function(cvd) {
  cvd_eu <- cvd %>%
    filter(id %in% EU) %>%
    group_by(date) %>%
    summarise(
      cases = sum(cases),
      deaths = sum(deaths),
      new_cases = sum(new_cases),
      new_deaths = sum(new_deaths),
      population = sum(population)
    ) %>%
    mutate(
      country = "EU",
      cases_pop = 1e6 * cases / population,
      deaths_pop = 1e6 * deaths / population,
      new_cases_pop = 1e6 * new_cases / population,
      new_deaths_pop = 1e6 * new_deaths / population
    )
  bind_rows(cvd, cvd_eu)
}


plot_staging_cumul <- function(stag) {
  stag %>%
    arrange(date) %>%
    group_by(nation) %>%
    mutate(deaths = cumsum(new_deaths), cases = cumsum(new_cases)) %>%
    ungroup %>%
    left_join(uk_pop, by="nation") %>%
    mutate(`Deaths per million` = 1e6 * deaths / population, `Cases per million` = 1e6 * cases / population) %>%
    pivot_longer(cols=c(`Deaths per million`, `Cases per million`)) %>% 
    mutate(nation = factor(nation, levels=names(ukPalette))) %>% 
  ggplot(aes(x=date, y=value, colour=nation)) +
    theme_bw() +
    geom_point(size=0.6) +
    scale_colour_manual(values=ukPalette) +
    facet_wrap(~name, scales="free_y") +
    labs(x=NULL, y="", colour=NULL)
}

plot_staging_hospitals <- function(stag) {
  s <- stag %>%
    left_join(uk_pop, by="nation") %>%
    pivot_longer(c(new_admissions, hospital_cases, mv_beds)) %>%
    mutate(
      name = recode(name,
                    "new_admissions" = "New admissions",
                    "hospital_cases" = "Patients in hospital",
                    "mv_beds" = "Patients on ventillators"
      ),
      value = 1e6*value/population,
      nation = factor(nation, levels=names(ukPalette))
  )
  map(unique(s$name), function(nm) {
    s %>% filter(name == nm) %>% 
    ggplot(aes(x=date, y=value, colour=nation)) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      geom_point(size=0.7) +
      #facet_wrap(~nation, scales="fixed", ncol=4) +
      scale_colour_manual(values=ukPalette) +
      labs(x=NULL, y="Counts per million", colour=NULL, title=nm)
  }) %>% 
    plot_grid(plotlist = ., ncol=1)
}


plot_global <- function(cvd, span=0.3) {
  cvd %>% 
    group_by(date) %>% 
    summarise(`Daily cases` = sum(new_cases), `Daily deaths` = sum(new_deaths)) %>%
    pivot_longer(cols=c(`Daily cases`, `Daily deaths`)) %>% 
  ggplot(aes(x=date, y=value)) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank()
    ) +
    geom_point(size=0.6) +
    geom_line(stat="smooth", method="loess", span=span, se=FALSE, alpha=0.5, colour=cbPalette[3], size=1) +
    facet_wrap(~name, scale="free_y") +
    labs(x=NULL, y="Count") +
    scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'))
}

plot_eu_uk_us <- function(cvd) {
  d <- cvd %>% 
    make_eu() %>% 
    filter(country %in% c("EU", "United Kingdom", "United States") & date>as.Date("2020-03-01") & new_cases_pop >= 0)
  ggplot(d, aes(x=date, y=new_cases_pop, colour=country, fill=country)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    geom_point(alpha=0.2) +
    geom_smooth(method="loess", span=0.3) +
    scale_color_manual(values=cbPalette) +
    scale_fill_manual(values=cbPalette) +
    labs(x=NULL, y="Daily cases per million", fill=NULL, colour=NULL) +
    scale_y_continuous(expand = c(0,0), limits=c(0, max(d$new_cases_pop)*1.03))
}