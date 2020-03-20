countries_eu <- c("Italy", "United Kingdom", "France", "Germany", "Netherlands", "Sweden", "Switzerland", "Spain")
shapes <- c(15:18, 0:14)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey30", "grey70", "black")


read_covid <- function() {
  yesterday <- Sys.Date() - 1
  urlc <- glue("https://ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{yesterday}.xlsx")
  stopifnot(RCurl::url.exists(urlc))
  tmp <- tempfile()
  download.file(urlc, tmp, mode="wb")
  readxl::read_excel(tmp)
}

process_covid <- function(cvd) {
  cvd %>% 
    rename(
      country = `Countries and territories`,
      new_cases = Cases,
      new_deaths = Deaths
    ) %>%
    mutate(country = str_replace_all(country, "_", " ")) %>% 
    mutate(country = recode(country,
      'United States of America' = "United States"
    )) %>%
    mutate(date = as.Date(DateRep, format="%d/%m/%Y")) %>% 
    group_by(country) %>% 
    arrange(date) %>% 
    mutate(tot_cases = sum(new_cases), cases = cumsum(new_cases)) %>% 
    mutate(deaths = cumsum(new_deaths)) %>% 
    ungroup() %>% 
    filter(
      tot_cases > 200 &
      country != "Cases on an international conveyance Japan"
    )
}

basic_plot <- function(d, x="date", y="cases", xlab="Date", ylab=NULL, palette=cbPalette, shps=shapes, point.size=1) {
  brks <- rep(c(1, 2, 5), 5) * 10^sort(rep(0:4,3))
  if(is.null(ylab)) ylab = glue("Reported {y}")
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
    filter(!!sym(what) >= val.min) %>% 
    left_join(shifts, by="country") %>% 
    mutate(days = as.integer(date) - shift - ref)
}

plot_shifted <- function(cvd, what="cases", val.min=100) {
  shifts <- linear_shifts(cvd, what=what, val.min=val.min)
  cvd %>% 
    mutate(value = !!sym(what)) %>% 
    filter(value >= val.min) %>% 
    left_join(shifts, by="country") %>% 
    mutate(days = as.integer(date) - shift - ref) %>% 
    basic_plot(x="days", y="value", ylab=glue("Reported {what}"), xlab="Normalized day")
}

linear_shifts <- function(cvd, what="cases", base_country = "Italy", val.min=100, val.max=1000) {
  dat <- cvd %>%
    mutate(value = !!sym(what)) %>% 
    filter(value >= val.min & value <= val.max) %>%
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
    filter(value >= val.min & cases < 10000 & country != "China") %>% 
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

plot_doubling_times <- function(cvd, what="cases", val.min=100) {
  get_doubling_times(cvd, what, val.min=val.min) %>% 
    ggplot(aes(x=country, y=slope, ymin=lo, ymax=up)) +
    theme_bw() +
    geom_errorbar(width=0.3) +
    geom_point() +
    coord_flip() +
    labs(x=NULL, y=glue::glue("Reported {what} doubling time (day)"))
}


plot_country <- function(cvd, cntry="United Kingdom",
                         what="cases", val.min=100) {
  shifts <- linear_shifts(cvd, what=what, val.min=val.min)
  d <- cvd %>%
    shift_days(shifts, what = what, val.min = val.min)
  d_eu <- d %>% filter(country %in% countries_eu)
  d_sel <- d %>% filter(country == cntry)
  basic_plot(d_eu, x="days", y=what, xlab="Normalized day",
             palette=rep("grey",10), shps=rep(1, 10)) +
    ggtitle(cntry) +
    theme(legend.position = "none") +
    geom_point(data=d_sel, shape=21, fill="royalblue", colour="black", size=2.5) +
    geom_line(data=d_sel, colour="black")
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
    labs(x="Normalized day", y=glue::glue("log10 {what}"), title=cntry)
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


plot_derivative <- function(cvd, cntry="United Kingdom",
                         what="cases", val.min=100, span=2) {
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
    labs(x=NULL, y=glue::glue("log10 {what}"), title=cntry) +
    xlim(xlim)
  g2 <- ggplot(dif, aes(x=x, y=dbl)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    geom_line() +
    labs(x = "Normalized day", y = "Doubling time") +
    xlim(xlim)
  plot_grid(g1, g2, ncol=1, rel_heights = c(1,0.6), align="v")
  
}



plot_death_ratio <- function(cvd, text.size=8, mortality=0.034) {
  d <- cvd %>% 
    group_by(country) %>% 
    summarise(deaths = sum(new_deaths), cases = sum(new_cases)) %>% 
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