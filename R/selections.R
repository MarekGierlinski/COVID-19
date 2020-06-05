countries_sel <- c(
  "Italy",
  "Spain",
  "France",
  "Germany",
  "United Kingdom",
  "United States",
  "Sweden"
)
countries_day <- c(
  countries_sel,
  "Belgium",
  "Netherlands",
  "Ireland",
  "Switzerland",
  "Canada"
)
countries_2 <- c(
  "Argentina",
  "Brazil",
  "Chile",
  "Colombia",
  "Mexico",
  "Egypt",
  "Iran",
  "Russia",
  "Pakistan",
  "India",
  "Bangladesh",
  "Indonesia"
)

europe <- "AL-AD-AT-BY-BE-BA-BG-HR-CZ-DK-EE-FI-FR-DE-EL-HU-IS-IE-IT-XK-LV-LT-LU-MT-NL-MD-ME-NO-PL-PT-RO-SM-ES-RS-SK-SI-SE-CH-UA-TR-UK" %>% str_split("-") %>% unlist()
