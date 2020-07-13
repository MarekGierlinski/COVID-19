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
  "Armenia",
  "Brazil",
  "Chile",
  "Colombia",
  "Mexico",
  "Egypt",
  "India",
  "Bangladesh",
  "South Africa",
  "Bolivia",
  "Guatemala"
)

countries_3 <- c(
  "Israel",
  "Iran",
  "North Macedonia",
  "Serbia",
  "Portugal",
  "South Korea",
  "United States",
  "Romania",
  "Turkey",
  "Czechia",
  "Australia"
)

europe <- "AL-AD-AT-BY-BE-BA-BG-HR-CZ-DK-EE-FI-FR-DE-EL-HU-IS-IE-IT-XK-LV-LT-LU-MT-NL-MD-ME-NO-PL-PT-RO-SM-ES-RS-SK-SI-SE-CH-UA-TR-UK" %>% str_split("-") %>% unlist()

EU <- "AT-BE-BG-HR-CY-CZ-DK-EE-FI-FR-DE-EL-HU-IE-IT-LV-LT-LU-MT-NL-PL-PT-RO-SK-SI-ES-SE" %>% str_split("-") %>% unlist()
