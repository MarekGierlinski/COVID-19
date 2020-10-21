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
  "Czechia"
)
countries_2 <- c(
  "Argentina",
  "Colombia",
  "India",
  "Indonesia",
  "Iraq",
  "Kenya",
  "Kazakhstan",
  "Uzbekistan",
  "Moldova",
  "Venezuela",
  "Morocco",
  "South Africa"
  
)

countries_3 <- c(
  "Israel",
  "Japan",
  "Iran",
  "North Macedonia",
  "Serbia",
  "Luxembourg",
  "Switzerland",
  "Portugal",
  "South Korea",
  "United States",
  "Romania",
  "Turkey",
  "Czechia",
  "Australia"
)

countries_4 <- c(
  "Albania",
  "Austria",
  "Belgium",
  "Bosnia and Herzegovina",
  "Bulgaria",
  "Czechia",
  "Denmark",
  "France",
  "Germany",
  "Hungary",
  "Ireland",
  "Italy",
  "Spain",
  "Switzerland",
  "Romania",
  "Netherlands",
  "Serbia",
  "United Kingdom",
  "Luxembourg",
  "Poland"
)

europe <- "AL-AD-AT-BY-BE-BA-BG-HR-CZ-DK-EE-FI-FR-DE-EL-HU-IS-IE-IT-XK-LV-LT-LU-MT-NL-MD-ME-NO-PL-PT-RO-SM-ES-RS-SK-SI-SE-CH-UA-TR-UK" %>% str_split("-") %>% unlist()

EU <- "AT-BE-BG-HR-CY-CZ-DK-EE-FI-FR-DE-EL-HU-IE-IT-LV-LT-LU-MT-NL-PL-PT-RO-SK-SI-ES-SE" %>% str_split("-") %>% unlist()
