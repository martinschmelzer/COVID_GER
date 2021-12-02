require(data.table)
require(tidyverse)
require(RcppRoll)

bundeslaender <- c("Schleswig-Holstein" = 1, "Hamburg" = 2, "Niedersachsen" = 3,
                   "Bremen" = 4, "Nordrhein-Westfalen" = 5, "Hessen" = 6,
                   "Rheinland-Pfalz" = 7, "Baden-Württemberg" = 8, "Bayern" = 9,
                   "Saarland" = 10, "Berlin" = 11, "Brandenburg" = 12,
                   "Mecklenburg-Vorpommern" = 13, "Sachsen" = 14, 
                   "Sachsen-Anhalt" = 15, "Thüringen" = 16)

abbreviate_bl <- function(x) {
  case_when(x == "Nordrhein-Westfalen" ~ "NW",
            x == "Berlin" ~ "BE",
            x == "Bremen" ~ "HB",
            x == "Hamburg" ~ "HH",
            x == "Rheinland-Pfalz" ~ "RP",
            x == "Hessen" ~ "HE",
            x == "Niedersachsen" ~ "NI",
            x == "Sachsen" ~ "SN",
            x == "Thüringen" ~ "TH",
            x == "Schleswig-Holstein" ~ "SH",
            x == "Sachsen-Anhalt" ~ "ST",
            x == "Saarland" ~ "SL",
            x == "Brandenburg" ~ "BB",
            x == "Mecklenburg-Vorpommern" ~ "MV",
            x == "Bayern" ~ "BY",
            x == "Baden-Württemberg" ~ "BW")
}

# Gesamtbevölkerung der BL (GENESIS Datenbank, Stichtag 31.12.2020)
bev <- fread("data/12411-0010.csv", skip = 5, header = T, encoding = "Latin-1")
bev <- bev %>% 
  select(Bundesland = V1, Gesamtbevölkerung = `31.12.2020`) 


# Impfquoten vom 16.09.2021, vor der 4. Welle und erstes Bereitstellungsdatum
quoten <- fread("https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/raw/master/Archiv/2021-09-17_Deutschland_Impfquoten_COVID-19.csv", encoding = "UTF-8")
quoten <- quoten %>%
  filter(Bundesland != "Bundesressorts", Bundesland != "Deutschland") %>%
  select(-Datum, BundeslandId_Impfort)


# Inzidenzen (große Datei)
inz <- fread("https://github.com/robert-koch-institut/SARS-CoV-2_Infektionen_in_Deutschland/blob/master/Aktuell_Deutschland_SarsCov2_Infektionen.csv?raw=true", encoding = "UTF-8")
df_inz <- inz %>% 
  mutate(Bundesland = parse_integer(substr(IdLandkreis, 1, ifelse(IdLandkreis >= 10000, 2, 1)))) %>%  # BL ermitteln
  mutate(Bundesland = names(bundeslaender)[Bundesland]) %>%  # BL Klarnamen
  arrange(Meldedatum) %>%  # Nach Meldedatum aufsteigend sortieren
  group_by(Meldedatum, Bundesland) %>%
  filter(NeuerFall != -1) %>%
  summarise(Sum_AnzahlFall = sum(AnzahlFall)) %>%  # Fälle pro Tag
  group_by(Bundesland) %>%
  mutate(Faelle7Tage = roll_sum(Sum_AnzahlFall, n = 7, align = "right", fill = NA, na.rm = T)) %>%  # Fälle der jeweils letzten 7 Tage
  merge(x = ., y = quoten, by = "Bundesland") %>%  # Impfquoten mergen
  merge(x = ., y = bev, by = "Bundesland") %>%  # Gesamtbevölkerungszahl mergen
  mutate(Faelle7Tage_p100T = Faelle7Tage/Gesamtbevölkerung*100000) %>%
  filter(Meldedatum >= lubridate::ymd("20210901")) %>%
  mutate(Bundesland = abbreviate_bl(Bundesland))
  
# Bundesland zu Faktor konvertieren und für den Plot nach Impfquote sortieren
df_inz$Bundesland <- as.factor(df_inz$Bundesland)
df_inz$Bundesland <- reorder(df_inz$Bundesland, -df_inz$Impfquote_gesamt_voll)
  
# Plot
pdf("impfquote_inzidenz.pdf", width = 6, height = 14)
ggplot(df_inz) +
  geom_line(aes(x = Meldedatum, y = Faelle7Tage_p100T, col = Impfquote_gesamt_voll), size = 1) +
  scale_color_gradient(low = "red", high = "green", "Vollst. Geimpfte", breaks = seq(min(df_inz$Impfquote_gesamt_voll), max(df_inz$Impfquote_gesamt_voll), length.out = 3), labels = function(x) { paste0(x, "%") }) +
  scale_x_date(breaks = "1 month", date_labels = "%d.%m") +
  facet_grid(rows = vars(Bundesland)) +
  ylab("7-Tage-Inzidenz") +
  xlab("Datum") +
  ggtitle("7-Tage-Inzidenz nach Bundesland", subtitle = paste0("Stand: ",format(lubridate::now(), "%d.%m.%Y"), ", Impfquoten vom 16.09.2021, Daten: RKI")) +
  theme(text = element_text(size = 11),
        strip.text.y = element_text(angle = 0))
dev.off()



# Hospitalisierungsraten (ausgewählte Altersgruppen)
hosp <- fread("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/master/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv", encoding = "UTF-8")
df_hosp <- hosp %>%
  filter(Bundesland != "Bundesgebiet", 
         Altersgruppe %in% c("15-34", "35-59", "60-79", "80+"), 
         Datum >= lubridate::ymd("20210916")) %>%
  merge(x = ., y = quoten, by = "Bundesland") %>%
  mutate(Bundesland = abbreviate_bl(Bundesland))

# Bundesland zu Faktor konvertieren und für Plot sortieren
df_hosp$Bundesland <- as.factor(df_hosp$Bundesland)
df_hosp$Bundesland <- reorder(df_hosp$Bundesland, -df_hosp$Impfquote_gesamt_voll)

# Plot
pdf("impfquote_hospitalisierung.pdf", width = 10, height = 14)
ggplot(df_hosp) +
  geom_line(aes(x = Datum, y = `7T_Hospitalisierung_Inzidenz`, col = Impfquote_gesamt_voll), size = 1) +
  scale_color_gradient(low = "red", high = "green", "Vollst. Geimpfte", breaks = seq(min(df_hosp$Impfquote_gesamt_voll), max(df_hosp$Impfquote_gesamt_voll), length.out = 3), labels = function(x) { paste0(x, "%") }) +
  scale_x_date(breaks = "1 month", date_labels = "%d.%m") +
  facet_grid(rows = vars(Bundesland), cols = vars(Altersgruppe)) +
  ylab("7-Tage-Hospitalisierungsinzidenz") +
  xlab("Datum") +
  ggtitle("7-Tage-Hospitalisierungsinzidenz nach Bundesland und Altersgruppe", subtitle = paste0("Stand: ", format(lubridate::now(), "%d.%m.%Y"), ", Impfquoten vom 16.09.2021, Daten: RKI")) +
  theme(text = element_text(size = 11),
        strip.text.y = element_text(angle = 0))
dev.off()




