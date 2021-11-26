require(data.table)
require(tidyverse)

# Impfquoten vom 16.09.2021, vor der 4. Welle und erstes Bereitstellungsdatum
quoten <- fread("https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/raw/master/Archiv/2021-09-17_Deutschland_Impfquoten_COVID-19.csv", encoding = "UTF-8")
quoten <- quoten %>%
  select(Bundesland, Impfquote_gesamt_min1, Impfquote_gesamt_voll) %>%
  filter(Bundesland != "Bundesressorts")

# Hospitalisierungsraten (ausgewählte Altersgruppen)
df <- fread("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/master/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv", encoding = "UTF-8")
df <- df %>%
  filter(Bundesland != "Bundesgebiet", Altersgruppe %in% c("15-34", "35-59", "60-79", "80+"), Datum >= lubridate::ymd("20210916")) %>%
  merge(x = ., y = quoten, by = "Bundesland") %>%
  mutate(Bundesland = case_when(Bundesland == "Nordrhein-Westfalen" ~ "NW",
                                Bundesland == "Berlin" ~ "BE",
                                Bundesland == "Bremen" ~ "HB",
                                Bundesland == "Hamburg" ~ "HH",
                                Bundesland == "Rheinland-Pfalz" ~ "RP",
                                Bundesland == "Hessen" ~ "HE",
                                Bundesland == "Niedersachsen" ~ "NI",
                                Bundesland == "Sachsen" ~ "SN",
                                Bundesland == "Thüringen" ~ "TH",
                                Bundesland == "Schleswig-Holstein" ~ "SH",
                                Bundesland == "Sachsen-Anhalt" ~ "ST",
                                Bundesland == "Saarland" ~ "SL",
                                Bundesland == "Brandenburg" ~ "BB",
                                Bundesland == "Mecklenburg-Vorpommern" ~ "MV",
                                Bundesland == "Bayern" ~ "BY",
                                Bundesland == "Baden-Württemberg" ~ "BW"))
# Bundesland als Faktor
df$Bundesland <- as.factor(df$Bundesland)
df$Bundesland <- reorder(df$Bundesland, -df$Impfquote_gesamt_voll)

# Plot
pdf(paste0(format(lubridate::now(), "%Y%m%d"),"_impfquote_hospitalisierung.pdf"), width = 10, height = 14)
ggplot(df) +
  geom_line(aes(x = Datum, y = `7T_Hospitalisierung_Inzidenz`, col = Impfquote_gesamt_voll)) +
  scale_color_gradient(low = "red", high = "green", "Vollständig Geimpfte (%)", breaks = seq(min(df$Impfquote_gesamt_voll), max(df$Impfquote_gesamt_voll), length.out = 3)) +
  scale_x_date(breaks = "1 month", date_labels = "%d. %b") +
  facet_grid(rows = vars(Bundesland), cols = vars(Altersgruppe)) +
  ylab("7-Tage-Hospitalisierungsinzidenz") +
  xlab("") +
  ggtitle("7-Tage-Hospitalisierungsinzidenz nach Bundesland und Altersgruppe", subtitle = "Impfquoten vom 16.09.2021, Quelle: RKI") +
  theme(text = element_text(size = 11),
        strip.text.y = element_text(angle = 0))
dev.off()






