require(data.table)
require(tidyverse)

# Impfquoten
quoten <- fread("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Impfquoten_COVID-19.csv", encoding = "UTF-8")
quoten <- quoten %>%
  select(Bundesland, Impfquote_gesamt_min1, Impfquote_gesamt_voll) %>%
  filter(Bundesland != "Bundesressorts")

# Hospitalisierungsraten (ausgewählte Altersgruppen)
df <- fread("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/master/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv", encoding = "UTF-8")
df <- df %>%
  filter(Bundesland != "Bundesgebiet", Altersgruppe %in% c("15-34", "35-59", "60-79", "80+"), Datum >= lubridate::ymd("20210801")) %>%
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
                                Bundesland == "Mecklenburg-Vorpommern" ~ "NRW",
                                Bundesland == "Bayern" ~ "BY",
                                Bundesland == "Baden-Württemberg" ~ "BW"))
# Bundesland als Faktor
df$Bundesland <- as.factor(df$Bundesland)
df$Bundesland <- reorder(df$Bundesland, -df$Impfquote_gesamt_voll)

# Plot
ggplot(df) +
  geom_line(aes(x = Datum, y = `7T_Hospitalisierung_Inzidenz`, col = Impfquote_gesamt_voll)) +
  scale_color_gradient(low = "red", high = "green", "Quote der voll-\nständigen Impfungen (%)", breaks = seq(min(df$Impfquote_gesamt_voll), max(df$Impfquote_gesamt_voll), length.out = 3)) +
  facet_grid(rows = vars(Bundesland), cols = vars(Altersgruppe)) +
  ylab("7-Tage Hospitalisierung Inzidenz") +
  xlab("") +
  ggtitle("Inzidenz der 7-Tage Hospitalisierung nach Bundesland und Altersgruppe", subtitle = "Impfquoten vom 22.11.2021, Quelle: RKI") +
  theme(text = element_text(size = 14))


