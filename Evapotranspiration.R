## Jecam analiza pliku CSV z wartościami ewapotranspiracji pól uprawnych w obszarze

tabela1 <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - 166-177_poranki.csv")

# Ładowanie biblioteki ggplot2
library(ggplot2)
library(tidyverse)

tabela1


# lista upraw

uprawy_do_agregacji <- c("rzepak ozimy", "pszenica ozima", "buraki cukrowe", "kukurydza", "laka")

# Agregowanie danych

pola2_long <- reshape2::melt(tabela1, id.vars = c("uprawa", "NR_POLA"), measure.vars = paste0("X", num_dni))
pola2_agregowane <- subset(pola2_long, uprawa %in% uprawy_do_agregacji)
pola2_agregowane <- aggregate(value ~ variable + uprawa, data = pola2_agregowane, mean) # Uśrednienie wartości

pola2_agregowane


uprawy <- ggplot(pola2_agregowane, aes(x = variable, y = value, color = uprawa, group = uprawa)) +
  geom_line() +
  labs(title = "Zmiany wysokości upraw w kolejnych dniach",
       x = "Numer dnia roku",
       y = "Średnia wysokość uprawy") +
  theme_minimal()

uprawy

mapowanie_dni <- c("15.06.2022; 9:24", "18.06.2022; 8:36", "20.06.2022; 7:48", "23.06.2022; 6:12", "24.06.2022 7:01", "26.06.2022; 7:01")

uprawy1 <- ggplot(pola2_agregowane, aes(x = variable, y = value, color = uprawa, group = uprawa)) +
  geom_line() +
  labs(title = "Zmiany w ewapotranspiracji upraw obserwowanych w dniach 15.06.2022 - 24.06.2022 w godzinach 6:12-9:24",
       x = "Czas pomiaru",
       y = "Średnia ewapotranspiracja z powierzchni liści rośliny") +
  theme_minimal() +
  scale_x_discrete(labels = mapowanie_dni)+
  scale_y_continuous(limits = c(0, 100))

uprawy1

## Druga cześć pomiarów upraw (27.07.2022-3.08.2022, godziny popołudniowe)

tabela2 <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - 208-215 popudnia-3.csv")

tabela2

uprawy_do_agregacji <- c("rzepak ozimy", "pszenica ozima", "buraki cukrowe", "kukurydza", "laka")

dane_avg <- tabela2 %>%
  group_by(uprawa) %>%
  summarise_at(vars(starts_with("X")), mean, na.rm = TRUE)

dane_long <- dane_avg %>%
  pivot_longer(cols = starts_with("X"), names_to = "dzien", values_to = "wysokosc")

dane_long

mapowanie_dni2 <- c("X208" = "27.07.2022;16:32", "X212" = "31.07.2022;14:55", "X213" = "31.07.2022;16:31", "X214" = "02.08.2022;14:54", "X215" = "03.08.2022;15:43")

uprawy2 <- ggplot(dane_long, aes(x = dzien, y = wysokosc, color = uprawa, group = uprawa)) +
  geom_line() +
  labs(title = "Zmiany w ewapotranspiracji upraw obserwowanych w dniach 27.07.2022 - 03.08.2022 w godzinach 14:54-16:32",
       x = "Czas Pomiaru",
       y = "Średnia ewapotranspiracja z powierzchni liści rośliny") +
  theme_minimal() +
  scale_x_discrete(labels = mapowanie_dni2) +
  scale_y_continuous(limits = c(0, 100))

uprawy2
