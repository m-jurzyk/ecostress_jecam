## Jecam analiza pliku CSV z wartościami ewapotranspiracji pól uprawnych w obszarze

tabela1 <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - 166-177_poranki.csv")

# Ładowanie biblioteki ggplot2
library(ggplot2)
library(tidyverse)

tabela1


# Lista upraw, które chcesz zagregować
uprawy_do_agregacji <- c("rzepak ozimy", "pszenica ozima", "buraki cukrowe", "kukurydza", "laka")

# Przekształcenie danych do postaci długiej (long format) i zagregowanie danych
pola2_long <- reshape2::melt(tabela1, id.vars = c("uprawa", "NR_POLA"), measure.vars = paste0("X", num_dni))
pola2_agregowane <- subset(pola2_long, uprawa %in% uprawy_do_agregacji)
pola2_agregowane <- aggregate(value ~ variable + uprawa, data = pola2_agregowane, mean) # Uśrednienie wartości

uprawy <- ggplot(pola2_agregowane, aes(x = variable, y = value, color = uprawa, group = uprawa)) +
  geom_line() +
  labs(title = "Zmiany wysokości upraw w kolejnych dniach",
       x = "Numer dnia roku",
       y = "Średnia wysokość uprawy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

uprawy

mapowanie_dni <- c("15 czerwca", "18 czerwca", "19 czerwca", "20 czerwca", "24 czerwca", "26 czerwca")

uprawy1 <- ggplot(pola2_agregowane, aes(x = variable, y = value, color = uprawa, group = uprawa)) +
  geom_line() +
  labs(title = "Zmiany w ewapotranspiracji upraw obserwowanych w dniach 15.06.2022 - 24.06.2022 w godzinach 6:30-10:00",
       x = "Data",
       y = "Ewapotranspiracja z powierzchni liści rośliny) +
  theme_minimal() +
  scale_x_discrete(labels = mapowanie_dni)+
  scale_y_continuous(limits = c(0, 100))# Ustawienie etykiet na osi X jako daty

uprawy1

