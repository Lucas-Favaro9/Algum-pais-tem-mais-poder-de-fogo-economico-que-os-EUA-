setwd('C:\\Users\\Lucas\\Downloads\\Relatório tarifaço')

library(tidyverse)
library(readxl)
library(magrittr)
library(scales)
library(grid)

# Importando dados
export <- read_excel('export.xlsx')
import <- read_excel('import.xlsx')
gdp <- read_excel('gdp.xls')

# Modificando o nome de alguns países
gdp %<>%
  mutate(Country = replace(Country, Country == 'Brunei Darussalam', 'Brunei'),
         Country = replace( Country, Country == 'Czechia', 'Czech Republic'),
         Country = replace(Country, Country == 'Hong Kong SAR, China', 'Hong Kong, China'),
         Country = replace(Country, Country == 'Timor-Leste', 'East Timor'),
         Country = replace(Country, Country == 'Turkiye', 'Turkey'),
         Country = replace(Country, Country == 'Viet Nam', 'Vietnam'))

export %<>%
  mutate(Country = replace(Country, Country == 'Ethiopia(excludes Eritrea)', 'Ethiopia'),
         Country = replace(Country, Country == 'Serbia, FR(Serbia/Montenegro)', 'Serbia'))
         
import %<>%
  mutate(Country = replace(Country, Country == 'Ethiopia(excludes Eritrea)', 'Ethiopia'),
         Country = replace(Country, Country == 'Serbia, FR(Serbia/Montenegro)', 'Serbia'))

## EXERCÍCIO IMPORTAÇÕES ----

# Calculando a fração das importações dos EUA no produto dos países
base_ex <- left_join(export, gdp, by = 'Country') %>%
  mutate(valor_ex = Export/gdp*100) %>%
  select(Country, valor_ex)

# Calculando a fração das importações dos países no produto dos EUA
gdp_USA <- gdp %>%
  filter(Country == "United States") %>%
  pull(gdp)

base_im <- import %>%
  mutate(valor_im = Import/gdp_USA*100) %>%
  select(-Import)

# Juntando as bases
base_M <- left_join(base_ex, base_im, by = 'Country')

base_M[base_M == "World"] <- NA
base_M[base_M == "East Asia & Pacific"] <- NA
base_M[base_M == "Europe & Central Asia"] <- NA
base_M[base_M == "Latin America & Caribbean"] <- NA
base_M[base_M == "North America"] <- NA
base_M[base_M == "Middle East & North Africa"] <- NA
base_M[base_M == "South Asia"] <- NA
base_M[base_M == "Sub-Saharan Africa"] <- NA
base_M <- na.omit(base_M)

base_M %<>%
  mutate(leverage_M = if_else(valor_ex > valor_im, 1, 0),
         diferença_M = valor_ex - valor_im)

# Gráfico 1
ggplot(base_M, aes(x = valor_ex, y = valor_im)) +
  geom_point(color = "lightblue") +
  geom_point(data = subset(base_M, Country == "China"), aes(x = valor_ex, y = valor_im),
             color = "red", size = 2) +
  geom_point(data = subset(base_M, Country == "Russian Federation"), aes(x = valor_ex, y = valor_im),
             color = "red", size = 2) +
  geom_text(data = subset(base_M, Country == "China"),
            aes(x = valor_ex, y = valor_im + .3, label = "China"), color = "black", size = 3.5) +
  geom_text(data = subset(base_M, Country == "Russian Federation"),
            aes(x = valor_ex - .03, y = valor_im + .5, label = "Rússia"), color = "black", size = 3.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_segment(aes(x = 5, y = 5, xend = 5, yend = 7),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "black") +
  geom_segment(aes(x = 6, y = 6, xend = 6, yend = 4),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "black") +
  annotate("text", x = 3.2, y = 7.2, label = "Vantagem do outro país", hjust = 0, size = 3.5) +
  annotate("text", x = 4.5, y = 3.8, label = "Vantagem dos EUA", hjust = 0, size = 3.5) +
  labs(x = "Fração 'Importação dos EUA pelo país'/'PIB do país' (em %)",
       y = "Fração 'Importação do país pelos EUA'/'PIB dos EUA' (em %)") +
  theme_minimal() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))

ggsave(plot = last_plot(), file = "gráfico_M.png", width = 5, height = 5, bg = 'white')

# Gráfico 2
base_2 <- left_join(base_M, gdp, by = 'Country') %>%
  arrange(desc(gdp)) %>%
  head(50) %>%
  select(Country, diferença_M)

base_2$fill_color <- ifelse(base_2$Country == "Brazil", "Brazil",
                            ifelse(base_2$Country %in% c("Russian Federation", "China"),
                                   "Competidores", "Outros"))

ggplot(base_2, aes(x = reorder(Country, -diferença_M),
                   y = diferença_M, fill = fill_color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Brazil" = "darkgreen", 
                               "Competidores" = "red", 
                               "Outros" = "steelblue")) +
  labs(y = "Diferença (em p.p.)", x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ggsave(plot = last_plot(), file = "gráfico_M_2.png", width = 10, height = 5, bg = 'white')

## EXERCÍCIO EXPORTAÇÕES ----

# Calculando a fração das exportações para os EUA no produto dos países
base_ex_1 <- left_join(import, gdp, by = 'Country') %>%
  mutate(valor_ex = Import/gdp*100) %>%
  select(Country, valor_ex)

# Calculando a fração das exportações dos EUA aos países no produto dos EUA
base_ex_2 <- export %>%
  mutate(valor_ex_2 = Export/gdp_USA*100) %>%
  select(-Export)

# Juntando as bases
base_X <- left_join(base_ex_1, base_ex_2, by = 'Country')

base_X[base_X == "World"] <- NA
base_X[base_X == "East Asia & Pacific"] <- NA
base_X[base_X == "Europe & Central Asia"] <- NA
base_X[base_X == "Latin America & Caribbean"] <- NA
base_X[base_X == "North America"] <- NA
base_X[base_X == "Middle East & North Africa"] <- NA
base_X[base_X == "South Asia"] <- NA
base_X[base_X == "Sub-Saharan Africa"] <- NA
base_X <- na.omit(base_X)

base_X %<>%
  mutate(leverage_X = if_else(valor_ex > valor_ex_2, 1, 0),
         diferença_X = valor_ex - valor_ex_2)

# Gráfico 1
ggplot(base_X, aes(x = valor_ex, y = valor_ex_2)) +
  geom_point(color = "lightblue") +
  geom_point(data = subset(base_X, Country == "China"), aes(x = valor_ex, y = valor_ex_2),
             color = "red", size = 2) +
  geom_text(data = subset(base_X, Country == "China"),
            aes(x = valor_ex, y = valor_ex_2 + .3, label = "China"), color = "black", size = 3.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_segment(aes(x = 5, y = 5, xend = 5, yend = 7),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "black") +
  geom_segment(aes(x = 6, y = 6, xend = 6, yend = 4),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "black") +
  annotate("text", x = 3.2, y = 7.2, label = "Vantagem do outro país", hjust = 0, size = 3.5) +
  annotate("text", x = 4.5, y = 3.8, label = "Vantagem dos EUA", hjust = 0, size = 3.5) +
  labs(x = "Fração 'Exportação do país aos EUA'/'PIB do país' (em %)",
       y = "Fração 'Exportação dos EUA ao país'/'PIB dos EUA' (em %)") +
  theme_minimal() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))

ggsave(plot = last_plot(), file = "gráfico_X.png", width = 5, height = 5, bg = 'white')

# Gráfico 2
base_2 <- left_join(base_X, gdp, by = 'Country') %>%
  arrange(desc(gdp)) %>%
  head(50) %>%
  select(Country, diferença_X)

base_2$fill_color <- ifelse(base_2$Country == "Brazil", "Brazil",
                            ifelse(base_2 == "China" , "China", "Outros"))

ggplot(base_2, aes(x = reorder(Country, -diferença_X),
                   y = diferença_X, fill = fill_color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Brazil" = "darkgreen", 
                               "China" = "red", 
                               "Outros" = "steelblue")) +
  labs(y = "Diferença (em p.p.)", x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ggsave(plot = last_plot(), file = "gráfico_X_2.png", width = 10, height = 5, bg = 'white')

## JUNTANDO AMBOS ----

base <- inner_join(base_M, base_X, by = 'Country') %>%
  select(Country, diferença_M, diferença_X) %>%
  mutate(soma = diferença_M + diferença_X)

base <- left_join(base, gdp, by = 'Country') %>%
  arrange(desc(gdp)) %>%
  head(50) %>%
  select(Country, soma)

base$fill_color <- ifelse(base$Country == "Brazil", "Brazil",
                          ifelse(base$Country == "China", "China", "Outros"))

ggplot(base, aes(x = reorder(Country, -soma),
                   y = soma, fill = fill_color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Brazil" = "darkgreen", 
                               "China" = "red", 
                               "Outros" = "steelblue")) +
  labs(y = "Diferença (em p.p.)", x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ggsave(plot = last_plot(), file = "agregado.png", width = 10, height = 5, bg = 'white')
