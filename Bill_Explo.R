# Tarea 1 | Licenciatura Otoño 2024

library(tidyverse)
library(stargazer)
library(sandwich)
library(hdm) 
library(magrittr)
library(MatchIt)
library(car)
library(AER)
library(schoolmath)
library(jtools) 
library(dplyr)
library(estimatr)
library(fixest)
library(kableExtra)
library(scales)
library(ggpubr)
library(lmtest)
library(haven)
library(tidyr) 
library(ggplot2)

## Base de datos Billionaires_2023

bill1 <- read.csv("Billionaires_Statistics_Dataset.csv")
bill1 <- data.frame(bill1)
bill1 <- dplyr::select(bill1, rank, personName, finalWorth, age, country, industries, selfMade, gender, gdp_country, gross_tertiary_education_enrollment,tax_revenue_country, total_tax_rate_country, population_country)
bill1 <- bill1 %>% rename(tax_revenue_country = tax_revenue_country)
bill1 <- na.omit(bill1)
bill1$selfMade <- ifelse(bill1$selfMade == TRUE, 1, 0)
bill1$female <- ifelse(bill1$gender == "F", 1, 0)
bill1 <- dplyr::select(bill1, -gender)
bill1$gdp_country <- as.numeric(gsub("[\\$,]", "", bill1$gdp_country))
bill1 <- bill1 %>% 
  mutate(gdp_country = gdp_country / 1e9)
bill1 <- bill1 %>% 
  mutate(population_country = population_country / 1e6)

colnames(bill1)

write.csv(bill1, file = "Billionaires_2023.csv", row.names = FALSE)

## Base de datos Philanthropy_2023

phil1 <- read.csv("subset_data.csv")
phil1 <- data.frame(phil1)
phil1 <- phil1 %>% 
  select(Name_Org, Self.Made.Score, Philanthropy.Score, Marital.Status, Bachelor, Master, Doctorate, Drop.Out)
bill_phil1 <- merge(bill1, phil1, by.x = "personName", by.y = "Name_Org", all.x = TRUE)
bill_phil1 <- bill_phil1[match(bill1$personName, bill_phil1$personName), ]
bill_phil1 <- na.omit(bill_phil1)
colnames(bill_phil1)

write.csv(bill_phil1, file = "Philanthropy_2023.csv", row.names = FALSE)

bill <- read.csv("Billionaires_2023.csv")


# Convertir el dataframe de formato ancho a largo para usar ggplot de manera eficiente
bill_long <- bill %>%
  pivot_longer(cols = c(rank, finalWorth, age, selfMade, cpi_country, gdp_country,gross_tertiary_education_enrollment,gross_primary_education_enrollment_country,tax_revenue_country,total_tax_rate_country,population_country,female),
               names_to = "variable", values_to = "value")

# Distribución de variables numéricas 
ggplot(bill_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de Variables", x = "Valor", y = "Frecuencia")

# Proporción de mujeres por industria
proporcion_mujeres_industria <- bill %>%
  group_by(industries) %>%
  summarise(proporcion_mujeres = mean(female)) %>%
  arrange(desc(proporcion_mujeres)) 

ggplot(proporcion_mujeres_industria, aes(x = reorder(industries, proporcion_mujeres), y = proporcion_mujeres)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  coord_flip() +  
  labs(title = "Proporción de Mujeres por Industria", x = "Industria", y = "Proporción de Mujeres") +
  theme_minimal()

# Proporción de self made por industria
proporcion_selfmade_industria <- bill %>%
  group_by(industries) %>%
  summarise(proporcion_selfMade = mean(selfMade)) %>%
  arrange(desc(proporcion_selfMade))

ggplot(proporcion_selfmade_industria, aes(x = reorder(industries, proporcion_selfMade), y = proporcion_selfMade)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  coord_flip() +  
  labs(title = "Proporción de SelfMade por Industria", x = "Industria", y = "Proporción de SelfMade") +
  theme_minimal()

# Proporción de mujeres self-made vs hombres self-made
proporcion_selfmade_genero <- bill %>%
  group_by(female) %>%
  summarise(proporcion_selfMade_genero = mean(selfMade)) %>%
  arrange(desc(proporcion_selfMade_genero))

# Edad de mujeres vs hombres
edad_genero <- bill %>%
  group_by(female) %>%
  summarise(edad_female = mean(age))

# Relación entre educación terciaria y la industria
modelo_terciaria_industria <- lm(gross_tertiary_education_enrollment ~ industries, data = bill)
summary(modelo_terciaria_industria)
coefficients_model_ti <- coef(modelo_terciaria_industria)
sorted_coefficients_ti <- sort(coefficients_model_ti, decreasing = TRUE)


## Distribución en diferentes industrias, wealth share, etc 
wealth_by_industry <- bill %>%
  group_by(industries) %>%
  summarise(total_wealth_industry = sum(finalWorth, na.rm = TRUE))

total_wealth <- sum(bill$finalWorth)

wealth_by_industry <- wealth_by_industry %>%
  arrange(desc(wealth_share_industry)) %>%
  mutate(industries = factor(industries, levels = industries))

ggplot(wealth_by_industry, aes(x = "", y = wealth_share_industry, fill = industries)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución del Wealth por Industria", y = "", x = "") +
  theme_void() + # Remueve las etiquetas y líneas innecesarias
  theme(legend.title = element_blank()) +  # Elimina el título de la leyenda
  scale_fill_discrete(name = "Industrias") # Etiqueta para la leyenda

top_5_industries <- wealth_by_industry %>%
  top_n(5, wt = total_wealth_industry) 

top_5_industries_df <- bill %>%
  filter(industries %in% top_5_industries$industries)

ggplot(bill, aes(x = log(finalWorth))) +
  geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
  facet_wrap(~ industries, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de la Riqueza por Industria", x = "Log Riqueza Final (en millones de dólares)", y = "Frecuencia")

ggplot(top_5_industries_df, aes(x = log(finalWorth))) +
  geom_density(alpha = 0.5, fill = "cornflowerblue",color = "black") +
  facet_wrap(~ industries, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de la Proporción de Riqueza para las 5 Principales Industrias",
       x = "Log Riqueza Final",
       y = "Densidad") 

ggplot(top_5_industries_df, aes(x = log(finalWorth))) +
  geom_density(alpha = 0.5, fill = "cornflowerblue",color = "black") +
  facet_wrap(~ industries, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de la Proporción de Riqueza para las 5 Principales Industrias",
       x = "Log Riqueza Final",
       y = "Densidad") 

Edad_Industria <- bill %>%
  group_by(industries) %>%
  summarise(age_industry = mean(age)) %>%
  arrange(desc(age_industry))

ggplot(bill, aes(x = age)) +
  geom_density(alpha = 0.5, fill = "cornflowerblue",color = "black") +
  facet_wrap(~ industries, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de la Proporción de Riqueza para las 5 Principales Industrias",
       x = "Log Riqueza Final",
       y = "Densidad") 

#Países 

## Número de multimillonarios 

billionaire_count_country <- bill %>%
  group_by(country) %>%
  summarise(num_billionaires = n()) %>%
  arrange(desc(num_billionaires))

print(billionaire_count_country, n=64)

top_20_bill_count_country <- billionaire_count_country %>%
  top_n(20, wt = num_billionaires)

top_20_bill_count <- bill %>%
  filter(country %in% top_20_bill_count_country$country)

ggplot(top_20_bill_count_country, aes(x = reorder(country, num_billionaires), y = num_billionaires, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Rotar el gráfico para una mejor visualización
  labs(title = "Número de Multimillonarios por País", x = "País", y = "Número de Multimillonarios") +
  theme_minimal() +
  theme(legend.position = "none")  

top_20_countries_count_df <- bill %>%
  filter(country %in% top_20_bill_count_country$country)

ggplot(top_20_countries_count_df, aes(x = log(finalWorth))) +
  geom_density(alpha = 0.5, fill = "cornflowerblue",color = "black") +
  facet_wrap(~ country, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de la Proporción de Riqueza para países con mayor número de Multimillonarios",
       x = "Log Riqueza Final",
       y = "Densidad") 

## Con mayor riqueza acumulada 

billionaire_wealth_country <- bill %>%
  group_by(country) %>%
  summarise(total_wealth_country = sum(finalWorth, na.rm = TRUE)) %>%
  arrange(desc(total_wealth_country))

top_20_bill_wealth_country <- billionaire_wealth_country %>%
  top_n(20, wt = total_wealth_country)

ggplot(top_20_bill_wealth_country, aes(x = reorder(country, total_wealth_country), y = total_wealth_country, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +  
  labs(title = "Total Wealth by Country", x = "Country", y = "Total Wealth") +
  theme_minimal() +
  theme(legend.position = "none") 

## Promedio 

avg_billionaire_wealth_country <- bill %>%
  group_by(country) %>%
  summarise(avg_wealth = mean(finalWorth, na.rm = TRUE),
            num_billionaires = n()) %>%
  arrange(desc(avg_wealth))

filtered_countries <- avg_billionaire_wealth_country %>%
  filter(num_billionaires > 5)

top_20_bill_avg_wealth_country <- avg_billionaire_wealth_country %>%
  top_n(20, wt = avg_wealth)%>%
  arrange(desc(avg_wealth))

## finalWorth como % del PIB (Dado que el PIB esta en miles de millones y finalWeealth en Millones tener cuidado)
bill <- bill %>%
  mutate(wealth_pct_gdp = finalWorth*1e6 / (gdp_country*1e9) * 100)

summary(bill$wealth_pct_gdp)

billionaires_over_0_1_pct_gdp <- bill %>%
  filter(wealth_pct_gdp > 10)
nrow(billionaires_over_0_1_pct_gdp)

max_wealth_pct_gdp_row <- bill %>%
  filter(wealth_pct_gdp == max(wealth_pct_gdp, na.rm = TRUE))

billionaire_rank_552 <- bill1 %>%
  filter(rank == 552) %>%
  filter(country == "Georgia")

## Bidzina Ivanishvili de Georgia es el multimillonario 27.61% del PIB 


ggplot(bill, aes(y = wealth_pct_gdp)) +
  geom_boxplot(fill = "cornflowerblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribución de % del PIB (finalWorth)",
       y = "% del PIB (finalWorth)",
       x = "") 

ggplot(bill, aes(x = wealth_pct_gdp)) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Wealth vs Avg Person",
       x = "Wealth as % of GDP",
       y = "Densidad") +
  theme_minimal()

ggplot(bill, aes(x = log(wealth_pct_gdp))) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Wealth vs Avg Person",
       x = "Wealth as % of GDP",
       y = "Densidad") +
  theme_minimal()

billionaire_wealth_country <- bill %>%
  group_by(country) %>%
  summarise(total_wealth_country = sum(finalWorth, na.rm = TRUE),
            gdp_country = first(gdp_country))

billionaire_wealth_country <- billionaire_wealth_country %>%
  mutate(aggregated_wealth_percentage_of_gdp = (total_wealth_country*1e6 / (gdp_country*1e9)) * 100)

top_billionaire_wealth_countries <- billionaire_wealth_country %>%
  arrange(desc(aggregated_wealth_percentage_of_gdp)) %>%
  top_n(20, wt = aggregated_wealth_percentage_of_gdp)

ggplot(top_billionaire_wealth_countries, aes(x = reorder(country, aggregated_wealth_percentage_of_gdp), y = aggregated_wealth_percentage_of_gdp, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +  
  labs(title = "% of GDP that billionaires' wealth represents", x = "Country", y = "%") +
  theme_minimal() +
  theme(legend.position = "none") 

total_billionaire_wealth_world <- sum(bill$finalWorth, na.rm = TRUE)
total_gdp_world <- bill %>%
  group_by(country) %>%
  summarise(gdp_country = first(gdp_country)) %>%
  summarise(total_gdp_world = sum(gdp_country, na.rm = TRUE)) %>%
  pull(total_gdp_world)

world_billionaire_wealth_percentage <- ((total_billionaire_wealth_world*1e6) / (total_gdp_world*1e9) * 100)

summary(bill$tax_revenue_country) 
summary(bill$total_tax_rate_country) 

## Cuanto más que la persona promedio de su país 

bill <- bill %>%
  mutate(gdp_per_capita = ((gdp_country*1e9) / (population_country*1e6)))

summary(bill$gdp_per_capita)
summary(bill$finalWorth)

bill <- bill %>%
  mutate(wealth_vs_avg_person = finalWorth*1e6 / gdp_per_capita)  

summary(bill$wealth_vs_avg_person)

ggplot(bill, aes(x = log(wealth_vs_avg_person))) +
  geom_density(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Wealth vs Avg Person",
       x = "Wealth vs Avg Person",
       y = "Densidad") +
  theme_minimal()

ggplot(bill, aes(x = wealth_vs_avg_person)) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Wealth vs Avg Person",
       x = "Wealth vs Avg Person",
       y = "Densidad") +
  theme_minimal()

ggplot(bill, aes(x = log(wealth_vs_avg_person))) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Wealth vs Avg Person",
       x = "Wealth vs Avg Person",
       y = "Densidad") +
  theme_minimal()



top_20_billionaires %>% select(personName, country, finalWorth,gdp_country,wealth_pct_gdp, gdp_per_capita, wealth_vs_avg_person)


## Tabla descriptiva 

summary(bill$gross_tertiary_education_enrollment)

bill_tb <- bill %>%
  select(rank, finalWorth, age, selfMade, cpi_country, gdp_country,gross_tertiary_education_enrollment,gross_primary_education_enrollment_country,tax_revenue_country,total_tax_rate_country,population_country,female, wealth_pct_gdp, gdp_per_capita,wealth_vs_avg_person)

nombres <- colnames(bill_tb)

is_dummy <- ifelse(nombres %in% c("selfMade", "female"), "Si", "No")

dummy_info <- data.frame(Variable = nombres, Is_Dummy = is_dummy)

tabletb_bill <- stargazer(bill_tb, title = "Estadísticas Descriptivas", 
                          out = "table1c.tex", 
                          notes = c("Nota: 'Si' en la columna 'Is_Dummy' indica una variable dummy. 
                     Variables dummy en este conjunto de datos incluyen: selfMade, female"))

table_td <- stargazer(bill_tb,
                      title = "Estadísticas Descriptivas",
                      out = "table1c.tex")

## Modelos 

model1 <- lm(finalWorth ~ age + log(gross_tertiary_education_enrollment) + selfMade + industries, data = bill)
summary(model1)

model2 <- lm(log(finalWorth) ~ age + log(gross_tertiary_education_enrollment) + selfMade + industries, data = bill)

summary(model2)


mexico_data <- bill %>%
  filter(country == "Mexico") %>% 
  select(personName, industries,selfMade,finalWorth,gdp_country,wealth_pct_gdp, gdp_per_capita, wealth_vs_avg_person)

(sum(mexico_data$finalWorth*1e6))/(mexico_data$gdp_country*1e9)
sum(mexico_data$wealth_pct_gdp)

bill %>%
  filter(grepl("Musk", personName, ignore.case = TRUE))


library(ineq)
gini_coefficient <- Gini(bill$finalWorth, na.rm = TRUE)
gini_coefficient

## Si los multimillonarios tendrian un pais, serian el 5to pais mas desigual en el 2023 
## Sudafrica 0.63 
## Namibia 0.58 
## Zambia 0.57 
## Mozambique 0.57 
## Jamaica 0.53 

## Wealth as % of GDP of ocuntry 

bill_phil <- bill_phil %>%
  mutate(wealth_pct_gdp = finalWorth*1e6 / (gdp_country*1e9) * 100)

summary(bill_phil$wealth_pct_gdp)

max_wealth_pct_gdp_row <- bill_phil %>%
  filter(wealth_pct_gdp == max(wealth_pct_gdp, na.rm = TRUE))

## Iris Fontbona Chile 8.18% 

ggplot(bill_phil, aes(x = Philanthropy.Score)) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Wealth vs Avg Person",
       x = "Wealth vs Avg Person",
       y = "Densidad") +
  theme_minimal()

summary(bill$finalWorth)
### PHILANTHROPY SCORE 
# --[a]---

# Cálculo de la media muestral 
(media_ps <- mean(bill_phil$Philanthropy.Score))

# Cálculo de la varianza muestral
(var_ps <- var(bill_phil$Philanthropy.Score))

# Asignar a n número de observaciones 
(n <- nrow(bill_phil))

# Intervalo de Confianza 
(IC1 <- media_ps-1.96*sqrt(var_ps/n))
(IC2 <- media_ps+1.96*sqrt(var_ps/n))

## Bailleres 

# 1. Definir los parámetros conocidos

#Media muestral de Precio de Menu para todos los foodtrucks menos los de tacos 
(finalWorth_Bailleres <- bill$finalWorth[bill$personName == "Alejandro Bailleres Gual & family"])

# 2. Calcular la media y desviación estándar de la muestra
(mean_finalWorth <- mean(bill$finalWorth, na.rm = TRUE))
(sd_finalWorth <- sd(bill$finalWorth, na.rm = TRUE))
(sd_finalWorth <- sqrt(var(bill$finalWorth)))
(n <- length(bill$finalWorth)) 

# 3. Calcular el estadístico t para la prueba
(t_stat <- (finalWorth_Bailleres - mean_finalWorth) / (sd_finalWorth / sqrt(n)))

(p_value <- 2*(1-pnorm(abs(t_stat))))

# 4. Calcular el valor p 
# Valor p con la distribución t
(p_value_t <- 2 * pt(-abs(t_stat), df = n - 1))

# Valor p con la aproximación normal
(p_value_normal <- 2 * (1 - pnorm(abs(t_stat))))

# Patrimonio neto de Bailleres (en millones de dólares)
X_Bailleres <- 6900

# Calcular la media muestral del patrimonio neto de los multimillonarios
(mean_finalWorth <- mean(bill$finalWorth, na.rm = TRUE))

# Calcular el estadístico T
(T <- X_Bailleres / mean_finalWorth)

# Calcular la varianza muestral del patrimonio neto de los multimillonarios
(var_finalWorth <- var(bill$finalWorth, na.rm = TRUE))

# Calcular el número de multimillonarios en la muestra
(n <- length(bill$finalWorth))

# Calcular la varianza de T
(var_T <- (X_Bailleres^2 * var_finalWorth) / (n * mean_finalWorth^4))

# Calcular el error estándar de T
(se_T <- sqrt(var_T))

# Calcular el intervalo de confianza
(IC_lower2<- T - 1.96 * se_T)
(IC_upper2 <- T + 1.96 * se_T)

## Scatter Plots 
# Crear la gráfica de edad vs patrimonio neto, distinguiendo por selfMade
ggplot(bill, aes(x = age, y = log(finalWorth), color = as.factor(selfMade))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # Línea de regresión
  labs(title = "Relación entre Edad y Patrimonio Neto",
       x = "Edad",
       y = "Patrimonio Neto (FinalWorth)",
       color = "Self-made") +
  theme_minimal()


# Crear la gráfica de PIB vs patrimonio neto, distinguiendo por selfMade

ggplot(bill, aes(x = log(gdp_country), y = log(finalWorth), color = as.factor(selfMade))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # Línea de regresión
  labs(title = "Relación entre PIB del País y Patrimonio Neto",
       x = "Log PIB del País (GDP_country)",
       y = "Log Patrimonio Neto (FinalWorth)",
       color = "Self-made") +
  theme_minimal()

# Modelo 1: Relación entre Edad y Patrimonio Neto (log(finalWorth) ~ age)
modelo_edad <- lm(log(finalWorth) ~ age, data = bill)

# Resumen del modelo
summary(modelo_edad)


# Modelo 2: Relación entre Log(PIB del país) y Log(Patrimonio Neto)
modelo_pib <- lm(log(finalWorth) ~ log(gdp_country), data = bill)

# Resumen del modelo
summary(modelo_pib)

# Scatter plot con ajustes polinomiales de grado 2 y 3
ggplot(bill, aes(x = age, y = log(finalWorth))) +
  geom_point(alpha = 0.5) +  # Puntos del scatterplot
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue", linetype = "dashed") +  # Polinomio grado 2
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "red") +  # Polinomio grado 3
  labs(title = "Relación entre Edad y Patrimonio Neto con Ajustes Polinomiales",
       x = "Edad",
       y = "Patrimonio Neto (FinalWorth)") +
  theme_minimal()

# Modelo 1: Polinomio de grado 2
modelo_grado_2 <- lm(finalWorth ~ age + I(age^2) + selfMade + female, data = bill)

# Modelo 2: Polinomio de grado 3
modelo_grado_3 <- lm(finalWorth ~ age + I(age^2) + I(age^3), data = bill)

# Resúmenes de los modelos
summary(modelo_grado_2)
summary(modelo_grado_3)

# Creación de finalWorth_75 como una dummy del percentil 75
bill$finalWorth_75 <- ifelse(bill$finalWorth > quantile(bill$finalWorth, 0.75, na.rm = TRUE), 1, 0)

# Crear las dummies para las industrias
bill$Technology <- ifelse(bill$industries == "Technology", 1, 0)
bill$Finance <- ifelse(bill$industries == "Finance & Investments", 1, 0)
bill$Fashion_Retail <- ifelse(bill$industries == "Fashion & Retail", 1, 0)
bill$Manufacturing <- ifelse(bill$industries == "Manufacturing", 1, 0)
bill$Food_Beverage <- ifelse(bill$industries == "Food & Beverage", 1, 0)

# Dummy 'Others' que agrupa las demás industrias
bill$Others <- ifelse(bill$Technology == 0 & bill$Finance == 0 & bill$Fashion_Retail == 0 &
                        bill$Manufacturing == 0 & bill$Food_Beverage == 0, 1, 0)


# Modelos con finalWorth como variable dependiente
reg_1 <- lm(finalWorth ~ age + female + selfMade + log(gdp_per_capita) + tax_revenue_country + 
              Technology + Finance + Fashion_Retail + Manufacturing + Food_Beverage, data = bill)

reg_2 <- lm(finalWorth ~ age + female + selfMade + log(gdp_per_capita) + total_tax_rate_country +
              Technology + Finance + Fashion_Retail + Manufacturing + Food_Beverage, data = bill)

# Modelos con log(finalWorth) como variable dependiente
reg_3 <- lm(log(finalWorth + 0.0001) ~ age + female + selfMade + log(gdp_per_capita) + tax_revenue_country + 
              Technology + Finance + Fashion_Retail + Manufacturing + Food_Beverage, data = bill)

reg_4 <- lm(log(finalWorth + 0.0001) ~ age + female + selfMade + log(gdp_per_capita) + total_tax_rate_country + 
              Technology + Finance + Fashion_Retail + Manufacturing + Food_Beverage, data = bill)

# Modelos con finalWorth_75 (dummy) como variable dependiente
reg_5 <- lm(finalWorth_75 ~ age + female + selfMade + log(gdp_per_capita) + tax_revenue_country + 
              Technology + Finance + Fashion_Retail + Manufacturing + Food_Beverage, data = bill)

reg_6 <- lm(finalWorth_75 ~ age + female + selfMade + log(gdp_per_capita) + total_tax_rate_country + 
              Technology + Finance + Fashion_Retail + Manufacturing + Food_Beverage, data = bill)

es_list <- lapply(list(reg_1, reg_2, reg_3, reg_4, reg_5, reg_6), function(x) sqrt(diag(vcovHC(x, type = "HC1"))))
stargazer(reg_1, reg_2, reg_3, reg_4, reg_5, reg_6,
          digits = 2,
          title = "Regresiones para finalWorth, log(finalWorth) y finalWorth_75",
          label = "tab:finalWorth_regresiones",
          omit.stat = c("f", "adj.rsq", "ser"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          single.row = FALSE,
          no.space = TRUE,
          se = es_list,  # Incluir los errores estándar robustos
          out = "tabla_finalWorth_regresiones.tex")
colnames(bill_phil)

mod1 <- lm(wealth_pct_gdp ~ age + Philanthropy.Score + selfMade + total_tax_rate_country + female + Bachelor , data = bill_phil)
summary(mod1)


yaNoSe <- bill_phil %>%
  select(personName,country,industries,selfMade,wealth_pct_gdp,wealth_vs_avg_person,Philanthropy.Score,Bachelor,Drop.Out)%>% 
  arrange(desc(bill_phil$wealth_pct_gdp))

mean(bill_phil$selfMade[bill_phil$country == "United States"])
mean(bill_phil$selfMade[bill_phil$country == "Mexico"])
mean(bill_phil$Self.Made.Score[bill_phil$country == "United States"])
mean(bill_phil$Self.Made.Score[bill_phil$country == "Mexico"])
mean(bill_phil$wealth_pct_gdp[bill_phil$country == "United States"])
mean(bill_phil$wealth_pct_gdp[bill_phil$country == "Mexico"])
mean(bill$wealth_pct_gdp[bill$country == "United States"])
mean(bill$wealth_pct_gdp[bill$country == "Mexico"])
sum(bill$country == "United States")
