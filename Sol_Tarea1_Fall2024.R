# Tarea 2 | Licenciatura Otoño 2024

# Para ejecutar este script desde su computadora pueden guardar
# la base de datos correspondiente a la tarea junto con este 
# script en un folder dentro de la memoria de su computadora 
# y crear un proyecto referenciado a dicho folder.

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
library(car)


# Importamos la base 
phil <- read.csv("Philanthropy_2023.csv")
colnames(phil)

# ====/ Procesamiento de Datos \====
## Variable worth_pct_gdp 
### Importante considerar que finalWorth se mide en millones de dólares 
### y GDP en mil millones
phil <- phil %>%
  mutate(worth_pct_gdp = finalWorth*1e6 / (gdp_country*1e9) * 100)
summary(phil$worth_pct_gdp)

# Variable gdp_per_capita 
### Importante considerar que population_country se mide en millones de dólares 
### y GDP en mil millones
phil <- phil %>%
  mutate(gdp_per_capita = ((gdp_country*1e9) / (population_country*1e6)))

# Variable worth_v_gdp_pc 
### Importante considerar que population_country se mide en millones de dólares 
### y GDP en mil millones
phil <- phil %>%
  mutate(worth_v_gdp_pc = finalWorth*1e6 / gdp_per_capita)

# Variable finalWorth_75 dummy 
phil$finalWorth_75 <- ifelse(phil$finalWorth > quantile(phil$finalWorth, 0.75, na.rm = TRUE), 1, 0)

# Crear las dummies para las industrias
phil$Technology <- ifelse(phil$industries == "Technology", 1, 0)
phil$Finance <- ifelse(phil$industries == "Finance & Investments", 1, 0)
phil$Fashion_Retail <- ifelse(phil$industries == "Fashion & Retail", 1, 0)
phil$Manufacturing <- ifelse(phil$industries == "Manufacturing", 1, 0)
phil$Food_Beverage <- ifelse(phil$industries == "Food & Beverage", 1, 0)

# Dummy 'Others' que agrupa las demás industrias
phil$Others <- ifelse(phil$Technology == 0 & phil$Finance == 0 & phil$Fashion_Retail == 0 &
                        phil$Manufacturing == 0 & phil$Food_Beverage == 0, 1, 0)

# ====/ Análisis Descriptivo \====
# --[1.Tabla descriptiva]---

unique(phil$Marital.Status)
# Agrupar Married, Engaged, In Relationship, Remarried bajo "InRelationship"
phil$in_relationship_group <- ifelse(phil$Marital.Status %in% c("Married", "Engaged", "In Relationship", "Widowed, Remarried"), 1, 0)
# Agrupar Divorced, Separated, Widowed bajo "PreviouslyMarried"
phil$previously_married <- ifelse(phil$Marital.Status %in% c("Divorced", "Separated", "Widowed"), 1, 0)
# Mantener "Single" como categoría de referencia (no se crea dummy para "Single")
phil$unknown_status <- ifelse(phil$Marital.Status == "Unknown", 1, 0)

phil_tb <- phil %>%
  select( finalWorth, age, selfMade, gdp_country,tax_revenue_country,total_tax_rate_country,population_country,female, Self.Made.Score, Bachelor, Master, Doctorate, Drop.Out, worth_pct_gdp, gdp_per_capita,worth_v_gdp_pc, Technology, Finance, Fashion_Retail,Manufacturing, Food_Beverage, Others,in_relationship_group,previously_married,unknown_status)

phil_tb <- phil_tb %>%
  rename("selfMade*" = selfMade,
         "female*" = female,
         "Bachelor*" = Bachelor,
         "Master*" = Master,
         "Doctorate*" = Doctorate,
         "Drop.Out*" = Drop.Out,
         "Technology*" = Technology,
         "Finance*" = Finance,
         "Fashion_Retail*" = Fashion_Retail,
         "Manufacturing*" = Manufacturing,
         "Food_Beverage*" = Food_Beverage,
         "Others*" = Others,
         "in_relationship_group*" = in_relationship_group,
         "previously_married*" = previously_married,
         "unknown_status*" = unknown_status)


# Generar la tabla descriptiva con una nota al pie
table_td <- stargazer(phil_tb,
                      title = "Estadísticas Descriptivas",
                      out = "EstDesc.tex",
                      notes = "Las variables con un asterisco (*) son dummies.")

# --[2.Distribución de worth_pct_gdp]---
graph2_2 <- ggplot(phil, aes(x = worth_pct_gdp)) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  labs(x = "worth_pct_gdp",
       y = "Densidad") +
  theme_minimal()

ggsave("graph2_2.pdf", plot = graph2_2, width = 10, height = 6, dpi = 300)

graph2_3 <- ggplot(phil, aes(x = log(worth_pct_gdp))) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  labs(x = "log(worth_pct_gdp)",
       y = "Densidad") +
  theme_minimal()

ggsave("graph2_3.pdf", plot = graph2_3, width = 10, height = 6, dpi = 300)

# --[3. Bootstrap]---
# Creamos un vector para guardar los resultados de las simulaciones del boot1 
sd_boot <- c() 
# Realizamos las simulaciones
for (n in 1:500){
  sampleboot <- sample(phil$worth_pct_gdp, # Extraemos una submuestra 
                       size = 205, # aleatoria con reemplazo
                       replace = T)
  sd_boot <-c(sd_boot, sd(sampleboot))
}

sd_boot <- data.frame(sd_boot)

graph2_4 <- ggplot(sd_boot, aes(x = sd_boot)) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) + 
  labs(x = "Estimaciones de sigma w ",
       y = "Densidad") +
  theme_minimal()
ggsave("graph2_4.pdf", plot = graph2_4, width = 10, height = 6, dpi = 300)

# --[4. Varianza e Intervalo de Confianza Bootstrap]---
# Calculamos la varianza sin ajuste porque el tamaño de la submuestra 
# es igual al tamaño de la muestra 
(var_sigma_w <- var(sd_boot$sd_boot))
(mean_sigma_w <- mean(sd_boot$sd_boot))


# Calculamos el intervalo de confianza con la varianza sin ajuste  
(cii_var <- quantile(sd_boot$sd_boot, 0.025))
(cis_var <- quantile(sd_boot$sd_boot, 0.975))

graph2_4_5 <- ggplot(sd_boot, aes(x = sd_boot)) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = cii_var, color = "coral", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cis_var, color = "coral", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_sigma_w, color = "blue", linetype = "solid", size = 1.2) + 
  labs(x = "Estimaciones de sigma w ",
       y = "Densidad") +
  theme_minimal()
ggsave("graph2_4_5.pdf", plot = graph2_4_5, width = 10, height = 6, dpi = 300)

# --[5.Puntos Extras Slim]---
## Patrimonio Neto Slim 
(finalWorth_Slim <- phil$finalWorth[phil$personName == "Carlos Slim Helu & family"])

#PIB México 
(gdp_mex <- phil$gdp_country[phil$personName == "Carlos Slim Helu & family"])

#Worth_pct_gdp Slim 
(worth_pct_gdp_Slim <- phil$worth_pct_gdp[phil$personName == "Carlos Slim Helu & family"])

## Con un patrimonio neto de 93 mil millones y 
## el PIB de México 1,258.287 mil millones el Patrimonio de Slim representa 7.391% 

# ====/ Análisis de riqueza y factores socioeconómicos \====
# --[1.Gráficas y estimaciones]---
## [a. edad y patrimonio neto distinguiendo por selfMade]
graph3_1 <- ggplot(phil, aes(x = age, y = log(finalWorth), color = as.factor(selfMade))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # Línea de regresión
  labs(x = "Edad",
       y = "Patrimonio Neto (FinalWorth)",
       color = "Self-made") +
  theme_minimal() +
  scale_color_manual(values = c("1" = "cornflowerblue", "0" = "coral"))

ggsave("graph3_1.pdf", plot = graph3_1, width = 10, height = 6, dpi = 300)

## [b. PIB del país de origen y patrimonio neto distinguiendo por selfMade]
graph3_2 <- ggplot(phil, aes(x = log(gdp_country), y = log(finalWorth), color = as.factor(selfMade))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # Línea de regresión
  labs(x = "Log PIB del País (GDP_country)",
       y = "Log Patrimonio Neto (FinalWorth)",
       color = "Self-made") +
  theme_minimal() +
  scale_color_manual(values = c("1" = "cornflowerblue", "0" = "coral"))

ggsave("graph3_2.pdf", plot = graph3_2, width = 10, height = 6, dpi = 300)

## [c. Estimaciones de (a) y (b)]
### Regresión con polinomio cuadrático para la edad
modelo_edad <- lm(log(finalWorth) ~ age + I(age^2), data = phil)
summary(modelo_edad)

# Regresión log-log para PIB y Patrimonio Neto
modelo_pib <- lm(log(finalWorth) ~ log(gdp_country), data = phil)
summary(modelo_pib)

# Graficar la regresión en la gráfica de edad vs patrimonio
graph3_2_5 <- ggplot(phil, aes(x = age, y = log(finalWorth), color = as.factor(selfMade))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +  # Polinomio cuadrático
  labs(x = "Edad",
       y = "Patrimonio Neto (log FinalWorth)",
       color = "Self-made") +
  theme_minimal() +
  scale_color_manual(values = c("1" = "cornflowerblue", "0" = "coral"))

ggsave("graph3_2_5.pdf", plot = graph3_2_5, width = 10, height = 6, dpi = 300)

# --[2.Errores homocedasticos vs Homoscedasticos]---

# --[3.Estimaciones cont.]---


cor(phil$Master, phil$Doctorate)

# a. $finalWorth$ vs age, selfMade, female, log(gdp_country), total_tax_rate_country, PhilanthropyScore, education, MaritalStatus 
reg_1 <- lm(finalWorth ~ age + selfMade + female + log(gdp_country) + total_tax_rate_country + 
              Philanthropy.Score + Master + Doctorate + Drop.Out + 
              in_relationship_group + previously_married + unknown_status, 
            data = phil)

# b. $\log(finalWorth)$ vs age, selfMade, female, log(gdp_country), total_tax_rate_country, PhilanthropyScore, education, MaritalStatus agrupado
reg_2 <- lm(log(finalWorth) ~ age + selfMade + female + log(gdp_country) + total_tax_rate_country + 
              Philanthropy.Score + Master + Doctorate + Drop.Out + 
              in_relationship_group + previously_married + unknown_status, 
            data = phil)

# c. $worth_v_gdp_pc$ vs age, selfMade, female, log(gdp_country), total_tax_rate_country, PhilanthropyScore, education, MaritalStatus agrupado
reg_3 <- lm(worth_v_gdp_pc ~ age + selfMade + female + log(gdp_country) + total_tax_rate_country + 
              Philanthropy.Score + Master + Doctorate + Drop.Out + 
              in_relationship_group + previously_married + unknown_status, 
            data = phil)

# d. $finalWorth_75$ vs age, selfMade, female, log(gdp_country), total_tax_rate_country, PhilanthropyScore, education, MaritalStatus agrupado
reg_4 <- lm(finalWorth_75 ~ age + selfMade + female + log(gdp_country) + total_tax_rate_country + 
              Philanthropy.Score + Master + Doctorate + Drop.Out + 
              in_relationship_group + previously_married + unknown_status, 
            data = phil)

# Errores heteroscedasticos
es_list <- lapply(list(reg_1, reg_2, reg_3, reg_4), function(x) sqrt(diag(vcovHC(x, type = "HC1"))))

stargazer(reg_1, reg_2, reg_3, reg_4,
          digits = 2,
          title = "Regresiones para finalWorth, log(finalWorth), worth_v_pct_gdp, finalWorth_75",
          label = "tab:finalWorth_regresiones",
          omit.stat = c("f", "adj.rsq", "ser"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          single.row = FALSE,
          no.space = TRUE,
          se = es_list,  # Incluir los errores estándar robustos
          out = "tabla_finalWorth_regresiones.tex")

# --[4.Interpretación de coeficientes]---
# --[5.Impacto industrias]---
# [a] Modelo extendido
reg_5 <- lm(log(finalWorth) ~ age + selfMade + female + log(gdp_country) + total_tax_rate_country + 
              Philanthropy.Score + Master + Doctorate + Drop.Out + 
              in_relationship_group + previously_married + unknown_status + 
              Technology + Finance + Fashion_Retail + Manufacturing + Food_Beverage, 
            data = phil) 

# Errores heteroscedasticos
es_list2 <- lapply(list(reg_1, reg_2, reg_3, reg_4, reg_5), function(x) sqrt(diag(vcovHC(x, type = "HC1"))))

stargazer(reg_1, reg_2, reg_3, reg_4,reg_5,
          digits = 2,
          title = "Regresiones para finalWorth, log(finalWorth), worth_v_pct_gdp, finalWorth_75",
          label = "tab:finalWorth_regresiones",
          omit.stat = c("f", "adj.rsq", "ser"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          single.row = FALSE,
          no.space = TRUE,
          se = es_list2,  # Incluir los errores estándar robustos
          out = "tabla_finalWorth_regresiones.tex")

# [b] Plantear hipótesis
# [c] Prueba de hipótesis 
names(coef(reg_5))
anova(reg_2, reg_5)

# Realizar la prueba conjunta de hipótesis para las dummies de industria
resultado_f <- linearHypothesis(reg_5, 
                                c("Technology = 0", "Finance = 0", "Fashion_Retail = 0", 
                                  "Manufacturing = 0", "Food_Beverage = 0"),
                                vcov = vcovHC(reg_5, type = "HC1"))  # Errores robustos
# Mostrar los resultados
print(resultado_f)

# [d] Comentario 
# [e] Cambios en coeficiente female con y sin industria comentario 

# --[6.Interacción Female e Industrias]---
#[a] Interacción Female e Industrias
women_in_manufacturing <- subset(phil, female == 1 & Manufacturing == 1)
nrow(women_in_manufacturing)

reg_female_industry <- lm(log(finalWorth) ~ age + selfMade + female + log(gdp_country) + total_tax_rate_country + 
                            Philanthropy.Score + Master + Doctorate + Drop.Out + 
                            in_relationship_group + previously_married + unknown_status +
                            Technology + Finance + Fashion_Retail + Manufacturing + Food_Beverage + 
                            female:Technology + female:Finance + female:Fashion_Retail + 
                            female:Manufacturing + female:Food_Beverage, 
                          data = phil)

# Resumen de la regresión
summary(reg_female_industry)

#[b] Food & Bevarge
# Obtener los intervalos de confianza al 90% para los coeficientes de las interacciones
ci_90 <- confint(reg_female_industry, level = 0.90)

# Filtrar solo los coeficientes de interés (interacciones con female)
ci_female_industries <- ci_90[c("female:Technology", "female:Finance", "female:Fashion_Retail", 
                                "female:Manufacturing", "female:Food_Beverage"), ]

# Reportar los valores exactos de la industria Food and Beverage
ci_female_food_beverage <- ci_female_industries["female:Food_Beverage", ]
ci_female_food_beverage

# Crear un dataframe para las gráficas con los coeficientes y los intervalos
coef_female_industries <- coef(reg_female_industry)[c("female:Technology", "female:Finance", 
                                                      "female:Fashion_Retail", "female:Manufacturing", 
                                                      "female:Food_Beverage")]

# Combinar coeficientes e intervalos de confianza
industries_df <- data.frame(
  Industry = c("Technology", "Finance", "Fashion & Retail", "Manufacturing", "Food & Beverage"),
  Coefficient = coef_female_industries,
  Lower = ci_female_industries[, 1],
  Upper = ci_female_industries[, 2]
)

# Graficar los intervalos de confianza al 90%
graph_6 <- ggplot(industries_df, aes(x = Industry, y = Coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  labs(y = "Coeficiente de la Interacción (Female x Industria)", 
       x = "Industria") +
  theme_minimal()

ggsave("graph_6.pdf", plot = graph_6, width = 10, height = 6, dpi = 300)

