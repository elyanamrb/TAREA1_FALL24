# Tarea 1 | Econometria Otoño 2024

# Creado por Elyana Ramos
# Revisado y editado por Arturo Aguilar

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

# Importamos la base 
phil <- read.csv("Philanthropy_2023.csv")
colnames(phil)

# ====/ 1. Procesamiento inicial de datos \====
## Variable worth_pct_gdp 
### Importante considerar que finalWorth se mide en millones de dólares 
### y GDP en mil millones
phil <- phil %>%
  mutate(worth_pct_gdp = 100 * (finalWorth / (gdp_country*1000)),
         gdp_per_capita = gdp_country*1000/population_country,
         worth_v_gdp_pc = finalWorth*1000000 / gdp_per_capita,
         finalWorth_75 = ifelse(finalWorth > quantile(finalWorth, 0.75, na.rm = TRUE), 1, 0),
         Technology = ifelse(industries == "Technology", 1, 0),
         Finance = ifelse(industries == "Finance & Investments", 1, 0),
         Fashion_Retail = ifelse(industries == "Fashion & Retail", 1, 0),
         Manufacturing = ifelse(industries == "Manufacturing", 1, 0),
         Food_Beverage = ifelse(industries == "Food & Beverage", 1, 0),
         Others = 1-Technology-Finance-Fashion_Retail-Manufacturing-Food_Beverage,
         hi_phil = (Philanthropy.Score)>1,
         in_relationship_group = ifelse(Marital.Status %in% c("Married", "Engaged", "In Relationship", "Widowed, Remarried"), 1, 0),
         previously_married = ifelse(Marital.Status %in% c("Divorced", "Separated", "Widowed"), 1, 0),
         unknown_status = ifelse(Marital.Status == "Unknown", 1, 0),
         martial_other= 1-in_relationship_group-previously_married-unknown_status )


phil_tb <- phil %>%
  select(-personName,-rank,-country,-industries,-gross_tertiary_education_enrollment,
         -Philanthropy.Score)
#  select( finalWorth, age, selfMade, gdp_country,tax_revenue_country,total_tax_rate_country,population_country,female, Self.Made.Score, Bachelor, Master, Doctorate, Drop.Out, worth_pct_gdp, gdp_per_capita,worth_v_gdp_pc, Technology, Finance, Fashion_Retail,Manufacturing, Food_Beverage, Others,in_relationship_group,previously_married,unknown_status)

phil_tb <- phil_tb %>%
  rename("selfMade*" = selfMade,
         "female*" = female,
         "hi_phil*" = hi_phil,
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
         "unknown_status*" = unknown_status,
         "martial_other*" = martial_other)

# ====/ 2. Analisis descriptivo \====

# [1. Generar la tabla descriptiva con una nota al pie]
table_td <- stargazer(phil_tb,
                      title = "Estadísticas Descriptivas",
                      out = "EstDesc.tex",
                      notes = "Las variables con un asterisco (*) son dummies.")

# --[2.Distribución de worth_pct_gdp]---
ggplot(phil, aes(x = worth_pct_gdp)) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  labs(x = "worth_pct_gdp",
       y = "Densidad") +
  theme_minimal()

ggsave("graph2_2.pdf", width = 10, height = 6, dpi = 300)

ggplot(phil, aes(x = log(worth_pct_gdp))) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  labs(x = "log(worth_pct_gdp)",
       y = "Densidad") +
  theme_minimal()

ggsave("graph2_3.pdf", width = 10, height = 6, dpi = 300)

# --[3. Bootstrap]---
# Creamos un vector para guardar los resultados de las simulaciones del boot1 
sd_boot <- c() 
# Realizamos las simulaciones
N_boot <- length(phil$worth_pct_gdp)
for (n in 1:500){
  sampleboot <- sample(phil$worth_pct_gdp, # Extraemos una submuestra 
                       size = N_boot, # aleatoria con reemplazo
                       replace = T)
  sd_boot <-c(sd_boot, sd(sampleboot))
}

sd_boot <- data.frame(sd_boot)

ggplot(sd_boot, aes(x = sd_boot)) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) + 
  labs(x = "Estimaciones de sigma w ",
       y = "Densidad") +
  theme_minimal()
ggsave("graph2_4.pdf", width = 10, height = 6, dpi = 300)

# --[4. Varianza e Intervalo de Confianza Bootstrap]---
# Calculamos la varianza sin ajuste porque el tamaño de la submuestra 
# es igual al tamaño de la muestra 
(var_sigma_w <- var(sd_boot$sd_boot))
(mean_sigma_w <- mean(sd_boot$sd_boot))


# Calculamos el intervalo de confianza con la varianza sin ajuste  
(cii_var <- quantile(sd_boot$sd_boot, 0.025))
(cis_var <- quantile(sd_boot$sd_boot, 0.975))

ggplot(sd_boot, aes(x = sd_boot)) +
  geom_histogram(fill = "cornflowerblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = cii_var, color = "coral", linetype = "dashed", size = 1) +
  geom_vline(xintercept = cis_var, color = "coral", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_sigma_w, color = "blue", linetype = "solid", size = 1.2) + 
  labs(x = "Estimaciones de sigma w ",
       y = "Densidad") +
  theme_minimal()
ggsave("graph2_4_5.pdf", width = 10, height = 6, dpi = 300)

# --[5.Puntos Extras Slim]---
#Worth_pct_gdp Slim 
(worth_pct_gdp_Slim <- phil$worth_pct_gdp[phil$personName == "Carlos Slim Helu & family"])

# ====/ 3. Análisis de riqueza y factores socioeconómicos \====
# --[1.Gráficas y estimaciones]---

phil <- phil %>% mutate(C_USA = ifelse(country == "United States", 1, 0),
                        C_China = ifelse(country == "China", 1, 0))


## [a. edad y patrimonio neto distinguiendo por selfMade]
ggplot(phil, aes(x = age, y = log(finalWorth), color = as.factor(selfMade))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +  # Polinomio cuadrático
  labs(x = "Edad",
       y = "Patrimonio Neto (log FinalWorth)",
       color = "Self-made") +
  theme_minimal() +
  scale_color_manual(values = c("1" = "cornflowerblue", "0" = "coral"))

ggsave("graph3_1.pdf", width = 10, height = 6, dpi = 300)

## [b. PIB del país de origen y patrimonio neto distinguiendo por selfMade]
ggplot(phil, aes(x = log(gdp_country), y = log(finalWorth), color = as.factor(selfMade))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # Línea de regresión
  labs(x = "Log PIB del País (GDP_country)",
       y = "Log Patrimonio Neto (FinalWorth)",
       color = "Self-made") +
  theme_minimal() +
  scale_color_manual(values = c("1" = "cornflowerblue", "0" = "coral"))

ggsave("graph3_2.pdf", width = 10, height = 6, dpi = 300)

ggplot(phil%>% filter(C_USA==0 & C_China==0), aes(x = age, y = log(finalWorth), color = as.factor(selfMade))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE) +  # Polinomio cuadrático
  labs(x = "Edad",
       y = "Patrimonio Neto (log FinalWorth)",
       color = "Self-made") +
  theme_minimal() +
  scale_color_manual(values = c("1" = "cornflowerblue", "0" = "coral"))

ggsave("graph3_2B.pdf",width = 10, height = 6, dpi = 300)

## [c. Estimaciones de (a) y (b)]
### Regresión con polinomio cuadrático para la edad
modelo_edad <- lm(log(finalWorth) ~ age + I(age^2), data = phil)
summary(modelo_edad)
(theta_bar <- modelo_edad$coefficients['age']+
              2*modelo_edad$coefficients['I(age^2)']*mean(phil$age))

# Regresión log-log para PIB y Patrimonio Neto
modelo_pib <- lm(log(finalWorth) ~ C_USA + C_China + log(gdp_country), data = phil)
summary(modelo_pib)

# --[3.Estimaciones cont.]---

# Analisis previo para decidir controles categoricos
table(phil$Philanthropy.Score)

# a. $finalWorth$ vs age, selfMade, female, log(gdp_country), total_tax_rate_country, PhilanthropyScore, education, MaritalStatus 
reg_1 <- lm(finalWorth ~ age + selfMade + female + log(gdp_country) + total_tax_rate_country + 
              hi_phil + Bachelor + Master + Doctorate + 
              in_relationship_group + previously_married + unknown_status, 
            data = phil)

# b. $\log(finalWorth)$ vs age, selfMade, female, log(gdp_country), total_tax_rate_country, PhilanthropyScore, education, MaritalStatus agrupado
reg_2 <- lm(log(finalWorth) ~ age + selfMade + female + log(gdp_country) + total_tax_rate_country + 
              hi_phil + Bachelor + Master + Doctorate +  
              in_relationship_group + previously_married + unknown_status, 
            data = phil)

# c. $worth_v_gdp_pc$ vs age, selfMade, female, log(gdp_country), total_tax_rate_country, PhilanthropyScore, education, MaritalStatus agrupado
reg_3 <- lm(worth_v_gdp_pc ~ age + selfMade + female + log(gdp_country) + total_tax_rate_country + 
              hi_phil + Bachelor + Master + Doctorate +  
              in_relationship_group + previously_married + unknown_status, 
            data = phil)

# d. $finalWorth_75$ vs age, selfMade, female, log(gdp_country), total_tax_rate_country, PhilanthropyScore, education, MaritalStatus agrupado
reg_4 <- lm(finalWorth_75 ~ age + selfMade + female + log(gdp_country) + total_tax_rate_country + 
              hi_phil + Bachelor + Master + Doctorate +  
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

# --[5.Impacto industrias]---
# [a] Modelo extendido
reg_5 <- lm(log(finalWorth) ~ age + selfMade + female + log(gdp_country) + total_tax_rate_country + 
              hi_phil + Bachelor + Master + Doctorate +  
              in_relationship_group + previously_married + unknown_status +
              Technology + Finance + Fashion_Retail + Manufacturing + Food_Beverage, 
              data = phil)
summ(reg_5,robust = "HC1",digits=4)

# [b] Plantear hipótesis
# [c] Prueba de hipótesis 
linearHypothesis(reg_5, 
                 c("Technology = 0", "Finance = 0", "Fashion_Retail = 0", 
                   "Manufacturing = 0", "Food_Beverage = 0"),
                 white.adjust="hc1")  

# --[6.Interacción Female e Industrias]---
#[a] Interacción Female e Industrias
women_in_manufacturing <- subset(phil, female == 1 & Manufacturing == 1)
nrow(women_in_manufacturing)

# Crear las interacciones
phil <- phil %>% mutate(fem_other = female * Others,
                        fem_tech = female * Technology,
                        fem_fin = female * Finance,
                        fem_fashion = female * Fashion_Retail,
                        fem_manuf = female * Manufacturing,
                        fem_food = female * Food_Beverage)

<<<<<<< HEAD
reg_female_industry <- lm_robust(log(finalWorth) ~ age + selfMade + log(gdp_country) + total_tax_rate_country + 
=======
reg_female_industry <- lm(log(finalWorth) ~ age + selfMade + log(gdp_country) + total_tax_rate_country + 
>>>>>>> 132e4435c53d205bbdf2e2b7241ccd83f4e75915
                            hi_phil + Bachelor + Master + Doctorate +  
                            in_relationship_group + previously_married + unknown_status +
                            Technology + Finance + Fashion_Retail + Manufacturing + Food_Beverage + 
                            fem_other + fem_tech + fem_fin + fem_fashion +
                            fem_manuf + fem_food,
                          data = phil)

#[b] Food & Bevarge
# Obtener los intervalos de confianza al 90% para los coeficientes de female y las interacciones
ci_90 <- confint(reg_female_industry, level = 0.90)

# Filtrar solo los coeficientes de interés (interacciones con female)
ci_female_industries <- ci_90[c("fem_other", "fem_tech", "fem_fin", 
                                "fem_fashion", "fem_food"),]

# Reportar los valores exactos de la industria Food and Beverage
ci_female_industries["fem_food", ]

# Crear un dataframe para las gráficas con los coeficientes y los intervalos
coef_female_industries <- coef(reg_female_industry)[c("fem_other", "fem_tech", "fem_fin", 
                                                      "fem_fashion", "fem_food")]

# Combinar coeficientes e intervalos de confianza
industries_df <- data.frame(
  Industry = c("Other", "Technology", "Finance", "Fashion & Retail", "Food & Beverage"),
  Coefficient = coef_female_industries,
  Lower = ci_female_industries[, 1],
  Upper = ci_female_industries[, 2]
)

# Graficar los intervalos de confianza al 90%
ggplot(industries_df, aes(x = Industry, y = Coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  labs(y = "Coeficiente de la Interacción (Female x Industria)", 
       x = "Industria") +
  theme_minimal()

ggsave("graph_6.pdf", width = 10, height = 6, dpi = 300)

