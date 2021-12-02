# EME ---------------------------------------------------------------------
## Codigo por V. Andrade

# 1.Cargar librerias ------------------------------------------------------
pacman::p_load(tidyverse, sjmisc, #manipulacion
               srvyr, survey) # Muestras

# 2.Cargar bases de datos -------------------------------------------------
## Se utilio EME-2018-personas (https://www.ine.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-suplementaria-de-ingresos)

data <- haven::read_dta("input/data/Base de datos Full VI EME.dta")

### Puedes cargarla desde csv, spss o dta.

# 3. Explorar ----------------------------------------------------------
sjmisc::find_var(esi,"fa")
## Hay dos factores uno trimestral y otro de personas. Ocuparemos de personas (fact_cal_esi)
# Ocuparemos sexo, ing_t_p, cine (nivel educacional)

# 4. Manipular ------------------------------------------------------------str(esi$sexo)
esi <- esi %>%
  mutate(sexo = as_factor(sexo),
         cine = as_factor(cine))

# 5.Expandir --------------------------------------------------------------

# 5.1.Expandir
## En base a metodologia EME se identifica un diseno muestral complejo (bietapidoc por conglomerados)
exp <- esi %>%
  as_survey_design(ids = 1, weights = fact_cal_esi)
### Nuevo objeto exp

## 5.2 Definir diseño muestral
options(survey.lonely.psu = "certainty" )


# 6. Tablas  -------------------------------------------------------------
# 6.1 Sexo
exp  %>% 
  summarise(n=survey_total(sexo,vartype = "ci",na.rm = TRUE))
## Mira la consola, te aparece el resultado. 

# 6.2 Ingresos por sexo 
exp %>% 
  group_by(sexo) %>% 
  summarise(n=survey_mean(ing_t_p,vartype = "ci",na.rm = TRUE))
##Recuerda que un warning no es un error. 

# 6.3 Ingresos por nivel educacional 
exp %>% 
  group_by(cine) %>% 
  summarise(n=survey_mean(ing_t_p,vartype = "ci",na.rm = TRUE))


# 7. Exportar -------------------------------------------------------------

ing_sex <- exp %>% 
  group_by(sexo) %>% 
  summarise(n=survey_mean(ing_t_p,vartype = "ci",na.rm = TRUE))

## Si despues quieres exportar esas tablas las creas como objeto y con write.xlsx las llevas a excel
## Tomas ing_sex


