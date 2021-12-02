# EME ---------------------------------------------------------------------
## Codigo por V. Andrade

# 1.Cargar librerias ------------------------------------------------------
pacman::p_load(tidyverse, sjmisc, #manipulacion
               srvyr, survey) # Muestras

# 2.Cargar bases de datos -------------------------------------------------
## Se utilio EME-2018-personas (https://www.ine.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-suplementaria-de-ingresos)

data <- haven::read_dta("input/data/Base de datos Full VI EME (extracto).dta")

### Puedes cargarla desde csv, spss o dta.

# 3. Explorar ----------------------------------------------------------
sjmisc::find_var(esi,"Factor")

# •	Enc_rph: Identificador único de personas
# •	ganancia_final_mensual: Ganancias mensual de los microemprendedores/as
# •	conta_completa: Registro contable (1 Sí // 2 No)
# •	registro_SII: Registro en impuestos internos (1 Sí // 2 No)
# •	CISE: Clasificación Internacional de la situación en el empleo (0 Cuenta Propia // 1 Empleador)
# •	region: región de residencia de la persona (codificadas de 1 a 16 https://www.gob.cl/nuestro-pais/)
# •	Factor_EME: Factor de expansión de personas

# 4. Manipular ------------------------------------------------------------

# 4.1 Seleccionar variables -----------------------------------------------
# Explorar
data %>% 
  select(Enc_rph,ganancia_final_mensual, conta_completa, registro_SII, CISE, region,Factor_EME)


# 4.2 Recodificar ---------------------------------------------------------
sjmisc::frq(data$region)
# Personas microemprendedoras
sjmisc::frq(data$a1)

# Recodificar regiones
data_proc <- data %>% mutate(macrozona = case_when(region %in% c(15, 1, 2, 3) ~ "Norte",
                                           region %in% c(4, 5, 6, 7, 16, 8) ~ "Centro",
                                           region %in% c(9, 14, 10) ~ "Sur",
                                           region %in% c(11, 12) ~ "Austral",
                                           TRUE ~ NA_character_),
                                  micro = case_when(a1 %in% c(1,2) ~ "Microemprendedor",
                                                                 TRUE ~ NA_character_),
                              micro_informal = if_else(registro_SII == 2 & conta_completa == 2, "Informal","Formal")) %>% 
  mutate_at(vars(macrozona, micro, micro_informal, CISE), funs(forcats::as_factor(.))) %>% 
  mutate_at(vars(macrozona, micro, micro_informal, CISE), funs(as.character(.)))


# 5.Expandir --------------------------------------------------------------
# 5.1.Expandir
## En base a metodologia EME se identifica un diseno muestral complejo (bietapidoc por conglomerados)
exp <- data_proc %>%
  as_survey_design(ids = Enc_rph, weights = Factor_EME)
### Nuevo objeto exp

## 5.2 Definir diseño muestral
options(survey.lonely.psu = "certainty" )


# 6. Tablas  -------------------------------------------------------------
# Tasa informalidad (con factores)
# Ganancia promedio mensual empleadores (ganancia final mensual grupo 1)
# Ganancia promedio mensual de trabajadores cuenta propia (ganancia final mensual 0)

# 6.1 Numero de personas microemprendedoras por zona
exp  %>%
  filter(micro == "Microemprendedor") %>% 
  group_by(macrozona) %>% 
  summarise(n=survey_total(vartype = "ci",na.rm = TRUE))

# 6.1 Numero de personas microemprendedoras por zona
exp  %>%
  filter(micro == "Microemprendedor", !is.na(macrozona)) %>% 
  group_by(macrozona, micro_informal) %>%
  summarise(n=survey_total(vartype = "ci",na.rm = TRUE)) %>%
  mutate(tasa = n /2057903) %>%  # Total de microemprendedores
  filter(micro_informal == "Informal") %>% 
  select(macrozona, n, tasa)
# 6.3 Ganancia promedio mensual empleadores y cuenta propia
exp %>% 
  group_by(CISE, macrozona) %>% 
  summarise(ganancia_promedio=survey_mean(ganancia_final_mensual,vartype = "ci",na.rm = TRUE))

# Datos empleo ------------------------------------------------------------
## cada fila representa la información de un trabajador, pudiendo un informante tener información de sus trabajadores en varias filas
# 2.Cargar bases de datos -------------------------------------------------
data2 <- haven::read_dta("input/data/Base de datos empleo VI EME.dta")

# Explorar ----------------------------------------------------------------
sjPlot::view_df(data2)


# Filtrar sin datos o no responde (se dejan no sabe) -----------------------------------------------------------------------
data_proc2 <- data2 %>% filter(!f2_b %in% c(96,99), !f2_c %in% c(96,99), !f2_d %in% c(999), !f2_e %in% c(96,99), !f2_f %in% c(96,99),
                               !f2_g %in% c(96,99), !f2_i %in% c(996,999))

# 3. Generar variables ----------------------------------------------------

# 3.1 Numero de trabajadores totales --------------------------------------
#
data_proc2 <- data_proc2 %>% 
  group_by(Enc_rph) %>% 
  mutate(f2_totales = n(),
         sex = forcats::as_factor(f2_c),
         asalariado = forcats::as_factor(f2_f)) %>% 
  group_by(Enc_rph,sex) %>%
  mutate(f2_ = n()) %>%
  pivot_wider(names_from = c(sex), values_from = c(f2_)) %>% 
  ungroup() %>% 
  group_by(Enc_rph, asalariado) %>% 
  mutate(f2_ = n(),
         gasto_remun_asal = mean(f2_h, na.omit = T)) %>% 
  pivot_wider(names_from = c(asalariado), values_from = c(f2_)) %>% 
  select(1,f2_mujeres = Mujer, f2_hombres = Hombre, f2_asalariados = 16, gasto_remun_asal) %>% 
  ungroup()


# Fusionar ----------------------------------------------------------------
#Fusion completa
data_final <- merge(data, data_proc2, all = T, by = "Enc_rph")  
#Fusion como ENE de referencia dado que es mejor registro
data_final <- right_join(data, data_proc2, by = "Enc_rph")  

### Son distintas pues no todos los informantes contestan



# 3. Validaciones ---------------------------------------------------------

validacion <- 
  data_final %>% mutate(error_1=if_else(is.na(a2_otro),1,0), #Validacion de falta de respuesta
                        error_2=if_else((a3==1|a3==2)&(a1<3),1,0), # No respuesta
                        error_3=if_else(a4>4 & a4 < 77,1,0)) # Fuera de rango
