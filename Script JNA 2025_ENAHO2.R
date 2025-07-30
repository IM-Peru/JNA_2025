###############################################
############# Indicadores JNA 2025 ############
###############################################

# Este script tiene como objetivo calcular diversos indicadores para el año 2025
# a partir de múltiples bases de datos en formato .sav (SPSS).
# Los indicadores se agrupan en categorías como Integración, Educación, Salud, WASH,
# Alojamiento, y Protección, y se exportan a un archivo Excel.

# -----------------------------------------------------------------------------
# 1. Carga de librerías necesarias
#    Estas líneas cargan los paquetes de R que proporcionan las funciones
#    que usaremos a lo largo del script. Si no los tienes instalados,
#    deberías hacerlo con install.packages("nombre_del_paquete").
# -----------------------------------------------------------------------------
install.packages("srvyr")

library(srvyr)     # Para trabajar con datos de encuestas con sintaxis tidyverse
library(survey)    # Librería fundamental para análisis de encuestas complejos
library(haven)     # Para importar y exportar archivos SPSS (.sav), Stata, SAS
library(tidyverse) # Colección de paquetes que incluye dplyr (manipulación de datos),
                   # readr (lectura de datos), ggplot2 (gráficos), etc.
                   # Es CRÍTICA para funciones como 'left_join', 'mutate', 'filter', etc.
library(writexl)   # Para exportar data frames de R a archivos Excel (.xlsx)

# -----------------------------------------------------------------------------
# 2. Carga de las bases de datos .sav
#    Estas líneas leen los archivos .sav desde la ruta especificada.
#    Hemos actualizado la ruta a "C:/Users/edusilva/Downloads/1. BASE DE DATOS/"
#    para evitar conflictos con OneDrive, que fue la causa del error "does not exist".
# -----------------------------------------------------------------------------
enaho_100 <- read_sav("EnahoPV01-2024-100.sav")
enaho_200 <- read_sav("EnahoPV01-2024-200.sav")
enaho_300 <- read_sav("EnahoPV01A-2024-300.sav")
enaho_400 <- read_sav("EnahoPV01A-2024-400.sav")
enaho_500 <- read_sav("EnahoPV01A-2024-500.sav")
enaho_1000 <- read_sav("EnahoPV01A-2024-1000.sav")

# -----------------------------------------------------------------------------
# 3. Procesamiento del Capítulo 100 (Shelter y WASH)
#    Se realizan transformaciones y creación de variables para indicadores
#    relacionados con alojamiento y agua, saneamiento e higiene.
# -----------------------------------------------------------------------------

# Se completa los valores para los hogares secundarios (rellena NA con valores anteriores)
# y se crea un identificador único de hogar (hh_id).
enaho_100 <- enaho_100 |>
  filter(RESULT %in% c(1,2))|> # Filtra por un resultado específico de la encuesta
  tidyr::fill(everything(), .direction = "down") |> # Rellena NA's hacia abajo
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) # Crea ID de hogar

# Calculamos la variable total de miembros del hogar usando enaho_200
enaho_200_members <- enaho_200 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |> # Crea ID de hogar
  group_by(hh_id) |> # Agrupa por ID de hogar
  summarise(hh_members = n()) # Cuenta el número de miembros por hogar

# Une (left_join) los datos del número de miembros del hogar a enaho_100
enaho_100 <- left_join(enaho_100, enaho_200_members, by = "hh_id")

# Creación de variables SHE (Shelter/Alojamiento) y WA (WASH/Agua, Saneamiento, Higiene)
enaho_100 <- enaho_100 |>
  mutate(
    SHE_D1_Q1 = ifelse(P101 %in% c(5:8), 1,0), # Pregunta 1 de Alojamiento  // P101 Tipo de vivienda -> Debería ser SHE_D1_Q1a
    SHE_D1_Q3 = ifelse((P102 %in% c(3:9) | P103 == 6 | P103A %in% c(5:8)), 1, 0), # Pregunta 3 // P102 Material en paredes P103 Material de pisos P103A Material en techos  -> Debería ser SHE_D1_Q1a
    SHE_D1_Q4 = ifelse((P1121 == 0 | (P1132 == 0 & P1133 == 0) | P111A %in% c(3:9) | P110 %in% c(3:8)), 1, 0) # Pregunta 4 // P110 Agua en el hogar P111A Baño o servicios P1132 Combustible para cocinar P1133 Combustible P1121 Tipo de alumbrado  -> Debería ser SHE_D1_Q3
  ) |>
  mutate(SHE_D1 = ifelse(SHE_D1_Q1 == 1 | SHE_D1_Q3 == 1 | SHE_D1_Q4 == 1, 1, 0)) |> # Indicador SHE_D1
  mutate(SHE_D2 = ifelse(hh_members/P104A > 3, 1, 0)) |> # Indicador SHE_D2 (hacinamiento) // P104A Habitaciones para dormir
  mutate(
    WA_D1 = ifelse(P110 %in% c(3:8), 1, 0), # Indicador WA_D1 // P110 Agua en el hogar
    WA_D2 = ifelse(!(P110 == 1 & P110C == 1 & P110C1 == 24), 1, 0), # Indicador WA_D2 // P110 Agua en el hogar P110C Días que accede P110C1 Horas al día
    WA_D3 = ifelse(P110A1 == 2, 1, 0), # Indicador WA_D3 // P110A1 Agua potable
    WA_D4 = ifelse(P111A %in% c(2:9), 1, 0) # Indicador WA_D4 // P111A Conexión de baños
  )

# -----------------------------------------------------------------------------
# 4. Procesamiento del Capítulo 200 (Características de los Miembros del Hogar)
#    Creación de identificadores de miembro de hogar y variables de población.
# -----------------------------------------------------------------------------
enaho_200 <- enaho_200 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |> # Crea ID de hogar
  mutate(hh_member_id = paste0(hh_id, CODPERSO)) |> # Crea ID único para cada miembro
  mutate(
    resid = case_when(
      P204 == 1 & P205 == 2  ~ 1, # Define si es residente o no según preguntas P204 Miembro del hogar y P205 Ausente del hogar 30+/P206 Presente 30+
      P204 == 2 & P206 == 1  ~ 1,
      TRUE ~ 2
    ),
    pobinic = ifelse(P204 == 1 & !P203 %in% c(8:9) & P208A >= 3 & P208A <= 5, 1, 0), # Población inicial (edad) // P204 Miembro del hogar P203 Relación de parentesco P208A Edad 
    pobprim = ifelse(P204 == 1 & !P203 %in% c(8:9) & P208A >= 6 & P208A <= 11, 1, 0), # Población primaria (edad)
    pobsec = ifelse(P204 == 1 & !P203 %in% c(8:9) & P208A >= 12 & P208A <= 16, 1 , 0) # Población secundaria (edad)
  ) |>
  select(hh_id, hh_member_id, P203, P203B, P204, P205, P206, P207, P208A, P209A, P212, P213, P214, resid, pobinic, pobprim, pobsec, FACTOR_POBLACIONAL_CAP200) # Selecciona columnas relevantes

# -----------------------------------------------------------------------------
# 5. Procesamiento del Capítulo 300 (Educación)
#    Creación de variables relacionadas con la matrícula y asistencia escolar.
# -----------------------------------------------------------------------------
enaho_300 <- enaho_300 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |> # Crea ID de hogar
  mutate(hh_member_id = paste0(hh_id, CODPERSO)) |> # Crea ID de miembro
  mutate(
    matricula_inic = ifelse(P306 == 1 & P308A == 1, 1, 0), # Matrícula inicial   // P306 Matriculado o no P308A Año o grado
    matricula_prim = ifelse(P306 == 1 & (P308A %in% c(2, 7)), 1, 0), # Matrícula primaria // CHECK inclusión de Básica especial
    matricula_secun = ifelse(P306 == 1 & P308A == 3, 1, 0), # Matrícula secundaria
    inic = ifelse(P306 == 1 & P307 == 1 & P308A == 1, 1, 0), # Asistencia inicial // P307 Asistencia
    prim = ifelse(P306 == 1 & P307 == 1 & (P308A %in% c(2, 7)), 1, 0), # Asistencia primaria
    secun = ifelse(P306 == 1 & P307 == 1 & P308A == 3, 1 , 0) # Asistencia secundaria
  ) |>
  select(hh_id, hh_member_id, matricula_inic, matricula_prim, matricula_secun, inic, prim, secun) # Selecciona columnas relevantes

# -----------------------------------------------------------------------------
# 6. Procesamiento del Capítulo 400 (Salud)
#    Creación de variables relacionadas con el acceso a servicios de salud.
# -----------------------------------------------------------------------------
enaho_400 <- enaho_400 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |> # Crea ID de hogar
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |> # Crea ID de miembro
  mutate(HE_D1 = ifelse(P4191 == 2 & P4192 == 2 & P4193 == 2 & P4194 == 2 & P4195 == 2 & P4196 == 2 & P4197 == 2 & P4198 == 2, 1 ,0)) |> # Indicador HE_D1 (acceso a cobertura) // P4191 ESSALUD P4192 Seguro privado P4193 EPRES P4194 FFAA/PNP P4195 SIS P4196 Seguro universitario P4197 Seguro escolar P4198 Otro
  mutate(P403 = P4031 + P4032 + P4033 + P4034 + P4035 + P4036 + P4037 + P4038 + P4039 + P40310  + P40311  + P40313 ) |> # Suma de preguntas P403 Acudir a un centro (MINSA, CLAS, ESSALUD PNP/FFAA, particular)
  mutate(HE_D2 = if_else(P403 == 0, 1, 0, missing = 0)) |> # Indicador HE_D2 (atención requerida no accedida)
  select(hh_member_id, HE_D1, HE_D2) # Selecciona columnas relevantes


# -----------------------------------------------------------------------------
# 7. Procesamiento del Capítulo 1000 (Protección)
#    Creación de variables relacionadas con seguridad y discriminación.
# -----------------------------------------------------------------------------
enaho_1000 <- enaho_1000 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |> # Crea ID de hogar
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |> # Crea ID de miembro
  mutate(
    PRO_D4 = ifelse(P1002 %in% c(6:9), 1, 0), # Indicador PRO_D4 // P1002 Tipo de permiso migratorio
    PRO_D5 = ifelse(P1007 == 1 & (P1008_1 == 1 | P1008_2 == 2 | P1008_3 == 3 | P1008_4 == 4 | P1008_12 == 12), 1, 0), # Indicador PRO_D5 // P1007 Preocupación de retorno 
    INT_D4 = ifelse(P1009 == 1 & P1010_5 == 5, 1, 0), # Indicador INT_D4 (discriminación por nacionalidad) // P1009 discriminado P1010_5 motivo (nacionalidad)
    GBV_D3 = ifelse(P1012 %in% c(1, 6), 1, 0), # Indicador GBV_D3 (percepción de inseguridad/violencia de género) // P1012 Seguridad
    GBV_fake = ifelse(P1013_1 == 1 | P1013_2 == 1 | P1013_3 == 1 | P1013_4 == 1 | P1013_5 == 1 | P1013_6 == 1 | P1013_7 == 1, 1, 0) # Indicador GBV_fake // P1013 _ violencia de género
  ) |>
  select(hh_member_id, PRO_D4, PRO_D5, INT_D4, GBV_D3, GBV_fake) # Selecciona columnas relevantes


# -----------------------------------------------------------------------------
# 8. Procesamiento del Capítulo 500 (Integración y Mercado Laboral)
#    Creación de variables para PEA, contratos, pensiones e inclusión financiera.
# -----------------------------------------------------------------------------
enaho_500 <- enaho_500 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |> # Crea ID de hogar
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) # Eliminado: select(hh_id, hh_member_id, CODINFOR, P501, P502, P503)

# Se comentan los objetivos de los indicadores INT
# INT_D1: 1 - PEA Ocupada
# INT_D2: (No definido en el script)
# INT_D3: Contrato laboral y pension
# INT_D5: Inclusión financiera


# Cálculos para PEA Ocupada
pea_ocupada <- enaho_500 |>
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |>
  select(P501, P502, P503, P5041:P50411, P545, FACTOR_POBLACIONAL_CAP500, hh_member_id) |>
  left_join(enaho_200, by = "hh_member_id") |> # Une con datos del capítulo 200
  filter(P209A == 2) |> # Filtra por nacionalidad Venezolana 
  filter(P208A >= 14) |> # Filtra por edad (14 años o más) 
  mutate(
    P504_rec = if_else(if_any(P5041:P50411, ~ . == 1), 1, 0), # Recodifica P504 Actividad para ingresos
    pea = case_when( # Define si es parte de la Población Económicamente Activa (PEA)
      P501 == 1 | P502 == 1 | P503 == 1 | P504_rec == 1 | P545 == 1 ~ "pea", #// P501 trabajo P502 Empleo fijo P503 Negocio propio P545 Labor artesanal
      TRUE ~ "no pea"
    ),
    ocupada = case_when( # Define si está ocupada
      P501 == 1 | P502 == 1 | P503 == 1 | P504_rec == 1 ~ "ocupada",
      TRUE ~ "no ocupada"
    ),
    pea_ocupada = case_when( # Combina PEA y ocupación
      pea == "pea" & ocupada == "ocupada" ~ "PEA Ocupada",
      pea == "pea" & ocupada == "no ocupada" ~ "PEA No Ocupada",
      TRUE ~ "resto"
    )
  ) |>
  filter(pea_ocupada != "resto") |> # Elimina categorías no relevantes
  select(hh_member_id, pea_ocupada) # Selecciona columnas finales


# Cálculos para Contrato laboral
contrato <- enaho_500 |>
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |>
  select(P511A, FACTOR_POBLACIONAL_CAP500, hh_member_id) |>
  left_join(enaho_200, by = "hh_member_id") |> # Une con datos del capítulo 200
  filter(P209A == 2) |> # Filtra por nacionalidad Venezolana
  filter(!is.na(P511A)) |> # Elimina NA en la variable de contrato // P511A tipo de contrato
  filter(!is.na(FACTOR_POBLACIONAL_CAP500)) |> # Elimina NA en el factor poblacional
  filter(P208A >= 14) |> # Filtra por edad (14 años o más)
  mutate(contrato = case_when( # Categoriza el tipo de contrato
    P511A == 1 ~ "Contrato indefinido, nombrado, permanente",
    P511A == 2 ~ "Contrato a plazo fijo (sujeto a modalidad)",
    P511A == 3 ~ "Está en período de prueba",
    P511A == 4 ~ "Convenios de Formación Laboral Juvenil /Prácticas Pre-profesionales",
    P511A == 5 ~ "Contrato por locación de servicios (Honorarios profesionales, RUC), SNP",
    P511A == 6 ~ "Regimen Especial de Contratación Administrativa (CAS)",
    P511A == 7 ~ "Sin contrato",
    P511A == 8 ~ "Otro"
  )) |>
  select(hh_member_id, contrato) # Selecciona columnas finales

# Cálculos para Pensión
pension <- enaho_500 |>
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |>
  select(P558A1:P558A5, FACTOR_POBLACIONAL_CAP500, hh_member_id) |>
  left_join(enaho_200, by = "hh_member_id") |> # Une con datos del capítulo 200
  filter(P209A == 2) |> # Filtra por nacionalidad Venezolana
  filter(!is.na(FACTOR_POBLACIONAL_CAP500)) |> # Elimina NA en el factor poblacional
  filter(P208A >= 14) |> # Filtra por edad (14 años o más)
  mutate(pension = case_when( # Categoriza el tipo de afiliación a pensión
    P558A1 == 1 ~ "Sistema privado de pensiones (AFP)",
    P558A2 == 2 ~ "Sistema Nacional de Pensiones: Ley 19990",
    P558A3 == 3 ~ "Sistema Nacional de Pensiones: Ley 20530",
    P558A4 == 4 ~ "Otro",
    P558A5 == 5 ~ "No esta afiliado",
    TRUE ~ NA_character_ # Si no coincide, asigna NA
  )) |>
  filter(!is.na(pension)) |> # Elimina filas con NA en la variable de pensión
  select(hh_member_id, pension) # Selecciona columnas finales


# Cálculos para Inclusión Financiera
inclusion <- enaho_500 |>
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |>
  select(P558E1_1:P558E1_10, FACTOR_POBLACIONAL_CAP500, hh_member_id) |>
  left_join(enaho_200, by = "hh_member_id") |> # Une con datos del capítulo 200
  filter(P209A == 2) |> # Filtra por nacionalidad Venezolana
  filter(!is.na(FACTOR_POBLACIONAL_CAP500)) |> # Elimina NA en el factor poblacional
  filter(P208A >= 14) |> # Filtra por edad (14 años o más)
  mutate(
    cuentas = if_else(if_any(c(P558E1_1, P558E1_2, P558E1_3, P558E1_7), ~ . > 0), "Cuenta", "Sin Cuenta"), # Si tiene algún tipo de cuenta
    pagos = if_else(if_any(c(P558E1_4, P558E1_8), ~ . > 0), "Tarjeta/Billetera", "Sin Tarjeta/Billetera"), # Si usa tarjeta/billetera digital
    prestamo = if_else(P558E1_9 > 0, "Prestamo", "Sin Prestamo") # Si tiene préstamo
  ) |>
  mutate(INT_D5 = ifelse(cuentas == "Cuenta" | pagos == "Tarjeta/Billetera" | prestamo == "Prestamo", 1, 0)) |> # Indicador INT_D5 (acceso a servicios financieros)
  select(hh_member_id, INT_D5) # Selecciona columnas finales


## Merge de los datos del capítulo 500
# Une las tablas de PEA, contrato, pensión e inclusión financiera con enaho_500
enaho_500 <- enaho_500 |>
  left_join(pea_ocupada, by = "hh_member_id") |>
  left_join(contrato, by = "hh_member_id") |>
  left_join(pension, by = "hh_member_id") |>
  left_join(inclusion, by = "hh_member_id")


# -----------------------------------------------------------------------------
# 9. Merge final de los datos de todos los capítulos
#    Combina todas las bases de datos procesadas en una sola tabla grande.
# -----------------------------------------------------------------------------
enaho_merged <- enaho_200 |>
  left_join(enaho_300, by = "hh_member_id") |>
  left_join(enaho_400, by = "hh_member_id") |>
  left_join(enaho_500, by = "hh_member_id") |>
  left_join(enaho_1000, by = "hh_member_id")


#### -----------------------------------------------------------------------------
#### 10. Preparación final y Cálculo de Indicadores
####     Se calculan las proporciones para cada indicador deseado.
#### -----------------------------------------------------------------------------

# Indicador INT_D1: PEA No Ocupada
INT_D1_2025 <- enaho_merged |>
  filter(resid == 1 & P209A.x == 2) |> # Filtra por residentes venezolanos
  count(pea_ocupada, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(pea_ocupada)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(pea_ocupada == "PEA No Ocupada") |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador INT_D2: No definido en este script
INT_D2_2025 <- NA # Se asigna NA ya que no hay cálculo para este indicador

# Indicador INT_D3: No tiene trabajo formal (sin pensión o sin contrato)
INT_D3_2025 <- enaho_merged |>
  filter(resid == 1 & P209A.x == 2) |> # Filtra por residentes venezolanos
  mutate(INT_D3 = ifelse(pension == "No esta afiliado" | contrato == "Sin contrato", 1, 0))|> # Define el indicador
  mutate(INT_D3 = case_when( # Categoriza el indicador
    INT_D3 == 1 ~ "No tiene trabajo formal",
    INT_D3 == 0 ~ "Tiene trabajo formal"
  )) |>
  count(INT_D3, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(INT_D3)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(INT_D3 == "No tiene trabajo formal") |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador INT_D4: Personas que se han sentido discriminadas por su nacionalidad
INT_D4_2025 <- enaho_merged |>
  filter(resid == 1 & P209A.x == 2) |> # Filtra por residentes venezolanos
  mutate(INT_D4 = case_when( # Categoriza el indicador
    INT_D4 == 1 ~ "Personas que se han sentido discriminadas por su nacionalidad",
    INT_D4 == 0 ~ "Personas que no se han sentido discriminadas por su nacionalidad"
  )) |>
  count(INT_D4, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(INT_D4)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(INT_D4 == "Personas que se han sentido discriminadas por su nacionalidad") |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador INT_D5: Personas sin acceso a servicios financieros
INT_D5_2025 <- enaho_merged |>
  filter(resid == 1 & P209A.x == 2) |> # Filtra por residentes venezolanos
  mutate(INT_D5 = case_when( # Categoriza el indicador
    INT_D5 == 1 ~ "Personas que tienen acceso a servicios financieros",
    INT_D5 == 0 ~ "Personas sin acceso a servicios financieros"
  )) |>
  count(INT_D5, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(INT_D5)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(INT_D5 == "Personas sin acceso a servicios financieros") |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador EDU_D1: NNA sin matrícula (primaria/secundaria)
EDU_D1_2025 <- enaho_merged |>
  filter(resid == 1 & P209A.x == 2 & (pobprim == 1 | pobsec == 1)) |> # Filtra por NNA residentes venezolanos en edad escolar primaria/secundaria
  mutate(EDU_D1 = ifelse(matricula_prim == 1 | matricula_secun == 1, 1, 0)) |> # Define si está matriculado
  mutate(EDU_D1 = case_when( # Categoriza el indicador
    EDU_D1 == 1 ~ "NNA refugiados y migrantes entre los 3 y 16 años matriculados",
    EDU_D1 == 0 ~ "NNA refugiados y migrantes entre los 3 y 16 años sin matrícula"
  ))|>
  count(EDU_D1, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(EDU_D1)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(EDU_D1 == "NNA refugiados y migrantes entre los 3 y 16 años sin matrícula") |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador EDU_D2: NNA sin matrícula inicial
EDU_D2_2025 <- enaho_merged |>
  filter(resid == 1 & P209A.x == 2 & pobinic == 1) |> # Filtra por NNA residentes venezolanos en edad inicial
  mutate(matricula_inic = case_when( # Categoriza el indicador
    matricula_inic == 1 ~ "NNA refugiados y migrantes entre los 0 y 3 años matriculados",
    matricula_inic == 0 ~ "NNA refugiados y migrantes entre los 0 y 3 años sin matrícula inicial"
  )) |>
  count(matricula_inic, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(matricula_inic)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(matricula_inic == "NNA refugiados y migrantes entre los 0 y 3 años sin matrícula inicial") |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador EDU_D3: NNA que no asisten de manera regular
EDU_D3_2025 <- enaho_merged |>
  filter(resid == 1 & P209A.x == 2 & (pobprim == 1 | pobsec == 1)) |> # Filtra por NNA residentes venezolanos en edad escolar primaria/secundaria
  mutate(EDU_D3 = ifelse(prim == 1 | secun == 1, 1, 0)) |> # Define si asisten regularmente
  mutate(EDU_D3 = case_when( # Categoriza el indicador
    EDU_D3 == 1 ~ "NNA refugiados y migrantes entre los 3 y 16 años asisten de manera regular",
    EDU_D3 == 0 ~ "NNA refugiados y migrantes entre los 3 y 16 años no asisten de manera regular"
  )) |>
  count(EDU_D3, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(EDU_D3)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(EDU_D3 == "NNA refugiados y migrantes entre los 3 y 16 años no asisten de manera regular") |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador EDU_D4: No definido en este script
EDU_D4_2025 <- NA # Se asigna NA

# Indicador HE_D1: Personas que no tienen acceso a cobertura de salud
HE_D1_2025 <- enaho_merged |>
  filter(resid == 1 & P209A.x == 2) |> # Filtra por residentes venezolanos
  mutate(HE_D1 = case_when( # Categoriza el indicador
    HE_D1 == 1 ~ "Personas que no tienen acceso a cobertura de salud",
    HE_D1 == 0 ~ "Personas que tienen acceso a cobertura de salud"
  )) |>
  count(HE_D1, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(HE_D1)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(HE_D1 == "Personas que no tienen acceso a cobertura de salud") |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador HE_D2: Personas que han requerido atención pero no han accedido
HE_D2_2025 <- enaho_merged |>
  filter(resid == 1 & P209A.x == 2) |> # Filtra por residentes venezolanos
  mutate(HE_D2 = case_when( # Categoriza el indicador
    HE_D2 == 1 ~ "Personas que han requerido algún tipo de atención en salud en el país de destino, pero no han podido acceder al mismo",
    HE_D2 == 0 ~ "" # Sin etiqueta para 0
  )) |>
  count(HE_D2, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(HE_D2)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(HE_D2 == "Personas que han requerido algún tipo de atención en salud en el país de destino, pero no han podido acceder al mismo") |>
  pull(p) # Extrae el valor de la proporción

# Indicador WA_D1: (Calculado con enaho_100 y FACTORBASICO)
WA_D1_2025 <- enaho_100 |>
  count(WA_D1, wt = FACTORBASICO) |> # Cuenta con ponderación
  filter(!is.na(WA_D1)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(WA_D1 == 1) |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador WA_D2: (Calculado con enaho_100 y FACTORBASICO)
WA_D2_2025 <- enaho_100 |>
  count(WA_D2, wt = FACTORBASICO) |> # Cuenta con ponderación
  filter(!is.na(WA_D2)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(WA_D2 == 1) |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador WA_D3: (Calculado con enaho_100 y FACTORBASICO)
WA_D3_2025 <- enaho_100 |>
  count(WA_D3, wt = FACTORBASICO) |> # Cuenta con ponderación
  filter(!is.na(WA_D3)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(WA_D3 == 1) |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador WA_D4: (Calculado con enaho_100 y FACTORBASICO)
WA_D4_2025 <- enaho_100 |>
  count(WA_D4, wt = FACTORBASICO) |> # Cuenta con ponderación
  filter(!is.na(WA_D4)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(WA_D4 == 1) |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicadores WA_D6, WA_D8, WA_D11: No definidos en este script
WA_D6_2025 <- NA
WA_D8_2025 <- NA
WA_D11_2025 <- NA

# Indicadores NUT_D1 a NUT_D11: No definidos en este script (Nutrición)
NUT_D1_2025 <- NA
NUT_D2_2025 <- NA
NUT_D4_2025 <- NA
NUT_D5_2025 <- NA
NUT_D8_2025 <- NA
NUT_D10_2025 <- NA
NUT_D11_2025 <- NA

# Indicadores HT_D1 a HT_D5: No definidos en este script (Transferencias Humanitarias)
HT_D1_2025 <- NA
HT_D2_2025 <- NA
HT_D5_2025 <- NA

# Indicador SHE_D1: (Calculado con enaho_100 y FACTORBASICO)
SHE_D1_2025 <- enaho_100 |>
  count(SHE_D1, wt = FACTORBASICO) |> # Cuenta con ponderación
  filter(!is.na(SHE_D1)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(SHE_D1 == 1) |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador SHE_D2: (Calculado con enaho_100 y FACTORBASICO)
SHE_D2_2025 <- enaho_100 |>
  count(SHE_D2, wt = FACTORBASICO) |> # Cuenta con ponderación
  filter(!is.na(SHE_D2)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(SHE_D2 == 1) |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicadores SHE_D3 y SHE_D4: No definidos en este script, se inicializan a NA
SHE_D3_2025 <- NA
SHE_D4_2025 <- NA

# Indicadores PRO_D1 y PRO_D3: No definidos en este script (Protección)
PRO_D1_2025 <- NA
PRO_D3_2025 <- NA

# Indicador PRO_D4: (Calculado con enaho_merged y FACTOR_POBLACIONAL_CAP200)
PRO_D4_2025 <- enaho_merged |>
  count(PRO_D4, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(PRO_D4)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(PRO_D4 == 1) |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador PRO_D5: (Calculado con enaho_merged y FACTOR_POBLACIONAL_CAP200)
PRO_D5_2025 <- enaho_merged |>
  count(PRO_D5, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(PRO_D5)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(PRO_D5 == 1) |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicadores CP_D1 y CP_D3: No definidos en este script (Protección de la Niñez)
CP_D1_2025 <- NA
CP_D3_2025 <- NA

# Indicadores HTS_D1 y HTS_D2: No definidos en este script (Trata de Personas)
HTS_D1_2025 <- NA
HTS_D2_2025 <- NA

# Indicadores FS_D1 a FS_D4: No definidos en este script (Seguridad Alimentaria)
FS_D1_2025 <- NA
FS_D2_2025 <- NA
FS_D3_2025 <- NA
FS_D4_2025 <- NA

# Indicador GBV_D3: (Calculado con enaho_merged y FACTOR_POBLACIONAL_CAP200)
GBV_D3_2025 <- enaho_merged |>
  count(GBV_D3, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(GBV_D3)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(GBV_D3 == 1) |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# Indicador GBV_fake: (Calculado con enaho_merged y FACTOR_POBLACIONAL_CAP200)
GBV_fake_2025 <- enaho_merged |>
  count(GBV_fake, wt = FACTOR_POBLACIONAL_CAP200) |> # Cuenta con ponderación
  filter(!is.na(GBV_fake)) |> # Elimina NA
  mutate(p = n / sum(n)) |> # Calcula la proporción
  filter(GBV_fake == 1) |> # Filtra por la categoría de interés
  pull(p) # Extrae el valor de la proporción

# -----------------------------------------------------------------------------
# 11. Creación de la tabla final de indicadores
#     Se combinan todos los indicadores calculados en un solo data frame.
# -----------------------------------------------------------------------------
my_table <- tibble( # Crea un tibble (data frame moderno)
  "ID 2025" = c("INT_D1", "INT_D2", "INT_D3", "INT_D4", "INT_D5",
                "EDU_D1", "EDU_D2", "EDU_D3", "EDU_D4",
                "HE_D1", "HE_D2",
                "WA_D1", "WA_D2", "WA_D3", "WA_D4", "WA_D6", "WA_D8", "WA_D11",
                "NUT_D1", "NUT_D2", "NUT_D4", "NUT_D5", "NUT_D8", "NUT_D10", "NUT_D11",
                "HT_D1", "HT_D2", "HT_D5",
                "SHE_D1", "SHE_D2", "SHE_D3", "SHE_D4",
                "PRO_D1", "PRO_D3", "PRO_D4", "PRO_D5",
                "CP_D1", "CP_D3",
                "GBV_D1", # Usamos GBV_D3_2025 como valor para este ID
                "HTS_D1", "HTS_D2", "FS_D1", "FS_D2", "FS_D3", "FS_D4"),
  "Indicador 2025" = c(INT_D1_2025, INT_D2_2025, INT_D3_2025, INT_D4_2025, INT_D5_2025,
                       EDU_D1_2025, EDU_D2_2025, EDU_D3_2025, EDU_D4_2025,
                       HE_D1_2025, HE_D2_2025,
                       WA_D1_2025, WA_D2_2025, WA_D3_2025, WA_D4_2025, WA_D6_2025, WA_D8_2025, WA_D11_2025,
                       NUT_D1_2025, NUT_D2_2025, NUT_D4_2025, NUT_D5_2025, NUT_D8_2025, NUT_D10_2025, NUT_D11_2025,
                       HT_D1_2025, HT_D2_2025, HT_D5_2025,
                       SHE_D1_2025, SHE_D2_2025, SHE_D3_2025, SHE_D4_2025,
                       PRO_D1_2025, PRO_D3_2025, PRO_D4_2025, PRO_D5_2025,
                       CP_D1_2025, CP_D3_2025,
                       GBV_D3_2025,
                       HTS_D1_2025, HTS_D2_2025,
                       FS_D1_2025, FS_D2_2025, FS_D3_2025, FS_D4_2025)
)

# -----------------------------------------------------------------------------
# 12. Exportar la tabla de indicadores a un archivo Excel
# -----------------------------------------------------------------------------
write_xlsx(my_table, "JNA_Peru_2025_R.xlsx")
message("¡El archivo 'JNA_Peru_2025_R.xlsx' se ha exportado exitosamente!")