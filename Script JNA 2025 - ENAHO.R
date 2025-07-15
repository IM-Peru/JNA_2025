
###############################################
############# Indicadores JNA 2025 ############ 
###############################################

library(srvyr)
library(survey)
library(haven)

enaho_100 <- read_sav("C:/Users/GARCIAAC/UNHCR/Peru - Interagency - IM/1. Situation Analysis/4. Other Assessments/ENAHO - INTERNAL/ENAHO/data/EnahoPV01-2024-100.sav")

enaho_200 <- read_sav("C:/Users/GARCIAAC/UNHCR/Peru - Interagency - IM/1. Situation Analysis/4. Other Assessments/ENAHO - INTERNAL/ENAHO/data/EnahoPV01-2024-200.sav")

enaho_300 <- read_sav("C:/Users/GARCIAAC/UNHCR/Peru - Interagency - IM/1. Situation Analysis/4. Other Assessments/ENAHO - INTERNAL/ENAHO/data/EnahoPV01A-2024-300.sav")

enaho_400 <- read_sav("C:/Users/GARCIAAC/UNHCR/Peru - Interagency - IM/1. Situation Analysis/4. Other Assessments/ENAHO - INTERNAL/ENAHO/data/EnahoPV01A-2024-400.sav")

enaho_500 <- read_sav("C:/Users/GARCIAAC/UNHCR/Peru - Interagency - IM/1. Situation Analysis/4. Other Assessments/ENAHO - INTERNAL/ENAHO/data/EnahoPV01A-2024-500.sav")

enaho_1000 <- read_sav("C:/Users/GARCIAAC/UNHCR/Peru - Interagency - IM/1. Situation Analysis/4. Other Assessments/ENAHO - INTERNAL/ENAHO/data/EnahoPV01A-2024-1000.sav")


# Capítulo 100 para indicadores de Shelter y WASH

# Se completa los valores para los hogares secundarios
enaho_100 <- enaho_100 |>
  filter(RESULT %in% c(1,2))|>
  tidyr::fill(everything(), .direction = "down") |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR))

# Calculamos la variable total de miembros del hogar
enaho_200_members <- enaho_200 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |>
  group_by(hh_id) |>
  summarise(hh_members = n())

enaho_100 <- left_join(enaho_100, enaho_200_members, by = "hh_id")

enaho_100 <- enaho_100 |>
  mutate(SHE_D1_Q1 = ifelse(P101 %in% c(5:8), 1,0),
         SHE_D1_Q3 = ifelse((P102 %in% c(3:9) | 
                               P103 == 6 | 
                               P103A %in% c(5:8)), 1, 0),
         SHE_D1_Q4 = ifelse((P1121 == 0 |
                               (P1132 == 0 & P1133 == 0) |
                               P111A %in% c(3:9) |
                               P110 %in% c(3:8)), 1, 0)) |>
  mutate(SHE_D1 = ifelse(SHE_D1_Q1 == 1 | SHE_D1_Q3 == 1 | SHE_D1_Q4 == 1, 1, 0)) |>
  mutate(SHE_D2 = ifelse(hh_members/P104A > 3, 1, 0)) |>
  mutate(WA_D1 = ifelse(P110 %in% c(3:8), 1, 0),
         WA_D2 = ifelse(!(P110 == 1 & P110C == 1 & P110C1 == 24), 1, 0),
         WA_D3 = ifelse(P110A1 == 2, 1, 0),
         WA_D4 = ifelse(P111A %in% c(2:9), 1, 0)) 


enaho_200 <- enaho_200 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |>
  mutate(hh_member_id = paste0(hh_id, CODPERSO)) |>
  mutate(
    resid = case_when(
      P204 == 1 & P205 == 2  ~ 1,
      P204 == 2 & P206 == 1  ~ 1,
      TRUE ~ 2
    ),
    pobinic = ifelse(P204 == 1 & !P203 %in% c(8:9) & P208A >= 3 & P208A <= 5, 1, 0),
    pobprim = ifelse(P204 == 1 & !P203 %in% c(8:9) & P208A >= 6 & P208A <= 11, 1, 0),
    pobsec = ifelse(P204 == 1 & !P203 %in% c(8:9) & P208A >= 12 & P208A <= 16, 1 , 0)) |>
  select(hh_id, hh_member_id, P203, P203B, P204, P205, P206, P207, P208A, P209A, P212, P213, P214, resid, pobinic, pobprim, pobsec, FACTOR_POBLACIONAL_CAP200)


# Indicadores de educación
enaho_300 <- enaho_300 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |>
  mutate(hh_member_id = paste0(hh_id, CODPERSO)) |>
  mutate(matricula_inic = ifelse(P306 == 1 & P308A == 1, 1, 0),
         matricula_prim = ifelse(P306 == 1 & (P308A %in% c(2, 7)), 1, 0),
         matricula_secun = ifelse(P306 == 1 & P308A == 3, 1, 0),
         inic = ifelse(P306 == 1 & P307 == 1 & P308A == 1, 1, 0),
         prim = ifelse(P306 == 1 & P307 == 1 & (P308A %in% c(2, 7)), 1, 0),
         secun = ifelse(P306 == 1 & P307 == 1 & P308A == 3, 1, 0)) |>
  select(hh_id, hh_member_id, matricula_inic, matricula_prim, matricula_secun, inic, prim, secun)

# Indicadores de salud
enaho_400 <- enaho_400 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |>
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |>
  mutate(HE_D1 = ifelse(P4191 == 2 & P4192 == 2 & P4193 == 2 & P4194 == 2 & P4195 == 2 & P4196 == 2 & P4197 == 2 & P4198 == 2, 1 ,0)) |>
  mutate(P403 = P4031 +	P4032 +	P4033 +	P4034 +	P4035 +	P4036 +	P4037 +	P4038 +	P4039) |>
  mutate(HE_D2 = if_else(P403 == 0, 1, 0, missing = 0)) |>
  select(hh_member_id, HE_D1, HE_D2)


# Indicadores de protección
enaho_1000 <- enaho_1000 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |>
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |>  
  mutate(PRO_D4 = ifelse(P1002 %in% c(6:9), 1, 0),
         PRO_D5 = ifelse(P1007 == 1 & (P1008_1 == 1 | P1008_2 == 2 | P1008_3 == 3 | P1008_4 == 4 | P1008_12 == 12), 1, 0),
         INT_D4 = ifelse(P1009 == 1 & P1010_5 == 5, 1, 0),
         GBV_D3 = ifelse(P1012 %in% c(1, 6), 1, 0),  #la escala es distinta  1) Muy inseguro/a? 2)Un poco seguro/a? 3) Bastante seguro/a? 4)Muy seguro/a? 5) No sabe 6) Prefiere no responder #considerando 1 y 2 el % sube a +40% aprox
         GBV_fake = ifelse(P1013_1 == 1 | P1013_2 == 1 | P1013_3 == 1 | P1013_4 == 1 | P1013_5 == 1 | P1013_6 == 1 | P1013_7 == 1, 1, 0)) |>
  select(hh_member_id, PRO_D4, PRO_D5, INT_D4, GBV_D3, GBV_fake)


# Indicadores de integración
enaho_500 <- enaho_500 |>
  mutate(hh_id = paste0(CONGLOME, VIVIENDA, HOGAR)) |>
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO))   |>
  select(hh_id, hh_member_id, CODINFOR, P501, P502, P503)
# INT_D1: 1 - PEA Ocupada
# INT_D2: 
# INT_D3: Contrato laboral y pension
# INT_D5: Inclusion financiera


# PEA Ocupada
pea_ocupada <- enaho_500 |>
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |>
  select(P501, P502, P503, P5041:P50411, P545, FACTOR_POBLACIONAL_CAP500, hh_member_id) |>
  left_join(enaho_200, by = "hh_member_id") |>
  filter(P209A == 2) |>  # Venezolanas
  filter(P208A >= 14) |>  # 14 años a más
  mutate(
    P504_rec = if_else(if_any(P5041:P50411, ~ . == 1), 1, 0),
    pea = case_when(
      P501 == 1 | P502 == 1 | P503 == 1 | P504_rec == 1 | P545 == 1 ~ "pea",
      TRUE ~ "no pea"
    ),
    ocupada = case_when(
      P501 == 1 | P502 == 1 | P503 == 1 | P504_rec == 1 ~ "ocupada",
      TRUE ~ "no ocupada"
    ),
    pea_ocupada = case_when(
      pea == "pea" & ocupada == "ocupada" ~ "PEA Ocupada",
      pea == "pea" & ocupada == "no ocupada" ~ "PEA No Ocupada",
      TRUE ~ "resto"
    )
  ) |>
  filter(pea_ocupada != "resto") |>
  select(hh_member_id, pea_ocupada)


# Contrato
contrato <- enaho_500 |>
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |> 
  select(P511A, FACTOR_POBLACIONAL_CAP500, hh_member_id) |> 
  left_join(enaho_200, by = "hh_member_id") |> 
  filter(P209A == 2) |> 
  filter(!is.na(P511A)) |>
  filter(!is.na(FACTOR_POBLACIONAL_CAP500)) |>
  filter(P208A >= 14) |> 
  mutate(contrato = case_when(
    P511A == 1 ~ "Contrato indefinido, nombrado, permanente",
    P511A == 2 ~ "Contrato a plazo fijo (sujeto a modalidad)",
    P511A == 3 ~ "Está en período de prueba",
    P511A == 4 ~ "Convenios de Formación Laboral Juvenil /Prácticas Pre-profesionales",
    P511A == 5 ~ "Contrato por locación de servicios (Honorarios profesionales, RUC), SNP",
    P511A == 6 ~ "Regimen Especial de Contratación Administrativa (CAS)",
    P511A == 7 ~ "Sin contrato",
    P511A == 8 ~ "Otro"
  )) |>
  select(hh_member_id, contrato) 

# Pensión
pension <- enaho_500 |>
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |> 
  select(P558A1:P558A5, FACTOR_POBLACIONAL_CAP500, hh_member_id) |> 
  left_join(enaho_200, by = "hh_member_id") |> 
  filter(P209A == 2) |> 
  filter(!is.na(FACTOR_POBLACIONAL_CAP500)) |>
  filter(P208A >= 14) |> 
  mutate(pension = case_when(
    P558A1 == 1 ~ "Sistema privado de pensiones (AFP)",
    P558A2 == 2 ~ "Sistema Nacional de Pensiones: Ley 19990",
    P558A3 == 3 ~ "Sistema Nacional de Pensiones: Ley 20530",
    P558A4 == 4 ~ "Otro",
    P558A5 == 5 ~ "No esta afiliado",
    TRUE ~ NA_character_
  )) |> 
  filter(!is.na(pension)) |>
  select(hh_member_id, pension)



# Inclusión Financiera
inclusion <- enaho_500 |>
  mutate(hh_member_id = paste0(CONGLOME, VIVIENDA, HOGAR, CODPERSO)) |> 
  select(P558E1_1:P558E1_10, FACTOR_POBLACIONAL_CAP500, hh_member_id) |> 
  left_join(enaho_200, by = "hh_member_id") |> 
  filter(P209A == 2) |> 
  filter(!is.na(FACTOR_POBLACIONAL_CAP500)) |>
  filter(P208A >= 14) |>
  mutate(cuentas = if_else(if_any(c(P558E1_1, P558E1_2, P558E1_3, P558E1_7), ~ . > 0), "Cuenta", "Sin Cuenta")) |> 
  mutate(pagos = if_else(if_any(c(P558E1_4, P558E1_8), ~ . > 0), "Tarjeta/Billetera", "Sin Tarjeta/Billetera")) |> 
  mutate(prestamo = if_else(P558E1_9 > 0, "Prestamo", "Sin Prestamo")) |>
  mutate(INT_D5 = ifelse(cuentas == "Cuenta" | pagos == "Tarjeta/Billetera" | prestamo == "Prestamo", 1, 0)) |>
  select(hh_member_id, INT_D5)


## Merge de los datos de capítulo 500
enaho_500 <- enaho_500 |>
  left_join(pea_ocupada, by = "hh_member_id") |>
  left_join(contrato, by = "hh_member_id") |>
  left_join(pension, by = "hh_member_id") |>
  left_join(inclusion, by = "hh_member_id")


## Merge de los datos de todos los capítulos
enaho_merged <- enaho_200 |>
  left_join(enaho_300, by = "hh_member_id") |>
  left_join(enaho_400, by = "hh_member_id") |>
  left_join(enaho_500, by = "hh_member_id") |>
  left_join(enaho_1000, by = "hh_member_id")


#### Preparación final

INT_D1_2025 <- enaho_merged |>
  filter(resid == 1 & P209A == 2) |>
  count(pea_ocupada, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(pea_ocupada)) |>
  mutate(p = n / sum(n)) |>
  filter(pea_ocupada == "PEA No Ocupada") |>
  pull(p)


INT_D2_2025 <- NA


INT_D3_2025 <- enaho_merged |>
  filter(resid == 1 & P209A == 2) |>
  mutate(INT_D3 = ifelse(pension == "No esta afiliado" | contrato == "Sin contrato", 1, 0))|>
  mutate(INT_D3 = case_when(
    INT_D3 == 1 ~ "No tiene trabajo formal",
    INT_D3 == 0 ~ "Tiene trabajo formal"
  )) |>
  count(INT_D3, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(INT_D3)) |>
  mutate(p = n / sum(n)) |>
  filter(INT_D3 == "No tiene trabajo formal") |>
  pull(p)


INT_D4_2025 <- enaho_merged |>
  filter(resid == 1 & P209A == 2) |>
  mutate(INT_D4 = case_when(
    INT_D4 == 1 ~ "Personas que se han sentido discriminadas por su nacionalidad",
    INT_D4 == 0 ~ "Personas que no se han sentido discriminadas por su nacionalidad"
  )) |>
  count(INT_D4, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(INT_D4)) |>
  mutate(p = n / sum(n)) |>
  filter(INT_D4 == "Personas que se han sentido discriminadas por su nacionalidad") |>
  pull(p)


INT_D5_2025 <- enaho_merged |>
  filter(resid == 1 & P209A == 2) |>
  mutate(INT_D5 = case_when(
    INT_D5 == 1 ~ "Personas que tienen acceso a servicios financieros",
    INT_D5 == 0 ~ "Personas sin acceso a servicios financieros"
  )) |>
  count(INT_D5, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(INT_D5)) |>
  mutate(p = n / sum(n)) |>
  filter(INT_D5 == "Personas sin acceso a servicios financieros") |>
  pull(p)


EDU_D1_2025 <- enaho_merged |>
  filter(resid == 1 & P209A == 2 & (pobprim == 1 | pobsec == 1)) |>
  mutate(EDU_D1 = ifelse(matricula_prim == 1 | matricula_secun == 1, 1, 0)) |>
  mutate(EDU_D1 = case_when(
    EDU_D1 == 1 ~ "NNA refugiados y migrantes entre los 3 y 16 años matriculados",
    EDU_D1 == 0 ~ "NNA refugiados y migrantes entre los 3 y 16 años sin matrícula"
  ))|>
  count(EDU_D1, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(EDU_D1)) |>
  mutate(p = n / sum(n)) |>
  select(-n)


EDU_D1_2025 <- enaho_merged |>
  filter(resid == 1 & P209A == 2 & (pobprim == 1 | pobsec == 1)) |>
  mutate(EDU_D1 = ifelse(matricula_prim == 1 | matricula_secun == 1, 1, 0)) |>
  mutate(EDU_D1 = case_when(
    EDU_D1 == 1 ~ "NNA refugiados y migrantes entre los 3 y 16 años matriculados",
    EDU_D1 == 0 ~ "NNA refugiados y migrantes entre los 3 y 16 años sin matrícula"
  ))|>
  count(EDU_D1, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(EDU_D1)) |>
  mutate(p = n / sum(n)) |>
  filter(EDU_D1 == "NNA refugiados y migrantes entre los 3 y 16 años sin matrícula") |>
  pull(p)


EDU_D2_2025 <- enaho_merged |>
  filter(resid == 1 & P209A == 2 & pobinic == 1) |>
  mutate(matricula_inic = case_when(
    matricula_inic == 1 ~ "NNA refugiados y migrantes entre los 0 y 3 años matriculados",
    matricula_inic == 0 ~ "NNA refugiados y migrantes entre los 0 y 3 años sin matrícula inicial"
  )) |>
  count(matricula_inic, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(matricula_inic)) |>
  mutate(p = n / sum(n)) |>
  filter(matricula_inic == "NNA refugiados y migrantes entre los 0 y 3 años sin matrícula inicial") |>
  pull(p)


EDU_D3_2025 <- enaho_merged |>
  filter(resid == 1 & P209A == 2 & (pobprim == 1 | pobsec == 1)) |>
  mutate(EDU_D3 = ifelse(prim == 1 | secun == 1, 1, 0)) |>
  mutate(EDU_D3 = case_when(
    EDU_D3 == 1 ~ "NNA refugiados y migrantes entre los 3 y 16 años asisten de manera regular",
    EDU_D3 == 0 ~ "NNA refugiados y migrantes entre los 3 y 16 años no asisten de manera regular"
  )) |>
  count(EDU_D3, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(EDU_D3)) |>
  mutate(p = n / sum(n)) |>
  filter(EDU_D3 == "NNA refugiados y migrantes entre los 3 y 16 años no asisten de manera regular") |>
  pull(p)

EDU_D4_2025 <- NA

HE_D1_2025 <- enaho_merged |>
  filter(resid == 1 & P209A == 2) |>
  mutate(HE_D1 = case_when(
    HE_D1 == 1 ~ "Personas que no tienen acceso a cobertura de salud",
    HE_D1 == 0 ~ "Personas que tienen acceso a cobertura de salud"
  )) |>
  count(HE_D1, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(HE_D1)) |>
  mutate(p = n / sum(n)) |>
  filter(HE_D1 == "Personas que no tienen acceso a cobertura de salud") |>
  pull(p)


HE_D2_2025 <- enaho_merged |>
  filter(resid == 1 & P209A == 2) |>
  mutate(HE_D2 = case_when(
    HE_D2 == 1 ~ "Personas que han requerido algún tipo de atención en salud en el país de destino, pero no han podido acceder al mismo",
    HE_D2 == 0 ~ ""
  )) |>
  count(HE_D2, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(HE_D2)) |>
  mutate(p = n / sum(n)) |>
  filter(HE_D2 == "Personas que han requerido algún tipo de atención en salud en el país de destino, pero no han podido acceder al mismo") |>
  pull(p)

WA_D1_2025 <- enaho_100 |>
  count(WA_D1, wt = FACTORBASICO) |>
  filter(!is.na(WA_D1)) |>
  mutate(p = n / sum(n)) |>
  filter(WA_D1 == 1) |>
  pull(p)


WA_D2_2025 <- enaho_100 |>
  count(WA_D2, wt = FACTORBASICO) |>
  filter(!is.na(WA_D2)) |>
  mutate(p = n / sum(n)) |>
  filter(WA_D2 == 1) |>
  pull(p)

WA_D3_2025 <- enaho_100 |>
  count(WA_D3, wt = FACTORBASICO) |>
  filter(!is.na(WA_D3)) |>
  mutate(p = n / sum(n)) |>
  filter(WA_D3 == 1) |>
  pull(p)

WA_D4_2025 <- enaho_100 |>
  count(WA_D4, wt = FACTORBASICO) |>
  filter(!is.na(WA_D4)) |>
  mutate(p = n / sum(n)) |>
  filter(WA_D4 == 1) |>
  pull(p)


WA_D6_2025 <- NA
WA_D8_2025 <- NA
WA_D11_2025 <- NA

NUT_D1_2025 <- NA
NUT_D2_2025 <- NA
NUT_D4_2025 <- NA
NUT_D5_2025 <- NA
NUT_D8_2025 <- NA
NUT_D10_2025 <- NA
NUT_D11_2025 <- NA
HT_D1_2025 <- NA
HT_D2_2025 <- NA
HT_D5_2025 <- NA


SHE_D1_2025 <- enaho_100 |>
  count(SHE_D1, wt = FACTORBASICO) |>
  filter(!is.na(SHE_D1)) |>
  mutate(p = n / sum(n)) |>
  filter(SHE_D1 == 1) |>
  pull(p)

SHE_D2_2025 <- enaho_100 |>
  count(SHE_D2, wt = FACTORBASICO) |>
  filter(!is.na(SHE_D2)) |>
  mutate(p = n / sum(n)) |>
  filter(SHE_D2 == 1) |>
  pull(p)

PRO_D1_2025 <- NA
PRO_D3_2025 <- NA


PRO_D4_2025 <- enaho_merged |>
  count(PRO_D4, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(PRO_D4)) |>
  mutate(p = n / sum(n)) |>
  filter(PRO_D4 == 1) |>
  pull(p)

PRO_D5_2025 <- enaho_merged |>
  count(PRO_D5, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(PRO_D5)) |>
  mutate(p = n / sum(n)) |>
  filter(PRO_D5 == 1) |>
  pull(p)

CP_D1_2025 <- NA 
CP_D3_2025 <- NA
HTS_D1_2025 <- NA
HTS_D2_2025 <- NA
FS_D1_2025 <- NA
FS_D2_2025 <- NA
FS_D3_2025 <- NA
FS_D4_2025 <- NA
SHE_D3_2025 <- NA
SHE_D4_2025 <- NA

GBV_D3_2025  <- enaho_merged |>
  count(GBV_D3, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(GBV_D3)) |>
  mutate(p = n / sum(n)) |>
  filter(GBV_D3 == 1) |>
  pull(p)

GBV_fake_2025  <- enaho_merged |>
  count(GBV_fake, wt = FACTOR_POBLACIONAL_CAP200) |>
  filter(!is.na(GBV_fake)) |>
  mutate(p = n / sum(n)) |>
  filter(GBV_fake == 1) |>
  pull(p)


my_table <- tibble(
  "ID 2025" = c("INT_D1", "INT_D2", "INT_D3", "INT_D4", "INT_D5", 
                "EDU_D1", "EDU_D2", "EDU_D3", "EDU_D4",
                "HE_D1", "HE_D2", 
                "WA_D1", "WA_D2", "WA_D3", "WA_D4", "WA_D6", "WA_D8", "WA_D11", 
                "NUT_D1", "NUT_D2", "NUT_D4", "NUT_D5", "NUT_D8", "NUT_D10", "NUT_D11",
                "HT_D1", "HT_D2", "HT_D5", 
                "SHE_D1", "SHE_D2", "SHE_D3", "SHE_D4",
                "PRO_D1", "PRO_D3", "PRO_D4", "PRO_D5", 
                "CP_D1", "CP_D3",
                "GBV_D1", 
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

write_xlsx(my_table, "JNA_Peru_2025.xlsx")





