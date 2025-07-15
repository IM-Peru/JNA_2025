library(tidyverse)
library(readxl)
library(ggplot2)
library(writexl)
library(labelled)
library(robotoolbox)
library(Hmisc)
library(modeest)
library(dplyr)

data <- read_xlsx("JNA_merged_data.xlsx")

###############################################
######## Indicadores a nivel Individual ####### 
###############################################

#Integración 
INT_D1 <- data |>
  count(INT_D1_Q1) |>
  filter(INT_D1_Q1 %in% c(1,2)) |>
  summarise(value = n[INT_D1_Q1 == 2] / sum(n)) |>
  pull(value)

#INT_D2 "Porcentaje de personas en subempleo (por insuficiencia de horas)" es nuevo y no se calculó para JNA 2024. Se cambió el correlativo
  
INT_D3 <- data |>
  filter(INT_D1_Q1 %in% c(1)) |>
  count(INT_D2) |>
  summarise(value = n[INT_D2 == 1] / sum(n)) |>
  pull(value)

EDU_D1 <- data |>
  filter(!is.na(EDU_D1_Q1)) |>
  count(EDU_D1_Q1) |>
  summarise(value = n[EDU_D1_Q1 == 0] / sum(n)) |>
  pull(value)

EDU_D2 <- data |>
  filter(!is.na(EDU_D2_Q1)) |>
  count(EDU_D2) |>
  summarise(value = n[EDU_D2 == 1] / sum(n)) |>
  pull(value)

EDU_D3 <- data |>
  filter(EDU_D1_Q1 == 1) |>
  count(EDU_D3_Q1) |>
  summarise(value = sum(n[EDU_D3_Q1 %in% c(1,2,3,4,98,99)], na.rm = TRUE) / sum(n)) |>
  pull(value)

HE_D1 <- data |>
  count(HE_D4) |>
  summarise(value = n[HE_D4 == 1] / sum(n)) |>
  pull(value)

HE_D2 <- data |>
  count(HE_D1_Q2) |>
  summarise(value = sum(n[HE_D1_Q2 %in% c(0)], na.rm = TRUE) / sum(n)) |>
  pull(value)

PRO_D4 <- data |>
  filter(MH_3_1 == "1") |>
  mutate(
    irregular_person = ifelse(
    PRO_D4_Q1_3 == "0" & PRO_D4_Q1_4 == "0" & PRO_D4_Q1_7 == "0", 
    1, 0)) |>
  count(irregular_person) |>
  summarise(value = n[irregular_person == 1] / sum(n)) |>
  pull(value)
  


###############################################
########### Indicadores a nivel Hogar ######### 
###############################################

INT_D4 <- data |>
  group_by(id_hogar) |>
  summarise(
    INT_D3_Q1B_7 = max(INT_D3_Q1B_7),
    INT_D3 = max(INT_D3)) |>
  count(INT_D3) |>
  summarise(value = n[INT_D3 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

INT_D5 <- data |>
  group_by(id_hogar) |>
  summarise(
    INT_D4_Q1_10  = max(INT_D4_Q1_10 ),
    INT_D4 = max(INT_D4)) |>
  count(INT_D4) |>
  summarise(value = n[INT_D4 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

WA_D1 <- data |>
  group_by(id_hogar) |> 
  summarise(WA_D1 = max(WA_D1)) |>
  count(WA_D1) |>
  summarise(value = n[WA_D1 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

WA_D2 <- data |>
  group_by(id_hogar) |> 
  summarise(WA_D2 = max(WA_D2)) |>
  count(WA_D2) |>
  summarise(value = n[WA_D2 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

#WA_D3 "Porcentaje de hogares de refugiados y  migrantes venezolanos con necesidad de agua de suficiente calidad" no se calculó para JNA 2024 

WA_D4 <- data |>
  group_by(id_hogar) |> 
  summarise(WA_D4 = max(WA_D4)) |>
  count(WA_D4) |>
  summarise(value = n[WA_D4 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

WA_D6 <- data |>
  group_by(id_hogar) |> 
  summarise(WA_D6 = max(WA_D6)) |>
  count(WA_D6) |>
  summarise(value = n[WA_D6 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

WA_D8 <- data |>
  group_by(id_hogar) |> 
  summarise(WA_D8 = max(WA_D8)) |>
  count(WA_D8) |>
  summarise(value = n[WA_D8 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

WA_D11 <- data |>
  group_by(id_hogar) |> 
  summarise(WA_D11 = max(WA_D11)) |>
  count(WA_D11) |>
  summarise(value = n[WA_D11 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

HT_D1 <- data |>
  mutate(
    HT_D1_review = ifelse(
      (
        HT_D1_Q1_first %in% c(1, 6) | 
          HT_D1_Q1_second %in% c(1, 6)) & 
        HT_D1_Q2 == "4", 1,
      ifelse(
        HT_D1_Q1_first %in% c(4, 96) & 
          HT_D2_Q1 %in% c(1, 2, 3, 4, 7, 8), 1, 0))) |>
  group_by(id_hogar) |>
  summarise(HT_D1_review = max(HT_D1_review)) |>
  count(HT_D1_review) |>
  summarise(value = n[HT_D1_review == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)


# HT_D2 Porcentaje de personas encuestadas o jefes de hogar que no tienen recursos, información, documentación requerida para viajar o que expresan miedo a utilizar medios de transporte por miedo a ser identificados por la policía o autoridades
# HT_D5 Limitaciones de acceso a servicios por falta de recursos/información sobre transporte en su lugar de destino

SHE_D1 <- data |>
  group_by(id_hogar) |> 
  summarise(SHE_D1 = max(SHE_D1)) |>
  count(SHE_D1) |>
  summarise(value = n[SHE_D1 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

#versión reducida
SHE_D1_rev <- data |>
  group_by(id_hogar) |> 
  mutate(SHE_D1_rev = ifelse((SHE_D1_Q1 %in% c("3","7","8","9","10","11", "98", "99") | 
                                SHE_D1_Q3_ALL == "0"), 1, 0)) |>
  summarise(SHE_D1_rev = max(SHE_D1_rev)) |>
  count(SHE_D1_rev) |>
  summarise(value = n[SHE_D1_rev == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

SHE_D2 <- data |>
  group_by(id_hogar) |> 
  summarise(SHE_D2 = max(SHE_D2)) |>
  count(SHE_D2) |>
  summarise(value = n[SHE_D2 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

SHE_D3 <- data |>
  group_by(id_hogar) |> 
  summarise(SHE_D3 = max(SHE_D3)) |>
  count(SHE_D3) |>
  summarise(value = n[SHE_D3 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

SHE_D4 <- data |>
  group_by(id_hogar) |> 
  summarise(SHE_D4 = max(SHE_D4)) |>
  count(SHE_D4) |>
  summarise(value = n[SHE_D4 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

PRO_D1 <- data |>
  group_by(id_hogar) |> 
  summarise(PRO_D1 = max(PRO_D1)) |>
  count(PRO_D1) |>
  summarise(value = n[PRO_D1 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

# PRO_D2 "% de hogares que enfrentan dificultades para el acceso seguro al país de destino" se eliminó para 2025 

PRO_D3 <- data |>
  group_by(id_hogar) |> 
  summarise(PRO_D3 = max(PRO_D3)) |>
  count(PRO_D3) |>
  summarise(value = n[PRO_D3 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

PRO_D5 <- data |>
  group_by(id_hogar) |> 
  summarise(PRO_D5 = max(PRO_D5)) |>
  count(PRO_D5) |>
  summarise(value = n[PRO_D5 == 1] / sum(n, na.rm = TRUE)) |>
  pull(value)

# GBV_D3: Porcentaje de personas refugiadas y migrantes que se sienten o han sentido inseguras/os en su localidad/ comunidad frente al riesgo de VBG
GBV_D3  <- data |>
  group_by(id_hogar) |> 
  summarise(GBV_D3_Q1  = max(GBV_D3_Q1)) |>
  count(GBV_D3_Q1) |>
  summarise(value = sum(n[GBV_D3_Q1 %in% c(4,5)] / sum(n, na.rm = TRUE))) |>
  pull(value)







library(tibble)

INT_D2 <- NA
EDU_D4 <- NA
WA_D3 <- NA
NUT_D1 <- NA 
NUT_D2 <- NA
NUT_D4 <- NA
NUT_D5 <- NA
NUT_D8 <- NA
NUT_D10 <- NA
NUT_D11 <- NA
HT_D2 <- NA
HT_D5 <- NA
CP_D1 <- NA 
CP_D3 <- NA
HTS_D1 <- NA
HTS_D2 <- NA
FS_D1 <- NA
FS_D2 <- NA
FS_D3 <- NA
FS_D4 <- NA


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
  "Indicador 2024" = c(INT_D1, INT_D2, INT_D3, INT_D4, INT_D5, 
                       EDU_D1, EDU_D2, EDU_D3, EDU_D4,
                       HE_D1, HE_D2,
                       WA_D1, WA_D2, WA_D3, WA_D4, WA_D6, WA_D8, WA_D11,
                       NUT_D1, NUT_D2, NUT_D4, NUT_D5, NUT_D8, NUT_D10, NUT_D11,
                       HT_D1, HT_D2, HT_D5,
                       SHE_D1, SHE_D2, SHE_D3, SHE_D4,
                       PRO_D1, PRO_D3, PRO_D4, PRO_D5,
                       CP_D1, CP_D3, 
                       GBV_D3, 
                       HTS_D1, HTS_D2,
                       FS_D1, FS_D2, FS_D3, FS_D4)
  )

write_xlsx(my_table, "JNA_Peru_2024.xlsx")


