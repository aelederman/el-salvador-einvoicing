# Load libraries
library(tidyverse)
library(here)
library(purrr)
library(stringr)
library(lubridate)
library(zoo)
library(ggplot2)
library(did)
library(scales)
library(patchwork)

# =============================================================
# Title: E-Invoicing in El Salvador: VAT Revenue & Compliance
# Author: Adam Lederman
# Description: Data prep, grouping, and event study estimation
# Last Updated: 7/21/2025
# =============================================================

# === LOAD METADATA ===
# Tax codes
impuesto_codes <- read_delim(
  here("data","raw", "impuesto.csv"),
  delim = ";", show_col_types = FALSE
) %>%
  rename(CODIMP = CODIGO) %>%
  mutate(CODIMP = as.character(CODIMP),
         NOMBRE = toupper(NOMBRE))

# Industry lookup
actividad <- read_delim(
  here("data","raw","actividadEconomica.csv"),
  delim = ";", show_col_types = FALSE
) %>%
  set_names(~ str_remove_all(.x, '"')) %>%
  rename(CODIGO = CODIGO, INDUSTRY_NAME = NOMBRE) %>%
  mutate(CODIGO = as.character(CODIGO)) %>%
  filter(str_length(CODIGO) == 2)

# === DATA LOADER ===
load_and_clean <- function(path) {
  read_delim(path, delim = ";", show_col_types = FALSE) %>%
    set_names(~ str_remove_all(.x, '"')) %>%
    mutate(across(c(ACTECO, CODIMP, MUNICIPIO, DEPARTAMENTO),
                  ~ str_trim(str_remove_all(as.character(.), '"')))) %>%
    mutate(
      ANIO         = as.integer(ANIO),
      MES          = as.integer(MES),
      ACTECO       = str_pad(ACTECO, width = 6, side = "left", pad = "0"),
      DEPARTAMENTO = str_pad(DEPARTAMENTO, width = 2, side = "left", pad = "0"),
      MUNICIPIO    = if_else(
        str_detect(MUNICIPIO, "^[0-9]{1,2}$"),
        str_c(DEPARTAMENTO, str_pad(MUNICIPIO, width = 2, side = "left", pad = "0")),
        str_pad(MUNICIPIO, width = 4, side = "left", pad = "0")
      ),
      VALOR = as.numeric(VALOR)
    )
}

# === LOAD & FILTER TAX DATA ===
# IMPORTANT: Comment out last file if not including 2025 data
files <- c(
  "DGT_IMPUESTO1616094232377-0112-2021.csv",
  "DGT_IMPUESTO1648495403637-012022-122022.csv",
  "DGT_IMPUESTO1707167152598-122023-122023.csv",
  "DGT_IMPUESTO1738685926586-122024-122024.csv"#,
  #"DGT_IMPUESTO1750427962354-052025-052025.csv"
) %>%
  map_chr(~ here("data","raw", .x))
raw_data <- map_dfr(files, load_and_clean)

# === FILTER FOR VAT REVENUE ONLY ===
vat_codes <- impuesto_codes %>%
  filter(str_detect(NOMBRE, "IVA")) %>%
  pull(CODIMP)

raw_data <- raw_data %>%
  filter(CODIMP %in% vat_codes)

# === FIRM PANEL SETUP ===
df_class <- raw_data %>%
  mutate(
    firm_id = str_c(ACTECO, MUNICIPIO, sep = "_"),
    code2d  = str_sub(ACTECO, 1, 2)
  ) %>%
  left_join(actividad, by = c("code2d" = "CODIGO")) %>%
  mutate(industry = coalesce(INDUSTRY_NAME, "Other"))

# === BUILD PANEL ===
panel_obs <- df_class %>%
  group_by(firm_id, ANIO, MES) %>%
  summarise(
    revenue0  = sum(VALOR, na.rm = TRUE),
    MUNICIPIO = first(MUNICIPIO),
    .groups   = "drop"
  ) %>%
  mutate(
    year_month = ANIO * 100 + MES,
    date       = ymd(paste(ANIO, MES, 1, sep = "-"))
  )

all_firms  <- unique(panel_obs$firm_id)
all_months <- sort(unique(panel_obs$year_month))

monthly_data <- expand_grid(firm_id = all_firms, year_month = all_months) %>%
  left_join(panel_obs, by = c("firm_id","year_month")) %>%
  left_join(
    panel_obs %>% distinct(firm_id, MUNICIPIO) %>% rename(MUNICIPIO_fm = MUNICIPIO),
    by = "firm_id"
  ) %>%
  mutate(
    ANIO        = floor(year_month/100),
    MES         = year_month %% 100,
    MUNICIPIO   = coalesce(MUNICIPIO, MUNICIPIO_fm),
    revenue0    = replace_na(revenue0, 0),
    remitted    = as.integer(revenue0 > 0),
    revenue     = revenue0 + 1,
    Y           = log(revenue),
    date        = ymd(paste(ANIO, MES, 1, sep = "-"))
  ) %>%
  select(-MUNICIPIO_fm)

# === PRE-TREATMENT GROUPS ===
baseline_end <- ymd("2023-01-01")

first_obs <- panel_obs %>%
  group_by(firm_id) %>%
  summarise(first_date = min(date), .groups = "drop")

firm_baseline <- panel_obs %>%
  filter(date <= baseline_end) %>%
  group_by(firm_id) %>%
  summarise(
    avg_rev_pre = mean(revenue0, na.rm = TRUE) * 12,
    months_obs  = sum(revenue0 > 0, na.rm = TRUE),
    .groups     = "drop"
  )

# Strict grouping logic, includes post-baseline entrants in data visualization
df_class <- df_class %>%
  left_join(first_obs, by = "firm_id") %>%
  left_join(firm_baseline, by = "firm_id") %>%
  mutate(
    size_group_strict = case_when(
      first_date  > baseline_end ~ "no_baseline",
      avg_rev_pre > 1e6         ~ "large",
      avg_rev_pre > 1e5         ~ "medium",
      TRUE                       ~ "small"
    )
  )

# Classic grouping logic, excludes post-baseline entrants for event study (safest)
classic_baseline <- panel_obs %>%
  filter(date <= baseline_end) %>%
  group_by(firm_id) %>%
  summarise(
    avg_rev_pre_classic = mean(revenue0, na.rm = TRUE) * 12,
    .groups = "drop"
  )
df_class <- df_class %>%
  left_join(classic_baseline, by = "firm_id") %>%
  mutate(
    size_group_classic = case_when(
      is.na(avg_rev_pre_classic)    ~ NA_character_,  # drop post-baseline firms
      avg_rev_pre_classic > 1e6     ~ "large",
      avg_rev_pre_classic > 1e5     ~ "medium",
      TRUE                           ~ "small"
    )
  )

# === ASSIGN 10-GROUPS BASED ON PRE-TREATMENT REVENUE ===
target_sizes <- tibble(
  group_id    = paste0("G", 1:10),
  target_size = c(233,239,249,345,657,701,704,4982,14286,15000)
)
firm_groups <- df_class %>%
  distinct(firm_id, avg_rev_pre) %>%
  filter(!is.na(avg_rev_pre)) %>%
  arrange(desc(avg_rev_pre)) %>%
  mutate(row_id = row_number()) %>%
  mutate(
    group_id = cut(
      row_id,
      breaks         = c(0, cumsum(target_sizes$target_size)),
      labels         = target_sizes$group_id,
      include.lowest = TRUE
    )
  ) %>%
  select(firm_id, group_id)

# === DEFINE GROUP TIMING ===
group_timing <- tibble(
  group_id      = paste0("G", 1:10),
  notified_date = ymd(c(
    "2023-02-01","2023-05-01","2023-08-01","2023-12-01",
    "2024-02-01","2024-05-01","2024-08-01","2024-12-01",
    "2025-02-01","2025-05-01"
  )),
  treat_date    = ymd(c(
    "2023-07-01","2023-10-01","2024-01-01","2024-04-01",
    "2024-07-01","2024-10-01","2025-01-15","2025-04-01",
    "2025-07-01","2025-10-01"
  ))
)

firm_info <- firm_groups %>% left_join(group_timing, by = "group_id")
df_class <- df_class %>% left_join(firm_info, by = "firm_id")

# === MERGE INTO PANEL AND JOIN GROUPS ===
monthly_data <- monthly_data %>%
  mutate(
    size_group_classic = df_class$size_group_classic[match(firm_id, df_class$firm_id)],
    size_group_strict  = df_class$size_group_strict[match(firm_id, df_class$firm_id)]
  ) %>%
  left_join(firm_info, by = "firm_id") %>%
  mutate(
    notified_ym     = year(notified_date)*100 + month(notified_date),
    treat_ym        = year(treat_date)*100 + month(treat_date),
    period          = as.integer(factor(year_month, sort(unique(year_month)))),
    notified_period = match(notified_ym, sort(unique(year_month))),
    treat_period    = match(treat_ym,    sort(unique(year_month))),
    id              = as.integer(factor(firm_id))
  )

# === 5) MUNICIPIO NAMES ===
municipio_lookup <- tribble(
  ~MUNICIPIO, ~municipio_name,
  # Dept 01
  "0101","Ahuachapán","0102","Apaneca","0103","Atiquizaya","0104","Concepción de Ataco",
  "0105","El Refugio","0106","Guaymango","0107","Jujutla","0108","San Francisco Menéndez",
  "0109","San Lorenzo","0110","San Pedro Puxtla","0111","Tacuba","0112","Turín",
  # Dept 02
  "0201","Candelaria de la Frontera","0202","Coatepeque","0203","Chalchuapa","0204","El Congo",
  "0205","El Porvenir","0206","Masahuat","0207","Metapán","0208","San Antonio Pajonal",
  "0209","San Sebastián Salitrillo","0210","Santa Ana","0211","Santa Rosa Guachipilín",
  "0212","Santiago de la Frontera","0213","Texistepeque",
  # Dept 03
  "0301","Acajutla","0302","Armenia","0303","Caluco","0304","Cuisnahuat","0305","Santa Isabel Ishuatán",
  "0306","Izalco","0307","Juayúa","0308","Nahuizalco","0309","Nahuilingo","0310","Salcoatitán",
  "0311","San Antonio del Monte","0312","San Julián","0313","Santa Catarina Masahuat",
  "0314","Santo Domingo de Guzmán","0315","Sonsonate","0316","Sonzacate",
  # Dept 04
  "0401","Agua Caliente","0402","Arcatao","0403","Azacualpa","0404","Citalá","0405","Comalapa",
  "0406","Concepción Quezaltepeque","0407","Chalatenango","0408","Dulce Nombre de María",
  "0409","El Carrizal","0410","El Paraíso","0411","La Laguna","0412","La Palma","0413","La Reina",
  "0414","Las Vueltas","0415","Nombre de Jesús","0416","Nueva Concepción","0417","Nueva Trinidad",
  "0418","Ojos de Agua","0419","Potonico","0420","San Antonio la Cruz","0421","San Antonio los Ranchos",
  "0422","San Fernando","0423","San Francisco Lempa","0424","San Francisco Morazán","0425","San Ignacio",
  "0426","San Isidro Labrador","0427","San José Cancasque","0428","San José las Flores","0429","San Luis del Carmen",
  "0430","San Miguel de Mercedes","0431","San Rafael","0432","Santa Rita","0433","Tejutla",
  # Dept 05
  "0501","Antiguo Cuscatlán","0502","Ciudad Arce","0503","Colón","0504","Comasagua","0505","Chiltiupán",
  "0506","Huizúcar","0507","Jayaque","0508","Jicalapa","0509","La Libertad","0510","Nuevo Cuscatlán",
  "0511","Santa Tecla","0512","Quezaltepeque","0513","Sacacoyo","0514","San José Villanueva",
  "0515","San Juan Opico","0516","San Matías","0517","San Pablo Tacachico","0518","Tamanique",
  "0519","Talnique","0520","Teotepeque","0521","Tepecoyo","0522","Zaragoza",
  # Dept 06
  "0601","Aguilares","0602","Apopa","0603","Ayutuxtepeque","0604","Cuscatancingo","0605","El Paisnal",
  "0606","Guazapa","0607","Ilopango","0608","Mejicanos","0609","Nejapa","0610","Panchimalco",
  "0611","Rosario de Mora","0612","San Marcos","0613","San Martín","0614","San Salvador",
  "0615","Santiago Texacuangos","0616","Santo Tomás","0617","Soyapango","0618","Tonacatepeque",
  "0619","Ciudad Delgado",
  # Dept 07
  "0701","Candelaria","0702","Cojutepeque","0703","El Carmen","0704","El Rosario","0705","Monte San Juán",
  "0706","Oratorio de Concepción","0707","San Bartolomé Perulapía","0708","San Cristóbal",
  "0709","San José Guayabal","0710","San Pedro Perulapán","0711","San Rafael Cedros","0712","San Ramón",
  "0713","Santa Cruz Analquito","0714","Santa Cruz Michapa","0715","Suchitoto","0716","Tenancingo",
  # Dept 08
  "0801","Cuyultitán","0802","El Rosario","0803","Jerusalén","0804","Mercedes La Ceiba","0805","Olocuilta",
  "0806","Paraíso de Osorio","0807","San Antonio Masahuat","0808","San Emigdio","0809","San Francisco Chinameca",
  "0810","San Juan Nonualco","0811","San Juan Talpa","0812","San Juan Tepezontes","0813","San Luis Talpa",
  "0814","San Miguel Tepezontes","0815","San Pedro Masahuat","0816","San Pedro Nonualco",
  "0817","San Rafael Obrajuelo","0818","Santa María Ostuma","0819","Santiago Nonualco","0820","Tapalhuaca",
  "0821","Zacatecoluca","0822","San Luis La Herradura",
  # Dept 09
  "0901","Cinquera","0902","Guacotecti","0903","Ilobasco","0904","Jutiapa","0905","San Isidro",
  "0906","Sensuntepeque","0907","Tejutepeque","0908","Victoria","0909","Villa Dolores",
  # Dept 10
  "1001","Apastepeque","1002","Guadalupe","1003","San Cayetano Istepeque","1004","Santa Clara",
  "1005","Santo Domingo","1006","San Esteban Catarina","1007","San Idelfonso","1008","San Lorenzo",
  "1009","San Sebastián","1010","San Vicente","1011","Tecoluca","1012","Tepetitán","1013","Verapaz",
  # Dept 11
  "1101","Alegría","1102","Berlín","1103","California","1104","Concepción Batres","1105","El Triunfo",
  "1106","Ereguayquín","1107","Estanzuelas","1108","Jiquilisco","1109","Jucuapa","1110","Jucuarán",
  "1111","Mercedes Umaña","1112","Nueva Granada","1113","Ozatlán","1114","Puerto El Triunfo",
  "1115","San Agustín","1116","San Buenaventura","1117","San Dionisio","1118","Santa Elena",
  "1119","San Francisco Javier","1120","Santa María","1121","Santiago de María","1122","Tecapán",
  "1123","Usulután",
  # Dept 12
  "1201","Carolina","1202","Ciudad Barrios","1203","Comacarán","1204","Chapeltique","1205","Chinameca",
  "1206","Chirilagua","1207","El Tránsito","1208","Lolotique","1209","Moncagua","1210","Nueva Guadalupe",
  "1211","Nuevo Edén de San Juan","1212","Quelepa","1213","San Antonio del Mosco","1214","San Gerardo",
  "1215","San Jorge","1216","San Luis de la Reina","1217","San Miguel","1218","San Rafael Oriente",
  "1219","Sesori","1220","Uluazapa",
  # Dept 13
  "1301","Arambala","1302","Cacaopera","1303","Corinto","1304","Chilanga","1305","Delicia de Concepción",
  "1306","El Divisadero","1307","El Rosario","1308","Gualococti","1309","Guatajiagua","1310","Joateca",
  "1311","Jocoaitique","1312","Jocoro","1313","Lolotiquillo","1314","Meanguera","1315","Osicala",
  "1316","Perquín","1317","San Carlos","1318","San Fernando","1319","San Francisco Gotera",
  "1320","San Isidro","1321","San Simón","1322","Sensembra","1323","Sociedad","1324","Torola",
  "1325","Yamabal","1326","Yoloayquín",
  # Dept 14
  "1401","Anamorós","1402","Bolívar","1403","Concepción de Oriente","1404","Conchagua",
  "1405","El Carmen","1406","El Sauce","1407","Intipucá","1408","La Unión","1409","Lislique",
  "1410","Meanguera del Golfo","1411","Nueva Esparta","1412","Pasaquina","1413","Polorós",
  "1414","San Alejo","1415","San José La Fuente","1416","Santa Rosa de Lima","1417","Yayantique",
  "1418","Yucuaiquín"
)

# Join municipio name into monthly_data
monthly_data <- monthly_data %>%
  left_join(municipio_lookup, by = "MUNICIPIO")

# === VAT REVENUE PLOT ===
library(scales)

# Compute total VAT revenue across all firms each month
rev_total <- monthly_data %>%
  group_by(date) %>%
  summarise(total_rev = sum(revenue0, na.rm = TRUE), .groups = "drop")

# Plot
vatrevenue <- ggplot(rev_total, aes(x = date, y = total_rev)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Total Monthly VAT Revenue",
    x     = "Date",
    y     = "Total Revenue (USD$)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

ggsave(
  filename = here("data","analysis","vatrevenue.png"),
  plot = vatrevenue,
  width = 6.5, height = 3.25, units = "in", dpi = 300
)

# total 2023 vat revenue
total_2023_rev <- rev_total %>%
  filter(year(date) == 2023) %>%
  summarise(total_rev = sum(total_rev, na.rm = TRUE))
cat("Total VAT Revenue in 2023: $", format(total_2023_rev$total_rev, big.mark = ",", scientific = FALSE), "\n")

# === DROP OCTOBER 2023 AS MISSING ===
monthly_data <- monthly_data %>%
  filter(!(ANIO == 2023 & MES == 10))

# === FIRM-LEVEL EVENT STUDIES ===
# Dynamic Event Study ATT of log(revenue)
att_global <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = TRUE,
  clustervars   = "id",
  anticipation  = 5
)
dyn_global <- aggte(att_global, type = "dynamic", min_e = -12, max_e = Inf)

# Print summary stats
summary(dyn_global)
write_csv(tidy(dyn_global),here("data","analysis","summary_attrevenue.csv"))

attrevenue <- ggdid(dyn_global) +
  labs(
    title = "Effect of E-Invoicing on VAT Revenue",
    x     = "Event Time (months)",
    y     = "Change in log VAT revenue"
  ) +
  theme_minimal()

ggsave(
  filename = here("data","analysis","attrevenue.png"),
  plot = attrevenue,
  width = 6.5, height = 3.25, units = "in", dpi = 300
)

# Dynamic Event Study ATT of probability remitting
att_rem_c <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = TRUE,
  clustervars   = "id"
)
dyn_rem_c <- aggte(att_rem_c, type = "dynamic", min_e = -12, max_e = 9)

# Print summary stats
summary(dyn_rem_c)
write_csv(tidy(dyn_rem_c), here("data","analysis","summary_attremittance.csv"))

attremittance <- ggdid(dyn_rem_c) +
  labs(
    title = "Effect of E-Invoicing on VAT Filing Probability",
    x     = "Event Time (months)",
    y     = "Change in VAT filing probability"
  ) +
  theme_minimal()

ggsave(
  filename = here("data","analysis","attremittance.png"),
  plot = attremittance,
  width = 6.5, height = 3.25, units = "in", dpi = 300
)

# Total Monthly VAT Revenue by Group
tool_data <- monthly_data %>%
  mutate(
    group_id      = firm_info$group_id[match(firm_id, firm_info$firm_id)],
    notified_date = firm_info$notified_date[match(firm_id, firm_info$firm_id)],
    treat_date    = firm_info$treat_date[match(firm_id, firm_info$firm_id)]
  ) %>%
  filter(!is.na(group_id)) %>%
  group_by(group_id, date) %>%
  summarise(
    total_rev     = sum(revenue0, na.rm = TRUE),
    notified_date = first(notified_date),
    treat_date    = first(treat_date),
    .groups       = "drop"
  )

monthlyvat <- ggplot(tool_data, aes(x = date, y = total_rev)) +
  geom_line() +
  geom_vline(aes(xintercept = notified_date), linetype = "dashed") +
  geom_vline(aes(xintercept = treat_date),    linetype = "solid") +
  labs(
    title    = "Monthly VAT Revenue by Group",
    subtitle = "Dashed = notification date; Solid = treatment date",
    x        = "Date",
    y        = "Total Revenue (USD$)"
  ) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~group_id, scales = "free_y", ncol = 2, drop = FALSE) +
  theme_minimal()

ggsave(
  filename = here("data","analysis","monthlyvat.png"),
  plot = monthlyvat,
  width = 6.5, height = 3.25, units = "in", dpi = 300
)

# === INDUSTRY-SPECIFIC EVENT STUDIES ===

# Prepare data
monthly_data <- monthly_data %>%
  left_join(df_class %>% distinct(firm_id, industry), by = "firm_id")

# Make sure industry and group_id are joined into df_class
firm_counts <- df_class %>%
  filter(!is.na(group_id), !is.na(industry)) %>%
  group_by(group_id, industry) %>%
  summarise(
    n_firms = n_distinct(firm_id),
    .groups = "drop"
  ) %>%
  arrange(group_id, desc(n_firms))


# Log-Revenue by Top-20 Industries
top20_inds <- df_class %>%
  count(industry, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(industry)

for (ind in top20_inds) {
  df_i_cl <- monthly_data %>%
    filter(!is.na(size_group_classic), industry == ind)
  att_cl <- att_gt(
    yname         = "Y",
    tname         = "period",
    idname        = "id",
    gname         = "treat_period",
    data          = df_i_cl,
    control_group = "notyettreated",
    panel         = TRUE,
    clustervars   = "id"
  )
  dyn_cl <- aggte(att_cl, type = "dynamic", min_e = -12, max_e = Inf)
  p <- ggdid(dyn_cl) +
    labs(
      title = paste("Log Revenue ATT classic –", ind),
      x     = "Event Time (months)",
      y     = "ATT (log revenue)"
    ) +
    theme_minimal()
  ggsave(
    filename = here("data","analysis",
                    paste0("LogRevenue_ATT_", str_replace_all(ind, "[^A-Za-z0-9]", "_"), ".png")),
    plot     = p,
    width    = 6.5, height = 3.25, units = "in", dpi = 300
  )
}

# === Pr(Remitting) by Top-20 Industries ===
top20_inds <- df_class %>%
  count(industry, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(industry)

for (ind in top20_inds) {
  df_i_remit <- monthly_data %>%
    filter(!is.na(size_group_classic), industry == ind)
  
  att_remit <- att_gt(
    yname         = "remitted",
    tname         = "period",
    idname        = "id",
    gname         = "treat_period",
    data          = df_i_remit,
    control_group = "notyettreated",
    panel         = TRUE,
    clustervars   = "id"
  )
  
  dyn_remit <- aggte(att_remit, type = "dynamic", min_e = -12, max_e = Inf)
  
  p_remit <- ggdid(dyn_remit) +
    labs(
      title = paste("ATT on Pr(Remitting) –", ind),
      x     = "Event Time (months)",
      y     = "ATT Pr(remitted)"
    ) +
    theme_minimal()
  
  ggsave(
    filename = here("data","analysis",
                    paste0("RemitProb_ATT_", str_replace_all(ind, "[^A-Za-z0-9]", "_"), ".png")),
    plot     = p_remit,
    width    = 6.5, height = 3.25, units = "in", dpi = 300
  )
}

# === GROUP-INDUSTRY-MONTH EVENT STUDIES ===
group_industry_data <- monthly_data %>%
  filter(!is.na(group_id), !is.na(industry)) %>%
  group_by(group_id, industry, date) %>%
  summarise(total_rev = sum(revenue0, na.rm = TRUE),
            prop_remit = mean(remitted, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(
    Y_agg = log(total_rev + 1),
    month = as.integer(factor(date, levels = sort(unique(date))))
  )

# Add treatment period info
group_timing_trim <- group_timing %>%
  mutate(period = as.integer(factor(treat_date, levels = sort(unique(monthly_data$date))))) %>%
  rename(g = period)

agg_data <- group_industry_data %>%
  left_join(group_timing_trim %>% select(group_id, g), by = "group_id") %>%
  mutate(id = as.integer(factor(paste(group_id, industry))))

# Dynamic ATT on log revenue
agg_att <- att_gt(
  yname = "Y_agg",
  tname = "month",
  idname = "id",
  gname = "g",
  data = agg_data,
  control_group = "notyettreated",
  panel = TRUE,
  clustervars = "id",
  anticipation = 5
)
agg_dyn <- aggte(agg_att, type = "dynamic", min_e = -12, max_e = Inf)
summary(agg_dyn)
write_csv(tidy(agg_dyn), here("data","analysis","summary_grouprevenue.csv"))

grouprevenue <- ggdid(agg_dyn) +
  labs(
    title = "ATT on Log Aggregated VAT Revenue (Group-Industry-Month)",
    x = "Event Time (Months)",
    y = "ATT (log total revenue)"
  ) +
  theme_minimal()

ggsave(
  filename = here("data","analysis","grouprevenue.png"),
  plot = grouprevenue,
  width = 6.5, height = 3.25, units = "in", dpi = 300
)

# Dynamic ATT on Pr(remitting)
agg_remit_att <- att_gt(
  yname = "prop_remit",
  tname = "month",
  idname = "id",
  gname = "g",
  data = agg_data,
  control_group = "notyettreated",
  panel = TRUE,
  clustervars = "id",
  anticipation = 5
)
agg_remit_dyn <- aggte(agg_remit_att, type = "dynamic", min_e = -12, max_e = Inf)
summary(agg_remit_dyn)
write_csv(tidy(agg_remit_dyn), here("data","analysis","summary_groupremittance.csv"))

groupremitting <- ggdid(agg_remit_dyn) +
  labs(
    title = "ATT on Probability of Remitting (Group-Industry-Month)",
    x = "Event Time (Months)",
    y = "ATT Pr(remitted)"
  ) +
  theme_minimal()

ggsave(
  filename = here("data","analysis","groupremittance.png"),
  plot = groupremitting,
  width = 6.5, height = 3.25, units = "in", dpi = 300
)

# === Log-Revenue by Top-50 Municipalities ===
# Identify top 50 municipalities by number of firms
top50_muns <- df_class %>%
  filter(!is.na(MUNICIPIO)) %>%
  count(MUNICIPIO, sort = TRUE) %>%
  slice_head(n = 50)

# Loop through each top municipality with names
# for (i in seq_len(nrow(top50_muns))) {
#   mun_code <- top50_muns$MUNICIPIO[i]
#   mun_name <- municipio_lookup %>%
#     filter(MUNICIPIO == mun_code) %>%
#     pull(municipio_name)
#   df_mun_cl <- monthly_data %>%
#     filter(!is.na(size_group_classic), MUNICIPIO == mun_code)
#   att_cl <- att_gt(
#     yname         = "Y",
#     tname         = "period",
#     idname        = "id",
#     gname         = "treat_period",
#     data          = df_mun_cl,
#     control_group = "notyettreated",
#     panel         = TRUE,
#     clustervars   = "id"
#   )
#   dyn_cl <- aggte(att_cl, type = "dynamic", min_e = -12, max_e = Inf)
#   p <- ggdid(dyn_cl) +
#     labs(
#       title = paste("Log Revenue ATT –", mun_name),
#       x     = "Event Time (months)",
#       y     = "ATT (log revenue)"
#     ) +
#     theme_minimal()
#   ggsave(
#     filename = here("data", "analysis",
#                     paste0("LogRevenue_ATT_Municipio_", mun_code, ".png")),
#     plot     = p,
#     width    = 6.5, height = 3.25, units = "in", dpi = 300
#   )
# }


# === Probability of Remitting by Top-50 Municipalities ===
# for (i in seq_len(nrow(top50_muns))) {
#   mun_code <- top50_muns$MUNICIPIO[i]
#   mun_name <- municipio_lookup %>%
#     filter(MUNICIPIO == mun_code) %>%
#     pull(municipio_name)
#   df_mun_cl <- monthly_data %>%
#     filter(!is.na(size_group_classic), MUNICIPIO == mun_code)
#   att_rem <- att_gt(
#     yname         = "remitted",
#     tname         = "period",
#     idname        = "id",
#     gname         = "treat_period",
#     data          = df_mun_cl,
#     control_group = "notyettreated",
#     panel         = TRUE,
#     clustervars   = "id"
#   )
#   dyn_rem <- aggte(att_rem, type = "dynamic", min_e = -12, max_e = Inf)
#   p_remit <- ggdid(dyn_rem) +
#     labs(
#       title = paste("ATT on Pr(Remitting) –", mun_name),
#       x     = "Event Time (months)",
#       y     = "ATT Pr(remitted)"
#     ) +
#     theme_minimal()
#   ggsave(
#     filename = here("data", "analysis",
#                     paste0("RemitProb_ATT_Municipio_", mun_code, ".png")),
#     plot     = p_remit,
#     width    = 6.5, height = 3.25, units = "in", dpi = 300
#   )
# }

