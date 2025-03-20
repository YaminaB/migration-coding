#install.packages("pacman")
library(pacman)
p_load(remotes, dplyr, ggplot2, scales, viridis, flextable, officer, readr, RColorBrewer, readxl, readODS, tidyr)
#remotes::install_github("bennettoxford/opencodes")
library(opencodes)

# Create df from the snomed codes 
snomed_usage <- opencodes::snomed_usage

#codelist_out <- codelist |> 
#  filter(!(code %in% snomed_usage$snomed_code))

# general migrant ######

general_migrant_codelist <- data.frame(get_codelist("user/YaminaB/migration-status/5fb4000d"))

# Filter codelist to codes included in SNOMED-CT data (i.e. usage >0)
general_migrant_in <- general_migrant_codelist |>
  filter(code %in% snomed_usage$snomed_code)
general_migrant_codelist_table <- flextable(general_migrant_in) |>
  autofit() |>
  set_table_properties(layout = "autofit")
general_migrant_codelist_table <- read_docx() %>% 
  body_add_flextable(general_migrant_codelist_table) 
print(general_migrant_codelist_table, target = "output/general_migrant_codelist_table.docx")

general_migrant_data <- snomed_usage |> 
  filter(snomed_code %in% general_migrant_codelist$code) 

general_migrant_total <- general_migrant_data |>
  summarise(usage = sum(usage), .groups = "drop") |>
  mutate(code_type = "All migration-related codes")

general_migrant_top5 <- general_migrant_data |>
  group_by(snomed_code, description) |>
  summarise(usage = sum(usage), .groups = "drop") |>
  mutate(percent_usage = round(100*(usage/ sum(usage)),2)) |>
  slice_max(usage, n = 5)

general_migrant_top5 <- flextable(general_migrant_top5) |>
  autofit() |>
  set_table_properties(layout = "autofit")
general_migrant_top5_doc <- read_docx() %>% 
  body_add_flextable(general_migrant_top5) 
print(general_migrant_top5_doc, target = "output/general_migrant_top5.docx")

for_plot <- general_migrant_data |>
  group_by(start_date, end_date) |>
  summarise(usage = sum(usage), .groups = "drop") %>%
  mutate(code_type = "All migration-related SNOMED-CT codes")

all_migration_codes_percentage_increase <- for_plot %>%
  mutate(perc_increase = (usage-first(usage))/first(usage) *100)

## Country of birth ----

cob_codelist <- data.frame(get_codelist("user/YaminaB/born-outside-the-uk/226b7482"))

cob_in <- cob_codelist |>
  filter(code %in% snomed_usage$snomed_code)
cob_codelist_table <- flextable(cob_in) |>
  autofit() |>
  set_table_properties(layout = "autofit")
cob_codelist_table <- read_docx() %>% 
  body_add_flextable(cob_codelist_table) 
print(cob_codelist_table, target = "output/cob_codelist_table.docx")

cob_data <- snomed_usage |> 
  filter(snomed_code %in% cob_codelist$code) 

cob_total <- cob_data |>
  summarise(usage = sum(usage), .groups = "drop") |>
  mutate(code_type = "Country of birth codes")

cob_top5 <- cob_data |>
  group_by(snomed_code, description) |>
  summarise(usage = sum(usage), .groups = "drop") |>
  mutate(percent_usage = round(100*(usage/ sum(usage)),2)) |>
  slice_max(usage, n = 5)

cob_top5 <- flextable(cob_top5) |>
  autofit() |>
  set_table_properties(layout = "autofit")
cob_top5_doc <- read_docx() %>% 
  body_add_flextable(cob_top5) 
print(cob_top5_doc, target = "output/Tables_figures/cob_top5.docx")

for_plot_cob <- cob_data |>
  group_by(start_date, end_date) |>
  summarise(usage = sum(usage), .groups = "drop") %>%
  mutate(code_type = "Country of birth codes")

# Interpreter needed -----

interpreter_codelist <- data.frame(get_codelist("user/YaminaB/interpreter-required/373ac0a3"))

interpreter_in <- interpreter_codelist |>
  filter(code %in% snomed_usage$snomed_code)
interpreter_codelist_table <- flextable(interpreter_in) |>
  autofit() |>
  set_table_properties(layout = "autofit")
interpreter_codelist_table <- read_docx() %>% 
  body_add_flextable(interpreter_codelist_table) 
print(interpreter_codelist_table, target = "output/interpreter_codelist_table.docx")

interpreter_data <- snomed_usage |> 
  filter(snomed_code %in% interpreter_codelist$code) 

interpreter_total <- interpreter_data |>
  summarise(usage = sum(usage), .groups = "drop") |>
  mutate(code_type = "Interpreter codes")

interpreter_top5 <- interpreter_data |>
  group_by(snomed_code, description) |>
  summarise(usage = sum(usage), .groups = "drop") |>
  mutate(percent_usage = round(100*(usage/ sum(usage)),2)) |>
  slice_max(usage, n = 5)

interpreter_top5 <- flextable(interpreter_top5) |>
  autofit() |>
  set_table_properties(layout = "autofit")
interpreter_top5_doc <- read_docx() %>% 
  body_add_flextable(interpreter_top5) 
print(interpreter_top5_doc, target = "output/interpreter_top5.docx")

for_plot_interpreter <- interpreter_data |>
  group_by(start_date, end_date) |>
  summarise(usage = sum(usage), .groups = "drop") %>%
  mutate(code_type = "Interpreter-related codes")

# Refugees and asylum seekers -----

refugee_codelist <- data.frame(get_codelist("user/YaminaB/asylum-seeker-or-refugee/6266c003"))

refugee_in <- refugee_codelist |>
  filter(code %in% snomed_usage$snomed_code)
refugee_codelist_table <- flextable(refugee_in) |>
  autofit() |>
  set_table_properties(layout = "autofit")
refugee_codelist_table <- read_docx() %>% 
  body_add_flextable(refugee_codelist_table) 
print(refugee_codelist_table, target = "output/refugee_codelist_table.docx")

refugee_data <- snomed_usage |> 
  filter(snomed_code %in% refugee_codelist$code) 

refugee_total <- refugee_data |>
  summarise(usage = sum(usage), .groups = "drop") |>
  mutate(code_type = "Refugee and asylum-related codes")

refugee_top5 <- refugee_data |>
  group_by(snomed_code, description) |>
  summarise(usage = sum(usage), .groups = "drop") |>
  mutate(percent_usage = round(100*(usage/ sum(usage)),2)) |>
  slice_max(usage, n = 5)

refugee_top5 <- flextable(refugee_top5) |>
  autofit() |>
  set_table_properties(layout = "autofit")
refugee_top5_doc <- read_docx() %>% 
  body_add_flextable(refugee_top5) 
print(refugee_top5_doc, target = "output/refugee_top5.docx")

for_plot_refugee <- refugee_data |>
  group_by(start_date, end_date) |>
  summarise(usage = sum(usage), .groups = "drop") %>%
  mutate(code_type = "Refugee or asylum-seeker codes")

# Immigration legal status -----

legal_status_codelist <- data.frame(get_codelist("user/YaminaB/uk-visa/2d9519a3"))

legal_status_in <- legal_status_codelist |>
  filter(code %in% snomed_usage$snomed_code)
legal_status_codelist_table <- flextable(legal_status_in) |>
  autofit() |>
  set_table_properties(layout = "autofit")
legal_status_codelist_table <- read_docx() %>% 
  body_add_flextable(legal_status_codelist_table) 
print(legal_status_codelist_table, target = "output/legal_status_codelist_table.docx")

legal_status_data <- snomed_usage |> 
  filter(snomed_code %in% legal_status_codelist$code) 

legal_status_total <- legal_status_data |>
  summarise(usage = sum(usage), .groups = "drop") |>
  mutate(code_type = "Immigration legal status codes")

legal_status_top5 <- legal_status_data |>
  group_by(snomed_code, description) |>
  summarise(usage = sum(usage), .groups = "drop") |>
  mutate(percent_usage = round(100*(usage/ sum(usage)),2)) |>
  slice_max(usage, n = 5)

legal_status_top5 <- flextable(legal_status_top5) |>
  autofit() |>
  set_table_properties(layout = "autofit")
legal_status_top5_doc <- read_docx() %>% 
  body_add_flextable(legal_status_top5) 
print(legal_status_top5_doc, target = "output/legal_status_top5.docx")

for_plot_legalstatus <- legal_status_data |>
  group_by(start_date, end_date) |>
  summarise(usage = sum(usage), .groups = "drop") %>%
  mutate(code_type = "Immigration legal status codes")

# Add immigration data from lamis: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/localareamigrationindicatorsunitedkingdom
# for international long-term migration: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/datasets/longterminternationalimmigrationemigrationandnetmigrationflowsprovisional 

ons <- read_csv("long_term_international_migration_UK_ONS.csv", col_types = cols(population = col_double()))

for_plot_ons <- for_plot %>%
  select(start_date, end_date) %>%
  mutate(code_type = "Immigration to the UK (ONS)")

for_plot_ons$year <- as.numeric(format(for_plot_ons$end_date, "%Y"))

for_plot_ons <- for_plot_ons |>
  left_join(ons, by = "year") |>
  select(-c(year, year_full)) |>
  rename("usage" = "population")

#for_plot_ons$usage <- as.numeric(for_plot_ons$usage)

## Combine data ----

combined_data <- rbind(for_plot, for_plot_cob, for_plot_interpreter, for_plot_refugee, for_plot_legalstatus, for_plot_ons) 
combined_data$usage[is.na(combined_data$usage)] <- 0  # or use mean(combined_data$usage, na.rm = TRUE)
combined_data$code_type <- factor(combined_data$code_type, levels = 
                                    c("Immigration to the UK (ONS)", 
                                      "All migration-related codes", 
                                      "Country of birth codes", 
                                      "Immigration legal status codes", 
                                      "Refugee or asylum-seeker codes", 
                                      "Interpreter-related codes"))


combined_totals <- rbind(general_migrant_total, cob_total, interpreter_total, refugee_total, legal_status_total)
combined_totals_table <- flextable(combined_totals) |>
  autofit() |>
  set_table_properties(layout = "autofit")
combined_totals_table <- read_docx() %>% 
  body_add_flextable(combined_totals_table) 
print(combined_totals_table, target = "output/combined_totals_table.docx")

## Plot data ----

plot <- ggplot(combined_data, aes(x = end_date, y = usage, color = code_type)) +
  geom_line(aes(linewidth = code_type, linetype = code_type)) + 
  geom_point() +  
  #scale_color_viridis_d() +  
  scale_color_brewer(palette = "Dark2") +
  scale_linewidth_manual(values = c("Immigration to the UK (ONS)" = 1.0, 
                                 "All migration-related codes" = 0.5, 
                                 "Country of birth codes" = 0.5, 
                                 "Immigration legal status codes" = 0.5, 
                                 "Refugee or asylum-seeker codes" = 0.5, 
                                 "Interpreter-related codes" = 0.5))+
  scale_linetype_manual(values = c("Immigration to the UK (ONS)" = "dashed", 
                                   "All migration-related codes" = "solid", 
                                   "Country of birth codes" = "solid", 
                                   "Immigration legal status codes" = "solid", 
                                   "Refugee or asylum-seeker codes" = "solid", 
                                   "Interpreter-related codes" = "solid")) +
  labs(
    title = " ",
    x = "Date",
    y = "Usage/population size"
  ) +
  scale_x_date(
    breaks = seq(from = min(combined_data$end_date), to = max(combined_data$end_date), by = "1 year"),  
    labels = date_format("%Y") 
  ) +
  scale_y_continuous(limits = c(0, max(combined_data$usage) + 10),  
                     breaks = seq(0, max(combined_data$usage) + 500000, by = 500000),  
                     labels = label_comma()) +  
  theme_bw() + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position.inside = c(0.2, 0.8),
    legend.title = element_blank()
    
  )

plot

ggsave("output/migration_code_usage.png", plot = plot, width = 8, height = 6, dpi = 300)


# All SNOMED-CT code usage -----

for_plot_allcodes <- snomed_usage |>
  group_by(start_date, end_date) |>
  summarise(usage = sum(usage), .groups = "drop") 

all_snomed_percentage_increase <- for_plot_allcodes %>%
  mutate(perc_increase = (usage-first(usage))/first(usage) *100) %>%
  mutate(code_type = "All SNOMED-CT codes")

plot_allcodes <- ggplot(for_plot_allcodes, aes(x = end_date, y = usage)) +
  geom_line(color = "black") + 
  geom_point(color = "black") +  
  scale_color_viridis_d() +  
  labs(
    title = " ",
    x = "Date",
    y = "Usage"
  ) +
  scale_x_date(
    breaks = seq(from = min(for_plot_allcodes$end_date), to = max(for_plot_allcodes$end_date), by = "1 year"),  
    labels = date_format("%Y") 
  ) +
  scale_y_continuous(limits = c(0, max(for_plot_allcodes$usage) + 10),  
                     labels = label_comma()) +  
  theme_bw() + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.title = element_blank()
  )

plot_allcodes

ggsave("output/allsnomed_code_usage.png", plot = plot_allcodes, width = 8, height = 6, dpi = 300)

# Comparison with overall SNOMED-CT code usage ----

percentage_increase_combined <- rbind(all_snomed_percentage_increase, all_migration_codes_percentage_increase)

percentage_increase_plot <- ggplot(percentage_increase_combined, aes(x = end_date, y = perc_increase, fill = code_type)) +
  geom_bar(stat = "identity", position = "dodge") +  # "dodge" places bars side by side
  labs(
    x = "Year",
    y = "Percentage increase"
  ) +
  scale_x_date(
    breaks = seq(from = min(for_plot_allcodes$end_date), to = max(for_plot_allcodes$end_date), by = "1 year"),  
    labels = date_format("%Y") 
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),  
        axis.title.y = element_text(margin = margin(r = 10)),) 

percentage_increase_plot
ggsave("output/percentage_increase_plot.png", plot = percentage_increase_plot, width = 8, height = 6, dpi = 300)



#comparison <- data.frame(start_date = for_plot_allcodes$start_date,
                         end_date = for_plot_allcodes$end_date,
                         all_snomed = for_plot_allcodes$usage,
                         migration_snomed = for_plot$usage)
# 
# comparison <- comparison |>
#   mutate(percent = round((migration_snomed/all_snomed * 100),4))
# 
# plot_comparison <- ggplot(comparison, aes(x = end_date, y = percent)) +
#   geom_line(color = "black") + 
#   geom_point(color = "black") +  
#   scale_color_viridis_d() +  
#   labs(
#     title = " ",
#     x = "Date",
#     y = "Percentage of all SNOMED-CT coding"
#   ) +
#   scale_x_date(
#     breaks = seq(from = min(comparison$end_date), to = max(comparison$end_date), by = "1 year"),  
#     labels = date_format("%Y") 
#   ) +
#   scale_y_continuous(#limits = c(0, max(comparison$percent) + 2),  
#                      labels = label_comma()) +  
#   theme_bw() + 
#   theme(
#     axis.title.x = element_text(margin = margin(t = 10)),  
#     axis.title.y = element_text(margin = margin(r = 10)),
#     legend.title = element_blank()
#   )
# 
# plot_comparison
# 
# ggsave("output/comparison_plot.png", plot = plot_comparison, width = 8, height = 6, dpi = 300)

# Asylum applications, initial positive decisions, and resettled refugees (incl. Afghan resettlement scheme) - accessed 19 March 2025
# https://assets.publishing.service.gov.uk/media/67bc506cb3a80ad63e782c90/asylum-applications-datasets-dec-2024.xlsx
# via this main page: https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#asylum-and-resettlement 

url <- "https://assets.publishing.service.gov.uk/media/67bc506cb3a80ad63e782c90/asylum-applications-datasets-dec-2024.xlsx"
download.file(url, destfile = "temp.xlsx", mode = "wb")

# asylum applications
sheet_name <- "Data - Asy_D01" # asylum applications
asylum_applications_data <- read_excel("temp.xlsx", sheet = sheet_name, skip = 1) 
asylum_applications_data <- na.omit(asylum_applications_data)

asylum_applications_yeartotals <- asylum_applications_data |>
  group_by(Year) |>
  summarise(total = sum(Applications), .groups = "drop") |>
  mutate(group = "Asylum applications") |>
  filter(Year >= 2012) |>
  rename(year = Year)

# resettled refugees (excluding Hong Kong and Ukraine schemes)
sheet_name <- "Data - Asy_D02"
resettled_refugee_noHKorUkr_data <- read_excel("temp.xlsx", sheet = sheet_name, skip = 1) 
resettled_refugee_noHKorUkr_data <- na.omit(resettled_refugee_noHKorUkr_data)

resettled_refugee_noKHorUKr_yeartotals <- resettled_refugee_noHKorUkr_data |>
  filter(`Case type` != "Asylum Case") |>
  group_by(Year) |>
  summarise(total = sum(Decisions), .groups = "drop") |>
  mutate(group = "Resettled refugees (excluding Hong Kong and Ukraine schemes") |>
  filter(Year >= 2012) |>
  rename(year = Year)

# Ukraine schemes and BNO (Hong Kong) visas - accessed 19 March 2025
# From: https://assets.publishing.service.gov.uk/media/67bc514298ea2db44faddd4f/asylum-summary-dec-2024-tables.ods 

url <- "https://assets.publishing.service.gov.uk/media/67bc514298ea2db44faddd4f/asylum-summary-dec-2024-tables.ods"
download.file(url, destfile = "temp.xlsx", mode = "wb")
sheet_name <- "Asy_11" 
bno_ukraine_data <- read_ods("temp.xlsx", sheet = sheet_name, skip = 1) 

bno_ukraine_data <- bno_ukraine_data[c(6,9),1:(ncol(bno_ukraine_data)-3)]

bno_ukraine_data_yeartotals <- bno_ukraine_data %>%
  pivot_longer(cols = `2010`:`2024`,             
               names_to = "year",
               values_to = "total") %>%
  rename(group = Year) %>%
  filter(year >=2012) %>%
  mutate(group = ifelse(group == "Total BN(O) Hong Kong visa grants [Note 5](subset of Country-specific routes)", "BNO Hong Kong visas", group)) %>%
  mutate(group = ifelse(group == "Total Ukraine Visa grants [Note 6](subset of Country-specific routes)", "Ukraine visa schemes", group))

# Other visa types - accessed 19 March 2025
# From https://assets.publishing.service.gov.uk/media/67bc8251d157fd4b79addd86/entry-clearance-visa-outcomes-datasets-dec-2024.xlsx
# From this webpage: https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#entry-clearance-visas-granted-outside-the-uk

url <- "https://assets.publishing.service.gov.uk/media/67bc8251d157fd4b79addd86/entry-clearance-visa-outcomes-datasets-dec-2024.xlsx"
download.file(url, destfile = "temp.xlsx", mode = "wb")
sheet_name <- "Data_Vis_D02" 
entry_clearance_visa_data <- read_excel("temp.xlsx", sheet = sheet_name, skip = 1) 
entry_clearance_visa_data <- na.omit(entry_clearance_visa_data)

## Study

study_visas_yeartotals <- entry_clearance_visa_data |>
  filter(`Visa type group` == "Study") |>
  group_by(Year) |>
  summarise(total = sum(Decisions), .groups = "drop") |>
  mutate(group = "Study visa status") |>
  filter(Year >= 2012) |>
  rename(year = Year)

## Work visas 

work_visas_yeartotals <- entry_clearance_visa_data |>
  filter(`Visa type group` == "Work") |>
  group_by(Year) |>
  summarise(total = sum(Decisions), .groups = "drop") |>
  mutate(group = "Work visa status") |>
  filter(Year >= 2012) |>
  rename(year = Year)

## Family visas

family_visas_yeartotals <- entry_clearance_visa_data |>
  filter(`Visa type group` == "Family") |>
  group_by(Year) |>
  summarise(total = sum(Decisions), .groups = "drop") |>
  mutate(group = "Family visa status") |>
  filter(Year >= 2012) |>
  rename(year = Year)

# Combine asylum and refugee related visa types 

all_refugee_asylum_yeartotals <- rbind(asylum_applications_yeartotals, 
                            resettled_refugee_noKHorUKr_yeartotals,
                            bno_ukraine_data_yeartotals)
all_refugee_asylum_yeartotals <- all_refugee_asylum_yeartotals %>%
  group_by(year) %>%
  summarise(total = sum(total)) %>%
  mutate(group = "Asylum and refugee status")

# Combine visa types 

all_visa_types_totals <- rbind(study_visas_yeartotals, 
                               work_visas_yeartotals, 
                               family_visas_yeartotals, 
                               all_refugee_asylum_yeartotals)

all_visa_types_totals$year <- as.numeric(all_visa_types_totals$year)

plot_immigration_data <- ggplot(all_visa_types_totals, aes(x = year, y = total, color = group)) +
  geom_line(na.rm = TRUE) + 
  geom_point() +   
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = " ",
    x = "Date",
    y = "Number of individuals"
  ) +
  scale_y_continuous(limits = c(0, max(all_visa_types_totals$total) + 10),  
                     breaks = seq(0, max(all_visa_types_totals$total) + 100000, by = 100000),  
                     labels = label_comma()) +  
  scale_x_continuous(
    breaks = seq(from = min(all_visa_types_totals$year), to = max(all_visa_types_totals$year), by = 1)
  ) +
  theme_bw() + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position.inside = c(0.2, 0.8),
    legend.title = element_blank()
    
  )

plot_immigration_data

ggsave("output/visa_type_plot.png", plot = plot_immigration_data, width = 8, height = 6, dpi = 300)



