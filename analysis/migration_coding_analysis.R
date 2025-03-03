install.packages("pacman")
library(pacman)
p_load(remotes, dplyr, ggplot2, scales, viridis, flextable, officer, readr, RColorBrewer)
remotes::install_github("ebmdatalab/codeusage")
library(opencodes)

# Create df from the snomed codes 
snomed_usage <- opencodes::snomed_usage

codelist_out <- codelist |> 
  filter(!(code %in% snomed_usage$snomed_code))

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
print(general_migrant_codelist_table, target = "C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/general_migrant_codelist_table.docx")

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
print(general_migrant_top5_doc, target = "C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/general_migrant_top5.docx")

for_plot <- general_migrant_data |>
  group_by(start_date, end_date) |>
  summarise(usage = sum(usage), .groups = "drop") %>%
  mutate(code_type = "All migration-related codes")

## Country of birth ----

cob_codelist <- data.frame(get_codelist("user/YaminaB/born-outside-the-uk/226b7482"))

cob_in <- cob_codelist |>
  filter(code %in% snomed_usage$snomed_code)
cob_codelist_table <- flextable(cob_in) |>
  autofit() |>
  set_table_properties(layout = "autofit")
cob_codelist_table <- read_docx() %>% 
  body_add_flextable(cob_codelist_table) 
print(cob_codelist_table, target = "C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/cob_codelist_table.docx")

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
print(cob_top5_doc, target = "C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/cob_top5.docx")

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
print(interpreter_codelist_table, target = "C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/interpreter_codelist_table.docx")

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
print(interpreter_top5_doc, target = "C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/interpreter_top5.docx")

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
print(refugee_codelist_table, target = "C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/refugee_codelist_table.docx")

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
print(refugee_top5_doc, target = "C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/refugee_top5.docx")

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
print(legal_status_codelist_table, target = "C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/legal_status_codelist_table.docx")

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
print(legal_status_top5_doc, target = "C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/legal_status_top5.docx")

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
print(combined_totals_table, target = "C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/combined_totals_table.docx")


## Plot data ----

plot <- ggplot(combined_data, aes(x = end_date, y = usage, color = code_type)) +
  geom_line(aes(linewidth = code_type)) + 
  geom_point() +  
  #scale_color_viridis_d() +  
  scale_color_brewer(palette = "Dark2") +
  scale_linewidth_manual(values = c("Immigration to the UK (ONS)" = 1.0, 
                                 "All migration-related codes" = 0.5, 
                                 "Country of birth codes" = 0.5, 
                                 "Immigration legal status codes" = 0.5, 
                                 "Refugee or asylum-seeker codes" = 0.5, 
                                 "Interpreter-related codes" = 0.5))+
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

ggsave("C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/migration_code_usage.png", plot = plot, width = 8, height = 6, dpi = 300)


# All SNOMED-CT code usage -----

for_plot_allcodes <- snomed_usage |>
  group_by(start_date, end_date) |>
  summarise(usage = sum(usage), .groups = "drop") 

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

ggsave("C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/allsnomed_code_usage.png", plot = plot_allcodes, width = 8, height = 6, dpi = 300)

# Comparison with overall SNOMED-CT code usage ----

comparison <- data.frame(start_date = for_plot_allcodes$start_date,
                         end_date = for_plot_allcodes$end_date,
                         all_snomed = for_plot_allcodes$usage,
                         migration_snomed = for_plot$usage)

comparison <- comparison |>
  mutate(percent = round((migration_snomed/all_snomed * 100),4))

plot_comparison <- ggplot(comparison, aes(x = end_date, y = percent)) +
  geom_line(color = "black") + 
  geom_point(color = "black") +  
  scale_color_viridis_d() +  
  labs(
    title = " ",
    x = "Date",
    y = "Percentage of all SNOMED-CT coding"
  ) +
  scale_x_date(
    breaks = seq(from = min(comparison$end_date), to = max(comparison$end_date), by = "1 year"),  
    labels = date_format("%Y") 
  ) +
  scale_y_continuous(#limits = c(0, max(comparison$percent) + 2),  
                     labels = label_comma()) +  
  theme_bw() + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.title = element_blank()
  )

plot_comparison

ggsave("C:/Users/Yamina/OneDrive - Nexus365/Documents/Fellowship documents Oxford/Code usage paper/Tables_figures/comparison_plot.png", plot = plot_comparison, width = 8, height = 6, dpi = 300)




