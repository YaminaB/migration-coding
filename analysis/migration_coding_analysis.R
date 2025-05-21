
##################################################################################################################
# Analysis code for the paper entitled "The coding of migration status in English primary care from 2011 to 2024"
# Author: Yamina Boukari
#         Bennett Institute for Applied Data Science
#         University of Oxford, 2025
###################################################################################################################

library(pacman)
p_load(remotes, dplyr, ggplot2, scales, viridis, flextable, 
       officer, readr, RColorBrewer, readxl, readODS, tidyr, 
       stringr, opencodecounts)

# function to generate outputs
generate_codelist_analysis <- function(codelist_path, output_name, snomed_usage) {
  
  # Load codelist
  codelist <- data.frame(get_codelist(codelist_path))
  
  # Filter to codes in use
  codelist_in_use <- codelist %>%
    filter(code %in% snomed_usage$snomed_code)
  
  # Export full codelist table
  ft_codelist <- flextable(codelist_in_use) %>%
    autofit() %>%
    set_table_properties(layout = "autofit") 
  doc <- read_docx() %>%
    body_add_flextable(ft_codelist) %>%
    print(target = paste0("output/", output_name, "_codelist_table.docx"))
  
  # Filter usage data
  codelist_usage <- snomed_usage %>%
    filter(snomed_code %in% codelist$code)
  
  # Total usage summary
  total <- codelist_usage %>%
    summarise(usage = sum(usage), .groups = "drop") %>%
    mutate(code_type = paste(output_name, "codes"))
  
  # Top 5 codes
  top5 <- codelist_usage %>%
    group_by(snomed_code, description) %>%
    summarise(usage = sum(usage), .groups = "drop") %>%
    mutate(percent_usage = round(100 * usage / sum(usage), 0)) %>%
    slice_max(usage, n = 5)
  
  ft_top5 <- flextable(top5) %>%
    autofit() %>%
    set_table_properties(layout = "autofit")
  doc_2 <- read_docx() %>%
    body_add_flextable(ft_top5) %>%
    print(target = paste0("output/", output_name, "_top5.docx"))
  
  # For plotting
  usage_trend <- codelist_usage %>%
    group_by(start_date, end_date) %>%
    summarise(usage = sum(usage), .groups = "drop") %>%
    mutate(code_type = paste(output_name, "codes"))
  
  # Calculate % increase
  usage_trend <- usage_trend %>%
    mutate(perc_increase = (usage - first(usage)) / first(usage) * 100,
           perc_increase_yearly = (usage - lag(usage)) / lag(usage) * 100)
  
  return(list(
    codelist = codelist_in_use,
    total = total,
    top5 = top5,
    usage_trend = usage_trend
  ))
}

# General migrant ######
general_migrant_results <- generate_codelist_analysis(
  codelist_path = "user/YaminaB/migration-status/47586e6d",
  output_name = "All migration-related",
  snomed_usage = snomed_usage
)

# Country of birth ----
cob_migrant_results <- generate_codelist_analysis(
  codelist_path = "user/YaminaB/born-outside-the-uk/0637ca14",
  output_name = "Country of birth",
  snomed_usage = snomed_usage
)

# Interpreter needed -----
interpreter_migrant_results <- generate_codelist_analysis(
  codelist_path = "user/YaminaB/interpreter-required/3856b07e",
  output_name = "Interpreter-related",
  snomed_usage = snomed_usage
)

# Refugees and asylum seekers -----
refugee_migrant_results <- generate_codelist_analysis(
  codelist_path = "user/YaminaB/asylum-seeker-or-refugee/35a3f088",
  output_name = "Refugee or asylum-seeker",
  snomed_usage = snomed_usage
)

# Immigration legal status -----
legal_status_migrant_results <- generate_codelist_analysis(
  codelist_path = "user/YaminaB/uk-visa/4eb363bd",
  output_name = "Immigration legal status",
  snomed_usage = snomed_usage
)

# Language
language_migrant_results <- generate_codelist_analysis(
  codelist_path = "user/YaminaB/english-not-main-language/2e48fb38",
  output_name = "Language-related",
  snomed_usage = snomed_usage
)

# Combine data ----
combined_data <- bind_rows(
  general_migrant_results$usage_trend,
  language_migrant_results$usage_trend,
  cob_migrant_results$usage_trend,
  interpreter_migrant_results$usage_trend,
  refugee_migrant_results$usage_trend,
  legal_status_migrant_results$usage_trend
)
combined_data$usage[is.na(combined_data$usage)] <- 0

combined_data$code_type <- factor(combined_data$code_type, levels = 
                                    c("All migration-related codes", 
                                      "Language-related codes",
                                      "Interpreter-related codes",
                                      "Country of birth codes", 
                                      "Immigration legal status codes", 
                                      "Refugee or asylum-seeker codes"))

combined_data_totals <- bind_rows(
  general_migrant_results$total,
  cob_migrant_results$total,
  language_migrant_results$total,
  interpreter_migrant_results$total,
  refugee_migrant_results$total,
  legal_status_migrant_results$total
)

combined_totals_table <- flextable(combined_data_totals) |>
  autofit() |>
  set_table_properties(layout = "autofit")
combined_totals_table <- read_docx() %>% 
  body_add_flextable(combined_totals_table) 
print(combined_totals_table, target = "output/combined_totals_table.docx")

# All snomed code usage (bars on the same graph - Figure 1)
for_plot_allcodes <- snomed_usage %>%
  group_by(start_date, end_date) %>%
  summarise(usage = sum(usage), .groups = "drop") 

combined_data <- left_join(combined_data, for_plot_allcodes, by = "end_date")

# Plot data ----

# create scale factor for second y axis 
scale_factor <- max(combined_data$usage.x, na.rm = TRUE) / max(combined_data$usage.y, na.rm = TRUE)

combined_data$scaled_usage <- combined_data$usage.y * scale_factor

bar_plot_data <- combined_data %>%
  filter(code_type == "All migration-related codes") %>%
  select(c(end_date, usage.y, scaled_usage))

plot <- ggplot(combined_data, aes(x = end_date)) +
  # All SNOMED-CT code usage bar chart (right y-xis)
  geom_col(data = bar_plot_data, aes(y = scaled_usage), position = "identity", fill = "lightgrey") + 
  # Migration-related SNOMED-CT code usage line graphs (left y-axis)
  geom_line(aes(y = usage.x, color = code_type, linewidth = code_type, linetype = code_type)) +  
  geom_point(aes(y = usage.x, color = code_type)) +  
  scale_color_brewer(palette = "Dark2") +
  scale_linewidth_manual(values = c("All migration-related codes" = 0.5, 
                                    "Country of birth codes" = 0.5, 
                                    "Immigration legal status codes" = 0.5, 
                                    "Refugee or asylum-seeker codes" = 0.5,
                                    "Language-related codes" = 0.5,
                                    "Interpreter-related codes" = 0.5)) +
  scale_linetype_manual(values = c("All migration-related codes" = "solid", 
                                   "Country of birth codes" = "solid", 
                                   "Immigration legal status codes" = "solid", 
                                   "Refugee or asylum-seeker codes" = "solid",
                                   "Language-related codes" = "solid",
                                   "Interpreter-related codes" = "solid")) + 
  
  labs(
    title = " ",
    x = "Date",
    y = "Usage"
  ) +
  scale_x_date(
    breaks = seq(from = min(combined_data$end_date), to = max(combined_data$end_date), by = "1 year"),  
    labels = date_format("%Y") 
  ) +
  # Left y-axis scale (for usage.x)
  scale_y_continuous(
    name = "Migration-related SNOMED-CT code usage",
    limits = c(0, max(combined_data$usage.x, na.rm = TRUE) + 10),
    breaks = seq(0, max(combined_data$usage.x, na.rm = TRUE) + 500000, by = 500000),
    labels = scales::comma_format(),
    # Right y-axis scale (scaled usage.y)
    sec.axis = sec_axis(~ . / scale_factor, name = "All SNOMED-CT code usage",
                        labels = scales::comma_format())
  ) +
  theme_bw() + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.y.right = element_text(margin = margin(l = 15)), 
    legend.position = "bottom",
    legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))  

plot

ggsave("output/migration_code_usage.png", plot = plot, width = 8, height = 6, dpi = 300)

# Comparisons with overall SNOMED-CT coding ----
all_snomed_percentage_increase <- snomed_usage %>%
  group_by(start_date, end_date) %>%
  summarise(usage = sum(usage), .groups = "drop") %>%
  mutate(perc_increase = (usage-first(usage))/first(usage) *100) %>%
  mutate(perc_increase_yearly = (usage -lag(usage))/lag(usage) *100) %>%
  mutate(code_type = "All SNOMED-CT codes")

all_migration_snomed_percentage_increase <- general_migrant_results$usage_trend 

percentage_increase_combined <- rbind(all_snomed_percentage_increase, all_migration_snomed_percentage_increase)

# Migration-related SNOMED-CT coding as a percentage of overall SNOMED-CT coding (Supplementary Figure 1)
migration_codes_as_percentage_of_overall <- all_migration_snomed_percentage_increase %>%
  left_join(all_snomed_percentage_increase, by = c("start_date", "end_date")) %>%
  mutate(percent_of_overall = usage.x/usage.y * 100) %>%
  select(c(start_date, end_date, usage.x, usage.y, percent_of_overall))

percentage_of_overall_plot <- ggplot(migration_codes_as_percentage_of_overall, aes(x = end_date, y = percent_of_overall)) +
  geom_bar(stat = "identity", fill = "#1b9e77") +  
  labs(
    x = "Year",
    y = "Migration-related SNOMED-CT coding \nas percentage of overall SNOMED-CT coding"
  ) +
  scale_x_date(
    breaks = seq(from = min(for_plot_allcodes$end_date), to = max(for_plot_allcodes$end_date), by = "1 year"),  
    labels = date_format("%Y") 
  ) +
  ylim( c(0,0.10)) +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),  
        axis.title.y = element_text(margin = margin(r = 10))) 

percentage_of_overall_plot
ggsave("output/percentage_of_overall_plot.png", plot = percentage_of_overall_plot, width = 8, height = 6, dpi = 300)

# Period Percentage increase in migration-related and overall SNOMED-CT code usage (Supplementary Figure 2)
percentage_increase_plot <- ggplot(percentage_increase_combined, aes(x = end_date, y = perc_increase, fill = code_type)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    x = "Year",
    y = "Percentage change in SNOMED-CT code usage"
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


# Annual percentage increase in migration-related and overall SNOMED-CT code usage (Supplementary Figure 3) -----
percentage_increase_annual_plot <- ggplot(percentage_increase_combined, aes(x = end_date, y = perc_increase_yearly, fill = code_type)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    x = "Year",
    y = "Percentage change in SNOMED-CT code usage"
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

percentage_increase_annual_plot
ggsave("output/percentage_increase_annual_plot.png", plot = percentage_increase_annual_plot, width = 8, height = 6, dpi = 300)

# Immigration data -----

# Add immigration data from lamis: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/localareamigrationindicatorsunitedkingdom
# for international long-term migration: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/datasets/longterminternationalimmigrationemigrationandnetmigrationflowsprovisional 

ons <- read_csv("long_term_international_migration_UK_ONS.csv", col_types = cols(population = col_double()))

for_plot_ons <- ons %>%
  mutate(group = "Long-term immigration to the UK") %>%
  select(-year_full) %>%
  rename(total = population)

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

bno_ukraine_data <- bno_ukraine_data[c(7,10),1:(ncol(bno_ukraine_data)-3)]
bno_ukraine_data$Year <- as.factor(bno_ukraine_data$Year)
levels(bno_ukraine_data$Year)

bno_ukraine_data_yeartotals <- bno_ukraine_data %>%
  pivot_longer(cols = `2010`:`2024`,             
               names_to = "year",
               values_to = "total") %>%
  rename(group = Year) %>%
  filter(year >=2012) %>%
  mutate(group = ifelse(group == "BN(O) Route visa grants(subset of Total BN(O) Hong Kong visa grants)", "BNO Hong Kong visas", group)) %>%
  mutate(group = ifelse(group == "Ukraine Visa Schemes grants(subset of Total Ukraine Visa grants)", "Ukraine visa schemes", group))

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
all_visa_types_totals <- rbind(for_plot_ons, study_visas_yeartotals, 
                               work_visas_yeartotals, 
                               family_visas_yeartotals, 
                               all_refugee_asylum_yeartotals)

all_visa_types_totals$year <- as.numeric(all_visa_types_totals$year)


all_visa_types_totals$group <- factor(all_visa_types_totals$group, levels = 
                                    c("Long-term immigration to the UK", 
                                      "Study visa status", 
                                      "Work visa status", 
                                      "Family visa status", 
                                      "Asylum and refugee status"))

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
    legend.title = element_blank()) 

plot_immigration_data

ggsave("output/visa_type_plot.png", plot = plot_immigration_data, width = 8, height = 6, dpi = 300)

# Census 2021 comparison ----
# get snomed-ct data for 2020/21 and earlier (Census was on 21st March 2021)

snomed_usage_20_21_and_earlier <- snomed_usage %>%
  filter(start_date <= "2020-08-01")

cob_data_snomed <- snomed_usage_20_21_and_earlier |> 
  filter(snomed_code %in% cob_codelist$code) 

cob_data_snomed_ranked <- cob_data_snomed |>
  group_by(snomed_code, description) |>
  summarise(usage = sum(usage), .groups = "drop") |>
  mutate(percent_usage = round(100*(usage/ sum(usage)),2)) |>
  arrange(desc(usage)) %>%
  mutate(country = str_remove_all(description, "Born in |\\(finding\\)"),
         country = str_trim(country))

# Census 2021 CoB data summary - accessed 04 April 2025
# from: https://www.ons.gov.uk/datasets/create/filter-outputs/ac2b3af3-76a6-46e0-94e1-de929e9fc920#get-data
census_cob_data <- read_excel("custom-filtered-2025-04-04T12_09_23Z.xlsx") 

census_cob_data <- census_cob_data %>%
  filter(Countries == "England")

census_cob_data$`Country of birth (extended) (190 categories)` <- as.factor(census_cob_data$`Country of birth (extended) (190 categories)`)

uk_cobs <- c("Europe: United Kingdom: England", 
  "Europe: United Kingdom: Northern Ireland",
  "Europe: United Kingdom: Scotland",
  "Europe: United Kingdom: Wales",
  "Europe: United Kingdom: Great Britain not otherwise specified",
  "Europe: United Kingdom: United Kingdom not otherwise specified")

census_cob_data <- census_cob_data %>%
  filter(!(`Country of birth (extended) (190 categories)`%in% uk_cobs)) %>%
  arrange(desc(Observation)) 

total_nonUKcob_census <- census_cob_data %>%
  summarise(total = sum(Observation))

census_cob_data <- census_cob_data %>%
  mutate(`Census 2021` = (Observation/total_nonUKcob_census$total)*100) %>%
  slice(1:10) %>%
  rename("country" = "Country of birth (extended) (190 categories)") %>%
  select(c(country, `Census 2021`, Observation)) %>%
  mutate(country = str_split_fixed(country, ":", 3)[, 3] %>%
           str_trim())
  
# join
census_and_snomed_cob <- census_cob_data %>%
  left_join(cob_data_snomed_ranked, by = c("country")) %>%
  select(c(country, `Census 2021`, percent_usage)) %>%
  rename("SNOMED-CT codes" = "percent_usage") %>%
  pivot_longer(cols = c(`SNOMED-CT codes`, `Census 2021`),
               names_to = "data_source",
               values_to = "percentage")

census_and_snomed_cob$country <- factor(census_and_snomed_cob$country, levels =
                                                     c("India",
                                                       "Poland",
                                                       "Pakistan",
                                                       "Romania",
                                                       "Ireland",
                                                       "Italy",
                                                       "Bangladesh",
                                                       "Nigeria",
                                                       "Germany",
                                                       "South Africa"))
  
  
# plot
plot_census_snomed_cob <- ggplot(census_and_snomed_cob, aes(x = country, y = percentage, fill = data_source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) + 
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = " ",
    x = "Country of birth",
    y = "Percentage"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)))

plot_census_snomed_cob

ggsave("output/census_snomed_cob.png", plot = plot_census_snomed_cob, width = 8, height = 6, dpi = 300)

