library(readxl)

# import data community
comm_raw_low <- read_excel("data/ZAF/comm/Ralph_Native Species Survey_March 2022.xlsx",
                       sheet = "LowerSite_cover metrics") |> 
  clean_names()

comm_raw_high <- read_excel("data/ZAF/comm/Ralph_Native Species Survey_March 2022.xlsx",
                           sheet = "Upper Site_cover") |> 
  clean_names()

graminoid_feb_raw <- read_excel("data/ZAF/comm/Lesego_Native Species Survey_February 2023.xlsx") |> 
  clean_names()

graminoid_nov_raw <- read_excel("data/ZAF/comm/Lesego_Native Species Survey_November 2023.xlsx") |> 
  clean_names()

meta <- read_csv("data/ZAF/RangeX_PlotMetadata_ZAF_clean.csv") |> 
  select(region, site, block_id = block_id_original, plot_id = plot_id_original, treat_warming, treat_competition, added_focals, unique_plot_id) |> 
  mutate(block_id = as.character(block_id),
         plot_id = as.character(plot_id))

comm_low <- comm_raw_low |> 
  # remove rows after 34, because they do not contain data
  slice(1:34) |>
  rename("species" = plot_id) |> 
  pivot_longer(cols = c(x1_1:x10_2), names_to = "plot_id", values_to = "cover") |> 
  separate(col = plot_id, into = c("block_id", "plot_id"), sep = "_") |> 
  mutate(site = "lo",
         year = 2022,
         month = "march",
         block_id = str_remove(block_id, "x"),
         cover = as.numeric(cover),
         cover = if_else(cover == 0, NA_real_, cover)) |> 
  separate(col = species, into = c("species", "group"), sep = " \\(") |> 
  mutate(group = gsub("\\)", "", group),
         group = case_when(group == "focal planted" ~ "focal",
                           species %in% c("Bare Soil", "Litter", "All grasses", "Forbs") ~ "plot_level",
                           species == "Helichrysum spp." ~ "native",
                           is.na(group) ~ "native",
                           TRUE ~ group),
         remark = case_when(species == "All grasses" ~ "include. Focal grasses",
                            species == "Helichrysum spp." ~ "includes all Helichrysum species",
                            .default =  NA_character_)) |> 
  filter(!is.na(cover))
         


comm_high <- comm_raw_high |> 
  rename("species" = plot_id) |>
  mutate(x1_1 = as.numeric(x1_1),
         x1_2 = as.numeric(x1_2),
         x1_3 = as.numeric(x1_3),
         x1_4 = as.numeric(x1_4),
         x1_5 = as.numeric(x1_5),
         x1_6 = as.numeric(x1_6),
         x2_5 = if_else(x2_5 == "15-20", "17.5", x2_5),
         x2_5 = as.numeric(x2_5),
         x4_6 = as.numeric(x4_6)) |> 
  pivot_longer(cols = c(x1_1:x10_6), names_to = "plot_id", values_to = "cover") |> 
  separate(col = plot_id, into = c("block_id", "plot_id"), sep = "_") |> 
  mutate(site = "hi",
         year = 2022,
         month = "march",
         block_id = str_remove(block_id, "x"),
         cover = if_else(cover == 0, NA_real_, cover)) |> 
  filter(!is.na(cover)) |> 
  separate(col = species, into = c("species", "group"), sep = " \\(") |> 
  mutate(group = gsub("\\)", "", group),
         species = case_when(species == "Helichrysum" ~ "Helichrysum spp.", 
                             species == "BRYOPHYTES" ~ "Bryophytes",
                             .default = species),
         group = case_when(group == "focal panted" ~ "focal",
                           group == "focal planted" ~ "focal",
                           group == "wild" ~ "native",
                           species %in% c("Bare Soil", "Litter", "Bryophytes", "All grasses", "Forbs") ~ "plot_level",
                           species == "Helichrysum spp." ~ "native",
                           is.na(group) ~ "native",
                           TRUE ~ group),
         remark = case_when(species == "All grasses" ~ "include. Focal grasses",
                            species == "Helichrysum spp." ~ "includes all Helichrysum species",
                            .default =  NA_character_))


graminoid_feb <- graminoid_feb_raw |> 
  select(site, plot_id, species = native_species_grasses, cover = percentage_cover) |> 
  mutate(site = case_match(site,
                           "Top" ~ "hi",
                           "Lower" ~ "lo"),
         year = 2023,
         month = "february",
         group = "native") |> 
  separate(col = plot_id, into = c("block_id", "plot_id"), sep = "\\.") |> 
  filter(!is.na(species)) |> 
  mutate(cover = cover*100)

graminoid_nov <- graminoid_nov_raw |> 
  select(site, plot_id, species = native_species_grasses, cover = percentage_cover) |> 
  mutate(site = case_match(site,
                           "Top" ~ "hi",
                           "Lower" ~ "lo"),
         year = 2023,
         month = "november",
         group = "native") |> 
  separate(col = plot_id, into = c("block_id", "plot_id"), sep = "\\.") |> 
  filter(!is.na(species)) |> 
  mutate(cover = cover*100)


community <- bind_rows(comm_low, comm_high, graminoid_feb, graminoid_nov) |> 
  mutate(species = if_else(species == "Bare Soil", "Bare_soil", species)) |> 
  left_join(meta, by = c("site", "block_id", "plot_id")) |> 
  # make order
  select(year, month, region, site, block_id, plot_id, treat_warming, treat_competition, added_focals, unique_plot_id, group, species, cover, remark)

write_csv(x = community, file = "RangeX_ZAR_clean_community_2022-2023.csv")
range(community$cover)


# data checking
# community |> 
#   filter(group == "focal") |> 
#   distinct(site, species) |> 
#   mutate(precense = 1) |> 
#   pivot_wider(names_from = site, values_from = precense)  |> 
#   arrange(species)  %>% print(n = Inf)
#   write_csv(., "focals.csv")


