# Load required libraries
library(tidyverse)
library(sf)
library(tictoc)
library(terra)
library(exactextractr)

# Set input and output paths
drive_path <- "C:/Users/kks1a24/PNLP_DATA_CLEANING/"
pre_ea_path <- paste0(drive_path, "Extended_Pre_EA/")
data_path1  <- paste0(drive_path, "Sp_join_pnlp_ea/")
output_path <- paste0(drive_path, "OUTPUT_PNLP_EA/")
bld_raster_path <- paste0(drive_path, "input_data/COD_bld_count_20240617_unpublished.tif")
pop_raster_path <- paste0(drive_path, "input_data/WPGlobal2_2024_DRC/cod_pop_2024_CN_100m_R2024B_v1.tif")

# Load spatial data from GDB
pre_ea <- st_read(paste0(pre_ea_path, "Extended_Pre_EA.shp"))
data1  <- st_read(paste0(data_path1, "Haut_Lomami_XY.gpkg"))

# Fix invalid geometries in EA polygons
pre_ea <- st_make_valid(pre_ea)

# Reproject PNLP points to match EA CRS
data1 <- st_transform(data1, st_crs(pre_ea))

#List of unique province names
unique(pre_ea$Province)

#how many polygons exist for each province
table(pre_ea$Province)


# Filter only relevant EAs based on EA_ID in point data
ea_id <- data1$EA_ID
pnlp_pre <- pre_ea %>% filter(EA_ID %in% ea_id)
plot(pnlp_pre)

# Assign EA_ID to each point based on nearest EA polygon
nearest_indices <- st_nearest_feature(data1, pnlp_pre)
data1$EA_ID <- pnlp_pre$EA_ID[nearest_indices]

# --- Flag EAs with >5% zero or NA in nbr_person_menage ---
ea_flagged <- data1 %>%
  st_drop_geometry() %>%
  group_by(EA_ID) %>%
  summarise(
    total_points        = n(),
    num_zero_nbr_person = sum(nbr_person_menage == 0, na.rm = TRUE),
    num_na_nbr_person   = sum(is.na(nbr_person_menage)),
    num_zero_or_na      = num_zero_nbr_person + num_na_nbr_person,
    percent_zero_na     = (num_zero_or_na / total_points) * 100,
    flag_zero_na        = if_else(percent_zero_na > 5, "YES", "NO")
  )

# --- Calculate stats: mean, median, total ---
ea_stats <- data1 %>%
  st_drop_geometry() %>%
  group_by(EA_ID) %>%
  summarise(
    mean_nbr_person   = mean(nbr_person_menage, na.rm = TRUE),
    median_nbr_person = median(nbr_person_menage, na.rm = TRUE)
  )

pre_ea_pop <- data1 %>%
  st_drop_geometry() %>%
  group_by(EA_ID) %>%
  summarise(
    total_pop = sum(nbr_person_menage, na.rm = TRUE)
  )

# --- Combine all summaries ---
ea_combined <- ea_flagged %>%
  left_join(ea_stats,   by = "EA_ID") %>%
  left_join(pre_ea_pop, by = "EA_ID")

# --- Extract building count from raster ---
r_build <- rast(bld_raster_path)
pre_ea_bu_ras <- st_transform(pnlp_pre, crs(r_build))
pre_ea_bu_ras$building_count <- exact_extract(r_build, pre_ea_bu_ras, 'sum')

# --- Extract WorldPop population from raster ---
r_pop <- rast(pop_raster_path)
pre_ea_bu_ras <- st_transform(pre_ea_bu_ras, crs(r_pop))
pre_ea_bu_ras$wp_pop2024 <- exact_extract(r_pop, pre_ea_bu_ras, 'sum')

# --- Join all summaries back to spatial EA layer ---
pre_ea_final <- left_join(pre_ea_bu_ras, ea_combined, by = "EA_ID")

# --- Calculate population densities ---
pre_ea_final <- pre_ea_final %>%
  mutate(
    pnlp_pop_den = if_else(building_count > 0, total_pop / building_count, NA_real_),
    wp_pop_den   = if_else(building_count > 0, wp_pop2024 / building_count, NA_real_)
  )


# Export final GeoPackage
st_write(pre_ea_final, paste0(output_path, "Haut_Lomami_combined.gpkg"), delete_layer = TRUE)


#Export the result as csv
st_write(pre_ea_final, paste0(output_path, "Haut_Lomami_combined.gpkg"), delete_layer = TRUE)


