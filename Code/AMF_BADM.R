library(amerifluxr)
library(dplyr)
library(sf)
library(mapview)
library(ggplot2)

setwd('../')
git.fp = getwd()
data.fp = paste(git.fp, 'Data', sep = '/')
plots.fp = paste(git.fp, 'Plots', sep = '/')

# Get site info
amf.sitelist = amf_sites()
amf.site.ids = amf.sitelist$SITE_ID
amf.site.locs = amf.sitelist[,c(1,3,10,9)]
colnames(amf.site.locs) = c('SiteID','Country','lon','lat')

# # Convert lat/lon locations simple feature (sf)
# amf.site.locs.sf = st_as_sf(x = amf.site.locs,
#                             coords = c('lon','lat'),
#                             crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
# 
# # Generate interactive map
# mapview(amf.site.locs.sf, zcol = 'Country')

# Get BIF
bif = amf_download_bif(user_id = 'cjdevine',
                       user_email = 'cjdevine@arizona.edu',
                       data_policy = "CCBY4.0",
                       agree_policy = TRUE,
                       intended_use = "remote_sensing",
                       intended_use_text = "Synthesis work with Dave Moore",
                       out_dir = data.fp)

# Read BADM from BIF file
badm = amf_read_bif(file = bif)

# Define the list of specific variables to be considered
selected_variables = c('LAI', 'BASAL_AREA', 'TREES_NUM', 'AG_BIOMASS_TREE', 
                       'AG_LIT_PROD_TOT', 'AG_PROD_TREE', 'BIOMASS_N', 
                       'LAI_TOT', 'ROOT_BIOMASS_FINE', 'ROOT_BIOMASS_CRS', 
                       'SOIL_CHEM_N_TOT', 'SOIL_CHEM_C_ORG', 'SOIL_TEX_SAND')

# Filter data for the selected VARIABLES and non-zero values
filtered_data = badm %>%
  filter(VARIABLE %in% selected_variables, as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Summarize the number of unique SITE_IDs for each VARIABLE
site_id_summaryKEY = filtered_data %>%
  group_by(VARIABLE) %>%
  summarise(UniqueSiteIDs = n_distinct(SITE_ID), .groups = 'drop')


# Mapping function (requires 'variable.name' input included within selected_variables)
map.amf.variable = function(variable.name) {
  
  # Subset filtered data by variable name (or variable group for LAI)
  if (variable.name == 'LAI') {
    var = filtered_data[filtered_data$VARIABLE_GROUP == 'GRP_LAI',]
    } else {var = filtered_data[filtered_data$VARIABLE == variable.name,]}
  
  # Create column assigning integer value for instances of variable collection at each site
  var$VarGroup_Int = 1
  
  # Summarize by site by summing the number of instances of the specified variable
  var = var %>%
    group_by(SITE_ID) %>%
    summarise(Var_count = sum(VarGroup_Int))
  
  # Filter site locations (all AMF sites) to include USA-only sites that contain the specified variable 
  var.site.locs = amf.site.locs[match(var$SITE_ID, amf.site.locs$SiteID),]
  
  # Bind columns from filtered site lon/lat coordinates to the summarized data frame
  var = cbind(var, var.site.locs[,2:4])
  var = var[var$Country == 'USA',]
  var$lon = as.numeric(var$lon)
  var$lat = as.numeric(var$lat)
  
  # Filter AMF site locations for USA-only sites
  usa.locs = amf.site.locs[amf.site.locs$Country == 'USA',]
  usa.locs$lon = as.numeric(usa.locs$lon)
  usa.locs$lat = as.numeric(usa.locs$lat)
  
  # Get state polygons
  state.polys = map_data('state')
  
  # Generate map
  var.map = ggplot() +
    geom_polygon(data = state.polys, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
    geom_point(data = var, aes(x = lon, y = lat), fill = 'red', color = 'black', shape = 21, alpha = 0.25, size = 10) +
    geom_point(data = usa.locs, aes(x = lon, y = lat), col = 'black', size = 2) +
    coord_map(xlim = c(-125,-68), ylim = c(25,49)) +
    theme_void() +
    ggtitle(variable.name) +
    theme(axis.title = element_blank(),
          plot.title = element_text(size = 25, face = 'bold', hjust = 0.5),
          plot.background = element_rect(fill = 'white', color = 'white'))
  
  var.map
  
  ggsave(filename = paste0(plots.fp, '/', variable.name, '_BADM_Map.png'),
         plot = var.map,
         width = 6, height = 5)
}


# Run mapping function for all variables specified in select_variables
for (i in 1 : length(selected_variables)) {
  map.amf.variable(selected_variables[i])
}

# ---------------------------------------------------------------------------------------

# ------------------- US-NR1

# Subset US-NR1 info from BADM
nr1.badm = badm[badm$SITE_ID == 'US-NR1',]

# Get list of unique variables 
nr1.badm.vars = unique(nr1.badm$VARIABLE)

# Subset by variable and date
nr1.badm.vars.dates = as.data.frame(nr1.badm[grepl('DATE$', nr1.badm$VARIABLE),])

library(flextable)
library(ftExtra)

# Create summary table
table = flextable(nr1.badm.vars.dates[,c(3,5)]) %>%
  merge_v(j = c(1,2))



