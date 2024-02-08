library(amerifluxr)
library(dplyr)
library(sf)
library(mapview)
library(ggplot2)
library(Polychrome)

setwd('../')
git.fp = getwd()
data.fp = paste(git.fp, 'Data', sep = '/')
plots.fp = paste(git.fp, 'Plots', sep = '/')

# Get site info
amf.sitelist = amf_sites()
amf.site.ids = amf.sitelist$SITE_ID
amf.site.locs = amf.sitelist[,c(1,3,10,9)]
colnames(amf.site.locs) = c('SiteID','Country','lon','lat')
ca.ak.site.ids = c(as.vector(amf.sitelist[amf.sitelist$COUNTRY == 'Canada',1]),
                   na.omit(as.vector(amf.sitelist[amf.sitelist$STATE == 'AK',1])))

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

# ---------------------------------------------------------------------------------------

# ------------------- Variables by site

# Define the list of specific variables to be considered
selected_variables = c('LAI', 'BASAL_AREA', 'TREES_NUM', 'AG_BIOMASS_TREE', 
                       'AG_LIT_PROD_TOT', 'AG_PROD_TREE', 'BIOMASS_N', 
                       'LAI_TOT', 'ROOT_BIOMASS_FINE', 'ROOT_BIOMASS_CRS', 
                       'SOIL_CHEM_N_TOT', 'SOIL_CHEM_C_ORG', 'SOIL_TEX_SAND')

# Filter data for the selected VARIABLES and non-zero values
filtered_data = badm %>%
  filter(VARIABLE %in% selected_variables, as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Mapping function (requires 'variable.name' input included within selected_variables)
# For CONUS maps, use region = 'CONUS'
# For Alaska/Canada maps, use region = 'Alaska/Canada'
map.amf.variable = function(variable.name, region) {
  
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
  
  # Filter site locations (all AMF sites) to include sites that contain the specified variable 
  var.site.locs = amf.site.locs[match(var$SITE_ID, amf.site.locs$SiteID),]
  
  # Bind columns from filtered site lon/lat coordinates and BADM states to the variable summarized data frame
  var = cbind(var, var.site.locs[,2:4])
  
  # Convert lat/lon values to numeric values
  var$lon = as.numeric(var$lon)
  var$lat = as.numeric(var$lat)
  
  # Filter spatially
  if (region == 'CONUS') {var = var[var$Country == 'USA',]}
  if (region == 'Alaska/Canada') {
    var = var[var$SITE_ID %in% ca.ak.site.ids,]
    }
  
  # Filter AMF site locations 
  if (region == 'CONUS') { all.site.locs = amf.site.locs[amf.site.locs$Country == 'USA',] }
  if (region == 'Alaska/Canada') { all.site.locs = amf.site.locs[amf.site.locs$Country %in% c('USA','Canada'),] }
  
  # Convert lat/lon values to numeric values
  all.site.locs$lon = as.numeric(all.site.locs$lon)
  all.site.locs$lat = as.numeric(all.site.locs$lat)
  
  if (region == 'Alaska/Canada') {all.site.locs = all.site.locs[all.site.locs$SiteID %in% ca.ak.site.ids,]}
  
  # Get state and country polygons for mapping
  state.polys = map_data('state')
  country.polys = map_data('world', xlim = c(-170,-65), ylim = c(17,70))
  
  # Generate map and save output PNGs
  if (region == 'CONUS') {
    var.map = ggplot() +
      geom_polygon(data = state.polys, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
      geom_point(data = var, aes(x = lon, y = lat), fill = 'red', color = 'black', shape = 21, alpha = 0.25, size = 10) +
      geom_point(data = all.site.locs, aes(x = lon, y = lat), col = 'black', size = 2) +
      coord_map(xlim = c(-125,-68), ylim = c(25,49)) +
      theme_bw() +
      ggtitle(variable.name) +
      theme(axis.title = element_blank(),
            plot.title = element_text(size = 25, face = 'bold', hjust = 0.5),
            plot.background = element_rect(fill = 'white', color = 'white'))
    
    var.map
    
    ggsave(filename = paste0(plots.fp, '/SiteVariables/CONUS/', variable.name, '_BADM_Map_CONUS.png'),
           plot = var.map,
           width = 6, height = 5)
  }
  
  if (region == 'Alaska/Canada') {
    var.map = ggplot() +
      geom_polygon(data = country.polys, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
      geom_point(data = var, aes(x = lon, y = lat), fill = 'red', color = 'black', shape = 21, alpha = 0.25, size = 10) +
      geom_point(data = all.site.locs, aes(x = lon, y = lat), col = 'black', size = 2) +
      coord_map(xlim = c(-168,-60), ylim = c(41,70)) +
      theme_bw() +
      ggtitle(variable.name) +
      theme(axis.title = element_blank(),
            plot.title = element_text(size = 25, face = 'bold', hjust = 0.5),
            plot.background = element_rect(fill = 'white', color = 'white'))
    
    var.map
    
    ggsave(filename = paste0(plots.fp, '/SiteVariables/Alaska_Canada/', variable.name, '_BADM_Map_AK_CA.png'),
           plot = var.map,
           width = 6, height = 4)
  }
}


# Run mapping function for all variables specified in select_variables
for (i in 1 : length(selected_variables)) {
  map.amf.variable(selected_variables[i], 'CONUS')
  map.amf.variable(selected_variables[i], 'Alaska/Canada')
}


# ---------------------------------------------------------------------------------------

# ------------------- Disturbance history

# Subset BADM by sites which contain disturbance history information (VARIABLES containing "DM")
badm.dm = badm[grep('DM', badm$VARIABLE),]

# Filter out extra information - we only need specific DM categories 
badm.dm = badm.dm[grep('DM_DATE|DM_GENERAL|DM_COMMENT', badm.dm$VARIABLE, invert = TRUE),]

# Get unique DM_VARIABLE names
dm.vars = unique(badm.dm$VARIABLE)

# Exclude 'DM_SURF' (percentage of footprint area affected by disturbance event) from dm.vars
dm.vars = dm.vars[grep('DM_SURF', dm.vars, invert = TRUE)]

# Mapping function (requires 'dm.variable.name')
# For CONUS maps, use region = 'CONUS'
# For Alaska/Canada maps, use region = 'Alaska/Canada'
map.dm.function = function(dm.variable.name, region) {
  
  # Set disturbance (DM) variable 
  dm.var = dm.variable.name
  
  # Filter BADM using specified DM variable
  dm = badm.dm[badm.dm$VARIABLE %in% dm.var,]
  
  # Filter site locations (all AMF sites) to include only those which contain the specified DM variable
  dm.site.locs = amf.site.locs[match(dm$SITE_ID, amf.site.locs$SiteID),]
  
  # Bind columns from filtered site lon/lat coordinates to the filtered DM data frame, select USA-only instances
  dm = cbind(dm, dm.site.locs[,2:4])
  dm$lon = as.numeric(dm$lon)
  dm$lat = as.numeric(dm$lat)
  
  # Filter according to specified region
  if (region == 'CONUS') {dm = dm[dm$Country == 'USA',]}
  if (region == 'Alaska/Canada') {dm = dm[dm$SITE_ID %in% ca.ak.site.ids,]}
  
  # Filter AMF site locations 
  if (region == 'CONUS') { all.site.locs = amf.site.locs[amf.site.locs$Country == 'USA',] }
  if (region == 'Alaska/Canada') { all.site.locs = amf.site.locs[amf.site.locs$Country %in% c('USA','Canada'),] }
  
  # Convert lat/lon values to numeric values
  all.site.locs$lon = as.numeric(all.site.locs$lon)
  all.site.locs$lat = as.numeric(all.site.locs$lat)
  
  if (region == 'Alaska/Canada') {all.site.locs = all.site.locs[all.site.locs$SiteID %in% ca.ak.site.ids,]}
  
  # Get state and country polygons for mapping
  state.polys = map_data('state')
  country.polys = map_data('world', xlim = c(-170,-65), ylim = c(17,70))
  
  # Generate map for selected DM variable symbolizing specific disturbance categories
  if (region == 'CONUS') {
    
    dm.map = ggplot() +
      geom_polygon(data = state.polys, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
      geom_point(data = dm, aes(x = lon, y = lat, fill = DATAVALUE, shape = DATAVALUE), color = 'black', alpha = 0.5, size = 10) +
      scale_shape_manual(values = c(21,22,23,24)) +
      geom_point(data = all.site.locs, aes(x = lon, y = lat), col = 'black', size = 2) +
      coord_map(xlim = c(-125,-68), ylim = c(25,49)) +
      theme_bw() +
      ggtitle(dm.var) +
      theme(axis.title = element_blank(),
            plot.title = element_text(size = 25, face = 'bold', hjust = 0.5),
            legend.title = element_blank(),
            legend.position = 'bottom',
            #legend.position = c(0,0),
            #legend.justification = c('left','bottom'),
            #legend.box.just = 'right',
            legend.text = element_text(size = 10),
            plot.background = element_rect(fill = 'white', color = 'white'))
    
    dm.map
    
    ggsave(filename =  paste0(plots.fp, '/Disturbance/CONUS/', dm.variable.name, '_BADM_Map_CONUS.png'),
           dm.map,
           width = 8, height = 5)
  }
  
  if (region == 'Alaska/Canada') {
    
    dm.map = ggplot() +
      geom_polygon(data = country.polys, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
      geom_point(data = dm, aes(x = lon, y = lat, fill = DATAVALUE, shape = DATAVALUE), color = 'black', alpha = 0.5, size = 10) +
      scale_shape_manual(values = c(21,22,23,24)) +
      geom_point(data = all.site.locs, aes(x = lon, y = lat), col = 'black', size = 2) +
      coord_map(xlim = c(-168,-60), ylim = c(41,70)) +
      theme_bw() +
      ggtitle(dm.var) +
      theme(axis.title = element_blank(),
            plot.title = element_text(size = 25, face = 'bold', hjust = 0.5),
            legend.title = element_blank(),
            legend.position = 'bottom',
            #legend.position = c(0,0),
            #legend.justification = c('left','bottom'),
            #legend.box.just = 'right',
            legend.text = element_text(size = 10),
            plot.background = element_rect(fill = 'white', color = 'white'))
    
    dm.map
    
    ggsave(filename =  paste0(plots.fp, '/Disturbance/Alaska_Canada/', dm.variable.name, '_BADM_Map_AK_CA.png'),
           dm.map,
           width = 8, height = 4)
  }
}


for (i in 1 : length(dm.vars)) {
  #map.dm.function(dm.vars[i], 'CONUS')
  map.dm.function(dm.vars[i], 'Alaska/Canada')
}

# ---------------------------------------------------------------------------------------

# ------------------- Land cover at arctic/boreal flux sites

# Create data frame with IGBP Class IDs and Names
igbp.class = data.frame('ClassID' = c('BSV',
                                      'CRO',
                                      'CSH',
                                      'CVM',
                                      'DBF',
                                      'DNF',
                                      'EBF',
                                      'ENF',
                                      'GRA',
                                      'MF',
                                      'OSH',
                                      'SAV',
                                      'SNO',
                                      'URB',
                                      'WAT',
                                      'WET',
                                      'WSA'),
                        'ClassName' = c('Barren/Sparse Vegetation',
                                        'Croplands',
                                        'Closed Shrublands',
                                        'Cropland/Natural Vegetation Mosaics',
                                        'Deciduous Broadleaf Forests',
                                        'Deciduous Needleleaf Forests',
                                        'Evergreen Broadleaf Forests',
                                        'Evergreen Needleleaf Forests',
                                        'Grasslands',
                                        'Mixed Forests',
                                        'Open Shrublands',
                                        'Savannas',
                                        'Snow and Ice',
                                        'Urband and Built-Up Lands',
                                        'Water Bodies',
                                        'Permanent Wetlands',
                                        'Woody Savannas'))

# Subset BADM IGBP variables
badm.igbp = badm[badm$VARIABLE == 'IGBP',]

# Filter site locations by matching SITE_ID characters in badm.igbp
amf.site.locs.igbp = amf.site.locs[amf.site.locs$SiteID %in% badm.igbp$SITE_ID,]

# Remove duplicate sites from badm.igbp
badm.igbp = badm.igbp[!duplicated(badm.igbp$SITE_ID),]

# Combine in single data frame
badm.igbp.sites = cbind(badm.igbp, amf.site.locs.igbp)
badm.igbp.sites$lon = as.numeric(badm.igbp.sites$lon)
badm.igbp.sites$lat = as.numeric(badm.igbp.sites$lat)

# Filter sites in North America only
badm.igbp.sites = badm.igbp.sites[badm.igbp.sites$Country %in% c('USA','Canada','Mexico','Panama','Puerto Rico','Costa Rica'),]

# Rename IGBP class column to "IGBP_Class
colnames(badm.igbp.sites)[5] = 'IGBP_Class'

# Create category for grouping similar IGBP_Class types
igbp.group.fun = function() {
  df.out = badm.igbp.sites
  df.out$IGBP_Group = NA
  df.out[df.out$IGBP_Class %in% c('DBF','EBF','ENF','MF'), 10] = 'Forest sites'
  df.out[df.out$IGBP_Class %in% c('CSH','OSH'), 10] = 'Shrub sites'
  df.out[df.out$IGBP_Class %in% c('CRO','CVM'), 10] = 'Crop sites'
  df.out[df.out$IGBP_Class %in% c('SAV','WSA'), 10] = 'Savanna sites'
  df.out[df.out$IGBP_Class %in% c('WAT','WET'), 10] = 'Wet sites'
  df.out[df.out$IGBP_Class %in% c('BSV','SNO'), 10] = 'Low-veg. sites'
  df.out[df.out$IGBP_Class %in% c('URB'), 10] = 'Urban sites'
  df.out[df.out$IGBP_Class %in% c('GRA'), 10] = 'Grassland sites'
  return(df.out)
}

# Run function
badm.igbp.sites = igbp.group.fun()

# Factor IGBP_Class 
badm.igbp.sites$IGBP_Class = factor(badm.igbp.sites$IGBP_Class, levels = sort(unique(badm.igbp.sites$IGBP_Class)))

# Plot map showing site locations symbolize by IGBP cover class
country.polys = map_data('world', xlim = c(-170,-65), ylim = c(17,70))

# Assign IGBP colors and labels
igbp.cols = unname(dark.colors(n = length(unique(badm.igbp.sites$IGBP_Class))))
igbp.class.labs = igbp.class[igbp.class$ClassID %in% unique(badm.igbp.sites$IGBP_Class),]

# Generate map for North America
igbp.map = ggplot() +
  geom_polygon(data = country.polys, 
               aes(x = long, y = lat, group = group), 
               fill = NA, color = 'black') +
  geom_point(data = badm.igbp.sites,
             aes(x = lon, y = lat, color = IGBP_Class, shape = IGBP_Group), 
             alpha = 1, size = 1.5) +
  scale_color_manual(values = igbp.cols, labels = igbp.class.labs$ClassName) +
  scale_shape_manual(values = seq(1,8,1)) +
  coord_map(xlim = c(-170,-62), ylim = c(17,70)) +
  theme_bw() +
  ggtitle('IGBP cover classes for North American Ameriflux sites') +
  theme(axis.title = element_blank(),
        plot.title = element_text(size = 12, face = 'bold', hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.background = element_rect(fill = 'white', color = 'white'))

ggsave(filename = paste0(plots.fp, '/IGBP/', 'IGBP_NorthAmerica.png'),
       igbp.map,
       width = 10, height = 7)

# Generate map for everything north of 50 deg.
badm.igbp.ca.ak = badm.igbp.sites[badm.igbp.sites$SITE_ID %in% ca.ak.site.ids,]

igbp.map.n50 = ggplot() +
  geom_polygon(data = country.polys, 
               aes(x = long, y = lat, group = group), 
               fill = NA, color = rgb(0,0,0,0.5)) +
  geom_point(data = badm.igbp.ca.ak,
             aes(x = lon, y = lat, color = IGBP_Class, shape = IGBP_Class), 
             alpha = 1, size = 9) +
  geom_point(data = badm.igbp.ca.ak,
             aes(x = lon, y = lat), 
             inherit.aes = FALSE,
             color = 'black', size = 0.5) +
  scale_color_manual(values = igbp.cols, labels = igbp.class.labs$ClassName) +
  scale_shape_manual(values = seq(1,9,1), labels = igbp.class.labs$ClassName) +
  coord_map(xlim = c(-168,-60), ylim = c(42,71)) +
  theme_bw() +
  ggtitle('IGBP cover classes for Ameriflux sites in Alaska and Canada') +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 30, face = 'bold', hjust = 0.5),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        plot.background = element_rect(fill = 'white', color = 'white'),
        aspect.ratio = 2/5)

igbp.map.n50

ggsave(filename = paste0(plots.fp, '/IGBP/', 'IGBP_Alaska_Canada.png'),
       igbp.map.n50,
       width = 19, height = 9)
