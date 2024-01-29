library(amerifluxr)
library(flextable)
library(ftExtra)
library(dplyr)
library(sf)
library(mapview)

setwd('../')
git.fp = getwd()
data.fp = paste(git.fp, 'Data', sep = '/')

# Get site info
amf.sitelist = amf_sites()
amf.site.ids = amf.sitelist$SITE_ID
amf.site.locs = amf.sitelist[,c(1,3,10,9)]
colnames(amf.site.locs) = c('SiteID','Country','lon','lat')

# Convert lat/lon locations simple feature (sf)
amf.site.locs.sf = st_as_sf(x = amf.site.locs,
                            coords = c('lon','lat'),
                            crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# Generate interactive map
mapview(amf.site.locs.sf, zcol = 'Country')

# Get BIF
bif = amf_download_bif(user_id = 'cjdevine',
                       user_email = 'cjdevine@arizona.edu',
                       data_policy = "CCBY4.0",
                       agree_policy = TRUE,
                       intended_use = "remote_sensing",
                       intended_use_text = "Synthesis work with Dave Moore",
                       out_dir = data.fp)

# Open BADM
badm = amf_read_bif(file = bif)


# ------------------- US-NR1

# Subset US-NR1 info from BADM
nr1.badm = badm[badm$SITE_ID == 'US-NR1',]

# Get list of unique variables 
nr1.badm.vars = unique(nr1.badm$VARIABLE)

# Subset by variable and date
nr1.badm.vars.dates = as.data.frame(nr1.badm[grepl('DATE$', nr1.badm$VARIABLE),])

# Create summary table
table = flextable(nr1.badm.vars.dates[,c(3,5)]) %>%
  merge_v(j = c(1,2))



# Filter out zero values and non-numeric values in DATAVALUE
non_zero_values = badm %>%
  filter(as.numeric(DATAVALUE) != 0, !is.na(as.numeric(DATAVALUE)))

# Group by VARIABLE_GROUP, count non-zero observations, and sort the summary
summary_table = non_zero_values %>%
  group_by(VARIABLE_GROUP) %>%
  summarise(NonZeroCount = n(), .groups = "drop") %>%
  arrange(desc(NonZeroCount))  # Add this line to sort in descending order

# Display the summary table in a clean and elegant format
print(summary_table)


