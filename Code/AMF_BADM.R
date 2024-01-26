library(amerifluxr)
library(flextable)
library(ftExtra)

setwd('../')
git.fp = getwd()
data.fp = paste(git.fp, 'Data', sep = '/')

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
  