remove(list = ls())
path <- dirname(rstudioapi::getSourceEditorContext()$path)

source(paste(path, 'settings.R', sep='/'))

source(paste(tools, 'format_data.R', sep='/'))

# If births files have not been generated run the following line BIRTHS
source(paste(prerequisites, 'births.R', sep='/'))

# If regions.RDS not in processed data file, run the following line: 
source(paste(prerequisites, 'regional_subdivision.R', sep='/'))
