
# Language Configuration - English or Chinese
app_language = 'en'
i18n <- shiny.i18n::Translator$new(translation_csvs_path = "inst/extdata/translation/",
                                   translation_csv_config = 'inst/extdata/translation/config.yaml')
i18n$set_translation_language(app_language)
app_name <- i18n$translate('llmapr')


# Set up logging
# if (!requireNamespace("R.utils", quietly = TRUE)) {
#   install.packages("R.utils")
# }
# renv::restore()
# box::use(logger[log_info, log_warn,
#                 log_debug, log_error,
#                 log_threshold,log_formatter,formatter_pander,formatter_json,
#                 log_layout,layout_json_parser,
#                 log_appender, appender_file,
#                 INFO, DEBUG, WARN,ERROR,OFF])
# Log file configuration

logger::log_formatter(logger::formatter_json)
logger::log_layout(logger::layout_json_parser(fields = c('time', 'level', 'fn', 'pid')))
logger::log_threshold(logger::INFO, namespace = "global")
# log_file <- './llmapr.log'
# logger::log_appender(logger::appender_file(log_file,max_line=1000,max_files = 3L))
# language settting
#Sys.setlocale("LC_ALL", 'Chinese (Simplified)_China.utf8')

# 英文环境
#Sys.setlocale(category = "LC_ALL",locale = "English_United States")
#Sys.setlocale(category = "LC_ALL",locale = "Chinese (Simplified)_China.utf8")



IS_DEBUG <- FALSE

# model list configuration
model_id_list <- c('gpt35','gemini','llama','claude3s','mixtral','deepseekv2','phi','gpt3v','gpt4o')
sql_model_id_list <- model_id_list
vision_model_list <- c('gpt4v','gemini')
MAX_TOKENS <- 1000 # the max token allowed
global_seed <- 42 # the model random seed preconfig. With this config, the model result will be reproduciable. Otherwise, it will be random
api_timeout_seconds <- 60 # the maxium wait time for connection between model restapi server and local.

# database list configuration
db_id_list <- c('music', 'dvd_rental', 'hospital')
# db_chinook_url <- './data/chinook.db'
db_url_map <- list(
  music = 'inst/extdata/chinook.db',
  dvd_rental = 'inst/extdata/sakila_1.sqlite',
  academic = 'inst/extdata/academic.sqlite',
  hospital = 'inst/extdata/hospital_1.sqlite'
)

# imag analylsize prompt configuration
img_vision_prompt <- "As a image tag and classification expert, pls help to analyse the image. Provide follow output:\n1. main topic tag and elements list\n2. using markdown format\n"

# Probe databse related configuration
# the maximum sql query result row number
sql_agent_config_file <- 'inst/extdata/sql_agent_prompt.txt'
max_sql_query_rows <- 100

