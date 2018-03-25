library(tidyverse)
library(stringr)
library(forcats)
library(httr)
library(jsonlite)

# Get API key from dotfile.
# Should be saved as: 
# survey-gizmo_api-token=[...]
# survey-gizmo_api-secret=[...]
api_token <- read_file('~/.ssh/.dotfile') %>% 
  str_split(pattern = '\n') %>% unlist() %>%
  str_split(pattern = '=', simplify = TRUE) %>% 
  {.[,2]} %>% {.[. != '']}


# Get survey ----

# Test survey
surveyid = '4274510' 
# generate httr call
# with surveyid
response <- GET(
  url = 'https://restapi.surveygizmo.com',
  path = paste('v5/survey', surveyid, sep = '/'),
  query = list(
    api_token = api_token[1],
    api_token_secret = api_token[2]),
  progress())

# check response
warn_for_status(response)
stop_for_status(response)
# parse response
x <- content(response, as = 'text') %>% 
  fromJSON(
    flatten = FALSE,
    simplifyVector = FALSE,
    simplifyMatrix = FALSE,
    simplifyDataFrame = TRUE)

# assume english responses for now
### TODO: allow for language selection

# Get survey level info
df_survey <- with(x$data,
  data_frame(
    id = id,
    title = title,
    type = type,
    forward_only = forward_only,
    date_created = created_on %>% lubridate::as_datetime(),
    date_modified = modified_on %>% lubridate::as_datetime(),
    status = status
  )
)










# Testing ----------------------------------------------------------------------

surveyid = '4253549' # Banking survey
surveyid = '4274510' # Test survey


# generate httr call
response <- GET(
  url = 'https://restapi.surveygizmo.com/v5/survey', 
  query = list(
    api_token = api_token[1],
    api_token_secret = api_token[2]))

# check response
warn_for_status(response)
stop_for_status(response)

# parse response using jsonlite
x <- content(response, as = 'text') %>% 
  fromJSON(flatten = FALSE)

x$data %>% as_data_frame()



