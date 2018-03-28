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

# Test survey
survey_id = '4274510' 


# API call functions -----------------------------------------------------------

# get survey
sg_get_questions <- function(
  survey_id,
  api_token) {
  #' This function takes a survey id and api token
  #' to retrieve a dataframe of questions in the given survey
  #' along with question id and question type
  
  # generate httr call for survey subobject
  # with surveyid
  response <- GET(
    url = 'https://restapi.surveygizmo.com',
    path = paste('v5/survey', surveyid, 'surveyquestion', sep = '/'),
    query = list(
      api_token = api_token[1],
      api_token_secret = api_token[2]),
    progress())
  # check response
  warn_for_status(response)
  stop_for_status(response)
  # parse response
  x <- content(response, as = 'text') %>% 
    jsonlite::fromJSON()
  
  # assumes english responses for now
  ### TODO: allow for language selection
  
  # get question information
  sg_questions <- data_frame(
    # question ID
    q_id = x$data$id,
    # question type, e.g. checkbox, radio
    q_type = x$data$`_subtype`,
    # question text without HTML tags
    q_text = x$data$title$English %>%
      str_replace_all(pattern = '<.*?>', replacement = ''))
  
  return(sg_questions)
}


# get responses
sg_get_responses <- function(
  survey_id,
  api_token) {
  #' This function takes a survey id and api token
  #' to retrieve a dataframe of responses to the given survey
  
  # generate httr call
  # with surveyid
  response <- GET(
    url = 'https://restapi.surveygizmo.com',
    path = paste('v5/survey', surveyid, 'surveyresponse', sep = '/'),
    query = list(
      api_token = api_token[1],
      api_token_secret = api_token[2]),
    progress())
  # check response
  warn_for_status(response)
  stop_for_status(response)
  # parse response
  x <- content(response, as = 'text') %>% 
    jsonlite::fromJSON()

}






# Get survey level info
df_survey <- with(x$data,
  data_frame(
    sur_id = id,
    sur_title = title,
    sur_type = type,
    sur_forward_only = forward_only,
    sur_date_created = created_on %>% lubridate::as_datetime(),
    sur_date_modified = modified_on %>% lubridate::as_datetime(),
    sur_status = status
  )
)









# Testing ----------------------------------------------------------------------

# run function to retrieve questions
df_questions <- sg_get_questions(
  survey_id = survey_id,
  api_token = api_token)





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



