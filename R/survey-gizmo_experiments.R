library(tidyverse)
library(stringr)
library(forcats)
library(httr)
library(jsonlite)
library(lubridate)


# API call functions -----------------------------------------------------------

sg_get_api_token <- function() {
  #' Get API key from dotfile.
  #' Should be saved as: 
  #' survey-gizmo_api-token=[...]
  #' survey-gizmo_api-secret=[...]
  api_token <- read_file('~/.ssh/.dotfile') %>% 
    str_split(pattern = '\n') %>% unlist() %>%
    str_split(pattern = '=', simplify = TRUE) %>% 
    {.[,2]} %>% {.[. != '']}
  return(api_token)
}


sg_apicall <- function(
  # survey id as string
  survey_id,
  # API information as list of length 2 (token and secret)
  api_token,
  # e.g. 'surveyquestion' or 'surveyresponse'
  api_object_class,
  api_url = 'https://restapi.surveygizmo.com',
  api_version = 'v5',
  api_calltype = 'survey') {
  
  #' Generates and executes an httr call to the surveygismo REST API.
  #' Takes survey_id, api_token, api_object to generate path.
  #' Assumes default version 5.
  #' 
  #' Further work to be done to access different kinds of objects,
  #' for example account information etc.
  
  # generate httr call
  # with surveyid
  response <- GET(
    url = api_url,
    path = paste(
      api_version,
      api_calltype,
      survey_id,
      api_object_class,
      sep = '/'),
    query = list(
      api_token = api_token[1],
      api_token_secret = api_token[2],
      resultsperpage = 50),
    progress())
  
  # check response
  stop_for_status(response)
  
  # parse response
  out <- content(response, as = 'text') %>% 
    jsonlite::fromJSON()
  return(out)
}


# Function to standardise question options
sg_clean_options <- function(df) {
  if (nrow(df) > 0) {
    out <- df %>% 
      ### TODO: Allow for different languages
      #select(id, value, starts_with('title')) %>%
      #select(1,2,3) %>%
      #set_names(c('o_id','o_value','o_text'))
      transmute(
        o_id = id,
        o_value = value
      )
  } else {
    out <- data.frame(
      o_id = NA,
      o_value = NA)
  }
  return(out)
}


# get survey
sg_get_survey <- function(
  survey_id,
  api_token) {
  #' Takes a survey id and api token
  #' to retrieve a dataframe of questions in the given survey
  #' along with question id and question type
  
  # get data from REST API
  response <- sg_apicall(
    survey_id = survey_id,
    api_token = api_token,
    api_object_class = 'surveyquestion')
  
  # get question information
  sg_questions <- data_frame(
    # question ID
    q_id = response$data$id,
    # question type, e.g. checkbox, radio
    q_type = response$data$type,
    # question text without HTML tags
    q_text = response$data$title$English %>%
      str_replace_all(pattern = '<.*?>', replacement = ''))
  
  # get option information
  sg_survey <- response$data$options %>% 
    # make lists of lists into dataframes
    map(flatten) %>%
    # standardise formatting
    map(sg_clean_options) %>%
    # make into list of dataframes
    enframe(name = 'index', value = 'options') %>%
    select(options) %>%
    # append to question information
    bind_cols(sg_questions, .) %>%
    # one row per option
    unnest(options) %>%
    filter(!is.na(o_id)) %>%
    # create option labels
    mutate(o_tag = str_c(
      'Q', str_pad(q_id, width = max(nchar(.$q_id)), pad = 0), '_',
      'O', str_pad(o_id, width = max(nchar(.$o_id)), pad = 0))) %>%
    select(
      q_id, q_type, q_text,
      o_id, o_tag, o_value)
  
  return(sg_survey)
}


sg_relabel_answers <- function(question_index, df_answers, df_survey) {
  ### TODO: Double-check that this returns correct factor levels
  assign('qi', question_index, envir = .GlobalEnv)
  
  # test question type
  df_question <- filter(df_survey, q_id == question_index)
  q_type <- unique(df_question$q_type)
  
  # check that column was found
  if ((df_answers %>% select(starts_with(question_index)) %>% ncol) == 0) {
    out <- rep(NA, nrow(df_answers))
    return(out)
  } else if (df_answers %>% 
             select(matches(str_c('^', question_index, '.*shown$'))) %>% 
             unlist %>% {!any(.)}) {
    out <- rep(NA, nrow(df_answers))
    return(out)
    
  } else if (q_type %in% c('RADIO', 'MENU')) {
    if (!str_c(question_index, '.answer_id') %in% colnames(df_answers)) {
      out <- rep(NA, nrow(df_answers))
      return(out)
    } else {
      # extract answers vector
      x <- df_answers %>% 
        pull(str_c(question_index, '.answer_id')) %>%
        as.character %>%
        as.factor
      # get label lookup vector
      str_labels <- pull(df_question, o_value)
      names(str_labels) <- pull(df_question, o_id)
      # execute recoding
      out <- do.call(dplyr::recode, c(list(x), str_labels))
      return(out)
    }
  } else if (q_type == 'CHECKBOX') {
    # get selected option from multiple choice question
    x <- df_answers %>%
      select(matches(str_c('^', question_index, '\\..*option$')))
    # get index number of options
    option_index <- colnames(x) %>% 
      str_split('\\.', simplify = TRUE) %>% {.[,3]}
    # set names with a string generated from question and option index
    out <- set_names(x, str_c(
      'Q', str_pad(question_index, width = 3, pad = 0),
      'O', str_pad(option_index, width = 5, pad = 0)))
    return(out)
  }
}


sg_relabel_answers <- function(question_index, df_answers, df_survey) {
  ### TODO: Double-check that this returns correct factor levels
  assign('qi', question_index, envir = .GlobalEnv)
  
  # test that answer data is valid
  # check that column was found
  if ((df_answers %>% select(starts_with(question_index)) %>% ncol) == 0) {
    out <- rep(NA, nrow(df_answers)); return(out)}
  if (df_answers %>% select(matches(str_c('^', question_index, '.*shown$'))) %>% 
    unlist %>% {!any(.)}) {
    out <- rep(NA, nrow(df_answers)); return(out)}
  
  # test question type from df_survey
  df_question <- filter(df_survey, q_id == question_index)
  
  
  
  
  
  # TODO: continue here
  
  # test question type from df_answers
  q_type <- select(df_answers, starts_with(str_c(question_index, '.type')))
  if (ncol(q_type) == 0) {
    q_type <- 'unknown'
  } else {
    q_type <- q_type %>% filter(complete.cases(.)) %>% pull(1) %>% unique
  }
  #'TABLE', 'IMAGE_SELECT', 'MULTI_TEXTBOX', 'RANK', 'HIDDEN', 'parent'
  print(q_type)

  
  
  
  # Single choice questions
  if (q_type %in% c('RADIO', 'MENU')) {
    # extract answers vector
    x <- df_answers %>% 
      pull(str_c(question_index, '.answer_id')) %>%
      as.character %>%
      as.factor
    # get label lookup vector
    str_labels <- pull(df_question, o_value)
    names(str_labels) <- pull(df_question, o_id)
    # execute recoding
    out <- do.call(dplyr::recode, c(list(x), str_labels))
    return(out)
  
  # Multiple choice questions
  } else if (q_type %in% c('CHECKBOX')) {
    x <- df_answers %>%
      select(matches(str_c('^', question_index, '\\..*option$')))
    # get index number of options
    option_index <- colnames(x) %>% 
      str_split('\\.', simplify = TRUE) %>% {.[,3]}
    # set names with a string generated from question and option index
    out <- set_names(x, str_c(
      'Q', str_pad(question_index, width = 3, pad = 0),
      'O', str_pad(option_index, width = 5, pad = 0)))
    return(out)
  
  # Hierarchical questions
  } else if (q_type %in% c('parent')) {
    out <- rep(NA, nrow(df_answers))
    return(out)
  }
}








# get responses
sg_get_responses <- function(
  survey_id,
  api_token) {
  #' This function takes a survey id, api token, and a list of questions
  #' to retrieve a dataframe of responses to the given survey
  
  # get survey structure from REST API
  df_survey <- sg_get_survey(survey_id, api_token)
  
  # get response data from REST API
  api_resp <- sg_apicall(
    survey_id = survey_id,
    api_token = api_token,
    api_object_class = 'surveyresponse')
  
  # transform participant level general information
  df_participants <- api_resp$data %>%
    select(which(!map(api_resp$data, class) %in% c('list', 'data.frame'))) %>%
    as_tibble() %>%
    transmute(
      resp_id = id,
      resp_status = status,
      resp_test = is_test_data,
      resp_time_started = date_started %>% lubridate::as_datetime(),
      resp_time_finished = date_submitted %>% lubridate::as_datetime(),
      resp_time_diff = response_time,
      resp_linkid = link_id,
      resp_referrer = referer,
      resp_useragent = user_agent,
      resp_country = country,
      resp_city = city,
      resp_region = region,
      # resp_postcode = postal,
      # resp_lat = latitude,
      # resp_lon = longitude,
      # resp_ip = ip_address,
      resp_dma = dma)
  
  # transform question level information
  df_answers <- api_resp$data$survey_data %>% flatten()
  
  # question ids
  i_questions <- df_survey$q_id %>% unique %>% as.character
  
  bla <- map(
    .x = i_questions,
    .f = sg_relabel_answers,
    df_answers,
    df_survey)
  
  
  
  
  
  
  
  # bind participant info and answers
  
  return(df_responses)
}











# Testing ----------------------------------------------------------------------

# Get API token and secret from .dotfile
api_token <- sg_get_api_token()

survey_id = '4253549' # Banking survey
survey_id = '4274510' # Test survey


# %% get survey --------------------
# run function to retrieve questions
df_survey <- sg_get_survey(
  survey_id = survey_id,
  api_token = api_token)

# # Single choice vs multiple choice
# q_type_singlechoice <- c('RADIO', 'MENU', 'NPS')
# q_type_multiplechoice <- c('CHECKBOX', 'RANK')
# 
# df_single_choice <- df_survey %>%
#   filter(q_type %in% q_type_singlechoice)
# 
# df_multiple_choice <- df_survey %>%
#   filter(q_type %in% q_type_multiplechoice)


xx <- df_survey %>% 
  nest(starts_with('o'), .key = 'options')
  
sg_relabel_answers(x, '5', df_survey)



# %% get responses -------------------------------------------------------------






# Code bin -------------------------




# # get ranking where appropriate
# bind_cols(
#   'option' = select(out, -ends_with('.rank')) %>% t,
#   'rank' = select(out, ends_with('.rank')) %>% t)

# 
# 
# 
# # bla <- function(q_id, response) {
# #   # convert numeric q_id to character
# #   q_id <- as.character(q_id)
# #   # use q_id to extract raw responses to question
# #   x <- response$data$survey_data[as.character(q_id)]
# #   
# #   
# #   return(x)
# # }
# 
# 
# 
# map(.x = qs, .f = bla, x)
# 
# x$data$survey_data
# 
# 
# x$data$survey_data %>%
#   pull('31') %>%
#   flatten() %>%
#   select(id, starts_with('answer'))
# 
# 
# tibble(
#   response_id = x$data$id,
#   response_status = x$data$status,
#   response_test = x$data$
# )
# 
# 





















