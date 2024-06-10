# box::use(httr2[request, req_cache, req_progress,
#                req_perform, req_perform_stream,
#                last_response,
#                resp_status,req_retry,req_error,req_timeout, req_dry_run,
#                resp_check_status,resp_is_error,
#                req_body_json, req_user_agent,req_headers,
#                req_url_query, req_url_path_append,
#                resp_body_json],
#          jsonlite[fromJSON, toJSON])
#
# box::use(purrr[map_dfr, pluck])

# box::use(dplyr[as_tibble])
# box::use(stringr[str_extract,str_glue])
# box::use(rlang[abort,warn])
# box::use(base64enc[base64encode])
# box::use(../etl/agent_sql[get_db_schema])
# box::use(../global_constant[app_name,app_language,                          img_vision_prompt, MAX_TOKENS,
#                            model_id_list,vision_model_list,
#                            global_seed,timeout_seconds,
#                            i18n])
#
# box::use(logger[log_info, log_warn,
#                 log_debug, log_error,
#                 INFO, DEBUG, WARN,ERROR,OFF])
# TIMEOUT_SECONDS = 30
#
# # cache_dir <- cache_disk("./cache",max_age = 3600*24)
source('R/global_constant.R')

#' Set connection to large language model api service
#'
#' @param url  the url of model, default value is openrouter service
#'          note: the api_key of should be existed in system environment and named as OPENROUTER_API_KEY
#' @param timeout_seconds , default value is 30
#'
#' @return a htt2r request object.
#'
#' @export
#'
#' @examples
#'  conn <- set_llm_conn()
#'
set_llm_conn <- function(
    url = "https://openrouter.ai/api/v1/chat/completions",
    timeout_seconds=api_timeout_seconds ) {
  api_key = Sys.getenv('OPENROUTER_API_KEY')

  req <- httr2::request(url) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_headers(Authorization = paste0('Bearer ', api_key)) |>
    # req_cache(tempdir(), debug = TRUE) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 1) |>
    httr2::req_progress() |>
    httr2::req_user_agent('shiny_ai')

  return(req)
}



get_select_model_name <- function(model_id) {
  select_model = switch(model_id,
                       gpt35 = "openai/gpt-3.5-turbo-0125",
                       gpt4o = "openai/gpt-4o",
                       # gpt4v = "openai/gpt-4-vision-preview",
                       'llama' = 'meta-llama/llama-3-8b-instruct:free',
                       mistral = 'mistralai/mistral-7b-instruct:free',
                       gemini = "google/gemini-pro-1.5",
                       llama = 'meta-llama/llama-3-8b-instruct',
                       claude3s = 'anthropic/claude-3-sonnet:beta',
                       mixtral = 'mistralai/mixtral-8x7b-instruct',
                       deepseekv2 = 'deepseek/deepseek-chat',
                       phi = "microsoft/phi-3-medium-128k-instruct:free",
                      "microsoft/phi-3-medium-128k-instruct:free"
                       )
  return(select_model)
}

get_chat_history <- function(message, role='user', last_history=NULL){
  # this function is designed to hangle the chat history
  # case 1: initial chat,
    #   setup system role and  prompt
    #   append the user role and prompt
  # case 2: record ai response message
    #  append the assistant role and message
  # case 3: 2nd, 3rd  and nth user input append
    # append the user role and message

  # If last_history is NULL, initialize the chat history
  if (is.null(last_history)) {
    last_history <- list(list(
      role = 'system',
      content =  i18n$translate("You are a helpful AI assitan")
    ))

  }
  new_history <- last_history
  new_history <- c(new_history, list(list(role = role, content = message)))

  return(new_history)
}


get_json_data <- function(user_input,select_model){


  # # prepare the configure
  # json_generationConfig = list( temperature = 0.5,
  #                               maxOutputTokens = 1024)
  # prepare the data
  json_contents <- list(list(role = 'user', content = user_input))

  json_data <- list(model = select_model, messages = json_contents)
  return(json_data)
}


#' get large language model  chat json data for request
#'
#' @param user_input  the user input
#' @param select_model  the selected model name
#' @param history the chat history
#' @param max_tokens  the maximum allow tokens
#'
#' @return a chat request named list
#' @export
#'
#' @examples
#'  # example 1 - first chat
#'  model_name = "mistralai/mistral-7b-instruct:free"
#'  user_input = 'hello ai world'
#'  post_body <- get_json_chat_data(user_input = user_input, select_mode=model_name, history=NULL )

#'  # example 2 - continual chat
#'  # TODO not full defined yet. may be append or object re-programe this function

get_json_chat_data <- function(user_input, select_model, history=NULL, max_tokens = MAX_TOKENS){
  # this function is used for short memory conversation.
  # The ai could remember what user said and conversation based on history topic'

  # # prepare the configure
  # json_generationConfig = list(temperature = 0.5,
  #                              # maxOutputTokens = 1024
  #                              )
  # prepare the data
  # user_message <- list(role = 'user',content=user_input)


  json_contents <- get_chat_history(user_input, role = 'user', history)
  json_data <- list(
    model = select_model,
    messages = json_contents,
    seed = global_seed,
    max_tokens = MAX_TOKENS,
    temperature = 1#,
    #top_k = 0.1
  )

  return(json_data)
}

get_json_img <- function(user_input, img_url, select_model,image_type='file', max_tokens = MAX_TOKENS){

  # # prepare the configure
  # json_generationConfig = list( temperature = 0.5,
  #                               maxOutputTokens = 1024)
  image_content = switch(
    image_type,
    file = paste0("data:image/jpeg;base64,", base64enc::base64encode(img_url)),
    url = img_url,
    img_url
  )

  # prepare the data
  json_contents <- list(list(role = 'user', content = list(
    list(type = 'text', text = user_input),
    list(
      type = 'image_url',
      image_url = list(url = image_content, detail =
                         'auto')
    )
  )))
  json_data <- list(model = select_model,
                    messages = json_contents,
                    max_tokens = max_tokens)
  return(json_data)
}

get_json_agent <- function(user_input, select_model,funcs_json){

  # prepare the configure
  # json_generationConfig = list( temperature = 0.5,
  #                               maxOutputTokens = 1024)
  # prepare the data
  json_contents <- list(list(role = 'user', content = user_input))

  #func_json
  # TODO: there should be some code to validate the json of function
  # for function_call, the google gemini api is not full compatible with chatgpt.
  # so I need remove the extra field chinese_name
  funcs_json

  if (grepl('gemini', select_model)) {
    gemini_func_json <- list(function_declarations = funcs_json)
    json_data <- list(
      model = select_model,
      messages = json_contents,
      functions = gemini_func_json,
      tool_config = list(function_calling_config = 'AUTO')
    )

  } else {
    json_data <- list(
      model = select_model,
      messages = json_contents,
      functions = funcs_json,
      function_call = 'auto'
    )

  }
  return(json_data)
}



get_llm_post_data <- function(prompt = 'hi',
                              history = NULL,
                              llm_type = 'chat',
                              model_id = 'llama',
                              img_url = NULL,
                              funcs_json = NULL,
                              max_tokens = MAX_TOKENS) {

  # select the model
  select_model <- get_select_model_name(model_id)

  # select the post_body
  post_body <- switch(
    llm_type,
    chat = get_json_chat_data(
      user_input = prompt,
      select_model = select_model,
      history = history,
      max_tokens = max_tokens
    ),
    sql = get_json_chat_data(
      user_input = prompt,
      select_model = select_model,
      history = history
    ),
    answer = get_json_data(user_input = prompt, select_model =
                             select_model),
    img_url = get_json_img(
      user_input = prompt,
      img_url = img_url,
      select_model = select_model,
      image_type = 'url',
      max_tokens = max_tokens
    ),
    img = get_json_img(
      user_input = prompt,
      img_url = img_url,
      select_model = select_model,
      image_type = 'file',
      max_tokens = max_tokens
    ),
    #func=get_json_func(user_input=prompt,select_model=select_model,history=history),
    agent = get_json_agent(
      user_input = prompt,
      select_model = select_model,
      funcs_json = funcs_json
    ),
    get_json_data(user_input = prompt, select_model =
                    select_model)
  )
  return(post_body)
}


#' Get LLM response by sent request support chat, image, and agent function.
#' @param prompt, the user input of prompt. It should be prompt engineering based on feature type(chat, image, and
#' agent). It must be provided.
#'
#' @param img_url, for image llm  type , it should be the image file path.
#' @param model_id, it is a model short name, for easy input, the inside function will convert it to full model name
#' @param llm_type, its should be one of item in the list c('chat','sql','answer','img_url','img','agent')
#'                  note:
#'                  chat: for normal large language chatting. It include history feature.
#'                  sql: for one time, text to sql feature. It convert natural language quesion to sql code
#'                  answer: for role based Q&A. It not full implemented yet.
#'                  img: for provide local image file path , for resize and encoding , sent to llm for analyzing. At
#'                        present, only gemini, and gpt-4v support only
#'                  img_url mean remote image url . The img_url not support yet.
#'                  agent: for extract desired information from  user prompt  by predefined json template.
#'
#' @param history, for llm type : chat, it is a list store previously chat between user and model
#' @param funcs_json, for llm type: agent, it is the list of json template for extract information.
#' @param timeout_seconds, for timeout settting purpose.
#'
#' @export
#'
#' @examples
#' # example for chat
#'
#' # example for image analyzing
#'
#' # example for agent fucntion
#'
#' # example for sql text 2 sql
#'
get_llm_result <- function(prompt='hello,who are you',
                           img_url=NULL,
                           model_id='llama',
                           llm_type='chat',
                           history=NULL,
                           funcs_json=NULL,
                           timeout_seconds=api_timeout_seconds){
  post_body <- get_llm_post_data(
    prompt = prompt,
    history = history,
    llm_type = llm_type,
    model_id = model_id,
    img_url = img_url,
    funcs_json = funcs_json
  )
  logger::log_debug(paste(' the llm post data is ===> ', post_body, sep =
                            '\n'))
  request <-
    set_llm_conn(timeout_seconds = timeout_seconds) |>
    httr2::req_body_json(data = post_body, type = "application/json")


  response_message <-  get_stream_data(request,timeout_sec=api_timeout_seconds)

  logger::log_debug(response_message)
  # print(response_message)
  return(  response_message)
}


check_llm_connection <- function() {
  # out of date

 #  is_connected =FALSE
 #
 #  resp <- get_llm_result()
 #
 #  # Check the response status code (should be 200 for success)
 #  if (resp|>resp_status() == 200) {
 #    cat(paste0(url,' ', "Connection successful!\n"))
 #    is_connected=TRUE
 #  } else {
 #    cat(paste0(url,' ', "Connection failed with status code:", response$status, "\n"))
 #    is_connected=TRUE
 #  }
 # return(is_connected)
}

llm_chat <- function(user_input, model_id='llama', history=NULL){

  # select_model is a fake model_info, it will be actual assigned in get_llm_result function.
  chat_history <- get_json_chat_data(user_input = user_input,
                                     select_model = 'llama',
                                     history = history)

  response_message <- get_llm_result(
    prompt = user_input,
    model_id = model_id,
    history = chat_history,
    llm_type = 'chat'
  )

  chat_history$messages <- append(chat_history$messages, list(response_message))

  return(chat_history)
}




get_ai_result <- function(ai_response,ai_type='chat', parameter = NULL){

  ai_message <- ai_response |> purrr::pluck('choices', 1, 'message')
  ai_model_name <- ai_response |> purrr::pluck('model')
  logger::log_debug(paste0('the get_ai_result function ai_message is======>', ai_message))
  finish_reason <- ai_response |> purrr::pluck('choices', 1, 'finish_reason') |>
    tolower()

  ai_result <- switch(
    tolower(finish_reason),
    # chat_type
    stop = list(role = ai_message$role, content = ai_message$content),
    chat.completion = list(role = ai_message$role, content =
                             ai_message$content),
    end_turn = list(role = ai_message$role, content =
                      ai_message$content),
    error = list(role = ai_message$role, content = ai_message$content),
    function_call = list(role = ai_message$role, content =
                           ai_message$function_call),
    max_tokens = list(
      role = ai_message$role,
      content = paste0(
        ai_message$content,
        '\n--##',
        i18n$translate('finish reason'),
        ':',
        finish_reason
      )
    ),
    #sql_query=list(role=ai_message$role, content=list(name='sql_query',arguments=ai_message$content)),
    list(
      role = ai_message$role,
      content = paste0(
        ai_message$content,
        '\n--##',
        i18n$translate('finish reason'),
        ':',
        finish_reason
      )
    )
  )
  logger::log_debug(ai_result)
  if (ai_type %in% c('sql_query', 'dot', 'sql') &
      finish_reason %in% c('stop', 'function_call', 'max_tokens')) {
    code <- ai_message$content
    if (grepl('ERROR', code)) {
      code <- paste0('-- ', code)
    }
    db_id <- parameter$db_id
    model_id <- ai_model_name
    generate_time <- ai_response$created |> as.POSIXct() |> as.character()

    ai_result <- list(role = ai_message$role,
                      content = list(
                        name = 'sql_query',
                        arguments = list(
                          db_id = db_id,
                          sql_query = code,
                          model_id = model_id,
                          generated_time = generate_time
                        )
                      ))

  }
  # ai_result <-list(
  #   role=ai_message$role,
  #   content=list(name='sql_query',arguments=code))


  logger::log_debug(paste0('the ai message result is ====>', ai_result))
  return(ai_result)
}


#' Get streamed data from large language model rest api server in chunk mode.
#'
#' @param req  the httr2 request instance and llm filled post json data
#' @inheritParams httr2::req_perform_stream

#' @return a openrouter format response json message which will be convert to named list
#' normal response message is below:
#'  https://openrouter.ai/docs/responses#response-body
#'
#'  For timeout case, and http response error case, only role and message will be fill meaningful string.#'
#'
#' openrouter http response error code map
#'
#' 400: Bad Request (invalid or missing params, CORS)
#' 401: Invalid credentials (OAuth session expired, disabled/invalid API key)
#' 402: Your account or API key has insufficient credits. Add more credits and retry the request.
#' 403: Your chosen model requires moderation and your input was flagged
#' 408: Your request timed out
#' 429: You are being rate limited
#' 502: Your chosen model is down or we received an invalid response from it
#' 503: There is no available model provider that meets your routing requirements
#'
#' @export
#'
#' @examples
#'  library(httr2)
#'  library(llmapr)
#'
#'  model_name = "mistralai/mistral-7b-instruct:free"
#'  user_input = 'hello ai world'
#'  post_body <- get_json_chat_data(user_input = user_input, select_mode=model_name )
#'
#'  req<-
#'    set_llm_conn() |>
#'    req_body_json(data = post_body, type = "application/json")
#'  req |>  get_stream_data()
#'
#'
#'
get_stream_data <- function(req,timeout_sec=30, buffer_kb=2){
  # Initialize an empty list to store the streamed data
  streamed_data <- list()

  # Define a callback function to process each chunk of data
  process_chunk <- function(chunk) {
    # Convert the raw chunk to character
    # Process the JSON data (assuming each chunk is a complete JSON object)

    logger::log_debug("Got ", length(chunk), " bytes\n", sep = "")
    text <-  rawToChar(x = chunk)
    # Append the data to the external list
    streamed_data <<- append(streamed_data, text)

    # Return TRUE to continue streaming
    TRUE
  }

  # After streaming request
  response <-
    try(req |>  httr2::req_perform_stream(
      process_chunk,
      timeout_sec = timeout_sec,
      buffer_kb = buffer_kb,
      round = 'line'
    )

    )

  if ('try-error' %in% class(response)) {


    error_message <- response |> errorCondition()
    #error_message <- response |> httr2::resp_status_desc()
    #http_code <- response |> httr2::resp_status()
    #error_code <- paste0('HTTP ',http_code)
    response_message <- list(model = NULL, choices = list(list(
      message = list(role = 'error', content = error_message),
      finish_reason = 'time out'
    )))
    logger::log_error(paste0('get_stream_data failed, the reason is: ==',error_message))

  } else {
    if (httr2::resp_is_error(response)) {
      error_message <- response |> httr2::resp_status_desc()
      http_code <- response |> httr2::resp_status()
      error_code <- paste('HTTP ', http_code, ':', error_message)

      response_message <- list(model = NULL,
                               choices = list(list( message = list(role = 'error',
                                                                   content = error_code),
                                                    finish_reason = error_message
                                                    )
                                              )
                               )
    logger::log_error(paste0('http response error, the reason is: ==',error_code))
    } else {
      logger::log_debug(streamed_data)
      response_message <-
        streamed_data |>
        paste0(collapse = '') |>
        jsonlite::fromJSON(txt = _,
                           simplifyVector = FALSE )#|>
    }
  }
  logger::log_debug(paste('the response data is ===> ',
                         response_message ,
                         sep = '\n')
                   )
  # Access the streamed data
  return(response_message)

}
