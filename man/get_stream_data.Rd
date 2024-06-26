% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llmapi.R
\name{get_stream_data}
\alias{get_stream_data}
\title{Get streamed data from large language model rest api server in chunk mode.}
\usage{
get_stream_data(req, timeout_sec = 30, buffer_kb = 2)
}
\arguments{
\item{req}{the httr2 request instance and llm filled post json data}

\item{timeout_sec}{Number of seconds to process stream for.}

\item{buffer_kb}{Buffer size, in kilobytes.}
}
\value{
a openrouter format response json message which will be convert to named list
normal response message is below:
 https://openrouter.ai/docs/responses#response-body

 For timeout case, and http response error case, only role and message will be fill meaningful string.#'

openrouter http response error code map

400: Bad Request (invalid or missing params, CORS)
401: Invalid credentials (OAuth session expired, disabled/invalid API key)
402: Your account or API key has insufficient credits. Add more credits and retry the request.
403: Your chosen model requires moderation and your input was flagged
408: Your request timed out
429: You are being rate limited
502: Your chosen model is down or we received an invalid response from it
503: There is no available model provider that meets your routing requirements
}
\description{
Get streamed data from large language model rest api server in chunk mode.
}
\examples{
 library(httr2)
 library(llmapr)

 model_name = "mistralai/mistral-7b-instruct:free"
 user_input = 'hello ai world'
 post_body <- get_json_chat_data(user_input = user_input, select_mode=model_name )

 req<-
   set_llm_conn() |>
   req_body_json(data = post_body, type = "application/json")
 req |>  get_stream_data()



}
