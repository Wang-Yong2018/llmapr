% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llmapi.R
\name{get_json_chat_data}
\alias{get_json_chat_data}
\title{get large language model  chat json data for request}
\usage{
get_json_chat_data(
  user_input,
  select_model,
  history = NULL,
  max_tokens = MAX_TOKENS
)
}
\arguments{
\item{user_input}{the user input}

\item{select_model}{the selected model name}

\item{history}{the chat history}

\item{max_tokens}{the maximum allow tokens}
}
\value{
a chat request named list
}
\description{
get large language model  chat json data for request
}
\examples{
 # example 1 - first chat
 model_name = "mistralai/mistral-7b-instruct:free"
 user_input = 'hello ai world'
 post_body <- get_json_chat_data(user_input = user_input, select_mode=model_name, history=NULL )
 # example 2 - continual chat
 # TODO not full defined yet. may be append or object re-programe this function
}
