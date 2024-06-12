test_that("openrouter request", {
  openrouter_conn <- set_llm_conn()
  actual <- list(api_key = openrouter_conn$headers$Authorization,
                  url = openrouter_conn$url)

  openrouter_key <- paste0('Bearer ', Sys.getenv('OPENROUTER_API_KEY'))
  expect <- list(api_key = openrouter_key, url = "https://openrouter.ai/api/v1/chat/completions")
  expect_equal(actual, expect)
})
