test_that("WebAPI download preserves expressions and keeps headers in the request", {
  expression <- fixtureExpression()
  payload <- list(expression = jsonlite::toJSON(expression, auto_unbox = TRUE))
  requested <- NULL
  local_mocked_bindings(req_perform = function(req) {
    requested <<- req
    httr2::response(200L, headers = list("content-type" = "application/json"),
      body = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE)))
  }, .package = "httr2")
  result <- .getCohortDefinition(12L, "https://atlas.example/WebAPI/",
                                 headers = c(Authorization = "Bearer fixture-token"))
  expect_equal(result$expression, expression)
  expect_identical(requested$url, "https://atlas.example/WebAPI/cohortdefinition/12")
  expect_true("Authorization" %in% names(requested$headers))
  expect_false(any(grepl("fixture-token", unlist(result))))
  expect_error(.getCohortDefinition(12L, "not-a-url"), "HTTP")
})


test_that("WebAPI errors identify HTTP status without exposing credentials", {
  local_mocked_bindings(req_perform = function(req) {
    httr2::resp_check_status(httr2::response(401L))
  }, .package = "httr2")
  error <- tryCatch(.getCohortDefinition(12L, "https://atlas.example/WebAPI",
    headers = c(Authorization = "Bearer fixture-token")), error = identity)
  expect_match(conditionMessage(error), "HTTP 401", fixed = TRUE)
  expect_match(conditionMessage(error), "baseUrl = NULL", fixed = TRUE)
  expect_false(grepl("fixture-token", conditionMessage(error), fixed = TRUE))
})
