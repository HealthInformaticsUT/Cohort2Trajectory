# Download one cohort definition. Header values stay in this call and never
# enter saved settings or diagnostics. No interactive authentication is started.
.getCohortDefinition <- function(cohortId, baseUrl, headers = NULL) {
  if (!is.numeric(cohortId) || length(cohortId) != 1L || is.na(cohortId) ||
      !is.finite(cohortId) || cohortId < 0 || cohortId != floor(cohortId)) {
    stop("cohortId must be one nonnegative integer.", call. = FALSE)
  }
  if (!is.character(baseUrl) || length(baseUrl) != 1L || is.na(baseUrl) ||
      !grepl("^https?://", baseUrl, ignore.case = TRUE)) {
    stop("baseUrl must be an HTTP(S) WebAPI URL, or use NULL for local JSON definitions.", call. = FALSE)
  }
  if (!is.null(headers) && (!is.character(headers) || is.null(names(headers)) ||
      anyNA(headers) || anyNA(names(headers)) || any(!nzchar(names(headers))) ||
      anyDuplicated(tolower(names(headers))))) {
    stop("webApiHeaders must be a named character vector with unique header names.", call. = FALSE)
  }
  request <- httr2::request(paste0(sub("/+$", "", baseUrl), "/cohortdefinition/", cohortId))
  request <- httr2::req_timeout(request, 60)
  if (length(headers)) request <- do.call(httr2::req_headers, c(list(request), as.list(headers)))
  object <- tryCatch({
    response <- httr2::req_perform(request)
    httr2::resp_body_json(response, simplifyVector = FALSE)
  }, error = function(e) {
    status <- if (inherits(e, "httr2_http") && length(e$status) == 1L) {
      paste0(" (HTTP ", e$status, ")")
    } else ""
    stop("Could not download ATLAS cohort ", cohortId, status,
      ". Check the WebAPI URL, network and access permissions. For an authenticated endpoint, ",
      "supply webApiHeaders. Alternatively export the cohort JSON from ATLAS and use baseUrl = NULL.",
      call. = FALSE)
  })
  expression <- if (is.list(object)) object$expression else NULL
  if (is.character(expression) && length(expression) == 1L) {
    expression <- tryCatch(jsonlite::fromJSON(expression, simplifyVector = FALSE),
      error = function(e) NULL)
  }
  if (!is.list(expression) || is.null(names(expression)) || !length(expression)) {
    stop("ATLAS cohort ", cohortId, " did not contain a valid cohort expression.", call. = FALSE)
  }
  list(expression = expression)
}
