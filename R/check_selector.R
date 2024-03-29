#' Check selector `dataname` element
#'
#' @param dataname (`character(1)`) selector element.
#'
#' @return Raises an error when check fails, otherwise, it returns the `dataname`
#' parameter, invisibly and unchanged.
#'
#' @keywords internal
#'
check_selector_dataname <- function(dataname) {
  checkmate::assert_string(dataname)
}

#' Check selector filters element
#'
#' @param filters (`list`) selector element generated by `data_extract_srv`.
#'
#' @return Raises an error when the check fails, otherwise it returns `NULL`, invisibly.
#'
#' @keywords internal
#'
check_selector_filters <- function(filters) {
  check_selector_filter <- function(x) {
    is.list(x) &&
      all(c("columns", "selected") %in% names(x)) &&
      checkmate::test_character(x$columns, null.ok = TRUE, min.len = 1, any.missing = FALSE) &&
      (
        is.null(x$selected) ||
          all(vapply(x$selected, is.character, logical(1))) ||
          all(vapply(x$selected, is.numeric, logical(1)))
      )
  }
  stopifnot(is.null(filters) || all(vapply(filters, check_selector_filter, logical(1))))
}

#' Check selector select element
#'
#' @param select (`character`) selector element generated by `data_extract_srv`.
#'
#' @return Raises an error when check fails, otherwise, it returns the `select`
#' parameter, invisibly and unchanged.
#'
#' @keywords internal
#'
check_selector_select <- function(select) {
  checkmate::assert_character(select)
}

#' Check selector keys element
#'
#' @param keys (`character`) selector element generated by `data_extract_srv`.
#'
#' @return Raises an error when check fails, otherwise, it returns the `keys`
#' parameter, invisibly and unchanged.
#'
#' @keywords internal
#'
check_selector_keys <- function(keys) {
  checkmate::assert_character(keys, min.len = 0L, any.missing = FALSE)
}

#' Check selector reshape element
#'
#' @param reshape (`logical(1)`) selector element generated by `data_extract_srv`.
#'
#' @return Raises an error when check fails, otherwise, it returns the `reshape`
#' parameter, invisibly and unchanged.
#'
#' @keywords internal
#'
check_selector_reshape <- function(reshape) {
  checkmate::assert_flag(reshape)
}

#' Check selector internal_id element
#'
#' @param internal_id (`character(1)`) selector element generated by `data_extract_srv`.
#'
#' @return Raises an error when check fails, otherwise, it returns the `internal_id`
#' parameter, invisibly and unchanged.
#'
#' @keywords internal
#'
check_selector_internal_id <- function(internal_id) {
  checkmate::assert_string(internal_id)
}

#' Check selector
#'
#' @param selector (`list`) of selector elements generated by `data_extract_srv`.
#'
#' @return Raises an error when check fails, otherwise, it returns the `selector`
#' parameter, invisibly and unchanged.
#'
#' @keywords internal
#'
check_selector <- function(selector) {
  # An error from the checks below is transformed to a shiny::validate error
  # so shiny can display it in grey not in red in an application
  tryCatch(
    expr = {
      checkmate::assert_list(selector)
      checkmate::assert_names(
        names(selector),
        must.include = c("dataname", "filters", "select", "keys", "reshape", "internal_id")
      )
      check_selector_dataname(selector$dataname)
      check_selector_filters(selector$filters)
      check_selector_select(selector$select)
      check_selector_keys(selector$keys)
      check_selector_reshape(selector$reshape)
      check_selector_internal_id(selector$internal_id)
    },
    error = function(e) shiny::validate(e$message)
  )
  invisible(selector)
}
