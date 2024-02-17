## ----setup, include = FALSE, echo=FALSE---------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(teal.transform)
library(teal.data)
library(shiny)

# Define data.frame objects
ADSL <- teal.transform::rADSL
ADTTE <- teal.transform::rADTTE

# create a list of reactive data.frame objects
datasets <- list(
  ADSL = reactive(ADSL),
  ADTTE = reactive(ADTTE)
)

# create join_keys
join_keys <- join_keys(
  join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
  join_key("ADSL", "ADTTE", c("STUDYID", "USUBJID")),
  join_key("ADTTE", "ADTTE", c("STUDYID", "USUBJID", "PARAMCD"))
)

## -----------------------------------------------------------------------------
adsl_extract <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    label = "Select variable:",
    choices = c("AGE", "SEX", "BMRKR1"),
    selected = "AGE",
    multiple = TRUE,
    fixed = FALSE
  )
)

adtte_extract <- data_extract_spec(
  dataname = "ADTTE",
  select = select_spec(
    choices = c("AVAL", "AVALC", "ASEQ"),
    selected = "AVAL",
    multiple = TRUE,
    fixed = FALSE
  ),
  filter = filter_spec(
    vars = "PARAMCD",
    choices = c("CRSD", "EFS", "OS", "PFS"),
    selected = "OS"
  )
)

data_extracts <- list(adsl_extract = adsl_extract, adtte_extract = adtte_extract)

## -----------------------------------------------------------------------------
merge_ui <- function(id, data_extracts) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Encoding"),
      div(
        data_extract_ui(
          ns("adsl_extract"), # must correspond with data_extracts list names
          label = "ADSL extract",
          data_extracts[[1]]
        ),
        data_extract_ui(
          ns("adtte_extract"), # must correspond with data_extracts list names
          label = "ADTTE extract",
          data_extracts[[2]]
        )
      )
    ),
    mainPanel(
      h3("Output"),
      verbatimTextOutput(ns("expr")),
      dataTableOutput(ns("data"))
    )
  )
}

## -----------------------------------------------------------------------------
merge_srv <- function(id, datasets, data_extracts, join_keys) {
  moduleServer(id, function(input, output, session) {
    selector_list <- data_extract_multiple_srv(data_extracts, datasets, join_keys)
    merged_data <- merge_expression_srv(
      selector_list = selector_list,
      datasets = datasets,
      join_keys = join_keys,
      merge_function = "dplyr::left_join"
    )
    ANL <- reactive({
      data_list <- lapply(datasets, function(ds) ds())
      eval(envir = list2env(data_list), expr = as.expression(merged_data()$expr))
    })

    output$expr <- renderText(paste(merged_data()$expr, collapse = "\n"))
    output$data <- renderDataTable(ANL())
  })
}

## ----eval=FALSE---------------------------------------------------------------
#  shinyApp(
#    ui = fluidPage(merge_ui("data_merge", data_extracts)),
#    server = function(input, output, session) {
#      merge_srv("data_merge", datasets, data_extracts, join_keys)
#    }
#  )

