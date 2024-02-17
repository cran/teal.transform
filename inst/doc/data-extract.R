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
simple_des <- data_extract_spec(
  dataname = "ADSL",
  filter = filter_spec(vars = "SEX", choices = c("F", "M")),
  select = select_spec(choices = c("BMRKR1", "AGE"))
)

## -----------------------------------------------------------------------------
extract_ui <- function(id, data_extract) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Encoding"),
      data_extract_ui(ns("data_extract"), label = "variable", data_extract)
    ),
    mainPanel(
      h3("Output"),
      verbatimTextOutput(ns("output"))
    )
  )
}

extract_srv <- function(id, datasets, data_extract, join_keys) {
  moduleServer(id, function(input, output, session) {
    reactive_extract_input <- data_extract_srv("data_extract", datasets, data_extract, join_keys)
    s <- reactive({
      format_data_extract(reactive_extract_input())
    })
    output$output <- renderPrint({
      cat(s())
    })
  })
}

## ----eval=FALSE---------------------------------------------------------------
#  shinyApp(
#    ui = fluidPage(extract_ui("data_extract", simple_des)),
#    server = function(input, output, session) {
#      extract_srv("data_extract", datasets, simple_des, join_keys)
#    }
#  )

