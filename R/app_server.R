
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import sensemakerdatar
# #' @import com.icatalyst.singularity
#' @import shinyWidgets
#' @import shinyjs
#' @import shinyvalidate
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate as_datetime interval now as_datetime as.difftime
#' @importFrom shinyStore updateStore
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom utils URLencode
#' @importFrom jose base64url_decode
#' @importFrom stringr str_remove
#' @importFrom utils URLencode
#' @importFrom jose base64url_decode
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # Basic helpers file
  source("R/helpers.R")

  security_return <- reactiveValues(sec_values = NULL, return = FALSE, rtoken = NULL)
  control_values <- reactiveValues(workbenchID = NULL, allowed_workbench_ids = NULL, allowed_dashboard_ids = NULL, selected_workbench_id = "nothing selected", selected_dashboard_id = "nothing selected", authorised_click = FALSE)
  update_type <-  reactiveValues(type = NULL)
  build_headers <- reactiveVal(TRUE)




  fw_allowed <- reactive(get_authorised_frameworks(security_return$sec_values[["securitySettingsToken"]]))
  db_allowed <- reactive(get_authorised_dashboards(security_return$sec_values[["securitySettingsToken"]]))
  fwd <- reactive({
    if (is.null(update_type$type)) {
      tmpframework_id <- NULL
      tmpdashboard_id <- NULL
    } else {
      if (update_type$type == "dashboard") {tmpframework_id <- NULL} else {tmpframework_id <- control_values$selected_workbench_id}
      if (update_type$type == "workbench") {tmpdashboard_id <- NULL} else {tmpdashboard_id <- control_values$selected_dashboard_id}
    }
    sensemakerdatar::Data$new(framework_id = tmpframework_id, dashboard_id = tmpdashboard_id, token = security_return$sec_values[["securitySettingsToken"]])
  })


  # start security
  securityVal <- NULL
  securityNR <- FALSE


  # end point for the api calls.
  openAPIEndPoint <- "openapi"

  security_return$return <-FALSE
  observe ({while(!securityNR) {
    securityVal <- handleSecurity(session, input, openAPIEndPoint)
    security_return$sec_values <- securityVal
    security_return$rtoken <- securityVal[["securitySettingsToken"]]
    if (securityVal[["doReturn"]]) {
      security_return$return <-TRUE
      break}
    securityNR <- securityVal$security
    # if ( security_return$return) {return()}
    if (securityNR) {break}
  }


    if (!is.null(isolate(security_return$rtoken))) {

      output$token <- renderText(security_return$rtoken)

      control_values$allowed_workbench_ids <- unlist(unname(fw_allowed()))
      control_values$allowed_dashboard_ids <- unlist(unname(db_allowed()))

      if (control_values$authorised_click == FALSE) {
        if (!is.null((security_return$sec_values[["workbenchID"]])) &&   !((stringr::str_remove(security_return$sec_values[["workbenchID"]], "/") %in% control_values$allowed_workbench_ids) |
                                                                           (stringr::str_remove(security_return$sec_values[["workbenchID"]], "/") %in% control_values$allowed_dashboard_ids) )) {
          output$select_framework <- renderText("workbenchID in URL is invalid or not authorised")
        } else {
          if (!is.null(security_return$sec_values[["workbenchID"]])) {
            output$select_framework_framework <- renderText("")
            wb_id <- stringr::str_remove(security_return$sec_values[["workbenchID"]], "/")
            if (wb_id %in% control_values$allowed_workbench_ids) {
              control_values$selected_workbench_id <- wb_id

            } else {
              if (wb_id %in% control_values$allowed_dashboard_ids) {
                print("we are hear okay")
                control_values$selected_dashboard_id <- wb_id
              }
            }

            updateQueryString("?noParameters", mode = "replace")
            # security_return$sec_values[["workbenchID"]] <- NULL
          }
        }
      }

      # we get the stuff
      if (!fwd()$is_invalid) {
        output$full_count <- renderText(ifelse(fwd()$has_zero_records, 0, nrow(fwd()$df1)))
        output$data_count <- renderText(ifelse(fwd()$has_zero_records, 0, nrow(filter_data(fwd()))))
        output$select_framework <- renderText(fwd()$sm_framework$get_parent_framework_name())
        # Assign the actual R6 class object to a non reactive
        nrfwd <- fwd()
      } else {
        output$full_count <- renderText(0)
        output$data_count <- renderText(0)
      }



      if(build_headers()) {
        output$authorisedFrameworks <- renderUI({

          tagList(
            dropdown(
              tags$h3("Collection options"),
              pickerInput(inputId = "authorisedFramework",
                          label = "Your Authorised Frameworks",
                          choices = fw_allowed(),
                          selected =  ifelse(!control_values$authorised_click, control_values$selected_workbench_id, "nothing selected"),
                          multiple = FALSE,
                          options = list(
                            `actions-box` = TRUE)
              ),
              pickerInput(inputId = "authorisedDashboard",
                          label = "Your Authorised Dashboards",
                          choices = db_allowed(),
                          selected = ifelse(!control_values$authorised_click, control_values$selected_dashboard_id, "nothing selected"),
                          multiple = FALSE,
                          options = list(
                            `actions-box` = TRUE)
              ),
              # style = "unite", icon = icon("gear"),
              tooltip = tooltipOptions(title = "Click to select workbench and language options"),
              status = "danger", width = "300px",
              label = "Select Workbench"
            )
          )
        })
        build_headers(FALSE)
      }


      observeEvent(input$authorisedFramework, ignoreInit = TRUE, handlerExpr = {
        if (input$authorisedFramework != "nothing selected") {
          if (input$authorisedDashboard != "nothing selected") {
            updatePickerInput(session = session, "authorisedDashboard", selected = "nothing selected")
          }
          if (input$authorisedFramework != "nothing selected") {
            #  output$select_framework <- renderText("")
          }
          update_type$type <- "workbench"
          control_values$authorised_click <- TRUE
          control_values$selected_workbench_id <- input$authorisedFramework
        }

      })
      observeEvent(input$authorisedDashboard, ignoreInit = TRUE, handlerExpr = {
        if (input$authorisedDashboard != "nothing selected") {
          if (input$authorisedFramework != "nothing selected") {
            updatePickerInput(session = session, "authorisedFramework", selected = "nothing selected")
          }
          if (input$authorisedDashboard != "nothing selected") {
            #   output$select_framework <- renderText("")
          }
          update_type$type <- "dashboard"
          control_values$authorised_click <- TRUE
          control_values$selected_dashboard_id <- input$authorisedDashboard

        }
      })



    }
  })
}
