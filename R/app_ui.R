#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @noRd
#'
singularity <<-
  com.icatalyst.singularity::Singularity$new('client_id', 'client_secret', 'client_key')

app_ui <- function(request) {
  # Define UI for application that draws a histogram
  fluidPage(
    singularity$shiny_tags(shiny::tags),
    shinyStore::initStore(id = "wbtokStore", namespace = "CEWorkbench"),
    tagList(
      # Leave this function for adding external resources
      golem_add_external_resources(),
      # Your application UI logic
      header = dashboardPage(
        title = "Workbench",
        header = dashboardHeader(
          title = "",
          titleWidth = 1,
          leftUi = tagList(
            useShinyjs(),
            uiOutput("filterButton"),
            uiOutput("authorisedFrameworks"),
            uiOutput("linked_frameworks"),
            dropdownBlock(
              id = "ResetOptions",
              title  = "Reset Options",
              badgeStatus = NULL,
              tagList(
                actionButton("resetBrushFilter", label = "Reset brush filters"),
                actionButton("resetFilters", label = "Reset left menu filters"),
                actionButton("deselectAll", label = "Clear fragment list"),
              )

            )
          )
        ),

        sidebar = dashboardSidebar(id = "dashSideBar",
                                   width = 340,
                                   tags$style("#sidebarItemExpanded {overflow: auto; max-height: 300vh;}"),

                                   sidebarMenu(
                                     id = "tabs",
                                     menuItem(text = "Modules", fluidRow(uiOutput("modules")), tabName = "modules"),
                                     menuItem(text = "Filters", fluidRow(
                                       shiny::tagList(
                                         selectInput(inputId = "gender",
                                                     label = "Gender:",
                                                     choices = c("Female" = "92ae82dc-b6dd-4eed-81b7-809194893b90", "Male" = "5dcc8b01-4807-4e4d-870f-5f656c8a761f"),
                                                     selected = "Female",
                                                     multiple = FALSE,
                                         ),
                                         shiny::br(), shiny::br(), shiny::br(), shiny::br()
                                       )
                                     ), tabName = "filter_options")
                                   )
        ),

        body = dashboardBody(
          # tags$head(includeCSS('www/custom.css'))
          uiOutput("body")
        ),

        controlbar =
          dashboardControlbar(id = "controlBarState",
                              skin = "light",
                              overlay = "FALSE",
                              width = 300,
                              controlbarMenu(
                                id = "controlbarMenu",
                                controlbarItem(
                                  "Tab Properties",
                                  uiOutput("rightMenu"),
                                  numericInput("num", "Observations:", 150, min = 1, max = 1000, step = 100)
                                ),
                                controlbarItem(
                                  "Selective Search",
                                  numericInput("num1", "Observations1:", 200, min = 1, max = 1000, step = 100)
                                )
                              )
          )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "sensemakerworkbenchr"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )

}
