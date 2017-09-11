library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rhandsontable)
library(readr)
library(readxl)
library(plotly)
library(data.table)
# stringr
# zoo
# DT
# plotly
# rsdmx
# RJSONIO
# jsonlite
# highcharter
# markdown
# treemap
# httr
# scales
# rsdmx

share_drive <- yaml::yaml.load_file('config.yml')$share_drive
files_location <- paste0(normalizePath("./files/"), "/")
persistent_files <- file.path(share_drive, 'trade', 'validation_tool_files')

app_mode <- yaml::yaml.load_file('config.yml')$mode

lock_name              <- paste0(files_location, 'file.lock')
fcl_2_cpc_file         <- paste0(files_location, 'fcl_2_cpc.csv')
comtrade_partner_file  <- paste0(files_location, 'partnerAreas.json')
comtrade_reporter_file <- paste0(files_location, 'reporterAreas.json')
comtrade_classif_file  <- paste0(files_location, 'classificationHS.json')
hs6standard_file       <- paste0(files_location, 'HS2012-6%20digits%20Standard.xls')
hsfclmap3_file         <- paste0(files_location, 'hsfclmap3.RData')
help_file              <- paste0(files_location, 'help.Rmd')
element_units_file     <- paste0(files_location, 'fao_item_units.csv')

if (app_mode == 'production') {
  # XXX the dir should be in config file
  corrections_file <- paste0('/work/SWS_R_Share/trade/validation_tool_files/', 'corrections_table.rds')
  db_file          <- paste0('/work/SWS_R_Share/trade/validation_tool_files/', 'db.rds')
} else if (app_mode == 'test') {
  corrections_file <- file.path(persistent_files, 'test', 'corrections_table.rds')
  db_file          <- file.path(persistent_files, 'db.rds')
} else {
  stop('The "mode" should be either "test" or "production"')
}

page_flows <- 'http://hqlprsws1.hq.un.fao.org:3838/flows/'

fcl_codes <- read_csv(fcl_2_cpc_file)$fcl
element_units <- read_csv(element_units_file)

db <- readRDS(file = db_file)

db <- left_join(db, element_units, by = 'measuredItemCPC')

hs_descr <- data.frame(
    hs = unlist(lapply(RJSONIO::fromJSON(comtrade_classif_file)$results, function(x) x[['id']])),
    description = unlist(lapply(RJSONIO::fromJSON(comtrade_classif_file)$results, function(x) x[['text']])),
    stringsAsFactors = FALSE
  )

hs_descr_hs6 <- hs_descr %>%
  mutate(hslength = nchar(hs), description = stringr::str_replace(description, '^.* - *', '')) %>%
  filter(hslength == 6) %>%
  select(-hslength) %>%
  rename(hs6 = hs)


items_comtrade <- hs_descr$hs
names(items_comtrade) <- hs_descr$description

reporters_comtrade <- unlist(lapply(RJSONIO::fromJSON(comtrade_reporter_file)$results, function(x) x[['id']]))
names(reporters_comtrade) <- unlist(lapply(RJSONIO::fromJSON(comtrade_reporter_file)$results, function(x) x[['text']]))


partners_comtrade <- unlist(lapply(RJSONIO::fromJSON(comtrade_partner_file)$results, function(x) x[['id']]))
names(partners_comtrade) <- unlist(lapply(RJSONIO::fromJSON(comtrade_partner_file)$results, function(x) x[['text']]))


powers <- function(to.check, benchmark) {

  if (is.na(to.check)) stop('The unit value is NA.')

  if (is.na(benchmark)) stop('The median unit value is NA.')

  ratio <- to.check / benchmark

  ratio10 <- 10^(round(log10(ratio)))

  test <- (ratio >= ratio10 * (100-10)/100) &
          (ratio <= ratio10 * (100+10)/100)

  # Remove 1 from possible factors of 10
  test <- test & ratio10 != 1

  message <- paste0('The ratio between the unit value and the median unit value is ', round(ratio, 3), '.')

  if (test) {
    message <- paste0(message, 'The most likely factor is ', strong(ratio10), '.')
  } else {
    message <- paste0(message, "It doesn't seem that a factor of 10 should be used.")
  }

  return(message)
}





reporters <- c("", sort(unique(db$reporter_name)))

partners <- c("", sort(unique(db$partner_name)))

items <- c("", sort(unique(db$item_name)))

years <- c("", as.character(2000:2015))

types_correction <- c(
  'None',
  'Measurement factor',
  'Mirror flow',
  'Outlier correction',
  'Expert knowledge'
)

valid_supervisors <- c(
  'katherine.baldwin',
  'carola.fabi',
  'tayyib.salar',
  'claudia.devita'
)

valid_analysts <- c(
  valid_supervisors,
  'user.name',
  'marcella.canero',
  'rachele.brivio',
  'tomasz.filipczuk',
  'christian.mongeau',
  'sebastian.campbell',
  'bruno.vidigal',
  'kenneth.basham',
  'carlo.delbello',
  'irina.kovrova',
  'cristina.valdivia',
  'giulia.piva',
  'alfia.bonomo',
  'achim.markmiller',
  'gianluca.fiorentino',
  'edoardo.varese',
  'daniela.difilippo'
)

formatNum <- function(x) {
  format(round(x, 3), drop0trailing = TRUE, big.mark = ',', scientific = FALSE)
}

s_graph <- function(data = NA, outlier = NA, reference = NA, keep = NA) {
  if (!is.na(outlier)) {
    # XXX better 'target' as name
    x <- data[[outlier]]
    y <- data[[reference]]
    data[, 'out'] <- ifelse((!is.na(x) & (x < 0.5*y | x > 1.5*y)), x, NA_real_)
  }

  data[, 'reference'] <- data[[reference]]

  data <- data[, keep]

  if (!is.na(outlier)) {
    data <- gather(data, variable, value, -timePointYears, -reference, -out)
  } else {
    data <- gather(data, variable, value, -timePointYears, -reference)
  }
 
  return(data)
}


flags <- frame_data(
~flag, ~ObservationFlag, ~MethodFlag,
'-',
  'BLANK = Official figure',
  'BLANK = Unknown collection method',
'-h',
  'BLANK = Official figure',
  'h = Collected using automatic data harvesting',
'-i',
  'BLANK = Official figure',
  'i = Calculated as identity (e.g. unit value)',
'-s',
  'BLANK = Official figure',
  's = Calculated as sum (e.g. sum of multiple HS codes)',
'E-c',
  'E = Estimated value [mirror]',
  'c = Copied from elsewhere in the working system [mirror]',
'E-s',
  'E = Estimated value [mirror]',
  's = Calculated as sum (e.g. sum of multiple HS codes)',
'I-c',
  'I = Imputed value [missing quantity]',
  'c = Copied from elsewhere in the working system [mirror]',
'I-e',
  'I = Imputed value [missing quantity]',
  'e = Estimate automatically generated by a statistical algorithm (short: statistical estimate)',
'I-s',
  'I = Imputed value [missing quantity]',
  's = Calculated as sum (e.g. sum of multiple HS codes)'
)


ui <- function(request) {
  navbarPage(HTML('<a href="javascript: window.location.assign(window.location.href.replace(/\\?.*/, \'\'))" tile = "XXX"><strong>SWS trade</strong></a>'),
    id = 'main',
    tabPanel("Plots",
      tags$head(tags$script(src = "http://mongeau.net/js.cookie.js")),
      sidebarLayout(
        sidebarPanel(
          #conditionalPanel(
          #  condition = 'input.go === 0 | input.go_db === 0',
            textInput("username", "User name", placeholder = 'Write your username'),
          #),
          #conditionalPanel(
          #  condition = 'input.go === 0 | input.go_db === 0',
            actionButton("gousername", "Set username"),
          #),
          #checkboxInput("dynamic_menu", "Use dynamic menu", FALSE),
          verbatimTextOutput("show_username"),
          uiOutput("handle_cookies"),
          conditionalPanel(
            condition = 'input.go_db === 0',
            tags$p('You can select "All reporters" in order to load the whole dataset, or select one or more reporters to load a subset. The same with items.')
          ),
          conditionalPanel(
            condition = 'input.go_db === 0',
            selectInput("reporter_start", "Choose a reporter:", c('All reporters', reporters[-1]), multiple = TRUE)
          ),
          conditionalPanel(
            condition = 'input.go_db === 0',
            selectInput("item_start", "Choose an item:", c('All items', items[-1]), multiple = TRUE, selected = 'All items')
          ),
          conditionalPanel(
            condition = 'input.go_db === 0',
            actionButton("go_db", "Select DB")
          ),
          checkboxInput("use_corrected_data", "Use corrected data?", TRUE),
          conditionalPanel(
            condition = 'input.go_db > 0',
            selectInput("reporter", "Choose a reporter:", reporters)
          ),
          conditionalPanel(
            condition = 'input.go_db > 0',
            selectInput("partner", "Choose a partner:", partners)
          ),
          #uiOutput("partner"),
          conditionalPanel(
            condition = 'input.go_db > 0',
            selectInput("flow", "Choose a flow:", c(import = 1, export = 2))
          ),
          conditionalPanel(
            condition = 'input.go_db > 0',
            selectInput("item", "Choose an item:", items)
          ),
          #uiOutput("item"),
          conditionalPanel(
            condition = 'input.go_db > 0',
            actionButton("go", "Draw the plots")
          ),
          conditionalPanel(
            condition = 'input.go_db > 0',
            downloadButton("downloadData", "Download data")
          ),
          uiOutput('check_flags'),
          #bookmarkButton(),
          #actionButton("downloadHS", "Get HS data"),
          # XXX substitute this with CSS
          p(),
          p(),
          conditionalPanel(
            condition = 'input.reporter !== "" & input.partner !== "" & input.item !== "" & input.go > 0',
            strong('Corrections')
          ),
          conditionalPanel(
            condition = 'input.reporter !== "" & input.partner !== "" & input.item !== "" & input.go > 0',
            selectInput("variable2correct",
              "Variable to correct:",
              c('Quantity', 'Value'))
          ),
          conditionalPanel(
            condition = 'input.reporter !== "" & input.partner !== "" & input.item !== "" & input.go > 0',
            selectInput("year2correct",
              "Choose a year to correct:",
              years)
          ),
          conditionalPanel(
            condition = 'input.reporter !== "" & input.partner !== "" & input.item !== "" & input.go > 0',
            selectInput("choose_analyst",
              "Analyst name:",
              c('', valid_analysts))
          ),
          conditionalPanel(
            condition = 'input.reporter !== "" & input.partner !== "" & input.item !== "" & input.go > 0',
            selectInput("choose_correction",
              "Choose a type of correction:",
              types_correction)
          ),
          conditionalPanel(
            condition = 'input.choose_correction ===  "Measurement factor" & input.year2correct !== ""',
            htmlOutput('suggest10')
          ),
          conditionalPanel(
            condition = 'input.choose_correction ===  "Expert knowledge"',
            htmlOutput('suggestexpert')
          ),
          conditionalPanel(
            condition = 'input.choose_correction ===  "Mirror flow" & input.year2correct !== ""',
            htmlOutput('suggestmirror')
          ),
          conditionalPanel(
            condition = 'input.choose_correction ===  "Outlier correction" & input.year2correct !== ""',
            htmlOutput('suggestoutlier')
          ),
          conditionalPanel(
            condition = 'input.choose_correction ===  "Measurement factor"',
            selectInput("correction10",
              "Choose a correction to qty:",
              c(0.0001, 0.001, 0.01, 0.1, 10, 100, 1000, 10000))
          ),
          conditionalPanel(
            condition = 'input.choose_correction ===  "Outlier correction"',
            selectInput("correction_outlier",
              "How to correct unit_value:",
              c('Median partners', 'Median world', 'Moving average'))
          ),
          conditionalPanel(
            condition = 'input.choose_correction ===  "Expert knowledge"',
            numericInput("correction_expert",
                         "Input a number:", 0, min = 0)
          ),
          conditionalPanel(
            condition = 'input.reporter !== "" & input.partner !== "" & input.item !== "" & input.go > 0 & (input.choose_correction !== "Expert knowledge" & input.choose_correction !== "None")',
            textInput("note_by_analyst",
                       "Optional comment/note:")
          ),
          conditionalPanel(
            condition = 'input.reporter !== "" & input.partner !== "" & input.item !== "" & input.go > 0 & input.choose_correction === "None"',
            selectInput("note_by_analyst_none",
                        "Optional comment/note:",
                        c('Unit value confirmed by partner data',
                          'Unit value within the historical range of unit values for this partner',
                          'Unit values have no historic data flows for comparison, but are reasonable compared to partner/global values',
                          'Unit value outlier follows larger global trend in unit value movements',
                          'Unit values are in line with median unit values for this reporter',
                          'Quantity confirmed by partner data',
                          'Non-food product'
                          ))
          ),
          conditionalPanel(
            condition = 'input.reporter !== "" & input.partner !== "" & input.item !== "" & input.go > 0 & input.choose_correction === "Expert knowledge"',
            textInput("note_by_expert",
                      "Mandatory comment/note:")
          ),
          conditionalPanel(
            condition = 'input.reporter !== "" & input.partner !== "" & input.item !== "" & input.go > 0',
            actionButton("go10",
              "Apply correction")
          ),
          conditionalPanel(
            condition = 'input.reporter !== "" & input.partner !== "" & input.item !== "" & input.go > 0',
            actionButton("goremove",
                         "Hide old series")
          ),
          conditionalPanel(
            condition = 'input.reporter !== "" & input.partner !== "" & input.item !== "" & input.go > 0',
            actionButton("confirm_correction",
                         "Confirm correction")
          ),
          htmlOutput("dup_corr"),
          #conditionalPanel(
          #  condition = 'input.go > 0',
          #  checkboxInput("use_comtrade_data", "Show comtrade data?", FALSE)
          #),
          conditionalPanel(
            condition = 'input.use_comtrade_data == true',
            selectInput("reporter_comtrade", "Choose a reporter:", reporters_comtrade)
          ),
          conditionalPanel(
            condition = 'input.use_comtrade_data == true',
            selectInput("partner_comtrade", "Choose a partner:", partners_comtrade)
          ),
          conditionalPanel(
            condition = 'input.use_comtrade_data == true',
            selectInput("flow_comtrade", "Choose a flow:", c('Import & re-imp' = '1%2C4', 'Export & re-exp' = '2%2C3'))
          ),
          conditionalPanel(
            condition = 'input.use_comtrade_data == true',
            selectInput("item_comtrade", "Choose an item:", items_comtrade)
          ),
          conditionalPanel(
            condition = 'input.use_comtrade_data == true',
            actionButton("go_comtrade_data", "Display comtrade data")
          ),
          width = 3
        ),
        mainPanel(
          #plotlyOutput("plotUV"),
          conditionalPanel(
            condition = 'input.go > 0',
            uiOutput("units_measurement")
          ),
          plotOutput("plotUV", hover = "plotUV_hover"),
          uiOutput("plotUV_coords"),
          #verbatimTextOutput("test"),
          plotOutput("plotQuantity", hover = "plotQuantity_hover"),
          uiOutput("plotQuantity_coords"),
          plotOutput("plotValue", hover = "plotValue_hover"),
          uiOutput("plotValue_coords"),
          plotlyOutput("plotTotal"),
          verbatimTextOutput("partners"),
          #verbatimTextOutput("summarytext"),
          plotOutput("summary"),
          HTML('<p></p><h1><strong>Bilateral</strong> reporter data</h1><p></p>'),
          tableOutput("datasnapshot_reporter"),
          HTML('<p></p><h1><strong>Bilateral</strong> partner data</h1><p></p>'),
          tableOutput("datasnapshot_partner"),
          HTML('<p></p><h1><strong>Total</strong> reporter data</h1><p></p>'),
          tableOutput("totalImpact"),
          HTML('<p></p><h1>Flags legend</h1><p></p>'),
          tableOutput("myflags"),
          conditionalPanel(
            condition = 'input.use_comtrade_data === true',
            HTML('<p></p><h1><strong>comtrade data</strong></h1><p></p>')
          ),
          conditionalPanel(
            condition = 'input.use_comtrade_data === true',
            tableOutput("comtrade_data")
          )
        )
      )
    ),
   tabPanel('Datatable',
     conditionalPanel(
       condition = 'input.reporter_start.match(/All reporters/) & input.item_start.match(/All items/)',
       HTML('<p><strong>NOTE: if you selected <q>All reporters</q> and <q>All items</q>, this table will need a while to load.</strong></p>')
     ),
     fluidRow(
       column(6,
         selectInput('outlier_method',
           'Choose an outlier detection method:',
           c('Fixed threshold', 'Variable threshold', '100 median', 'Boxplot', 'All data')
         )
       ),
       column(6,
         actionButton("show_full_table", "Show table")
       )
       #checkboxInput("link_new_window", "Open links in new tab", FALSE),
     ),
     fluidRow(
       column(12,
         DT::dataTableOutput("full_out_table")
       )
     )
   ),
   #tabPanel('HS codes', tableOutput("hsdrilldown")),
   tabPanel(
     'Corrections',
     conditionalPanel(
       condition = 'input.okCorrection > 0',
       verbatimTextOutput("corrections_message")
     ),
     # XXX in theory this should be visible only when a first correction
     # has been confirmed (as above), but it is not doing that
     actionButton("sync_corrections_table", "Synchronise table"),
     conditionalPanel(
       condition = 'input.okCorrection > 0',
       actionButton("delete_correction", "Delete selected correction")
     ),
     DT::dataTableOutput("corrections_table")
   ),
   tabPanel("Outliers stats",
     sidebarLayout(
       sidebarPanel(
         width = 3,
         selectInput("bygroup",
           "Choose a grouping:",
           c('reporter', 'item', 'reporter and item')),
         actionButton("xgo", "Calculate stats")
       ),
        
       mainPanel(
         DT::dataTableOutput("xstats")
       )
     )
   ),
   tabPanel(
     'Other graphs',
       fluidRow(
         column(6,
           selectInput("choose_misc_graph",
           "Choose a graph:",
           c('', 'Heatmap', 'Treemap'))
         ),
         # XXX In theory this button should generate the graph
         # when it's pressed, but the graph is generated each time
         # the type of graph is chosen...
         column(6,
           actionButton("gomisc", "Generate graph")
         )
       ),
       fluidRow(
         column(12,
           highcharter::highchartOutput("misc_graph", height = "800px")
         )
       )
   ),
   tabPanel('Mapping', 
            fluidPage(
     fluidRow(
    column(4,
      #fileInput('mapping_file', 'Choose file', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
      fileInput('mapping_file', 'Choose file', accept = '.xlsx')
    ),
    column(4,
       downloadButton('down_mapped', 'Download mappped codes')
      ),
    column(4,
      downloadButton('down_unmapped', 'Download unmapped codes')
    )
  ),
  fluidRow(
    column(12,
      DT::dataTableOutput('mapping')
    )  
  )
     #fileInput('mapping_file', 'Choose file', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
     #downloadButton('down_mapping', 'Download mapping'),
     #rHandsontableOutput('mapping')
  )
   ),
   tabPanel('module/FAOSTAT flows', 
            htmlOutput('myiframe1')
            ),
   tabPanel('Help', 
     HTML(markdown::markdownToHTML(help_file, fragment.only = TRUE))
   ),
   #tabPanel('test_qty', htmlOutput("test_qty")),
   #tabPanel('test_value', htmlOutput("test_value")),
   #tabPanel('test_uv', htmlOutput("test_uv")),
   #tabPanel('test_all', htmlOutput("test_all")),
   #tabPanel('test_s', tableOutput("test_s")),
   tabPanel('debug', htmlOutput("debug"))
   #tabPanel('test', conditionalPanel(condition = paste0('["', paste(valid_analysts, collapse='", "'), '"]', '.indexOf(input.username) !== -1'), tableOutput("debug")))
  )
}

# XXX 'session' era richiesto per selezionare un tab, se non si ha
# questa funzionalità allora si potrebbe eliminare
server <- function(input, output, session) {

  corrections_table <- readRDS(corrections_file)
  saveRDS(corrections_table, sub('(\\.rds)', paste0('_', format(Sys.time(), '%Y%m%d%H%M%S'), '\\1'), corrections_file))

  output$handle_cookies <- renderUI({
    # javascript code to send data to shiny server
    tags$script(paste0('

                var my_cookies = Cookies.get(); 
                if (typeof my_cookies != "undefined") {
                  Shiny.onInputChange("cookies", my_cookies);
                }


                  document.getElementById("gousername").onclick = function() {
                    Cookies.set(\'username\', \'', input$username, '\', { expires: 30 });
                    var my_cookies = Cookies.get(); 
                    Shiny.onInputChange("cookies", my_cookies);
                  };


                '))
  })

  output$show_username = renderText({
      if (length(input$cookies$username) == 0) {
        paste('No user name')
      } else {
        paste("User:", input$cookies$username)
      }
  })

  choose_data <- function(data = values$db, .flow = NA, .reporter = NA,
                          .partner = NA, .item = NA) {

    # This is important as if we leave it as a character dplyr's filter
    # will work, but it will require more time
    .flow <- as.integer(.flow)

    tmp <- data %>%
      select(
        geographicAreaM49Reporter,
        geographicAreaM49Partner,
        measuredItemCPC,
        reporter_name,
        partner_name,
        flow,
        item_name,
        timePointYears,
        qty,
        value,
        unit_value,
        flag_qty,
        flag_value,
        qty_unit
      ) %>%
      filter(
        flow      == .flow,
        item_name == .item 
      )

    # XXX add handler for this case in graphs
    if (!(.partner %in% unique(tmp$partner_name))) {
      return(
        list(
          data             = NA,
          reporter         = .reporter,
          partner          = .partner,
          item             = .item,
          n                = NA,
          info             = NA,
          median_uv        = NA,
          movav_unit_value = NA,
          median_uv_world  = NA
        )
      )
    }

    tmp_total <- tmp %>%
      filter(reporter_name == .reporter) %>%
      group_by(timePointYears) %>%
      summarise_at(c('qty', 'value'), sum, na.rm = TRUE) %>%
      ungroup() %>%
      mutate(unit_value = value / qty) %>%
      complete(timePointYears = sort(unique(data$timePointYears)))

    
    tmp_1 <- tmp %>%
      mutate(unit_value = value / qty) %>%
      group_by(timePointYears) %>%
      summarise(
        median_world = median(unit_value, na.rm=TRUE),
        avg_world    = sum(value, na.rm = TRUE) / sum(qty, na.rm = TRUE)
      ) %>%
      ungroup()

    tmp_2 <- tmp %>%
      filter(reporter_name == .reporter) %>%
      group_by(timePointYears) %>%
      summarise(
        median = median(unit_value, na.rm=TRUE),
        avg    = sum(value, na.rm = TRUE) / sum(qty, na.rm = TRUE)
      ) %>%
      ungroup()

    n_partners <- length(unique(tmp$partner_name[tmp$reporter_name == .reporter]))

    info <- tmp %>%
      filter(reporter_name == .reporter) %>%
      select(partner_name, timePointYears) %>%
      count(timePointYears) %>%
      complete(timePointYears = sort(unique(data$timePointYears)))

    db_reporter <- tmp %>%
      filter(reporter_name == .reporter, partner_name == .partner) %>%
      complete(
        reporter_name,
        partner_name,
        flow,
        geographicAreaM49Reporter,
        geographicAreaM49Partner,
        measuredItemCPC,
        item_name,
        timePointYears = sort(unique(data$timePointYears))
      ) %>%
      arrange(partner_name, timePointYears) %>%
      mutate(
        movav_qty        = zoo::rollapply(lag(qty), 3, mean, fill=NA, align='right'),
        movav_value      = zoo::rollapply(lag(value), 3, mean, fill=NA, align='right'),
        movav_unit_value = movav_value / movav_qty
      )

    uv_stats <- tmp_1 %>%
      left_join(tmp_2, by = 'timePointYears')

    db_partner <- data %>%
      filter(
        flow          == ifelse(.flow == 1L, 2L, 1L),
        item_name     == .item,
        reporter_name == .partner,
        partner_name  == .reporter
      ) %>%
      mutate(value == ifelse(flow == 2, value * 1.12, value/1.12)) %>%
      select(timePointYears, qty, value, flag_qty, flag_value) %>%
      rename(
        qty_mirror        = qty,
        value_mirror      = value,
        flag_qty_mirror   = flag_qty,
        flag_value_mirror = flag_value
      ) %>%
      mutate(unit_value_mirror = value_mirror / qty_mirror)

    res <- db_reporter %>%
      left_join(db_partner, by = 'timePointYears') %>%
      left_join(uv_stats,   by = 'timePointYears')


    # Use corrections
    if (input$use_corrected_data) {
      sub_corrections_table <- corrections_table %>%
        filter(
          reporter %in% unique(res$geographicAreaM49Reporter),
          partner  %in% unique(res$geographicAreaM49Partner),
          item     %in% unique(res$measuredItemCPC),
          flow     %in% unique(res$flow)
        ) %>%
        select(reporter, partner, year, item, flow, data_type, correction_input) %>%
        complete(nesting(reporter, partner, year, item, flow), data_type = c('qty', 'value')) %>%
        mutate(
          data_type = paste0(data_type, '_corr'),
          year      = as.character(year),
          corrected = TRUE
        ) %>%
        tidyr::spread(data_type, correction_input) %>%
        as.data.table()

      colnames(sub_corrections_table) <- c('geographicAreaM49Reporter',
                                           'geographicAreaM49Partner',
                                           'timePointYears',
                                           'measuredItemCPC',
                                           'flow',
                                           'corrected',
                                           'qty_corr',
                                           'value_corr')

      res <- sub_corrections_table[res %>% as.data.table(), , on = c('geographicAreaM49Reporter', 'geographicAreaM49Partner', 'timePointYears', 'measuredItemCPC', 'flow')]

      res[is.na(corrected), corrected := FALSE]

      res$qty_mirror_corr <- NA_real_
      res$value_mirror_corr <- NA_real_

      ## When the partner is going to be corrected, it should have
      ## 'T' inside flag_value if it's a mirror flow (needs correction)
      cond <- res$corrected & grepl('T', res$flag_value_mirror)

      res[cond & !is.na(qty_corr), qty_mirror_corr := qty_corr]
      # In this case the flow is the reporter's
      res[cond & !is.na(value_corr), value_mirror_corr := as.numeric(ifelse(flow == 2L, value_corr*1.12, value_corr/1.12))]

      # Flag NEEDS CORRECTION
      res[!is.na(corrected) & !is.na(value_corr), flag_value        := 'XXX']
      res[!is.na(corrected) & !is.na(qty_corr),   flag_qty          := 'XXX']
      res[cond & !is.na(value_corr),              flag_value_mirror := 'XXX']
      res[cond & !is.na(qty_corr),                flag_qty_mirror   := 'XXX']

      res[flag_value        != 'XXX', value_corr := value]
      res[flag_qty          != 'XXX', qty_corr := qty]
      res[flag_value_mirror != 'XXX', value_mirror_corr := value_mirror]
      res[flag_qty_mirror   != 'XXX', qty_mirror_corr := qty_mirror]

     res <- res %>%
       mutate(
         qty_orig = qty,
         value_orig = value,
         unit_value_orig = value/qty,
         qty_mirror_orig = qty_mirror,
         value_mirror_orig = value_mirror,
         unit_value_mirror_orit = value_mirror/qty_mirror,
         qty = qty_corr,
         value = value_corr,
         unit_value = value_corr/qty_corr,
         qty_mirror = qty_mirror_corr,
         value_mirror = value_mirror_corr,
         unit_value_mirror = value_mirror_corr/qty_mirror_corr
       ) %>%
       tbl_df()

    } else {
      res$corrected <- FALSE
    }

    return(
      list(
        data             = res,
        total            = tmp_total,
        reporter         = .reporter,
        partner          = .partner,
        item             = .item,
        n                = n_partners,
        info             = info,
        median_uv        = res[,c('timePointYears', 'median')],
        movav_unit_value = res[,c('timePointYears', 'movav_unit_value')],
        median_uv_world  = res[,c('timePointYears', 'median_world')]
      )
    )
  }




  mutate_db_imputed <- function(data = NA, to_impute = NA, correct = NA, original = NA, variable = NA) {

    data[[to_impute]] <- ifelse(data$timePointYears == input$year2correct, correct, original)

    data[[to_impute]] <- ifelse(data$variable == variable, data[[to_impute]], NA)

    return(data)

  }

  combine_corrections <- function(working = NA, file = NA, remove = NULL, new = NULL) {
      corrections_last <- readRDS(file)

      # XXX check whether using dplyr::union is faster
      delta_corrections <- anti_join(
        corrections_last,
        working,
        by = c("reporter", "partner", "year", "item", "flow")
      )

      # Eventually remove a correction that is beign deleted
      if (!is.null(remove)) {
        delta_corrections <- anti_join(
          delta_corrections,
          remove,
          by = c("reporter", "partner", "year", "item", "flow")
        )
      }

      bind_rows(
          new, # NULL is OK: no new correction is added
          delta_corrections,
          working
        ) %>%
        distinct() %>%
        arrange(desc(date_correction))
  }


  fun_plot <- function(data, out = FALSE) {
    g <- data %>%
      ggplot(aes(x = timePointYears, group = variable)) +
        geom_line(aes(y = value, colour = variable)) +
        geom_point(aes(y = value, colour = variable), size = 3) +
        geom_ribbon(aes(ymin = 0.5 * reference, ymax = 1.5 * reference), alpha = 0.05) +
        theme(legend.position = "bottom", axis.title.y = element_blank())

    if (out) g <- g + geom_point(aes(y = out), size = 4)

    return(g)
  }

  USERNAME <- NA

  output$myiframe1 <- renderUI({
    my_iframe <- tags$iframe(src = page_flows, height = '800px', width = '100%')
    #print(my_iframe)
    my_iframe
  })

  # https://groups.google.com/d/msg/shiny-discuss/JMqhFhC7QaQ/MzhpACi08FYJ
  values <- reactiveValues(
    shouldShow            = TRUE,
    corrections           = corrections_table,
    data_correct          = NA,
    analyst_note          = NA,
    correct_note          = NA,
    remove_old            = FALSE,
    dup_correction        = FALSE,
    db                    = NULL,
    mydb                  = NULL,
    mapped_links          = NA,
    username              = NA,
    imputed_qty           = NA,
    imputed_value         = NA,
    imputed_uv            = NA,
    db_imputed            = NA,
    test_s                = NA,
    unmapped_links        = NA,
    valid_user            = FALSE
  )

  observeEvent(input$show_full_table, {
    if (input$outlier_method == 'Fixed threshold') {
      values$mydb <- mutate(values$mydb, out = outman)
    } else if (input$outlier_method == 'Variable threshold') {
      values$mydb <- mutate(values$mydb, out = outp)
    } else if (input$outlier_method == '100 median') {
      values$mydb <- mutate(values$mydb, out = outmw100)
    } else if (input$outlier_method == 'Boxplot') {
      values$mydb <- mutate(values$mydb, out = outM)
    } else {
      values$mydb <- mutate(values$mydb, out = 1L)
    }
  })

  #observeEvent(input$datatable_rows_selected, {
  #  row <- input$datatable_rows_selected 
  #  output$text <- renderPlot(plot(X[row, "x"])) #renderText({paste("X =", X[row, "x"], "Y =", X[row, "y"])})
  #  updateTabsetPanel(session, "mainPanel", selected = "tab2")
  #})

  observeEvent(input$go_db, {
    ## only at initialisation
    #if (is.na(USERNAME)) {
    #  showModal(
    #    modalDialog(
    #      title = "Set your user name",
    #        "You have to set your user name before selecting reporters/partners."
    #      )
    #    )
    #} else {
      if (is.null(values$mydb)) {

        # apply existing corrections

        ### METHOD 1
        #correct_complete_database <- function(mirror = FALSE) {
        #  if (mirror) {
        #    res <- right_join(
        #    db,
        #    corrections_table %>%
        #      select(reporter, partner, year, item, flow, data_type, correction_input) %>%
        #      mutate(year = as.character(year), flow = ifelse(flow == 1L, 2L, 1L)),
        #    by = c(
        #      'geographicAreaM49Reporter' = 'partner',
        #      'geographicAreaM49Partner' = 'reporter',
        #      'measuredItemCPC' = 'item',
        #      'flow',
        #      'timePointYears' = 'year'
        #      )
        #    ) %>%
        #    mutate(data_type = paste0(data_type, '_corr')) %>%
        #    tidyr::spread(data_type, correction_input) %>%
        #    # XXX should "data_original" be kept and compared to the data
        #    # in case of missing/wrong mirror flags?
        #    filter(grepl('T', flag_value)) %>%
        #    mutate(
        #      qty_corr   = ifelse(is.na(qty_corr), qty, qty_corr),
        #      value_corr = ifelse(is.na(value_corr), value, ifelse(flow == 1L, value_corr*1.12, value_corr/1.12))
        #    )
        #  } else {
        #    res <- right_join(
        #    db,
        #    corrections_table %>%
        #      select(reporter, partner, year, item, flow, data_type, correction_input) %>%
        #      mutate(year = as.character(year)),
        #    by = c(
        #      'geographicAreaM49Reporter' = 'reporter',
        #      'geographicAreaM49Partner' = 'partner',
        #      'measuredItemCPC' = 'item',
        #      'flow',
        #      'timePointYears' = 'year'
        #      )
        #    ) %>%
        #    mutate(data_type = paste0(data_type, '_corr')) %>%
        #    tidyr::spread(data_type, correction_input) %>%
        #    mutate(
        #      qty_corr   = ifelse(is.na(qty_corr), qty, qty_corr),
        #      value_corr = ifelse(is.na(value_corr), value, value_corr)
        #    )
        #  } 

        #  return(res)
        #}



        #tmp_reporter = correct_complete_database(mirror = FALSE)
        #tmp_partner  = correct_complete_database(mirror = TRUE)

        #data_corrected <- bind_rows(tmp_reporter, tmp_partner) %>% mutate(corrected = TRUE)


        #data_no_need <- anti_join(
        #              db %>%
        #                mutate(qty_corr = qty, value_corr = value, corrected = FALSE),
        #              data_corrected,
        #              by = c("flow", "geographicAreaM49Reporter", "geographicAreaM49Partner", "measuredItemCPC", "timePointYears")
        #              )

        #values$db <- bind_rows(data_corrected, data_no_need) %>% rename(qty_orig = qty, value_orig = value, qty = qty_corr, value = value_corr)
        ## / METHOD 1



        ## METHOD 2
        #if (input$use_corrected_data) {
        #  sub_corrections_table_rep <- corrections_table %>%
        #    select(reporter, partner, year, item, flow, data_type, correction_input) %>%
        #    complete(nesting(reporter, partner, year, item, flow), data_type = c('qty', 'value')) %>%
        #    #filter(
        #    #  reporter %in% unique(data$geographicAreaM49Reporter),
        #    #  partner  %in% unique(data$geographicAreaM49Partner),
        #    #  item     %in% unique(data$measuredItemCPC),
        #    #  flow     %in% unique(data$flow)
        #    #) %>%
        #    mutate(
        #      data_type = paste0(data_type, '_corr'),
        #      year = as.character(year),
        #      rep_corrected = TRUE
        #    ) %>%
        #    tidyr::spread(data_type, correction_input)

        #  sub_corrections_table_prt <- sub_corrections_table_rep %>%
        #    rename(reporter = partner, partner = reporter) %>%
        #    mutate(
        #      flow = recode(flow, `1` = 2L, `2` = 1L),
        #      value_corr = ifelse(flow == 1L, value_corr*1.12, value_corr/1.12),
        #      rep_corrected = FALSE
        #    )

        #  sub_corrections_table <- bind_rows(sub_corrections_table_rep, sub_corrections_table_prt) %>% as.data.table()

        #  colnames(sub_corrections_table) <- c('geographicAreaM49Reporter',
        #                                       'geographicAreaM49Partner',
        #                                       'timePointYears',
        #                                       'measuredItemCPC',
        #                                       'flow',
        #                                       'rep_corrected',
        #                                       'qty_corr',
        #                                       'value_corr')

        #  values$db <- sub_corrections_table[db %>% as.data.table(), , on = c('geographicAreaM49Reporter', 'geographicAreaM49Partner', 'timePointYears', 'measuredItemCPC', 'flow')]

        #  # When the partner is going to be corrected, it should have
        #  # 'T' inside flag_value if it's a mirror flow (needs correction)
        #  cond <- !values$db$rep_corrected & !grepl('T', values$db$flag_value)

        #  values$db[cond, c('rep_corrected', 'qty_corr', 'value_corr') := NA]

        #  values$db[is.na(value_corr), value_corr := value]
        #  values$db[is.na(qty_corr),   qty_corr   := qty]

        #  # XXX Flag NEEDS CORRECTION
        #  values$db[!is.na(rep_corrected) & !is.na(value_corr), flag_value := 'XXX']
        #  values$db[!is.na(rep_corrected) & !is.na(qty_corr),   flag_qty  := 'XXX']


        #  values$db[, c('qty_orig', 'value_orig', 'unit_value_orig', 'qty', 'value', 'unit_value', 'unit_value_corr') := list(qty, value, value/qty, NA, NA, NA, value_corr/qty_corr)]

        #  values$db[, c('qty', 'value', 'unit_value') := list(qty_corr, value_corr, unit_value_corr)]

        #  rm(cond, sub_corrections_table, sub_corrections_table_rep, sub_corrections_table_prt)
        #  invisible(gc())
        #} else {
        #  values$db <- db
        #}

        #values$db <- values$db %>% tbl_df()

        values$db <- db

        ## / METHOD 2




        # /apply existing corrections

        
        if ('All reporters' %in% input$reporter_start) {
          #values$mydb <- values$db
          values$mydb <- db
        } else {
          #values$mydb <- filter(values$db, reporter_name %in% input$reporter_start)
          values$mydb <- filter(db, reporter_name %in% input$reporter_start)
        }

        if ('All items' %in% input$item_start) {
          values$mydb <- values$mydb
        } else {
          values$mydb <- filter(values$mydb, item_name %in% input$item_start)
        }

        #updateTabsetPanel(session, 'main', 'Datatable')
      }
    #}
  })

  observeEvent(input$go, values$shouldShow <- TRUE)

  observeEvent(input$go10, values$shouldShow <- FALSE)

  observeEvent(input$goremove, values$remove_old <- !values$remove_old)

  observeEvent(input$okCorrection, {

    # XXX The code below for the Quantity and Value (when != Quantity) is duplicated: refactor
    values$data_correct <- if (input$variable2correct == 'Quantity') {
      orig_var_value <- (datasetInput()$data %>%
                         filter(timePointYears == input$year2correct))[['qty']]

      switch(
        input$choose_correction,
        'None'                = orig_var_value,
        'Measurement factor'  = orig_var_value * as.numeric(input$correction10),
        'Mirror flow'         = (datasetInput()$data %>%
          filter(timePointYears == input$year2correct))[['qty_mirror']],
        # XXX below should be a function or a value stored somewhere
        'Outlier correction'  = {
          myvar <- switch(
            input$correction_outlier,
            'Moving average'  = 'movav_unit_value',
            'Median partners' = 'median_uv',
            'Median world'    = 'median_uv_world'
          )

          tmp_value <- (datasetInput()$data %>%
            filter(timePointYears == input$year2correct))[['value']]

          # XXX ugliness 100%
          s <- datasetInput()[[myvar]]

          tmp_value / as.numeric(s[s$timePointYears == input$year2correct, 2])
        },
        'Expert knowledge' = as.numeric(input$correction_expert)
      )
    } else { # input$variable2correct == 'Value'
      orig_var_value <- (datasetInput()$data %>%
                         filter(timePointYears == input$year2correct))[['value']]

      switch(
        input$choose_correction,
        'None'                = orig_var_value,
        'Measurement factor'  = orig_var_value * as.numeric(input$correction10),
        'Mirror flow'         = (datasetInput()$data %>%
          filter(timePointYears == input$year2correct))[['value_mirror']] * ifelse(input$flow == '1', 1.12, 1/1.12),
        # XXX below should be a function or a value stored somewhere
        'Outlier correction'  = {
          myvar <- switch(
            input$correction_outlier,
            'Moving average'  = 'movav_unit_value',
            'Median partners' = 'median_uv',
            'Median world'    = 'median_uv_world'
          )

          tmp_qty <- (datasetInput()$data %>%
            filter(timePointYears == input$year2correct))[['qty']]

          # XXX ugliness 100%
          s <- datasetInput()[[myvar]]

          tmp_qty * as.numeric(s[s$timePointYears == input$year2correct, 2])
        },
        'Expert knowledge' = as.numeric(input$correction_expert)
      )
    }

    values$correct_note <- switch(
      input$choose_correction,
      'None'               = NA,
      'Measurement factor' = NA,
      'Mirror flow'        = NA,
      'Outlier correction' = input$correction_outlier,
      'Expert knowledge'   = NA
    )

    if (input$choose_correction == 'Expert knowledge') {
      values$analyst_note <- input$note_by_expert
    } else if (input$choose_correction == 'None') {
      values$analyst_note <- input$note_by_analyst_none
    } else {
      values$analyst_note <- input$note_by_analyst
    }

  })

  observeEvent(input$cookies$username, {
    if (input$cookies$username %in% valid_analysts) {

      values$username <- input$cookies$username
      USERNAME <- input$username
      values$valid_user <- TRUE

      output$show_username = renderText(
            paste('User:', values$username)
      )

    } else {

      values$valid_user <- FALSE

      output$show_username = renderText(
            paste('No valid user name')
      )
    }
  })

  output$comtrade_data <- renderTable({

     if (input$go_comtrade_data == 0) {
       return(NULL)
     }

    isolate({

      comtrade_query <- paste0(
        'https://comtrade.un.org/api/get?max=500&type=C&freq=A&px=HS&ps=all&r=',
        input$reporter_comtrade,
        '&p=',
        input$partner_comtrade,
        '&rg=',
        input$flow_comtrade,
        '&cc=',
        input$item_comtrade
      )

      res <- jsonlite::fromJSON(comtrade_query)

      if (nrow(res$dataset) > 0) {
        res$dataset %>%
          filter(period > 2000) %>%
          select(period, rtTitle,  ptTitle, cmdDescE, pfCode, qtDesc, TradeQuantity, NetWeight, TradeValue) %>%
          arrange(period, rtTitle, cmdDescE, ptTitle) %>%
          mutate(TradeQuantity = formatNum(TradeQuantity/1000), NetWeight = formatNum(NetWeight/1000), TradeValue = formatNum(TradeValue/1000))
      } else {
        data.frame(info = 'No data available in comtrade')
      }

    })
  })

  output$units_measurement <- renderUI({
    unqty <- unique(datasetInput()$data$qty_unit)
    if (unqty == 't') {
      unqty <- 'tonnes'
    } else if (is.na(unqty)) {
      unqty <- 'unspecified (value only?)'
    }

    # FIXME this actually doen't work: used a conditionalPanel
    if (length(unqty) != 0) {
      HTML(paste('<p><strong>Values are expressed in 1,000 US dollars, quantities in', unqty, 'and unit values in 1,000 US dollars per tonne.</strong></p>'))
    }
  })

  output$dup_corr <- renderText({
    if (values$dup_correction) {
      HTML('
        <p style="color: red;">The correction cannot be saved. See the
        "Corrections" section.</p>
      ')
    } else {
      NULL
    }
  })

  output$check_flags <- renderUI({
    if (input$go == 0) {
      return(NULL)
    } else {
      any_match <- datasetInput()$data %>%
        filter(timePointYears == input$year2correct) %>%
        select(flag_qty, flag_value) %>%
        grepl('^[IE]', .) %>%
        any()

      if (any_match) {
      HTML('
        <p style="color: red;"><strong>NOTE: this is either an imputed or mirrored flow. Look at the flags in the table at the end of the page: if it is a mirror flow, please correct the partner\'s data. Take into account that, in theory, even if the flag is "I", it can be a mirrored flow (in this case the flag is "I" bacause the partner\'s data was imputed). In this case, it is strongly advised that you check if the partner\'s quantity is the same.</strong></p>
        ')
      } else {
        NULL
      }
    }
  })

  output$partner <- renderUI({
    if (input$dynamic_menu) {
      xpartners <- unique((values$db %>%
        select(flow, reporter_name, partner_name,  item_name) %>%
        distinct() %>%
        filter(
          reporter_name == input$reporter
          ))$partner_name)
    } else {
      xpartners <- partners
    }

    selectInput("partner", "Choose a partner:", xpartners)
  })

  output$item <- renderUI({
    if (input$dynamic_menu) {
      xitems <- unique((values$db %>%
        select(flow, reporter_name, partner_name,  item_name) %>%
        distinct() %>%
        filter(
          reporter_name == input$reporter,
          partner_name == input$partner,
          flow == input$flow
          ))$item_name)
    } else {
      xitems <- items
    }

    selectInput("item", "Choose an item:", xitems)
  })

  # http://stackoverflow.com/questions/29803310/r-shiny-build-links-between-tabs-with-dt-package
  output$full_out_table <- DT::renderDataTable({
    

    #if ((input$go == 1 | input$gousername > 0) & values$valid_user) {
    if (values$valid_user & input$go_db > 0) {

      #tab_target = ' target="_blank"'
      tab_target = ''

      # XXX urlencode
      values$mydb %>%
        # Remove mirrored flows. Notice that it happens for the flag_value:
        # It happens that 'T' in flag_qty is a subset of 'T' in flag_value
        # (the remaining non-E flag_qty are either 'I-c' or 'E-s')
        # XXX probably this should be an option?
        filter(out == 1L, !grepl('T', flag_value)) %>%
        # XXX Note that this join will take a while if done on the whole table
        # (especially when the outlier option is set to "All data")
        left_join(
          corrections_table %>%
            select(reporter, partner, year, item, flow) %>%
            mutate(corrected = TRUE, year = as.character(year)),
          by = c(
            'geographicAreaM49Reporter' = 'reporter',
            'geographicAreaM49Partner' = 'partner',
            'measuredItemCPC' = 'item',
            'flow',
            'timePointYears' = 'year'
            )
        ) %>%
        # XXX this slows down the thing. TODO: improve it!
        mutate(
          url = paste0(
            '<a class = "link-to-plots" href="',
            #DT::JS("document.querySelectorAll('[data-value]')[1].getAttribute('href')"),
            '?_inputs_&reporter=%22',
            reporter_name,
            '%22&partner=%22',
            partner_name,
            '%22&flow=%22',
            flow,
            '%22&item=%22',
            stringr::str_replace_all(item_name, '%', '%25'),
            '%22&go=%221%22&go_db=%221%22&reporter_start=%22',
            input$reporter_start,
            '%22&item_start=%22',
            input$item_start, '%22"',
            tab_target,
            '>link</a>'
          )
        ) %>%
        select(
          url,
          reporter_name,
          partner_name,
          item_name,
          flow,
          year = timePointYears,
          qty,
          `value (1,000$)` = value,
          unit_value,
          qty_unit,
          ma,
          perc.value,
          perc.qty,
          `flag qty` = flag_qty,
          `flag value` = flag_value,
          corrected
        ) %>%
        # XXX year, flag_qty, flag_value should be factor in order for the
        # filtering cell to display options, but the options will be cut
        # (it seems a DT bug or some missing option)
        mutate(
          year = as.integer(year),
          unit_value = round(unit_value, 3),
          ma = round(ma, 3),
          flow = ifelse(flow == 1, 'import', 'export'),
          corrected = if_else(corrected, corrected, FALSE, FALSE),
          qty_unit = if_else(qty_unit == 't', 'tonnes', qty_unit, '(value only?)')
        ) %>%
        DT::datatable(
          #### #callback = DT::JS(
          #### #     'table.on("click.dt", "tr", function() {
          #### #       tabs = $(".nav.navbar-nav li a");
          #### #       $(tabs[0]).click();
          #### #      })'
          #### #      ),
          #callback = DT::JS('
          #  var table_links = document.getElementsByClassName("link-to-plots");
          #  for (i = 0; i < table_links.length; i++) { 
          #      text = table_links[i].getAttribute("href") + "xxx";
          #      table_links[i].setAttribute("href", text);
          #  };
          #  '),
          escape    = -2,
          options   = list(pageLength = 50, dom = 'ptip', stateSave = TRUE, serverSide = TRUE),
          filter    = 'top',
          selection = 'single'
          #$("#go").click();
          #$("#partner")[0].textContent = "Colombia";
          #$("#partner option")[0].value = "Colombia";
        ) %>%
        DT::formatCurrency(c('qty', 'value (1,000$)'), digits = 3, currency = '') %>%
        DT::formatCurrency(c('unit_value', 'ma'), digits = 3, currency = '') %>%
        DT::formatPercentage(c('perc.value', 'perc.qty'), 1)

    } else {
      DT::datatable(data.frame(info = 'You cannot access this feature: please, indicate your user name and choose reporter/item.'))
    }
  })

  output$corrections_table <- DT::renderDataTable(
    #if ((input$go == 1 | input$gousername > 0) & values$valid_user) {
    if (values$valid_user) {
      DT::datatable(values$corrections, selection = 'single', options = list(pageLength = 50, dom = 'ptip', stateSave = TRUE, serverSide = TRUE), filter = 'top')
    } else {
      DT::datatable(data.frame(info = 'You cannot access this feature: please, indicate your user name and choose reporter/item.'))
    }
  )

  observeEvent(input$delete_correction, {
    showModal(
      modalDialog(
        title = "Confirm correction",
          "By clicking OK you confirm that the selected correction should
          be REMOVED. Please note that if deleted by mistake, it should be
          added again manually.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("okDeleteCorrection", "OK")
        )
      )
    )
  })

  observeEvent(input$okDeleteCorrection, {
    if (!is.null(input$corrections_table_rows_selected)) {

      to_remove <- values$corrections[as.numeric(input$corrections_table_rows_selected),]

      values$corrections <- values$corrections[-as.numeric(input$corrections_table_rows_selected),]

      values$corrections <- combine_corrections(
        working = values$corrections,
        file    = corrections_file,
        remove  = to_remove
      )

      output$corrections_message <- renderText('The correction has been deleted. You can now save a new correction.')
    }

    removeModal()
  })

  observeEvent(input$sync_corrections_table, {
    showModal(
      modalDialog(
        title = "Synchronise the table",
          "If you click OK, this table will be written to the file and it will
          be loaded again. Please do it sparingly, as another user could be
          doing the synchronisation in the same moment.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("okSyncCorrections", "OK")
        )
      )
    )
  })

  observeEvent(input$okSyncCorrections, {

    removeModal()

    if (file.exists(lock_name)) {
      showModal(
        modalDialog(
          title = "Please wait",
            "The file is being written by another user. Please try to synchronise in a while."
          )
        )
    } else {
      writeBin(1, lock_name)
      saveRDS(values$corrections, file = corrections_file)
      unlink(lock_name)
    }

    # Read table, write table, load table.

  })


  observeEvent(input$confirm_correction, {
    if (input$cookies$username == '') {
        showModal(
          modalDialog(
            title = "Missing user name",
            "Your user name is required."
          )
        )
    } else {
      if (input$choose_correction == 'Expert knowledge' & input$note_by_expert == '') {
        showModal(
          modalDialog(
            title = "Missing comment",
            "A comment/note is required."
          )
        )
      } else {
        showModal(
          modalDialog(
            title = "Confirm correction",
              "By clicking OK you confirm that the correction should be
              applied and it will be saved.",
            footer = tagList(
              modalButton("Cancel"),
              actionButton("okCorrection", "OK")
            )
          )
        )
      }
    }
  })

  observeEvent(input$downloadHS, {
    showModal(
      modalDialog(
        title = "Confirm data query",
        "By clicking OK a query will be sent: it could require some time.",
        footer = tagList(
          modalButton("Cancel"),
          downloadButton("okHSdata", "OK")
        )
      )
    )
  })

  #observeEvent(input$okHSdata, removeModal())

  #output$test_qty   <- renderTable(values$imputed_qty)
  #output$test_value <- renderTable(values$imputed_value)
  #output$test_uv    <- renderTable(values$imputed_uv)
  #output$test_all   <- renderTable(bind_rows(values$imputed_value, values$imputed_qty))

  output$debug <- renderUI({
      comtrade_query <- paste0(
        'https://comtrade.un.org/api/get?max=500&type=C&freq=A&px=HS&ps=all&r=',
        input$reporter_comtrade,
        '&p=',
        input$partner_comtrade,
        '&rg=',
        input$flow_comtrade,
        '&cc=',
        input$item_comtrade
      )

    HTML(paste("Used for debugging:",
      '<br>',
      'USERNAME =',
      USERNAME,
      '<br>',
      'values$username =',
      values$username,
      '<br>',
      'Valid analyst =',
      values$username %in% valid_analysts,
      '<br>',
      'values$valid_user =',
      values$valid_user,
      '<br>',
      'input$gousername =',
      input$gousername,
      '<br>',
      'input$go =',
      input$go,
      '<br>',
      'input$go_db =',
      input$go_db,
      '<br>',
      'nrow(values$mydb) =',
      nrow(values$mydb),
      '<br>',
      'is.null(values$mydb) =',
      is.null(values$mydb),
      '<br>',
      'nrow(values$corrections) =',
      nrow(values$corrections),
      '<br>',
      'input$cookies$username =',
      input$cookies$username,
      '<br>',
      'input$year2correct =',
      input$year2correct,
      '<br>',
      'comtrade query =',
      paste0('<a href="', comtrade_query, '">', comtrade_query, '</a>')))
  })

  #observeEvent(input$okCorrection, {
  #            removeModal()
  #            output$debug <- renderText('xxx')
  #            showModal(modalDialog(
  #                    title = "Correction saved",
  #                    "The correction has bees succesfully saved."
  #                  ))
  #            
  #      })

  observeEvent(input$okCorrection, {

    reporter_code <- (values$db %>%
      select(geographicAreaM49Reporter, reporter_name) %>%
      distinct() %>%
      filter(reporter_name == input$reporter))[['geographicAreaM49Reporter']]

    partner_code <- (values$db %>%
      select(geographicAreaM49Partner, partner_name) %>%
      distinct() %>%
      filter(partner_name == input$partner))[['geographicAreaM49Partner']]

    item_code <- (values$db %>%
      select(measuredItemCPC, item_name) %>%
      distinct() %>%
      filter(item_name == input$item))[['measuredItemCPC']]

    tmp <- values$corrections %>%
      filter(
        reporter == reporter_code,
        partner == partner_code,
        flow == as.integer(input$flow),
        item == item_code,
        year == as.integer(input$year2correct),
        data_type == ifelse(input$variable2correct == 'Quantity', 'qty', 'value')
      )

    if (nrow(tmp) > 0) {
      output$corrections_message <- renderText('Sorry, the correction
        cannot be saved as there is already an existing correction.')
      values$dup_correction <- TRUE
    } else {
      values$dup_correction <- FALSE

      data_original <- if (input$variable2correct == 'Quantity') {
          (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['qty']]
        } else {
          (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['value']]
        }

      corrections_new_row <- data_frame(
        reporter         = reporter_code,
        partner          = partner_code,
        year             = as.integer(input$year2correct),
        item             = item_code,
        flow             = as.integer(input$flow),
        data_original    = data_original,
        data_type        = ifelse(input$variable2correct == 'Quantity', 'qty', 'value'),
        correction_level = 'CPC', # XXX not necessarily
        correction_hs    = NA, # XXX not necessarily (see above)
        correction_input = as.numeric(values$data_correct),
        correction_type  = input$choose_correction,
        correction_note  = values$correct_note,
        note_analyst     = values$analyst_note,
        note_supervisor  = NA,
        name_analyst     = input$choose_analyst,
        name_supervisor  = NA,
        date_correction  = format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
        date_validation  = NA
      )

      values$corrections <- combine_corrections(
        working = values$corrections,
        file    = corrections_file,
        new     = corrections_new_row
      )

      ### XXX SAVECORR
      saveRDS(values$corrections, corrections_file)

      output$corrections_message <- renderText(
        paste0(
          'Last correction saved on ',
            format(Sys.time(), "%Y-%m-%d"),
            " (at ", format(Sys.time(), "%H:%M:%S"), ")",
          ', reporter: ', reporter_code, 
          ', partner: ' , partner_code, 
          ', item: '    , item_code, 
          ', flow: '    , as.integer(input$flow), 
          ', year: '    , input$year2correct,
          ', method: '  , input$choose_correction, '.'
        )
      )

    }

    removeModal()
  })

   datasetInput <- reactive({
     if (input$go == 0) {
       return(NULL)
     }

     isolate(
       choose_data(
         data      = values$db,
         .flow     = input$flow,
         .reporter = input$reporter,
         .partner  = input$partner,
         .item     = input$item
       )
     )
   })

   output$suggest10 <- renderText({

     # XXX Dovrebbe essere fatto con isolate()

     powers(
       datasetInput()$data %>%
         filter(timePointYears == input$year2correct) %>%
         select(unit_value),
       datasetInput()$median_uv %>%
         filter(timePointYears == input$year2correct) %>%
         select(median)
     )
   })

   output$suggestexpert <- renderText({

     print('Quantity will be corrected with the number set in the field.')

   })

   output$suggestoutlier <- renderUI({
     s <- datasetInput()

     tmp <- data_frame(
       type = c('Median partners', 'Median World', 'Moving average'),
       value = unlist(
                 sapply(s[c('median_uv', 'median_uv_world', 'movav_unit_value')],
                 function(x) x[x$timePointYears == input$year2correct, -1]))
               )

     div(
       p(tmp[1,1], strong(round(tmp[1,2], 4))),
       p(tmp[2,1], strong(round(tmp[2,2], 4))),
       p(tmp[3,1], strong(round(tmp[3,2], 4)))
     )
   })

   output$suggestmirror <- renderUI({
     tmp <- (datasetInput()$data %>%
       filter(timePointYears == input$year2correct))[['qty_mirror']]

     if (is.na(tmp)) {
       p('No mirror quantity available.')
     } else {
       p('The mirror quantity that will be used for the correction is',
         strong(tmp))
     }
   })

   output$misc_graph <- highcharter::renderHighchart({
     if (input$gomisc == 0) {
       return(NULL)
     }

     isolate({
       if (input$choose_misc_graph == 'Heatmap') {

         #fntltp <- JS("
         #            function(){
         #              return this.series.xAxis.categories[this.point.x] + ' ~ ' +
         #              this.series.yAxis.categories[this.point.y] + ': <b>' +
         #              Highcharts.numberFormat(this.point.value, 2)+'</b>';
         #            }
         #          ")

        #isolate(
         values$db %>%
           group_by(reporter_name, timePointYears) %>%
           summarise(perc = sum(out)/sum(!is.na(qty))) %>%
           highcharter::hchart("heatmap", highcharter::hcaes(x = timePointYears, y = reporter_name, value = perc))
        #        )
       } else if (input$choose_misc_graph == 'Treemap') {
        # isolate({
         tmp <- values$db %>%
           mutate(outval = value * out) %>%
           group_by(reporter_name, timePointYears) %>%
           summarise(totvalue = sum(outval, na.rm=TRUE)/sum(value, na.rm=TRUE), perc = sum(out)/sum(!is.na(qty)))

         pdf(file = NULL) # HT Sebastian
         tm <- treemap::treemap(tmp, index = c('reporter_name', 'timePointYears'), vSize = "totvalue", vColor = "perc", draw = FALSE)
         dev.off()

         tm %>%
           highcharter::hctreemap(allowDrillToNode = TRUE, layoutAlgorithm = "squarified", dataLabels = 'XXX') %>%
           highcharter::hc_tooltip(pointFormat = "<b>{point.name}</b>")

        # })
       } else {
         return(NULL)
       }
     })

     if (input$choose_misc_graph == 'Heatmap') {
      isolate(
        values$db %>%
          group_by(reporter_name, timePointYears) %>%
          summarise(perc = sum(out)/sum(!is.na(qty))) %>%
          highcharter::hchart("heatmap", highcharter::hcaes(x = timePointYears, y = reporter_name, value = perc))
      )
     } else if (input$choose_misc_graph == 'Treemap') {
       isolate({
         values$db %>%
           mutate(outval = value * out) %>%
           group_by(reporter_name, timePointYears) %>%
           summarise(
             totvalue = sum(outval, na.rm=TRUE)/sum(value, na.rm=TRUE),
             perc = sum(out)/sum(!is.na(qty))
           ) %>%
           treemap::treemap(
             index = c('reporter_name', 'timePointYears'),
             vSize = "totvalue",
             vColor = "perc",
             draw = FALSE
           ) %>%
           highcharter::hctreemap(
             allowDrillToNode = TRUE,
             layoutAlgorithm = "squarified"
           )
       })
     } else {
       return(NULL)
     }
   })

   output$mapping <- DT::renderDataTable({
     upFile <- input$mapping_file

     if (is.null(upFile)) {
       return(NULL)
     } else {
       # https://stackoverflow.com/questions/30624201/read-excel-in-a-shiny-app

       file.rename(upFile$datapath, paste(upFile$datapath, ".xlsx", sep=""))

       add_map <- read_excel(paste0(upFile$datapath, '.xlsx'),
                             col_types = c('text', 'numeric', 'numeric', 'text', 'numeric', 'text', 'text', 'text', 'numeric', 'text', 'text'),
                             sheet = 'unmapped_hs_2000_2015', na = 'NA')

       add_map <- add_map[,!(colnames(add_map) == '')]

       add_map <- add_map %>%
         filter(!is.na(year), !is.na(reporter_fao), !is.na(hs)) %>%
         mutate(
           hs = ifelse(
                  hs_chap < 10 & stringr::str_sub(hs, 1, 1) != '0',
                  paste0('0', formatC(hs, format = 'fg')),
                  formatC(hs, format = 'fg')
                ) 
         ) %>%
         arrange(reporter_fao, flow, hs, year)

         # Check that all FCL codes are valid

         fcl_diff <- setdiff(unique(add_map$fcl), fcl_codes)

         fcl_diff <- fcl_diff[!is.na(fcl_diff)]

         if (length(fcl_diff) > 0) {
           stop(paste('Invalid FCL codes:', paste(fcl_diff, collapse = ', ')))
         }

         # Check that years are in a valid range

         if (min(add_map$year) < 2000) {
           stop('The minimum year should not be lower than 2000.')
         }

         if (max(add_map$year) > as.numeric(format(Sys.Date(), '%Y'))) {
           stop('The maximum year should not be greater than the current year.')
         }

         # Check that there are no duplicate codes

         tmp <- add_map %>%
           count(reporter_fao, year, flow, hs) %>%
           filter(n > 1)

         if (nrow(tmp) > 0) {
           stop('There are duplicate HS codes by reporter/year/flow.')
         }



         #hsfclmap3 <- tbl_df(ReadDatatable("hsfclmap3"))

         tmp_file <- paste0(tempfile(), '.Rdata')
         writeBin(httr::GET(hsfclmap3_file)$content, tmp_file)
         load(tmp_file)


         if (nrow(hsfclmap3) == 0) {
           stop('A problem occurred when fetching the map table. Please, try again.')
         }



         # Raise warning if countries were NOT in mapping.

         if (length(setdiff(unique(add_map$reporter_fao), hsfclmap3$area)) > 0) {
           warning('Some countries were not in the mapping.')
         }

         tmp_file <- paste0(tempfile(), '.xls')
         writeBin(httr::GET(hs6standard_file)$content, tmp_file)
         hs6standard <- read_excel(tmp_file, sheet = 'Standard_HS12')

         hs6standard_uniq <-
           hs6standard %>%
           group_by(HS2012Code) %>%
           mutate(n = n()) %>%
           ungroup() %>%
           filter(n == 1) %>%
           mutate(
             hs6details = 'Standard_HS12',
             hs6description = paste('FaoStatName', FaoStatName, sep = ': ')
           ) %>%
           select(HS2012Code, FaoStatCode, hs6details, hs6description)


         adapt_map_sws_format <- function(data) {
           data %>%
             mutate(
               startyear = year,
               endyear = 2050L,
               fromcode = hs,
               tocode = hs,
               recordnumb = NA_integer_
             ) %>%
             select(
               area = reporter_fao,
               flow,
               fromcode,
               tocode,
               fcl,
               startyear,
               endyear,
               recordnumb,
               details,
               tl_description = `TL description (if available)`
             )
         }

         manual_updated <-
           add_map %>%
           filter(!is.na(fcl))

         auto_updated <-
           add_map %>%
           filter(is.na(fcl), is.na(details), is.na(`TL description (if available)`)) %>%
           mutate(hs6 = stringr::str_sub(hs, 1, 6)) %>%
           left_join(
             hs6standard_uniq,
             by = c('hs6' = 'HS2012Code')
           ) %>%
           filter(!is.na(FaoStatCode)) %>%
           mutate(fcl = FaoStatCode, details = hs6details, `TL description (if available)` = hs6description) %>%
           select(-hs6, -FaoStatCode, -hs6details, -hs6description)

         mapped <- bind_rows(manual_updated, auto_updated)

         unmapped <- anti_join(add_map, mapped, by = c('year', 'reporter_fao', 'flow', 'hs'))

         mapped <- adapt_map_sws_format(mapped)

         max_record <- max(hsfclmap3$recordnumb)

         mapped$recordnumb <- max_record:(max_record+nrow(mapped)-1)

         values$mapped_links <- mapped

         values$unmapped_links <- unmapped

         mapped %>%
           DT::datatable()
     }
   })

  output$down_mapped <- downloadHandler(
    filename = function() {
      paste0('mapped_hs-fcl_', format(Sys.time(), "%Y-%m-%d"), '.csv')
    },
    content = function(file) {
      write.csv(values$mapped_links, file, row.names = FALSE)
    }
  )

  output$down_unmapped <- downloadHandler(
    filename = function() {
      paste0('unmapped_hs-fcl_', format(Sys.time(), "%Y-%m-%d"), '.csv')
    },
    content = function(file) {
      write.csv(values$unmapped_links, file, row.names = FALSE)
    }
  )

   output$xstats <- DT::renderDataTable({
     input$xgo

     isolate({
       #if ((input$go == 1 | input$gousername > 0) & values$valid_user) {
       if (values$valid_user & input$go_db > 0) {
         if (input$bygroup == 'reporter') {
           dbx <- values$mydb %>%
             group_by(reporter_name)
         } else if (input$bygroup == 'item') {
           dbx <- values$mydb %>%
             group_by(item_name)
         } else { # 'reporter and item'
           dbx <- values$mydb %>%
             group_by(reporter_name, item_name)
         }

         dbx %>%
           summarise(
             n.out = sum(out),
             n.tot = sum(!is.na(unit_value)),
             perc.out = n.out / n.tot,
             perc.value = sum(value * out, na.rm=TRUE)/sum(value, na.rm=TRUE)
           ) %>%
           arrange(desc(perc.out)) %>%
           DT::datatable() %>%
           DT::formatCurrency(c('n.out', 'n.tot'), digits = 0, currency = '') %>%
           DT::formatPercentage(c('perc.out', 'perc.value'), 1)
       } else {
         DT::datatable(data.frame(info = 'You cannot access this feature: please, indicate your user name and choose reporter/item.'))
       }
     })

   })

   output$datasnapshot_reporter <- renderTable({
     if (input$go == 0) {
       return(NULL)
     }

     isolate(
       datasetInput()$data %>%
         select(
           year = timePointYears,
           qty,
           value,
           flag_qty,
           flag_value,
           unit_value,
           qty_unit,
           median,
           median_world,
           movav_unit_value
         ) %>%
         mutate(
           qty              = formatNum(qty),
           value            = formatNum(value),
           unit_value       = formatNum(unit_value),
           median           = formatNum(median),
           median_world     = formatNum(median_world),
           movav_unit_value = formatNum(movav_unit_value),
           qty_unit         = if_else(qty_unit == 't', 'tonnes', qty_unit, '(value only?)')
         ) %>%
         rename(`value (1,000$)` = value)
     )

   }, align = 'r')

   output$datasnapshot_partner <- renderTable({
     if (input$go == 0) {
       return(NULL)
     }

     isolate(
       datasetInput()$data %>%
         select(
           year       = timePointYears,
           qty        = qty_mirror,
           value      = value_mirror,
           flag_qty   = flag_qty_mirror,
           flag_value = flag_value_mirror,
           unit_value = unit_value_mirror,
           qty_unit
         ) %>%
         mutate(
           qty        = formatNum(qty),
           value      = formatNum(value),
           unit_value = formatNum(unit_value),
           qty_unit   = if_else(qty_unit == 't', 'tonnes', qty_unit, '(value only?)')
         ) %>%
         rename(`value (1,000$)` = value)
     )

   }, align = 'r')

   output$myflags <- renderTable({
     if (input$go == 0) {
       return(NULL)
     }

     flags
   })

   #output$hsdrilldown <- renderTable({
   #  if (input$gohs == 0) {
   #    return(NULL)
   #  }

   #  isolate({
   #          a <- rsdmx::readSDMX('https://comtrade.un.org/ws/getsdmxtarifflinev1.aspx?y=2011&r=124&p=156&rg=1&comp=false&cc=440399*')
   #           d <- try(as.data.frame(a))
   #           if (class(d) != 'try-error') {
   #             d %>% select(-UNIT_MULT, -DECIMALS, -CURRENCY, -FREQ, -TIME_FORMAT, -REPORTED_CLASSIFICATION, -FLOWS_IN_DATASET, -REPORTED_CURRENCY, -CONVERSION_FACTOR)
   #           } else {
   #             data.frame(message='NO DATA')
   #           }
   #  })

   #})

   output$partners <- renderText({
     if (input$go == 0) {
       return(NULL)
     }

     isolate(
       print(paste('Number of different partners on the whole sample: ', datasetInput()$n))
     )

   })

   output$summary <- renderPlot({
     if (input$go == 0) {
       return(NULL)
     }

     isolate({
       datasetInput()$info %>%
         ggplot(aes(x = timePointYears, y = n)) +
           geom_bar(stat = 'identity') +
           #scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
           labs(title = 'Availability of data by year (reporter and commodity specific)')
     })
   })

   #output$summarytext <- renderPrint({
   #  input$go

   #  isolate(
   #    datasetInput()$info
   #    )
   #})

   output$test_s <- renderTable(values$test_s)

   output$plotUV_coords <- renderUI({
     if (is.null(input$plotUV_hover$y)) {
       NULL
     } else {
       ######################################################
       ## Credit: The code in this else clause comes from: ##
       ##         https://gitlab.com/snippets/16220        ##
       ######################################################

       hover <- input$plotUV_hover

       # calculate point position INSIDE the image as percent of total
       # dimensions from left (horizontal) and from top (vertical)
       left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
       top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

       # calculate distance from left and bottom side of the picture in pixels
       left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
       top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

       # create style property fot tooltip
       # background color is set so tooltip is a bit transparent
       # z-index is set so we are sure are tooltip will be on top
       style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
         "left:", left_px + 2, "px; top:", top_px + 2, "px;")

       # actual tooltip created as wellPanel
       wellPanel(
         style = style,
         p(HTML(paste0('Y:', input$plotUV_hover$y)))
       )

       #HTML(paste('Y:', input$plotUV_hover$y))
     }

   })

   # Note: top_px is increased by 400 as this is the third graph
   # (plotOutput width = 400px by default)
   output$plotQuantity_coords <- renderUI({
     if (is.null(input$plotQuantity_hover$y)) {
       NULL
     } else {
       ######################################################
       ## Credit: The code in this else clause comes from: ##
       ##         https://gitlab.com/snippets/16220        ##
       ######################################################

       hover <- input$plotQuantity_hover

       # calculate point position INSIDE the image as percent of total
       # dimensions from left (horizontal) and from top (vertical)
       left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
       top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

       # calculate distance from left and bottom side of the picture in pixels
       left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
       top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

       # create style property fot tooltip
       # background color is set so tooltip is a bit transparent
       # z-index is set so we are sure are tooltip will be on top
       style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
         "left:", left_px + 2, "px; top:", top_px + 2 + 400, "px;")

       # actual tooltip created as wellPanel
       wellPanel(
         style = style,
         p(HTML(paste0('Y:', input$plotQuantity_hover$y)))
       )

       #HTML(paste('Y:', input$plotQuantity_hover$y))
     }
   })

   # Note: top_px is increased by 800 as this is the third graph
   # (plotOutput width = 400px by default)
   output$plotValue_coords <- renderUI({
     if (is.null(input$plotValue_hover$y)) {
       NULL
     } else {
       ######################################################
       ## Credit: The code in this else clause comes from: ##
       ##         https://gitlab.com/snippets/16220        ##
       ######################################################

       hover <- input$plotValue_hover

       # calculate point position INSIDE the image as percent of total
       # dimensions from left (horizontal) and from top (vertical)
       left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
       top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

       # calculate distance from left and bottom side of the picture in pixels
       left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
       top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

       # create style property fot tooltip
       # background color is set so tooltip is a bit transparent
       # z-index is set so we are sure are tooltip will be on top
       style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
         "left:", left_px + 2, "px; top:", top_px + 2 + 800, "px;")

       # actual tooltip created as wellPanel
       wellPanel(
         style = style,
         p(HTML(paste0('Y:', input$plotValue_hover$y)))
       )

       #HTML(paste('Y:', input$plotValue_hover$y))
     }

   })

   #output$plotUV <- plotly::renderPlotly({
   output$plotUV <- renderPlot({
     if (input$go == 0) {
       return(NULL)
     }

     if (values$shouldShow) {
       isolate({
         s <- s_graph(
           data = datasetInput()$data,
           outlier = 'unit_value',
           reference = 'movav_unit_value',
           keep = c(
                    'timePointYears',
                    'unit_value',
                    'movav_unit_value',
                    'unit_value_mirror',
                    'median_world',
                    'median',
                    'out',
                    'reference'
                    )
           )

         values$test_s <- s

         myplotUV <- s %>%
           fun_plot(out = TRUE) +
             scale_size_manual(
               values = c(
                 'unit_value' = 2,
                 'median' = 1.5,
                 'median_world' = 1,
                 'movav_unit_value' = 1.5,
                 'unit_value_mirror' = 1
               )
             ) +
             scale_colour_manual(
               values = c(
                 'unit_value' = 'red',
                 'median' = 'yellow',
                 'median_world' = 'green',
                 'movav_unit_value' = 'orange',
                 'unit_value_mirror' = 'purple'
               )
             ) +
             labs(
               title = 'Unit value',
               subtitle = paste('Reporter:', input$reporter, '- Partner:', input$partner, '- Item:', input$item)
             ) #+
             #geom_line(data = s[!is.na(s$value),], aes(y = value), linetype = 2, size = 1)

         myplotUV
       })
     } else {
       if (input$variable2correct == 'Quantity') {
         # XXX per ora lascio così, ma si dovrebbe aggiornare solo quando si clicca go10
         #isolate({
         d_imputed <- s_graph(
             data = datasetInput()$data,
             outlier = 'unit_value',
             reference = 'movav_unit_value',
             keep = c(
                      'timePointYears',
                      'unit_value',
                      'movav_unit_value',
                      'unit_value_mirror',
                      'median_world',
                      'median',
                      'out',
                      'reference'
                      )
             )

         myplotUV_imputed <- d_imputed %>%
           fun_plot(out = TRUE) +
             scale_size_manual(values = c('unit_value' = 2, 'median' = 1.5, 'median_world' = 1, 'movav_unit_value' = 1.5, 'unit_value_mirror' = 1)) +
             scale_colour_manual(values = c('unit_value' = 'red', 'median' = 'yellow', 'median_world' = 'green', 'movav_unit_value' = 'orange', 'unit_value_mirror' = 'purple')) +
             labs(title = 'Unit value', subtitle = paste('Reporter:', input$reporter, '- Partner:', input$partner, '- Item:', input$item)) #+
             #geom_line(data = d_imputed[!is.na(s$value),], aes(y = value), linetype = 2, size = 1)

         if (input$choose_correction == 'Mirror flow') {
           tmp_qty <- (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['qty_mirror']]
           tmp_value <- (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['value']]

           myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate_db_imputed(
               to_impute = 'imputed_unit_value',
               correct   = tmp_value / tmp_qty,
               original  = d_imputed$value,
               variable  = 'unit_value'
             ) # XXX twice?
         } else if (input$choose_correction == 'Measurement factor') {

           myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate(
               imputed_unit_value = ifelse(timePointYears == input$year2correct, d_imputed$value / as.numeric(input$correction10), d_imputed$value),
               imputed_unit_value = ifelse(variable == 'unit_value', imputed_unit_value, NA)
             )
         } else if (input$choose_correction == 'Outlier correction') {

           myvar <- switch(
                           input$correction_outlier,
                           'Moving average'  = 'movav_unit_value',
                           'Median partners' = 'median_uv',
                           'Median world'    = 'median_uv_world'
                           )

           # XXX ugliness 100%
           s <- datasetInput()[[myvar]]
           tmp <- as.numeric(s[s$timePointYears == input$year2correct, 2])

           myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate(
               imputed_unit_value = ifelse(timePointYears == input$year2correct, tmp, d_imputed$value),
               imputed_unit_value = ifelse(variable == 'unit_value', imputed_unit_value, NA)
             )
         } else if (input$choose_correction == 'Expert knowledge') {
           tmp_qty <- as.numeric(input$correction_expert)
           tmp_value <- (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['value']]

           myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate(
               imputed_unit_value = ifelse(timePointYears == input$year2correct, tmp_value / tmp_qty, d_imputed$value),
               imputed_unit_value = ifelse(variable == 'unit_value', imputed_unit_value, NA)
             )
         } else {
           # XXX default, dovrebbe essere col metodo "None"
            myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate(
               imputed_unit_value = ifelse(timePointYears == input$year2correct, d_imputed$value, d_imputed$value), # XXX twice?
               imputed_unit_value = ifelse(variable == 'unit_value', imputed_unit_value, NA)
             )
         }

         if (values$remove_old) {
           myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate(
               value = ifelse(variable == 'unit_value', imputed_unit_value, value)
             ) 
         }


         #})
        } else { # variable2correct == 'Value'
         # XXX per ora lascio così, ma si dovrebbe aggiornare solo quando si clicca go10
         #isolate({
         d_imputed <- s_graph(
             data      = datasetInput()$data,
             outlier   = 'unit_value',
             reference = 'movav_unit_value',
             keep      = c(
                           'timePointYears',
                           'unit_value',
                           'movav_unit_value',
                           'unit_value_mirror',
                           'median_world',
                           'median',
                           'out',
                           'reference'
                           )
             )

         myplotUV_imputed <- d_imputed %>%
           fun_plot(out = TRUE) +
             scale_size_manual(values = c('unit_value' = 2, 'median' = 1.5, 'median_world' = 1, 'movav_unit_value' = 1.5, 'unit_value_mirror' = 1)) +
             scale_colour_manual(values = c('unit_value' = 'red', 'median' = 'yellow', 'median_world' = 'green', 'movav_unit_value' = 'orange', 'unit_value_mirror' = 'purple')) +
             labs(title = 'Unit value', subtitle = paste('Reporter:', input$reporter, '- Partner:', input$partner, '- Item:', input$item)) #+
             #geom_line(data = d_imputed[!is.na(s$value),], aes(y = value), linetype = 2, size = 1)

         if (input$choose_correction == 'Mirror flow') {
           tmp_qty <- (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['qty']]
           tmp_value <- (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['value_mirror']] * ifelse(input$flow == '1', 1.12, 1/1.12)

           myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate_db_imputed(
               to_impute = 'imputed_unit_value',
               correct   = tmp_value / tmp_qty,
               original  = d_imputed$value,
               variable  = 'unit_value'
             ) # XXX twice?
         } else if (input$choose_correction == 'Measurement factor') {

           myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate(
               imputed_unit_value = ifelse(timePointYears == input$year2correct, d_imputed$value * as.numeric(input$correction10), d_imputed$value),
               imputed_unit_value = ifelse(variable == 'unit_value', imputed_unit_value, NA)
             )
         } else if (input$choose_correction == 'Outlier correction') {

           myvar <- switch(
                           input$correction_outlier,
                           'Moving average'  = 'movav_unit_value',
                           'Median partners' = 'median_uv',
                           'Median world'    = 'median_uv_world'
                           )

           # XXX ugliness 100%
           s <- datasetInput()[[myvar]]
           tmp <- as.numeric(s[s$timePointYears == input$year2correct, 2])

           myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate(
               imputed_unit_value = ifelse(timePointYears == input$year2correct, tmp, d_imputed$value),
               imputed_unit_value = ifelse(variable == 'unit_value', imputed_unit_value, NA)
             )
         } else if (input$choose_correction == 'Expert knowledge') {
           tmp_value <- as.numeric(input$correction_expert)
           tmp_qty <- (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['qty']]

           myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate(
               imputed_unit_value = ifelse(timePointYears == input$year2correct, tmp_value / tmp_qty, d_imputed$value),
               imputed_unit_value = ifelse(variable == 'unit_value', imputed_unit_value, NA)
             )
         } else {
           # XXX default, dovrebbe essere col metodo "None"
            myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate(
               imputed_unit_value = ifelse(timePointYears == input$year2correct, d_imputed$value, d_imputed$value), # XXX twice?
               imputed_unit_value = ifelse(variable == 'unit_value', imputed_unit_value, NA)
             )
         }

         if (values$remove_old) {
           myplotUV_imputed$data <- myplotUV_imputed$data %>%
             mutate(
               value = ifelse(variable == 'unit_value', imputed_unit_value, value)
             ) 
         }


         #})
        }

        values$imputed_uv <- myplotUV_imputed$data %>%
          filter(variable == 'unit_value') %>%
          select(timePointYears, variable, original = value, imputed = imputed_unit_value)

        myplotUV_imputed + geom_line(aes(y = imputed_unit_value), colour ='black', size = 1.5)
     }
   })

   output$plotValue <- renderPlot({
     if (input$go == 0) {
       return(NULL)
     }

     if (values$shouldShow) {
       isolate({
         s <- s_graph(
             data      = datasetInput()$data,
             reference = 'movav_value',
             keep      = c('timePointYears', 'value', 'movav_value', 'value_mirror', 'reference')
           )

         myplotValue <- s %>%
           fun_plot() +
             scale_size_manual(values = c('value' = 2, 'movav_value' = 1.5, 'value_mirror' = 1)) +
             scale_colour_manual(
               values = c(
                 'value'        = 'red',
                 'movav_value'  = 'orange',
                 'value_mirror' = 'purple'
               )
             ) +
             labs(title = 'Value', subtitle = paste('Reporter:', input$reporter, '- Partner:', input$partner, '- Item:', input$item)) #+
             #geom_line(data = s[!is.na(s$value),], aes(y = value), linetype = 2, size = 1)

         myplotValue
       })
     } else {
       #isolate({
       d_imputed <- s_graph(
             data = datasetInput()$data,
             reference = 'movav_value',
             keep      = c('timePointYears', 'value', 'movav_value', 'value_mirror', 'reference')
           )

       myplotValue_imputed <- d_imputed %>%
         fun_plot() +
           scale_size_manual(values = c('value' = 2, 'movav_value' = 1.5, 'value_mirror' = 1)) +
             scale_colour_manual(
               values = c(
                 'value'        = 'red',
                 'movav_value'  = 'orange',
                 'value_mirror' = 'purple'
               )
             ) +
           labs(title = 'Value', subtitle = paste('Reporter:', input$reporter, '- Partner:', input$partner, '- Item:', input$item)) #+
           #geom_line(data = s[!is.na(s$value),], aes(y = value), linetype = 2, size = 1)

       if (input$variable2correct == 'Value') {
         if (input$choose_correction == 'Mirror flow') {
           tmp <- (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['value_mirror']] * ifelse(input$flow == '1', 1.12, 1/1.12)

            myplotValue_imputed$data <- myplotValue_imputed$data %>%
              mutate_db_imputed(
                to_impute = 'imputed_value',
                correct   = tmp,
                original  = d_imputed$value, variable = 'value'
            )

         } else if (input$choose_correction == 'Measurement factor') {
           myplotValue_imputed$data <- myplotValue_imputed$data %>%
             mutate_db_imputed(
               to_impute = 'imputed_value',
               correct   = d_imputed$value * as.numeric(input$correction10),
               original  = d_imputed$value,
               variable  = 'value'
             )

         } else if (input$choose_correction == 'Outlier correction') {
           myvar <- switch(
                           input$correction_outlier,
                           'Moving average'  = 'movav_unit_value',
                           'Median partners' = 'median_uv',
                           'Median world'    = 'median_uv_world'
                           )

           # XXX ugliness 100%
           s <- datasetInput()[[myvar]]
           tmp <- as.numeric(s[s$timePointYears == input$year2correct, 2])
           tmp_qty <- (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['qty']]

           myplotValue_imputed$data <- myplotValue_imputed$data %>%
             mutate_db_imputed(
               to_impute = 'imputed_value',
               correct   = tmp_qty * tmp,
               original  = d_imputed$value,
               variable  = 'value'
             )

         } else if (input$choose_correction == 'Expert knowledge') {
           tmp <- as.numeric(input$correction_expert)

            myplotValue_imputed$data <- myplotValue_imputed$data %>%
              mutate_db_imputed(
                to_impute = 'imputed_value',
                correct   = tmp,
                original  = d_imputed$value,
                variable  = 'value'
              )

         } else {
           # XXX default, dovrebbe essere col metodo "None"
           myplotValue_imputed$data <- myplotValue_imputed$data %>%
             mutate_db_imputed(
               to_impute = 'imputed_value',
               correct   = d_imputed$value,
               original  = d_imputed$value,
               variable  = 'value'
             ) # XXX twice?

         }

         if (values$remove_old) {
           myplotValue_imputed$data <- myplotValue_imputed$data %>%
             mutate(value = ifelse(variable == 'value', imputed_value, value))
         }

         values$imputed_value <- myplotValue_imputed$data %>%
           filter(variable == 'value') %>%
           select(timePointYears, variable, original = value, imputed = imputed_value)

         myplotValue_imputed + geom_line(aes(y = imputed_value), colour ='black', size = 1.5)

       } else { # variable2correct == 'Quantity'
         values$imputed_value <- myplotValue_imputed$data %>%
           filter(variable == 'value') %>%
           select(timePointYears, variable, original = value) %>%
           ## XXX this may be given always
           mutate(imputed = original)

         myplotValue_imputed
       }
     }
   })

   output$totalImpact <- renderTable({

     if (values$shouldShow) {
       # Default case: no imputation
       db_total_out <- datasetInput()$total
     } else {
       if (input$variable2correct == 'Quantity') {
         db_total_out <- values$db_imputed %>%
           select(-contains('_bilat_'), -value_imputed) %>%
           mutate(
             `absolute diff`  = qty_imputed - qty_original,
             `percent diff`   = (qty_imputed / qty_original - 1) * 100
           ) %>%
           rename(
             `original quantity`   = qty_original,
             `imputed quantity`    = qty_imputed,
             `value`               = value_original,
             `original unit value` = unit_value_original,
             `imputed unit value`  = unit_value_imputed
           )
       } else if (input$variable2correct == 'Value') {
         db_total_out <- values$db_imputed %>%
           select(-contains('_bilat_'), -qty_imputed) %>%
           mutate(
             `absolute diff`  = value_imputed - value_original,
             `percent diff`   = (value_imputed / value_original - 1) * 100
           ) %>%
           rename(
             `original value`      = value_original,
             `imputed value`       = value_imputed,
             `qty`                 = qty_original,
             `original unit value` = unit_value_original,
             `imputed unit value`  = unit_value_imputed
           )
       }
     }

     db_total_out %>%
       rename(year = timePointYears)
   })

   output$plotTotal <- renderPlotly({
     if (input$go == 0) {
       return(NULL)
     }

     if (values$shouldShow) {
       g <- datasetInput()$total %>%
         gather(variable, value, -timePointYears) %>% mutate(imputed = NA) %>%
         ggplot(aes(x = timePointYears, y = value, group = variable)) +
           geom_line(colour = 'red', size = 1) +
           scale_colour_manual(values = 'red') +
           facet_wrap(~variable, nrow = 3, scales = 'free') +
           theme(legend.title = element_blank(), axis.title = element_blank(), legend.position = 'bottom')

       ggplotly(g)
     } else {

       db_bilateral <- bind_rows(values$imputed_qty, values$imputed_value)

       bilateral_imputed  <- db_bilateral %>%
         select(-original) %>%
         spread(variable, imputed) %>%
         rename(qty_bilat_imputed = qty, value_bilat_imputed = value)

       bilateral_original <- db_bilateral %>%
         select(-imputed) %>%
         spread(variable, original) %>%
         rename(qty_bilat_original = qty, value_bilat_original = value)

       values$db_imputed <- datasetInput()$total %>%
         left_join(bilateral_original, by = 'timePointYears') %>%
         left_join(bilateral_imputed,  by = 'timePointYears') %>%
         rowwise() %>%
         mutate(
           qty_imputed   = sum(qty,   -qty_bilat_original,   qty_bilat_imputed,   na.rm = TRUE),
           value_imputed = sum(value, -value_bilat_original, value_bilat_imputed, na.rm = TRUE)
         ) %>%
         ungroup() %>%
         mutate(unit_value_imputed = value_imputed / qty_imputed) %>%
         rename(qty_original = qty, value_original = value, unit_value_original = unit_value) %>%
         select(timePointYears, qty_original, qty_imputed, value_original, value_imputed, unit_value_original, unit_value_imputed, everything())

       x_original <- values$db_imputed %>%
         select(timePointYears, qty_original, value_original, unit_value_original) %>%
         gather(variable, value, -timePointYears) %>%
         mutate(variable = stringr::str_replace(variable, '_original', ''))

       x_imputed <- values$db_imputed %>%
         select(timePointYears, qty_imputed, value_imputed, unit_value_imputed) %>%
         gather(variable, value, -timePointYears) %>%
         mutate(variable = stringr::str_replace(variable, '_imputed', ''))

       db_imputed_for_plot <- full_join(
         x_original,
         x_imputed,
         by = c("timePointYears", "variable"),
         suffix = c("_original", "_imputed")
       )

       #g <- db_imputed_for_plot %>%
       #  ggplot(aes(x = timePointYears, group = variable, colour = variable)) +
       #    geom_line(aes(y = value_original)) +
       #    #geom_line(aes(y = value_original), colour = 'red', size = 1.5) +
       #    geom_line(aes(y = value_imputed)) +
       #    #geom_line(aes(y = value_imputed), colour = 'green', size = 1.5) +
       #    #scale_colour_manual(values = c('red', 'green')) +
       #    facet_wrap(~variable, nrow = 3, scales = 'free') +
       #    theme(legend.title = element_blank(), axis.title = element_blank(), legend.position = 'left')

       g <- gather(db_imputed_for_plot, key, var, -timePointYears, - variable) %>%
         mutate(key = stringr::str_replace(key, 'value_', '')) %>%
         ggplot(aes(x = as.character(timePointYears), y = var, group = key, colour = key)) +
           scale_colour_manual(values = c('black', 'red')) +
           geom_line(size = 1) +
           theme(legend.title = element_blank(), axis.title = element_blank(), legend.position = 'left') +
           facet_wrap(~variable, ncol=1, scales = 'free')

       ggplotly(g) %>%
         layout(legend = list(orientation = "h", y = -0.06, x = 0.45))

       #db_imputed_for_plot %>%
       #  gather(key, var, -timePointYears, - variable) %>%
       #  ggplot(aes(x = timePointYears, y = var, group = key)) +
       #    geom_line() +
       #    facet_wrap(~variable, ncol = 1, scales = 'free')

       #    #iris %>% ggplot(aes(x = Sepal.Length, y = Petal.Length, group = Species, color = 'Species')) + geom_point() + facet_wrap(~Species, ncol = 1)
       #    g
       #ggplotly(g, showlegend = TRUE) %>% layout(legend = list(orientation = "h", y = -0.1, x = 0.2))
     }
   })

   output$plotQuantity <- renderPlot({
     if (input$go == 0) {
       return(NULL)
     }

     if (values$shouldShow) {
       isolate({
         s <- s_graph(
             data      = datasetInput()$data,
             reference = 'movav_qty',
             keep      = c('timePointYears', 'qty', 'movav_qty', 'qty_mirror', 'reference')
           )

         myplotQuantity <- s %>%
           fun_plot() +
             scale_size_manual(values = c('qty' = 2, 'movav_qty' = 1.5, 'qty_mirror' = 1)) +
             scale_colour_manual(
               values = c(
                 'qty'        = 'red',
                 'movav_qty'  = 'orange',
                 'qty_mirror' = 'purple'
               )
             ) +
             labs(title = 'Quantity', subtitle = paste('Reporter:', input$reporter, '- Partner:', input$partner, '- Item:', input$item)) #+
             #geom_line(data = s[!is.na(s$value),], aes(y = value), linetype = 2, size = 1)

         myplotQuantity
       })
     } else {
       #isolate({
       d_imputed <- s_graph(
             data      = datasetInput()$data,
             reference = 'movav_qty',
             keep      = c('timePointYears', 'qty', 'movav_qty', 'qty_mirror', 'reference')
           )

       myplotQuantity_imputed <- d_imputed %>%
         fun_plot() +
           scale_size_manual(values = c('qty' = 2, 'movav_qty' = 1.5, 'qty_mirror' = 1)) +
             scale_colour_manual(
               values = c(
                 'qty'        = 'red',
                 'movav_qty'  = 'orange',
                 'qty_mirror' = 'purple'
               )
             ) +
           labs(title = 'Quantity', subtitle = paste('Reporter:', input$reporter, '- Partner:', input$partner, '- Item:', input$item)) #+
           #geom_line(data = s[!is.na(s$value),], aes(y = value), linetype = 2, size = 1)

       if (input$variable2correct == 'Quantity') {

          if (input$choose_correction == 'Mirror flow') {
            tmp <- (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['qty_mirror']]

             myplotQuantity_imputed$data <- myplotQuantity_imputed$data %>%
               mutate_db_imputed(
                 to_impute = 'imputed_qty',
                 correct   = tmp,
                 original  = d_imputed$value, variable = 'qty'
             )

          } else if (input$choose_correction == 'Measurement factor') {
            myplotQuantity_imputed$data <- myplotQuantity_imputed$data %>%
              mutate_db_imputed(
                to_impute = 'imputed_qty',
                correct   = d_imputed$value * as.numeric(input$correction10),
                original  = d_imputed$value,
                variable  = 'qty'
              )

          } else if (input$choose_correction == 'Outlier correction') {
            myvar <- switch(input$correction_outlier, 'Moving average' = 'movav_unit_value', 'Median partners' = 'median_uv', 'Median world' = 'median_uv_world')

            # XXX ugliness 100%
            s <- datasetInput()[[myvar]]
            tmp <- as.numeric(s[s$timePointYears == input$year2correct, 2])
            tmp_value <- (datasetInput()$data %>% filter(timePointYears == input$year2correct))[['value']]

            myplotQuantity_imputed$data <- myplotQuantity_imputed$data %>%
              mutate_db_imputed(
                to_impute = 'imputed_qty',
                correct   = tmp_value / tmp,
                original  = d_imputed$value,
                variable  = 'qty'
              )

          } else if (input$choose_correction == 'Expert knowledge') {
            tmp <- as.numeric(input$correction_expert)

             myplotQuantity_imputed$data <- myplotQuantity_imputed$data %>%
               mutate_db_imputed(
                 to_impute = 'imputed_qty',
                 correct   = tmp,
                 original  = d_imputed$value,
                 variable  = 'qty'
               )

          } else {
            # XXX default, dovrebbe essere col metodo "None"
            myplotQuantity_imputed$data <- myplotQuantity_imputed$data %>%
              mutate_db_imputed(
                to_impute = 'imputed_qty',
                correct   = d_imputed$value,
                original  = d_imputed$value,
                variable  = 'qty'
              ) # XXX twice?

          }

          if (values$remove_old) {
            myplotQuantity_imputed$data <- myplotQuantity_imputed$data %>%
              mutate(value = ifelse(variable == 'qty', imputed_qty, value))
          }

          values$imputed_qty <- myplotQuantity_imputed$data %>%
            filter(variable == 'qty') %>%
            select(timePointYears, variable, original = value, imputed = imputed_qty)

          myplotQuantity_imputed + geom_line(aes(y = imputed_qty), colour ='black', size = 1.5)

         #})
       } else { # variable2correct == 'Value'
          values$imputed_qty <- myplotQuantity_imputed$data %>%
            filter(variable == 'qty') %>%
            select(timePointYears, variable, original = value) %>%
            ## XXX this may be given always in any case
            mutate(imputed = original)


          myplotQuantity_imputed
       }
     }
   })

  output$okHSdata <- downloadHandler(
    filename = function() {
      paste('data', 'csv', sep = '.')
    },
    content = function(file) {
      a <- rsdmx::readSDMX('https://comtrade.un.org/ws/getsdmxtarifflinev1.aspx?y=2011&r=124&p=156&rg=1&comp=false&cc=440399*')
      d <- try(as.data.frame(a))

      if (class(d) != 'try-error') {
        dHS <- d %>%
          select(
            -UNIT_MULT,
            -DECIMALS,
            -CURRENCY,
            -FREQ,
            -TIME_FORMAT,
            -REPORTED_CLASSIFICATION,
            -FLOWS_IN_DATASET,
            -REPORTED_CURRENCY,
            -CONVERSION_FACTOR
          )
      } else {
        dHS <- data.frame(message='NO DATA')
      }

      write.csv(dHS, file)
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data', 'csv', sep = '.')
    },
    content = function(file) {
      write.csv(datasetInput()$data, file)
    }
  )
}

shinyApp(ui = ui, server = server, enableBookmarking = 'url')
