# Packages: shiny (required for the app) + the Task 1 packages dplyr, sf, knitr, kableExtra.
# Charts and maps use base R graphics (plot, barplot, legend).
library(shiny); library(dplyr); library(sf); library(knitr); library(kableExtra)

d   <- readRDS("data/processed/lga_map.rds")
tab <- st_drop_geometry(d)
ta1 <- readRDS("data/processed/ta1_raw_values.rds")
ta2 <- readRDS("data/processed/ta2_processed_values.rds")
ta3 <- readRDS("data/processed/ta3_normalised_index.rds")
issue_log <- read.csv("data/processed/issue_log.csv", stringsAsFactors = FALSE)

fmt <- function(x, n = 0) trimws(format(round(x, n), big.mark = ",", nsmall = n))
ind_choices <- c("Unique stops" = "unique_stops", "Unique routes" = "unique_routes",
                 "Total daily frequency (services/day)" = "total_daily_freq")
sector_cols <- c("Inner Melbourne" = "#1b9e77", "Middle Melbourne" = "#d95f02", "Outer Melbourne" = "#7570b3")
lga_names <- sort(tab$lga_name21)

# kable table -> HTML for Shiny (knitr + kableExtra)
kt <- function(df, height = NULL, ...) {
  k <- kable(df, format = "html", format.args = list(big.mark = ","), ...) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "hover"), full_width = TRUE, font_size = 13)
  if (!is.null(height)) k <- scroll_box(k, height = height, width = "100%")
  HTML(as.character(k))
}

# Choropleth with base graphics: colour bins hold roughly equal numbers of LGAs (quantile bins)
draw_map <- function(vals, pal, digits = 0, legend_title = "", selected = "") {
  brks <- unique(quantile(vals, probs = seq(0, 1, length.out = 7), na.rm = TRUE))
  cols <- hcl.colors(length(brks) - 1, pal, rev = TRUE)
  col  <- cols[as.integer(cut(vals, brks, include.lowest = TRUE))]
  lab  <- paste(formatC(brks[-length(brks)], format = "f", digits = digits, big.mark = ","), "to",
                formatC(brks[-1], format = "f", digits = digits, big.mark = ","))
  par(mar = c(0, 0, 0, 0))
  plot(st_geometry(d), col = col, border = "white", lwd = 0.8)
  if (nzchar(selected) && selected %in% d$lga_name21)
    plot(st_geometry(d[d$lga_name21 == selected, ]), add = TRUE, col = NA, border = "#d7191c", lwd = 3)
  legend("bottomright", legend = lab, fill = cols, title = legend_title, bty = "n", cex = 0.9)
}

# Which LGA is under the mouse? (point-in-polygon on the map coordinates)
lga_at <- function(h) {
  if (is.null(h) || is.null(h$x) || is.null(h$y)) return(NULL)
  pt <- st_sfc(st_point(c(h$x, h$y)), crs = st_crs(d))
  i <- which(lengths(st_intersects(d, pt)) > 0)
  if (length(i)) d$lga_name21[i[1]] else NULL
}

# Findings text is computed from the data so it always matches the tables
top_idx    <- tab %>% arrange(index_rank) %>% slice(1:3)
low_idx    <- tab %>% arrange(desc(index_rank)) %>% slice(1:3)
most_stops <- tab[which.max(tab$unique_stops), ]
densest    <- tab[which.max(tab$stops_per_km2), ]
sparsest   <- tab[which.min(tab$stops_per_km2), ]
low_match  <- tab %>% filter(freq_match_rate < 0.7) %>% pull(lga_name21)

ui <- navbarPage("Public Transport Service Availability in Greater Melbourne",
  tabPanel("1. Findings",
    h3("Where is public transport service concentrated, and is it fair per resident?"),
    p("Three linked views: (A) how much service each area has, (B) how intense it is for the area's size, and (C) how it compares once population is taken into account."),
    hr(),
    h4("A. How much service does each LGA have? (raw counts)"),
    p(strong("How to read:"), " pick a measure; darker areas have more (colours group the LGAs into six equal-sized bands). The bars rank all 35 LGAs. Hover over the map, or pick an LGA to highlight it in red on both."),
    fluidRow(column(4, selectInput("ind1", "Measure", ind_choices)),
             column(4, selectInput("hl1", "Highlight an LGA", c("None", lga_names)))),
    fluidRow(column(6, plotOutput("map1", height = 560, hover = hoverOpts("map1_hover", delay = 100, delayType = "throttle")),
                    uiOutput("info1")),
             column(6, plotOutput("bar1", height = 640))),
    wellPanel(strong("Finding: "), sprintf("%s has the most unique stops (%s), but raw counts favour large or populous areas, so they do not show how well residents are served. ",
      most_stops$lga_name21, fmt(most_stops$unique_stops)),
      strong("Limitation: "), "an LGA total does not describe every suburb inside it."),
    hr(),
    h4("B. How intense is service for the area's size? (derived values)"),
    p(strong("How to read:"), " each bubble is an LGA. Further right = more stops per km²; higher = more frequent routes on average; bigger bubble = more routes; colour = metropolitan sector it mostly lies in. Hover over a bubble for details."),
    fluidRow(column(8, plotOutput("scatter2", height = 520, hover = hoverOpts("scatter_hover", delay = 100, delayType = "throttle"))),
             column(4, selectInput("lga2", "Or select an LGA for a summary", lga_names), uiOutput("card2"))),
    wellPanel(strong("Finding: "), sprintf("Stop density is highest in %s (%s stops/km²) and lowest in %s (%s). Inner and middle LGAs sit to the right of outer LGAs. ",
      densest$lga_name21, fmt(densest$stops_per_km2, 1), sparsest$lga_name21, fmt(sparsest$stops_per_km2, 2)),
      strong("Limitation: "), "sector colour shows where most of the LGA's area lies, not where services are. Average frequency uses only routes that have frequency data."),
    hr(),
    h4("C. Which LGAs are best and least served per resident? (index 0–100)"),
    p(strong("How to read:"), " the Transport Service Availability Index averages three scaled measures: stops, routes and daily services per 1,000 residents. 0 = lowest of these 35 LGAs, 100 = highest. It is a relative score, not a standard. Hover over the map, or pick an LGA to highlight it."),
    fluidRow(column(4, selectInput("hl3", "Highlight an LGA", c("None", lga_names)))),
    fluidRow(column(6, plotOutput("map3", height = 560, hover = hoverOpts("map3_hover", delay = 100, delayType = "throttle")),
                    uiOutput("info3")),
             column(6, uiOutput("tbl3"))),
    wellPanel(strong("Finding: "), sprintf("Highest index: %s. Lowest: %s. ",
      paste0(top_idx$lga_name21, " (", fmt(top_idx$service_index, 1), ")", collapse = ", "),
      paste0(low_idx$lga_name21, " (", fmt(low_idx$service_index, 1), ")", collapse = ", ")),
      "Large, fast-growing outer LGAs rank low even though their raw counts are high. ",
      strong("Limitations: "), "the index measures service availability, not accessibility, travel time or reliability. Per-resident measures favour low-population areas and the CBD (many workers, few residents). ",
      if (length(low_match)) paste0("Frequency data is incomplete for ", paste(low_match, collapse = ", "), " (under 70% of routes matched), so their frequency and index are understated."))
  ),

  tabPanel("2. Purpose & Task Abstraction",
    h3("Purpose and audience"),
    p("Help members of the public explore and compare public transport service availability across Greater Melbourne LGAs, without technical knowledge. The app shows scale, then intensity, then fairness per resident."),
    h3("Narrative"),
    tags$ol(tags$li("Problem: do LGAs have different levels of service?"), tags$li("A. Scale: how much service exists (raw values)."),
            tags$li("B. Intensity: how concentrated it is for the area's size (processed values)."),
            tags$li("C. Fairness: how it compares per resident (normalised index)."), tags$li("Limitations are stated beside every result.")),
    h3("Task abstractions, indicators and idioms"),
    uiOutput("tbl_ta"),
    h3("Index formula"),
    p("x' = (x − min) / (max − min) × 100, calculated across the 35 LGAs for stops, routes and daily frequency per 1,000 residents. Index = (stops' + routes' + frequency') / 3 (equal weights; no evidence favours one component)."),
    h3("Refinements since Task 1"),
    p("Following feedback, the indicator rules were made explicit: unique LGA-route key (vehicle + route number + route name), frequency counted once per LGA-route pair, dominant-sector tie rule, unmatched routes excluded from frequency (never set to zero), and min-max scaling with equal weights. The three analytical ideas are unchanged.")
  ),

  tabPanel("3. Raw Data & Sources", h3("Datasets used"), uiOutput("tbl_raw"),
    p("Population source: ABS, Regional population by age and sex, 2024 release (Population estimates by age and sex, by LGA, Table 3). ",
      tags$a("abs.gov.au", href = "https://www.abs.gov.au/statistics/people/population/regional-population-age-and-sex/latest-release#data-downloads", target = "_blank"))),

  tabPanel("4. Preprocessing", h3("Data quality issues and decisions"),
    p("Each issue: what it is, how it was detected, why it matters, what was done and why that is reasonable."),
    uiOutput("tbl_issues")),

  tabPanel("5. Processed Tables",
    h3("Analysis-ready tables behind the findings"),
    radioButtons("which", NULL, inline = TRUE,
      c("A: Raw values" = "ta1", "B: Processed values" = "ta2", "C: Normalised index" = "ta3")),
    uiOutput("tbl_note"), uiOutput("tbl5")),

  tabPanel("6. Student Credit",
    h3("Student"), p("Kunal Verma (22884100) – CSE5DEV, Semester 2 2026, La Trobe University."),
    h3("Data acknowledgement"),
    p("Public transport stop, route, frequency, LGA boundary, sector and mapping data were supplied by the unit. Population data: Australian Bureau of Statistics (Regional population by age and sex, 2024)."),
    h3("R packages"),
    p("shiny, dplyr, sf, knitr, kableExtra and base R graphics (data preparation also used readr, readxl, tidyr, stringr)."),
    h4("AI Declaration"),
    p("I have used AI to check my code and text. I reviewed, tested amd made final decisions myself. ."),
    )
)

server <- function(input, output, session) {
  sel1 <- reactive(if (is.null(input$hl1) || input$hl1 == "None") "" else input$hl1)
  sel3 <- reactive(if (is.null(input$hl3) || input$hl3 == "None") "" else input$hl3)
  hov1 <- reactive(tryCatch(lga_at(input$map1_hover), error = function(e) NULL))
  hov3 <- reactive(tryCatch(lga_at(input$map3_hover), error = function(e) NULL))

  # ---- A: raw values
  output$map1 <- renderPlot({
    v <- input$ind1
    draw_map(d[[v]], "YlGnBu", 0, names(ind_choices)[ind_choices == v], sel1())
  })
  output$bar1 <- renderPlot({
    v <- input$ind1; x <- tab[order(tab[[v]]), ]
    cols <- ifelse(x$lga_name21 == sel1(), "#d7191c", "#2c7fb8")
    par(mar = c(4, 11, 1, 1))
    barplot(x[[v]], names.arg = x$lga_name21, horiz = TRUE, las = 1, col = cols, border = NA,
            xlab = names(ind_choices)[ind_choices == v], cex.names = 0.8, xlim = c(0, max(x[[v]]) * 1.05))
  })
  output$info1 <- renderUI({
    nm <- hov1(); if (is.null(nm) && nzchar(sel1())) nm <- sel1()
    if (is.null(nm)) return(p(em("Hover over the map or pick an LGA to see its numbers.")))
    r <- tab[tab$lga_name21 == nm, ]
    wellPanel(h4(r$lga_name21), p("Unique stops: ", strong(fmt(r$unique_stops)), " | Unique routes: ", strong(fmt(r$unique_routes)),
                                  " | Daily frequency: ", strong(fmt(r$total_daily_freq))))
  })

  # ---- B: processed values
  output$scatter2 <- renderPlot({
    cols <- unname(sector_cols[as.character(tab$dominant_sector)])
    par(mar = c(4.5, 4.5, 3.5, 1))
    plot(tab$stops_per_km2, tab$avg_daily_freq, pch = 21, col = "white", bg = adjustcolor(cols, 0.65),
         cex = 1 + 4 * sqrt(tab$unique_routes / max(tab$unique_routes)),
         xlab = "Stops per km²", ylab = "Average daily frequency per route")
    sel <- tab[tab$lga_name21 == input$lga2, ]
    points(sel$stops_per_km2, sel$avg_daily_freq, pch = 21, col = "#d7191c", lwd = 3, bg = NA,
           cex = 1 + 4 * sqrt(sel$unique_routes / max(tab$unique_routes)))
    par(xpd = TRUE)
    legend("top", inset = c(0, -0.12), legend = names(sector_cols), pt.bg = adjustcolor(sector_cols, 0.65),
           pch = 21, pt.cex = 2, horiz = TRUE, bty = "n", title = "Dominant sector (bubble size = number of routes)")
  })
  output$card2 <- renderUI({
    near <- tryCatch(nearPoints(tab, input$scatter_hover, xvar = "stops_per_km2", yvar = "avg_daily_freq",
                                threshold = 25, maxpoints = 1), error = function(e) tab[0, ])
    r <- if (nrow(near) == 1) near else tab[tab$lga_name21 == input$lga2, ]
    wellPanel(h4(r$lga_name21),
      p("Stops per km²: ", strong(fmt(r$stops_per_km2, 1))), p("Routes per km²: ", strong(fmt(r$routes_per_km2, 2))),
      p("Avg daily frequency per route: ", strong(fmt(r$avg_daily_freq, 1))), p("Vehicle types: ", strong(r$vehicle_mix)),
      p("Sector mix: ", strong(r$sector_mix)),
      if (r$freq_match_rate < 0.7) p(style = "color:#b00", "Note: only ", round(r$freq_match_rate * 100), "% of routes have frequency data."))
  })

  # ---- C: normalised index
  output$map3 <- renderPlot(draw_map(d$service_index, "viridis", 1, "Index (0–100)", sel3()))
  output$info3 <- renderUI({
    nm <- hov3(); if (is.null(nm) && nzchar(sel3())) nm <- sel3()
    if (is.null(nm)) return(p(em("Hover over the map or pick an LGA to see its index.")))
    r <- tab[tab$lga_name21 == nm, ]
    wellPanel(h4(r$lga_name21, " – index ", fmt(r$service_index, 1), " (rank ", r$index_rank, " of 35)"),
      p("Population: ", strong(fmt(r$population)), " | Stops/1,000: ", strong(fmt(r$stops_per_1000, 2)),
        " | Routes/1,000: ", strong(fmt(r$routes_per_1000, 2)), " | Daily frequency/1,000: ", strong(fmt(r$freq_per_1000, 1))))
  })
  output$tbl3 <- renderUI({
    x <- tab %>% arrange(index_rank) %>%
      transmute(Rank = index_rank, LGA = lga_name21, Index = round(service_index, 1), Population = population,
                `Stops/1,000` = round(stops_per_1000, 2), `Routes/1,000` = round(routes_per_1000, 2),
                `Freq/1,000` = round(freq_per_1000, 1))
    kt(x, height = "560px")
  })

  # ---- Tabs 2-5
  output$tbl_ta <- renderUI(kt(data.frame(
    Task = c("A. Raw value", "B. Processed value", "C. Normalised / indexed"),
    `Action-target` = c("Explore the spatial distribution of service volume (stops, routes, daily frequency) across LGAs",
                        "Compare service intensity (stops per km², average frequency) across LGAs, grouped by sector",
                        "Rank LGAs by a population-normalised service availability index"),
    Idioms = c("Choropleth map + ranked horizontal bar chart", "Bubble scatter plot (size, colour) + summary card", "Choropleth map + ranked table"),
    check.names = FALSE)))
  output$tbl_raw <- renderUI(kt(data.frame(
    File = c("LGA_2021_LGA_2024.csv", "lga_area.csv / .geojson", "lga_details.csv", "lga_adjacents.csv", "lga_sector.csv",
             "lga_stops_routes.csv", "routes_city_freq.csv", "stops.geojson", "ABS Table 3 (2024)"),
    Description = c("2021 to 2024 LGA code mapping", "LGA area, distance to CBD; boundaries", "Facility counts (context only; not used in indicators)",
                    "Neighbouring LGAs (context only)", "LGA overlap with metropolitan sectors", "Stop-route records by LGA",
                    "Route-level service frequency", "Raw stop locations", "Estimated resident population by age, LGA, 30 June 2024"),
    Rows = c("565", "36", "35", "174", "41", "41,188", "453", "28,977", "547 (Australia)"),
    Source = c(rep("Supplied by unit", 8), "Australian Bureau of Statistics"))))
  output$tbl_issues <- renderUI(kt(issue_log, height = "600px",
    col.names = c("#", "File", "Issue", "Detected by", "Why it matters", "Treatment", "Why reasonable")))
  output$tbl_note <- renderUI(switch(input$which,
    ta1 = p("Unique stops = distinct stop IDs; unique routes = distinct LGA-route pairs; daily frequency summed once per LGA-route pair (routes without frequency excluded, not zero)."),
    ta2 = p("Stops per km² = unique stops / area; average frequency = mean over routes with frequency data; dominant sector = largest area share (ties go to the more central sector)."),
    ta3 = p("Per 1,000 residents using ABS 2024 population via the 2021→2024 crosswalk; scaled 0–100 with min-max; index = equal-weight average.")))
  output$tbl5 <- renderUI({
    x <- switch(input$which, ta1 = ta1, ta2 = ta2, ta3 = ta3)
    kt(x %>% mutate(across(where(is.numeric), ~ round(.x, 3))), height = "600px")
  })
}

shinyApp(ui, server)
