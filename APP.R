# =====================================================================
# BAZAR JELLIAN B - DRUGSTORE INVENTORY DASHBOARD (Shiny)
# Save as app.R in the same folder as the .xlsx file,
# then click "Run App" in Posit Cloud.
# =====================================================================
pkgs <- c("shiny", "bslib", "DT", "dplyr", "ggplot2", "readxl")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
library(shiny); library(bslib); library(DT)
library(dplyr); library(ggplot2); library(readxl)

# ---- Palette (shared by CSS, tables and charts) -----------------------
COL <- list(
  ink    = "#1B2A41",  # text, headings
  paper  = "#F2F5F8",  # page background
  line   = "#D9E0E8",  # borders
  brand  = "#2446A8",  # primary actions, headers
  ok     = "#2F8F6B",  # healthy stock
  warn   = "#E2A03F",  # expiring soon
  alert  = "#C93C2E",  # low stock, expired
  rx     = "#B7791F",  # prescription items
  otc    = "#6B7C93"   # over-the-counter items
)

# ---- Data -------------------------------------------------------------
DATA_FILE <- if (file.exists("DRUGSTORE_DATASET_1.xlsx")) {
  "DRUGSTORE_DATASET_1.xlsx"
} else {
  "DRUGSTORE_DATASET.xlsx"
}

# Expiry_Date mixes "8/15/2027" text with Excel serial numbers (46390)
parse_expiry <- function(x) {
  x <- trimws(as.character(x))
  is_serial <- grepl("^[0-9]+(\\.[0-9]+)?$", x)
  out <- rep(as.Date(NA), length(x))
  out[is_serial]  <- as.Date(as.numeric(x[is_serial]), origin = "1899-12-30")
  out[!is_serial] <- as.Date(x[!is_serial], format = "%m/%d/%Y")
  out
}

add_derived <- function(d) {
  d %>% mutate(Stock_Value    = Unit_Price_USD * Stock_Qty,
               Days_To_Expiry = as.numeric(Expiry_Date - Sys.Date()))
}

drug0 <- read_excel(
  DATA_FILE,
  col_types = c("text", "text", "text", "text",
                "numeric", "numeric", "text", "text")
) %>%
  mutate(Expiry_Date = parse_expiry(Expiry_Date),
         Rx_Required = trimws(Rx_Required) == "Yes") %>%
  add_derived()

# ---- Look and feel ----------------------------------------------------
app_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF", fg = COL$ink,
  primary = COL$brand, success = COL$ok,
  warning = COL$warn, danger = COL$alert,
  base_font    = "'Public Sans', system-ui, sans-serif",
  heading_font = "'Bricolage Grotesque', 'Public Sans', sans-serif",
  "border-radius" = "6px"
)

css <- sprintf("
  body { background: %s; }
  .container-fluid { max-width: 1500px; padding: 0 28px 40px; }

  .app-head { display:flex; align-items:baseline; justify-content:space-between;
              flex-wrap:wrap; gap:8px; padding:26px 0 18px; }
  .app-head h1 { font-size:2rem; font-weight:700; letter-spacing:-0.02em; margin:0; }
  .app-head .sub { color:%s; font-size:.95rem; margin-left:12px; font-weight:400; }
  .app-head .asof { color:%s; font-size:.9rem; }

  .filters { background:#fff; border:1px solid %s; border-radius:8px; padding:18px; }
  .filters .form-label, .filters label.control-label { font-weight:600; font-size:.88rem; }
  .filters .form-group, .filters .shiny-input-container { margin-bottom:14px; }

  .kpi-row { display:grid; grid-template-columns:repeat(auto-fit, minmax(170px, 1fr));
             gap:12px; margin-bottom:18px; }
  .kpi { background:#fff; border:1px solid %s; border-left:5px solid %s;
         border-radius:8px; padding:12px 16px; }
  .kpi .lbl { font-size:.82rem; color:%s; }
  .kpi .val { font-family:'Bricolage Grotesque', sans-serif; font-size:1.9rem;
              font-weight:700; line-height:1.15; }
  .kpi.alert { border-left-color:%s; }
  .kpi.alert .val { color:%s; }
  .kpi.warn  { border-left-color:%s; }
  .kpi.good  { border-left-color:%s; }

  .panel-box { background:#fff; border:1px solid %s; border-radius:8px; padding:16px 18px;
               margin-bottom:18px; }
  .panel-box h4 { font-size:1.05rem; font-weight:700; margin:0 0 10px; }

  .nav-tabs { border-bottom:1px solid %s; margin-bottom:16px; }
  .nav-tabs .nav-link { color:%s; font-weight:600; border:none;
                        border-bottom:3px solid transparent; }
  .nav-tabs .nav-link.active { color:%s; border-bottom-color:%s; background:transparent; }

  table.dataTable thead th { font-weight:600; font-size:.85rem; border-bottom:2px solid %s; }
  table.dataTable td { font-size:.9rem; }
  .btn:focus-visible, .form-control:focus-visible { outline:3px solid %s55; outline-offset:2px; }
  .irs--shiny .irs-bar, .irs--shiny .irs-handle { background:%s; border-color:%s; }
  @media (max-width: 768px) { .container-fluid { padding: 0 14px 30px; } }
",
               COL$paper, COL$otc, COL$otc, COL$line,
               COL$line, COL$brand, COL$otc,
               COL$alert, COL$alert, COL$warn, COL$ok,
               COL$line, COL$line, COL$otc, COL$brand, COL$brand, COL$ink,
               COL$brand, COL$brand, COL$brand)

kpi <- function(label, value, tone = "") {
  div(class = paste("kpi", tone), div(class = "lbl", label), div(class = "val", value))
}

theme_shop <- function() {
  theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold", colour = COL$ink),
          plot.title.position = "plot",
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          text = element_text(colour = COL$ink))
}

# ---- UI ---------------------------------------------------------------
ui <- fluidPage(
  theme = app_theme,
  title = "Bazar Jellian B - Drugstore Inventory",
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Bricolage+Grotesque:wght@600;700&family=Public+Sans:wght@400;600&display=swap"),
    tags$style(HTML(css))
  ),
  div(class = "app-head",
      div(h1("Bazar Jellian B", span(class = "sub", "Drugstore inventory"))),
      div(class = "asof", paste("As of", format(Sys.Date(), "%a, %b %d, %Y")))),
  
  sidebarLayout(
    sidebarPanel(
      width = 3, class = "filters",
      textInput("search", "Search name or brand", "", placeholder = "e.g. Paracetamol"),
      selectInput("cat", "Category", choices = sort(unique(drug0$Category)),
                  multiple = TRUE),
      radioButtons("rx", "Prescription", inline = TRUE,
                   choices = c("All", "Rx only", "OTC only")),
      sliderInput("thr", "Low-stock threshold (units)", 10, 300, 60, step = 5),
      sliderInput("soon", "Expiring-soon window (days)", 30, 730, 180, step = 30),
      downloadButton("dl", "Download CSV", class = "btn-outline-primary w-100")
    ),
    mainPanel(
      width = 9,
      uiOutput("kpis"),
      tabsetPanel(
        tabPanel("Inventory", div(class = "panel-box", DTOutput("tbl"))),
        tabPanel("Alerts",
                 div(class = "panel-box", h4("Low stock"), DTOutput("low_tbl")),
                 div(class = "panel-box", h4("Expired or expiring soon"), DTOutput("exp_tbl"))),
        tabPanel("Charts",
                 div(class = "panel-box", plotOutput("p_cat", height = 300)),
                 div(class = "panel-box", plotOutput("p_stock", height = 420)),
                 div(class = "panel-box", plotOutput("p_exp", height = 420))),
        tabPanel("Sell / restock",
                 fluidRow(
                   column(5, div(class = "panel-box",
                                 h4("Record a transaction"),
                                 selectInput("pid", "Product",
                                             choices = setNames(
                                               drug0$Product_ID,
                                               paste(drug0$Product_ID, "-", drug0$Product_Name))),
                                 numericInput("qty", "Quantity", 1, min = 1, step = 1),
                                 checkboxInput("has_rx", "Customer has a prescription", FALSE),
                                 div(class = "d-flex gap-2",
                                     actionButton("sell", "Sell", class = "btn-primary"),
                                     actionButton("restock", "Restock", class = "btn-success")),
                                 helpText(class = "mt-3",
                                          "Changes last for this session only. The Excel file is not modified."))),
                   column(7, div(class = "panel-box",
                                 h4("Session sales log"), DTOutput("log_tbl")))
                 ))
      )
    )
  )
)

# ---- Server -----------------------------------------------------------
server <- function(input, output, session) {
  
  data  <- reactiveVal(drug0)
  sales <- reactiveVal(data.frame(Time = character(), Product = character(),
                                  Qty = integer(), Total_USD = numeric(),
                                  stringsAsFactors = FALSE))
  
  # Filtered view with status labels
  view <- reactive({
    d <- data() %>%
      mutate(Stock_Status = ifelse(Stock_Qty <= input$thr, "Low", "OK"),
             Expiry_Status = case_when(
               Days_To_Expiry < 0            ~ "Expired",
               Days_To_Expiry <= input$soon  ~ "Expiring soon",
               TRUE                          ~ "OK"))
    if (nzchar(input$search)) {
      s <- tolower(input$search)
      d <- d %>% filter(grepl(s, tolower(Product_Name), fixed = TRUE) |
                          grepl(s, tolower(Brand), fixed = TRUE))
    }
    if (length(input$cat)) d <- d %>% filter(Category %in% input$cat)
    if (input$rx == "Rx only")  d <- d %>% filter(Rx_Required)
    if (input$rx == "OTC only") d <- d %>% filter(!Rx_Required)
    d
  })
  
  pretty <- function(d) {
    d %>% mutate(Rx_Required = ifelse(Rx_Required, "Yes", "No"))
  }
  
  output$kpis <- renderUI({
    d <- view()
    n_low <- sum(d$Stock_Status == "Low")
    n_exp <- sum(d$Expiry_Status != "OK")
    div(class = "kpi-row",
        kpi("Products", nrow(d)),
        kpi("Units in stock", format(sum(d$Stock_Qty), big.mark = ",")),
        kpi("Stock value", paste0("$", format(round(sum(d$Stock_Value), 2),
                                              big.mark = ",", nsmall = 2))),
        kpi("Low stock", n_low, if (n_low > 0) "alert" else "good"),
        kpi("Expired or expiring soon", n_exp, if (n_exp > 0) "alert" else "good"))
  })
  
  output$tbl <- renderDT({
    d <- pretty(view()) %>%
      select(Product_ID, Product_Name, Category, Brand, Unit_Price_USD,
             Stock_Qty, Stock_Value, Expiry_Date, Days_To_Expiry,
             Rx_Required, Stock_Status, Expiry_Status)
    datatable(d, rownames = FALSE, options = list(pageLength = 15)) %>%
      formatCurrency(c("Unit_Price_USD", "Stock_Value")) %>%
      formatStyle("Stock_Status",
                  color = styleEqual("Low", COL$alert),
                  fontWeight = styleEqual("Low", "600"),
                  backgroundColor = styleEqual("Low", "#FBE4E1")) %>%
      formatStyle("Expiry_Status",
                  color = styleEqual(c("Expired", "Expiring soon"),
                                     c(COL$alert, "#8A5A0A")),
                  fontWeight = styleEqual(c("Expired", "Expiring soon"),
                                          c("600", "600")),
                  backgroundColor = styleEqual(c("Expired", "Expiring soon"),
                                               c("#FBE4E1", "#FDF0D8"))) %>%
      formatStyle("Rx_Required",
                  color = styleEqual("Yes", COL$rx),
                  fontWeight = styleEqual("Yes", "600"))
  })
  
  output$low_tbl <- renderDT({
    d <- view() %>% filter(Stock_Status == "Low") %>% arrange(Stock_Qty) %>%
      select(Product_ID, Product_Name, Brand, Stock_Qty)
    datatable(d, rownames = FALSE, options = list(dom = "t")) %>%
      formatStyle("Stock_Qty", color = COL$alert, fontWeight = "600")
  })
  
  output$exp_tbl <- renderDT({
    d <- view() %>% filter(Expiry_Status != "OK") %>% arrange(Expiry_Date) %>%
      select(Product_ID, Product_Name, Expiry_Date, Days_To_Expiry, Expiry_Status)
    datatable(d, rownames = FALSE, options = list(dom = "t")) %>%
      formatStyle("Expiry_Status",
                  color = styleEqual(c("Expired", "Expiring soon"),
                                     c(COL$alert, "#8A5A0A")),
                  backgroundColor = styleEqual(c("Expired", "Expiring soon"),
                                               c("#FBE4E1", "#FDF0D8")))
  })
  
  output$p_cat <- renderPlot({
    d <- view() %>% group_by(Category) %>%
      summarise(Value = sum(Stock_Value), .groups = "drop")
    ggplot(d, aes(reorder(Category, Value), Value)) +
      geom_col(fill = COL$brand, width = 0.7) + coord_flip() +
      scale_y_continuous(labels = scales::dollar) +
      labs(title = "Stock value by category", x = NULL, y = NULL) +
      theme_shop()
  })
  
  output$p_stock <- renderPlot({
    d <- view()
    ggplot(d, aes(reorder(Product_Name, Stock_Qty), Stock_Qty, fill = Stock_Status)) +
      geom_col(width = 0.75) + coord_flip() +
      geom_hline(yintercept = input$thr, linetype = "dashed", colour = COL$ink) +
      scale_fill_manual(values = c(Low = COL$alert, OK = COL$ok)) +
      labs(title = "Stock level by product (dashed line = low-stock threshold)",
           x = NULL, y = "Units", fill = NULL) +
      theme_shop() + theme(legend.position = "top")
  })
  
  output$p_exp <- renderPlot({
    d <- pretty(view())
    ggplot(d, aes(Expiry_Date, reorder(Product_Name, Expiry_Date),
                  colour = Rx_Required)) +
      geom_point(size = 3) +
      geom_vline(xintercept = as.numeric(Sys.Date()), linetype = "dashed",
                 colour = COL$ink) +
      scale_colour_manual(values = c(Yes = COL$rx, No = COL$otc)) +
      labs(title = "Expiry dates (dashed line = today)", x = NULL, y = NULL,
           colour = "Rx required") +
      theme_shop() + theme(legend.position = "top")
  })
  
  # ---- Sell ----
  observeEvent(input$sell, {
    d <- data(); i <- which(d$Product_ID == input$pid)
    q <- input$qty
    if (is.na(q) || q < 1) {
      showNotification("Enter a quantity of 1 or more.", type = "error"); return()
    }
    if (d$Expiry_Date[i] < Sys.Date()) {
      showNotification("This product is expired and cannot be sold.", type = "error"); return()
    }
    if (d$Rx_Required[i] && !input$has_rx) {
      showNotification("A prescription is required for this product.", type = "error"); return()
    }
    if (q > d$Stock_Qty[i]) {
      showNotification(paste("Only", d$Stock_Qty[i], "in stock."), type = "error"); return()
    }
    d$Stock_Qty[i] <- d$Stock_Qty[i] - q
    data(add_derived(d))
    total <- round(q * d$Unit_Price_USD[i], 2)
    sales(rbind(sales(), data.frame(
      Time = format(Sys.time(), "%H:%M:%S"), Product = d$Product_Name[i],
      Qty = as.integer(q), Total_USD = total, stringsAsFactors = FALSE)))
    showNotification(sprintf("Sold %d x %s ($%.2f)", as.integer(q), d$Product_Name[i], total),
                     type = "message")
  })
  
  # ---- Restock ----
  observeEvent(input$restock, {
    q <- input$qty
    if (is.na(q) || q < 1) {
      showNotification("Enter a quantity of 1 or more.", type = "error"); return()
    }
    d <- data(); i <- which(d$Product_ID == input$pid)
    d$Stock_Qty[i] <- d$Stock_Qty[i] + q
    data(add_derived(d))
    showNotification(sprintf("Restocked %s. New stock: %d", d$Product_Name[i],
                             as.integer(d$Stock_Qty[i])), type = "message")
  })
  
  output$log_tbl <- renderDT(
    datatable(sales(), rownames = FALSE, options = list(dom = "t"),
              caption = if (nrow(sales()) == 0) "No sales yet. Sold items appear here." else NULL) %>%
      formatCurrency("Total_USD")
  )
  
  output$dl <- downloadHandler(
    filename = function() paste0("drugstore_inventory_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(pretty(data()), file, row.names = FALSE)
  )
}

shinyApp(ui, server)