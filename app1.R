# ============================================================
#  ELDER CARE COMPANION  -  R Shiny
#  Companionship visits + Student volunteers + Health care
#
#  Install once:  install.packages(c("shiny", "DT"))
#  Run:           shiny::runApp("elder_care_app.R")
#
#  Data is saved automatically as CSV files in the same folder:
#  elders.csv, volunteers.csv, visits.csv, health.csv, meds.csv
#
#  NOTE: Health alert limits are simple general guidelines to
#  flag readings for attention. They are not medical advice.
#  Always follow the doctor's instructions for each person.
# ============================================================

library(shiny)
library(DT)

# ------------------------------------------------------------
# 1. DATA SETUP
# ------------------------------------------------------------
elder_cols  <- c("ID", "Name", "Age", "Gender", "Phone", "Address", "Conditions",
                 "Emergency_Contact", "Emergency_Phone", "Needs")
vol_cols    <- c("ID", "Name", "College", "Phone", "Email", "Availability", "Skills", "Status")
visit_cols  <- c("ID", "Elder_ID", "Volunteer_ID", "Date", "Time", "Activity", "Notes", "Status")
health_cols <- c("ID", "Elder_ID", "DateTime", "Systolic", "Diastolic", "Pulse",
                 "Sugar", "Temp_F", "SpO2", "Notes")
med_cols    <- c("ID", "Elder_ID", "Medicine", "Dose", "Time", "Notes")

empty_df <- function(cols) {
  as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                stringsAsFactors = FALSE)
}


load_store <- function(file, cols, sample = NULL) {
  if (file.exists(file)) {
    read.csv(file, stringsAsFactors = FALSE, colClasses = "character")
  } else if (!is.null(sample)) {
    sample
  } else {
    empty_df(cols)
  }
}

sample_elders <- data.frame(
  ID = c("E001", "E002"),
  Name = c("Kamalamma Pillai", "Joseph Varghese"),
  Age = c("78", "82"), Gender = c("Female", "Male"),
  Phone = c("9447000001", "9447000002"),
  Address = c("Pattom, Thiruvananthapuram", "Kowdiar, Thiruvananthapuram"),
  Conditions = c("Diabetes, Hypertension", "Arthritis"),
  Emergency_Contact = c("Suresh (Son)", "Mary (Daughter)"),
  Emergency_Phone = c("9847000001", "9847000002"),
  Needs = c("Companionship, medicine reminders", "Reading, walking help, shopping"),
  stringsAsFactors = FALSE
)


sample_vols <- data.frame(
  ID = c("V001", "V002"),
  Name = c("Anjali S", "Rohan Mathew"),
  College = c("University College", "College of Engineering"),
  Phone = c("9605000001", "9605000002"),
  Email = c("anjali@mail.com", "rohan@mail.com"),
  Availability = c("Weekends", "Weekday evenings"),
  Skills = c("Conversation, reading aloud", "Tech help, shopping"),
  Status = c("Active", "Active"),
  stringsAsFactors = FALSE
)

sample_visits <- data.frame(
  ID = "VS001", Elder_ID = "E001", Volunteer_ID = "V001",
  Date = as.character(Sys.Date() + 1), Time = "16:00",
  Activity = "Companionship / Chat", Notes = "Bring a newspaper",
  Status = "Scheduled", stringsAsFactors = FALSE
)


activities <- c("Companionship / Chat", "Reading aloud", "Walk / Light exercise",
                "Shopping / Errands", "Medicine pickup", "Phone / Tech help",
                "Games / Hobbies", "Hospital / Clinic accompany", "Other")

# Simple alert rules (general guidelines only)
check_vitals <- function(sys, dia, pulse, sugar, temp, spo2) {
  a <- character(0)
  ok <- function(x) !is.na(x)
  if (ok(sys)   && (sys >= 160 || sys <= 90))   a <- c(a, paste("Blood pressure (systolic)", sys))
  if (ok(dia)   && (dia >= 100 || dia <= 55))   a <- c(a, paste("Blood pressure (diastolic)", dia))
  if (ok(pulse) && (pulse > 110 || pulse < 50)) a <- c(a, paste("Pulse", pulse))
  if (ok(sugar) && (sugar >= 200 || sugar < 70)) a <- c(a, paste("Blood sugar", sugar))
  if (ok(temp)  && temp >= 100.4)               a <- c(a, paste("Fever", temp, "F"))
  if (ok(spo2)  && spo2 < 92)                   a <- c(a, paste("Low oxygen (SpO2)", spo2))
  a
}

# ------------------------------------------------------------
# 2. UI
# ------------------------------------------------------------
ui <- navbarPage(
  title = "Elder Care Companion",
  header = tags$head(tags$style(HTML("
    .stat {text-align:center;padding:15px;border-radius:8px;color:#fff;margin-bottom:10px;}
    .stat h2 {margin:0;font-size:38px;} .stat p {margin:0;}
    .alertbox {background:#fdecea;border-left:5px solid #d9534f;padding:8px 12px;margin-bottom:6px;}
    .okbox {background:#eaf6ea;border-left:5px solid #5cb85c;padding:8px 12px;}
  "))),
  
  # ---------------- Dashboard ----------------
  tabPanel("Dashboard", icon = icon("home"),
           fluidRow(
             column(3, uiOutput("s_elders")), column(3, uiOutput("s_vols")),
             column(3, uiOutput("s_upcoming")), column(3, uiOutput("s_done"))
           ),
           fluidRow(
             column(6, h4("Health alerts (last 7 days)"), uiOutput("alerts")),
             column(6, h4("Upcoming visits"), DTOutput("t_upcoming"))
           ),
           fluidRow(column(12, h4("Elders with no upcoming visit"), uiOutput("no_visit")))
  ),
  
  # ---------------- Elders ----------------
  tabPanel("Elders", icon = icon("user"),
           sidebarLayout(
             sidebarPanel(width = 4,
                          h4("Elder details"),
                          textInput("e_name", "Full name"),
                          fluidRow(
                            column(6, numericInput("e_age", "Age", 70, min = 50, max = 120)),
                            column(6, selectInput("e_gender", "Gender", c("Female", "Male", "Other")))
                          ),
                          textInput("e_phone", "Phone"),
                          textInput("e_addr", "Address"),
                          textInput("e_cond", "Health conditions"),
                          textInput("e_ec", "Emergency contact name"),
                          textInput("e_ep", "Emergency contact phone"),
                          textInput("e_needs", "Needs (e.g. companionship, shopping)"),
                          actionButton("e_add", "Add", class = "btn-success"),
                          actionButton("e_upd", "Update", class = "btn-warning"),
                          actionButton("e_del", "Delete", class = "btn-danger")
             ),
             mainPanel(width = 8, DTOutput("t_elders"))
           )
  ),
  
  # ---------------- Volunteers ----------------
  tabPanel("Student Volunteers", icon = icon("hands-helping"),
           sidebarLayout(
             sidebarPanel(width = 4,
                          h4("Volunteer details"),
                          textInput("v_name", "Full name"),
                          textInput("v_college", "College / School"),
                          textInput("v_phone", "Phone"),
                          textInput("v_email", "Email"),
                          textInput("v_avail", "Availability (e.g. weekends)"),
                          textInput("v_skills", "Skills (e.g. reading, tech help)"),
                          selectInput("v_status", "Status", c("Active", "Inactive")),
                          actionButton("v_add", "Add", class = "btn-success"),
                          actionButton("v_upd", "Update", class = "btn-warning"),
                          actionButton("v_del", "Delete", class = "btn-danger")
             ),
             mainPanel(width = 8, DTOutput("t_vols"),
                       br(), uiOutput("vol_hours"))
           )
  ),
  
  # ---------------- Visits ----------------
  tabPanel("Visits & Companionship", icon = icon("calendar"),
           sidebarLayout(
             sidebarPanel(width = 4,
                          h4("Schedule a visit"),
                          selectInput("vi_elder", "Elder", choices = NULL),
                          selectInput("vi_vol", "Volunteer", choices = NULL),
                          dateInput("vi_date", "Date", value = Sys.Date() + 1),
                          textInput("vi_time", "Time", value = "16:00"),
                          selectInput("vi_act", "Activity", activities),
                          textInput("vi_notes", "Notes"),
                          actionButton("vi_add", "Schedule", class = "btn-success"),
                          hr(),
                          h5("Selected visit"),
                          actionButton("vi_done", "Mark completed", class = "btn-primary"),
                          actionButton("vi_cancel", "Cancel visit", class = "btn-warning"),
                          actionButton("vi_del", "Delete", class = "btn-danger")
             ),
             mainPanel(width = 8, DTOutput("t_visits"))
           )
  ),
  
  # ---------------- Health ----------------
  tabPanel("Health Care", icon = icon("heartbeat"),
           sidebarLayout(
             sidebarPanel(width = 4,
                          selectInput("h_elder", "Elder", choices = NULL),
                          tabsetPanel(
                            tabPanel("Vitals",
                                     br(),
                                     fluidRow(
                                       column(6, numericInput("h_sys", "BP Systolic", NA)),
                                       column(6, numericInput("h_dia", "BP Diastolic", NA))
                                     ),
                                     fluidRow(
                                       column(6, numericInput("h_pulse", "Pulse (bpm)", NA)),
                                       column(6, numericInput("h_sugar", "Sugar (mg/dL)", NA))
                                     ),
                                     fluidRow(
                                       column(6, numericInput("h_temp", "Temp (F)", NA)),
                                       column(6, numericInput("h_spo2", "SpO2 (%)", NA))
                                     ),
                                     textInput("h_notes", "Notes"),
                                     actionButton("h_add", "Save reading", class = "btn-success"),
                                     actionButton("h_del", "Delete selected", class = "btn-danger")
                            ),
                            tabPanel("Medicines",
                                     br(),
                                     textInput("m_name", "Medicine"),
                                     textInput("m_dose", "Dose"),
                                     textInput("m_time", "Time (e.g. 8 AM, after food)"),
                                     textInput("m_notes", "Notes"),
                                     actionButton("m_add", "Add medicine", class = "btn-success"),
                                     actionButton("m_del", "Delete selected", class = "btn-danger")
                            )
                          )
             ),
             mainPanel(width = 8,
                       uiOutput("h_summary"),
                       tabsetPanel(
                         tabPanel("Readings", br(), DTOutput("t_health")),
                         tabPanel("Trends", br(),
                                  plotOutput("p_bp", height = 250), plotOutput("p_sugar", height = 250)),
                         tabPanel("Medicines", br(), DTOutput("t_meds"))
                       )
             )
           )
  ),
  
  # ---------------- Help ----------------
  tabPanel("About", icon = icon("info-circle"),
           h3("How to use"),
           tags$ol(
             tags$li("Register elders in the Elders tab, including an emergency contact."),
             tags$li("Register student volunteers with their availability and skills."),
             tags$li("Schedule companionship visits and mark them completed afterwards."),
             tags$li("Record vitals and medicines in Health Care. Unusual readings appear on the Dashboard."),
             tags$li("Click a table row to load it into the form for editing.")
           ),
           p(em("Health alerts are general guidelines only, not a medical diagnosis. In an emergency, call your local emergency number or the doctor."))
  )
)


# ------------------------------------------------------------
# 3. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  elders     <- reactiveVal(load_store("elders.csv", elder_cols, sample_elders))
  vols       <- reactiveVal(load_store("volunteers.csv", vol_cols, sample_vols))
  visits     <- reactiveVal(load_store("visits.csv", visit_cols, sample_visits))
  health     <- reactiveVal(load_store("health.csv", health_cols))
  meds       <- reactiveVal(load_store("meds.csv", med_cols))
  
  save_df <- function(rv, file, df) { rv(df); write.csv(df, file, row.names = FALSE) }
  
  new_id <- function(df, prefix) {
    n <- if (nrow(df)) max(as.integer(gsub("\\D", "", df$ID)), na.rm = TRUE) else 0
    sprintf("%s%03d", prefix, n + 1)
  }
  
  name_of <- function(df, id) {
    out <- df$Name[match(id, df$ID)]
    ifelse(is.na(out), "-", out)
  }
  
  num <- function(x) if (is.null(x) || is.na(x)) "" else as.character(x)
  need_row <- function(i, what) {
    if (is.null(i)) { showNotification(paste("Select a", what, "in the table first."), type = "warning"); FALSE } else TRUE
  }
  
  # Keep dropdowns updated
  observe({
    e <- elders(); ch <- setNames(e$ID, paste(e$Name, "-", e$ID))
    updateSelectInput(session, "vi_elder", choices = ch, selected = isolate(input$vi_elder))
    updateSelectInput(session, "h_elder",  choices = ch, selected = isolate(input$h_elder))
  })
  observe({
    v <- vols(); v <- v[v$Status == "Active", ]
    updateSelectInput(session, "vi_vol", choices = setNames(v$ID, paste(v$Name, "-", v$ID)),
                      selected = isolate(input$vi_vol))
  })
  
  # ======================================================
  #  DASHBOARD
  # ======================================================
  stat <- function(n, label, colour)
    div(class = "stat", style = paste0("background:", colour), h2(n), p(label))
  
  upcoming <- reactive({
    v <- visits()
    v[v$Status == "Scheduled" & as.Date(v$Date) >= Sys.Date(), ]
  })
  
  output$s_elders   <- renderUI(stat(nrow(elders()), "Elders registered", "#3c8dbc"))
  output$s_vols     <- renderUI(stat(sum(vols()$Status == "Active"), "Active volunteers", "#00a65a"))
  output$s_upcoming <- renderUI(stat(nrow(upcoming()), "Upcoming visits", "#f39c12"))
  output$s_done     <- renderUI(stat(sum(visits()$Status == "Completed"), "Visits completed", "#605ca8"))
  
  output$t_upcoming <- renderDT({
    u <- upcoming(); u <- u[order(u$Date, u$Time), ]
    d <- data.frame(Date = u$Date, Time = u$Time,
                    Elder = name_of(elders(), u$Elder_ID),
                    Volunteer = name_of(vols(), u$Volunteer_ID),
                    Activity = u$Activity, stringsAsFactors = FALSE)
    datatable(d, rownames = FALSE, options = list(dom = "tp", pageLength = 5))
  })
  
  output$alerts <- renderUI({
    h <- health()
    if (nrow(h) == 0) return(div(class = "okbox", "No readings recorded yet."))
    h <- h[as.Date(substr(h$DateTime, 1, 10)) >= Sys.Date() - 7, ]
    msgs <- lapply(seq_len(nrow(h)), function(i) {
      r <- h[i, ]
      a <- check_vitals(as.numeric(r$Systolic), as.numeric(r$Diastolic), as.numeric(r$Pulse),
                        as.numeric(r$Sugar), as.numeric(r$Temp_F), as.numeric(r$SpO2))
      if (length(a)) div(class = "alertbox",
                         strong(name_of(elders(), r$Elder_ID)), paste0(" (", r$DateTime, "): "),
                         paste(a, collapse = "; "))
    })
    msgs <- Filter(Negate(is.null), msgs)
    if (length(msgs)) tagList(msgs) else div(class = "okbox", "No unusual readings in the last 7 days.")
  })
  
  output$no_visit <- renderUI({
    e <- elders(); u <- upcoming()
    missing <- e$Name[!(e$ID %in% u$Elder_ID)]
    if (length(missing)) p(paste(missing, collapse = ", "))
    else div(class = "okbox", "Everyone has a visit scheduled.")
  })
  
  # ======================================================
  #  ELDERS
  # ======================================================
  output$t_elders <- renderDT({
    datatable(elders(), selection = "single", rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 10))
  })
  
  observeEvent(input$t_elders_rows_selected, {
    r <- elders()[input$t_elders_rows_selected, ]
    updateTextInput(session, "e_name", value = r$Name)
    updateNumericInput(session, "e_age", value = as.numeric(r$Age))
    updateSelectInput(session, "e_gender", selected = r$Gender)
    updateTextInput(session, "e_phone", value = r$Phone)
    updateTextInput(session, "e_addr", value = r$Address)
    updateTextInput(session, "e_cond", value = r$Conditions)
    updateTextInput(session, "e_ec", value = r$Emergency_Contact)
    updateTextInput(session, "e_ep", value = r$Emergency_Phone)
    updateTextInput(session, "e_needs", value = r$Needs)
  })
  
  observeEvent(input$e_add, {
    if (!nzchar(trimws(input$e_name))) return(showNotification("Name is required.", type = "error"))
    df <- elders()
    row <- data.frame(ID = new_id(df, "E"), Name = trimws(input$e_name), Age = num(input$e_age),
                      Gender = input$e_gender, Phone = input$e_phone, Address = input$e_addr,
                      Conditions = input$e_cond, Emergency_Contact = input$e_ec,
                      Emergency_Phone = input$e_ep, Needs = input$e_needs, stringsAsFactors = FALSE)
    save_df(elders, "elders.csv", rbind(df, row))
    showNotification("Elder added.", type = "message")
  })
  
  observeEvent(input$e_upd, {
    i <- input$t_elders_rows_selected
    if (!need_row(i, "person")) return()
    df <- elders()
    df[i, -1] <- list(trimws(input$e_name), num(input$e_age), input$e_gender, input$e_phone,
                      input$e_addr, input$e_cond, input$e_ec, input$e_ep, input$e_needs)
    save_df(elders, "elders.csv", df)
    showNotification("Updated.", type = "message")
  })
  
  observeEvent(input$e_del, {
    i <- input$t_elders_rows_selected
    if (!need_row(i, "person")) return()
    save_df(elders, "elders.csv", elders()[-i, ])
    showNotification("Deleted.")
  })
  
  # ======================================================
  #  VOLUNTEERS
  # ======================================================
  output$t_vols <- renderDT({
    datatable(vols(), selection = "single", rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 10))
  })
  
  observeEvent(input$t_vols_rows_selected, {
    r <- vols()[input$t_vols_rows_selected, ]
    updateTextInput(session, "v_name", value = r$Name)
    updateTextInput(session, "v_college", value = r$College)
    updateTextInput(session, "v_phone", value = r$Phone)
    updateTextInput(session, "v_email", value = r$Email)
    updateTextInput(session, "v_avail", value = r$Availability)
    updateTextInput(session, "v_skills", value = r$Skills)
    updateSelectInput(session, "v_status", selected = r$Status)
  })
  
  observeEvent(input$v_add, {
    if (!nzchar(trimws(input$v_name))) return(showNotification("Name is required.", type = "error"))
    df <- vols()
    row <- data.frame(ID = new_id(df, "V"), Name = trimws(input$v_name), College = input$v_college,
                      Phone = input$v_phone, Email = input$v_email, Availability = input$v_avail,
                      Skills = input$v_skills, Status = input$v_status, stringsAsFactors = FALSE)
    save_df(vols, "volunteers.csv", rbind(df, row))
    showNotification("Volunteer added.", type = "message")
  })
  
  observeEvent(input$v_upd, {
    i <- input$t_vols_rows_selected
    if (!need_row(i, "volunteer")) return()
    df <- vols()
    df[i, -1] <- list(trimws(input$v_name), input$v_college, input$v_phone, input$v_email,
                      input$v_avail, input$v_skills, input$v_status)
    save_df(vols, "volunteers.csv", df)
    showNotification("Updated.", type = "message")
  })
  
  observeEvent(input$v_del, {
    i <- input$t_vols_rows_selected
    if (!need_row(i, "volunteer")) return()
    save_df(vols, "volunteers.csv", vols()[-i, ])
    showNotification("Deleted.")
  })
  
  output$vol_hours <- renderUI({
    v <- visits(); v <- v[v$Status == "Completed", ]
    if (nrow(v) == 0) return(NULL)
    tb <- table(name_of(vols(), v$Volunteer_ID))
    tagList(h4("Completed visits per volunteer"),
            tags$ul(lapply(names(tb), function(n) tags$li(paste0(n, ": ", tb[[n]])))))
  })
  
  # ======================================================
  #  VISITS
  # ======================================================
  output$t_visits <- renderDT({
    v <- visits()
    d <- data.frame(ID = v$ID, Date = v$Date, Time = v$Time,
                    Elder = name_of(elders(), v$Elder_ID),
                    Volunteer = name_of(vols(), v$Volunteer_ID),
                    Activity = v$Activity, Notes = v$Notes, Status = v$Status,
                    stringsAsFactors = FALSE)
    datatable(d, selection = "single", rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 10, order = list(list(1, "desc"))))
  })
  
  observeEvent(input$vi_add, {
    if (!nzchar(input$vi_elder) || !nzchar(input$vi_vol))
      return(showNotification("Choose an elder and a volunteer.", type = "error"))
    df <- visits()
    row <- data.frame(ID = new_id(df, "VS"), Elder_ID = input$vi_elder, Volunteer_ID = input$vi_vol,
                      Date = as.character(input$vi_date), Time = input$vi_time,
                      Activity = input$vi_act, Notes = input$vi_notes, Status = "Scheduled",
                      stringsAsFactors = FALSE)
    save_df(visits, "visits.csv", rbind(df, row))
    showNotification("Visit scheduled.", type = "message")
  })
  
  set_visit_status <- function(status) {
    i <- input$t_visits_rows_selected
    if (!need_row(i, "visit")) return()
    df <- visits(); df$Status[i] <- status
    save_df(visits, "visits.csv", df)
    showNotification(paste("Visit marked", tolower(status), "."), type = "message")
  }
  observeEvent(input$vi_done,   set_visit_status("Completed"))
  observeEvent(input$vi_cancel, set_visit_status("Cancelled"))
  observeEvent(input$vi_del, {
    i <- input$t_visits_rows_selected
    if (!need_row(i, "visit")) return()
    save_df(visits, "visits.csv", visits()[-i, ])
    showNotification("Visit deleted.")
  })
  
  # ======================================================
  #  HEALTH
  # ======================================================
  elder_health <- reactive({
    h <- health(); h <- h[h$Elder_ID == input$h_elder, ]
    h[order(h$DateTime, decreasing = TRUE), ]
  })
  
  output$h_summary <- renderUI({
    req(input$h_elder)
    e <- elders()[elders()$ID == input$h_elder, ]
    req(nrow(e) == 1)
    wellPanel(
      strong(e$Name), paste0(" (", e$Age, ", ", e$Gender, ")"), br(),
      "Conditions: ", e$Conditions, br(),
      "Emergency contact: ", e$Emergency_Contact, " - ", e$Emergency_Phone
    )
  })
  
  observeEvent(input$h_add, {
    req(input$h_elder)
    vals <- c(input$h_sys, input$h_dia, input$h_pulse, input$h_sugar, input$h_temp, input$h_spo2)
    if (all(is.na(vals))) return(showNotification("Enter at least one reading.", type = "error"))
    df <- health()
    row <- data.frame(ID = new_id(df, "H"), Elder_ID = input$h_elder,
                      DateTime = format(Sys.time(), "%Y-%m-%d %H:%M"),
                      Systolic = num(input$h_sys), Diastolic = num(input$h_dia),
                      Pulse = num(input$h_pulse), Sugar = num(input$h_sugar),
                      Temp_F = num(input$h_temp), SpO2 = num(input$h_spo2),
                      Notes = input$h_notes, stringsAsFactors = FALSE)
    save_df(health, "health.csv", rbind(df, row))
    a <- check_vitals(input$h_sys, input$h_dia, input$h_pulse, input$h_sugar, input$h_temp, input$h_spo2)
    if (length(a)) {
      showNotification(paste("Attention needed:", paste(a, collapse = "; "),
                             "- inform the family or doctor."), type = "error", duration = 15)
    } else showNotification("Reading saved. All values look normal.", type = "message")
  })
  
  output$t_health <- renderDT({
    d <- elder_health()[, c("DateTime", "Systolic", "Diastolic", "Pulse", "Sugar", "Temp_F", "SpO2", "Notes")]
    datatable(d, selection = "single", rownames = FALSE, options = list(scrollX = TRUE, pageLength = 8))
  })
  
  observeEvent(input$h_del, {
    i <- input$t_health_rows_selected
    if (!need_row(i, "reading")) return()
    id <- elder_health()$ID[i]
    save_df(health, "health.csv", health()[health()$ID != id, ])
    showNotification("Reading deleted.")
  })
  
  trend_plot <- function(cols, labels, title, ylab, colours) {
    h <- elder_health()
    req(nrow(h) > 0)
    h <- h[order(h$DateTime), ]
    x <- as.POSIXct(h$DateTime)
    ys <- lapply(cols, function(cn) as.numeric(h[[cn]]))
    rng <- range(unlist(ys), na.rm = TRUE)
    if (!all(is.finite(rng))) return(NULL)
    plot(x, ys[[1]], type = "b", pch = 16, col = colours[1], lwd = 2, ylim = rng,
         xlab = "", ylab = ylab, main = title)
    if (length(ys) > 1) for (k in 2:length(ys)) lines(x, ys[[k]], type = "b", pch = 17, col = colours[k], lwd = 2)
    legend("topright", legend = labels, col = colours, pch = c(16, 17)[seq_along(cols)], bty = "n")
    grid()
  }
  
  output$p_bp <- renderPlot(trend_plot(c("Systolic", "Diastolic"), c("Systolic", "Diastolic"),
                                       "Blood pressure", "mmHg", c("#d9534f", "#3c8dbc")))
  output$p_sugar <- renderPlot(trend_plot("Sugar", "Sugar", "Blood sugar", "mg/dL", "#f39c12"))
  
  # Medicines
  elder_meds <- reactive({ m <- meds(); m[m$Elder_ID == input$h_elder, ] })
  
  output$t_meds <- renderDT({
    d <- elder_meds()[, c("Medicine", "Dose", "Time", "Notes")]
    datatable(d, selection = "single", rownames = FALSE, options = list(dom = "tp", pageLength = 8))
  })
  
  observeEvent(input$m_add, {
    req(input$h_elder)
    if (!nzchar(trimws(input$m_name))) return(showNotification("Medicine name required.", type = "error"))
    df <- meds()
    row <- data.frame(ID = new_id(df, "M"), Elder_ID = input$h_elder, Medicine = trimws(input$m_name),
                      Dose = input$m_dose, Time = input$m_time, Notes = input$m_notes,
                      stringsAsFactors = FALSE)
    save_df(meds, "meds.csv", rbind(df, row))
    showNotification("Medicine added.", type = "message")
  })
  
  observeEvent(input$m_del, {
    i <- input$t_meds_rows_selected
    if (!need_row(i, "medicine")) return()
    id <- elder_meds()$ID[i]
    save_df(meds, "meds.csv", meds()[meds()$ID != id, ])
    showNotification("Medicine deleted.")
  })
}

shinyApp(ui, server)
