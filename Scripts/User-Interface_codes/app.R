library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyjs)
library(lubridate)
require(ggplot2)
require(ggpubr)
require(dplyr)
require(circlize)
require(tibble)
require(pheatmap)
require(GenomicRanges)
require(IRanges)
require(TxDb.Hsapiens.UCSC.hg38.knownGene)
require(TxDb.Hsapiens.UCSC.hg19.knownGene)
require(org.Hs.eg.db)
require(musicatk)
library(shinyFiles)
library(shinyWidgets)
library(shinyBS)
library(visNetwork)
library(lubridate)
library(shinybusy)
library(shinyBS)
library(bsicons)
# runApp("/home/suvenduk/OncoGenAI/Final_oncogen/")
library(vcfR)
library(RColorBrewer)
library(VariantAnnotation)
library(shiny.gosling)
library(BioCircos)
library(tidyr)
library(data.table)

#require(StructuralVariantAnnotation)
require(ggbio)
require(shiny.gosling)
library(data.table)
library(stringr)
library(igvShiny)
library(GenomicAlignments)
library(htmlwidgets)
library(VariantAnnotation)
library(biomaRt)
library(fresh)
library(rintrojs)
# library(tidyverse)

# Set a debugging variable
# debug_env <- new.env()
# debug_env$debug = FALSE
# Oncogen202409050001
generate_id <- function(id_number) {
  # Get current date in YYYYMMDD format
  date_str <- format(Sys.Date(), "%Y%m%d")
  # Create the ID with prefix, date, and the provided id number
  id <- paste0("Oncogen", date_str, sprintf("%04d", id_number))
  return(id)
}

# Function to read the CSV file
read_jobs_csv <- function() {
  data = read.csv("OncoGenAI/running_jobs.csv", stringsAsFactors = FALSE)
  data <- data %>%
          mutate(Progress = case_when(
            Progress == 1 ~ "15%",
            Progress == 2 ~ "30%",
            Progress == 3 ~ "45%",
            Progress == 4 ~ "60%",
            Progress == 5 ~ "70%",
            Progress == 6 ~ "85%",
            Progress == 7 ~ "100%",
            TRUE ~ as.character(Progress)  # Default case if other values are present
          ))
          return(data)
}

read_jobs_csv2 <- function() {
  data = read.csv("OncoGenAI/running_jobs.csv", stringsAsFactors = FALSE)
  return(data)
}

# Function to update the CSV file
update_jobs_csv <- function(data) {
  write.csv(data, "OncoGenAI/running_jobs.csv", row.names = FALSE)
}
js <- "
var mytips = ['HyperCap Heredity_capture_targets.hg38.bed',
'HyperCap Heredity_primary_targets.hg38.bed',
'HyperCap Heredity_capture_targets.hg19.bed',
'HyperCap Heredity_primary_targets.hg19.bed',
'Devyser BRCA target file, likely related to BRCA gene capture or sequencing'];
$('#bed_files').on('shown.bs.select', function() {
  var $lis = $($(this).data('selectpicker').selectpicker.current.elements);
  $lis.each(function(i) {
    $(this).attr('title', mytips[i]);
  });
});"



my_theme <- create_theme(
  adminlte_color(
    light_blue = "#38aea8"  # Apply your desired color to the theme
  ),
  adminlte_sidebar(
    dark_bg = "#222d32",      # Sidebar background color (default color)
    dark_hover_bg = "#1e282c" # Sidebar hover background color (default color)
  ),
  adminlte_global(
    content_bg = "#ffffff"    # Background color for the content area
  )   
) 



# Define UI
library(shiny)
ui <- fluidPage(
  introjsUI(),
  dashboardPage(skin = "blue",
                    dashboardHeader(title = "OncoGen.ai",
                    tags$li(
      class = "dropdown",
      style = "margin-right: 10px; display: flex; align-items: center; justify-content: flex-end; height: 100%;",  # Flexbox for vertical centering and right-align
      actionButton(inputId = "tour_button", label = "Take a Tour", class = "custom-btn"),
      # actionButton(inputId = "settings", label= "", icon = icon("gear"), class = "custom-btn"), # New button
      actionButton(inputId = "logout", label= "", icon = icon("sign-out-alt"), class = "custom-btn") # New button
    )),
                    dashboardSidebar(collapsed = T,
                                     sidebarMenu(id= "tabs2",
                                                 menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                                 menuItem("FASTQ Quality Report", tabName = "fastqqualityreport", icon = icon("file-medical")),
                                                 menuItem("New Job", tabName = "newjob", icon = icon("plus-square")),
                                                 menuItem("Running Jobs", tabName = "runningjobs", icon = icon("spinner")),
                                                 menuItem("Variant Filteration", tabName = "filterationreport", icon = icon("file-medical")),
                                                #  menuItem("Basic", tabName = "rvcfshiny", icon = icon("star"),
                                                #           menuSubItem("Data", icon = icon("database"), tabName = "datainput"),
                                                #           #menuSubItem("Venn Diagram", icon = icon("chart-bar"), tabName = "venn_analyse"),
                                                #           menuSubItem("Variants Number", icon = icon("chart-bar"), tabName = "ALL_variants"),
                                                #           menuSubItem("SNVs Exploration", icon = icon("chart-bar"), tabName = "snp_analysis"),
                                                #           menuSubItem("Indels Exploration", icon = icon("chart-bar"), tabName = "Indel_analysis"),
                                                #           # menuSubItem("Genome Circos Plot", icon = icon("chart-bar"), tabName = "circle"),
                                                #           menuSubItem("Genomic Features",icon = icon("chart-bar"), tabName = "distribution"),
                                                #           menuSubItem("Variants Relevant Genes", icon = icon("chart-bar"), tabName = "Variants_summarise")
                                                #           # menuSubItem("Cancer Driver Genes", icon = icon("chart-bar"), tabName = "Variants_heatmap"),
                                                #           #menuSubItem("Cancer Mutational Signature", icon = icon("chart-bar"), tabName = "cancer_signature"),
                                                #           #menuSubItem("Data Annotation", icon = icon("book"), tabName = "dataAnno")
                                                #  ),
                                                 menuItem("Basic Analysis", tabName = "rvcfshiny2", icon = icon("star"),
                                                          menuSubItem("Data", icon = icon("database"), tabName = "datainput2"),
                                                          menuSubItem("Variants Number", icon = icon("chart-bar"), tabName = "ALL_variants2"),
                                                          menuSubItem("SNVs Exploration", icon = icon("chart-bar"), tabName = "snp_analysis2"),
                                                          menuSubItem("Indels Exploration", icon = icon("chart-bar"), tabName = "Indel_analysis2"),
                                                          menuSubItem("Genomic Features",icon = icon("chart-bar"), tabName = "distribution2"),
                                                          menuSubItem("Variants Relevant Genes", icon = icon("chart-bar"), tabName = "Variants_summarise2")
                                                 ),
                                                 
                                                 menuItem("Clinical Analysis", tabName = "results", icon = icon("chart-line"),
                                                          menuSubItem("VCF Summary", tabName = "result1"),
                                                          menuSubItem("Variant interpretation", tabName = "result2"),
                                                          menuSubItem("Tabular View", tabName = "result3"),
                                                          menuSubItem("CNV Analysis", tabName = "result4")
                                                 ),
                                                 menuItem("Genome Browser", tabName = "genome_browser", icon = icon("globe")),
                                                 menuItem("KG Imputation", tabName = "kgimputation", icon = icon("chart-bar")),
                                                 menuItem("Report Generation", tabName = "reportgeneration", icon = icon("file-alt")),
                                                 menuItem("TMB Score", tabName = "tmbscore", icon = icon("meteor")),
                                                 menuItem("Customized Workflow", tabName = "workflow", icon = icon("cogs")),
                                                 menuItem("Tutorials", tabName = "tutorials", icon = icon("book")),
                                                 menuItem("Resources", tabName = "resources", icon = icon("folder")),
                                                 menuItem("Bug Submission", tabName = "bugsubmission", icon = icon("bug"))
                                                #  tags$div(
                                                #    style = "position: absolute; bottom: 0; width: 100%; padding: 10px; text-align: left;",
                                                #    tags$button(
                                                #      id = "logout",
                                                #      class = "btn",
                                                #      icon("sign-out-alt"),
                                                #      tags$span("Logout", style = "margin-left: 5px;"),
                                                #      style = "background-color: transparent; border: none; cursor: pointer; display: inline-flex; align-items: center;"
                                                #    )
                                                #  )
                                     )
                    ),
                    dashboardBody(uiOutput("page"), use_theme(my_theme), 
                                  
                                  useShinyjs(),
                                  bsTooltip(id = "submissionsTooltip", title = "This shows the number of submissions by month.", placement = "top", trigger = "hover"),
                                      div(id = "contextMenu", 
        style = "display: none; position: absolute; top: 160px; left: 1120px; background-color: #f9f9f9; border: 1px solid #ccc; padding: 10px; z-index: 1000;",
        
        # Create a flex container for the dropdown and button
        div(style = "display: flex; align-items:center;", 
            selectInput("dropdown", "Select Filter:", 
                        choices = c("", NULL),  # Choices will be updated dynamically
                        selected = NULL,
                        width = "200px"
            ),
            
            # Move the button to be adjacent to the dropdown
            actionButton("createFilter", "Create New Filter", class = "btn-filter", style = "margin-left: 10px;")
        ),
        
        # Placeholder for dynamic buttons
        div(id = "modal_buttons", style = "margin-top: 10px;",
            uiOutput("modal_buttons")
        ),
        
        div(style = "margin-top: 10px;",
            actionButton("closeContextMenu", "Close")
        )
    ),
                                  tags$head(tags$style(HTML("
    
      .typewriter h1 {
        display: inline-block;
        overflow: hidden;
        border-right: .15em solid black;
        white-space: nowrap;
        margin: 0 auto;
        letter-spacing: .15em;
        font-style: italic;
        text-align: center;
        animation: typing 3.5s steps(30, end), blink-caret .75s step-end infinite;
      }
      .typewriter {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 5vh;
        font-size: 2em;
      }
      @keyframes typing {
        from { width: 0 }
        to { width: 100% }
      }
      @keyframes blink-caret {
        from, to { border-color: transparent }
        50% { border-color: black }
      }
      .login-page {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        height: 100vh;
        /*background: linear-gradient(to right, #1e90ff, #1c1c1c);*/
        color: white;
        position: absolute;
        width: 100%;
        top: 0;
        bottom: 0;
        left: 0;
        right: 0;
        z-index: 2000;
        background:linear-gradient(45deg, #2b8782, #79d1be, #217a5a, #8dd9e0);
        background-size: 600% 600%;
        animation: gradientAnimation 10s ease infinite;
      }
      .front-page {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        height: 100vh;
        /*background: linear-gradient(to right, #1e90ff, #1c1c1c);*/
        color: white;
        position: absolute;
        width: 100%;
        top: 0;
        bottom: 0;
        left: 0;
        right: 0;
        z-index: 2000;
        background:linear-gradient(45deg, #e4ebea, #ced6d6, #217a5a, #8dd9e0);
        background-size: 600% 600%;
        animation: gradientAnimation 10s ease infinite;
      }
      @keyframes gradientAnimation {
          0% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
          100% { background-position: 0% 50%; }
        }
      .login-box {
        background: white;
        position: absolute;
        right: 150px;;
        color: black;
        padding: 40px;
        border-radius: 10px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
        width: 300px;
        text-align: center;
      }
      .login-box h3 {
        margin-bottom: 20px;
      }
      /*.login-page .dynamic-background {
          background:linear-gradient(45deg, #38aea8, #4ec2a9, #278f69, #80cad1);
          background-size: 600% 600%;
          animation: gradientAnimation 10s ease infinite;
        }*/
        
        .container-fluid {
          padding-right: 0px;
          padding-left: 0px;
        }
      .logo-left {
        position: absolute;
        top: 10px;
        left: 10px;
        width: 120px;
      }
      .logo-right {
        position: absolute;
        top: 10px;
        right: 10px;
        width: 120px;
      }
      .logo-center {
        position: absolute;
        top: 100px;
        left: 500px;
        transform: translateX(-50%);
        width: 700px;
      }    
      /* Align header items correctly */
        .main-header .navbar {
          display: flex;
          align-items: center;
          justify-content: space-between;
          padding-right: 15px;
        }
        /* Right align custom button */
        .navbar-custom-menu {
          margin-left: auto;
        }
        /* Custom styles for the button */
        .custom-btn {
          font-weight: bold;        /* Make text bold */
          color: white !important;  /* Make text color white */
          background-color: #38aea8; /* Optional: Update background color */
          border: none;             /* Remove button border */
          box-shadow: none;         /* Remove any shadow effect */
          font-size: 16px;         /* Increase the text size */
          margin-left: 10px;        /* Space between buttons */
        }
        .custom-btn:hover {
          background-color: #329691;  /* Change to green on hover */
        }
      .buffering-icon {
        display: inline-block;
        width: 16px;
        height: 16px;
        border: 2px solid #007bff;
        border-radius: 50%;
        border-top: 2px solid transparent;
        animation: spin 1s linear infinite;
        margin-right: 5px;
      }
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      .btn {
        display: inline-flex;
        align-items: center;
        text-align: center;
      }
      .btn span {
        vertical-align: middle;
        font-size: 14px;
      }
      .btn-primary {
        background-color: #007bff;
        border-color: #007bff;
        color: #fff;
      }
      .required-label:after {
        content: '*';
        color: red;
        margin-left: 5px;
      }
      .btn {
        display: inline-flex;
        align-items: center;
        text-align: center;
      }
      .btn span {
        vertical-align: middle;
        font-size: 14px;
      }
      .btn-primary {
        background-color: #007bff;
        border-color: #007bff;
        color: #fff;
      }
      .btn-success {
        background-color: #28a745;
        border-color: #28a745;
        color: #fff;
      }
      .dt-center { text-align: center; }
      .tooltip-icon {
      color: #007bff;
      cursor: pointer;
      font-size: 10px;
      vertical-align: super;
      margin-right: 5px; /* Adds space between ? and label */
      }
      .label-with-tooltip {
      display: inline-flex;
      
      align-items: center;
      font-weight: bold; /* Makes the label bold */
      }
      .custom-tooltip {
        position: relative;
        display: inline-block;
      }
      .custom-tooltip .tooltiptext {
        visibility: hidden;
        width: 120px;
        background-color: black;
        color: #fff;
        text-align: center;
        border-radius: 6px;
        padding: 5px 0;
        position: absolute;
        z-index: 1;
        bottom: 125%; /* Position the tooltip above the text */
        left: 50%;
        margin-left: -60px;
      }
      .custom-tooltip:hover .tooltiptext {
        visibility: visible;
      }
      .btn-filter {
        background-color: #a72828;
        border-color: #252626;
        color: #fff;
      }
      .btn-add {
        background-color: #066619;
        border-color: #252626;
        color: #fff;
      }
      .btn-remove {
        background-color: #a72828;
        border-color: #252626;
        color: #fff;
      }
      .filter-row {
        display: flex;
        justify-content: space-between;  /* Space between select and button */
        align-items: center;
        margin-bottom: 10px;
      }
      .filter-row .selectize-control {
        margin-right: 10px;  /* Space between select input and button */
      }
      .filter-row .btn-filter {
        white-space: nowrap;  /* Prevent button text from wrapping */
        margin-left: auto;  /* Push the button to the far right */
      }
      .close-btn {
        position: absolute;
        top: 5px;
        right: 10px;
        color: #f5f5f5;
        background: none;
        border: none;
        font-size: 18px;
        cursor: pointer;
        padding: 0.1px 2px;  /* Adjust padding: 5px top/bottom, 8px left/right */
      }
      .modal-body .selectize-input {
        width: 100%;  /* Adjust select input width to full width of container */
      }
      .filter-options {
        display: flex;
        justify-content: space-between;
        gap: 0px; /* Space between boxes */
        width: 100%;
        height: 100%
        padding: 0px;
        margin: 0px;
      }
      .filter-options-left {
        width: 85%;
        margin-left: -80px;
        margin-top: -30px;
      }
      .filter-options-right {
        width: 85%;
        margin-right: -80px;
        margin-top: -30px;
      }
      .filter-row label {
        font-weight: bold;
        margin-right: 10px;
      }
      .modal-dialog {
        max-width: 90%;  /* Adjust the width as needed */
        width: 90%;
        padding: 0px;
      }
      .modal-content {
        height: 95vh;  /* Adjust the height as needed */
        overflow-y: auto;
        padding: 1px;
      }

            .btn-filter {
        background-color: #38c28d;
        border-color: #252626;
        color: #fff;
      }
      .context-menu {
        position: absolute;
        background-color: #f7f7f7;
        border: 1px solid #ccc;
        display: none;
        width: 300px;
        box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
        z-index: 100;
      }

      .context-menu ul {
        list-style: none;
        margin: 0;
        padding: 0;
      }

      .context-menu li {
        padding: 10px;
        cursor: pointer;
      }

      .context-menu li:hover {
        background-color: #ddd;
      }
      .btn-delete {
        background-color: #FFFFFF;
        color: #FF0000;
        border: none;
      }
      .btn-edit {
        background-color: #FFFFFF;
        color: #0000FF;
        border: none;
      }
      .btn-action {
        background-color: #007bff;
        color: #fff;
        border: none;
      }
      .btn {
        display: inline-flex;
        align-items: center;
        text-align: center;
      }
      .btn span {
        vertical-align: middle;
        font-size: 14px;
      }
      .dt-center { text-align: center; }
      .dataTables_wrapper {
        overflow-y: auto;
        overflow-x: auto;
      }
      .btn-add {
        background-color: #066619;
        border-color: #252626;
        color: #fff;
      }
      .btn-remove {
        background-color: #a72828;
        border-color: #252626;
        color: #fff;
      }
      .filter-row {
        display: flex;
        justify-content: space-between;  /* Space between select and button */
        align-items: center;
        margin-bottom: 10px;
      }
      .filter-row .selectize-control {
        margin-right: 10px;  /* Space between select input and button */
      }
      .filter-row .btn-filter {
        white-space: nowrap;  /* Prevent button text from wrapping */
        margin-left: auto;  /* Push the button to the far right */
      }
      .close-btn {
        position: absolute;
        top: 5px;
        right: 10px;
        color: #f5f5f5;
        background: none;
        border: none;
        font-size: 18px;
        cursor: pointer;
        padding: 0.1px 2px;  /* Adjust padding: 5px top/bottom, 8px left/right */
      }
      .modal-body .selectize-input {
        width: 100%;  /* Adjust select input width to full width of container */
      }
      .filter-options {
        display: flex;
        justify-content: space-between;
        gap: 0px; /* Space between boxes */
        width: 100%;
        height: 100%
        padding: 0px;
        margin: 0px;
      }
      .filter-options-left {
        width: 85%;
        margin-left: -80px;
        margin-top: -30px;
      }
      .filter-options-right {
        width: 85%;
        margin-right: -80px;
        margin-top: -30px;
      }
      .filter-row label {
        font-weight: bold;
        margin-right: 10px;
      }
      .modal-dialog {
        max-width: 90%;  /* Adjust the width as needed */
        width: 90%;
        padding: 0px;
      }
      .modal-content {
        height: 95vh;  /* Adjust the height as needed */
        overflow-y: auto;
        padding: 1px;
      }
      /* Custom styles for the surrounding box */
      .box-primary {
        background-color: #ffffff; /* Darker background for the box */
        border-radius: 10px; /* Rounded corners for the box */
        box-shadow: 2px 8px 16px rgba(0, 0, 0, 0.2); /* Shadow effect for the box */
        padding: 5px;
      }
      .info-icon {
        position: absolute;
        right: 10px;
        top: 50%;
        transform: translateY(-50%);
    }
      .top-label{
        font-weight: bold;
        font-size: 20px; /* Increase the font size */
        font-family: 'Bodoni Moda', serif;
        color: #2d7d79;
      }
      table.dataTable td, table.dataTable th {
          text-align: center; /* Align all table data and headers to center */
          vertical-align: middle; /* Vertically center the content */
        }
      .box {
          margin-bottom: 25px !important; /* Uniformly reduce the bottom margin for all boxes */
        }
        .col-sm-4, .col-sm-6, .col-sm-9, .col-sm-12 {
          padding-left: 3px !important;  /* Reduce the padding between columns */
          padding-right: 3px !important; /* Horizontal spacing between boxes */
        }
        .row {
          margin-left: 0px !important;   /* Remove row's left margin */
          margin-right: 0px !important;  /* Remove row's right margin */
        }
        .jobs-text {
        font-size: 1em;
        color: #000;
        font-weight: normal;
        margin: 0;         /* Remove margin between lines */
        padding: 0;        /* Remove padding between lines */
        line-height: 1.2;  /* Adjust line spacing */
      }
      .big-number {
        font-size: 2em;
        font-weight: bold;
        margin: 0;         /* Remove margin */
        padding: 0;        /* Remove padding */
        line-height: 1.2;  /* Adjust line spacing */
      }
      .icon-text-container .text {
        margin-left: 20px; /* Adds some space between the icon and text */
      }
    "))),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                # column(width = 4,
                #        box(
                #          status = "primary", solidHeader = F, width = 12, id = "dashboard_box1",
                #          div(
                #            class = "icon-text-container",  # Flex container to align icon and text horizontally
                #            style = "display: flex; align-items: center;",  # Flexbox settings
                #            span(style = "color: #38aea8;", icon("running", class = "fa-8x", style = "color: #38aea8;")),  # Running man icon
                #            div(
                #              class = "text",  # Text block that will appear next to the icon
                #              h4("A total of ", class = "jobs-text"),  # Regular text
                #              span("3222 jobs", class = "big-number"),  # Larger text for the number
                #              h4("have been successfully run on this app.", class = "jobs-text")  # Regular text
                #            )
                #          )
                #        )
                # ),
                # column(width = 4,
                #        box(
                #          status = "primary", solidHeader = F, width = 12, id = "dashboard_box1",
                #          div(
                #            class = "icon-text-container",  # Flex container to align icon and text horizontally
                #            style = "display: flex; align-items: center;",  # Flexbox settings
                #            span(style = "color: #38aea8;", icon("chart-line", class = "fa-8x", style = "color: #38aea8;")),  # Running man icon
                #            div(
                #              class = "text",  # Text block that will appear next to the icon
                #              h4("A total of ", class = "jobs-text"),  # Regular text
                #              span("3222 jobs", class = "big-number"),  # Larger text for the number
                #              h4("have been successfully run on this app.", class = "jobs-text")  # Regular text
                #            )
                #          )
                #        )
                # ),
                # column(width = 4,
                #        box(
                #          status = "primary", solidHeader = F, width = 12, id = "dashboard_box3",
                #          div(
                #            class = "icon-text-container",  # Flex container to align icon and text horizontally
                #            span(style = "color: #38aea8;", bs_icon("hourglass-split", size = "8em")),  # Icon
                #            div(
                #              class = "text",  # Text block that will appear next to the icon
                #              h4("A total of ", class = "jobs-text"),  # Regular text
                #              span("3222 jobs", class = "big-number"),  # Larger text for the number
                #              h4("have been successfully run on this app.", class = "jobs-text")  # Regular text
                #            )
                #          )
                #        )
                # ),
                column(width = 6,
                       box(
                         title = div(
                           tags$div("Submission Frequency", class = "top-label"),
                           tipify(tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           ),title = "Submission Frequency",placement="right", trigger ="hover")
                         ), status = "primary", solidHeader = F, width = 12, id = "dashboard_box4",
                         
                         fluidRow(
                           column(width = 1,actionButton("prevYear", "<",style = "padding: 6px; font-size: 12px;")),
                           column(width = 10,plotlyOutput("submissionsPlot", height = 300)),
                           column(width = 1,actionButton("nextYear", ">",style = "padding: 6px; font-size: 12px;"))
                         )
                       )
                ),
                
                column(width = 6,
                       box(
                         title = div(
                           tags$div("Workflow Distribution", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "dashboard_box5",
                         plotlyOutput("workflowPieChart", height = 300)
                       )
                ),
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Latest Submissions", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "dashboard_box6",
                         DT::dataTableOutput("submissionTable")
                       )
                )
              )
      ),

      # tabItem(tabName = "quality_check",
      #         fluidRow(
      #           column(width = 12,
      #                  box(
      #                    title = "Fastq Quality Check", status = "primary", width = 9,
      #                    tags$strong("Upload Your FASTQ Files", class = "required-label"), br(),
      #                    shinyFilesButton('fastq_file_check', "Browse...", title='Please select a file', multiple=TRUE), 
      #                    uiOutput("progressBar"), br()
      #                  )
      #           )
      #         )),
      tabItem(tabName = "fastqqualityreport",
              fluidRow(
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Fastq Quality Check", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", width = "100%",
                         tags$strong("Upload Your FASTQ File"), br(),
                         shinyFilesButton('fastq_file_check', "Browse...", title = 'Please select a FASTQ file', multiple = FALSE),
                         uiOutput("progressBar1"), br(),
                         actionButton("submit_fastq_check", "Submit", icon = icon("paper-plane"), class = "btn btn-primary"),
                         br(),br(), textOutput("last_line"), br(),
                         uiOutput("previous_reports_dropdown"),   actionButton("fastqc_refresh", label = NULL, icon = icon("redo")),  # Dropdown for previous reports
                         uiOutput("fastqc_report_ui")  # UI for displaying the FastQC report
                       )
                )
              )
      ),
      tabItem(tabName = "newjob",
              fluidRow(
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Submission Details", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "newjob_box1",
                         textInput("patient_id", label = tags$span("Patient ID", class = "required-label"), placeholder = "Enter the Patient ID"),
                         textInput("patient_details", label = "Patient Details", placeholder = "Enter the Patient details"),
                         textInput("submitting_author", label = tags$span("Submitting Author", class = "required-label"), placeholder = "Enter the Submitting author"),
                         selectInput("cancer_type", 
                                     label = tags$span("Cancer Type", class = "required-label"), 
                                     choices = c("", 
                                                 "Breast Carcinoma", 
                                                 "Lung Carcinoma", 
                                                 "Prostate Carcinoma", 
                                                 "Colon Carcinoma", 
                                                 "Leukemia", 
                                                 "Pancreatic Carcinoma", 
                                                 "Liver Carcinoma", 
                                                 "Ovarian Carcinoma", 
                                                 "Bladder Carcinoma", 
                                                 "Kidney Carcinoma", 
                                                 "Melanoma", 
                                                 "Esophageal Carcinoma", 
                                                 "Gastric (Stomach) Carcinoma", 
                                                 "Thyroid Carcinoma", 
                                                 "Brain Cancer", 
                                                 "Cervical Carcinoma", 
                                                 "Hodgkin Lymphoma", 
                                                 "Non-Hodgkin Lymphoma", 
                                                 "Multiple Myeloma")),
                         textInput("tumor_percent", label = tags$span("Tumor Percentage", class = "required-label"), placeholder = "Put the value in numbers")
                         
                       )
                ),
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Parameters", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "newjob_box2",
                         pickerInput("workflow", label = tags$span("Workflow Selection", class = "required-label"), choices = c("Illumina", "IonTorrent"), selected = NULL,
                                     multiple = TRUE,
                                     options = pickerOptions(maxOptions = 1, title = "Select an option")),
                         pickerInput("cnv_analysis", label = tags$span("CNV analysis", class = "required-label"), choices = c("Yes", "No"), selected = NULL,
                                     multiple = TRUE,
                                     options = pickerOptions(maxOptions = 1, title = "Select an option")),
                         pickerInput("adaptor_trimming", label = tags$span("Adaptor Trimming", class = "required-label"), choices = c("Yes", "No"), selected = NULL,
                                     multiple = TRUE,
                                     options = pickerOptions(maxOptions = 1, title = "Select an option")),
                         pickerInput("genome", label = tags$span("Genome", class = "required-label"), choices = c("GRCh38 (hg38)", "GRCh37 (hg19)"), selected = NULL,
                                     multiple = TRUE,
                                     options = pickerOptions(maxOptions = 1, title = "Select an option")),
                         pickerInput("mutations", label = tags$span("Mutations", class = "required-label"), choices = c("Somatic", "Germline"), selected = NULL,
                                     multiple = TRUE,
                                     options = pickerOptions(maxOptions = 1, title = "Select an option")),
                         #selectInput("bed_file", label = tags$span("Bed File", class = "required-label"), choices = c("", "-N/A-","KAPA HyperCap Heredity_primary_targets.hg38.bed","KAPA HyperCap Heredity_capture_targets.hg38.bed","KAPA HyperCap Heredity_capture_targets.hg19.bed","KAPA HyperCap Heredity_primary_targets.hg19.bed"))
                         pickerInput(
                           inputId = "bed_files",
                           label = tags$span("Bed File", class = "required-label"),
                           choices = c(                      
                              "AMPLISEQ_CLL_IAD211051_182_Designed.bed",
                              "OCAPlus.20210422.hotspots.bed",
                              "Oncomine_Lung_cfDNA.07112017.Hotspots.bed",
                              "AMPLISEQ_ICP_IAD192604_1000_Designed.bed",
                              "OCAPlus.20210824.designed.bed",
                              "Oncomine_Lung_cfNA.08212017.Designed.bed",
                              "AMPLISEQ_LUNG_CUSTOM_PANEL.bed",
                              "OCAv3.20170621.hotspots.bed",
                              "Oncomine_Lung_cfNA.08212017.Hotspots.bed",
                              "BRCA1_2.20131001.hotspots.bed",
                              "OCAv3.20180426.designed.bed",
                              "Oncomine_Myeloid.20170817.designed.bed",
                              "capture_targets_hg19.bed",
                              "OCAv3_designs_072117_Reference",
                              "Oncomine_Myeloid.20181009.hotspots.bed",
                              "capture_targets_hg38.bed",
                              "OCAv3_designs_072117_Reference.bed",
                              "Oncomine_Myeloid_designs_082217_Reference",
                              "CHP2.20131001.designed.bed",
                              "OFAssay_Fusion_Reference",
                              "Oncomine_Myeloid_designs_082217_Reference.bed",
                              "CHP2.20131001.hotspots.bed",
                              "Oncomine_BRCA_Research_Assay.20170303.designed.bed",
                              "Oncomine_PANCAN_cfNA_v5.Designed.bed",
                              "Devyser_BRCA.target.bed",
                              "Oncomine_BRCA_Research_Assay.20170316.hotspots.blist.318.bed",
                              "Oncomine_PANCAN_cfNA_v5.Hotspots.bed",
                              "FusionPlex_ALL_AK0074.bed",
                              "Oncomine_Breast_cfDNA.03062017.Designed.bed",
                              "Oncomine_Solid_Tumour_DNA.bed",
                              "Oncomine_Breast_cfDNA.03062017.Hotspots.bed",
                              "Oncomine_Solid_Tumour_Fusion.bed",
                              "Oncomine_Colon_cfDNA.03062017.Designed.bed",
                              "Oncomine_TML.20170222.Designed.bed",
                              "IAD161632_167_Designed.bed",
                              "Oncomine_Colon_cfDNA.03062017.Hotspots.bed",
                              "OST_Fusion_REFERENCE_MAN0013544_RevA",
                              "IAD192604_1000_Designed.bed",
                              "Oncomine_Focus.20160219.designed.bed",
                              "OST_Fusion_REFERENCE_MAN0013544_RevA_16_May_23",
                              "IAD204097_167_Designed.bed",
                              "Oncomine_Focus.20160219.hotspots.bed",
                              "primary_targets_hg19.bed",
                              "IAD211051_182_Designed.bed",
                              "Oncomine_Fusion_ST_designs_032916_Reference.bed",
                              "primary_targets_hg38.bed",
                              "Ion_AmpliSeq_SARS-CoV-2-Insight.20210329.designed.bed",
                              "Oncomine_Kidney.20200115_Designed.bed",
                              "QIAHEN_HRR_DHS-110Z.primers-250bp.bed",
                              "LYNCH_IAD161632_167_Designed.bed",
                              "Oncomine_Kidney.20200115_hotspot.bed",
                              "OCAPlus.20191203.designed.bed",
                              "Oncomine_Lung_cfDNA.07112017.Designed.bed",
                              "NA"),
                           selected = NULL,
                           multiple = TRUE,
                           options = pickerOptions(maxOptions = 1, title = "Select an option")
                         ),tags$script(HTML(js))
                       )
                ),
                # column(width = 12,
                #        box(
                #          title = "Parameters", status = "primary", solidHeader = F, width = 12, 
                #          selectInput("workflow", label = tags$span("Workflow Selection", class = "required-label"), choices = c("", "Illumina", "Ion Torrent")),
                #          selectInput("adaptor_trimming", label = tags$span("Adaptor Trimming", class = "required-label"), choices = c("", "Yes", "No")),
                #          selectInput("genome", label = tags$span("Genome", class = "required-label"),choices = c("","GRCh38 (hg38)" , "GRCh37 (hg19)" )),
                #          selectInput("mutations", label = tags$span("Mutations", class = "required-label"), choices = c("", "Somatic", "Germline")),
                #          selectInput("bed_file", label = tags$span("Bed File", class = "required-label"), choices = list(
                #            "",
                #            "primary_targets_hg38.bed",
                #            "capture_targets_hg38.bed",
                #            "capture_targets_hg19.bed",
                #            "primary_targets_hg19.bed",
                #            "Devyser_BRCA.target.bed",
                #            "NA"
                #          ),selectize = T,
                #          tags$script(HTML("
                #               $(document).ready(function() {
                #                 $('#bed_file option[value='primary_targets_hg38.bed']').attr('title', 'primary_targets_hg38.bed');
                #                 $('#bed_file option[value='capture_targets_hg38.bed']').attr('title', 'capture_targets_hg38.bed ');
                #                 $('#bed_file option[value='capture_targets_hg19.bed']').attr('title', 'capture_targets_hg19.bed');
                #               });
                #             ")))
                #        )
                # ),
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Upload Files", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         tags$strong("Upload FASTQ Files/Bam file", class = "required-label"), br(),
                         # fileInput("fastq_files", "Upload FASTQ Files", multiple = TRUE, accept = c(".fastq", ".fq")),
                         shinyFilesButton('fastq_file', "Browse...", title='Please select a file', multiple=TRUE),uiOutput("progressBar"),br(),
                         tableOutput("files"),
                         actionButton("submit_job", "Submit", icon = icon("paper-plane"), class = "btn btn-primary"),br(), br(),
                         
                         uiOutput("confirmation_ui"),
                         DTOutput("submitted_info"),
                         checkboxInput("test_data", "Try our test data", value = FALSE)
                         # Add the checkbox here
                       )
                )
              )
      ),
      tabItem(tabName = "filterationreport",
              fluidRow(
                column(width = 4,
                       box(
                         title = "Select Job ID", status = "primary", solidHeader = F, width = 12, id = "filterationreport_box1",
                         selectInput("select_job_id_filter", "Select Job ID", choices = c("", NULL)),
                         textOutput("display_patient_id_filter")
                         #textOutput("display_vcf_file_path_filter"),
                       ),
                       box(
                         title = div(
                           tags$div("Filters", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         radioButtons("filter_type", "Choose a filtering mode:", 
                                      choices = c("3 mode filtering" = "3_mode", "Manual filtering" = "manual"),
                                      selected = "3_mode"
                         ),
                         br(),  # Add a line break for spacing
                         conditionalPanel(
                           condition = "input.filter_type == '3_mode'",
                           selectInput("mode_level", "Select filtering level:",
                                       choices = c("Low", "Medium", "High"),
                                       selected = "Medium"
                           )
                         ),
                         conditionalPanel(
                           condition = "input.filter_type == 'manual'",
                           fluidRow(
                             column(6,
                                    h4("SNP"),
                                    fluidRow(
                                      column(6,
                                             tags$div(class = "label-with-tooltip",
                                                      tags$span("?", class = "tooltip-icon", title = "QD means Quality Depth"),
                                                      tags$span("QD:")
                                             ),
                                             selectInput("snp_qd_operator", NULL,
                                                         choices = c("-N/A-", ">" = ">", "<" = "<", ">=" = ">=", "<=" = "<=", "=" = "="),
                                                         selected = "<"
                                             )
                                      ),
                                      column(6,
                                             numericInput("snp_qd_value", "Value:", value = NA, min = 0, step = 0.1)
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                             tags$div(class = "label-with-tooltip",
                                                      tags$span("?", class = "tooltip-icon", title = "QD means Quality Depth"),
                                                      tags$span("SQR:")
                                             ),
                                             selectInput("snp_sqr_operator", NULL,
                                                         choices = c("-N/A-", ">" = ">", "<" = "<", ">=" = ">=", "<=" = "<=", "=" = "="),
                                                         selected = ">"
                                             )
                                      ),
                                      column(6,
                                             numericInput("snp_sqr_value", "Value:", value = NA, min = 0, step = 0.1)
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                             tags$div(class = "label-with-tooltip",
                                                      tags$span("?", class = "tooltip-icon", title = "QD means Quality Depth"),
                                                      tags$span("ReadPosRank:")
                                             ),
                                             selectInput("snp_pos_operator", NULL,
                                                         choices = c("-N/A-", ">" = ">", "<" = "<", ">=" = ">=", "<=" = "<=", "=" = "="),
                                                         selected = "<"
                                             )
                                      ),
                                      column(6,
                                             numericInput("snp_pos_value", "Value:", value = NA, min = 0, step = 0.1)
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                             tags$div(class = "label-with-tooltip",
                                                      tags$span("?", class = "tooltip-icon", title = "MQ means Mapping Quality"),
                                                      tags$span("MQ:")
                                             ),
                                             selectInput("snp_mq_operator", NULL,
                                                         choices = c("-N/A-", ">" = ">", "<" = "<", ">=" = ">=", "<=" = "<=", "=" = "="),
                                                         selected = "<"
                                             )
                                      ),
                                      column(6,
                                             numericInput("snp_mq_value", "Value:", value = NA, min = 0, step = 0.1)
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                             tags$div(class = "label-with-tooltip",
                                                      tags$span("?", class = "tooltip-icon", title = "MQ means Mapping Quality"),
                                                      tags$span("MQRankSum:")
                                             ),
                                             selectInput("snp_mqrank_operator", NULL,
                                                         choices = c("-N/A-", ">" = ">", "<" = "<", ">=" = ">=", "<=" = "<=", "=" = "="),
                                                         selected = "<"
                                             )
                                      ),
                                      column(6,
                                             numericInput("snp_mqrank_value", "Value:", value = NA, min = 0, step = 0.1)
                                      )
                                    )
                             ),
                             column(6,
                                    h4("Indel"),
                                    fluidRow(
                                      column(6,
                                             tags$div(class = "label-with-tooltip",
                                                      tags$span("?", class = "tooltip-icon", title = "QD means Quality Depth"),
                                                      tags$span("QD:")
                                             ),
                                             selectInput("indel_qd_operator", NULL,
                                                         choices = c("-N/A-", ">" = ">", "<" = "<", ">=" = ">=", "<=" = "<=", "=" = "="),
                                                         selected = "<"
                                             )
                                      ),
                                      column(6,
                                             numericInput("indel_qd_value", "Value:", value = NA, min = 0, step = 0.1)
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                             tags$div(class = "label-with-tooltip",
                                                      tags$span("?", class = "tooltip-icon", title = "QD means Quality Depth"),
                                                      tags$span("SQR:")
                                             ),
                                             selectInput("indel_sqr_operator", NULL,
                                                         choices = c("-N/A-", ">" = ">", "<" = "<", ">=" = ">=", "<=" = "<=", "=" = "="),
                                                         selected = ">"
                                             )
                                      ),
                                      column(6,
                                             numericInput("indel_sqr_value", "Value:", value = NA, min = 0, step = 0.1)
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                             tags$div(class = "label-with-tooltip",
                                                      tags$span("?", class = "tooltip-icon", title = "QD means Quality Depth"),
                                                      tags$span("ReadPosRank:")
                                             ),
                                             selectInput("indel_pos_operator", NULL,
                                                         choices = c("-N/A-", ">" = ">", "<" = "<", ">=" = ">=", "<=" = "<=", "=" = "="),
                                                         selected = ">"
                                             )
                                      ),
                                      column(6,
                                             numericInput("indel_pos_value", "Value:", value = NA, min = 0, step = 0.1)
                                      )
                                    ),
                                    fluidRow(column(6,checkboxInput("auto_fill", "Auto-fill Inputs", value = FALSE), ))
                                            
                             )
                           )
                           
                         ),
                         br(),  # Add a line break for spacing
                         actionButton("update_filters", "Update", icon = icon("sync")),
                         actionButton("Save_filters", "Save", icon = icon("save")),
                       ),
                       actionButton("results_view", "Go to Clinical Results",  class = "btn-primary")
                ),
                column(width = 8,
                       box(
                         title = div(
                           tags$div("Quality Filter Plots", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, collapsible = T, id = "variantfilteration_box1",
                         #fileInput("vcf_file", "Upload VCF File"),
                         #actionButton("process", "Process Data"),
                         fluidRow(
                           column(6,
                                  plotlyOutput("snp_plot_filter1"),
                                  plotlyOutput("snp_plot_filter2"),
                                  plotlyOutput("snp_plot_filter3"),
                                  plotlyOutput("snp_plot_filter4"),
                                  plotlyOutput("snp_plot_filter5")
                                  
                                  ),
                           column(6,
                                  plotlyOutput("indel_plot_filter1"),
                                  plotlyOutput("indel_plot_filter2"),
                                  plotlyOutput("indel_plot_filter3")
                                  
                                  )
                           
                           
                           )
                         
                       )
                )
              )
      ),
    

      tabItem(tabName = "runningjobs",
              fluidRow(
                column(width=12,actionButton("Refresh", label = NULL, icon = icon("redo"))),
                column(width = 12,
                       box(tags$script("
                                       $(document).on('click', '.abort-btn', function() {
                                        var jobId = $(this).data('job-id');
                                        Shiny.setInputValue('abort_job_id', jobId, { priority: 'event' });
                                        // Reset the input value to null after a short delay
                                        setTimeout(function() {
                                          Shiny.setInputValue('abort_job_id', null, { priority: 'event' });
                                        }, 5);
                                      });"),
                           title = div(
                           tags$div("Running Jobs", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "run_box1",
                           DTOutput("running_jobs_table")
                       )
                ),
                column(width = 12,
                       box(tags$script("
                                       $(document).on('click', '.view-btn', function() {
                                        var jobId = $(this).data('job-id');
                                        Shiny.setInputValue('view_job_id', jobId, { priority: 'event' });
                                        // Reset the input value to null after a short delay
                                        setTimeout(function() {
                                          Shiny.setInputValue('view_job_id', null, { priority: 'event' });
                                        }, 5);
                                      });"),
                           title = div(
                           tags$div("Completed Jobs", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "run_box2",
                           DTOutput("completed_jobs_table")
                       )
                ),
                column(width = 12,
                       box(tags$script("
                                    $(document).on('click', '.resume-btn', function() {
                                      var jobId = $(this).data('job-id');
                                      Shiny.setInputValue('resume_job_id', jobId, { priority: 'event' });
                                      
                                      // Reset the input value to null after a short delay
                                      setTimeout(function() {
                                        Shiny.setInputValue('resume_job_id', null, { priority: 'event' });
                                      }, 5);
                                    });
                                    
                                    $(document).on('click', '.delete-btn', function() {
                                      var jobId = $(this).data('job-id');
                                      Shiny.setInputValue('delete_job_id', jobId, { priority: 'event' });
                                      // Reset the input value to null after a short delay
                                      setTimeout(function() {
                                        Shiny.setInputValue('delete_job_id', null, { priority: 'event' });
                                      }, 5);
                                    });"),
                           title = div(
                           tags$div("Aborted Jobs", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "run_box3",
                           DTOutput("aborted_jobs_table")
                       )
                )
              )
      ),
          tabItem(tabName = "datainput2",
              fluidRow(
                column(width = 6,
                       box(
                         title = div(
                           tags$div("Select Job ID", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = 'basic_box1',
                         selectInput("select_job_id_di1", "Job ID", choices = c(NULL)),
                         #selectInput("select_patient_id", "Patient ID", choices = c(1, 2, 3))
                        #  textOutput("display_patient_id_di1"),
                         
                       )
                ),
                column(width = 6,
                       box(
                         title = div(
                           tags$div("Select Variant Type", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                        #  plotlyOutput("pieChart7"),
                        #  actionButton("view_r3", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                        selectInput("variant_type1", "Variant Types",
                                       choices = c("SNPs", "Indels", "CNV"),
                                       selected = "SNPs"
                           )
                       )
                ),
                
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Data Display", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         dataTableOutput("variant_data"),
                        #  actionButton("view_r3", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                       )
                ),
                # column(width = 6,
                #        box(
                #          title = "SNP Polyphen", status = "primary", solidHeader = F, width = 12,
                #          plotlyOutput("pieChart4")
                #        )
                # ),
                # column(width = 6,
                #        box(
                #          title = "SNP SIFT", status = "primary", solidHeader = F, width = 12,
                #          plotlyOutput("pieChart5")
                #        )
                # ),
                # column(width = 6,
                #        box(
                #          title = "Indel Coding Consequence", status = "primary", solidHeader = F, width = 12,
                #          plotlyOutput("pieChart6")
                #        )
                # ),
                # column(width = 12,
                #        box(
                #          title = "SNPs", status = "primary", solidHeader = F, width = 12,
                #          plotlyOutput("barGraph1")
                #        )
                # ),
                # column(width = 12,
                #        box(
                #          title = "Indels", status = "primary", solidHeader = F, width = 12,
                #          plotlyOutput("barGraph2")
                #        )
                # )
              )
      ),
tabItem(tabName = "ALL_variants2",
              fluidRow(
                column(width = 3,
                       box(
                         title = div(
                           tags$div("Select Job ID", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = 'basic_box2',
                         selectInput("select_job_id_di2", "Job ID", choices = c(NULL), multiple = T),
                         br(),
                         prettyRadioButtons(inputId = "Summariseplot_label",  label = "Display numbers label:", choices =  c("TRUE","FALSE"),
                           selected = "FALSE", inline = T, animation = "tada", shape = "square", bigger = T),
                           
                           sliderInput("bar_width","Bar width:", min = 0, max = 1, value = 0.3, step = 0.1, width = "100%"),
            sliderInput("jitter_size","Jitter_size:", min = 0, max = 2, value = 0.5, step = 0.1, width = "100%"),
            # sliderInput("jitter_size","Jitter size:", min = 0, max = 5,value = 1,step = 0.1, width = "100%"),
            # sliderInput("jitter_width","Jitter width:", min = 0, max = 0.5,value = 0.3,step = 0.01, width = "100%"),
            br(),
                           actionButton(
  "summarise_plot", 
  "Download Plot", 
  style = "background-color: #76afa5; border-radius: 28px; 
           color: #ffffff; font-weight: bold; text-align: center; 
           display: inline-block; width: 100%;"
)
                       )
                ),
                column(width = 9,
                       box(
                         title = div(
                           tags$div("Display SNP + Indels all Variant Plots", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         plotlyOutput("SNPbarchart"),
                       )
                ),
                
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Data Display", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         dataTableOutput("snpcomparisons")
                        #  actionButton("view_r3", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                       )
                ),
              )
      ),
      tabItem(tabName = "snp_analysis2",
              fluidRow(
                column(width = 3,
                       box(
                         title = div(
                           tags$div("Set SNP Analysis Plot", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = 'basic_box3',
                         selectInput("select_job_id_di3", "Select Job ID(s)", choices = c(NULL)),
                         br(),
                         textInput("heatmap_colors", label = "SNP Heatmap High Freq color", value = "white,#3d85c6" ),
                         br(),
                         prettyRadioButtons(inputId = "Summariseplot_label",  label = "Display numbers label:", choices =  c("TRUE","FALSE"),
                           selected = "FALSE", inline = T, animation = "tada", shape = "square", bigger = T),
                           br(),
                           actionButton(
  "summarise_plot", 
  "Download Plot", 
  style = "background-color: #76afa5; border-radius: 28px; 
           color: #ffffff; font-weight: bold; text-align: center; 
           display: inline-block; width: 100%;"
)
                       )
                ),
                column(width = 9,
                       box(
                         title = div(
                           tags$div("Display SNP Analysis HeatMap", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         plotlyOutput("SNPheatmap"),
                       )
                ),
                
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Display SNP Analysis Data", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         dataTableOutput("snp_analysis_data2")
                        #  actionButton("view_r3", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                       )
                ),
              )
      ),
      tabItem(tabName = "Indel_analysis2",
              fluidRow(
                column(width = 3,
                       box(
                         title = div(
                           tags$div("Set Indel Analysis Plot", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = 'basic_box4',
                         
                         selectInput("select_job_id_di4", "Select Job ID(s)", choices = c(NULL), multiple = T),
                        br(),
                         selectInput("analyse_mode", "Analyse Mode:", choices = c("density" = "density", "heatmap" = "heatmap"), selected = "density"),
                         
                         br(),  # Add a line break for spacing
                         conditionalPanel(
                           condition = "input.analyse_mode == 'density'",
                           selectInput("density_position", "Density Position:",
                                       choices = c("stack", "identity"),
                                       selected = "stack"
                           )
                         ),
                         conditionalPanel(
                           condition = "input.analyse_mode == 'heatmap'",
                          textInput("color", label = "Indel density plot", value = "white, red" )
                         ),
                         br(),
                         prettyRadioButtons(inputId = "Summariseplot_label",  label = "Display numbers label:", choices =  c("TRUE","FALSE"),
                           selected = "FALSE", inline = T, animation = "tada", shape = "square", bigger = T),
                           br(),
                           actionButton(
  "summarise_plot", 
  "Download Plot", 
  style = "background-color: #76afa5; border-radius: 28px; 
           color: #ffffff; font-weight: bold; text-align: center; 
           display: inline-block; width: 100%;"
)
                       )
                ),
                column(width = 9,
                       box(
                         title = div(
                           tags$div("Display Indel Insertion/Deletion length density", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         plotlyOutput("indel_density_plot1"),
                         plotlyOutput("indel_density_plot2")
                       )
                ),
                
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Display SNP Analysis Data", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         dataTableOutput("indel_analysis_data")
                        #  actionButton("view_r3", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                       )
                ),
              )
      ),
      tabItem(tabName = "distribution2",
              fluidRow(
                column(width = 3,
                       box(
                         title = div(
                           tags$div("Set Distribution Plot", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = 'basic_box5',
                         selectInput("select_job_id_di5", "Select Job ID(s)", choices = c(NULL), multiple = F),
                         prettyRadioButtons(inputId = "Summariseplot_label",  label = "Variant Type:", choices =  c("snp","indel"),
                           selected = "snp", inline = T, animation = "tada", shape = "square", bigger = T),
                           selectInput("genomic_feature", "Select Genomic Feature Column:", choices = c("Chr","Func.refGene", "Gene.refGene"), selected = "Func.refGene"),
                           selectInput("bar_position", "Bar Position:", choices = c("dodge", "fill"), selected = "dodge"),
                          #  sliderInput("bar_width", "Bar Width:", min = 0, max = 5, value = 3, step = 0.5, width = "100%"),
                           sliderInput("bar_size", "Bar Size:", min = 0, max = 5, value = 3, step = 0.5, width = "100%"),
                           actionButton(
  "summarise_plot", 
  "Download Plot", 
  style = "background-color: #76afa5; border-radius: 28px; 
           color: #ffffff; font-weight: bold; text-align: center; 
           display: inline-block; width: 100%;"
)
                       )
                ),
                column(width = 9,
                       box(
                         title = div(
                           tags$div("Display Variants Genes Plot", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         plotlyOutput("dist_plot1")
                       )
                ),
                
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Display Variants Genes Data", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         dataTableOutput("distibution_data1")
                        #  actionButton("view_r3", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                       )
                ),
              )),
tabItem(tabName = "Variants_summarise2",
              fluidRow(
                column(width = 3,
                       box(
                         title = div(
                           tags$div("Set Variations Plot", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = 'basic_box6',
                         selectInput("select_job_id_di6", "Select Job ID(s)", choices = c(NULL), multiple = F),
                         prettyRadioButtons(inputId = "Summariseplot_label1",  label = "Variant Type:", choices =  c("snp","indel", "all"),
                         selected = "snp", inline = T, animation = "tada", shape = "square", bigger = T),
                         selectInput("genomic_feature1", "Select Gene Column:", choices = c( "Gene.refGene"), selected = "Gene.refGene"),
                          #  sliderInput("bar_width", "Bar Width:", min = 0, max = 5, value = 3, step = 0.5, width = "100%"),
                          #  sliderInput("bar_size", "Bar Size:", min = 0, max = 5, value = 3, step = 0.5, width = "100%"),
                          actionButton(
                            "summarise_plot", 
                            "Download Plot", 
                            style = "background-color: #76afa5; border-radius: 28px; 
                                    color: #ffffff; font-weight: bold; text-align: center; 
                                    display: inline-block; width: 100%;"
                          )
                       )
                ),
                column(width = 9,
                       box(
                         title = div(
                           tags$div("Display Variants Genes Plot", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         plotlyOutput("dis_plot2"),
                       )
                ),
                
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Display Variants Genes Data", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         dataTableOutput("distribution_data2")
                        #  actionButton("view_r3", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                       )
                ),
              )
      ),
      tabItem(   tabName = "result1",
                 fluidRow(
                   column(width = 12,
                          box(
                            title = div(
                           tags$div("Select Job ID", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "result1_box1",
                            selectInput("select_job_id_r1", "Job ID", choices =  c(NULL)),
                            textOutput("display_patient_id_r1")
                          )
                   ),
                   column(width = 6,
                          box(
                            title = div(
                           tags$div("Variant Classes", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "result1_box2",
                            plotlyOutput("pieChart1"),
                            actionButton("view_table", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                          )
                   ),
                   column(width = 6,
                          box(
                            title = div(
                           tags$div("Variant Consequences", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                            plotlyOutput("pieChart2"),
                            actionButton("view_table", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                          )
                   ),
                   column(width = 6,
                          box(
                            title = div(
                           tags$div("SNP Coding Consequence", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                            plotlyOutput("pieChart3"),
                            actionButton("view_table", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                          )
                   ),
                   column(width = 6,
                          box(
                            title = div(
                           tags$div("SNP Polyphen", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                            plotlyOutput("pieChart4"),
                            actionButton("view_table", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                          )
                   ),
                   column(width = 6,
                          box(
                            title = div(
                           tags$div("SNP Sift", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                            plotlyOutput("pieChart5"),
                            actionButton("view_table", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                          )
                   ),
                   column(width = 6,
                          box(
                            title = div(
                           tags$div("Indel Coding Consequence", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                            plotlyOutput("pieChart6"),
                            actionButton("view_table", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                          )
                   ),
                   column(width = 12,
                          box(
                            title = div(
                           tags$div("SNPs", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "result1_box3",
                            plotlyOutput("barGraph1"),
                            actionButton("view_table", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                          )
                   ),
                   column(width = 12,
                          box(
                            title = div(
                           tags$div("Indels", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                            plotlyOutput("barGraph2"),
                            actionButton("view_table", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                          )
                   )
                   
                 )
      ),
      tabItem(tabName = "result2",
              fluidRow(
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Select Job ID", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = 'result2_box1',
                         selectInput("select_job_id_r2", "Job ID", choices = c(NULL)),
                         #selectInput("select_patient_id", "Patient ID", choices = c(1, 2, 3))
                         textOutput("display_patient_id_r2"),
                         
                       )
                ),
                # column(width = 6,
                #        box(
                #          title = "Patient ID", status = "primary", solidHeader = F, width = 12,
                #          textOutput("patient_id_display")
                #        )
                # ),
                column(width = 6,
                       box(
                         title = div(
                           tags$div("ACMG Classification", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "result2_box2",
                         plotlyOutput("pieChart7"),
                         actionButton("view_r3", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                       )
                ),
                column(width = 6,
                       box(
                         title = div(
                           tags$div("Clinvar Classification", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         plotlyOutput("pieChart8"),
                         actionButton("view_r3", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                       )
                ),
                
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Pathogenic/Likely Pathogenic Variants", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "result2_box3",
                         dataTableOutput("pathogenic_variants"),
                         actionButton("view_r3", label = NULL, icon = icon("eye"), class = "btn-primary", style = "float: right;")
                       )
                ),
                # column(width = 6,
                #        box(
                #          title = "SNP Polyphen", status = "primary", solidHeader = F, width = 12,
                #          plotlyOutput("pieChart4")
                #        )
                # ),
                # column(width = 6,
                #        box(
                #          title = "SNP SIFT", status = "primary", solidHeader = F, width = 12,
                #          plotlyOutput("pieChart5")
                #        )
                # ),
                # column(width = 6,
                #        box(
                #          title = "Indel Coding Consequence", status = "primary", solidHeader = F, width = 12,
                #          plotlyOutput("pieChart6")
                #        )
                # ),
                # column(width = 12,
                #        box(
                #          title = "SNPs", status = "primary", solidHeader = F, width = 12,
                #          plotlyOutput("barGraph1")
                #        )
                # ),
                # column(width = 12,
                #        box(
                #          title = "Indels", status = "primary", solidHeader = F, width = 12,
                #          plotlyOutput("barGraph2")
                #        )
                # )
              )
      ),
      tabItem(tabName = "result3",

              fluidRow(  column(width = 12,
                            box(
                                title = div(
                           tags$div("Select Job ID", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "result3_box1",
                                selectInput("select_job_id_r3", "Select Job ID", choices = c(NULL)),
                                textOutput("display_patient_id_r3")
                                )
              ),
                column(width = 12,
                        box(
                         title = div(
                           tags$div("ACMG Class", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "result3_box2",
                         div(style = "float: right; margin-bottom: 10px;",
                             actionButton("tab_filter", "Filters", icon = icon("filter"), class = "btn-filter" ),
                             actionButton("tab_remove", "Remove filters", icon = icon("filter-circle-xmark"), class = "btn-filter" )
                         ),
                         div(id = "select_column1", style = "float: left; margin-bottom: 10px;",
                             pickerInput(
                               inputId = "columns",
                               label = "Select Columns",
                               choices = NULL,
                               selected = NULL,
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 `selected-text-format` = "count > 3",
                                 `count-selected-text` = "{0} columns selected"
                               )
                             )),
                         br(),
                         dataTableOutput("selected_columns2"),
                         br(),  # Add a break for spacing between the table and the button
                         column(12, align = "left", actionButton("save_button", "Save Selected Rows"))
                       )),
                br()
              ),
              div(style = "text-align:center;",
                  actionButton("query_kg_button", "Query KG", class = "btn-primary"),
                  actionButton("cnv_button", "Go to CNV analysis", class = "btn-primary")
                  # actionButton("view_report", "View Report", class = "btn-success", disabled = TRUE)
              ),
              br()
      ),
      tabItem(tabName = "result4",

              fluidRow(  column(width = 12,
                            box(
                                title = div(
                           tags$div("Select Job ID", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "result4_box1",
                                selectInput("select_job_id_r4", "Select Job ID", choices = c(NULL)),
                                textOutput("display_patient_id_r4")
                                )
              ),
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Copy Number Segment", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "result4_box2",
                        
                         DTOutput("cnsTable"))
                       ),
                br(),
              column(width = 12,
                     box(
                       title = div(
                           tags$div("Copy Number Ratio", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "result4_box3",
                       br(),
                       DTOutput("cnrTable")
                       
                     )
              ),
              br(),
                    column(width = 12,
                     box(
                       title = div(
                           tags$div("Scoresheet", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "result4_box4",
                       br(),
                       div(id = "select_column_cnv", style = "float: left; margin-bottom: 10px;",
                             pickerInput(
                               inputId = "cnv_columns",
                               label = "Select Columns",
                               choices = NULL,
                               selected = NULL,
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 `selected-text-format` = "count > 3",
                                 `count-selected-text` = "{0} columns selected"
                               )
                             )),
                       DTOutput("scoresheetTable")
                       
                     )
              )
              
              ),
              
              br(),
              div(style = "text-align:center;",
                  actionButton("tmb_button", "View TMB Score",  class = "btn-primary")
                  # actionButton("view_report", "View Report", class = "btn-success", disabled = TRUE)
              )
              
                # column(width = 12,
                #        box(
                #          title = "SNPs", status = "primary", solidHeader = F, width = 12,
                #          dataTableOutput("snp_table_r3")
                #        )
                # ),
                # column(width = 12,
                #        box(
                #          title = "Indels", status = "primary", solidHeader = F, width = 12,
                #          dataTableOutput("indel_table_r3")
                #        )
                # ),
                # column(width = 12,
                #        box(
                #          title = "VEP", status = "primary", solidHeader = F, width = 12,
                #          dataTableOutput("vep_table_r3")
                #        )
                # ),
              
              

              # div(style = "text-align:center;",
              #     actionButton("generate_report", "Generate Report", class = "btn-primary"),
              #     actionButton("view_report", "Query KG", class = "btn-success", disabled = TRUE)
              # )
    ),
      tabItem(tabName = "kgimputation",
              fluidRow(
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Select Job ID", class = "top-label"),
                         ), status = "primary", solidHeader = F, width = 9, id = "kg_box1",
                         selectInput("select_job_id_kg", "Select Job ID", choices = c(NULL)),
                         textOutput("display_patient_id_kg")
                       )
                ),
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Knowledge Graph", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = FALSE, collapsible = T, width = 9, id = "kg_box2",
                         visNetworkOutput("network")
                       )
                ),
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Report Table", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "kg_box3",
                         dataTableOutput("kg_table"),
                         actionButton("save_row", "Save Selected Row", icon = icon("save"))
                       )
                )
                
              ),
              div(style = "text-align:center;",
                  actionButton("report_generation_button", "Report Generation", class = "btn-primary")
                  # actionButton("view_report", "View Report", class = "btn-success", disabled = TRUE)
              ),
              br()
      ),
      # source("modules/0.Introduction.R")$value,
      # source("modules/1.Upload_ui.R")$value,
      # ### first part
      # #source("modules/2.Venn_ui.R")$value,
      # source("modules/3.summarise_barplot_ui.R")$value,
      # source("modules/4.SNP_Analysis_ui.R")$value,
      # source("modules/5.Indel_Analysis_ui.R")$value,
      # # source("modules/6.circle_ui.R")$value,
      # ### second part
      # source("modules/7.Distribution_ui.R")$value,
      # source("modules/8.VariantsGene_Summarise_ui.R")$value,
      # source("modules/9.VariantsGene_Heatmap_ui.R")$value,
      # source("modules/10.mutational_signature_ui.R")$value,
      # source("modules/11.Annotation_ui.R")$value,
      
      tabItem(tabName = "reportgeneration",
              fluidRow(
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Select Job ID", class = "top-label"),
                         ), status = "primary", solidHeader = F, width = 12, id = "report_box1",
                         selectInput("select_job_id_rpt", "Select Job ID", choices = c("", NULL)),
                         textOutput("display_patient_id_rpt")
                       )
                ),
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Type Your Query", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         textAreaInput("file_content", "File Content", "", rows = 3, width = "100%"),
                         actionButton("save", "Save", class = "btn-primary"),
                         actionButton("generate_report", "Generate Report", class = "btn-success")
                       )
                ),
                column(width = 12,
                       box(
                         title = div(
                           tags$div("Generated Report", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         textAreaInput("report_content", "Generated Report", "", rows = 10, width = "100%"),
                         actionButton("save_report", "Save", class = "btn-primary")
                        #  downloadButton("get_pdf", "Get PDF FILE", class = "btn-primary")
                       )
                )
              )),    
              tabItem(tabName = "tmbscore",
              fluidRow(
                column(width = 4,
                       box(
                         title = div(
                           tags$div("Select Job ID", class = "top-label"),
                         ), status = "primary", solidHeader = F, width = 12, id = "tmb_box1",
                         selectInput("select_job_id_tmb", "Select Job ID", choices = c("", NULL)),
                         textOutput("display_patient_id_tmb")
                       )
                ),
                column(width = 4,
                       box(
                         title = div(
                           tags$div("Filter", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "vf_value1",
                         numericInput("vaf_value", "Enter the VF Value:", value = 0.5, min = 0, step = 0.1)
                       )
                ),
                column(width = 4,
                       box(
                         title = div(
                           tags$div("Panel Files", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12,
                         div (id = "panel_box", style = "margin-bottom: 10px;",
                         selectInput("panel_final", label = "Select the Panel File", choices = c("OncoKB-all-CancerGeneList.txt", "OncoKB-CancerGeneList-Oncogene.txt"), selected = NULL))
                       )
                ),
                column(width = 12,
                       box(
                         title = div(
                           tags$div("TMB Score Table", class = "top-label"),
                           tags$span(
                             icon("info-circle", id = "infoIcon", class = "info-icon"),
                             style = "float: right;" # Align info icon to the right
                           )
                         ), status = "primary", solidHeader = F, width = 12, id = "tmb_box2",
                         DTOutput("tmbscoretable")
                       )
                )
                # column(width = 12,
                #        box(
                #          title = div(
                #            tags$div("TMB Scores of Samples", class = "top-label"),
                #            tags$span(
                #              icon("info-circle", id = "infoIcon", class = "info-icon"),
                #              style = "float: right;" # Align info icon to the right
                #            )
                #          ), status = "primary", solidHeader = F, width = 12,
                #          plotlyOutput("tmbbargraph")
                #        )
                # )
                
              )),                                

      tabItem(tabName = "workflow",
              fluidRow(
                column(width = 12,
                       textInput("workflow_name", "Workflow Name", placeholder = "Enter the name of your customized workflow"),
                       uiOutput("workflowFields"),
                       actionButton("add_field", "Add New Field", icon = icon("plus"), class = "btn btn-primary add-field-btn"),
                       div(style = "margin-top: 10px;"),
                       actionButton("save_workflow", "Save Workflow", class = "btn btn-success")
                )
              )
      ),
      tabItem(tabName = "genome_browser",
              
              fluidPage(    tags$head(
                tags$style(HTML("
      .container-fluid {
        background-color: white !important;
        min-height: 100vh;  /* Ensure it covers the full viewport height */
      }
      body {
        margin: 0; /* Remove default margin to ensure full coverage */
      }
    "))
              ),
              fluidRow(
                column(12,igvShinyOutput('igvShiny_0'))
              ),
              fluidRow(
                column(3,selectInput("select_job_id_gb1", "Select Job ID", choices = c("", NULL)),
                       actionButton("removeUserTracksButton", "Remove Tracks"))
              ),
              fluidRow(
                column(3, textInput("roi", label = "", placeholder = "gene or chrN:start-end"),
                       actionButton("searchButton", "Search")),
                #column(2, actionButton("addLocalVCFTrackButton", "Add 1kg VCF (local file)")),
              ),

              
              br(), br(), br(),
              
              # fluidRow(
              #   column(3, actionButton("getChromLocButton", "Get Region")),
              #   column(3, actionButton("clearChromLocButton", "Clear Region Readout")),
              #   column(6, div(
              #     style = "background-color: white; width: 100%; height:30px; padding-left: 5px;
              #  margin-top: 10px; border: 1px solid blue;",
              #     htmlOutput("chromLocDisplay")
              #   ))
              # )
              
              # fluidRow(
              #   column(
              #     width = 12,
              #     BioCircosOutput("circoplot",  width = "100%", height = "900px")
              #   )
              # )
      )),
      
      tabItem(tabName = "bugsubmission",
              fluidRow(
                column(width = 12,
                       box(
                         title = "Bug Submission", status = "primary", solidHeader = F, width = 12,
                         div(id = "screenshotDiv", style = "margin-bottom: 10px;",  # Optional style for spacing
                       fileInput("bug_screenshot", "Upload Screenshot", 
                                 accept = c(".png", ".jpg", ".jpeg")),
                       tags$div(style = "font-size: 12px; color: #888;", 
                                "Please upload a screenshot that illustrates the bug. Supported formats: PNG, JPG, JPEG.")
                   ),
                        #  fileInput("bug_screenshot", "Upload Screenshot", accept = c(".png", ".jpg", ".jpeg"),
                         textAreaInput("bug_description", "Describe the issue", "", width = "100%", height = "200px"),
                         actionButton("submit_bug", "Submit Bug", icon = icon("paper-plane"), class = "btn btn-primary")
                       )
                )
              )),
              tabItem(tabName = "resources",
              fluidRow(
                HTML('
                    <h1 style="text-align: center;color: #2b3e50;margin-bottom: 20px;">Oncogen.AI - Resources</h1>
<h2>ILLUMINA</h2>
    <!-- Section for Illumina -->
    <div class="resource-section">
        <h2>Backend VCF Calling</h2>
        <table class="resource-table">
            <thead>
                <tr>
                    <th>Task</th>
                    <th>Tool/Package</th>
                    <th>Link</th>
                    <th>Version</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>Fastq to VCF</td>
                    <td>GATK</td>
                    <td><a href="https://gatk.broadinstitute.org/hc/en-us/articles/360035889511-GATK-4-5-0-0-release-notes" target="_blank">GATK-pipeline</a></td>
                    <td>4.5.0.0</td>
                </tr>
                <tr>
                    <td>Indexing reference genome</td>
                    <td>bwa</td>
                    <td><a href="http://bio-bwa.sourceforge.net/" target="_blank">bwa-resource</a></td>
                    <td>0.7.17</td>
                </tr>
                <tr>
                    <td>Alignment</td>
                    <td>bwa</td>
                    <td><a href="http://bio-bwa.sourceforge.net/" target="_blank">bwa-resource</a></td>
                    <td>0.7.17</td>
                </tr>
                <tr>
                    <td>Mark duplicate reads in alignment</td>
                    <td>GATK</td>
                    <td><a href="https://gatk.broadinstitute.org/hc/en-us/articles/360035889511-GATK-4-5-0-0-release-notes" target="_blank">GATK-release</a></td>
                    <td>4.5.0.0</td>
                </tr>
                <tr>
                    <td>Depth of coverage for aligned reads</td>
                    <td>samtools</td>
                    <td><a href="http://www.htslib.org/" target="_blank">samtools-resource</a></td>
                    <td>1.2</td>
                </tr>
                <tr>
                    <td>Base Quality Score recalibration</td>
                    <td>GATK</td>
                    <td><a href="https://gatk.broadinstitute.org/hc/en-us/articles/360035889511-GATK-4-5-0-0-release-notes" target="_blank">GATK-release</a></td>
                    <td>4.5.0.0</td>
                </tr>
                <tr>
                    <td>Bed-filtering</td>
                    <td>KAPA HyperExome</td>
                    <td><a href="https://sequencing.roche.com/global/en/products/group/kapa-hyperexome.html" target="_blank">bed-files</a></td>
                    <td>-</td>
                </tr>
                <tr>
                    <td>Variant calling</td>
                    <td>GATK</td>
                    <td><a href="https://gatk.broadinstitute.org/hc/en-us/articles/360035889511-GATK-4-5-0-0-release-notes" target="_blank">GATK-release</a></td>
                    <td>4.5.0.0</td>
                </tr>
                <tr>
                    <td>Separate SNPs and indels</td>
                    <td>GATK</td>
                    <td><a href="https://gatk.broadinstitute.org/hc/en-us/articles/360035889511-GATK-4-5-0-0-release-notes" target="_blank">GATK-release</a></td>
                    <td>4.5.0.0</td>
                </tr>
                <tr>
                    <td>Apply quality filters</td>
                    <td>GATK</td>
                    <td><a href="https://gatk.broadinstitute.org/hc/en-us/articles/360035889511-GATK-4-5-0-0-release-notes" target="_blank">GATK-pipeline</a></td>
                    <td>4.5.0.0</td>
                </tr>
            </tbody>
        </table>
    </div>

    <!-- Section for VCF Annotation -->
    <div class="resource-section">
        <h2>VCF Annotation</h2>
        <table class="resource-table">
            <thead>
                <tr>
                    <th>Task</th>
                    <th>Tool/Package</th>
                    <th>Link</th>
                    <th>Version</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>Various annotations info added to variants</td>
                    <td>VEP</td>
                    <td><a href="https://www.ensembl.org/info/docs/tools/vep/index.html" target="_blank">vep-install</a></td>
                    <td>112</td>
                </tr>
                <tr>
                    <td>Selection of principal Transcript ID</td>
                    <td>APRIS</td>
                    <td><a href="https://github.com/apris" target="_blank">APRIS</a></td>
                    <td>2024_06.v48</td>
                </tr>
                <tr>
                    <td>LOVD database</td>
                    <td>LOVD</td>
                    <td><a href="https://www.lovd.nl/" target="_blank">LOVD-db</a></td>
                    <td>3.0-3b</td>
                </tr>
                <tr>
                    <td>ACMG Clinical Significance Interpretation</td>
                    <td>InterVar</td>
                    <td><a href="https://wintervar.wglab.org/" target="_blank">InterVar-install</a></td>
                    <td>2021-08-version</td>
                </tr>
                <tr>
                    <td>Various annotations info added to variants</td>
                    <td>ANNOVAR</td>
                    <td><a href="https://www.openbioinformatics.org/annovar/annovar_download_form.php" target="_blank">ANNOVAR</a></td>
                    <td>-</td>
                </tr>
            </tbody>
        </table>
    </div>

<!-- Section for CNV Analysis -->
    <div class="resource-section">
        <h2>CNV Analysis</h2>
        <table class="resource-table">
            <thead>
                <tr>
                    <th>Task</th>
                    <th>Tool/Package</th>
                    <th>Link</th>
                    <th>Version</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td rowspan="2">Copy Number Variant Analysis</td>
                    <td>CNVKit</td>
                    <td><a href="https://github.com/etal/cnvkit" target="_blank">CNVKit</a></td>
                    <td>v0.9.11</td>
                </tr>
                <tr>
                    <td>ClassifyCNV</td>
                    <td><a href="https://github.com/Genotek/ClassifyCNV">ClassifyCNV</a></td>
                    <td>v1.1.1</td>
                </tr>
            </tbody>
        </table>
    </div>
<!-- Section for Genome Visualization -->
    <div class="resource-section">
        <h2>Genomic Visualizaton</h2>
        <table class="resource-table">
            <thead>
                <tr>
                    <th>Task</th>
                    <th>Tool/Package</th>
                    <th>Link</th>
                    <th>Version</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td rowspan="3">Genome Viewer</td>
                    <td>igvShiny</td>
                    <td><a href="https://www.bioconductor.org/packages/release/bioc/html/igvShiny.html">igvShiny</a></td>
                    <td>1.0.5</td>
                </tr>
                <tr>
                    <td>VariantAnnotation</td>
                    <td><a href="https://bioconductor.org/packages/release/bioc/html/VariantAnnotation.html">VariantAnnotation</a></td>
                    <td>1.50.0</td>
                </tr>
<tr>
                    <td>GenomicAlignments</td>
                    <td><a href="https://bioconductor.org/packages/release/bioc/html/GenomicAlignments.html">GenomicAlignments</a></td>
                    <td>1.40.0</td>
                </tr>
<tr>
                    <td rowspan="4">Knowledgebase Network</td>
                    <td>COSMIC </td>
                    <td><a href="https://cancer.sanger.ac.uk/cosmic/download/cosmic">COSMIC-datasets</a></td>
                    <td>v100</td>
                </tr>
<tr>
                    <td>ClinVar</td>
                    <td><a href="https://ftp.ncbi.nlm.nih.gov/pub/clinvar/">ClinVar-datasets</a></td>
                    <td>20241001</td>
                </tr>
<tr>
                    <td>Clinical Knowledge Graph </td>
                    <td><a href="https://data.mendeley.com/datasets/mrcf7f4tc2/1">CKG-datasets</a></td>
                    <td>Version 3</td>
                </tr>
<tr>
                    <td>OncoKB</td>
                    <td><a href="https://www.oncokb.org/">OncoKB-data</a></td>
                    <td>2024 version</td>
                </tr>
<tr>
                    <td>LLM based report generation</td>
                    <td>google.generativeai</td>
                    <td><a href="https://pypi.org/project/google-generativeai/">genai</a></td>
                    <td>0.8.3</td>
                </tr>
            </tbody>
        </table>
    </div>

<h2>ION-TORRENT</h2>
<div class="resource-section">
        <h2>Backend VCF calling</h2>
        <table class="resource-table">
            <thead>
                <tr>
                    <th>Task</th>
                    <th>Tool/Package</th>
                    <th>Link</th>
                    <th>Version</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>Backend vcf calling</td>
                    <td>Ion-Torrent Variant Caller</td>
                    <td><a href="https://github.com/domibel/IonTorrent-VariantCaller">Torrent-Variant-Caller</a></td>
                    <td>5.0-3</td>
                </tr>
                            </tbody>
        </table>
    </div>')
              ))
    )
)
                    
)
)


# Define Server
server <- function(input, output, session) {



            # observeEvent(input$select_job_id_di2,{
            #     job_id <- input$select_job_id_di2
            #     job_data <- read_jobs_csv2()
            #     job_data <- job_data %>% filter(Job_ID == job_id)
            #     folder_path= paste0("OncoGenAI/results/",job,"/")
                
            #     # print(job)
            #     genome_version = job_data$Genome
                
                
            #     vcf_snp = paste0(folder_path,job_id,"_snps.",genome_version,"_multianno.txt.grl_p")
            #     vcf_indels = paste0(folder_path, job_id,"_indels",genome_version,"_multianno.txt.grl_p")



            # }
            # )

            # observeEvent(input$select_job_id_di1,{
            #     job_id <- input$select_job_id_di1
            #     job_data <- read_jobs_csv2()
            #     job_data <- job_data %>% filter(Job_ID == job_id)
            #     folder_path= paste0("OncoGenAI/results/",job_id,"/")
                
            #     # print(job)
            #     genome_version = job_data$Genome
                

            #     if(input$variant_type_d1 == "SNPs"){

            #         output$variant_data <- DT::renderDataTable({
            #             req(input$select_job_id_di1, input$variant_data)
            #             vcf_snp = paste0(folder_path,job_id,"_snps.",genome_version,"_multianno.txt.grl_p")
            #             vcf_snp
            #         }, rownames = T, options = list(scrollX = TRUE, pageLength =5))
            #         # variant_data



            #     }else if(input$variant_type_d1 == "Indels"){
            #         output$variant_data <- DT::renderDataTable({
            #             req(input$select_job_id_di1, input$variant_data)
            #             vcf_indels = paste0(folder_path, job_id,"_indels",genome_version,"_multianno.txt.grl_p")
            #             vcf_indels
            #         }, rownames = T, options = list(scrollX = TRUE, pageLength =5))


            #     }

            # }
            # )


            observeEvent(input$select_job_id_di1, {
              job_id <- input$select_job_id_di1
              req(input$select_job_id_di1)
              job_data <- read_jobs_csv2()
              job_data <- job_data %>% filter(Job_ID == job_id)
              folder_path = paste0("OncoGenAI/results/", job_id, "/")  # Corrected 'job' to 'job_id'
              
              genome_version <- job_data$Genome  # Fixed variable assignment

              # Ensure the 'variant_type_d1' input is correctly referenced
              observe({
              if (input$variant_type1 == "SNPs") {
                  vcf_snp <- paste0(folder_path, job_id, "_snps.", genome_version, "_multianno.txt.grl_p")
                  data <- read.csv(vcf_snp, header = TRUE, sep = "\t")
                  output$variant_data <- DT::renderDataTable({
                      req(input$variant_type1,input$variant_type1)  # req() checks if input is not NULL
                      data
                      
                  }, rownames = TRUE, options = list(scrollX = TRUE, pageLength = 5))

              } 


              })

              
              observe({
                  if (input$variant_type1 == "Indels") {
                  vcf_indels <- paste0( folder_path, job_id, "_indels.", genome_version, "_multianno.txt.grl_p")
                  data <- read.csv(vcf_indels, header = TRUE, sep = "\t")
                  output$variant_data <- DT::renderDataTable({
                      req(input$select_job_id_di1, input$variant_type1)
                      data

                  }, rownames = TRUE, options = list(scrollX = TRUE, pageLength = 5))
              } 
              }) 
            })


       #   SNP indel counts

      # observeEvent(input$select_job_id_di2, {
      #   job_id <- input$select_job_id_di2
      #   req(input$select_job_id_di2)  # Ensure a job is selected
      #   job_data <- read_jobs_csv2()
      #   job_data <- job_data %>% filter(Job_ID == job_id)
      #   folder_path <- paste0("OncoGenAI/results/", job_id, "/")  # Path to the result files
      #   genome_version <- job_data$Genome  # Extract genome version
        
      #   # Initialize counters for SNPs and Indels
      #   snp_count <- 0
      #   indel_count <- 0

      #   # Read and count SNPs
      #   vcf_snp <- paste0(folder_path, job_id, "_snps.", genome_version, "_multianno.txt.grl_p")
      #   if (file.exists(vcf_snp)) {
      #     snp_data <- read.table(vcf_snp, header = TRUE, sep = "\t")
      #     snp_count <- nrow(snp_data)
      #   }

      #   # Read and count Indels
      #   vcf_indels <- paste0(folder_path, job_id, "_indels.", genome_version, "_multianno.txt.grl_p")
      #   if (file.exists(vcf_indels)) {
      #     indel_data <- read.table(vcf_indels, header = TRUE, sep = "\t")
      #     indel_count <- nrow(indel_data)
      #   }

      #   # Update plot output
      #   output$SNPbarchart <- renderPlotly({
      #     plot_data <- data.frame(
      #       VariantType = c("SNPs", "Indels"),
      #       Count = c(snp_count, indel_count)
      #     )
          
      #     p <- ggplot(plot_data, aes(x = VariantType, y = Count, fill = VariantType)) +
      #       geom_bar(stat = "identity", width = input$bar_width) +
      #       geom_text(aes(label = Count), vjust = -0.5, size = 5, color = "black") +
      #       theme_minimal() +
      #       labs(title = "Number of SNPs and Indels", x = "Variant Type", y = "Count")
          
      #     ggplotly(p)
      #   })
      # })

      observeEvent(input$select_job_id_di2, {
        job_ids <- input$select_job_id_di2  # Selected Job IDs (could be multiple)
        req(input$select_job_id_di2)  # Ensure at least one job is selected
        job_data <- read_jobs_csv2()
        
        plot_data <- data.frame(Job_ID = character(), VariantType = character(), Count = integer())

        # Loop over each selected job ID
        for (job_id in job_ids) {
          job_data_filtered <- job_data %>% filter(Job_ID == job_id)
          folder_path <- paste0("OncoGenAI/results/", job_id, "/")
          genome_version <- job_data_filtered$Genome  # Extract genome version for the job
          
          # Initialize counters for SNPs and Indels
          snp_count <- 0
          indel_count <- 0
          
          # Read and count SNPs
          vcf_snp <- paste0(folder_path, job_id, "_snps.", genome_version, "_multianno.txt.grl_p")
          if (file.exists(vcf_snp)) {
            snp_data <- read.csv(vcf_snp, header = TRUE, sep = "\t")
            snp_count <- nrow(snp_data)
          }
          
          # Read and count Indels
          vcf_indels <- paste0(folder_path, job_id, "_indels.", genome_version, "_multianno.txt.grl_p")
          if (file.exists(vcf_indels)) {
            indel_data <- read.csv(vcf_indels, header = TRUE, sep = "\t")
            indel_count <- nrow(indel_data)
          }

          # Append the counts for the current job to the plot data
          plot_data <- rbind(plot_data,
                            data.frame(Job_ID = job_id, VariantType = "SNPs", Count = snp_count),
                            data.frame(Job_ID = job_id, VariantType = "Indels", Count = indel_count))
        }



        output$snpcomparisons <- DT::renderDataTable({
            req(input$select_job_id_di2)
            plot_data

        }, rownames = TRUE, options = list(scrollX = TRUE, pageLength = 5))



        # Update the plot output to show SNP and Indel counts for all selected job IDs
        output$SNPbarchart <- renderPlotly({
              p <- ggplot(plot_data, aes(x = Job_ID, y = Count, fill = VariantType)) +
                geom_bar(stat = "identity", position = "dodge", width = input$bar_width) +
                geom_text(aes(label = Count), position = position_dodge(width = input$bar_width), 
                          vjust = -0.5, size = 3, color = "black") +  # Adjusted vjust to move text higher
                theme_minimal() +
                labs(title = "Number of SNPs and Indels", x = "Job ID", y = "Count")
              
              ggplotly(p)
            })
      

      })


## Snap analysis


# Server logic for SNP heatmap and analysis data (one job at a time)
observeEvent(input$select_job_id_di3, {
  req(input$select_job_id_di3)  # Ensure input is available

  # Get the selected job ID
  job_id <- input$select_job_id_di3
  job_data <- read_jobs_csv2()
  job_data <- job_data %>% filter(Job_ID == job_id)
  folder_path <- paste0("OncoGenAI/results/", job_id, "/")
  genome_version <- job_data$Genome

  # Load the SNP file for the selected job ID
  vcf_snp <- paste0(folder_path, job_id, "_snps.", genome_version, "_multianno.txt.grl_p")
  
  # Check if the SNP file exists
  if (file.exists(vcf_snp)) {
    snp_data <- read.csv(vcf_snp, header = TRUE, sep = "\t")

    # Process SNP data to get reference and alternative allele counts
    ref_alt_count <- snp_data %>%
      group_by(Ref, Alt) %>%
      summarise(SNP_numbers = n(), .groups = "drop")  # Avoid grouped output warning

    # Calculate the total SNP count for frequency
    total_snp_count <- sum(ref_alt_count$SNP_numbers)

    # Calculate frequency column
    ref_alt_count <- ref_alt_count %>%
      mutate(freq = (SNP_numbers / total_snp_count) * 100)

    # Create all combinations of REF and ALT and add the group ID
    possible_ref <- c("A", "C", "G", "T")
    possible_alt <- c("A", "C", "G", "T")
    
    all_combinations <- expand.grid(REF = possible_ref, ALT = possible_alt, stringsAsFactors = FALSE) %>%
      mutate(group = job_id)  # Add group ID here

    # Merge ref_alt_count with all_combinations
    ref_alt_count <- merge(all_combinations, ref_alt_count, by.x = c("REF", "ALT"), by.y = c("Ref", "Alt"), all.x = TRUE)
    
    # Fill NA values with 0 for SNP_numbers and freq
    ref_alt_count$SNP_numbers[is.na(ref_alt_count$SNP_numbers)] <- 0
    ref_alt_count$freq[is.na(ref_alt_count$freq)] <- 0  # Ensure freq is 0 for combinations with no SNPs

    # Sort the dataframe for clarity
    ref_alt_count <- ref_alt_count %>% arrange(group, REF, ALT)
    # print(ref_alt_count)
    # Render the heatmap plot using the provided ggplot code
    output$SNPheatmap <- renderPlotly({
      req(ref_alt_count)  # Ensure data is available

      # Custom ggplot for heatmap
      # p <- ggplot2::ggplot(ref_alt_count, aes(ALT, REF, fill = freq)) +
      #   geom_tile() +
      #   geom_text(aes(label = round(freq, digits = 2)), size = (input$heatmap_text.size) / 4) +
      #   facet_wrap(~group, ncol = input$heatmap_ncol) +
      #   scale_fill_gradient(low = stringr::str_split(input$heatmap_color, ",")[[1]][1],
      #                       high = stringr::str_split(input$heatmap_color, ",")[[1]][2]) +
      #   theme_test()
      heatmap_text_size <- 12       # Text size for labels on heatmap
      heatmap_ncol <- 1             # Number of columns for facet_wrap
      heatmap_color <- "blue,red"   # Color gradient as "low,high"



      p <- ggplot2::ggplot(ref_alt_count, aes(ALT,REF,fill = freq))+
        geom_tile()+
        geom_text(aes(label = round(freq, digits = 2)), size = (heatmap_text_size)/4)+
        facet_wrap(~group,ncol = heatmap_ncol)+
        scale_fill_gradient(low = stringr::str_split(input$heatmap_colors, ",")[[1]][1],
                            high = stringr::str_split(input$heatmap_colors, ",")[[1]][2])+
        theme_test()

      # Render ggplotly interactive plot
      ggplotly(p)
    })

      df_snp <- ref_alt_count
      df_snp$group <- NULL
      df_snp <- df_snp[df_snp$REF != df_snp$ALT, ]
        output$snp_analysis_data2 <- DT::renderDataTable({
            req(input$select_job_id_di3,df_snp)
            
            df_snp

        }, rownames = FALSE, options = list(
            scrollX = TRUE,
            pageLength = 12,
            columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ), class = 'cell-border stripe')
      

    # Render the SNP analysis data table
    # output$snp_analysis_data <- renderDataTable({
    #   req(ref_alt_count)  # Ensure data is available
    #   datatable(ref_alt_count, options = list(pageLength = 10), rownames = FALSE)
    # })
  } else {
    showNotification("SNP file not found for the selected Job ID", type = "error")
  }
})


observeEvent(input$select_job_id_di4,{
              req(input$select_job_id_di4)
              job_ids <- input$select_job_id_di4  # Multiple job IDs selected
              combined_data <- data.frame()  # Initialize an empty dataframe to store all job data
              
              for (job_id in job_ids) {
                job_data <- read_jobs_csv2()  # Load job data
                job_data <- job_data %>% filter(Job_ID == job_id)  # Filter for each job ID
                
                folder_path <- paste0("OncoGenAI/results/", job_id, "/")  # Define folder path
                genome_version <- job_data$Genome  # Extract genome version
                vcf_indels <- paste0(folder_path, job_id, "_indels.", genome_version, "_multianno.txt.grl_p")  # File path
                
                # Read the VCF indel data for this job ID
                indel_data <- read.csv(vcf_indels, header = TRUE, sep = "\t")
                
                # Check if required columns exist
                if (!all(c("Ref", "Alt") %in% colnames(indel_data))) {
                  next  # Skip this job ID if columns are missing
                }
                
                # Calculate insertion and deletion lengths
                indel_data <- indel_data %>%
                  mutate(
                    Insertion_Length = ifelse(nchar(Alt) > nchar(Ref), nchar(Alt) - nchar(Ref), NA),
                    Deletion_Length = ifelse(nchar(Ref) > nchar(Alt), nchar(Ref) - nchar(Alt), NA),
                    Job_ID = job_id  # Add Job ID column to identify each job
                  )
                
                indel_data_final<-indel_data[,c("Job_ID","Insertion_Length","Deletion_Length")]
                combined_data <- rbind(combined_data,indel_data_final)
                print(indel_data_final)
              }
              
              # The resulting 'indel_data' now contains calculated insertion and deletion lengths
              #print(head(indel_data))  # Optional: Print the first few rows for verification
              
              # Store the processed data for further use (e.g., plotting or displaying in a table)
              output$indel_analysis_data <- renderDataTable({
                datatable(indel_data_final, options = list(pageLength = 10))
              })

              output$indel_density_plot1 <- renderPlotly({
                req(combined_data)  # Ensures the data is available
                
                p1 <- ggplot(data=combined_data, aes(x=Insertion_Length, group=Job_ID, fill=Job_ID)) +
                geom_density(adjust=1.5, alpha = 0.5)
                p1


              })
              output$indel_density_plot2 <- renderPlotly({
                req(combined_data)  # Ensures the data is available
                

                p2 <- ggplot(data=combined_data, aes(x=Deletion_Length, group=Job_ID, fill=Job_ID)) +
                geom_density(adjust=1.5, alpha = 0.5)
                p2

              })
})

# indel analysis

  # Placeholder reactive data for indel analysis
  indel_data <- reactive({
    # Assuming you have a data source or function that fetches the indel data based on job IDs
    req(input$select_job_id_di4)  # Ensures a job is selected
    fetch_indel_data(input$select_job_id_di4)  # Replace with actual data-fetching function
  })
  
  # Plot output for indel length distribution

  
  # DataTable output for indel analysis data
  output$indel_analysis_data <- renderDataTable({
    req(indel_data())  # Ensure the data is available
    datatable(indel_data(), options = list(pageLength = 10))
  })
  
  # Handle plot download
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("indel_analysis_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Save the current plot as a PNG file
      ggsave(file, plot = last_plot(), device = "png", width = 10, height = 6)
    }
  )



# variant function plot
            observeEvent(input$select_job_id_di5, {
              job_id <- input$select_job_id_di5
              req(input$select_job_id_di5)
              job_data <- read_jobs_csv2()
              job_data <- job_data %>% filter(Job_ID == job_id)
              folder_path = paste0("OncoGenAI/results/", job_id, "/")  # Corrected 'job' to 'job_id'
              
              genome_version <- job_data$Genome  # Fixed variable assignment

              # Ensure the 'variant_type_d1' input is correctly referenced
              observe({
              if (input$Summariseplot_label == "snp") {
                  vcf_snp <- paste0(folder_path, job_id, "_snps.", genome_version, "_multianno.txt.grl_p")
                  data <- read.csv(vcf_snp, header = TRUE, sep = "\t")
                  freq_data <- as.data.frame(table(data[,input$genomic_feature]))
                  colnames(freq_data) <- c(input$genomic_feature, "Frequency")
                  output$distibution_data1 <- DT::renderDataTable({
                      req(input$Summariseplot_label,input$select_job_id_di5)  # req() checks if input is not NULL
                      freq_data
                      
                  }, rownames = FALSE, options = list(
                      scrollX = TRUE,
                      pageLength = 12,
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                  ), class = 'cell-border stripe')


                        output$dist_plot1 <- renderPlotly({
                            p <- ggplot(freq_data, aes(x = .data[[input$genomic_feature]], y = .data[["Frequency"]], fill = .data[[input$genomic_feature]])) +
                            geom_bar(stat = "identity") +
                            labs(x = input$genomic_feature, y = "Frequency") +
                            theme_minimal() +
                            theme(axis.text.x = element_text(angle = 45, hjust = 1))
                          
                          ggplotly(p)
                        })

              } 


              })
              observe({
                  if (input$Summariseplot_label == "indel") {
                  vcf_indels <- paste0( folder_path, job_id, "_indels.", genome_version, "_multianno.txt.grl_p")
                  data <- read.csv(vcf_indels, header = TRUE, sep = "\t")
                                    freq_data <- as.data.frame(table(data[,input$genomic_feature]))
                  colnames(freq_data) <- c(input$genomic_feature, "Frequency")
                  output$distibution_data1 <- DT::renderDataTable({
                      req(input$Summariseplot_label,input$select_job_id_di5)  # req() checks if input is not NULL
                      freq_data
                      
                  }, rownames = FALSE, options = list(
                      scrollX = TRUE,
                      pageLength = 12,
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                  ), class = 'cell-border stripe')


                        output$dist_plot1 <- renderPlotly({
                            p <- ggplot(freq_data, aes(x = .data[[input$genomic_feature]], y = .data[["Frequency"]], fill = .data[[input$genomic_feature]])) +
                            geom_bar(stat = "identity") +
                            labs(x = input$genomic_feature, y = "Frequency") +
                            theme_minimal() +
                            theme(axis.text.x = element_text(angle = 45, hjust = 1))
                          
                          ggplotly(p)
                        })
              } 
              }) 
            })

# gene distribution

          observeEvent(input$select_job_id_di6, {
              job_id <- input$select_job_id_di6
              req(input$select_job_id_di6)
              job_data <- read_jobs_csv2()
              job_data <- job_data %>% filter(Job_ID == job_id)
              folder_path = paste0("OncoGenAI/results/", job_id, "/")  # Corrected 'job' to 'job_id'
              
              genome_version <- job_data$Genome  # Fixed variable assignment
              print("hi")

              # Ensure the 'variant_type_d1' input is correctly referenced
              observe({
              if (input$Summariseplot_label1 == "snp") {
                  print("hi2")
                  vcf_snp <- paste0(folder_path, job_id, "_snps.", genome_version, "_multianno.txt.grl_p")
                  data <- read.csv(vcf_snp, header = TRUE, sep = "\t")
                  freq_data <- as.data.frame(table(data[,input$genomic_feature1]))
                  colnames(freq_data) <- c(input$genomic_feature1, "Frequency")
                  freq_data <- freq_data[order(-freq_data$Frequency), ][1:20, ]
                  output$distribution_data2 <- DT::renderDataTable({
                      print("hi3")
                      req(input$Summariseplot_label1,input$select_job_id_di6)  # req() checks if input is not NULL
                      freq_data
                      
                  }, rownames = FALSE, options = list(
                      scrollX = TRUE,
                      pageLength = 12,
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                  ), class = 'cell-border stripe')

                  freq_data[[input$genomic_feature1]] <- ifelse(nchar(as.character(freq_data[[input$genomic_feature1]])) > 10,
                                                                paste0(substr(as.character(freq_data[[input$genomic_feature1]]), 1, 10), "..."),
                                                                as.character(freq_data[[input$genomic_feature1]]))
                  freq_data[[input$genomic_feature1]] <- factor(freq_data[[input$genomic_feature1]], 
                                                                levels = freq_data[[input$genomic_feature1]][order(freq_data[["Frequency"]], decreasing = TRUE)])

                    output$dis_plot2 <- renderPlotly({
                        p <- ggplot(freq_data, aes(x = .data[[input$genomic_feature1]], y = .data[["Frequency"]], fill = .data[[input$genomic_feature1]])) +
                        geom_bar(stat = "identity") +
                        labs(x = input$genomic_feature1, y = "Frequency") +
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
                      
                      ggplotly(p)
                    })



              } 


              })
              observe({
                  if (input$Summariseplot_label1 == "indel") {
                  vcf_indels <- paste0( folder_path, job_id, "_indels.", genome_version, "_multianno.txt.grl_p")
                  data <- read.csv(vcf_indels, header = TRUE, sep = "\t")
                  freq_data <- as.data.frame(table(data[,input$genomic_feature1]))
                  colnames(freq_data) <- c(input$genomic_feature1, "Frequency")
                  freq_data <- freq_data[order(-freq_data$Frequency), ][1:20, ]
                  output$distribution_data2 <- DT::renderDataTable({
                      req(input$Summariseplot_label1,input$select_job_id_di6)  # req() checks if input is not NULL
                      freq_data
                      
                  }, rownames = FALSE, options = list(
                      scrollX = TRUE,
                      pageLength = 12,
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                  ), class = 'cell-border stripe')


                  freq_data[[input$genomic_feature1]] <- ifelse(nchar(as.character(freq_data[[input$genomic_feature1]])) > 10,
                                                                paste0(substr(as.character(freq_data[[input$genomic_feature1]]), 1, 10), "..."),
                                                                as.character(freq_data[[input$genomic_feature1]]))
                  freq_data[[input$genomic_feature1]] <- factor(freq_data[[input$genomic_feature1]], 
                                                                levels = freq_data[[input$genomic_feature1]][order(freq_data[["Frequency"]], decreasing = TRUE)])

                  output$dis_plot2 <- renderPlotly({
                      p <- ggplot(freq_data, aes(x = .data[[input$genomic_feature1]], y = .data[["Frequency"]], fill = .data[[input$genomic_feature1]])) +
                      geom_bar(stat = "identity") +
                      labs(x = input$genomic_feature1, y = "Frequency") +
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1))
                    
                    ggplotly(p)
                  })
              } 



              if (input$Summariseplot_label1 == "all") {
                    vcf_snp <- paste0(folder_path, job_id, "_snps.", genome_version, "_multianno.txt.grl_p")
                    vcf_indels <- paste0(folder_path, job_id, "_indels.", genome_version, "_multianno.txt.grl_p")
                    
                    # Read SNP and Indel data
                    data_snp <- read.csv(vcf_snp, header = TRUE, sep = "\t")
                    data_indel <- read.csv(vcf_indels, header = TRUE, sep = "\t")
                    
                    # Combine SNP and Indel data
                    combined_data <- rbind(data_snp, data_indel)
                    
                    # Calculate Frequency for Combined Data
                    freq_data <- as.data.frame(table(combined_data[, input$genomic_feature1]))
                    colnames(freq_data) <- c(input$genomic_feature1, "Frequency")
                    freq_data <- freq_data[order(-freq_data$Frequency), ][1:20, ]
                    # Render Combined Table
                    output$distribution_data2 <- DT::renderDataTable({
                      req(input$Summariseplot_label1, input$select_job_id_di6)
                      freq_data
                    }, rownames = FALSE, options = list(
                      scrollX = TRUE,
                      pageLength = 12,
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                    ), class = 'cell-border stripe')

                    freq_data[[input$genomic_feature1]] <- ifelse(nchar(as.character(freq_data[[input$genomic_feature1]])) > 10,
                                                                  paste0(substr(as.character(freq_data[[input$genomic_feature1]]), 1, 10), "..."),
                                                                  as.character(freq_data[[input$genomic_feature1]]))
                    freq_data[[input$genomic_feature1]] <- factor(freq_data[[input$genomic_feature1]], 
                                                                  levels = freq_data[[input$genomic_feature1]][order(freq_data[["Frequency"]], decreasing = TRUE)])

                    # Render Combined Plot
                    output$dis_plot2 <- renderPlotly({
                      p <- ggplot(freq_data, aes(x = .data[[input$genomic_feature1]], y = .data[["Frequency"]], fill = .data[[input$genomic_feature1]])) +
                        geom_bar(stat = "identity") +
                        labs(x = input$genomic_feature1, y = "Frequency") +
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
                      
                      ggplotly(p)
                    })
                  }




              }) 
            })


  #-------------------------------------------------------------------------------------------------------------
  #------------------------------ workflow button--------------------------------------------------------------
  observeEvent(input$tour_button, {


    if (input$tabs2=="dashboard") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu", intro = "Sidebar shows the diferent tabs in the app. Click on the tab name to navigate to a particular tab."),
        list(element = ".sidebar-menu li a[data-value='dashboard']", intro = "This is the Dashboard menu item. Click here to navigate to the Dashboard."),
        # list(element = "#dashboard_box1", intro = "Shows the number of jobs run on the app till date."),
        # list(element = "#dashboard_box2", intro = "Shows the average number of jobs run on the app till date."),
        # list(element = "#dashboard_box3", intro = "Average time taken to complete a job."),
        list(element = "#dashboard_box4", intro = "Monthly submission of a jobs"),
        list(element = "#prevYear", intro = "Click on the button to view the previous monthly submissions graph."),
        list(element = "#nextYear", intro = "Click on the button to view the next monthly submissions graph."),
        list(element = "#dashboard_box5", intro = "Total number of different types of workflow used for the analysis"),
        list(element = "#dashboard_box6", intro = "Show all the latest jobs submitted."),
        list(element = "#tour_button", intro = "Show the information of the current view in the app."),
        # list(element = "#settings", intro = "Latest jobs submitted."),
        list(element = "#logout", intro = "Logout of the app")
      )
    ))
    }
    else if (input$tabs2=="fastqqualityreport") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='fastqqualityreport']", intro = "This is the Fastq Quality Check tab. Click here to navigate to for checking the quality offastq files"),
        list(element = "#fastq_file_check", intro = "Browse your FASTQ files"),
        list(element = "#submit_fastq_check", intro = "Click the button to Submit the file"),
        list(element = "#previous_reports_dropdown", intro = "View reports of previous quality checks."),
        list(element = "#fastqc_refresh", intro = "Refresh the page to get latest updates") 
      )
    ))
    }
    else if (input$tabs2=="newjob") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='newjob']", intro = "Tab to sumit a new job."),
        list(element = "#newjob_box1", intro = "1. Patient ID:A unique identifier assigned to the patient for anonymization.<br/>
2. Patient Details: Additional information about the patient (e.g., age, diagnosis).<br/>
3. Submitting Author: The person responsible for submitting the data/sample for analysis.<br/>
4. Cancer Type: The specific type of cancer diagnosed (e.g., lung, breast).<br/>
5. Tumor Percentage: The proportion of tumor cells in the sample, used for analysis"),
        list(element = "#newjob_box2", intro = "1. Workflow Selection: Choose between Illumina or IonTorrent sequencing workflows.<br/>
2. CNV analysis: Indicate whether Copy Number Variation (CNV) analysis is performed (Yes/No).<br/>
3. Adaptor Trimming: Specify if adaptor trimming is required (Yes/No).<br/>
4. Genome: Select the reference genome used, either GRCh38 (hg38) or GRCh37 (hg19).<br/>
5. Mutations: Define whether the call Somatic or Germline mutations.<br/>
6. Bed File: Input the BED file specifying genomic regions of interest.<br/>
"),
        list(element = "#fastq_file", intro = "Select Fastq files to run the analysis"),
        list(element = "#submit_job", intro = "Sumbmit the job. After clicking this check the parameter and click of confirm submission to start the job."),
        list(element = "#test_data", intro = "Fill out example inputs in the new job tab")
       
      )
    ))
    }
    else if (input$tabs2=="runningjobs") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='runningjobs']", intro = "Tab to view running jobs."),
        list(element = "#run_box1", intro = "This section shows details of running job."),
        list(element = "#run_box2", intro = "This section shows details of Completed job."),
        list(element = "#run_box3", intro = "This section shows jobs that are aborted."),
        list(element = "#Refresh", intro = "Refresh the Running jobs tab.")
       
      )
    ))
    }
    else if (input$tabs2=="filterationreport") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='filterationreport']", intro = "Tab to do variant filtration on the basis of quality filters."),
        list(element = "#filterationreport_box1", intro = "This is the text input field where you can enter the unique patient id that will will be displayed in the entire workflow."),
        list(element = "#filter_type", intro = "Choose a filtering mode (3 mode filtering High low and mediam filtration"),
        list(element = "#update_filters", intro = "Update given filters and updates the plots."),
        list(element = "#Save_filters", intro = "Save filters and update the filters"),
        list(element = "#results_view", intro = "Click on the Button to go to the next tab for clinical anlysis.")
        
       
      )
    ))
    }
    else if (input$tabs2=="result1") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='result1']", intro = "Tab to see plots summary of the variants."),
        list(element = "#result1_box1", intro = "Select the patient ID."),
        list(element = "#result1_box2", intro = "VCF summary: Displays a concise overview of the variants identified in the VCF file, including counts of mutations and their clinical significance."),
        list(element = "#view_table", intro = "View the main table in the Tabular page.")
      )
    ))
    }
    else if (input$tabs2=="result2") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='result2']", intro = "Tab to see clinical interpretations of variants."),
        list(element = "#result2_box1", intro = "Select the patient id"),
        list(element = "#result2_box2", intro = "Plots showing variants by ACMG creteria"),
        list(element = "#view_r3", intro = "Navigate to tabular view tab"),
        list(element = "#result2_box3", intro = "Table showing pathogenic variants.")
       
      )
    ))
    }
    else if (input$tabs2=="result3") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='result3']", intro = "Tabular view of variants."),
        list(element = "#result3_box1", intro = "Select the patient id."),
        list(element = "#select_column1", intro = "Filter columns to display relevant information."),
        list(element = "#tab_filter", intro = "Opens the menu for filtering variants. Choose, apply, and add custom filters."),
        list(element = "#tab_remove", intro = "Remove all applied filters."),
        list(element = "#save_button", intro = "Select the rows and click this button to save the variants for the KG query."),
        list(element = "#query_kg_button", intro = "After saving the query variants, click to navigate to the KG query section and start the query.")
    )
    ))
    }
    else if (input$tabs2=="result4") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='result4']", intro = "Tab to view CNV Analysis."),
        list(element = "#result4_box1", intro = "Select the patient ID."),
        list(element = "#result4_box2", intro = "Copy Number Segment: This section provides an overview of segmented copy number data for analysis."),
        list(element = "#result4_box3", intro = "Copy Number Ratio: Shows the copy number ratios across different genomic regions."),
        list(element = "#result4_box4", intro = "Classify CNV Scoresheet: Evaluate and categorize CNV based on predefined scoring criteria."),
        list(element = "#select_column_cnv", intro = "Filter columns to display relevant information."),
        list(element = "#tmb_button", intro = "Click to navigate to the TMB tab for tumor mutational burden analysis.")
    )

    ))
    }
    else if (input$tabs2=="genome_browser") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='genome_browser']", intro = "Navigate to the Genome Browser tab."),
        list(element = "#igvShiny_0", intro = "Interactive IGV viewer for visualizing genomic data."),
        list(element = "#removeUserTracksButton", intro = "Remove selected tracks from the IGV viewer."),
        list(element = "#roi", intro = "Enter a genomic position to navigate directly within the viewer."),
        list(element = "#searchButton", intro = "Click to search and display the specified genomic position and variant.")
    )
    ))
    }
    else if (input$tabs2=="kgimputation") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='kgimputation']", intro = "Navigate to the KG Imputation tab."),
        list(element = "#kg_box1", intro = "Enter the unique patient ID to display it throughout the workflow."),
        list(element = "#kg_box2", intro = "View the KG as a graph for visualizing variant information."),
        list(element = "#kg_box3", intro = "Display the KG table with all query results."),
        list(element = "#save_row", intro = "Select and save rows for report generation."),
        list(element = "#report_generation_button", intro = "Navigate to the Report Generation tab to create a report.")
    )

    ))
    }
    else if (input$tabs2=="reportgeneration") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='reportgeneration']", intro = "Navigate to the Report Generation tab."),
        list(element = "#report_box1", intro = "Enter the unique patient ID to display it throughout the workflow."),
        list(element = "#file_content", intro = "Edit the prompt to customize the report content."),
        list(element = "#save", intro = "Save the edited prompt."),
        list(element = "#generate_report", intro = "Click here to generate the report after editing and saving the prompt."),
        list(element = "#report_content", intro = "Display the generated report."),
        list(element = "#save_report", intro = "Save the generated report.")
    )

    ))
    }
    else if (input$tabs2=="tmbscore") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='tmbscore']", intro = "Navigate to the TMB Score tab."),
        list(element = "#tmb_box1", intro = "Enter the unique patient ID, which will be displayed throughout the workflow."),
        list(element = "#vf_value1", intro = "Enter the VF (Variant Frequency) value for TMB calculation."),
        list(element = "#panel_box", intro = "Select the panel for analysis."),
        list(element = "#tmb_box2", intro = "Display the calculated TMB score.")
    )

    ))
    }
    else if (input$tabs2=="bugsubmission") {
    introjs(session, options = list(
      steps = list(
        list(element = ".sidebar-menu li a[data-value='bugsubmission']", intro = "Navigate to the Bug Submission tab."),
        list(element = "#screenshotDiv", intro = "Upload an image showing the bug."),
        list(element = "#bug_description", intro = "Provide a detailed description of the bug."),
        list(element = "#submit_bug", intro = "Click to submit the bug report.")
    )
    ))
    }
  })
  #--------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------
  
  observeEvent(input$login, {
    if (input$username == "oncogen" && input$password == "oncogen") {
      login_status(TRUE)
      
    } else {
      shinyjs::alert("Invalid username or password")
    }
  })
  
  login_status <- reactiveVal(FALSE)
  
  output$page <- renderUI({
    if (login_status()) {
      shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")
    }
    else{
      # fluidPage(
        tags$div(
          tags$div(class = "login-page",
                 tags$img(src = "IIITDLogo2.png", class = "logo-left"),
                 tags$img(src = "R.png", class = "logo-right"),
                 tags$img(src = "oncogen-logo2.png", class = "logo-center"),
                #  tags$div(class = "typewriter", h1("Welcome To OncoGen.AI!")),
                 tags$div(class = "login-box",
                          tags$h3("OncoGen.ai Login"),
                          textInput("username", "Username"),
                          passwordInput("password", "Password"),
                          actionButton("login", "Login", class = "btn-primary"),
                          textOutput("loginMessage")
                 )
        )
      )
      # )
      
    }
  })
  
  
  observeEvent(input$logout, {
    login_status(FALSE)
    shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")
  })
  
########################################### dashboard   ##################################
  
  
  
  observe({
    if(input$tabs2=="dashboard"){
      refresh_data()
      
    }
  })
  
   observe({if(login_status()){
        # Plot for number of submissions by month
    output$submissionsPlot <- renderPlotly({
      
      req(rv$data,rv$currentYear)
      
      data <- rv$data %>%
        mutate(Date = ymd(Date),
              YearMonth = floor_date(Date, "month"),
              Year = year(Date))

      # Count frequencies of each month
      month_counts <- table(data$Month_x)
      
      data <- data %>%
        filter(Year == rv$currentYear) ## to be added in the final code
      
      
      data$Date_x <- as.Date(data$Date, format = "%d.%m.%Y")
      
      # Extract month from Date column
      data$Month_x <- lubridate::month(data$Date_x, label = TRUE, abbr = FALSE)
      
      # Count frequencies of each month
      month_counts <- table(data$Month_x)
      
      
      # Plot the data using Plotly
      plot <- plot_ly(x = factor(names(month_counts), levels = month.name), y = as.numeric(month_counts), type = "bar",
                      name = 'Jobs') %>%
        layout(title = paste('Number of Jobs Per Month in', rv$currentYear), ### to be added in final code
              xaxis = list(title = 'Month'),
              yaxis = list(title = 'Number of Jobs', range = c(0, 20)  ))
      
    })
    
    
    
    # Plot for workflow distribution
    output$workflowPieChart <- renderPlotly({
      data_workflow <- rv$data %>%
        group_by(Workflow) %>%
        summarise(Count = n())

        colors <- c("#33806c", "#803347")
      
      plot_ly(data_workflow, labels = ~Workflow, values = ~Count, type = 'pie', marker = list(colors = colors)) %>%
        layout(title = 'Workflow Distribution')
    })
    
    # Interactive table
    output$submissionTable <- renderDT({
      data = rv$data
      # data = data[order(-data$Job_ID), ]
      data <- data %>%
        arrange(desc(Job_ID))
      datatable(data[,c("Job_ID","Patient_ID","Patient.Details","Submitting.Author","Workflow","Date"),drop=F], selection = 'single' , options = list(pageLength = 5, searching = TRUE, ordering= F, scrollX = TRUE, columnDefs = list(
        list(className = 'dt-center', targets = "_all")  # Center align text in all columns
      )))
    })
    observeEvent(input$submissionTable_rows_selected, {
  selected_row <- input$submissionTable_rows_selected  # Get the selected row index
  
  if (length(selected_row)) {  # Check if a row is selected
  sorted_data <- rv$data %>%
      arrange(desc(Job_ID))
    selected_job_id <- sorted_data[selected_row, "Job_ID"]  # Get the Job_ID of the selected row
    selected_patient_id <- sorted_data[selected_row, "Patient_ID"]
    # print(paste(selected_patient_id, "-", selected_job_id, sep = "")) 
    selected_ids <- paste(selected_patient_id, "-", selected_job_id, sep = "")
    print(selected_ids) # Print Job_ID to console (or use in other operations)

    # updateTabItems(session, "tabs2", selected = "filterationreport")
    # updateSelectInput(session, "select_job_id_filter", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected =input$select_job_id_r3 )
  updateTabItems(session, "tabs2", selected = "filterationreport")
    
    # Update the dropdown in the "filterationreport" tab
    updateSelectInput(session, "select_job_id_filter", choices = setNames(sorted_data$Job_ID, paste(sorted_data$Patient_ID, sorted_data$Job_ID, sep = "-")), selected = selected_job_id)
  }
})
   }else{

            # Plot for number of submissions by month
      output$submissionsPlot <- renderPlotly({
        return(NULL)
      })
      
      
      
      # Plot for workflow distribution
      output$workflowPieChart <- renderPlotly({
        return(NULL)
      })
      
      # Interactive table
      output$submissionTable <- renderDT({
        return(NULL) 
      })
      

   }
  
})
  
  
  
  
  
#########################################################################################################  

  
  ###################################### fastq quality check #######################


  roots <- c(wd = '.')
  shinyFileChoose(input, 'fastq_file_check', roots = roots, filetypes = c('', 'fastq', 'gz', 'fq','bam'))

  # Define the path to the CSV file
  csv_path <- file.path(roots, "fastqc_results.csv")
  
  # Function to check or create the CSV file
  check_or_create_csv <- function(csv_path) {
    if (!file.exists(csv_path)) {
      write.csv(data.frame(fastq_file_name = character(),
                           data_path = character(),
                           html_path = character()),
                csv_path, row.names = FALSE)
    }
  }
  
  check_or_create_csv(csv_path)
  
  # Function to update the progress bar
  updateProgressBar <- function(filename = NULL, show = TRUE) {
    if (show && !is.null(filename)) {
      return(
        tagList(
          div(style = "height: 30px; width: 40%; background-color: #f3f3f3; border-radius: 5px; overflow: hidden; position: relative;",
              div(style = "height: 100%; width: 100%; background-color: #4caf50; position: absolute; text-align: center; line-height: 30px; color: white;",
                  "File selected: ", filename
              )
          )
        )
      )
    } else {
      return(
        tagList(
          div(style = "height: 30px; width: 40%; background-color: #f3f3f3; border-radius: 5px; overflow: hidden; position: relative;",
              div(style = "height: 100%; width: 1%; background-color: #4caf50; position: absolute; text-align: center; line-height: 30px; color: white;",
                  "No file selected"
              )
          )
        )
      )
    }
  }
  
  
  
  # Render the progress bar UI
  output$progressBar1 <- renderUI({
    req(input$fastq_file_check)
    fileinfo1 <- parseFilePaths(roots, input$fastq_file_check)
    
    if (nrow(fileinfo1) > 0) {
      updateProgressBar(filename = fileinfo1$name)
    } else {
      updateProgressBar(show = FALSE)
    }
  })
  
  # Update the dropdown with previous FASTQ files
  output$previous_reports_dropdown <- renderUI({
    if (file.exists(csv_path)) {
      csv_data <- read.csv(csv_path)
      selectInput("previous_report", "Select a FASTQ file to view the report:",
                  choices = c("", csv_data$fastq_file_name),
                  selected = NULL,
                  width = "20%")  # No file selected initially
    } else {
      NULL
    }
  })
  
  # Render the FastQC report when a previous report is selected
  observeEvent(input$previous_report, {
    req(input$previous_report != "")  # Ensure that a valid selection is made
    csv_data <- read.csv(csv_path)
    print(csv_data$fastq_file_name )
    print(csv_data)
    selected_html_path <- csv_data$html_path[csv_data$fastq_file_name == input$previous_report]
    print(selected_html_path)
    if (file.exists(selected_html_path)) {
      # Modify the path to include the desired URL prefix and replace "/" with "%2F"

      base_url <- "http://192.168.51.235:1234"
      
      # Define the file path you want to convert
      selected_html_path<-basename(selected_html_path)
      file_path <- paste0("/fastq_results/",selected_html_path)
      print(file_path)
      
      # Replace slashes with %2F to make it URL-safe
      #url_safe_path <- gsub("/", "%2F", file_path)
      
      # Construct the full URL
      full_url <- paste0(base_url, file_path)

      print(full_url)
      # Modify the path to include the desired URL prefix and replace "/" with "%2F"
      
      
      output$fastqc_report_ui <- renderUI({
        tags$iframe(src = full_url, height = "800px", width = "100%", style = "border:none;")
      })
    }
  })
  


  
  last_line <- reactive({
    invalidateLater(10000, session)  # Re-evaluate every 10 seconds
    
    output_dir <- "fastq_results"
    fastq_file_df <- parseFilePaths(roots, input$fastq_file_check)
    
    file_path = paste0(output_dir,"/",fastq_file_df$name[1],".log")
    
    # Check if the file exists
    if (file.exists(file_path)) {
      # Read the file
      lines <- readLines(file_path, warn = FALSE)
      
      # Return the last line if the file has content
      if (length(lines) > 0) {
        return(tail(lines, 1))
      } else {
        return("Please wait ...") 
      }
    } else {
      return("Please wait ...")
    }
  })
  
observeEvent(input$submit_fastq_check, {

  fastq_file_df <- parseFilePaths(roots, input$fastq_file_check)
  
  # Show alert if no FASTQ file is selected
  if (is.null(fastq_file_df$name) || nrow(fastq_file_df) == 0 || fastq_file_df$name[1] == "") {
    shinyjs::alert("No file chosen.")
    return()  # Exit the function if no file is selected
  }
  
        # in server
        if (file.exists(csv_path)) {
          csv_data <- read.csv(csv_path)
        } else {
          csv_data <- data.frame(fastq_file_name = character(), data_path = character(), html_path = character())
          # shinyjs::alert("No fatq file selected.")
        }
        
        
        fastq_file_df <- parseFilePaths(roots, input$fastq_file_check)

        if (fastq_file_df$name[1] %in% csv_data$fastq_file_name) {
          # showModal(modalDialog(title = "File Already Processed", "The selected FASTQ file has already been processed.", easyClose = TRUE, footer = NULL))
          shinyjs::alert("The selectd FASTQ file has already been processed.")
          return()
        }else{
          #showModal(modalDialog(title = "Running FastQC", "", easyClose = TRUE, footer = NULL))
          #show_modal_progress_line() # show the modal window
          #update_modal_progress(0.2) # update progress bar value
          
          # Save the FASTQ file name and path to the CSV file after rendering the UI
          new_entry <- data.frame(fastq_file_name = fastq_file_df$name,
                                  data_path = fastq_file_df$datapath,
                                  html_path = "")
          write.table(new_entry, csv_path, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
          

          
          # Create output directory if it doesn't exist
          output_dir <- "fastq_results"
          if (!dir.exists(output_dir)) {
            dir.create(output_dir)
          }
          
          # Run the FastQC command
          fastqc_run_cmd <- paste0("nohup bash ", "fastq_quality.sh ", fastq_file_df$datapath[1]," > ",output_dir,"/",fastq_file_df$name[1],".log 2>&1 &")
          
          # Remove .fastq.gz, .fq.gz, .fastq, or .fq extensions
          name_without_extension <- gsub("\\.fastq\\.gz$|\\.fq\\.gz$|\\.fastq$|\\.fq$", "", fastq_file_df$name[1])
          
          # Append the _fastqc.html suffix
          html_report_name <- paste0(name_without_extension, "_fastqc.html")
          
          html_report_path <- file.path(output_dir, html_report_name)

          csv_data <- read.csv(csv_path)
          
          csv_data$html_path[csv_data$fastq_file_name == fastq_file_df$name] <- html_report_path
          write.csv(csv_data, csv_path, row.names = FALSE)
          
          # Use system command to get output in real time
          system(fastqc_run_cmd)
          #remove_modal_progress() 
          

          
          # Render the last line in the UI
          output$last_line <- renderText({
            last_line()
          })
          
          
        }

       
})



observeEvent(input$fastqc_refresh,{
  csv_data <- read.csv(csv_path)
  
  # Update the dropdown choices after adding a new report
  updateSelectInput(session, "previous_report", choices = c("", csv_data$fastq_file_name))
  
  # Reset the selection to display the new report
  #updateSelectInput(session, "previous_report", selected = fastq_file_df$name)
  
})
  
  

  
  
  
  ################################################################################
  
  
  
  
  
  ######################### new job tab ################################
  required_fields_filled <- reactive({
    all(
      nchar(input$patient_id) > 0,
      nchar(input$submitting_author) > 0,
      nchar(input$workflow) > 0,
      nchar(input$adaptor_trimming) > 0,
      nchar(input$genome) > 0,
      nchar(input$mutations) > 0,
      nchar(input$bed_files) > 0,
      nchar(input$cancer_type) > 0,  # Check cancer type
      nchar(input$tumor_percent) > 0,  # Check tumor percentage
      nchar(input$cnv_analysis) > 0,  # Check CNV analysis
      nrow(parseFilePaths(roots, input$fastq_file)) > 0
    )
  })
  
  output$progressBar <- renderUI({
    req(input$fastq_file)
    fileinfo <- parseFilePaths(roots, input$fastq_file)
    
    if(nrow(fileinfo) >0){
      tagList(
        div(style = "height: 30px; width: 20%; background-color: #f3f3f3; border-radius: 5px; overflow: hidden; position: relative;",
            div(style = "height: 100%; width: 100%; background-color: #4caf50; position: absolute; text-align: center; line-height: 30px; color: white;",
                "completed"
            )
        )
      )
    }else{
      tagList(
        div(style = "height: 30px; width: 20%; background-color: #f3f3f3; border-radius: 5px; overflow: hidden; position: relative;",
            div(style = "height: 100%; width: 1%; background-color: #4caf50; position: absolute; text-align: center; line-height: 30px; color: white;",
                "No file selected"
            )
        )
      )
    }
    
  })
  
  observeEvent(input$fastq_file,{
    output$files <- renderTable(parseFilePaths(roots, input$fastq_file))
    #print(nrow(parseFilePaths(roots, input$fastq_file)) )
  })
  
  
  
  # Enable or disable the submit button based on the required fields
  observe({
    shinyjs::toggleState("submit_job", condition = required_fields_filled())
  })
  
  
  
  # Display confirmation UI below the submit button with spacing
  observeEvent(input$submit_job, {
    output$confirmation_ui <- renderUI({
      fluidRow(
        column(width = 12,
               tags$div(style = "margin-top: 20px;",  # Adjust the margin-top as needed
                        box(
                          title = "Confirm Submission", status = "primary", solidHeader = F, width = 12,
                          tableOutput("summary_output"),
                          actionButton("confirm_submit", "Confirm", class = "btn btn-primary"),
                          actionButton("cancel_submit", "Cancel", class = "btn btn-default")
                        )
               )
        )
      )
    })
    
    output$summary_output <- renderTable({
      
      file_path_df = parseFilePaths(roots, input$fastq_file)
      
      # Define the CSV file path
      log_file_path <- "OncoGenAI/running_jobs.csv"
      
      # Read the CSV file into a data frame
      log_data <- read.csv(log_file_path)
      
      # Convert the Date column to Date type with the specified format
      log_data$Date <- as.Date(log_data$Date, format = "%Y.%m.%d")
      
      # Filter for today's date
      today_data <- log_data %>% filter(Date == Sys.Date())
      
      id_number <- nrow(today_data)+1  # Start with 1 or any other number
      generated_id <- generate_id(id_number)
      
      
      
      # data.frame(
      #   Field = c("Job ID",      "Patient ID",      "Patient Details",      "Submitting Author", "Workflow",       "Adaptor Trimming",      "Genome",    "Mutations",    "Bed File",         "Uploaded Files"),
      #   Value = c(generated_id, input$patient_id, input$patient_details, input$submitting_author, input$workflow, input$adaptor_trimming, input$genome, input$mutations, input$bed_file, paste(file_path_df$name, collapse = ", "))
      # )
      data.frame(
        Field = c("Job ID",      "Patient ID", "   Patient Details",       "Submitting Author",    "Workflow",     "Adaptor Trimming",     "Genome",     "Mutations",     "Bed File",       "Cancer Type",   "Tumor Percentage",   "CNV Analysis"),
        Value = c(generated_id, input$patient_id, input$patient_details, input$submitting_author, input$workflow, input$adaptor_trimming, input$genome, input$mutations, input$bed_files, input$cancer_type, input$tumor_percent, input$cnv_analysis)
      ) 
      
    })
    
    shinyjs::disable("submit_job")
  })
  
  
  observeEvent(input$fastq_file,{
    
    file_path_df = parseFilePaths(roots, input$fastq_file)
    
    input$file
    
  } )
  
  roots = c(wd='.')
  # shinyFileChoose(input, 'fastq_file', root=c(root='.'), filetypes=c('', 'sh','gz'))
  shinyFileChoose(input, 'fastq_file', roots=roots, filetypes=c('', 'fastq','gz','fq','bam'))
  
  # Handle confirmation
  observeEvent(input$confirm_submit, {
    
    
    file_path_df = parseFilePaths(roots, input$fastq_file)
    
    # Define the CSV file path
    running_job_path <- "OncoGenAI/running_jobs.csv"
    
    # Read the CSV file into a data frame
    running_job_data <- read.csv(running_job_path)
    
    # Convert the Date column to Date type with the specified format
    running_job_data$Date <- as.Date(running_job_data$Date, format = "%Y.%m.%d")
    
    # Filter for today's date
    today_data <- running_job_data %>% filter(Date == Sys.Date())
    
    id_number <- nrow(today_data)+1  # Start with 1 or any other number
    
    generated_id <- generate_id(id_number)
    
    file_path_df = parseFilePaths(roots, input$fastq_file)
    
    genome_version = ""
    
    if(input$genome == "GRCh38 (hg38)"){
      genome_version= "hg38"
    }else if(input$genome == "GRCh37 (hg19)"){
      genome_version= "hg19"
    }else{
      print(genome_version)
    }
    # "fastq_to_vcf.sh -f1 ./SRR741412/SRR741412_1.fastq -f2 ./SRR741412/SRR741412_2.fastq -g hg38 -s Oncogen202410110001 -a Germline -b  -t Illumina -p 0"
    print(genome_version)
    print(input$bed_files)
    # Replace with your script path
    if(input$workflow == "IonTorrent"){
      if (length(file_path_df$datapath) == 1){
        script_run_cmd <- paste0("bam_to_vcf.sh"," -f ",file_path_df$datapath[1]," -g ",genome_version," -s ",generated_id," -a ",input$mutations," -b ",input$bed_files," -t ",input$workflow," -p ",0," -trim ",input$adaptor_trimming," -cnv ",input$cnv_analysis," -tumor ",input$tumor_percent)
      }else{
        print("Please give single Bam file at a time")
      }
    }else{
      if (length(file_path_df$datapath) > 1){
        script_run_cmd <- paste0("fastq_to_vcf.sh"," -f1 ",file_path_df$datapath[1]," -f2 ",file_path_df$datapath[2]," -g ",genome_version," -s ",generated_id," -a ",input$mutations," -b ",input$bed_files," -t ", input$workflow," -p ",0," -trim ",input$adaptor_trimming," -cnv ",input$cnv_analysis)
      }else{  
        script_run_cmd <- paste0("fastq_to_vcf_SE.sh"," -f1 ",file_path_df$datapath[1]," -g ",genome_version," -s ",generated_id," -a ",input$mutations," -b ",input$bed_files," -t ",input$workflow," -p ",0," -trim ",input$adaptor_trimming," -cnv ",input$cnv_analysis)
      }

    }

    
    #echo "Usage: $0 -f1 <file_path1> -f2 <file_path2> -g <genome> -s <output_prefix>"
    log_path <- paste0("OncoGenAI/results/",generated_id, "_logfile.txt")
    # Run the Bash script with nohup
    print(script_run_cmd)
    
    
    date_str <- format(Sys.Date(), "%Y.%m.%d")
    
    
    # new_row <- c(generated_id,  #Job_ID
    # input$patient_id, #Patient_ID
    #  input$patient_details,  #Patient.Details
    #  input$submitting_author, # Submitting.Author
    #   input$workflow, #Workflow
    #   input$adaptor_trimming, #Adaptor.Trimming
    #   input$sub,genome_version,  #Genome
    #   input$mutations,  #Mutations
    #   input$bed_file,  # Bed.File
    #    input$cancer_type,  #Cancer.Type
    #    input$tumor_percent, # Tumor.Percentage
    #     input$cnv_analysis, # CNV.analysis
    #     paste(file_path_df$datapath,collapse = "\t"),  # Uploaded.Files
    #     "Running", # Status
    #     date_str, # Date
    #     "",#pid
    #     0)# Progress
    new_row <- c(generated_id,  #Job_ID
        input$patient_id, #Patient_ID
        input$patient_details,  #Patient.Details
        input$submitting_author, # Submitting.Author
        input$workflow, #Workflow
        input$adaptor_trimming, #Adaptor.Trimming
        genome_version,  #Genome
        input$mutations,  #Mutations
        input$bed_files,  #Bed.File
        input$cancer_type,  #Cancer.Type
        input$tumor_percent, # Tumor.Percentage
        input$cnv_analysis, # CNV.analysis
        paste(file_path_df$datapath, collapse = "\t"),  # Uploaded.Files
        "Running", # Status
        date_str, # Date
        "",#pid
        0) # Progress

    print(new_row)

    print(length(new_row))

    # Read the existing data
    existing_data <- read.csv(running_job_path, stringsAsFactors = FALSE)
    
    # Convert the new row to a data frame and ensure column names match
    new_data <- data.frame(new_row, stringsAsFactors = FALSE)
    new_data <- t(new_data)
    print(colnames(existing_data))
    colnames(new_data) <- colnames(existing_data)
    
    # Append the new row to the existing data
    updated_data <- rbind(existing_data, new_data)
    # Write the updated data back to the CSV file
    write.csv(updated_data, running_job_path, row.names = FALSE)
    
    output$confirmation_ui <- renderUI(NULL)  # Clear confirmation UI
    system(sprintf("nohup bash %s > %s  2>&1 &", script_run_cmd,log_path), intern = TRUE)
    shinyjs::enable("submit_job")
    shinyjs::alert("Job submitted successfully!")
    refresh_data()
  })
  
  # Handle cancellation
  observeEvent(input$cancel_submit, {
    output$confirmation_ui <- renderUI(NULL)  # Clear confirmation UI
    shinyjs::enable("submit_job")
  })
  


  # Observe changes in the "Try our test data" checkbox
  observeEvent(input$test_data, {
    if (input$test_data) {
      updateTextInput(session, "patient_id", value = "SAMITA")
      updateTextInput(session, "patient_details", value = "Samita")
      updateTextInput(session, "submitting_author", value = "Sanjay")
      updateSelectInput(session, "workflow", selected = "Illumina")
      updateSelectInput(session, "adaptor_trimming", selected = "No")
      updateSelectInput(session, "genome", selected = "GRCh38 (hg38)")
      updateSelectInput(session, "mutations", selected = "Germline")
      updateSelectInput(session, "bed_files", selected = "primary_targets_hg38.bed")
      updatePickerInput(session, "cancer_type", selected = "Breast Carcinoma")
      updateTextInput(session, "tumor_percent", value = "50%")
      updatePickerInput(session, "cnv_analysis", selected = "Yes")
    } else {
      # Optionally, clear fields if checkbox is unchecked
      updateTextInput(session, "patient_id", value = "")
      updateTextInput(session, "patient_details", value = "")
      updateTextInput(session, "submitting_author", value = "")
      updateSelectInput(session, "workflow", selected = "")
      updateSelectInput(session, "adaptor_trimming", selected = "")
      updateSelectInput(session, "genome", selected = "")
      updateSelectInput(session, "mutations", selected = "")
      updateSelectInput(session, "bed_files", selected = "")
      updatePickerInput(session, "cancer_type", selected = NULL)
      updateTextInput(session, "tumor_percent", value = "")
      updatePickerInput(session, "cnv_analysis", selected = NULL)
    }
  })
  
  
  
  
  ##############################################################################
  
  
  ##############################  running job ####################################
  
  
  # Reactive value to hold the data
  rv <- reactiveValues(data = read_jobs_csv(),raw_variants_list =list(), currentYear = year(Sys.Date()),merged_data_all = data.frame(), pathogenic_data = data.frame(),report_data = data.frame(), filtered_data_all=data.frame(),selected_filters=list())
  
  observeEvent(input$prevYear, {
    rv$currentYear <- rv$currentYear - 1
  })
  
  observeEvent(input$nextYear, {
    rv$currentYear <- rv$currentYear + 1
  })
  
  
  
  # Function to refresh the data from the CSV file
  refresh_data <- function() {
    rv$data <- read_jobs_csv()
    rv$data <- rv$data %>%
          mutate(Progress = case_when(
            Progress == 1 ~ "15%",
            Progress == 2 ~ "30%",
            Progress == 3 ~ "45%",
            Progress == 4 ~ "60%",
            Progress == 5 ~ "70%",
            Progress == 6 ~ "85%",
            Progress == 7 ~ "100%",
            TRUE ~ as.character(Progress)  # Default case if other values are present
          ))
  }
  
  # Render DataTables when the relevant tab is selected
  observe({
    if (input$tabs2 == "runningjobs") {
      
      
      
      #print("OK")
      output$running_jobs_table <- renderDT({
        data =rv$data %>% filter(Status == "Running") 
        #data = data[,c("Job_ID","Patient_ID","Submitting.Author","Workflow","Date","Progress"),drop=F]
        
        datatable(
          data %>%
            mutate(
              Abort = paste0('<button class="btn btn-danger abort-btn" data-job-id="', Job_ID, '">Abort</button>')
            ),
          escape = FALSE,
          rownames = FALSE,
          options = list(pageLength = 5, scrollX = TRUE)
        )
      }, server = FALSE)
      # } else if (input$tabs2 == "runningjobs") {
      output$completed_jobs_table <- renderDT({
        datatable(
          rv$data %>% filter(Status == "Completed") %>%
            mutate(
              View = paste0('<button class="btn btn-info view-btn" data-job-id="', Job_ID, '">View</button>')
            ),
          escape = FALSE,
          rownames = FALSE,
          options = list(pageLength = 5, scrollX = TRUE, ordering = F)
        )
      }, server = FALSE)
      # } else if (input$tabs2 == "runningjobs") {
      output$aborted_jobs_table <- renderDT({
        datatable(
          rv$data %>% filter(Status == "Aborted") %>%
            mutate(
              Resume = paste0('<button class="btn btn-success resume-btn" data-job-id="', Job_ID, '">Resume</button>'),
              Delete = paste0('<button class="btn btn-danger delete-btn" data-job-id="', Job_ID, '">Delete</button>')
            ),
          escape = FALSE,
          rownames = FALSE,
          options = list(pageLength = 5, scrollX = TRUE, ordering = F)
        )
      }, server = FALSE)
    }
  })
  
  observe({
    if(input$tabs2=="runningjobs"){
      refresh_data()
    }
  })
  
  
  # Handle button clicks
  observeEvent(input$abort_job_id, {
    job_id <- input$abort_job_id
    job_data <- read_jobs_csv2()
    job_data <- job_data %>%
      mutate(Status = ifelse(Job_ID == job_id, "Aborted", Status))
    update_jobs_csv(job_data)
    refresh_data()
    output$running_jobs_table <- renderDT({
      datatable(
        rv$data %>% filter(Status == "Running") %>% 
          mutate(
            Abort = paste0('<button class="btn btn-danger abort-btn" data-job-id="', Job_ID, '">Abort</button>')
          ),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE, ordering = F)
      )
    }, server = FALSE)
    output$aborted_jobs_table <- renderDT({
      datatable(
        rv$data %>% filter(Status == "Aborted") %>%
          mutate(
            Resume = paste0('<button class="btn btn-success resume-btn" data-job-id="', Job_ID, '">Resume</button>'),
            Delete = paste0('<button class="btn btn-danger delete-btn" data-job-id="', Job_ID, '">Delete</button>')
          ),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE, ordering = F)
      )
    }, server = FALSE)
    df = rv$data
    
    job_id_col <- which(names(df) == "Job_ID")
    pid_col <- which(names(df) == "PID")
    
    # Update the status for the specified Job.ID
    pid <- df[df[, job_id_col] == job_id, pid_col]
    print(sprintf("bash abort.sh %s", pid))
    system(sprintf("bash abort.sh %s", pid), intern = TRUE)
    print(paste("Aborted Job ID:", job_id))
  })
  
  observeEvent(input$resume_job_id, {
    job_id <- input$resume_job_id
    job_data <- read_jobs_csv2()
    job_data <- job_data %>%
      mutate(Status = ifelse(Job_ID == job_id, "Running", Status))
    update_jobs_csv(job_data)
    refresh_data()
    output$running_jobs_table <- renderDT({
      datatable(
        rv$data %>% filter(Status == "Running") %>%
          mutate(
            Abort = paste0('<button class="btn btn-danger abort-btn" data-job-id="', Job_ID, '">Abort</button>')
          ),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE, ordering = F)
      )
    }, server = FALSE)
    output$aborted_jobs_table <- renderDT({
      datatable(
        rv$data %>% filter(Status == "Aborted") %>%
          mutate(
            Resume = paste0('<button class="btn btn-success resume-btn" data-job-id="', Job_ID, '">Resume</button>'),
            Delete = paste0('<button class="btn btn-danger delete-btn" data-job-id="', Job_ID, '">Delete</button>')
          ),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE, ordering = F)
      )
    }, server = FALSE)
    log_path <- paste0("OncoGenAI/results/",job_id, "_logfile.txt")
    df = job_data
    job_id_col <- which(names(df) == "Job_ID")

    job_df <- df[df[, job_id_col] == job_id, ,drop=F]
    file_path = strsplit(job_df$Uploaded.Files, split = "\t")[[1]]
    

    if(job_df$Workflow == "IonTorrent"){
      if (length(file_path) == 1){
        script_run_cmd <- paste0("bam_to_vcf.sh" , " -f " , file_path[1]," -g ", job_df$Genome," -s ", job_id, " -a ", job_df$Mutations, " -b ", job_df$Bed.File, " -t ", job_df$Workflow, " -p ", job_df$Progress," -trim ",job_df$Adaptor.Trimming,  " -cnv " ,job_df$CNV," -tumor ", job_df$Tumor.Percentage)
      }else{
        print("Please give single Bam file at a time")
      }
    }else{
      if (length(file_path) > 1){
        script_run_cmd <- paste0("fastq_to_vcf.sh"," -f1 ",file_path[1]," -f2 ",file_path[2]," -g ",job_df$Genome," -s ",job_id," -a ",job_df$Mutations," -b ",job_df$Bed.File," -t ",job_df$Workflow," -p ",job_df$Progress," -trim ",job_df$Adaptor.Trimming," -cnv ",job_df$CNV.analysis)
        
      }else{
        script_run_cmd <- paste0("fastq_to_vcf_SE.sh"," -f1 ",file_path[1]," -g ",job_df$Genome," -s ",job_id," -a ",job_df$Mutations," -b ",job_df$Bed.File," -t ",job_df$Workflow," -p ",job_df$Progress," -trim ",job_df$Adaptor.Trimming," -cnv ",job_df$CNV.analysis)
      }
    }
    
    system(sprintf("nohup bash %s > %s 2>&1 &", script_run_cmd,log_path), intern = TRUE)
    print(sprintf("nohup bash %s > %s 2>&1 &", script_run_cmd,log_path))
    print(paste("Resumed Job ID:", job_id))
  })
  
  observeEvent(input$delete_job_id, {
    job_id <- input$delete_job_id
    job_data <- read_jobs_csv2()
    job_data <- job_data %>% filter(Job_ID != job_id)
    update_jobs_csv(job_data)
    refresh_data()
    output$aborted_jobs_table <- renderDT({
      datatable(
        rv$data %>% filter(Status == "Aborted") %>%
          mutate(
            Resume = paste0('<button class="btn btn-success resume-btn" data-job-id="', Job_ID, '">Resume</button>'),
            Delete = paste0('<button class="btn btn-danger delete-btn" data-job-id="', Job_ID, '">Delete</button>')
          ),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE, ordering = F)
      )
    }, server = FALSE)
    
    system(sprintf("rm -r OncoGenAI/results/%s",job_id))
    #print(sprintf("bash abort.sh %s", pid))
  })
  
  
  observeEvent(input$view_job_id, {
    job_id <- input$view_job_id
    print(paste("Viewed Job ID:", job_id))
    
    updateTabItems(session, "tabs2", selected = "filterationreport")
    updateSelectInput(session, "select_job_id_filter", selected = job_id)
    
  })
  
  
  observeEvent(input$Refresh, {
    rv$data <- NULL
    refresh_data()
    print(rv$data)
    output$running_jobs_table <- renderDT({
      datatable(
        rv$data %>% filter(Status == "Running") %>%
          mutate(
            Abort = paste0('<button class="btn btn-danger abort-btn" data-job-id="', Job_ID, '">Abort</button>')
          ),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE, ordering = F)
      )
    }, server = FALSE)
    output$completed_jobs_table <- renderDT({
      datatable(
        rv$data %>% filter(Status == "Completed") %>%
          mutate(
            View = paste0('<button class="btn btn-info view-btn" data-job-id="', Job_ID, '">View</button>')
          ),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE, ordering = F)
      )
    }, server = FALSE)
    output$aborted_jobs_table <- renderDT({
      datatable(
        rv$data %>% filter(Status == "Aborted") %>%
          mutate(
            Resume = paste0('<button class="btn btn-success resume-btn" data-job-id="', Job_ID, '">Resume</button>'),
            Delete = paste0('<button class="btn btn-danger delete-btn" data-job-id="', Job_ID, '">Delete</button>')
          ),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE, ordering = F)
      )
    }, server = FALSE)

    job_data <- read.csv("OncoGenAI/running_jobs.csv")
    job_data <- job_data[job_data$Status == "Completed",,drop=F]
      # Update Job ID choices
    updateSelectInput(session, "select_job_id", choices = c("", job_data$Job_ID))
    updateSelectInput(session, "select_job_id_r1", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
    updateSelectInput(session, "select_job_id_r2", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
    updateSelectInput(session, "select_job_id_r3", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
    updateSelectInput(session, "select_job_id_r4", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
    updateSelectInput(session, "select_job_id_data", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
    updateSelectInput(session, "select_job_id_kg", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
    updateSelectInput(session, "select_job_id_gb1", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
    updateSelectInput(session, "select_job_id_rpt", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
    updateSelectInput(session, "select_job_id_tmb", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
    updateSelectInput(session, "select_job_id_filter", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
    updateSelectInput(session, "select_job_id_di1", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
    updateSelectInput(session, "select_job_id_di2", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
    updateSelectInput(session, "select_job_id_di3", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
    updateSelectInput(session, "select_job_id_di4", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
    updateSelectInput(session, "select_job_id_di5", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
    updateSelectInput(session, "select_job_id_di6", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")

    print("Data refreshed")
  })
  
  ################################################################################
  
  ##############################  report generation #############################

  #choices = setNames( job_data$Job_ID, paste(job_data$Job_ID, job_data$Patient_ID sep="-"))
  
  # Read CSV file
 job_data <- read.csv("OncoGenAI/running_jobs.csv")
 job_data <- job_data[job_data$Status == "Completed",,drop=F]
  # Update Job ID choices
  updateSelectInput(session, "select_job_id", choices = c("", job_data$Job_ID))
  updateSelectInput(session, "select_job_id_r1", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
  updateSelectInput(session, "select_job_id_r2", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
  updateSelectInput(session, "select_job_id_r3", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
  updateSelectInput(session, "select_job_id_r4", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
  updateSelectInput(session, "select_job_id_data", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
  updateSelectInput(session, "select_job_id_kg", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
  updateSelectInput(session, "select_job_id_gb1", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
  updateSelectInput(session, "select_job_id_rpt", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
  updateSelectInput(session, "select_job_id_tmb", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
  updateSelectInput(session, "select_job_id_filter", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
  updateSelectInput(session, "select_job_id_di1", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
  updateSelectInput(session, "select_job_id_di2", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="" )
  updateSelectInput(session, "select_job_id_di3", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
  updateSelectInput(session, "select_job_id_di4", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
  updateSelectInput(session, "select_job_id_di5", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
  updateSelectInput(session, "select_job_id_di6", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected ="")
  #updateSelectInput(session, "select_job_id_filter", choices = c("", job_data$Job_ID), selected = "")

  # display Patient ID when Job ID is selected
  observeEvent(input$select_job_id, {
    req(input$select_job_id)
    if (input$select_job_id != "") {
      selected_job <- job_data[job_data$Job_ID == input$select_job_id, ]
      if (nrow(selected_job) == 1) {
        patient_id <- selected_job$Patient_ID
        output$display_patient_id <- renderText({ paste("Patient ID:", patient_id) })
        output$display_patient_id_r1 <- renderText({ paste("Patient ID:", patient_id) })
        output$display_patient_id_r2 <- renderText({ paste("Patient ID:", patient_id) })
        output$display_patient_id_r3 <- renderText({ paste("Patient ID:", patient_id) })
      } else {
        showNotification("Job ID not found", type = "error")
      }
    } else {
      output$display_patient_id <- renderText({ "" })
    }
  })
  
  # display Patient ID when Job ID is selected
  observeEvent(input$select_job_id_r1, {
    req(input$select_job_id_r1)
    if (input$select_job_id_r1 != "") {
      selected_job <- job_data[job_data$Job_ID == input$select_job_id_r1, ]
      if (nrow(selected_job) == 1) {
        patient_id <- selected_job$Patient_ID
        output$display_patient_id_r1 <- renderText({ paste("Patient ID:", patient_id) })
      } else {
        showNotification("Job ID not found", type = "error")
      }
    } else {
      output$display_patient_id <- renderText({ "" })
    }
  })
  
  # display Patient ID when Job ID is selected

  
  observeEvent(input$select_job_id_r2, {
    
    req(input$select_job_id_r2)
    
    if (input$select_job_id_r2 != "") {
      selected_job <- job_data[job_data$Job_ID == input$select_job_id_r2, ]
      if (nrow(selected_job) == 1) {
        patient_id <- selected_job$Patient_ID
        output$display_patient_id_r2 <- renderText({ paste("Patient ID:", patient_id) })
      } else {
        showNotification("Job ID not found", type = "error")
      }
    } else {
      output$display_patient_id <- renderText({ "" })
    }
    
    
    job_id <- input$select_job_id_r2
    
    selected_job <- job_data[job_data$Job_ID == input$select_job_id_r2, ]
    
    folder_path <- paste0("OncoGenAI/results/", job_id, "/")
    genome <- selected_job$Genome
    
    print(genome)
    print(selected_job)
    
    # SNPs
    file_path <- paste0(folder_path, job_id, "_snps.", genome, "_multianno.txt.intervar")
    
    anotated_acmg <- read.csv(file_path, sep = "\t")
    
    Variant_type <- c()
    Variant_type_clinvar <- c()
    pathogenic_index <- c()
    
    pathogenic_categories <- c("Likely pathogenic", "Pathogenic", "Pathogenic, risk_factor")
    
    for(i in 1:nrow(anotated_acmg)) {
      intervar <- anotated_acmg$InterVar..InterVar.and.Evidence[i]
      substring <- sub(".*InterVar: \\s*(.*?)\\s*PVS1.*", "\\1", intervar)
      substring <- trimws(substring)
      Variant_type <- c(Variant_type, substring)
      
      clinvar <- anotated_acmg$clinvar..Clinvar[i]  # Added index [i]
      substring2 <- clinvar #sub(".*clinvar: ", "", clinvar)
      substring2 <- trimws(substring2)
      Variant_type_clinvar <- c(Variant_type_clinvar, substring2)
      
      # Pathogenic index update logic
      if (any(substring %in% pathogenic_categories) | any(substring2 %in% pathogenic_categories)) {
        pathogenic_index <- c(pathogenic_index, i)
      }
    }
    
    pathogenic_snp <- anotated_acmg[pathogenic_index, , drop = FALSE]
    #print(pathogenic_snp)
    print(file_path)
    
    
    pathogenic_index <- c()
    
    # Indels
    file_path <- paste0(folder_path, job_id, "_indels.", genome, "_multianno.txt.intervar")
    anotated_acmg <- read.csv(file_path, sep = "\t")
    
    for(i in 1:nrow(anotated_acmg)) {
      intervar <- anotated_acmg$InterVar..InterVar.and.Evidence[i]
      substring <- sub(".*InterVar: \\s*(.*?)\\s*PVS1.*", "\\1", intervar)
      substring <- trimws(substring)
      Variant_type <- c(Variant_type, substring)
      
      clinvar <- anotated_acmg$clinvar..Clinvar[i]  # Added index [i]
      substring2 <- clinvar#sub(".*clinvar: ", "", clinvar)
      substring2 <- trimws(substring2)
      Variant_type_clinvar <- c(Variant_type_clinvar, substring2)
      
      # Pathogenic index update logic
      if (any(substring %in% pathogenic_categories) | any(substring2 %in% pathogenic_categories)) {
        pathogenic_index <- c(pathogenic_index, i)
      }
    }
    
    
    pathogenic_indels <- anotated_acmg[pathogenic_index, , drop = FALSE]
    #print(pathogenic_indels)
    
    # Plot for SNPs
    output$pieChart7 <- renderPlotly({
      counts <- as.data.frame(table(Variant_type))
      print(counts)
      
      plot_ly(counts, labels = ~Variant_type, values = ~Freq, type = 'pie', textinfo = 'label+percent',
              insidetextorientation = 'radial') %>%
        layout(title = "Automated Interpretation")
    })
    
    # Plot for Clinvar Interpretation
    output$pieChart8 <- renderPlotly({
      counts_clinvar <- as.data.frame(table(Variant_type_clinvar))
      print(counts_clinvar)
      counts_clinvar <- counts_clinvar %>%
        mutate(
          Broad_Category = case_when(
            grepl("Benign|Likely_benign", Variant_type_clinvar) ~ "Benign/Likely Benign",
            grepl("Pathogenic|Likely_pathogenic", Variant_type_clinvar) ~ "Pathogenic/Likely Pathogenic",
            grepl("Conflicting_interpretations_of_pathogenicity", Variant_type_clinvar) ~ "Conflicting",
            grepl("drug_response", Variant_type_clinvar) ~ "Drug Response",
            grepl("protective", Variant_type_clinvar) ~ "Protective",
            grepl("risk_factor", Variant_type_clinvar) ~ "Risk Factor",
            grepl("Uncertain_significance", Variant_type_clinvar) ~ "Uncertain Significance",
            TRUE ~ "Other"
          )
        )
      plot_ly(counts_clinvar, labels = ~Broad_Category, values = ~Freq, type = 'pie', textinfo = 'label+percent',
              insidetextorientation = 'radial') %>%
        layout(title = "Clinvar Interpretation")
    })
    
    
    pathogenic_df <- rbind(pathogenic_snp,pathogenic_indels)
    pathogenic_df<- pathogenic_df[,c("X.Chr","Start","End", "Ref","Alt","Ref.Gene","Func.refGene","OMIM","avsnp151")]
    colnames(pathogenic_df) <- c("Chromosome", "Start_Position", "End_Position", "Reference_Allele", 
                      "Alternate_Allele", "Gene", "Function_Gene", "OMIM_ID", "dbSNP_ID")
    output$pathogenic_variants<-renderDataTable({
      datatable(pathogenic_df)
    })
    
    
    
    
  })
  
    observeEvent(input$view_r3, {
    updateTabItems(session, "tabs2", selected =  "result3")
    updateSelectInput(session, "select_job_id_r3", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected =input$select_job_id_r2 )
  })
    
#--------------------end of reuslt page 2 ---------------------------


  
  observeEvent(input$select_job_id_r3, {
    req(input$select_job_id_r3)
    if (input$select_job_id_r3 != "") {
      selected_job <- job_data[job_data$Job_ID == input$select_job_id_r3, ]
      if (nrow(selected_job) == 1) {
        patient_id <- selected_job$Patient_ID
        output$display_patient_id_r3 <- renderText({ paste("Patient ID:", patient_id) })
      } else {
        showNotification("Job ID not found", type = "error")
      }
    } else {
      output$display_patient_id <- renderText({ "" })
    }
    shinyjs::disable("query_kg_button")
  })

  
  
  library(dplyr)
  library(stringr)
  library(tidyr)

observeEvent(input$tab_filter, {
    shinyjs::show("contextMenu")
    
    # Adjust position considering page scroll
    shinyjs::runjs("
        var contextMenu = document.getElementById('contextMenu');
        var xPos = event.clientX + window.scrollX;
        var yPos = event.clientY + window.scrollY;
        contextMenu.style.left = xPos + 'px';
        contextMenu.style.top = yPos + 'px';
    ")
    
    # Always re-read the CSV file when opening the context menu
    filters_df <- read.csv("user_defined_filter.csv")
    
    # Create new dropdown options based on the current data in the CSV
    new_choices <- paste(filters_df$username, filters_df$filter_name, sep = " - ")
    
    # Add an empty option as the default and then the dynamic options
    choices <- c("", new_choices)
    
    # Update the dropdown to reflect the latest CSV data
    updateSelectInput(session, "dropdown", choices = choices, selected = "")
})

create_varsome_link <- function(variation, location, ref, alt,genome) {
  if (grepl("^rs", variation)) {
    # If the variation starts with 'rs', create link using rsID
    link <- paste0(
      '<a href="https://varsome.com/variant/', genome,'/', variation,
      '" target="_blank">', variation, '</a>'
    )
  } else {
    # Case for genomic locations
    if (grepl("-", location)) {
      # Case for indel (range of positions)
      parts <- unlist(strsplit(location, "[:-]"))
      chromosome <- parts[1]
      start_pos <- as.numeric(parts[2])
      end_pos <- as.numeric(parts[3])
    } else {
      # Case for SNP (single position)
      parts <- unlist(strsplit(location, ":"))
      chromosome <- parts[1]
      start_pos <- as.numeric(parts[2])
      end_pos <- start_pos
    }
    
    # Create Varsome link for genomic location (indel or SNP)
    link <- paste0(
      '<a href="https://varsome.com/variant/',genome,'/', chromosome, "-", start_pos, "-", ref, "-", alt,
      '" target="_blank">', variation, '</a>'
    )
  }
  return(link)
}


  observeEvent(input$select_job_id_r3, {
    req(input$select_job_id_r3)
    
    
    if (input$select_job_id_r3 != "") {
      selected_job <- job_data[job_data$Job_ID == input$select_job_id_r3, ]
      if (nrow(selected_job) == 1) {
        patient_id <- selected_job$Patient_ID
        output$display_patient_id_r2 <- renderText({ paste("Patient ID:", patient_id) })
      } else {
        showNotification("Job ID not found", type = "error")
      }
    } else {
      output$display_patient_id <- renderText({ "" })
    }
    
    
    job_id <- input$select_job_id_r3
    
    selected_job <- job_data[job_data$Job_ID == input$select_job_id_r3, ]
    
    folder_path <- paste0("OncoGenAI/results/", job_id, "/")
    genome <- selected_job$Genome
    
  

      
    csv_file_path <- paste0(folder_path, job_id, "_transcript-id.csv")
    
    
    final_merged_data <- fread(csv_file_path)

    final_merged_data$Varsome_Link  <-  mapply(create_varsome_link, 
                  final_merged_data$Upload_variation, 
                  final_merged_data$Location, 
                  final_merged_data$Ref, 
                  final_merged_data$ALt,
                  genome)
    

    # columns_to_keep = c(
    #   'Location', 'Ref.Gene', 'Ref', 'ALt', 'Otherinfo',
    #   'InterVar..InterVar.and.Evidence', 'RefAF', 'AltAF', 'CLIN_SIG',
    #   'OMIM', 'AAChange.ensGene.y', 'Phenotype_MIM', 'Orpha', 'Consequence', 'CADD_phred.x', 
    #   'SIFT_score.x', 'Polyphen2_HDIV_score', 'PolyPhen', 'MAX_AF','Varsome_Link'
    # )
    # c(
    #   'LOCATION', 'GENE', 'REF', 'ALT', 'ZYG', 
    #   'ACMG', 'RefAF', 'AltAF', 'Clinvar',
    #   'OMIM', 'AA', 'Phenotype_MIM', 'Orpha', 'Consequence', 'CADD_phred', 'SIFT_score', 
    #   'Polyphen2_HDIV_score', 'PolyPhen','MaxPopAF','Varsome_Link'
    # )
    
    # final_merged_data <- final_merged_data[, ..columns_to_keep]
    
    # colnames(final_merged_data) <- c(
    #   'LOCATION', 'GENE', 'REF', 'ALT', 'ZYG', 
    #   'ACMG', 'RefAF', 'AltAF', 'Clinvar',
    #   'OMIM', 'AA', 'Phenotype_MIM', 'Orpha', 'Consequence', 'CADD_phred', 'SIFT_score', 
    #   'Polyphen2_HDIV_score', 'PolyPhen','MaxPopAF','Varsome_Link'
    # )
    

    # old_names <- c('Location', 'Ref.Gene', 'Ref', 'ALt', 'Otherinfo',
    #            'InterVar..InterVar.and.Evidence', 'RefAF', 'AltAF', 'CLIN_SIG',
    #            'OMIM', 'AAChange.ensGene.y', 'Phenotype_MIM', 'Orpha', 'Consequence', 'CADD_phred.x', 
    #            'SIFT_score.x', 'Polyphen2_HDIV_score', 'PolyPhen', 'MAX_AF','Varsome_Link')

    # new_names <- c('LOCATION', 'GENE', 'REF', 'ALT', 'ZYG', 
    #               'ACMG', 'RefAF', 'AltAF', 'Clinvar',
    #               'OMIM', 'AA', 'Phenotype_MIM', 'Orpha', 'Consequence', 'CADD_phred', 'SIFT_score', 
    #               'Polyphen2_HDIV_score', 'PolyPhen','MaxPopAF','Varsome_Link')

    # # Rename the columns
    # names(final_merged_data)[names(final_merged_data) %in% old_names] <- new_names

    # Rename columns manually
    names(final_merged_data)[names(final_merged_data) == 'Location'] <- 'LOCATION'
    names(final_merged_data)[names(final_merged_data) == 'Ref.Gene'] <- 'GENE'
    names(final_merged_data)[names(final_merged_data) == 'Ref'] <- 'REF'
    names(final_merged_data)[names(final_merged_data) == 'ALt'] <- 'ALT'
    names(final_merged_data)[names(final_merged_data) == 'Otherinfo'] <- 'ZYG'
    names(final_merged_data)[names(final_merged_data) == 'InterVar..InterVar.and.Evidence'] <- 'ACMG'
    names(final_merged_data)[names(final_merged_data) == 'RefAF'] <- 'RefAF'
    names(final_merged_data)[names(final_merged_data) == 'AltAF'] <- 'AltAF'
    names(final_merged_data)[names(final_merged_data) == 'CLIN_SIG'] <- 'Clinvar'
    names(final_merged_data)[names(final_merged_data) == 'OMIM'] <- 'OMIM'
    names(final_merged_data)[names(final_merged_data) == 'AAChange.refGene.x'] <- 'AA'
    names(final_merged_data)[names(final_merged_data) == 'Phenotype_MIM'] <- 'Phenotype_MIM'
    names(final_merged_data)[names(final_merged_data) == 'Orpha'] <- 'Orpha'
    names(final_merged_data)[names(final_merged_data) == 'Consequence'] <- 'Consequence'
    names(final_merged_data)[names(final_merged_data) == 'CADD_phred.x'] <- 'CADD_phred'
    names(final_merged_data)[names(final_merged_data) == 'SIFT_score.x'] <- 'SIFT_score'
    names(final_merged_data)[names(final_merged_data) == 'Polyphen2_HDIV_score'] <- 'Polyphen2_HDIV_score'
    names(final_merged_data)[names(final_merged_data) == 'PolyPhen'] <- 'PolyPhen'
    names(final_merged_data)[names(final_merged_data) == 'MAX_AF'] <- 'MaxPopAF'
    names(final_merged_data)[names(final_merged_data) == 'Varsome_Link'] <- 'Varsome_Link'
    names(final_merged_data)[names(final_merged_data) == 'Func.refGene.x'] <- 'Func.refGene'


    final_merged_data$GENE <- as.factor(final_merged_data$GENE)
    final_merged_data$REF <- as.factor(final_merged_data$REF)
    final_merged_data$ALT <- as.factor(final_merged_data$ALT)
    final_merged_data$AA <- as.factor(final_merged_data$AA)
    final_merged_data$ZYG <- as.factor(final_merged_data$ZYG)
    
    final_merged_data$ACMG <- as.factor(final_merged_data$ACMG)
    #final_merged_data$RefAF <- as.factor(final_merged_data$RefAF)
    #final_merged_data$AltAF <- as.factor(final_merged_data$AltAF)
    final_merged_data$RefAF <- as.numeric(final_merged_data$RefAF)
    final_merged_data$AltAF <- as.numeric(final_merged_data$AltAF)
    # # Now round the values to 2 decimal places
    final_merged_data$RefAF <- round(final_merged_data$RefAF, 2)
    final_merged_data$AltAF <- round(final_merged_data$AltAF, 2)
    #final_merged_data <- final_merged_data[, c(setdiff(names(final_merged_data), "Upload_variation"), "Upload_variation")]

    #final_merged_data <- Varsome_Link
    print(unique(final_merged_data$Clinvar))
    
    classify_clinvar <- function(clinvar_value) {
      # Convert to lowercase for consistent comparison
      clinvar_value <- tolower(clinvar_value)
      
      if (grepl("pathogenic", clinvar_value)) {
        return("Pathogenic")
      } else if (grepl("benign", clinvar_value)) {
        return("Benign")
      } else if (grepl("uncertain_significance", clinvar_value)) {
        return("Uncertain Significance")
      } else {
        return("Other")
      }
    }
    
    # Apply the classification function to the Clinvar column
    final_merged_data$Clinvar <- sapply(final_merged_data$Clinvar, classify_clinvar)
    
    final_merged_data$Clinvar <- as.factor(final_merged_data$Clinvar)
    
    
    final_merged_data$OMIM <- as.factor(final_merged_data$OMIM)
    final_merged_data$Phenotype_MIM <- as.factor(final_merged_data$Phenotype_MIM)
    final_merged_data$Orpha <- as.factor(final_merged_data$Orpha)
    final_merged_data$Consequence <- as.factor(final_merged_data$Consequence)
    final_merged_data$CADD_phred <- as.factor(final_merged_data$CADD_phred)
    final_merged_data$SIFT_score <- as.factor(final_merged_data$SIFT_score)
    
    final_merged_data$Polyphen2_HDIV_score <- as.factor(final_merged_data$Polyphen2_HDIV_score)
    final_merged_data$PolyPhen <- as.factor(final_merged_data$PolyPhen)
    final_merged_data$MaxPopAF <- final_merged_data$MaxPopAF
    
    

    
    # Sample data
    data <-  final_merged_data[,c("ACMG"),drop=F]
    
    # Function to extract and split values
    extract_and_split <- function(df, col, pattern, prefix, num_cols) {
      df %>%
        mutate(temp_values = str_extract(!!sym(col), pattern)) %>%
        mutate(temp_values = str_remove_all(temp_values, paste0(prefix, "=|\\[|\\]"))) %>%
        separate(temp_values, into = paste0(prefix, 1:num_cols), sep = ",\\s*", convert = TRUE)
    }
    
    
    data <- data %>%
      mutate(PVS1 = str_extract(ACMG, "PVS1=\\d")) %>%
      mutate(PVS1 = str_remove_all(PVS1, "PVS1="))
    
    # Extract and split for PS
    data <- extract_and_split(data, "ACMG", "PS=\\[.*?\\]", "PS", 5)
    
    # Extract and split for PM
    data <- extract_and_split(data, "ACMG", "PM=\\[.*?\\]", "PM", 7)
    
    # Extract and split for PP
    data <- extract_and_split(data, "ACMG", "PP=\\[.*?\\]", "PP", 6)
    
    # Extract BA1 (single value, no brackets)
    data <- data %>%
      mutate(BA1 = str_extract(ACMG, "BA1=\\d")) %>%
      mutate(BA1 = str_remove_all(BA1, "BA1="))
    
    # Extract and split for BS
    data <- extract_and_split(data, "ACMG", "BS=\\[.*?\\]", "BS", 5)
    
    # Extract and split for BP
    data <- extract_and_split(data, "ACMG", "BP=\\[.*?\\]", "BP", 8)
    
    
    
    data$ACMG <- apply(data[,1:33], 1, function(x) paste(names(x)[x == 1], collapse = ", "))
    

    # print(unique(final_merged_data$Clinvar))
    # final_merged_data$ACMG <- data$ACMG
    final_merged_data$ACMG <- gsub(".*InterVar:(.*)PVS1.*", "\\1",final_merged_data$ACMG)

    final_merged_data$ACMG <- as.factor(final_merged_data$ACMG )

    #print(unique(final_merged_data$ACMG))
    final_merged_data$Intervar <- data$ACMG
    

    rv$filtered_data_all<-as.data.frame(final_merged_data)
    rv$merged_data_all <-as.data.frame(final_merged_data)
    
    
    

    # Update pickerInput choices dynamically

    #updatePickerInput(session, "columns", choices =  names(final_merged_data), selected = names(final_merged_data)[1:17])
  



      updatePickerInput(session, "columns", choices =  names(final_merged_data), selected = names(final_merged_data)[1:17])
  
  
  # Render the data table
  output$selected_columns2 <- renderDT({
    datatable(
      rv$merged_data_all[, input$columns, drop = FALSE],
      # filter = "top",
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        ordering = F,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('selectAll', 'selectNone')
      ),
      escape = F,
      selection = 'multiple',
      rownames = TRUE,
      class = 'display'
    )
  })
    
  })



    observeEvent(input$dropdown, {
    # Show buttons only when a filter is selected
    if (input$dropdown != "") {
      output$modal_buttons <- renderUI({
        tagList(
          actionButton("delete_filter", label = NULL, icon = icon("trash"), class = "btn-delete"),
          actionButton("edit_filter", label = NULL, icon = icon("pencil"), class = "btn-edit"),
          actionButton("apply_filter_drop_down", "Apply", class = "btn-action")
        )
      })
    } else {
      output$modal_buttons <- renderUI(NULL)  # Hide buttons if no filter is selected
    }
  })
  observeEvent(input$closeContextMenu, {
    shinyjs::hide("contextMenu")
  })
  
  ###########delete the filter##########3
  observeEvent(input$delete_filter, {
    # Extract the selected filter from the dropdown
    selected_filter <- input$dropdown
    
    if (selected_filter != "") {
      # Separate the username and filter name from the selected filter
      selected_parts <- strsplit(selected_filter, " - ")[[1]]
      username <- selected_parts[1]
      filter_name <- selected_parts[2]
      
      # Read the CSV file
      filters_df <- read.csv("user_defined_filter.csv")
      
      # Find the row to delete based on username and filter name
      row_to_delete <- filters_df$username == username & filters_df$filter_name == filter_name
      
      # Remove the selected row
      updated_df <- filters_df[!row_to_delete, ]
      
      # Write the updated data back to the CSV file (overwriting the old file)
      write.csv(updated_df, "user_defined_filter.csv", row.names = FALSE)
      
      # Re-read the CSV file after the deletion to ensure the UI is updated
      updated_filters <- read.csv("user_defined_filter.csv")
      
      # Update the dropdown options with the new data
      new_choices <- paste(updated_filters$username, updated_filters$filter_name, sep = " - ")
      updateSelectInput(session, "dropdown", choices = c("", new_choices), selected = "")
      
      # Clear the modal buttons
      output$modal_buttons <- renderUI(NULL)
    }
  })
  
  ###############################edit button######

  
  csv_filter <- "user_defined_filter.csv"
  
  # Reactive expression to read the CSV file from the path
  csv_data_filter <- reactive({
    read.csv(csv_filter)
  })
  
  # Update the dropdown with usernames and filter names
  observe({
    data <- csv_data_filter()
    if (nrow(data) > 0) {
      choices <- paste(data$username, "-", data$filter_name)
      updateSelectInput(session, "user_filter_select", choices = choices)
    }
  })
  
  # Handle Edit button click
  observeEvent(input$edit_filter, {
    # unique_intervar <- unique(csv_data()$Intervar)
    # unique_chromosome <- unique(csv_data()$Chromosome)
    # unique_function <- unique(csv_data()$Function_Gene)
    unique_clinvar <- unique(rv$merged_data_all$Clinvar)
    #clinvar <- sub("clinvar: ", "", unique_clinvar)
    unique_acmg <- unique(rv$merged_data_all$ACMG)
    unique_zyg <- unique(rv$merged_data_all$ZYG)
    
    selected <- input$dropdown
    if (!is.null(selected) && selected != "") {
      # Extract username and filter name from the selected choice
      parts <- strsplit(selected, " - ")[[1]]
      username <- parts[1]
      filter_name <- parts[2]
      
      # Load CSV data to get the filters and options
      data <- csv_data_filter()
      
      # Find the corresponding row for the selected username and filter name
      selected_row <- data[data$username == username & data$filter_name == filter_name, ]
      
      if (nrow(selected_row) > 0) {
        # Extract selected filters and their options
        selected_filters <- list()
        for (i in seq_len(nrow(selected_row))) {
          selected_filters[[selected_row$columns_filter[i]]] <- unlist(strsplit(as.character(selected_row$options[i]), ", "))
        }
        
        showModal(modalDialog(
          title = "Edit Filter", class = "modal-dialog",
          div(class = "filter-options",
              div(class = "filter-options-left",
                  box(title = "Available Filters", status = "primary", solidHeader = F, width = 12,
                      box(title = tagList("ACMG Criteria", 
                                          actionButton("add_acmg", "Add", class = "btn-add", style = "float: right; margin-top: -5px; margin-right: -450px;")),
                          status = "info", solidHeader = F, collapsible = TRUE, collapsed = T, width = 12,
                          checkboxGroupInput("excluding_criteria_acmg", NULL, choices = c("Exclude variants that match")),
                          checkboxGroupInput("acmg_criteria", "Class", choices = unique_acmg, selected = NULL)
                      ),

                      box(title = tagList("ClinVar", 
                                          actionButton("add_clinvar", "Add", class = "btn-add", style = "float: right; margin-top: -5px; margin-left: 450px;")),
                          status = "info", solidHeader = F, collapsible = TRUE, collapsed = T, width = 12,
                          checkboxGroupInput("excluding_criteria_clinvar", NULL, choices = c("Exclude variants that match")),
                          checkboxGroupInput("clinvar", "Class", choices = unique_clinvar, selected = NULL)
                      ),
                      box(
                          title = tagList("Zygosity", 
                                          actionButton("add_zyg", "Add", class = "btn-add", 
                                                      style = "float: right; margin-top: -5px; margin-right: -450px;")),
                          status = "info", solidHeader = F, collapsible = TRUE, collapsed = T, width = 12,
                          checkboxGroupInput("excluding_criteria_zyg", NULL, choices = c("Exclude variants that match")),
                          # Zygosity checkbox group
                          checkboxGroupInput("zyg_criteria", "Zygosity", 
                                            choices = unique_zyg,
                                            selected = NULL)
                        ),
                      box(
                        title = tagList("Population Frequency", 
                                        actionButton("add_pop_freq", "Add", class = "btn-add", 
                                                     style = "float: right; margin-top: -5px; margin-right: -450px;")),
                        status = "info", solidHeader = F, collapsible = TRUE, collapsed = T, width = 12,
                        numericInput("pop_freq_from", "From:", value = NULL, min = 0, max = 1, step = 0.01),
                        numericInput("pop_freq_to", "To:", value = NULL, min = 0, max = 1, step = 0.01)
                      )
                  )),
              div(class = "filter-options-right",
                  box(title = tagList("Edit Filter Set"),status = "primary", solidHeader = F, width = 12,
                      div(style = "display: flex; justify-content: space-between; align-items: center;",
                          textInput("username", "User Name:", value = username, placeholder = "Enter the user name", width = "48%"),
                          textInput("filter_name", "Filter Name:", value = filter_name, placeholder = "Enter the filter name", width = "48%")
                      ),
                      uiOutput("selected_filters")
                  ), 
              )
          ),
          easyClose = TRUE,
          footer = div(style = "display: flex; justify-content: flex-end; align-items: center;",
                       actionButton("save_to_csv", "Save", class = "btn-save"),
                       actionButton("apply_filters", "Apply", class = "btn-filter", style = "margin-left: 10px;")
          )
        ))
        # Populate the selected filters in the modal
        output$selected_filters <- renderUI({
          filter_boxes <- lapply(names(selected_filters), function(filter) {
            options <- paste(selected_filters[[filter]], collapse = ", ")
            tagList(
              div(style = "font-weight: bold;", filter),
              div(options)
            )
          })
          do.call(tagList, filter_boxes)
        })
      }
    }
  })



  observeEvent(input$createFilter, {
    shinyjs::hide("contextMenu")
    # unique_intervar <- unique(rv$merged_data_all$Intervar)
    # unique_chromosome <- unique(rv$merged_data_all$Chromosome)
    # unique_function <- unique(rv$merged_data_all$Function_Gene)
    unique_clinvar <- unique(rv$merged_data_all$Clinvar)
    #clinvar <- sub("clinvar: ", "", unique_clinvar)
    unique_acmg <- unique(rv$merged_data_all$ACMG)
    unique_zyg <- unique(rv$merged_data_all$ZYG)
    
    showModal(modalDialog(
      title = "Create New Filter", class = "modal-dialog",
      div(class = "filter-options",
          div(class = "filter-options-left",
              box(title = "Available Filters", status = "primary", solidHeader = F, width = 12,
                  box(title = tagList("ACMG Criteria", 
                                      actionButton("add_acmg", "Add", class = "btn-add", style = "float: right; margin-top: -5px; margin-right: -450px;")),
                      status = "info", solidHeader = F, collapsible = TRUE, collapsed = F, width = 12,
                      checkboxGroupInput("excluding_criteria_acmg", NULL, choices = c("Exclude variants that match")),
                      checkboxGroupInput("acmg_criteria", "Class", choices = unique_acmg, selected = NULL)
                  ),
                  box(title = tagList("ClinVar", 
                                      actionButton("add_clinvar", "Add", class = "btn-add", style = "float: right; margin-top: -5px; margin-left: 450px;")),
                      status = "info", solidHeader = F, collapsible = TRUE, collapsed = F, width = 12,
                      checkboxGroupInput("excluding_criteria_clinvar", NULL, choices = c("Exclude variants that match")),
                      checkboxGroupInput("clinvar", "Class", choices = unique_clinvar, selected = NULL)
                  ),
                  box(title = tagList("Zygosity", 
                      actionButton("add_zyg", "Add", class = "btn-add", style = "float: right; margin-top: -5px; margin-left: 450px;")),
                      status = "info", solidHeader = F, collapsible = TRUE, collapsed = F, width = 12,
                      checkboxGroupInput("excluding_criteria_zyg", NULL, choices = c("Exclude variants that match")),
                      checkboxGroupInput("zyg_criteria", "Class", choices = unique_zyg, selected = NULL)
                  ),
                    box(
                        title = tagList("Population Frequency", 
                                        actionButton("add_pop_freq", "Add", class = "btn-add", 
                                                     style = "float: right; margin-top: -5px; margin-right: -450px;")),
                        status = "info", solidHeader = F, collapsible = TRUE, collapsed = T, width = 12,
                        numericInput("pop_freq_from", "From:", value = NULL, min = 0, max = 1, step = 0.01),
                        numericInput("pop_freq_to", "To:", value = NULL, min = 0, max = 1, step = 0.01)
                      )
                  
              )),
          div(class = "filter-options-right",
              box(title = tagList("New Filter Set"),status = "primary", solidHeader = F, width = 12,
                  div(style = "display: flex; justify-content: space-between; align-items: center;",
                      textInput("username", "User Name:", "", placeholder = "Enter the user name", width= "48%"),
                      textInput("filter_name", "Filter Name:", "", placeholder = "Enter the filter name", width ="48%")),
                      uiOutput("selected_filters")
              ), 
          )
      ),
      easyClose = TRUE,
      footer = div(style = "display: flex; justify-content: flex-end; align-items: center;",
                   actionButton("save_to_csv", "Save", class = "btn-save"),
                   actionButton("apply_filters", "Apply", class = "btn-filter", style = "margin-left: 10px;")
      )
    ))
  })
  
  # Add selected filters to the right panel when the corresponding "Add" button is clicked
  observeEvent(input$add_acmg, {
    rv$selected_filters[["ACMG_Criteria"]] <- input$acmg_criteria

    shinyjs::disable("add_acmg")
    shinyjs::disable("acmg_criteria")
    shinyjs::disable("excluding_criteria_acmg")
  })
  
  observeEvent(input$add_clinvar, {
    
    rv$selected_filters[["ClinVar"]] <- input$clinvar
    
    shinyjs::disable("add_clinvar")
    shinyjs::disable("clinvar")
    shinyjs::disable("excluding_criteria_clinvar")
  })

  observeEvent(input$add_zyg, {
    rv$selected_filters[["ZYG_Criteria"]] <- input$zyg_criteria

    shinyjs::disable("add_zyg")
    shinyjs::disable("zyg_criteria")
    shinyjs::disable("excluding_criteria_zyg")
  })

  observeEvent(input$add_pop_freq, {
    # Store the population frequency range selected by the user
    rv$selected_filters[["Population_Frequency"]] <- c(input$pop_freq_from, input$pop_freq_to)
    print(input$pop_freq_from)
    print(input$pop_freq_to)

    # Disable the input fields and button to prevent further changes
    shinyjs::disable("add_pop_freq")
    shinyjs::disable("pop_freq_from")
    shinyjs::disable("pop_freq_to")
})

  
  # Display selected filters on the right panel
  output$selected_filters <- renderUI({
    filters <- rv$selected_filters
    if (length(filters) == 0) return(NULL)
    
    tagList(
      # ACMG Criteria Filter
      if (!is.null(filters$`ACMG_Criteria`)) {
        div(
          h5(
            tags$strong("ACMG Criteria"),
            actionButton("remove_acmg_criteria", "Remove", class = "btn-remove", style = "margin-left: 450px")
          ),
          tags$ul(lapply(filters$`ACMG_Criteria`, function(option) tags$li(option)))
        )
      },
      
      if (!is.null(filters$ClinVar)) {
        div(
          h5(
            tags$strong("ClinVar"),
            actionButton("remove_clinvar", "Remove", class = "btn-remove", style = "margin-left: 489px")
          ),
          tags$ul(lapply(filters$ClinVar, function(option) tags$li(option)))
        )
      },
      if (!is.null(filters$`ZYG_Criteria`)) {
        div(
          h5(
            tags$strong("ZYG Criteria"),
            actionButton("remove_zyg_criteria", "Remove", class = "btn-remove", style = "margin-left: 450px")
          ),
          tags$ul(lapply(filters$`ZYG_Criteria`, function(option) tags$li(option)))
        )
      },
      if (!is.null(filters$`Population_Frequency`)) {
      div(
        h5(
          tags$strong("Population Frequency"),
          actionButton("remove_pop_freq", "Remove", class = "btn-remove", style = "margin-left: 400px")
        ),
        tags$ul(
          tags$li(paste("From:", filters$Population_Frequency[1])),
          tags$li(paste("To:", filters$Population_Frequency[2]))
        )
      )
      }
    )
  })
  
  
  
  # Remove filter categories
  observeEvent(input$remove_acmg_criteria, {

    rv$selected_filters[["ACMG_Criteria"]] <- NULL

    shinyjs::enable("add_acmg")
    shinyjs::enable("acmg_criteria")
    shinyjs::enable("excluding_criteria_acmg")
  })
   
  observeEvent(input$remove_clinvar, {

    rv$selected_filters[["ClinVar"]] <- NULL

    shinyjs::enable("add_clinvar")
    shinyjs::enable("clinvar")
    shinyjs::enable("excluding_criteria_clinvar")
  })

  observeEvent(input$remove_zyg_criteria, {

    rv$selected_filters[["ZYG_Criteria"]] <- NULL

    shinyjs::enable("add_zyg")
    shinyjs::enable("zyg_criteria")
    shinyjs::enable("excluding_criteria_zyg")
  })

observeEvent(input$remove_pop_freq, {
  
  # Remove the Population Frequency filter from the selected filters list
  rv$selected_filters[["Population_Frequency"]] <- NULL
  
  # Re-enable the Population Frequency input fields
  shinyjs::enable("pop_freq_from")
  shinyjs::enable("pop_freq_to")
  shinyjs::enable("add_pop_freq")
})

  
  # Filter the data based on selected filters and excluding criteria
  filtered_data <- reactive({
    data <- rv$merged_data_all
    print(head(data))
    filters <- rv$selected_filters
    print(filters)
    if (!is.null(filters$ACMG_Criteria)) {
      if ("Exclude variants that match" %in% input$excluding_criteria_acmg) {
        data <- data[!data$ACMG %in% filters$ACMG_Criteria, ,drop=F]
      } else {
        data <- data[data$ACMG %in% filters$ACMG_Criteria, ,drop=F]
      }
    }
    
    if (!is.null(filters$ClinVar)) {
      if ("Exclude variants that match" %in% input$excluding_criteria_clinvar) {
        data <- data[!data$Clinvar %in% filters$ClinVar, ,drop=F]
      } else {
        data <- data[data$Clinvar %in% filters$ClinVar, ,drop=F]
      }
    }

    if (!is.null(filters$ZYG_Criteria)) {
      if ("Exclude variants that match" %in% input$excluding_criteria_zyg) {
        data <- data[!data$ZYG %in% filters$ZYG_Criteria, ,drop=F]
      } else {
        data <- data[data$ZYG %in% filters$ZYG_Criteria, ,drop=F]
      }
    }


    if (!all(is.na(filters$Population_Frequency))) {
      #print(data$MaxPopAF)
      print(length(data$MaxPopAF >= as.numeric(filters$Population_Frequency[1])))
      print(length(data$MaxPopAF <= as.numeric(filters$Population_Frequency[2])))
      print(filters$Population_Frequency[1])
      print(filters$Population_Frequency[2])
      data <- data[data$MaxPopAF >= as.numeric(filters$Population_Frequency[1]) ,, drop=F ]
      data <- data[ data$MaxPopAF <= as.numeric(filters$Population_Frequency[2]),, drop=F ]
    }

    return(data)
  })

  #Apply filters when "Apply" button is clicked
  observeEvent(input$apply_filter_drop_down, {

    filters_df <- read.csv("user_defined_filter.csv")
    
    # Extract the selected filter from the dropdown
    selected_filter <- input$dropdown  # Assuming dropdown contains the filter name
    print(unlist(str_split(selected_filter," - ")))
    selected_user<- unlist(str_split(selected_filter," - "))[1]
    selected_filter_name<- unlist(str_split(selected_filter," - "))[2]
    print("in 2")
    # Find the row in the CSV corresponding to the selected filter
    #filter_row <- filters_df[filters_df$filter_name == selected_filter_name & filters_df$filter_name == selected_user, ,drop=F]
    filter_row <- filters_df[filters_df$filter_name == selected_filter_name & filters_df$username == selected_user, , drop = FALSE]

    print(filter_row)
    #Warning: Error in if: missing value where TRUE/FALSE needed
    for (i in 1:nrow(filter_row)) {
        filter_column <- filter_row$columns_filter[i]
        print(filter_column)
        filter_options <- unlist(strsplit(filter_row$options[i], ", "))

        rv$selected_filters[[filter_column]] <- filter_options
    }
    print("in 2")

     
    print(nrow(filtered_data()))
    rv$merged_data_all <- filtered_data()
    
    shinyjs::hide("contextMenu")
  })

  
  #Apply filters when "Apply" button is clicked
  observeEvent(input$apply_filters, {
    print("in")
    rv$selected_filters <- list(
      #Excluding = input$excluding_criteria,
      ACMG_Criteria = input$acmg_criteria,
      # Function_Gene = input$function_gene,
      ZYG_Criteria = input$zyg_criteria,
      ClinVar = input$clinvar,
      Population_Frequency = c(input$pop_freq_from,input$pop_freq_to)
    )
    
    print(rv$selected_filters)

    print(nrow(filtered_data()))
    rv$merged_data_all <- filtered_data()
    
    removeModal()
  })
  
observeEvent(input$save_to_csv, {
  if (!is.null(input$username) && input$username != "" && !is.null(input$filter_name) && input$filter_name != "") {
    # Collecting filter data
    filters <- rv$selected_filters
    print(rv$selected_filters)
    
    # Load existing CSV if it exists
    if (file.exists("user_defined_filter.csv")) {
      existing_filters <- read.csv("user_defined_filter.csv")
    } else {
      existing_filters <- data.frame(
        username = character(),
        filter_name = character(),
        columns_filter = character(),
        options = character(),
        excluding = character() 
      )
    }
    
    # Check if the username and filter name combination already exists
    if (nrow(subset(existing_filters, username == input$username & filter_name == input$filter_name)) > 0) {
      showNotification("Filter already exists for this user", type = "warning")
    } else {
      # Create a data frame to store the data
      filter_data <- data.frame(
        username = character(),
        filter_name = character(),
        columns_filter = character(),
        options = character(),
        excluding = character() 
      )
      
      # Loop through the filters to add each filter as a separate row
      for (filter_name in names(filters)) {
        options <- paste(filters[[filter_name]], collapse = ", ")
        excluding <- ifelse(filter_name == "ACMG_Criteria" && "Exclude variants that match" %in% input$excluding_criteria_acmg, "yes", "no")
        filter_data <- rbind(filter_data, data.frame(
          username = input$username,
          filter_name = input$filter_name,
          columns_filter = filter_name,
          options = options,
          excluding = excluding
        ))
      }
      print(filter_data)
      # Save to CSV
      write.table(filter_data, "user_defined_filter.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = !file.exists("user_defined_filter.csv"))
      showNotification("User Name, Filter Name, and Filters saved to CSV", type = "message")
    }
  } else {
    showNotification("Please fill out both User Name and Filter Name", type = "error")
  }
})

  # Clear all filters and show the original data when "Remove Filters" is clicked
  observeEvent(input$tab_remove, {
    rv$selected_filters<- list()
    #print(rv$filtered_data_all)
    rv$merged_data_all<-rv$filtered_data_all
    
  })




    # Observe button clicks and show the modal with Intervar value
  observeEvent(input$selected_columns2_cell_clicked, {
    click_info <- input$selected_columns2_cell_clicked
    # print(click_info)
    #print(colnames(csv_data()))

    
    # Check if click_info is valid
    if (!is.null(click_info)) {
      row <- click_info$row
      col <- click_info$col
      #print(!is.null(row) && !is.null(col) && !is.na(row) && !is.na(col))
      
      # Ensure row and col are valid
      if (!is.null(row) && !is.null(col) && !is.na(row) && !is.na(col)) {
        # Assuming the ACMG column is the second column (index 2)
        column_name <- colnames(rv$merged_data_all[, input$columns, drop = FALSE])[col]  
        if (column_name == "ACMG") {
          intervar_value <- rv$merged_data_all$Intervar[row]
          
          acmg_params <- c("BA1", "BS1", "BS2", "BS3", "BS4", "BP1", "BP","BP3", "BP4", "BP5", "BP6", "BP7", "PM1", "PM2", "PM3", "PM4", "PM5", "PM6", "PP1", "PP2", "PP3", "PP4", "PP5", "PS1", "PS2", "PS3", "PS4", "PVS1" )


# Example intervar_value for testing (this would come from your app)
# intervar_value <- "BA1, PA1, BS2"  # This would be dynamically set based on the clicked cell

# Additional criteria (for illustrative purposes)
benign_criteria <- c("BA1", "BS1", "BS2", "BS3", "BS4", "BP1", "BP","BP3", "BP4", "BP5", "BP6", "BP7")  # Benign criteria
pathogenic_criteria <- c("PM1", "PM2", "PM3", "PM4", "PM5", "PM6", "PP1", "PP2", "PP3", "PP4", "PP5", "PS1", "PS2", "PS3", "PS4", "PVS1")  # Pathogenic criteria

# Create boxes with dynamic colors based on criteria
acmg_boxes <- lapply(acmg_params, function(param) {
    # Determine the color based on the criteria
    if (param %in% benign_criteria && grepl(param, intervar_value)) {
        color <- "green"  # Benign and present
    } else if (param %in% pathogenic_criteria && grepl(param, intervar_value)) {
        color <- "red"  # Pathogenic and present
    } else {
        color <- "grey"  # Absent
    }
    div(style = paste0("display: inline-block; 
                       padding: 10px; 
                       margin: 5px; 
                       background-color: ", color, "; 
                       color: white; 
                       border-radius: 5px;"),
        param)
})
          print(intervar_value)
          print(intervar_value)
 htmltext = "<style>
        table {
            width: 100%;
            border-collapse: separate;
            margin: 20px auto;
            border-radius: 15px; /* Round the table's outer corners */
            overflow: hidden;
        }
        table, th, td {
            border: 1px solid white;
        }
        th, td {
            padding: 10px;
            text-align: center;
            border-radius: 10px;
        }
        th {
            background-color: #f2f2f2;
        }
        td {
            background-color: lightgrey;
        }
    </style>
    <table id='myTable'>
        <tr>
            <th style='background-color: grey; color: white;'>Criteria</th>
            <th style='background-color: green; color: white;' colspan='7'>Benign</th>
            <th style='background-color: red; color: white;' colspan='5'>Pathogenic</th>
        </tr>
        <tr>
            <th style='background-color: white; color: black;'></th>
            <th style='background-color: lightgrey; color: black;' colspan='3'>Strong</th>
            <th style= 'background-color: lightgrey; color: black;' colspan='4'>Supporting</th>
            <th style='background-color: lightgrey; color: black;'>Supporting</th>
            <th style='background-color: lightgrey; color: black;' colspan='2'>Moderate</th>
            <th style='background-color: lightgrey; color: black;'>Strong</th>
            <th style='background-color: lightgrey; color: black;'>Very Strong</th>
        </tr>
        <tr>
            <th style='background-color: lightgrey; color: black;'>Population</th>
            <td>BA1</td>
            <td>BS1</td>
            <td>BS2</td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td>PM2</td>
            <td style='background-color: white;'></td>
            <td>PS4</td>
            <td style='background-color: white;'></td>
        </tr>
        <tr>
            <th style='background-color: lightgrey; color: black;'>Computer and Predicted</th>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td>BP1</td>
            <td>BP3</td>
            <td>BP4</td>
            <td>BP7</td>
            <td>PP3</td>
            <td>PM4</td>
            <td>PM5</td>
            <td>PS1</td>
            <td>PVS1</td>
        </tr>
         <tr>
            <th style='background-color: lightgrey; color: black;'>Functional</th>
            <td>BS3</td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td>PP2</td>
            <td>PM1</td>
            <td style='background-color: white;'></td>
            <td>PS3</td>
            <td style='background-color: white;'></td>
        </tr>
        <tr>
            <th style='background-color: lightgrey; color: black;'>Segregation</th>
            <td>BS4</td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td>PP1</td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
        </tr>
         <tr>
            <th style='background-color: lightgrey; color: black;'>De Novo</th>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td>PM6</td>
            <td style='background-color: white;'></td>
            <td>PS2</td>
            <td style='background-color: white;'></td>
        </tr>
         <tr>
            <th style='background-color: lightgrey; color: black;'>Allelic</th>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td>BP2</td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td>PM3</td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
        </tr>
         <tr>
            <th style='background-color: lightgrey; color: black;'>Other databases</th>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: lightgrey;'>BP6</td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td>PP5</td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
        </tr>
         <tr>
            <th style='background-color: lightgrey; color: black;'>Other data</th>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td>BP5</td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td>PP4</td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
            <td style='background-color: white;'></td>
        </tr>
    </table>"

            # Split the string into a vector of values
            intervar_values <- strsplit(intervar_value, ",\\s*")[[1]]

            # Iterate over each intervar value and replace it in the HTML string
            for (value in intervar_values) {
              if (startsWith(value, "P")) {
                # If the value starts with "P", set the background color to red
                htmltext <- str_replace_all(htmltext, paste0("<td>", value, "</td>"), 
                                            paste0("<td style='background-color: red; color: white;'>", value, "</td>"))
              } else {
                # Otherwise, set the background color to green
                htmltext <- str_replace_all(htmltext, paste0("<td>", value, "</td>"), 
                                            paste0("<td style='background-color: green; color: white;'>", value, "</td>"))
              }
            }
          output$table_in_modal <- renderUI({
    HTML(htmltext)
  })
          showModal(modalDialog(
            title = "ACMG Guidelines",
            uiOutput("table_in_modal"), 
            paste("Fulfilled creteria:", intervar_value),
            # div(style = "display: flex; flex-wrap: wrap;", acmg_boxes),
            # tags$img(src = "acmg_guidlines.png", width = "400px", height = "500px"),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }
        #   showModal(modalDialog(
        #     title = "ACMG Guidelines",
        #     paste("Fulfilled creteria:"),
        #     div(style = "display: flex; flex-wrap: wrap;", acmg_boxes),
        #     tags$img(src = "acmg_guidlines.png", width = "400px", height = "500px"),
        #     easyClose = TRUE,
        #     footer = NULL
        #   ))
        # }
        else if (column_name == "LOCATION") {

            updateTabItems(session, "tabs2", selected = "genome_browser")

            removeUserAddedTracks(session, id="igvShiny_0")

            location <- rv$merged_data_all$LOCATION[row]


    
            updateSelectInput(session, "select_job_id_gb1", choices = c(input$select_job_id_r3),selected =input$select_job_id_r3)
            
            
            job_id <- input$select_job_id_r3
            
            selected_job <- job_data[job_data$Job_ID == input$select_job_id_r3, ]
            
            folder_path <- paste0("OncoGenAI/results/", job_id, "/")
            genome <- selected_job$Genome

            # SNPs
            file_path <- paste0(folder_path, job_id,  "_bed_filtered.merged.vcf")
            #if(input$workflow == "Illumina"){

            #}
            
            #vcf_file <- "/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/OncoGenAI/results/Oncogen202408200001/Oncogen202408200001_bed_filtered.snps.vcf"
            vcf <- readVcf(file_path, "hg19")

            print(location)
            if (grepl("-", location)) {
              # Case for indel (range of positions)
              parts <- unlist(strsplit(location, "[:-]"))
              chromosome <- parts[1]
              start_pos <- as.numeric(parts[2])
              end_pos <- as.numeric(parts[3])
            } else {
              # Case for SNP (single position)
              parts <- unlist(strsplit(location, ":"))
              chromosome <- parts[1]
              start_pos <- as.numeric(parts[2])
              end_pos <- start_pos
            }

            start_pos2 <- max(start_pos - 50, 1)
            end_pos2 <- end_pos + 50
            showGenomicRegion(session, id="igvShiny_0", sprintf("%s:%d-%d", chromosome,start_pos2-1000, end_pos2+1000))

            #showGenomicRegion(session, id="igvShiny_0", sprintf("chr19:%d-%d", 33299144-1000, 33300107+1000))
            loadVcfTrack(session, id="igvShiny_0", trackName=job_id, vcf)
            
            print(selected_job)
            # Replace with your input location (can be either for indel or SNP)
            if(selected_job$Workflow == "IonTorrent"){
              bam_file <- paste0("itor_example/", selected_job$Uploaded.Files)
              print(selected_job)
              print(bam_file)
            }
            else{
              bam_file <- paste0(folder_path, job_id,  "_sorted_dedup_reads.bam") # Replace with your BAM file path
            }
            
            output_bam <- "output_subset.bam"  # Replace with your desired output BAM file path

            # Extract chromosome and position information

            # Calculate start and end positions, ensuring start is at least 1 and adjusting for SNP or indel
            start_pos <- max(start_pos - 10000, 1)
            end_pos <- end_pos + 10000

            # One-liner for bash command to subset BAM file
            command <- sprintf("samtools-1.20/samtools view -b %s %s:%d-%d > %s", 
                              bam_file, 
                              chromosome, 
                              start_pos, 
                              end_pos, 
                              output_bam)



            # Execute the command in R
            system(command)
            index_command <- sprintf("samtools-1.20/samtools index %s", output_bam)
            system(index_command)
            print("BAM subseted")
            
            bam<- readGAlignments("output_subset.bam")
            loadBamTrackFromLocalData(session, id="igvShiny_0", trackName = "Alignment",bam )    
            
        }
      }
    }
  })
  

          observeEvent(input$searchButton, {
                searchString = isolate(input$roi)
                printf("--- search: %s", searchString)
                if(nchar(searchString) > 0)
                  showGenomicRegion(session, id="igvShiny_0", searchString)
              })
              observeEvent(input$removeUserTracksButton, {
                #printf("---- removeUserTracks")
                removeUserAddedTracks(session, id="igvShiny_0")
              })
              observeEvent(input[["igv-trackClick"]], {
                #printf("--- igv-trackClick event")
                x <- input[["igv-trackClick"]]
                attribute.name.positions <- grep("name", names(x))
                attribute.value.positions <- grep("value", names(x))
                attribute.names <- as.character(x)[attribute.name.positions]
                attribute.values <- as.character(x)[attribute.value.positions]
                tbl <- data.frame(name=attribute.names,
                                  value=attribute.values,
                                  stringsAsFactors=FALSE)
                dialogContent <- renderTable(tbl)
                html <- HTML(dialogContent())
                showModal(modalDialog(html, easyClose=TRUE))
              })
              
              observeEvent(input$getChromLocButton, {
                # printf("--- getChromLoc event")
                # sends message to igv.js in browser; currentGenomicRegion.<id> event sent back
                # see below for how that can be captured and displayed
                getGenomicRegion(session, id="igvShiny_0")
              })
              
              observeEvent(input$clearChromLocButton, {
                output$chromLocDisplay <- renderText({" "})
              })
              
              observeEvent(input[[sprintf("currentGenomicRegion.%s", "igvShiny_0")]], {
                newLoc <- input[[sprintf("currentGenomicRegion.%s", "igvShiny_0")]]
                #printf("new chromLocString: %s", newLoc)
                output$chromLocDisplay <- renderText({newLoc})
              })
              
              genomes <- c("hg38", "hg19", "mm10", "tair10", "rhos")
              loci <- c("chr5:88,466,402-89,135,305", "MEF2C", "Mef2c", "1:7,432,931-7,440,395", "NC_007494.2:370,757-378,078")
              i <- 2
              
                output$igvShiny_0 <- renderIgvShiny({
                  genomeOptions <- parseAndValidateGenomeSpec(genomeName="hg19",  initialLocus=loci[i])
                  igvShiny(genomeOptions)
                })
                
                gtd <- get_tracks_dir(env_var = "TRACKS_DIR")
                gtd2<- paste0("/", gtd)
                shiny::addResourcePath(gtd2,gtd)
  
  observeEvent(input$select_job_id_gb1, {
    req(input$select_job_id_gb1)

  })



  
  
  # Save selected rows to a text file
  observeEvent(input$save_button, {
    selected_rows <- input$selected_columns2_rows_selected
    selected_columns <- input$columns
    if (length(selected_rows) > 0) {
      #selected_data <- csv_data()[selected_rows, selected_columns, drop = FALSE]
      #write.table(selected_data, file = "rep_file.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
      pathogenic_df2 <- rv$merged_data_all[selected_rows, selected_columns, drop = FALSE]
      pathogenic_df2 <- pathogenic_df2 %>%
        separate_rows(AA, sep = ",") %>%   # Split the protein changes based on comma
        mutate(
          Protein_Name = str_extract(AA, "^[^:]+"),           # Extract protein name (before the first colon)
          Protein_Change_Detail = str_extract(AA, "p\\.[^,]+") # Extract the first protein change detail (starts with 'p.')
        ) 
      
      job_id <- input$select_job_id_r3
      folder_path <- paste0("OncoGenAI/results/", job_id, "/")
      new_df <- data.frame(Protein_Name = pathogenic_df2$Protein_Name,
                           Protein_Change_Detail = pathogenic_df2$Protein_Change_Detail,
                           Query = paste(pathogenic_df2$Protein_Name, pathogenic_df2$Protein_Change_Detail))
      print(head(new_df))
      write.csv(new_df,paste0(folder_path, job_id,  "_protein_change.csv"))
      
      #  showModal(modalDialog(
      #   title = "Success",
      #   "Selected rows have been saved ",
      #   easyClose = TRUE,
      #   footer = NULL
      # ))
      showNotification("Selected rows have been saved.", type = 'message')
      shinyjs::enable("query_kg_button")
    } else {
    # } else {
    # showNotification("No rows selected. Please select the rows.", type = "warning")
    shinyjs::alert("No rows selected. Please select the rows.")
      # showModal(modalDialog(
      #   title = "No Rows Selected",
      #   "Please select rows to save.",
      #   easyClose = TRUE,
      #   footer = NULL
      # ))
    }
  })
  
  observeEvent(input$query_kg_button, {
    updateTabItems(session, "tabs2", selected = "kgimputation")
    updateSelectInput(session, "select_job_id_kg", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected =input$select_job_id_r3 )
  })
  
  # observeEvent(input$generate_report, {
  #   python_path <- "/home/suvenduk/.conda/envs/myenv/bin/python3"
  #   script_path <- "report_generatr.py"
  #   cp /16Tbdrive1/sanjaym/RGCIRC/Rsubread/Final_oncogen/report_generatr.py .
    
  #   # Run the Python script and capture the output
  #   output <- system2(python_path, script_path, stdout = TRUE, stderr = TRUE)
    
  #   # Print the output to the R console
  #   cat(output, sep = "\n")
    
  #   shinyjs::enable("view_report")
    
  #   showNotification("Report generated", type = "message")
  # })
  
  
  ########################################### genome browser #########################
  
  
  # observeEvent(input$submit_IGV, {
  #   oncogen_id <- input$select_job_id_r3
    
  #   # Construct the URL based on the Oncogen ID
  #   folder <- paste0("OncoGenAI/results/", oncogen_id)
  #   bam <- paste0(oncogen_id, "_bed.recal.bam")
  #   bai <- paste0(oncogen_id, "_bed.recal.bam.bai")
  #   vcf <- paste0(oncogen_id, "_bed_filtered.merged.vcf")
  #   tbi <- paste0(oncogen_id, "_bed_filtered.merged.vcf.idx")
    
  #   # Construct the URL with query parameters
  #   url <- paste0("/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/IGV_viewer.html?id=", oncogen_id)
    
  #   # Redirect to the constructed URL
  #   print(url)
  #   url = paste0("http://192.168.51.235:8080/",url)
  #   output$url_text <- renderText({
  #     paste(url," ")
  #   })
  # })
  
  
  ####################################################################################
   
  # Save selected rows to a text file
  # observeEvent(input$save_button, {
  #   selected_rows <- input$selected_columns_rows_selected
  #   if (length(selected_rows) > 0) {
      
  #     selected_data <- rv$pathogenic_data
  #     write.table(selected_data, file = "rep_file.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  #     showModal(modalDialog(
  #       title = "Success",
  #       "Selected rows have been saved as Query",
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #   } else {
  #     showModal(modalDialog(
  #       title = "No Rows Selected",
  #       "Please select rows to save.",
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #   }
  # })
  

  
  # View report
  observeEvent(input$view_report, {
    updateTabItems(session, "tabs2", selected = "reportgeneration")
  })
  
  
  ##################end######################
  

  #############################  KG imputation   ################################
  # display Patient ID when Job ID is selected-> kg_imputation page
  #updateSelectInput(session, "select_job_id_rpt", choices = c("", job_data$Job_ID) )
  
  # display Patient ID when Job ID is selected-> kg_imputation page
  observeEvent(input$select_job_id_kg, {
    req(input$select_job_id_kg)
    if (input$select_job_id_kg != "") {
      selected_job <- job_data[job_data$Job_ID == input$select_job_id_kg, ]
      if (nrow(selected_job) == 1) {
        patient_id <- selected_job$Patient_ID
        output$display_patient_id_kg <- renderText({ paste("Patient ID:", patient_id) })
      } else {
        showNotification("Job ID not found", type = "error")
      }
    } else {
      output$display_patient_id_kg <- renderText({ "" })
    }
    
    print(input$select_job_id_kg)
    job_id <- input$select_job_id_kg

    selected_job <- job_data[job_data$Job_ID == input$select_job_id_kg, ]

  
    folder_path <- paste0("OncoGenAI/results/", job_id, "/")

    file_path   <- paste0(folder_path, job_id,  "_kg_results.csv")
    system(paste0("python3 query_kg.py ",folder_path,job_id))
    # _kg_results.csv
    #_protein_change
    #print(read.csv(file_path))
    report_df <- read.csv(file_path)


    report_df <- report_df %>%
      distinct(Query, .keep_all = TRUE)


    rv$report_data <- report_df
    
    # print("h")

    names(rv$report_data)[names(rv$report_data) == 'Query'] <- 'QUERY'
    names(rv$report_data)[names(rv$report_data) == 'ClinicalSignificance'] <- 'CLINICAL SIGNIFICANCE'
    names(rv$report_data)[names(rv$report_data) == 'ReviewStatus'] <- 'REVIEW STATUS'
    names(rv$report_data)[names(rv$report_data) == 'FDA_levels'] <- 'FDA LEVEL'
    names(rv$report_data)[names(rv$report_data) == 'Therapeutic_level'] <- 'THERAPEAUTIC LEVEL'
    names(rv$report_data)[names(rv$report_data) == 'Prognostic_level'] <- 'PROGNOSTIC LEVEL'
    names(rv$report_data)[names(rv$report_data) == 'Diagnostic_level'] <- 'DIAGNOSTIC LEVEL'
    names(rv$report_data)[names(rv$report_data) == 'Its_Nucleotide_Query'] <- 'NUCLEOTIDE QUERY'
    names(rv$report_data)[names(rv$report_data) == 'LEGACY_MUTATION_ID'] <- 'LEGACY MUTATION ID' 
    names(rv$report_data)[names(rv$report_data) == 'CLINVAR_CLNSIG'] <- 'CLINVAR SIGNIFICANCE'
    names(rv$report_data)[names(rv$report_data) == 'CLINVAR_TRAIT'] <- 'CLINVAR TRAIT'
    names(rv$report_data)[names(rv$report_data) == 'Mutation_CDS'] <- 'MUTATION CDS'
    names(rv$report_data)[names(rv$report_data) == 'treatment'] <- 'TREATMENT'
    #Query,ClinicalSignificance,ReviewStatus,FDA_levels,Therapeutic_level,Prognostic_level,Diagnostic_level,treatment
    # rv$report_data <- rv$report_data[,c('QUERY', 'LEGACY MUTATION ID', 'CLINVAR SIGNIFICANCE', 'CLINVAR TRAIT', 'REVIEW STATUS', 'FDA LEVEL', 'THERAPEAUTIC LEVEL', 'PROGNOSTIC LEVEL', 'DIAGNOSTIC LEVEL', 'MUTATION CDS', 'TREATMENT', 'NUCLEOTIDE QUERY', 'CLINICAL SIGNIFICANCE')]
    # Check if all required columns exist before subsetting
    # required_columns <- c("QUERY","CLINICAL SIGNIFICANCE","REVIEW STATUS",        
    #                     "FDA LEVEL","THERAPEAUTIC LEVEL","PROGNOSTIC LEVEL", 
    #                     "DIAGNOSTIC LEVEL","TREATMENT")
    required_columns <- c('QUERY', 'LEGACY MUTATION ID', 'CLINVAR SIGNIFICANCE', 'CLINVAR TRAIT', 
                          'REVIEW STATUS', 'FDA LEVEL', 'THERAPEAUTIC LEVEL', 'PROGNOSTIC LEVEL', 
                          'DIAGNOSTIC LEVEL', 'MUTATION CDS', 'TREATMENT', 'NUCLEOTIDE QUERY')

    # Verify if all required columns exist in the dataframe
    # missing_columns <- setdiff(required_columns, names(rv$report_data))
    # if (length(missing_columns) > 0) {
    #   stop(paste("The following columns are missing:", paste(missing_columns, collapse = ", ")))
    # }

    # Subset the dataframe to only include the required columns
    print(colnames(rv$report_data))
    rv$report_data <- rv$report_data[, required_columns]
    output$kg_table <- renderDataTable({
      datatable(rv$report_data, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'csv',
            text = '<i class="fa fa-download"></i>',
            titleAttr = 'Download CSV'
          )
        ),
        scrollX = TRUE,
        scrollY = TRUE,
        autoWidth = TRUE
      ), selection = 'single')
    })
    
    shinyjs::disable("report_generation_button")
    
    
  })
  ### kg imputation page
  
  # For report generation page
  observeEvent(input$select_job_id_rpt, {
    req(input$select_job_id_rpt)
    if (input$select_job_id_rpt != "") {
      selected_job <- job_data[job_data$Job_ID == input$select_job_id_rpt, ]
      if (nrow(selected_job) == 1) {
        patient_id <- selected_job$Patient_ID
        output$display_patient_id_rpt <- renderText({ paste("Patient ID:", patient_id) })
        
        # Read the text file and update the text area
        job_id <- input$select_job_id_rpt
    
        selected_job <- job_data[job_data$Job_ID == input$select_job_id_rpt, ]

        folder_path <- paste0("OncoGenAI/results/", job_id, "/")
        kg_file_path <- paste0(folder_path,"selected_row_sentence_", input$select_job_id_rpt, ".txt")
        
        
        if(file.exists(kg_file_path)){
          file_content <- readLines(kg_file_path, warn = FALSE)
          updateTextAreaInput(session, "file_content", value = paste(file_content, collapse = "\n"))
        }else {
          showNotification("No file found for the selected Job ID, please select another Job  ID", type = "warning")
          updateTextAreaInput(session, "file_content", value = "")
        }
      } else {
        showNotification("Job ID not found", type = "error")
      }
    } else {
      output$display_patient_id_rpt <- renderText({ "" })
    }
    
    # Disable the "Generate Report" button since the content might have changed
    shinyjs::disable("generate_report")
    shinyjs::disable("get_pdf")
  })
  
  
  ####### save the text in the text area input into the text file ##############
  observeEvent(input$save,{
    req(input$file_content)
    
    job_id <- input$select_job_id_rpt

    selected_job <- job_data[job_data$Job_ID == input$select_job_id_kg, ]
    
    folder_path <- paste0("OncoGenAI/results/", job_id, "/")
    prompt_file_path <- paste0(folder_path,"selected_row_sentence_edited_", input$select_job_id_rpt, ".txt")
    
    
    writeLines(input$file_content, con = prompt_file_path)
    
    showNotification("Text saved to report.txt", type = "message")
    
    # Enable the "Generate Report" button after saving
    shinyjs::enable("generate_report")
  })
  ###############   #################################
  
  # Disable "Generate Report" button if the user changes the text area input
  observeEvent(input$file_content, {
    shinyjs::disable("generate_report")
  })
  
  # Render network graph based on selected row
  observeEvent(input$kg_table_rows_selected, {
    selected_row <- input$kg_table_rows_selected
    if (length(selected_row)) {
      row_data <- rv$report_data[selected_row, ]
      
      # Create nodes with column names as labels
      nodes <- data.frame(
        id = c("Query", "Legacy_Mutation",    "Clinvar_Clnsig", "Clinvar_Trait",    "Mutation_CDS",    "Drug_Name",    "Drug_Response"),
        label = c(as.character(row_data[1]), "Legacy_Mutation",    "Clinvar_Clnsig", "Clinvar_Trait",    "Mutation_CDS",    "Drug_Name",    "Drug_Response"),
        title = c(NA, as.character(row_data[2]), as.character(row_data[3]), as.character(row_data[4]), as.character(row_data[5]), as.character(row_data[6]), as.character(row_data[7]))
      )
      
      # Create edges from the ID node to the attribute nodes
      edges <- data.frame(
        from = rep("Query", 6),
        to = c("Legacy_Mutation",    "Clinvar_Clnsig", "Clinvar_Trait",    "Mutation_CDS",    "Drug_Name",    "Drug_Response")
      )
      
      output$network <- renderVisNetwork({
        visNetwork(nodes, edges, width= "100%") %>%
          visNodes(shape = "circle", size = 20) %>%
          visEdges(arrows = 'to') %>%
          visOptions(highlightNearest = F, nodesIdSelection = F)
      })
    }
  })
  
  # Save selected row and update the CSV file
  observeEvent(input$save_row, {

    if (is.null(input$kg_table_rows_selected) || length(input$kg_table_rows_selected) == 0) {

    # If no row is selected, show a notification
    # showNotification("Please select a row before saving.", type = "error")
    shinyjs::alert("No rows selected. Please select the rows.")
    return()  # Exit the function early if no row is selected
   }

  
    req(input$kg_table_rows_selected, input$select_job_id_kg)
    
    # Get the selected row data
    print(input$kg_table_rows_selected)
    row_data <- rv$report_data[input$kg_table_rows_selected, ,drop=F]
    print(row_data)
    # Generate the sentence using paste0
    ## if condition
    # Remove "not specified" and "not provided" from the CLINICAL_TRAIT column in row_data
    # row_data$CLINICAL_TRAIT <- gsub("not specified|not provided", "", row_data$CLINICAL_TRAIT)

    # # Optionally, remove any extra "&" symbols left from the removal
    # row_data$CLINICAL_TRAIT <- gsub("&+", "&", row_data$CLINICAL_TRAIT)  # Removes consecutive '&'
    # row_data$CLINICAL_TRAIT <- gsub("^&|&$", "", row_data$CLINICAL_TRAIT)  # Removes leading or trailing '&'

    # # Trim any leading or trailing whitespace
    # row_data$CLINICAL_TRAIT <- trimws(row_data$CLINICAL_TRAIT)
    # Check the value of 'Query' and handle NA or empty cases
    # Check the value of 'Query' and handle NA or empty cases
    # Check the value of 'Query' and handle NA or empty cases
    # names(rv$report_data)[names(rv$report_data) == 'Query'] <- 'QUERY'
    # names(rv$report_data)[names(rv$report_data) == 'ClinicalSignificance'] <- 'CLINICAL SIGNIFICANCE'
    # names(rv$report_data)[names(rv$report_data) == 'ReviewStatus'] <- 'REVIEW STATUS'
    # names(rv$report_data)[names(rv$report_data) == 'FDA_levels'] <- 'FDA LEVEL'
    # names(rv$report_data)[names(rv$report_data) == 'Therapeutic_level'] <- 'THERAPEAUTIC LEVEL'
    # names(rv$report_data)[names(rv$report_data) == 'Prognostic_level'] <- 'PROGNOSTIC LEVEL'
    # names(rv$report_data)[names(rv$report_data) == 'Diagnostic_level'] <- 'DIAGNOSTIC LEVEL'
    # names(rv$report_data)[names(rv$report_data) == 'Its_Nucleotide_Query'] <- 'NUCLEOTIDE QUERY'
    # names(rv$report_data)[names(rv$report_data) == 'LEGACY_MUTATION_ID'] <- 'LEGACY MUTATION ID' 
    # names(rv$report_data)[names(rv$report_data) == 'CLINVAR_CLNSIG'] <- 'CLINVAR SIGNIFICANCE'
    # names(rv$report_data)[names(rv$report_data) == 'CLINVAR_TRAIT'] <- 'CLINVAR TRAIT'
    # names(rv$report_data)[names(rv$report_data) == 'Mutation_CDS'] <- 'MUTATION CDS'
    # names(rv$report_data)[names(rv$report_data) == 'treatment'] <- 'TREATMENT'
    # if (is.na(row_data[["QUERY"]]) || row_data[["QUERY"]] == "" || row_data[["QUERY"]] == "NA") {
    #   sentence1 <- "Write a paragraph in 300 words for mutation not known"
    # } else {
    #   sentence1 <- paste0("Write a paragraph in 300 words for mutation ", row_data[["QUERY"]])
    # }

    # # Check the value of 'Drug_x' and handle NA or empty cases
    # if (is.na(row_data[["TREATMENT"]]) || row_data[["TREATMENT"]] == "" || row_data[["TREATMENT"]] == "NA") {
    #   sentence2 <- " with unknown drug history "
    # } else {
    #   # Assuming you want to use 'CLINVAR_TRAIT' for the drug history sentence
    #   sentence2 <- paste0(" with known drug history with ", row_data[["CLINVAR_TRAIT"]])
    # }

    print("before if")

    # generate_sentence <- function(row) {
    #   paste(
    #     "Write a paragraph in 300 words for mutation ", row$QUERY, 
    #     if (row$`LEGACY MUTATION ID` != "") paste("with legacy ID", row$`LEGACY MUTATION ID`) else NULL, 
    #     if (row$`CLINVAR SIGNIFICANCE` != "") paste("is classified as", row$`CLINVAR SIGNIFICANCE`) else NULL, 
    #     if (row$`CLINVAR TRAIT` != "") paste("associated with the traits", row$`CLINVAR TRAIT`) else NULL, 
    #     if (row$`REVIEW STATUS` != "") paste("and has a review status of", row$`REVIEW STATUS`) else NULL, 
    #     if (row$`FDA LEVEL` != "Not found") paste("with FDA level", row$`FDA LEVEL`) else NULL, 
    #     if (row$`THERAPEAUTIC LEVEL` != "Not found") paste("therapeutic level", row$`THERAPEAUTIC LEVEL`) else NULL, 
    #     if (row$`PROGNOSTIC LEVEL` != "Not found") paste("prognostic level", row$`PROGNOSTIC LEVEL`) else NULL, 
    #     if (row$`DIAGNOSTIC LEVEL` != "Not found") paste("and diagnostic level", row$`DIAGNOSTIC LEVEL`) else NULL, 
    #     if (row$`MUTATION CDS` != "") paste("Mutation CDS is", row$`MUTATION CDS`) else NULL, 
    #     if (row$`NUCLEOTIDE QUERY` != "") paste("with nucleotide query", row$`NUCLEOTIDE QUERY`) else NULL, 
    #     if (row$`CLINICAL SIGNIFICANCE` != "") paste("and clinical significance of", row$`CLINICAL SIGNIFICANCE`) else NULL, 
    #     "."
    #   )
    # }
    generate_sentence <- function(row) {
      paste(
        "Write a paragraph in 300 words for mutation", row$QUERY, 
        if (!is.null(row$`LEGACY MUTATION ID`) && nzchar(row$`LEGACY MUTATION ID`)) paste("with legacy ID", row$`LEGACY MUTATION ID`) else NULL, 
        if (!is.null(row$`CLINVAR SIGNIFICANCE`) && nzchar(row$`CLINVAR SIGNIFICANCE`)) paste("is classified as", row$`CLINVAR SIGNIFICANCE`) else NULL, 
        if (!is.null(row$`CLINVAR TRAIT`) && nzchar(row$`CLINVAR TRAIT`)) paste("associated with the traits", row$`CLINVAR TRAIT`) else NULL, 
        if (!is.null(row$`REVIEW STATUS`) && nzchar(row$`REVIEW STATUS`) && row$`THERAPEAUTIC LEVEL` != "Not found") paste("and has a review status of", row$`REVIEW STATUS`) else NULL, 
        if (!is.null(row$`FDA LEVEL`) && row$`FDA LEVEL` != "Not found") paste("with FDA level", row$`FDA LEVEL`) else NULL, 
        if (!is.null(row$`THERAPEAUTIC LEVEL`) && row$`THERAPEAUTIC LEVEL` != "Not found") paste("therapeutic level", row$`THERAPEAUTIC LEVEL`) else NULL, 
        if (!is.null(row$`PROGNOSTIC LEVEL`) && row$`PROGNOSTIC LEVEL` != "Not found") paste("prognostic level", row$`PROGNOSTIC LEVEL`) else NULL, 
        if (!is.null(row$`DIAGNOSTIC LEVEL`) && row$`DIAGNOSTIC LEVEL` != "Not found") paste("and diagnostic level", row$`DIAGNOSTIC LEVEL`) else NULL, 
        if (!is.null(row$`MUTATION CDS`) && nzchar(row$`MUTATION CDS`)) paste("Mutation CDS is", row$`MUTATION CDS`) else NULL, 
        if (!is.null(row$`NUCLEOTIDE QUERY`) && nzchar(row$`NUCLEOTIDE QUERY`)) paste("with nucleotide query", row$`NUCLEOTIDE QUERY`) else NULL, 
        if (!is.null(row$`CLINICAL SIGNIFICANCE`) && nzchar(row$`CLINICAL SIGNIFICANCE`)) paste("and clinical significance of", row$`CLINICAL SIGNIFICANCE`) else NULL, 
        "."
      )
    }

    job_id <- input$select_job_id_kg
    
    selected_job <- job_data[job_data$Job_ID == input$select_job_id_kg, ]
    
    folder_path <- paste0("OncoGenAI/results/", job_id, "/")

    
    #sentence = paste0(sentence1, sentence2)#, sentence3, sentence4, sentence5)

    sentence = generate_sentence(row_data)
    print("after if")
    # Write the sentence to the text file
    
    
    file_name <- paste0(folder_path,"selected_row_sentence_", input$select_job_id_kg, ".txt")
    write(sentence, file = file_name)
    
    showNotification(paste("Row saved to", file_name), type = "message")
    shinyjs::enable("report_generation_button")
    print("end")
  })
  observeEvent(input$report_generation_button, {
    updateTabItems(session, "tabs2", selected = "reportgeneration")
    updateSelectInput(session, "select_job_id_rpt", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected =input$select_job_id_kg )
  })
  ### end of kg imputaion page##
  
  
  
  
  output$table1 <- renderDataTable({
    datatable(read.csv("samita_tab/samita-bed-filtered-snps.csv"), extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'csv',
          text = '<i class="fa fa-download"></i>',
          titleAttr = 'Download CSV'
        )
      ),
      scrollX = TRUE,
      scrollY = TRUE,
      autoWidth = TRUE
    ))
  })
  
  ### end of table1
  
  ### table2 ####
  
  output$table2 <- renderDataTable({
    datatable(read.csv("samita_tab/samita-bed-filtered-indels.csv"), extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'csv',
          text = '<i class="fa fa-download"></i>',
          titleAttr = 'Download CSV'
        )
      ),
      scrollX = TRUE,
      scrollY = TRUE,
      autoWidth = TRUE
    ))
  })
  
  ###end of table2 ###
  
  ### table3 ####
  
  output$table3 <- renderDataTable({
    datatable(read.csv("samita_tab/86886_SAMITA_S2_bed_filtered.merged-vep.csv"), extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'csv',
          text = '<i class="fa fa-download"></i>',
          titleAttr = 'Download CSV'
        )
      ),
      scrollX = TRUE,
      scrollY = TRUE,
      autoWidth = TRUE
    ))
  })
  
  ###end of table3 ###
  
  ### row selection csv ###
  
  ### table4###
  
  csv_file_path <- "SAMITA-merged.hg38_multianno.intervar_clmremoved_kg.csv"
  csv_data <- reactive({
    read.csv(csv_file_path)
  })
  
  column_names <- reactive({
    names(csv_data())
  })
  
  
  # Function to update the Python file 
  update_python_file <- function(job_id) {
    # Specify the path to the Python file
    python_file_path <- "genai.py"
    
    # Read the Python file content
    python_file_content <- readLines(python_file_path)
    
    # job_id <- input$select_job_id_kg
    
    # selected_job <- job_data[job_data$Job_ID == input$select_job_id_kg, ]
    
    folder_path <- paste0("OncoGenAI/results/", job_id, "/")
    file_name <- paste0(folder_path,"selected_row_", job_id, ".txt")
    
    # Update the first line with the new kg_file_path
    
    python_file_content[1] <- paste0("promt_fil= \"",file_name,"\"")
    
    # Write the updated content back to the Python file
    writeLines(python_file_content, python_file_path)
    
    #print(python_file_content)
  }
  ########### end of the function #####################

  observeEvent(input$generate_report, {
    req(input$select_job_id_rpt)
    req(input$file_content)
    
    if (input$select_job_id_rpt != "" && input$file_content != "") {
      # Create a file path with the selected Job ID
      # file_name <- paste0(input$select_job_id_rpt, "_prompt.txt")
      job_id <- input$select_job_id_rpt
      folder_path <- paste0("OncoGenAI/results/", job_id, "/")
      prompt_file_path <- paste0(folder_path,"selected_row_sentence_edited_", input$select_job_id_rpt, ".txt")
      # file_path <- file.path("OncoGenAI/results/", file_name)
      
      # Optional: Notify user of successful file creation
      showNotification(paste("Report saved as", prompt_file_path), type = "message")
      
      # Optionally, update the generated report text area
      output$report_content <- renderText({ paste("Report saved in:", file_name) })
    } else {
      showNotification("Please select a Job ID and enter text before generating the report.", type = "warning")
    }
    # OncoGenAI/results/Oncogen202409030002/selected_row_sentence_edited_Oncogen202409030002.txt
    
    # Run the Python script and capture the output
    #python_path <- "/home/suvenduk/.conda/envs/myenv/bin/python3"
    script_path <- paste0("genai.py ",prompt_file_path)
    
    output <- system2("python3", script_path, stdout = TRUE, stderr = TRUE)
    cat(output, sep = "\n")
    
    
    report_file_path <- "OncoGenAI/generated_report.txt"
    if (file.exists(report_file_path)) {
      report_content <- readLines(report_file_path, warn = FALSE)
      updateTextAreaInput(session, "report_content", value = paste(report_content, collapse = "\n"))
    } else {
      showNotification("Generated report file not found.", type = "error")
    }

    shinyjs::enable("view_report")
    
    showNotification("Report generated", type = "message")
  })
  
  
  
  
  ####### save the text in the text area input into the text file ##############
  observeEvent(input$save_report,{
    req(input$report_content)
    
    
    pdf_file_path <- file.path(getwd(), "pdf_file.txt")
    
    writeLines(input$report_content, con = pdf_file_path)
    
    showNotification("Text saved to pdf", type = "message")
    
    # Enable the "Generate Report" button after saving
    shinyjs::enable("get_pdf")
  })
  ###############   #################################
  
  # Disable "Generate Report" button if the user changes the text area input
  observeEvent(input$report_content, {
    shinyjs::disable("get_pdf")
  })
  
  # ### downlaod report############
  # output$get_pdf <- downloadHandler(
  #   filename = function() {
  #     paste('patient_report', sep = '.', 'pdf')
  #   },
  #   content = function(file) {
  #     # Path to your text file
  #     text_file_path <- "pdf_file.txt"
  #     # Read the content of the text file
  #     text_content <- readLines(text_file_path)
      
  #     # Create a temporary RMarkdown file to convert text to PDF
  #     rmd_file <- tempfile(fileext = ".Rmd")
  #     writeLines(c(
  #       "---",
  #       "title: ''",  # Empty title as we will add title manually in the body
  #       "output: pdf_document",
  #       "---",
  #       "",
  #       "![](", "/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/rajeev.jpeg", ")",  # Include image
  #       "",
  #       "# \\textbf{\\LARGE RAJIV GANDHI CANCER INSTITUTE AND RESEARCH CENTRE}",
  #       "",
  #       "## Patient File Report",
  #       "",
  #       paste(text_content, collapse = "\n")
  #     ), con = rmd_file)
      
  #     # Render the temporary RMarkdown file to PDF
  #     out <- render(rmd_file, pdf_document())
      
  #     # Move the generated PDF to the specified location
  #     file.rename(out, file)
  #   }
  # )
  
  ######################end#################################
  
  ###################################################
  
  ################################   results3 tabular view #################################
  
 
  
  
  
  ###########################################################################################
  
  # 
  observeEvent(input$select_job_id_r1,{
    
    req(input$select_job_id_r1)
    job_id = input$select_job_id_r1
    folder_path = paste0("OncoGenAI/results/",job_id,"/")
    
    
    
    output$pieChart1 <- renderPlotly({
      file_path = paste0(folder_path,job_id,"merged_all_varClasses.csv")
      data = read.csv(file_path)
      #plot_ly(type = 'pie', labels = c("stop_gained", "missense_variant", "synonymous_variant", "others"), values = c(33, 8677, 13275, 38))
      plot_ly(
        data,
        labels = ~category,
        values = ~value,
        type = 'pie',
        textinfo = 'label+percent'
      )
    })
    
    
    output$pieChart2 <- renderPlotly({
      file_path = paste0(folder_path,job_id,"merged_all_consequences.csv")
      data = read.csv(file_path)
      #plot_ly(type = 'pie', labels = c("stop_gained", "missense_variant", "synonymous_variant", "others"), values = c(33, 8677, 13275, 38))
      plot_ly(
        data,
        labels = ~category,
        values = ~value,
        type = 'pie',
        textinfo = 'label+percent'
      )
    })
    
    
    output$pieChart3 <- renderPlotly({
      file_path = paste0(folder_path,job_id,"snp_coding_consequences.csv")
      data = read.csv(file_path)
      #plot_ly(type = 'pie', labels = c("stop_gained", "missense_variant", "synonymous_variant", "others"), values = c(33, 8677, 13275, 38))
      plot_ly(
        data,
        labels = ~category,
        values = ~value,
        type = 'pie',
        textinfo = 'label+percent'
      )
    })    
    
    
    output$pieChart4 <- renderPlotly({
      file_path = paste0(folder_path,job_id,"snp_polyphen_count.csv")
      data = read.csv(file_path)
      #plot_ly(type = 'pie', labels = c("stop_gained", "missense_variant", "synonymous_variant", "others"), values = c(33, 8677, 13275, 38))
      plot_ly(
        data,
        labels = ~category,
        values = ~value,
        type = 'pie',
        textinfo = 'label+percent'
      )
    })    
    
    
    output$pieChart5 <- renderPlotly({
      file_path = paste0(folder_path,job_id,"snp_sift_count.csv")
      data = read.csv(file_path)
      #plot_ly(type = 'pie', labels = c("stop_gained", "missense_variant", "synonymous_variant", "others"), values = c(33, 8677, 13275, 38))
      plot_ly(
        data,
        labels = ~category,
        values = ~value,
        type = 'pie',
        textinfo = 'label+percent'
      )
    }) 
    
    
    output$pieChart6 <- renderPlotly({
      file_path = paste0(folder_path,job_id,"indel_coding_consequences.csv")
      data = read.csv(file_path)
      #plot_ly(type = 'pie', labels = c("stop_gained", "missense_variant", "synonymous_variant", "others"), values = c(33, 8677, 13275, 38))
      plot_ly(
        data,
        labels = ~category,
        values = ~value,
        type = 'pie',
        textinfo = 'label+percent'
      )
    }) 
    
    output$barGraph1 <- renderPlotly({
      file_path = paste0(folder_path,job_id,"snp_chr_count.csv")
      data = read.csv(file_path)
      x_labels <- unique(data$category)
      y_values <- data$value
      colors <- c("#266656", "#26705e", "#2d8771", "#2d9178", "#2e9e82", "#2dad8d", "#29b390", "#25b893", "#23c49c", 
              "#1ec99f", "#19d1a3", "#16d9a8", "#12e3af", "#0eebb3", "#0af2b8", "#02fabc", "#2dfcc9", "#3cfaca", 
              "#4bface", "#57f7cf", "#68f7d3", "#75fad9", "#84f5d9")
      plot_ly(x = factor(x_labels, levels = x_labels), y = y_values, type = 'bar', marker = list(color = colors)) %>%
        layout(xaxis = list(title = 'Chromosome', categoryorder = 'array', categoryarray = x_labels))
    })
    output$barGraph2 <- renderPlotly({
      
      file_path = paste0(folder_path,job_id,"indel_chr_count.csv")
      data = read.csv(file_path)
      x_labels <- unique(data$category)
      y_values <- data$value
      colors <- c("#266656", "#26705e", "#2d8771", "#2d9178", "#2e9e82", "#2dad8d", "#29b390", "#25b893", "#23c49c", 
              "#1ec99f", "#19d1a3", "#16d9a8", "#12e3af", "#0eebb3", "#0af2b8", "#02fabc", "#2dfcc9", "#3cfaca", 
              "#4bface", "#57f7cf", "#68f7d3", "#75fad9", "#84f5d9")
      
      plot_ly(x = factor(x_labels, levels = x_labels), y = y_values, type = 'bar', marker = list(color = colors)) %>%
        layout(xaxis = list(title = 'Chromosome', categoryorder = 'array', categoryarray = x_labels))
    })
    
  })
  
############################
  
############################3 Variant filter  ###########################################
  
  # Update Job ID choices
  #updateSelectInput(session, "select_job_id_filter", choices = c("", job_data$Job_ID), selected = "")
  
  # Display Patient ID and automatically load VCF file path when Job ID is selected
  observeEvent(input$select_job_id_filter, {
    req(input$select_job_id_filter)
    job_data <- read_jobs_csv2()
    if (input$select_job_id_filter != "") {
      # job_data<-read
      selected_job <- job_data[job_data$Job_ID == input$select_job_id_filter, ]
      
      if (nrow(selected_job) == 1) {
        patient_id <- selected_job$Patient_ID
        job <- selected_job$Job_ID
        folder_path= paste0("OncoGenAI/results/",job,"/")
        
        
        
        
        vcf_file_path = paste0(folder_path, job,"_bed.snps.vcf.gz")
        
        vcf_snp <- readVcf(vcf_file_path, genome = selected_job$Genome)
        info_data <- info(vcf_snp)
        snp_df <- as.data.frame(info_data)
        print(snp_df)
        
        vcf_file_path = paste0(folder_path, job,"_bed.indels.vcf.gz")
        vcf_indels <- readVcf(vcf_file_path, genome = selected_job$Genome)
        info_data <- info(vcf_indels)
        indel_df <- as.data.frame(info_data)
        print(indel_df)
        # for snps "QD", "SOR", "ReadPosRankSum",  "MQ", "MQRankSum", 
        # for indels "QD", "SOR", "ReadPosRankSum",

        
        # Display Patient ID
        output$display_patient_id_filter <- renderText({ paste("Patient ID:", patient_id) })
       
        output$snp_plot_filter1 <- renderPlotly({
          print(snp_df)
          snp_qd_plot <- plot_ly(snp_df, x = ~QD, type = "histogram", name = "QD") %>%
            layout(width = 400, height = 300,title = "SNP QD Histogram")
          snp_qd_plot
          
        })
        
        output$snp_plot_filter2 <- renderPlotly({
          
          snp_sor_plot <- plot_ly(snp_df, x = ~SOR, type = "histogram", name = "SOR") %>%
            layout(width = 400, height = 300,title = "SNP SOR Histogram")
          snp_sor_plot
          
        })
        output$snp_plot_filter3 <- renderPlotly({
          
          snp_readpos_plot <- plot_ly(snp_df, x = ~ReadPosRankSum, type = "histogram", name = "ReadPosRankSum") %>%
            layout(width = 400, height = 300,title = "SNP ReadPosRankSum Histogram")
          snp_readpos_plot
          
        })
        output$snp_plot_filter4 <- renderPlotly({
          
          snp_mq_plot <- plot_ly(snp_df, x = ~MQ, type = "histogram", name = "MQ") %>%
            layout(width = 400, height = 300,title = "SNP MQ Histogram")
          snp_mq_plot
          
        })
        output$snp_plot_filter5 <- renderPlotly({
          
          snp_mqrank_plot <- plot_ly(snp_df, x = ~MQRankSum, type = "histogram", name = "MQRankSum") %>%
            layout(width = 400, height = 300,title = "SNP MQRankSum Histogram")     
          snp_mqrank_plot
        })
        
        
        output$indel_plot_filter1 <- renderPlotly({
          indel_qd_plot <- plot_ly(indel_df, x = ~QD, type = "histogram", name = "QD") %>%
            layout(width = 400, height = 300,title = "Indel QD Histogram")
          indel_qd_plot
        })

        output$indel_plot_filter2 <- renderPlotly({
          indel_sor_plot <- plot_ly(indel_df, x = ~SOR, type = "histogram", name = "SOR") %>%
            layout(width = 400, height = 300,title = "Indel SOR Histogram")
          
        })
        output$indel_plot_filter3 <- renderPlotly({
          indel_readpos_plot <- plot_ly(indel_df, x = ~ReadPosRankSum, type = "histogram", name = "ReadPosRankSum") %>%
            layout(width = 400, height = 300,title = "Indel ReadPosRankSum Histogram")
          indel_readpos_plot
          
        })
        print(vcf_file_path)
        
      } else {
        showNotification("Job ID not found", type = "error")
      }
    } else {
      output$display_patient_id_filter <- renderText({ "" })
    }
    
  })
  

  
  observeEvent(input$update_filters, {
    req(input$select_job_id_filter)
    job_data <- read_jobs_csv2()
    selected_job <- job_data[job_data$Job_ID == input$select_job_id_filter, ]
    

    patient_id <- selected_job$Patient_ID
    job <- selected_job$Job_ID
    folder_path= paste0("OncoGenAI/results/",job,"/")
    
    
    
    vcf_file_path = paste0(folder_path, job,"_bed.snps.vcf.gz")
    vcf_snp <- readVcf(vcf_file_path, genome = selected_job$Genome)
    info_data <- info(vcf_snp)
    snp_df <- as.data.frame(info_data)
    
    if (input$filter_type == "manual") {
      if (!is.na(input$snp_qd_value)) {
        snp_df <- snp_df %>% filter(!eval(parse(text = paste0("QD ", input$snp_qd_operator, " ", input$snp_qd_value))))
      }
      if (!is.na(input$snp_sqr_value)) {
        snp_df <- snp_df %>% filter(!eval(parse(text = paste0("SOR ", input$snp_sqr_operator, " ", input$snp_sqr_value))))
      }
      if (!is.na(input$snp_pos_value)) {
        snp_df <- snp_df %>% filter(!eval(parse(text = paste0("ReadPosRankSum ", input$snp_pos_operator, " ", input$snp_pos_value))))
      }
      if (!is.na(input$snp_mq_value)) {
        snp_df <- snp_df %>% filter(!eval(parse(text = paste0("MQ ", input$snp_mq_operator, " ", input$snp_mq_value))))
      }
      if (!is.na(input$snp_mqrank_value)) {
        snp_df <- snp_df %>% filter(!eval(parse(text = paste0("MQRankSum ", input$snp_mqrank_operator, " ", input$snp_mqrank_value))))
      }
    
    }else if(input$filter_type == "3_mode"){
      # SNP Filters

      snp_df <- snp_df %>% filter(!QD < 2.0)  # QD < 2.0
      print(nrow(snp_df))
      snp_df <- snp_df %>% filter(!SOR > 3.0)  # SOR > 3.0
      print(nrow(snp_df))
      snp_df <- snp_df %>% filter(!ReadPosRankSum < -8.0)  # ReadPosRankSum < -8.0
      print(nrow(snp_df))
      snp_df <- snp_df %>% filter(!MQ < 25.0)  # MQ < 25.0
      print(nrow(snp_df))
      snp_df <- snp_df %>% filter(!MQRankSum < -12.5)  # MQRankSum < -12.5
      print(nrow(snp_df))

      print("inside snp")
    }

    
    vcf_file_path = paste0(folder_path, job,"_bed.indels.vcf.gz")
    vcf_indels <- readVcf(vcf_file_path, genome = selected_job$Genome)
    info_data <- info(vcf_indels)
    indel_df <- as.data.frame(info_data)
    
    if (input$filter_type == "manual") {
      if (!is.na(input$indel_qd_value)) {
        indel_df <- indel_df %>% filter(!eval(parse(text = paste0("QD ", input$indel_qd_operator, " ", input$indel_qd_value))))
      }
      if (!is.na(input$indel_sqr_value)) {
        indel_df <- indel_df %>% filter(!eval(parse(text = paste0("SOR ", input$indel_sqr_operator, " ", input$indel_sqr_value))))
      }
      if (!is.na(input$indel_pos_value)) {
        indel_df <- indel_df %>% filter(!eval(parse(text = paste0("ReadPosRankSum ", input$indel_pos_operator, " ", input$indel_pos_value))))
      }
    } else if(input$filter_type == "3_mode"){
      indel_df <- indel_df %>% filter(!QD < 2.0)  # QD < 2.0
      print(nrow(snp_df))
      indel_df <- indel_df %>% filter(!SOR > 10.0)  # SOR > 10.0
      print(nrow(snp_df))
      indel_df <- indel_df %>% filter(!ReadPosRankSum < -20.0)  # ReadPosRankSum < -20.0
      print(nrow(snp_df))

      print("inside indel")
    }
    
    
    output$snp_plot_filter1 <- renderPlotly({
      snp_qd_plot <- plot_ly(snp_df, x = ~QD, type = "histogram", name = "QD") %>%
        layout(width = 400, height = 300,title = "SNP QD Histogram")
      snp_qd_plot
      
    })
    
    output$snp_plot_filter2 <- renderPlotly({
      
      snp_sor_plot <- plot_ly(snp_df, x = ~SOR, type = "histogram", name = "SOR") %>%
        layout(width = 400, height = 300,title = "SNP SOR Histogram")
      
      snp_sor_plot
      
    })
    output$snp_plot_filter3 <- renderPlotly({
      #print(snp_df$ReadPosRankSum)
      # write.csv(snp_df,"test_snp_df.csv")
      snp_readpos_plot <- plot_ly(snp_df, x = ~ReadPosRankSum, type = "histogram", name = "ReadPosRankSum") %>%
        layout(width = 400, height = 300,title = "SNP ReadPosRankSum Histogram")
      
      snp_readpos_plot
      
    })
    output$snp_plot_filter4 <- renderPlotly({
      
      snp_mq_plot <- plot_ly(snp_df, x = ~MQ, type = "histogram", name = "MQ") %>%
        layout(width = 400, height = 300,title = "SNP MQ Histogram")
      snp_mq_plot
      
    })
    output$snp_plot_filter5 <- renderPlotly({
      
      snp_mqrank_plot <- plot_ly(snp_df, x = ~MQRankSum, type = "histogram", name = "MQRankSum") %>%
        layout(width = 400, height = 300,title = "SNP MQRankSum Histogram")     
      snp_mqrank_plot
    })
    
    
    output$indel_plot_filter1 <- renderPlotly({
      indel_qd_plot <- plot_ly(indel_df, x = ~QD, type = "histogram", name = "QD") %>%
        layout(width = 400, height = 300,title = "Indel QD Histogram")
      indel_qd_plot
    })
    
    output$indel_plot_filter2 <- renderPlotly({
      indel_sor_plot <- plot_ly(indel_df, x = ~SOR, type = "histogram", name = "SOR") %>%
        layout(width = 400, height = 300,title = "Indel SOR Histogram")
      indel_sor_plot
    })
    output$indel_plot_filter3 <- renderPlotly({
      indel_readpos_plot <- plot_ly(indel_df, x = ~ReadPosRankSum, type = "histogram", name = "ReadPosRankSum") %>%
        layout(width = 400, height = 300,title = "Indel ReadPosRankSum Histogram")
      indel_readpos_plot
      
    })
  })

  observeEvent(input$auto_fill, {
    if (input$auto_fill) {
      # Automatically fill SNP inputs
      updateSelectInput(session, "snp_qd_operator", selected = "<")
      updateNumericInput(session, "snp_qd_value", value = 2.0)
      
      updateSelectInput(session, "snp_sqr_operator", selected = ">")
      updateNumericInput(session, "snp_sqr_value", value = 3.0)
      
      updateSelectInput(session, "snp_mq_operator", selected = "<")
      updateNumericInput(session, "snp_mq_value", value = 25.0)
      
      updateSelectInput(session, "snp_mqrank_operator", selected = "<")
      updateNumericInput(session, "snp_mqrank_value", value = -12.5)
      
      updateSelectInput(session, "snp_pos_operator", selected = "<")
      updateNumericInput(session, "snp_pos_value", value = -8.0)
      
      # Indel filtering values based on GATK command
      updateSelectInput(session, "indel_qd_operator", selected = "<")
      updateNumericInput(session, "indel_qd_value", value = 2.0)
      
      updateSelectInput(session, "indel_sqr_operator", selected = ">")
      updateNumericInput(session, "indel_sqr_value", value = 10.0)
      
      updateSelectInput(session, "indel_pos_operator", selected = "<")
      updateNumericInput(session, "indel_pos_value", value = -20.0)


    } else {

      
      # Clear values if auto-fill is unchecked
      updateNumericInput(session, "snp_qd_value", value = NA)
      updateNumericInput(session, "snp_sqr_value", value = NA)
      updateNumericInput(session, "snp_pos_value", value = NA)
      updateNumericInput(session, "snp_mq_value", value = NA)
      updateNumericInput(session, "snp_mqrank_value", value = NA)
      

      updateNumericInput(session, "indel_qd_value", value = NA)
      updateNumericInput(session, "indel_sqr_value", value = NA)
      updateNumericInput(session, "indel_pos_value", value = NA)
    }
  })

  
 
  observeEvent(input$Save_filters,{

    if (input$filter_type == "3_mode") {

      if(input$mode_level == "Medium"){
        snp_filters <- list(
          QD = 2.0,
          MQ = 25.0,
          SQR = 3.0,
          MQRankSum = -12.5,
          ReadPosRankSum = -8
        )
        
        indel_filters <- list(
          QD = 2.0,
          SQR = 10.0,
          ReadPosRankSum = -20.0
        )
      }
        snp_filters <- list(
          QD = 2.0,
          MQ = 25.0,
          SQR = 3.0,
          MQRankSum = -12.5,
          ReadPosRankSum = -8
        )
        
        indel_filters <- list(
          QD = 2.0,
          SQR = 10.0,
          ReadPosRankSum = -20.0
        )
      indel_filters <- lapply(indel_filters, function(x) format(x, nsmall = 1))
      snp_filters <- lapply(snp_filters, function(x) format(x, nsmall = 1))
      job_id <- input$select_job_id_filter
      job_data <- read_jobs_csv2()
      # job_data <- job_data %>%
      #   mutate(Status = ifelse(Job_ID == job_id, "Running", Status))
      job_data <- job_data %>%
        mutate(Status = ifelse(Job_ID == job_id, "Running", Status))
      update_jobs_csv(job_data)
      refresh_data()

      selected_job <- job_data[job_data$Job_ID == input$select_job_id_filter, ]
      
      if (nrow(selected_job) == 1) {
        patient_id <- selected_job$Patient_ID
        job_id <- selected_job$Job_ID
        folder_path= paste0("OncoGenAI/results/",job_id,"/")
        
        input_vcf_prefix = paste0(folder_path, job_id)
        output_vcf_prefix =paste0(folder_path, job_id)
        reference_genome = selected_job$Genome 
         print("hello2")
        
        snp_command <- paste0(
          "/opt/gatk/gatk VariantFiltration -R ",reference_genome,"/", reference_genome, ".fa",
          " -V ", input_vcf_prefix, "_bed.snps.vcf.gz",
          " -O ", output_vcf_prefix, "_bed_filt.snps.vcf.gz",
          " --filter-expression 'QD < ", snp_filters$QD, "' --filter-name 'filterQD_lt", snp_filters$QD, "'",
          " --filter-expression 'MQ < ", snp_filters$MQ, "' --filter-name 'filterMQ_lt", snp_filters$MQ, "'",
          " --filter-expression 'SOR > ", snp_filters$SQR, "' --filter-name 'filterSOR_gt", snp_filters$SQR, "'",
          " --filter-expression 'MQRankSum < ", snp_filters$MQRankSum, "' --filter-name 'filterMQRankSum_lt", snp_filters$MQRankSum, "'",
          " --filter-expression 'ReadPosRankSum < ", snp_filters$ReadPosRankSum, "' --filter-name 'filterReadPosRankSum_lt", snp_filters$ReadPosRankSum, "'"
        )
        
        # Indel VariantFiltration command
        indel_command <- paste0(
          "/opt/gatk/gatk VariantFiltration -R ", reference_genome,"/",reference_genome, ".fa",
          " -V ", input_vcf_prefix, "_bed.indels.vcf.gz",
          " -O ", output_vcf_prefix, "_bed_filt.indels.vcf",
          " --filter-expression 'QD < ", indel_filters$QD, "' --filter-name 'filterQD_lt", indel_filters$QD, "'",
          " --filter-expression 'SOR > ", indel_filters$SQR, "' --filter-name 'filterSOR_gt", indel_filters$SQR, "'",
          " --filter-expression 'ReadPosRankSum < ", indel_filters$ReadPosRankSum, "' --filter-name 'filterReadPosRankSum_lt", indel_filters$ReadPosRankSum, "'"
        )

        #print("hello")

        print(snp_command)
        print(indel_command)
        
        # Run the SNP filtration script
        system(snp_command)
        
        # Run the Indel filtration script
        system(indel_command)
        
        

        log_path <- paste0("OncoGenAI/results/",job_id, "_logfile.txt")
        job_df = selected_job
        # job_id_col <- which(names(df) == "Job_ID")

            # job_df <- df[df[, job_id_col] == job_id, ,drop=F]
        file_path = strsplit(job_df$Uploaded.Files, split = "\t")[[1]]
            

            
        if (length(file_path) > 1){
          script_run_cmd <- paste0("vcf_annotate.sh" , " -f1 " , file_path[1]," -f2 ",file_path[2]," -g ", job_df$Genome," -s ", job_id, " -a ", job_df$Mutations, " -b ", job_df$Bed.File, " -t ", job_df$Workflow, " -p ", 6," -trim ",job_df$Adaptor.Trimming,  " -cnv " ,job_df$CNV.analysis)
          
        }else{
          script_run_cmd <- paste0("vcf_annotate.sh" , " -f1 " , file_path[1]," -g ", job_df$Genome," -s ", job_id, " -a ", job_df$Mutations, " -b ", job_df$Bed.File, " -t ", job_df$Workflow, " -p ", "6")
        }
        
        

        
        print(sprintf("nohup bash %s > %s 2>&1 &", script_run_cmd,log_path))
        print(paste("Resumed Job ID:", job_id))
        system(sprintf("nohup bash %s > %s 2>&1 &", script_run_cmd,log_path), intern = TRUE)

        # Notify user
        showNotification("Filtration process complete.", type = "message")   
        #vcf_file_path = paste0(folder_path, job_id,"_bed_filtered.snps.vcf.gz") 
        
        # Display Patient ID
        output$display_patient_id_filter <- renderText({ paste("Patient ID:", patient_id) })
        
      } else {
        showNotification("Job ID not found", type = "error")
      }

    
    } else if (input$filter_type == "manual") {



      job_id <- input$select_job_id_filter
      job_data <- read_jobs_csv2()
      # job_data <- job_data %>%
      #   mutate(Status = ifelse(Job_ID == job_id, "Running", Status))

      selected_job <- job_data[job_data$Job_ID == input$select_job_id_filter, ]
      
      if (nrow(selected_job) == 1) {
        patient_id <- selected_job$Patient_ID
        job_id <- selected_job$Job_ID
        folder_path= paste0("OncoGenAI/results/",job_id,"/")
        
        input_vcf_prefix = paste0(folder_path, job_id)
        output_vcf_prefix =paste0(folder_path, job_id)
        reference_genome = selected_job$Genome 
        
        # SNP Filtration Script
        

        gatk_cmd <- paste0("/opt/gatk/gatk VariantFiltration -R ",reference_genome,"/", reference_genome, ".fa",
          " -V ", input_vcf_prefix, "_bed.snps.vcf.gz",
          " -O ", output_vcf_prefix, "_bed_filt.snps.vcf.gz ")

        # Add SNP filters based on user inputs
        if (!is.null(input$snp_qd_value) && !is.na(input$snp_qd_value)) {
          snp_qd_filter <- paste0("QD", input$snp_qd_operator, input$snp_qd_value)
          gatk_cmd <- paste(gatk_cmd, "--filter-expression", shQuote(snp_qd_filter), "--filter-name", shQuote("SNP_QD"))
        }

        if (!is.null(input$snp_sqr_value) && !is.na(input$snp_sqr_value)) {
          snp_sqr_filter <- paste0("SQR", input$snp_sqr_operator, input$snp_sqr_value)
          gatk_cmd <- paste(gatk_cmd, "--filter-expression", shQuote(snp_sqr_filter), "--filter-name", shQuote("SNP_SQR"))
        }

        if (!is.null(input$snp_pos_value) && !is.na(input$snp_pos_value)) {
          snp_pos_filter <- paste0("ReadPosRank", input$snp_pos_operator, input$snp_pos_value)
          gatk_cmd <- paste(gatk_cmd, "--filter-expression", shQuote(snp_pos_filter), "--filter-name", shQuote("SNP_ReadPosRank"))
        }

        if (!is.null(input$snp_mq_value) && !is.na(input$snp_mq_value)) {
          snp_mq_filter <- paste0("MQ", input$snp_mq_operator, input$snp_mq_value)
          gatk_cmd <- paste(gatk_cmd, "--filter-expression", shQuote(snp_mq_filter), "--filter-name", shQuote("SNP_MQ"))
        }

        if (!is.null(input$snp_mqrank_value) && !is.na(input$snp_mqrank_value)) {
          snp_mqrank_filter <- paste0("MQRankSum", input$snp_mqrank_operator, input$snp_mqrank_value)
          gatk_cmd <- paste(gatk_cmd, "--filter-expression", shQuote(snp_mqrank_filter), "--filter-name", shQuote("SNP_MQRankSum"))
        }
        snp_cmd <- gatk_cmd

        gatk_cmd <- paste0("/opt/gatk/gatk VariantFiltration -R ",reference_genome,"/", reference_genome, ".fa",
          " -V ", input_vcf_prefix, "_bed.indels.vcf.gz",
          " -O ", output_vcf_prefix, "_bed_filt.indels.vcf ")

        # Add Indel filters based on user inputs
        if (!is.null(input$indel_qd_value) && !is.na(input$indel_qd_value)) {
          indel_qd_filter <- paste0("QD", input$indel_qd_operator, input$indel_qd_value)
          gatk_cmd <- paste(gatk_cmd, "--filter-expression", shQuote(indel_qd_filter), "--filter-name", shQuote("Indel_QD"))
        }

        if (!is.null(input$indel_sqr_value) && !is.na(input$indel_sqr_value)) {
          indel_sqr_filter <- paste0("SQR", input$indel_sqr_operator, input$indel_sqr_value)
          gatk_cmd <- paste(gatk_cmd, "--filter-expression", shQuote(indel_sqr_filter), "--filter-name", shQuote("Indel_SQR"))
        }

        if (!is.null(input$indel_pos_value) && !is.na(input$indel_pos_value)) {
          indel_pos_filter <- paste0("ReadPosRank", input$indel_pos_operator, input$indel_pos_value)
          gatk_cmd <- paste(gatk_cmd, "--filter-expression", shQuote(indel_pos_filter), "--filter-name", shQuote("Indel_ReadPosRank"))
        }

        
        indel_cmd <- gatk_cmd


        print(snp_cmd)
        print(indel_cmd)
        
        # Run the SNP filtration script
        system(snp_cmd)
        
        # Run the Indel filtration script
        system(indel_cmd)
        
        

        log_path <- paste0("OncoGenAI/results/",job_id, "_logfile.txt")
        job_df = selected_job
        # job_id_col <- which(names(df) == "Job_ID")

        # job_df <- df[df[, job_id_col] == job_id, ,drop=F]
        file_path = strsplit(job_df$Uploaded.Files, split = "\t")[[1]]
            

            
        if (length(file_path) > 1){
          script_run_cmd <- paste0("vcf_annotate.sh" , " -f1 " , file_path[1]," -f2 ",file_path[2]," -g ", job_df$Genome," -s ", job_id, " -a ", job_df$Mutations, " -b ", job_df$Bed.File, " -t ", job_df$Workflow, " -p ", 6," -trim ",job_df$Adaptor.Trimming,  " -cnv " ,job_df$CNV.analysis)
        }else{
          script_run_cmd <- paste0("vcf_annotate.sh" , " -f1 " , file_path[1]," -f2 ","NA"," -g ", job_df$Genome," -s ", job_id, " -a ", job_df$Mutations, " -b ", job_df$Bed.File, " -t ", job_df$Workflow, " -p ", 6," -trim ",job_df$Adaptor.Trimming,  " -cnv " ,job_df$CNV.analysis)
        }
        
        
        
        print(sprintf("nohup bash %s > %s 2>&1 &", script_run_cmd,log_path))
        print(paste("Resumed Job ID:", job_id))
        system(sprintf("nohup bash %s > %s 2>&1 &", script_run_cmd,log_path), intern = TRUE)

        # Notify user
        showNotification("Filtration process complete.", type = "message")   
        vcf_file_path = paste0(folder_path, job,"_bed_filtered.snps.vcf.gz") 
        
        # Display Patient ID
        output$display_patient_id_filter <- renderText({ paste("Patient ID:", patient_id) })
        
      } else {
        showNotification("Job ID not found", type = "error")
      }
    } else {
      output$display_patient_id_filter <- renderText({ "" })
    }
  })
  ####################################################################################
  ####################################################################################
  
  ########################### WF ###########################
  # Reactive value to keep track of workflow fields
  workflow_fields <- reactiveVal(list(list(id = 1)))
  
  # Generate UI for workflow fields
  output$workflowFields <- renderUI({
    fields <- workflow_fields()
    lapply(seq_along(fields), function(i) {
      field <- fields[[i]]
      boxTitle <- paste0("Field ", field$id)  # Generate box title based on id
      
      column(width = 3,  # Ensure each box takes up 1/4 of the row
             box(
               title = boxTitle, status = "primary", solidHeader = TRUE, width = NULL,
               div(class = "field-box",
                   selectInput(paste0("input_", field$id), " ", choices = c("Input1", "Input2", "Input3", "Processing1", "Processing2", "Processing3", "Output 1", "Output 2", "Output 3")),
                   # Optionally include other selectInputs or UI elements
                   if (field$id > 1) {  # Only show remove button if field ID is greater than 1
                     actionButton(paste0("remove_", field$id), "×", class = "remove-btn")
                   }
               )
             )
      )
    })
  })
  
  # Add new field
  observeEvent(input$add_field, {
    fields <- workflow_fields()
    new_id <- max(sapply(fields, `[[`, "id")) + 1
    fields <- c(fields, list(list(id = new_id)))
    workflow_fields(fields)
  })
  
  # Remove field
  observe({
    fields <- workflow_fields()
    remove_buttons <- grep("^remove_", names(input), value = TRUE)
    for (button in remove_buttons) {
      observeEvent(input[[button]], {
        id_to_remove <- as.numeric(sub("^remove_", "", button))
        fields <- Filter(function(f) f$id != id_to_remove, fields)
        # Reassign IDs to remaining fields
        fields <- lapply(seq_along(fields), function(i) {
          field <- fields[[i]]
          field$id <- i
          field
        })
        workflow_fields(fields)
      })
    }
  })
  
  # Save workflow
  observeEvent(input$save_workflow, {
    workflow_name <- input$workflow_name
    fields <- workflow_fields()
    
    # Here you can handle the save functionality, such as saving the data to a file or database
    # For demonstration, we'll just print the workflow name and fields to the console
    print(paste("Workflow Name:", workflow_name))
    print(fields)
    
    # Display a confirmation message to the user
    showModal(modalDialog(
      title = "Workflow Saved",
      paste("Your workflow", workflow_name, "has been saved successfully."),
      easyClose = TRUE,
      footer = NULL
    ))
  })


observeEvent(input$select_job_id_r4,{  
  req(input$select_job_id_r4)


    # Define file paths

  job_id <- input$select_job_id_r4
  
  selected_job <- job_data[job_data$Job_ID == input$select_job_id_r4, ]
  
  folder_path <- paste0("OncoGenAI/results/", job_id, "/")
  genome <- selected_job$Genome

    cnsFilePath <- paste0(folder_path,job_id,"call-cns.csv")
    cnrFilePath <- paste0(folder_path,job_id,"cnr.csv") 
    scoresheetFilePath <- paste0(folder_path,"/CNVclassify/Scoresheet.txt")
    
    print(scoresheetFilePath)
    
    # Reactive function to load and store .call.cns data
    callCNSData <- reactive({
      req(file.exists(cnsFilePath))  # Check if file exists
      # Read CSV file
      read.csv(cnsFilePath, header = TRUE)
    })
    
    # Reactive function to load and store .cnr data
    cnrData <- reactive({
      req(file.exists(cnrFilePath))  # Check if file exists
      # Read CSV file
      read.csv(cnrFilePath, header = TRUE)
    })
    
    # Reactive function to load and store scoresheet data, removing unwanted column
    scoresheetData <- reactive({
      req(file.exists(scoresheetFilePath))  # Check if file exists
      data <- read.csv(scoresheetFilePath, header = TRUE,sep="\t")
      data$`All.protein.coding.genes` <- NULL
      names(data) <- gsub("^X", "DB", names(data))
      db_columns <- grep("^DB", names(data), value = TRUE)

# Get the remaining column names (those not starting with 'DB')
other_columns <- setdiff(names(data), db_columns)

# Rearrange columns with 'DB' columns at the end
data <- data[, c(other_columns, db_columns)]
      data
    })
    
    # Render tables for the .call.cns, .cnr, and scoresheet data
    output$cnsTable <- renderDT({
      df <- callCNSData()
      df <- df[, c(setdiff(names(df), "gene"), "gene")]
      df
    }, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE, ordering = F))
    
    output$cnrTable <- renderDT({
      df <- cnrData()
      df
    }, options = list(pageLength = 10, scrollX = TRUE))
    
updatePickerInput(session, "cnv_columns", choices =  names(scoresheetData()), selected = names(scoresheetData())[1:8])
    output$scoresheetTable <- renderDT({
      scoresheetData()[, input$cnv_columns]
    }, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE, ordering = F, Filter = T))
    

})

observeEvent(input$tmb_button, {
    updateTabItems(session, "tabs2", selected =  "tmbscore")
    updateSelectInput(session, "select_job_id_tmb", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected =input$select_job_id_r4)
  })

  #-----------------------------end of result4 page----------------
  
  observeEvent(input$submit_bug, {
    file <- input$bug_screenshot
    description <- input$bug_description
    
    if (!is.null(file)) {
      file.copy(file$datapath, paste0("www/", file$name))
      showNotification(paste("Bug submitted with screenshot:", file$name), type = "message")
    } else {
      showNotification("Bug submitted without screenshot.", type = "message")
    }
    
    if (description != "") {
      showNotification(paste("Description:", description), type = "message")
    } else {
      showNotification("No description provided.", type = "warning")
    }
  })

  observeEvent(input$cnv_button, {
    updateTabItems(session, "tabs2", selected =  "result4")
    updateSelectInput(session, "select_job_id_r4", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected =input$select_job_id_r3 )
  })
  #----------------------------end of result4-------------------------------------------------

  #--------------------------- resources page -----------------------------
  # html_url <- "http://127.0.0.1:5500/resources.html"

  #  output$resources_html <- renderUI({
  #       tags$iframe(src = html_url, height = "1000px", width = "100%", style = "border:none;")
  #     })
  # ----------------------------------------------------------------------
  
  ##################################################################
  
  
  #############################    VCF shiny Upload  ##################################
  
  raw_variants_list <- eventReactive(input$sample_data , {
    
    req(input$sample_data)
    req(job_data,input$mutation_type_v1)
    
    if(T ) {
      print(input$tabs2)
      withProgress(message = "processing", min = 0, max = 1, {
        cat(">> preparing data upload ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
        
        
        
        data = job_data
        
        if ("TRUE") {
          # vcf_example <- readRDS(system.file("extdata", "example_data.rds", package = "VCFshiny"))
          # print(vcf_example)
          # vcf_ABCD <<- vcf_example
          vcf_data = list()
          folder_path= paste0("OncoGenAI/results/",job,"/")
          for( i in nrow(data)){
            
            job = data$Job_ID[i]
            print(job)
            genome_version = data$Genome[i]
            
            
            vcf_snp = paste0(folder_path,job,"_snps.",genome_version,"_multianno.txt.grl_p")
            vcf_indels = paste0(folder_path, job,"_indels",genome_version,"_multianno.txt.grl_p")
            print(vcf_snp)
            print(vcf_indels)
            # "/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/output/SAMITA80.hg38_multianno.txt"
            vcf_snp <- read.csv(vcf_snp,sep = "\t")
            vcf_indels <- read.csv(vcf_indels,sep = "\t")
            
            
            # job_id = "Oncogen202408030004"
            # genome= "hg38"
            # folder_path = paste0("OncoGenAI/results/",job_id,"/")
            # vcf_snp = folder_path(job_id,"_snps.",genome,"_multianno.txt.intervar")
            # vcf_indels = folder_path(job_id,"_indels.",genome,"_multianno.txt.intervar")
            
            
            # vcf_snp <- read.csv("/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/output/SAMITA80.hg38_multianno.txt",sep = "\t")
            # vcf_indels <- read.csv("/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/InterVar-master/output/SAMITA80.hg38_multianno.txt",sep = "\t")
            
            
            vcf_snp <- vcf_snp[, c("Chr", "Start", "End", "Ref", "Alt", "Func.refGene", "Gene.refGene", "avsnp151")]
            vcf_snp$Chr_Start_Ref_Alt <- apply(vcf_snp[, c("Chr", "Start", "Ref", "Alt")], 1, function(row) {
              paste(row, collapse = "_")
            })
            names(vcf_snp) <- c("Chr", "Start", "End", "REF", "ALT", "Func.refGene", "Gene.refGene", "avsnp151","Chr_Start_Ref_Alt")
            
            
            vcf_indels <- vcf_indels[, c("Chr", "Start", "End", "Ref", "Alt", "Func.refGene", "Gene.refGene", "avsnp151")]
            vcf_indels$Chr_Start_Ref_Alt <- apply(vcf_indels[, c("Chr", "Start", "Ref", "Alt")], 1, function(row) {
              paste(row, collapse = "_")
            })
            names(vcf_indels) <- c("Chr", "Start", "End", "REF", "ALT", "Func.refGene", "Gene.refGene", "avsnp151","Chr_Start_Ref_Alt")
            
            if (input$mutation_type == "snp"){
              vcf_data[[job]] == vcf_snp
            }else{
              vcf_data[[job]] == vcf_snp
            }
            
            
            
          }
          vcf_data_g<<-vcf_data
          return(vcf_data)
        }
        # else if(input$use_example == "FALSE"){
        #   req(input$input_file, input$sample_data)
        #   if(file.exists(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|.tar.gz"))){
        #     unlink(stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|.tar.gz"), unlink = T)
        #   }
        #   if(stringr::str_detect(input$input_file$name, pattern = "gz$")){
        #     utils::untar(input$input_file$datapath, exdir = ".")
        #   }else if(stringr::str_detect(input$input_file$name, pattern = "zip$")){
        #     utils::unzip(input$input_file$datapath, exdir = ".")
        #   }
        # 
        #   incProgress(0.2, detail = "uncompressing file...")
        #   input_dir <- stringr::str_remove(string = input$input_file$name, pattern = ".gz|.zip|.tar.gz")
        #   file_split_name <- strsplit(list.files(input_dir), ".", fixed = T)
        # 
        #   #读入文件
        #   if(file_split_name[[1]][length(file_split_name[[1]])] == "gz"){
        #     file_path <- dir(input_dir, pattern = "*.gz$", full.name = T) #构建文件夹路径
        #     file_names <- dir(input_dir, pattern = "*.gz$") %>%　stringr::str_remove(".vcf.gz")#构建文件名
        #     ALL_df_List <- lapply(file_path, function(x){ ALL <- as.data.frame(vcfR::read.vcfR(x,verbose = F)@fix) })
        #     names(ALL_df_List) <- (file_names)
        #   }else if(file_split_name[[1]][length(file_split_name[[1]])] == "vcf"){
        #     file_path <- dir(input_dir, pattern = "*.vcf$", full.name = T) #构建文件夹路径
        #     file_names <- dir(input_dir, pattern = "*.vcf$") %>% stringr::str_remove(".vcf") #构建去后缀后的文件名
        #     ALL_df_List <- lapply(file_path, function(x){ ALL <- as.data.frame(vcfR::read.vcfR(x,verbose = F)@fix) })
        #     names(ALL_df_List) <- (file_names)
        #   }else if(file_split_name[[1]][length(file_split_name[[1]])] == "txt"){
        #     file_path <- dir(input_dir, pattern = "*.txt$", full.name = T)#构建文件夹路径
        #     file_names <- dir(input_dir, pattern = "*.txt$") %>%　stringr::str_remove(".txt")#构建去后缀后的文件名
        #     ALL_df_List <- lapply(file_path, function(x){ ; ALL <- read.table(x, header = T, sep = "\t")})
        #     names(ALL_df_List) <- (file_names)
        #   }
        # 
        #   unlink(strsplit(input$input_file$name,".",fixed = T)[[1]][1], recursive = T)
        #   cat(">> preparing data classification ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
        #   incProgress(0.3, detail = "loading file...")
        # 
        #   #提取Ensembl-VEP注释结果
        #   if(stringr::str_detect(ALL_df_List[[1]][1,ncol(ALL_df_List[[1]])], pattern = "CSQ=")){
        #     cl <- parallel::makeCluster(4)
        #     ALL_List <- parallel::parLapply(cl, 1:length(ALL_df_List), function(x){
        #       Select_df <- lapply(1:nrow(ALL_df_List[[x]]), function(y){
        #         data <- ALL_df_List[[x]]$INFO[y] %>% strsplit("\\|") %>% unlist()
        #         number <- seq(1, length(data), 38)
        #         df <- data.frame(ALL_df_List[[x]][y, 1:5],
        #                          consequence = paste(na.omit(unique(data[number + 1][data[number + 1] != ""])), collapse = ';'),
        #                          symbol = paste(na.omit(unique(data[number + 3][data[number + 3] != ""])), collapse = ';'))
        #       }) %>% dplyr::bind_rows()
        #       return(Select_df)
        #     })
        #     parallel::stopCluster(cl)
        #     names(ALL_List) <- names(ALL_df_List)
        #     ALL_df_List <- ALL_List
        #   }
        # 
        #   incProgress(0.4, detail = "displaying file...")
        #   # 分割SNP、Indel 数据
        #   if("snp" %in% strsplit(names(ALL_df_List)[1], ".", fixed = T)[[1]] |
        #      "indel" %in% strsplit(names(ALL_df_List)[1], ".", fixed = T)[[1]]){ #检测是否已经分过SNP、Indel了
        #     return(ALL_df_List)
        #   }else{
        #     snp_list <- lapply(ALL_df_List, function(x){
        #       df <- x[nchar(x[, 4]) == nchar(x[, 5]), ]
        #     })
        #     names(snp_list) <- paste(names(ALL_df_List), "snp", sep = ".")
        # 
        #     indel_list <- lapply(ALL_df_List, function(x){
        #       df <- x[nchar(x[, 4]) != nchar(x[, 5]), ]
        #     })
        #     names(indel_list) <- paste(names(ALL_df_List),"indel", sep = ".")
        #     raw_variant_list <- append(indel_list, snp_list)
        #     
        #   }
        # 
        # }
        
      })
      
      
    }
    
  })
  output$sample_id <- renderUI({
    #req(rv$data)
    samples = job_data$Job_ID
    #samples <- names(raw_variants_list())
    selectInput("sampleID", "Select Job ID", choices = samples,selected = NULL,  width = "40%")
  })
  
  raw_variants_list <- eventReactive(input$tabs2,{
    #req(input$tabs2 == "datainput")
    #/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/OncoGenAI/results/Oncogen202408030004/Oncogen202408030004_snps.hg38_multianno.txt.intervar
    #req(rv$data)
    vcf_data = list()
    data = job_data %>% filter(Status == "Completed")
    
    
    for( i in 1:nrow(data)){
      
      job = data$Job_ID[i]
      print(job)
      folder_path= paste0("OncoGenAI/results/",job,"/")
      genome_version = data$Genome[i]
      
      
      vcf_snp = paste0(folder_path,job,"_snps.",genome_version,"_multianno.txt.grl_p")
      vcf_indels = paste0(folder_path, job,"_indels.",genome_version,"_multianno.txt.grl_p")
      print(vcf_snp)
      print(vcf_indels)
      vcf_snp <- read.csv(vcf_snp,sep = "\t")
      vcf_indels <- read.csv(vcf_indels,sep = "\t")
      # OncoGenAI/results/Oncogen202410190001/Oncogen202410190001_snps.hg19_multianno.txt.grl_p
      # vcf_snp <- read.csv("/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/output/SAMITA80.hg38_multianno.txt",sep = "\t")
      # vcf_indels <- read.csv("/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/InterVar-master/output/SAMITA80.hg38_multianno.txt",sep = "\t")
      
      
      # job_id = "Oncogen202408030004"
      # genome= "hg38"
      # folder_path = paste0("OncoGenAI/results/",job_id,"/")
      # vcf_snp = folder_path(job_id,"_snps.",genome,"_multianno.txt.intervar")
      # vcf_indels = folder_path(job_id,"_indels.",genome,"_multianno.txt.intervar")
      
      
      # vcf_snp <- read.csv("/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/output/SAMITA80.hg38_multianno.txt",sep = "\t")
      # vcf_indels <- read.csv("/16Tbdrive1/suvenduk/DNAseq/gatk/integrated-analysis/InterVar-master/output/SAMITA80.hg38_multianno.txt",sep = "\t")
      
      
      vcf_snp <- vcf_snp[, c("Chr", "Start", "End", "Ref", "Alt", "Func.refGene", "Gene.refGene", "avsnp151")]
      vcf_snp$Chr_Start_Ref_Alt <- apply(vcf_snp[, c("Chr", "Start", "Ref", "Alt")], 1, function(row) {
        paste(row, collapse = "_")
      })
      names(vcf_snp) <- c("Chr", "Start", "End", "REF", "ALT", "Func.refGene", "Gene.refGene", "avsnp151","Chr_Start_Ref_Alt")
      
      
      vcf_indels <- vcf_indels[, c("Chr", "Start", "End", "Ref", "Alt", "Func.refGene", "Gene.refGene", "avsnp151")]
      vcf_indels$Chr_Start_Ref_Alt <- apply(vcf_indels[, c("Chr", "Start", "Ref", "Alt")], 1, function(row) {
        paste(row, collapse = "_")
      })
      names(vcf_indels) <- c("Chr", "Start", "End", "REF", "ALT", "Func.refGene", "Gene.refGene", "avsnp151","Chr_Start_Ref_Alt")
      print(input$mutation_type_v1)
      print(vcf_snp)
      
      
      vcf_id_i = paste0(job,".indels")
      vcf_id_s = paste0(job,".snp")
      
      vcf_data[[vcf_id_i]] = vcf_indels
      vcf_data[[vcf_id_s]] = vcf_snp
    }
    vcf_data_g<<-vcf_data
    return(vcf_data)
    
  })
  
  
  # raw_variants_list <- eventReactive(input$sample_data, {
  # 
  # 
  # 
  # 
  # })
  
  
  raw_variants_select_list <- reactive({
    ALL_df_List <- raw_variants_list()
    
    if("snp" %in% strsplit(names(ALL_df_List)[1], ".", fixed = T)[[1]] |
       "indel" %in% strsplit(names(ALL_df_List)[1], ".", fixed = T)[[1]]){
      return(ALL_df_List)
    }else{
      snp_list <- lapply(ALL_df_List, function(x){
        df <- x[nchar(x[, 4]) == nchar(x[, 5]), ]
      })
      names(snp_list) <- paste(names(ALL_df_List), "snp", sep = ".")
      
      indel_list <- lapply(ALL_df_List, function(x){
        df <- x[nchar(x[, 4]) != nchar(x[, 5]), ]
      })
      names(indel_list) <- paste(names(ALL_df_List),"indel", sep = ".")
      raw_variant_list <- append(indel_list, snp_list)
      return(raw_variant_list)
    }
  })
  
  
  
  
  output$raw_sample <- DT::renderDataTable({
    req(input$sampleID,input$mutation_type_v1)
    sample_id<- paste0(input$sampleID,".",input$mutation_type_v1)
    raw_variants_list()[[sample_id]]
  }, rownames = T, options = list(scrollX = TRUE, pageLength =5))
  
  #############################################################################################################  
  
  ################################# VCF shiny 2.Venn_server ######################################
  
  venn_data_list <- reactive({
    raw_variants_list <- raw_variants_list()
    venn_data_list <- lapply(raw_variants_list,function(x){
      raw_variants <- x
      raw_variants$name <- paste(raw_variants[, 1], raw_variants[, 2], raw_variants[, 3], raw_variants[, 4],
                                 raw_variants[, 5],  sep = "_")
      raw_variants
    })
    return(venn_data_list)
  })
  
  
  
  output$venn_group_id <- renderUI({
    if(!is.null(raw_variants_list())){
      group <- names(venn_data_list()) %>% stringr::str_replace_all(pattern = "-[0-9].snp|_[0-9].snp|.snp|-[0-9].indel|_[0-9].indel|.indel", replacement = "") %>% unique()
      virtualSelectInput("venn_group_ID","Sample groups:", choices = group, selected = group[-1], multiple = F,
                         zIndex = 4,search = F, width = "100%")
    }
  })
  
  vennPlot_data <- eventReactive(input$venn_star, {
    withProgress(message = "Analyse", min = 0, max = 1, {
      venn_data_list <- venn_data_list()
      incProgress(0.4, detail = "Select Data ...")
      samples <- stringr::str_subset(names(venn_data_list), pattern = input$venn_group_ID, negate = F) %>% stringr::str_subset(pattern = input$venn_type_id, negate = F)
      venn_list <- lapply(samples, function(x){
        venn_data_list[[x]][,"name"] %>% unique()
      })
      names(venn_list) <- samples
      return(venn_list)
    })
  })
  
  vennPlot <- eventReactive(input$venn_star,{
    req(input$venn_box, input$venn_ellipse, input$venn_ilcs, input$venn_sncs)
    venn::venn(vennPlot_data(), zcolor = "style", box = as.logical(input$venn_box),
               ellipse = as.logical(input$venn_ellipse),ilcs = (input$venn_ilcs), sncs = (input$venn_sncs)
    )
  })
  
  
  output$Display_venn_plot <- renderPlot({
    vennPlot()
  })
  
  output$Venn_download_plot <- downloadHandler(
    req(input$venn_type_id, input$venn_group_ID, input$VennPlot_download_width, input$VennPlot_download_height),
    filename = function(){
      paste(paste("1",input$venn_type_id, input$venn_group_ID, "Venn_plot", sep = "_"), "pdf", sep = ".")
    },
    content = function(file){
      pdf(file, width = input$VennPlot_download_width, height = input$VennPlot_download_height)
      req(input$venn_box, input$venn_ellipse, input$venn_ilcs, input$venn_sncs)
      venn::venn(vennPlot_data(), zcolor = "style", box = as.logical(input$venn_box),
                 ellipse = as.logical(input$venn_ellipse),ilcs = (input$venn_ilcs), sncs = (input$venn_sncs))
      dev.off()
    }
  )
  
  
  venn_table <- eventReactive(input$venn_star, {
    samples <- stringr::str_subset(names(venn_data_list()),pattern = input$venn_group_ID, negate = F) %>% stringr::str_subset(pattern = input$venn_type_id, negate = F)
    venn_binded <- lapply(samples, function(x){
      df <- venn_data_list()[[x]]
    }) %>% bind_rows()
    
    duplicated_row <- (venn_binded$name %>% table() %>% as.data.frame() %>% dplyr::filter(Freq == max(Freq)))[,1]
    duplicated_row <- as.character(duplicated_row)
    venn_binded <- venn_binded[(venn_binded$name %in% duplicated_row) , ] %>% unique()
    venn_binded <- subset(venn_binded, select = -c(name))
    return(venn_binded)
  })
  
  output$venn_Table <- DT::renderDataTable(
    return(venn_table()),
    options = list(
      scrollX = TRUE,
      pageLength = 5
    )
  )
  
  
  output$venn_tab_download <- downloadHandler(
    filename = function()  {paste0("1_venn_tab",".csv")},
    content = function(file) {
      write.csv(venn_table(), file, row.names = F)
    }
  )
  
  ###############################################################################################
  
  ################################## VCF shiny 3.summarise_barplot_serve ################################
  
  sumData <- eventReactive(input$summarise_plot,{
    req(raw_variants_list())
    summary_df <- data.frame()
    for(i in 1:length(raw_variants_list())){
      variant_numbers = dim(raw_variants_list()[[i]])[1]
      samples = strsplit(names(raw_variants_list())[i], ".", fixed = T)[[1]][1]
      group =  gsub("-[0-9]$|_[0-9]$","",samples)
      type = strsplit(names(raw_variants_list())[i], ".", fixed = T)[[1]][length(strsplit(names(raw_variants_list())[i], ".",fixed = T)[[1]])]
      summary_df <- rbind(summary_df, data.frame(group, samples, type, variant_numbers))
    }
    return(summary_df)
  })
  
  output$information <- DT::renderDataTable(
    return(sumData()),
    options = list(
      scrollX = TRUE,
      pageLength = 5
    )
  )
  
  output$summary_tab <- downloadHandler(
    filename = function()  {paste0("2_summary_tab",".csv")},
    content = function(file) {
      write.csv(sumData(), file, row.names = F)
    }
  )
  
  #1-1.2 、 绘制SNP+Indel突变柱状图
  SummarisePlot <- eventReactive(input$summarise_plot,{
    p <- ggpubr::ggbarplot(sumData()[c(1,2),,drop=F], x = "group",y =  "variant_numbers" , facet.by = "type" ,
                           palette = "Set1", label = as.logical(input$Summariseplot_label), lab.nb.digits = 2,
                           fill = "group" , add = "mean_sd" ,  width = input$bar_width , position = position_dodge(),
                           add.params = list(width = input$error_bar_width ,size = input$error_bar_size), ggtheme = theme_classic())+
      geom_jitter(size = input$jitter_size,width = input$jitter_width)+
      scale_alpha_continuous(expand = expansion(mult = c(0.1, 0.1)))+
      facet_wrap( ~ type, ncol = 4, scales = "free")+
      xlab(NULL)+ylab("The total number of variants")
      #scale_y_continuous(limits = c(0, NA)) 
    #+
    # theme(legend.position = "none",
    #       strip.background = element_blank(),
    #       strip.text = element_text(size = 21, color = "black"),
    #       axis.text.x = element_text(angle = 45, hjust = 1),
    #       axis.text = element_text(color = "black", size = input$label_text_size),
    #       axis.title = element_text(color = "black", size = input$title_text_size),
    #       text = element_text(face = "bold", family = "Times", color = "black"))
    
    if (!is.null(input$summ_ggText)) {
      add_funcs <- strsplit(input$summ_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    return(p)
  })
  
  output$SummarisePlot <- renderPlot({
    SummarisePlot()
  })
  
  output$summarise_plot <- downloadHandler(
    filename = "2_Summarise_plot.pdf",
    content = function(file){
      pdf(file,width = input$summarise_download_width,height = input$summarise_download_height)
      print(SummarisePlot())
      dev.off()
    }
  )
  
  ####################################################################################################
  
  ################################### VCF shiny 4.SNP_Analysis_server ################################
  SNP_data <- reactive({
    snp_list <- lapply(stringr::str_subset(names(raw_variants_list()), pattern = ".snp"),function(x){
      SNP_ALL <- raw_variants_list()[[x]]
    })
    names(snp_list) <- stringr::str_subset(names(raw_variants_list()),pattern = ".snp")
    return(snp_list)
  })
  
  
  output$snp_analyse_group <- renderUI({
    observe(SNP_data())
    virtualSelectInput(
      inputId = "SNP_Analysis_Group",  label = "Sample groups:",
      choices = unique(gsub("-[0-9].snp$|_[0-9].snp$|.snp","",names(SNP_data()))),
      selected = unique(gsub("-[0-9].snp$|_[0-9].snp$|.snp","",names(SNP_data()))),
      multiple = F, search = F, width = "100%"
    )
  })
  
  
  SNP_Analysis_data <- eventReactive(input$SNP_Analysis_action, {
    req(input$SNP_Analyse_mode, input$SNP_Analysis_Group)
    
    
    SNP_data <- SNP_data()
    SNP_heatmap_data <- data.frame()
    for(x in 1:length(SNP_data)){
      seq = c("A","T","C","G")
      for(y in seq){
        for(z in seq){
          SNP_numbers <- sum(SNP_data[[x]][, 4] == y & SNP_data[[x]][, 5] == z)
          REF <- y
          ALT <- z
          samples <- strsplit(names(SNP_data)[x], ".",fixed = T)[[1]][1]
          group <- gsub("-[0-9]$|_[0-9]$","", samples)
          freq <- SNP_numbers / dim(SNP_data[[x]])[1]
          SNP_heatmap_data <- rbind(SNP_heatmap_data, data.frame(group, samples, REF, ALT, SNP_numbers, freq))
        }
      }
    }
    
    if(input$SNP_Analyse_mode == "heatmap"){
      SNP_Data <- SNP_heatmap_data[SNP_heatmap_data$group %in% input$SNP_Analysis_Group, ]
    }else{
      SNP_type_data <- SNP_heatmap_data[SNP_heatmap_data$group %in% input$SNP_Analysis_Group, ]
      SNP_type_data <- SNP_type_data[SNP_type_data$REF != SNP_type_data$ALT, ]
      SNP_type_data[(SNP_type_data$REF == "A" & SNP_type_data$ALT == "C") | (SNP_type_data$REF == "T" & SNP_type_data$ALT == "G"), "type"] <- "A>C/T>G"
      SNP_type_data[(SNP_type_data$REF == "A" & SNP_type_data$ALT == "G") | (SNP_type_data$REF == "T" & SNP_type_data$ALT == "C"), "type"] <- "A>G/T>C"
      SNP_type_data[(SNP_type_data$REF == "A" & SNP_type_data$ALT == "T") | (SNP_type_data$REF == "T" & SNP_type_data$ALT == "A"), "type"] <- "A>T/T>A"
      SNP_type_data[(SNP_type_data$REF == "C" & SNP_type_data$ALT == "A") | (SNP_type_data$REF == "G" & SNP_type_data$ALT == "T"), "type"] <- "C>A/G>T"
      SNP_type_data[(SNP_type_data$REF == "C" & SNP_type_data$ALT == "G") | (SNP_type_data$REF == "G" & SNP_type_data$ALT == "C"), "type"] <- "C>G/G>C"
      SNP_type_data[(SNP_type_data$REF == "C" & SNP_type_data$ALT == "T") | (SNP_type_data$REF == "G" & SNP_type_data$ALT == "A"), "type"] <- "C>T/G>A"
      SNP_Data <- SNP_type_data %>% group_by(type, group, samples) %>% summarise_if(is.numeric, sum) %>% as.data.frame()
    }
    return(SNP_Data)
  })
  
  
  SNP_Analysis_plot <- eventReactive(input$SNP_Analysis_action, {
    if(input$SNP_Analyse_mode == "heatmap"){
      data <- SNP_Analysis_data()
      SNP_data <- data %>% group_by(group, REF, ALT) %>% summarise_if(is.numeric, mean) %>% as.data.frame()
      SNP_data$freq <- SNP_data$freq * 100

      print(SNP_data)
      
      p <- ggplot2::ggplot(SNP_data, aes(ALT,REF,fill = freq))+
        geom_tile()+
        geom_text(aes(label = round(freq, digits = 2)), size = (input$heatmap_text.size)/4)+
        facet_wrap(~group,ncol = input$heatmap_ncol)+
        scale_fill_gradient(low = stringr::str_split(input$heatmap_color, ",")[[1]][1],
                            high = stringr::str_split(input$heatmap_color, ",")[[1]][2])+
        theme_test()
      #+
      #theme(text = element_text(face = "bold", family = "Times", size = input$heatmap_text.size, color = "black"))
    }else{
      SNP_type_data <- SNP_Analysis_data()
      if (input$SNP_barplot_mode == "dodge"){
        p <- ggpubr::ggbarplot(SNP_type_data, x = "type", y = "freq", fill = "group", palette = "Set1", label = F,
                               add = c("mean_sd"), width = input$SNV_barplot_bar_width, position = position_dodge(),
                               add.params = list(width = input$SNV_barplot_err_bar_width, size = input$SNV_barplot_err_bar_size),
                               ggtheme = theme_classic())+
          xlab(NULL) + ylab("Frequency of indicated SNVs")+
          theme(legend.position = "right",
                legend.title = element_text(color = "black", size = input$SNV_label_Text_size),
                legend.text = element_text(color = "black", size = input$SNV_label_Text_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(color = "black", size = input$SNV_label_Text_size),
                axis.title = element_text(color = "black", size = input$SNV_title_Text_size),
                text = element_text(face = "bold", family = "Times", color = "black"))
      }else{
        p <- ggplot2::ggplot(SNP_type_data, aes(x = group, y = freq, fill = type))+
          geom_bar(stat = "summary", color = NA, position = "stack")+
          scale_fill_brewer(palette = "Paired")+
          xlab(NULL) + ylab("Frequency of indicated SNVs")+
          theme_classic()+
          theme(legend.position = "right",
                legend.title = element_text(color = "black", size = input$SNV_label_Text_size),
                legend.text = element_text(color = "black", size = input$SNV_label_Text_size),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text = element_text(color = "black", size = input$SNV_label_Text_size),
                axis.title = element_text(color = "black", size = input$SNV_title_Text_size),
                text = element_text(face = "bold", family = "Times", color = "black"))
      }
      
      if(!is.null(input$barplot_ggText)){
        add_funcs <- strsplit(input$barplot_ggText, "\\+")[[1]]
        p <- p + lapply(add_funcs, function(x){
          eval(parse(text = x))
        })
      }
    }
    return(p)
  })
  
  
  
  output$Display_SNP_Plot <- renderPlot({
    return(SNP_Analysis_plot())
  })
  
  
  
  output$SNP_plot <- downloadHandler(
    filename = function(){ paste(paste("3_SNP_Analyse",input$SNP_Analyse_mode, sep = "_"), ".pdf")},
    content = function(file){
      pdf(file, width = input$SNP_download_width, height = input$SNP_download_height)
      print(SNP_Analysis_plot())
      dev.off()
    }
  )
  
  
  
  output$snp_analysis_data <- DT::renderDataTable(
    return(SNP_Analysis_data()),
    options = list(
      scrollX = TRUE,
      pageLength = 5
    ))
  
  output$snp_tab <- downloadHandler(
    filename = function()  {paste0("3_snp_tab",".csv")},
    content = function(file) {
      write.csv(SNP_Analysis_data(), file, row.names = F)
    }
  )
  
  ##################################################################################################3
  
  ######################################## VCF shiny 5.Indel_Analysis_server ########################
  
  
  Indel_data <- reactive({
    Indel_ALL_list <- lapply(stringr::str_subset(names(raw_variants_list()),pattern = ".indel"),function(x){
      Indel_ALL <- raw_variants_list()[[x]]
    })
    names(Indel_ALL_list) <- stringr::str_subset(names(raw_variants_list()),pattern = ".indel")
    return(Indel_ALL_list)
  })
  
  
  output$Indel_analysis_group <- renderUI({
    observe(Indel_data())
    virtualSelectInput(
      inputId = "Indel_analysis_Group",  label = "Sample groups:",
      choices = unique(gsub("-[0-9].indel$|_[0-9].indel$|.indel","",names(Indel_data()))),
      selected = unique(gsub("-[0-9].indel$|_[0-9].indel$|.indel","",names(Indel_data()))),
      multiple = T, search = F, width = "100%"
    )
  })
  
  Indel_Length_Data <- eventReactive(input$Indel_Analysis_action, {
    withProgress(message = "Loading Indel data...", min = 0, max = 1, {
      indel_list <- Indel_data()
      df <- lapply(names(indel_list), function(x){
        df <- indel_list[[x]]
        df$Indel_length <- nchar(df[, 5]) - nchar(df[, 4])
        df$Indel_type <- ifelse(df$Indel_length > 0, "Insertion", "Delection")
        
        df$group <- gsub("-[0-9].indel$|_[0-9].indel$|.indel","", x)
        df$samples <- x
        df
      }) %>% dplyr::bind_rows()
      incProgress(0.4, message = "Analyse Indel data ...")
      
      if(input$Indel_Analyse_mode == "density"){
        df$Indel_length <- abs(df$Indel_length)
      }else {
        df$Indel_size = ifelse(df$Indel_length>50, "I >50bp",
                               ifelse(df$Indel_length>25, "I 26-50bp",
                                      ifelse(df$Indel_length>10, "I 11-25bp",
                                             ifelse(df$Indel_length>7, "I 8-10bp",
                                                    ifelse(df$Indel_length>4, "I 5-7bp",
                                                           ifelse(df$Indel_length>1, "I 2-4bp",
                                                                  ifelse(df$Indel_length == 1, "I 1bp",
                                                                         ifelse(df$Indel_length == -1, "D 1bp",
                                                                                ifelse(df$Indel_length>-5, "D 2-4bp",
                                                                                       ifelse(df$Indel_length>-8,"D 5-7bp",
                                                                                              ifelse(df$Indel_length>-11,"D 8-10bp",
                                                                                                     ifelse(df$Indel_length>-26,"D 11-25bp",
                                                                                                            ifelse(df$Indel_length>-50,"D 26-50bp","D >50bp")))))))))))))
        df$Indel_size <- factor(df$Indel_size, levels = c("D >50bp","D 26-50bp","D 11-25bp","D 8-10bp","D 5-7bp","D 2-4bp","D 1bp",
                                                          "I >50bp","I 26-50bp","I 11-25bp","I 8-10bp","I 5-7bp","I 2-4bp","I 1bp"))
        df <- df %>% dplyr::group_by(samples, group, Indel_size) %>% dplyr::count() %>% as.data.frame()
        df <- lapply(df$samples, function(x){
          df <- df[df$samples == x, ]
          df$Proportion <- ((df$n) / sum(df$n)) * 100
          df
        }) %>% bind_rows()
      }
      df <- df[df$group %in% input$Indel_analysis_Group, ]
      incProgress(0.8, message = "Drawwing Indel Plot ...")
    })
    return(df)
  })
  
  Indel_Length_plot <- eventReactive(input$Indel_Analysis_action, {
    req(Indel_Length_Data(), input$Indel_Analyse_mode)
    if(input$Indel_Analyse_mode == "density"){
      ggplot(Indel_Length_Data(), aes(Indel_length, fill = samples, color = samples))+
        xlab("Indel Length (bp)")+
        geom_density(alpha = input$Indel_density_alpha, position = input$Indel_density_position)+
        facet_wrap(.~Indel_type)+
        geom_rug()+
        theme_bw()#+
        #theme(text = element_text(face = "bold", family = "Times", size = input$Indel_density_text_size, color = "black"))
    }else{
      ggplot2::ggplot(Indel_Length_Data(), aes(samples, Indel_size,fill = Proportion))+
        geom_tile()+
        labs(x = NULL, y = NULL)+
        geom_text(aes(label = round(Proportion, digits = input$Indel_heatmap_digits)), size = (input$Indel_heatmap_Text_size)/4)+
        scale_fill_gradient(low = stringr::str_split(input$Indel_heatmap_color, ",")[[1]][1],
                            high = stringr::str_split(input$Indel_heatmap_color, ",")[[1]][2])+
        theme_test()#+
        # theme(axis.text.x = element_text(angle = 45, hjust = 1),
        #       text = element_text(face = "bold", family = "Times", size = input$Indel_heatmap_Text_size, color = "black"))
    }
  })
  
  output$Indel_Density_data <- DT::renderDataTable(
    return(Indel_Length_Data()),
    options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      pageLength = 5
    ))
  
  output$indel_tab <- downloadHandler(
    filename = function()  {paste0("4_indel_tab",".csv")},
    content = function(file) {
      write.csv(Indel_Length_Data(), file, row.names = F)
    }
  )
  
  output$Display_Indel_plot <- renderPlot({
    return(Indel_Length_plot())
  })
  
  
  output$Indel_plot <- downloadHandler(
    filename = function() {paste(paste("4_Indel_Analyse", input$Indel_Analyse_mode, sep = "_"),".pdf")},
    content = function(file){
      pdf(file, width = input$Indel_download_width, height = input$Indel_download_height)
      print(Indel_Length_plot())
      dev.off()
    })
  
  #################################################################################################
  
  #######################################  VCF shiny 6.circle_server.R ###################
  
  tabItem(
    tabName = "Indel_analysis",
    fluidRow(
      box(
        title = "Set Indel Analysis Plot", width = 3,  status = "info", collapsible = T,
        virtualSelectInput("Indel_Analyse_mode", "Analyse Mode:", choices = c("density", "heatmap"), multiple = F,
                           zIndex = 4, search = F, width = "100%"),
        uiOutput("Indel_analysis_group"),
        conditionalPanel(
          "input.Indel_Analyse_mode == 'density'",
          virtualSelectInput("Indel_density_position", "Density Position:", choices = c("stack","identity"), multiple = F,
                             zIndex = 4, search = F, width = "100%"),
          sliderInput("Indel_density_text_size","Text Size:", min = 0, max = 30, value = 18, step = 1, width = "100%"),
          sliderInput("Indel_density_alpha", "Alpha Size:", min = 0 , max = 1, value = 0.5, step = 0.1, width = "100%")
        ),
        conditionalPanel(
          "input.Indel_Analyse_mode == 'heatmap'",
          textInput("Indel_heatmap_color", "Indel Heatmap High Freq Color:", value = "white,red", width = "100%"),
          sliderInput("Indel_heatmap_Text_size", "Text size:", min = 0, max = 30, value = 18, step = 1, width = "100%"),
          sliderInput("Indel_heatmap_digits", "Snp Heatmap Significant Digits:", min = 0, max = 5, value = 2, width = "100%")
        ),
        actionButton("Indel_Analysis_action","SNP ALL Variants Type Plot",
                     style = "background-color: #76a5af; border-radius: 28px;margin-top: 14px",  width = "100%")
      ),
      box(
        title = "Display Indel Analysis Plot", width = 9,  status = "primary", collapsible = T,
        div(
          style = "position: absolute; right: 0.5em; top: 0.5em;",
          dropdown(
            label = "Set Download Plot Elements",  width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save", lib = "glyphicon"),
            sliderInput("Indel_download_width", "Download Plot width:", min = 0, max = 15, value = 9, step = 0.5, width = "100%"),
            sliderInput("Indel_download_height", "Download Plot Height:", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
            h4("Download Plot:"),
            downloadButton("Indel_plot","Download Plot")
          )
        ),
        div(
          shinycssloaders::withSpinner(plotOutput("Display_Indel_plot", height = "442px"))
        )
      ),
      box(
        title = "Display Indel Analysis Data ", width = 12, status = "success", collapsible = T,
        div(
          style = "position: absolute; right:0.5em; top: 0.5em;",
          downloadButton('indel_tab','Download CSV', class = "btn", icon = icon("download", lib = "glyphicon"))
        ),
        div(
          DT::dataTableOutput("Indel_Density_data")
        )
      )
    )
  )
  
  
  ##############################################################################
  
  ######################################## VCF shiny 7.Distribution_server ################################
  
  output$distribution_type_group <- renderUI({
    virtualSelectInput(
      inputId = "distribution_type_group",  label = "Sample groups:",
      choices = unique(gsub("-[0-9].snp|_[0-9].snp|.snp|-[0-9].indel|_[0-9].indel|.indel","",names(raw_variants_list()))),
      selected = unique(gsub("-[0-9].snp|_[0-9].snp|.snp|-[0-9].indel|_[0-9].indel|.indel","",names(raw_variants_list()))),
      multiple = T, search = F, width = "100%"
    )
  })
  
  output$distribution_feature_column <- renderUI({
    virtualSelectInput(
      inputId = "distribution_feature_column",  label = "Select Genomic Feature Column:",
      choices = colnames(raw_variants_list()[[1]]),
      selected = colnames(raw_variants_list()[[1]])[6], multiple = F, search = F, width = "100%"
    )
  })
  
  
  
  distribution_binded <- eventReactive(input$plot_distribution, {
    distribution_bind <- data.frame()
    sample_ID <- stringr::str_subset(names(raw_variants_list()), pattern = input$distribution_type_id, negate = F)
    sample_ID <- lapply(input$distribution_type_group, function(x){
      stringr::str_subset(sample_ID, pattern = x, negate = F)
    }) %>% unlist
    
    distribution_bind <- lapply(sample_ID, function(x){
      data <- raw_variants_list()[[x]]
      colnames(data)[colnames(data) == input$distribution_feature_column] <- "feature_column"
      
      no_splite_df <- as.data.frame(data[grep(";|&", data$feature_column, invert = T), ])
      need_splite_df <- as.data.frame(data[grep(";|&", data$feature_column), ])
      if(nrow(need_splite_df)>0){
        splited_df <- need_splite_df %>% tidyr::separate_rows(feature_column, sep = ";|&") %>% unique()
        combined_df <- rbind(no_splite_df, splited_df)
      }else{
        combined_df <- no_splite_df
      }
      number_df <- combined_df %>% dplyr::group_by(feature_column) %>% dplyr::count() %>% as.data.frame()
      number_df$sample_name <- x
      number_df$group <- gsub("-[0-9].snp|_[0-9].snp|.snp|-[0-9].indel|_[0-9].indel|.indel", "", x)
      number_df$percentage <- number_df$n / sum(number_df$n) * 100
      number_df[number_df$percentage < 0.5, "feature_column"] <- "Other"
      number_df <- number_df %>% dplyr::group_by(feature_column, sample_name, group) %>% dplyr::summarise(numbers = sum(n),
                                                                                                          percentage = sum(percentage))
      return(number_df)
    }) %>% dplyr::bind_rows()
    
    return(distribution_bind)
  })
  
  output$distribution_data <- DT::renderDataTable(
    return(distribution_binded()),
    options = list(
      scrollX = TRUE,
      pageLength = 5
    )
  )
  
  output$distribution_tab_download <- downloadHandler(
    filename = function()  {paste0("6_distribution_tab",".csv")},
    content = function(file) {
      write.csv(distribution_binded(), file, row.names = F)
    }
  )
  
  
  distribution_plot <- eventReactive(input$plot_distribution, {
    distribution_binded <- distribution_binded()
    if(input$distribution_type_position == "dodge"){
      p <- ggpubr::ggbarplot(distribution_binded, x = "feature_column", y = "percentage", palette = "Paired",
                             add = c("mean_sd"), fill = "group", width = input$distribution_bar_width, position = position_dodge(),
                             add.params = list(width = input$distribution_error_bar_width, shape = "group", size = input$distribution_error_bar_size), ggtheme = theme_classic())+
        xlab(NULL) + ylab("Proportion of variants distribution \n in the genomic features (%)")#+
        # theme(legend.position = "right",
        #       legend.title = element_text(color = "black", size = input$distribution_label_text_size),
        #       legend.text = element_text(color = "black", size = input$distribution_label_text_size),
        #       axis.text.x = element_text(angle = 45, hjust = 1),
        #       axis.text = element_text(color = "black", size = input$distribution_label_text_size),
        #       axis.title = element_text(color = "black", size = input$distribution_title_text_size),
        #       text = element_text(face = "bold", family = "Times", color = "black"))
    }else{
      p <- ggplot2::ggplot(distribution_binded, aes(x = group, y = percentage, fill = feature_column))+
        geom_bar(stat = "summary", color = NA, position = "stack")+
        scale_fill_brewer(palette = "Paired")+
        xlab(NULL) + ylab("Proportion of variants distribution \n in the genomic features (%)")+
        theme_classic()#+
        # theme(legend.position = "right",
        #       legend.title = element_text(color = "black", size = input$distribution_label_text_size),
        #       legend.text = element_text(color = "black", size = input$distribution_label_text_size),
        #       axis.text.x = element_text(angle = 45, hjust = 1),
        #       axis.text = element_text(color = "black", size = input$distribution_label_text_size),
        #       axis.title = element_text(color = "black", size = input$distribution_title_text_size),
        #       text = element_text(face = "bold", family = "Times", color = "black"))
    }
    
    if (!is.null(input$distribution_ggText)) {
      add_funcs <- strsplit(input$distribution_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    
    return(p)
  })
  
  output$Distribution_Plot <- renderPlot({
    distribution_plot()
  })
  
  
  output$Distribution_Download <- downloadHandler(
    filename = function(){
      paste(paste("6", input$distribution_type_id, input$distribution_type_position, "ALL_Variants_Distribution_plot", sep = "_"), "pdf",sep = ".")
    },
    content = function(file){
      pdf(file,width = input$Distribution_download_width,height = input$Distribution_download_height)
      print(distribution_plot())
      dev.off()
    }
  )
  
  
  ######################################################################################
  
  ######################################### VCF shiny 8.VariantsGene_Summarise_server.R ##################
  output$Variants_sample_id <- renderUI({
    virtualSelectInput(
      inputId = "Variants_sampleID",  label = "Select samples:",
      choices = unique(gsub("\\..*","",names(raw_variants_list()))),
      selected = unique(gsub("\\..*","",names(raw_variants_list()))),
      multiple = F, search = F, width = "100%"
    )
  })
  
  output$Variants_feature_column <- renderUI({
    virtualSelectInput(
      inputId = "variants_feature_column",  label = "Select Genomic Feature Column:",
      choices = colnames(raw_variants_list()[[1]]),
      selected = colnames(raw_variants_list()[[1]])[6], multiple = F, search = F, width = "100%"
    )
  })
  
  output$Variants_gene_column <- renderUI({
    virtualSelectInput(
      inputId = "variants_gene_column",  label = "Select Related Gene Column:",
      choices = colnames(raw_variants_list()[[1]]),
      selected = colnames(raw_variants_list()[[1]])[7], multiple = F, search = F, width = "100%"
    )
  })
  
  output$Variants_bar_position_id <- renderUI({
    if (input$Variants_type_id == "all") {
      simplifed_df1 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".indel")]] %>% dplyr::bind_rows()
      simplifed_df2 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".snp")]] %>% dplyr::bind_rows()
      simplifed_df <- rbind(simplifed_df1, simplifed_df2)
      colnames(simplifed_df)[colnames(simplifed_df) == input$variants_feature_column] <- "feature_column"
    }else {
      simplifed_df <- raw_variants_list()[[paste0(input$Variants_sampleID, ".", input$Variants_type_id)]]
      colnames(simplifed_df)[colnames(simplifed_df) == input$variants_feature_column] <- "feature_column"
    }
    no_splite_df <- as.data.frame(simplifed_df[grep(";|&", simplifed_df$feature_column, invert = T), ])
    need_splite_df <- as.data.frame(simplifed_df[grep(";|&", simplifed_df$feature_column), ])
    if(nrow(need_splite_df) > 0){
      splited_df <- need_splite_df %>% tidyr::separate_rows(feature_column, sep = ";|&") %>% unique()
      combined_df <- rbind(splited_df, no_splite_df)
    }else{
      combined_df <- no_splite_df
    }
    
    virtualSelectInput(
      inputId = "Variants_bar_position_ID",  label = "Select Position To Analyse:",
      choices = unique(combined_df$feature_column),  selected = unique(combined_df$feature_column), multiple = T, zIndex = 4, search = F, width = "100%"
    )
  })
  
  Variants_genes <- eventReactive(input$plot_Variants_genes, {
    raw_variants_list <- raw_variants_list()
    
    if (input$Variants_type_id == "all") {
      simplifed_df1 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".indel")]] %>% dplyr::bind_rows()
      simplifed_df2 <- raw_variants_list()[[paste0(input$Variants_sampleID, ".snp")]] %>% dplyr::bind_rows()
      simplifed_df <- rbind(simplifed_df1, simplifed_df2)
      colnames(simplifed_df)[colnames(simplifed_df) == input$variants_feature_column] <- "feature_column"
      colnames(simplifed_df)[colnames(simplifed_df) == input$variants_gene_column] <- "gene_column"
    }else {
      simplifed_df <- raw_variants_list()[[paste0(input$Variants_sampleID, ".", input$Variants_type_id)]] %>% dplyr::bind_rows()
      colnames(simplifed_df)[colnames(simplifed_df) == input$variants_feature_column] <- "feature_column"
      colnames(simplifed_df)[colnames(simplifed_df) == input$variants_gene_column] <- "gene_column"
    }
    
    need_splite_df <- as.data.frame(simplifed_df[(grepl(";", simplifed_df$gene_column) | grepl(";|&", simplifed_df$feature_column)),])
    no_splite_df <- as.data.frame(simplifed_df[!(grepl(";", simplifed_df$gene_column) | grepl(";|&", simplifed_df$feature_column)) ,])
    
    if(nrow(need_splite_df) > 0){
      splited_df <- need_splite_df %>% tidyr::separate_rows(feature_column, sep = ";|&") %>%
        tidyr::separate_rows(gene_column, sep = ";") %>% unique()
      combined_df <- rbind(splited_df, no_splite_df)
    }else{
      combined_df <- no_splite_df
    }
    combined_df <- combined_df %>% dplyr::group_by(feature_column, gene_column) %>% dplyr::count()
    combined_df <- combined_df[combined_df$feature_column %in% input$Variants_bar_position_ID, ]
    colnames(combined_df) <- c("Position", "Genes", "Numbers")
    combined_df <- combined_df[combined_df$Genes !=  "NONE", ]
    combined_df <- combined_df[combined_df$Genes !=  "", ]
    df <- combined_df %>% dplyr::group_by(Position, Genes) %>% summarise_at(.vars = "Numbers", sum)
    df <- df[order(df$Numbers, decreasing = T), ]
    return(df)
  })
  
  
  output$Variants_df <- DT::renderDataTable(
    Variants_genes(),
    options = list(
      scrollX = TRUE,
      pageLength = 5
    )
  )
  
  output$summary_gene_tab <- downloadHandler(
    filename = function()  {paste0("7_Relevant Genes",".csv")},
    content = function(file) {
      write.csv(Variants_genes(), file, row.names = F)
    }
  )
  
  
  
  Variants_plot <- eventReactive(input$plot_Variants_genes, {
    withProgress(message = "processing", min = 0, max = 1, {
      incProgress(0.4, detail = "Analyse Data ...")
      Variants_df <- Variants_genes() %>% group_by(Genes) %>% summarise_at(.vars = "Numbers", sum)
      Variants_df <- Variants_df[Variants_df$Genes != "",]
      Variants_genes <- Variants_df[order(Variants_df$Numbers, decreasing = T), ][1:input$Variants_genes_numbers, ]
      incProgress(0.6, detail = "Analyse Data...")
      p <- ggpubr::ggbarplot(Variants_genes, x = "Genes", y = "Numbers", label = as.logical(input$Variants_genes_label),
                             width = input$Variants_genes_bar_width, position = position_dodge(),
                             fill = "Genes", ggtheme = theme_classic())+
        xlab(NULL) + ylab("Variants Genes Numbers of biotypes")#+
        # theme(legend.position = "none",
        #       axis.text.x = element_text(angle = 45, hjust = 1),
        #       axis.text = element_text(color = "black", size = input$Variants_genes_label_text_size),
        #       axis.title = element_text(color = "black", size = input$Variants_genes_title_text_size),
        #       text = element_text(face = "bold", family = "Times", color = "black"))
      
      if (!is.null(input$variants_genes_ggText)){
        add_funcs <- strsplit(input$variants_genes_ggText, "\\+")[[1]]
        p <- p + lapply(add_funcs, function(x){
          eval(parse(text = x))
        })
      }
      incProgress(0.8, detail = "Analyse data...")
      return(p)
    })
  })
  
  output$Variants_Plots <- renderPlot({
    Variants_plot()
  })
  
  
  output$Variants_genes_Download <- downloadHandler(
    req(input$Variants_Genes_download_width, input$Variants_Genes_download_height),
    filename = function(){
      paste(paste("7", input$Variants_sampleID,input$Variants_type_id, "Variants_Genes_Summerise_plot",  sep = "_"), "pdf",  sep = ".")
    },
    content = function(file){
      pdf(file, width = input$Variants_Genes_download_width, height = input$Variants_Genes_download_height)
      print(Variants_plot())
      dev.off()
    }
  )
  
  
  ########################################################################################################
  
  ######################################### VCF shiny 9.VariantsGene_Heatmap_server.R ###################
  
  output$Variants_heatmap_sampleID <- renderUI({
    virtualSelectInput(
      inputId = "Variants_heatmap_sampleID",  label = "Select samples:",
      choices = unique(gsub("\\..*","",names(raw_variants_list()))),
      selected = unique(gsub("\\..*","",names(raw_variants_list()))),
      multiple = T, search = F, width = "100%"
    )
  })
  
  output$Variants_heatmap_feature_column <- renderUI({
    virtualSelectInput(
      inputId = "variants_heatmap_feature_column",  label = "Select Genomic Feature Column:",
      choices = colnames(raw_variants_list()[[1]]),
      selected = colnames(raw_variants_list()[[1]])[6], multiple = F, search = F, width = "100%"
    )
  })
  
  output$Variants_heatmap_gene_column <- renderUI({
    virtualSelectInput(
      inputId = "variants_heatmap_gene_column",  label = "Select Related Gene Column:",
      choices = colnames(raw_variants_list()[[1]]),
      selected = colnames(raw_variants_list()[[1]])[7], multiple = F, search = F, width = "100%"
    )
  })
  
  
  output$Variants_position_id <- renderUI({
    if (input$Variants_heatmap_types == "all") {
      simplifed_df1 <- raw_variants_list()[paste0(input$Variants_heatmap_sampleID, ".indel")] %>% dplyr::bind_rows()
      simplifed_df2 <- raw_variants_list()[paste0(input$Variants_heatmap_sampleID, ".snp")] %>% dplyr::bind_rows()
      simplifed_df <- rbind(simplifed_df1, simplifed_df2)
    }else {
      simplifed_df <- raw_variants_list()[paste0(input$Variants_heatmap_sampleID, ".", input$Variants_heatmap_types)] %>% dplyr::bind_rows()
    }
    colnames(simplifed_df)[colnames(simplifed_df) == input$variants_heatmap_feature_column] <- "feature_column"
    no_splite_df <- as.data.frame(simplifed_df[grep(";|&", simplifed_df$feature_column, invert = T), ])
    need_splite_df <- as.data.frame(simplifed_df[grep(";|&", simplifed_df$feature_column), ])
    
    if(nrow(need_splite_df) > 0){
      splited_df <- need_splite_df %>% tidyr::separate_rows(feature_column, sep = ";|&") %>% unique()
      combined_df <- rbind(no_splite_df, splited_df)
    }else{
      combined_df <- no_splite_df
    }
    
    virtualSelectInput(
      inputId = "Variants_position_ID",  label = "Select Position To Analyse:",
      choices = unique(combined_df$feature_column),  selected = unique(combined_df$feature_column), multiple = T, zIndex = 4,search = F, width = "100%"
    )
  })
  
  
  
  Variants_heatmap_data <- eventReactive(input$Variants_heatmap, {
    Variants_heatmap_df <- lapply(input$Variants_heatmap_sampleID, function(x){
      if (input$Variants_heatmap_types == "all") {
        simplifed_df1 <- raw_variants_list()[paste0(x, ".indel")] %>% dplyr::bind_rows()
        simplifed_df2 <- raw_variants_list()[paste0(x, ".snp")] %>% dplyr::bind_rows()
        simplifed_df <- rbind(simplifed_df1, simplifed_df2)
        colnames(simplifed_df)[colnames(simplifed_df) == input$variants_heatmap_feature_column] <- "feature_column"
        colnames(simplifed_df)[colnames(simplifed_df) == input$variants_heatmap_gene_column] <- "gene_column"
      }else {
        simplifed_df <- raw_variants_list()[paste0(x, ".", input$Variants_heatmap_types)] %>% dplyr::bind_rows()
        colnames(simplifed_df)[colnames(simplifed_df) == input$variants_heatmap_feature_column] <- "feature_column"
        colnames(simplifed_df)[colnames(simplifed_df) == input$variants_heatmap_gene_column] <- "gene_column"
      }
      
      need_splite_df <- as.data.frame(simplifed_df[(grepl(";", simplifed_df$gene_column) | grepl(";|&", simplifed_df$feature_column)) ,])
      no_splite_df <- as.data.frame(simplifed_df[!(grepl(";", simplifed_df$gene_column) | grepl(";|&", simplifed_df$feature_column)) ,])
      
      if(nrow(need_splite_df) > 0){
        splited_df <- need_splite_df  %>% tidyr::separate_rows(feature_column, sep = ";|&") %>% tidyr::separate_rows(gene_column, sep = ";") %>% unique()
        combined_df <- rbind(splited_df,no_splite_df)
      }else{
        combined_df <- no_splite_df
      }
      combined_df <- combined_df %>% dplyr::group_by(feature_column, gene_column) %>% dplyr::count()
      combined_df <- combined_df[combined_df$feature_column %in% input$Variants_position_ID, ]
      colnames(combined_df) <- c("Position", "Genes", "Numbers")
      combined_df <- combined_df[combined_df$Genes !=  "NONE", ]
      combined_df <- combined_df[combined_df$Genes !=  "", ]
      
      combined_df <- combined_df %>% dplyr::group_by(Genes) %>% dplyr::summarise_at(.vars = "Numbers", sum) %>% as.data.frame()
      
      IntOGen <- readRDS(system.file("extdata", "IntOGen_cancer_driver_gene.rds", package = "VCFshiny"))
      combined_df <- combined_df[combined_df$Genes %in% IntOGen$Symbol, ]
      combined_df <- combined_df[order(combined_df$Numbers, decreasing = T), ]
      colnames(combined_df) <- c("Genes", x)
      combined_df <- na.omit(combined_df)
    }) %>% plyr::join_all(by = "Genes", type = "full")
    Variants_heatmap_df[is.na(Variants_heatmap_df)] <- 0
    
    Variants_heatmap_df_g <<-Variants_heatmap_df
    return(Variants_heatmap_df)
  })
  
  
  output$Variants_Heatmap_data <- DT::renderDataTable(
    return(Variants_heatmap_data()),
    options = list(
      scrollX = TRUE,
      pageLength = 5
    )
  )
  
  output$heatmap_gene_tab <- downloadHandler(
    filename = function()  {paste0("8_Heatmap_info_table",".csv")},
    content = function(file) {
      write.csv(Variants_heatmap_data(), file, row.names = F)
    }
  )
  
  
  Heat_Plot_2 <- eventReactive(input$Variants_heatmap, {
    withProgress(message = "processing", min = 0, max = 1, {
      
      Variants_heatmap_data <- as.data.frame(Variants_heatmap_data())
      Variants_heatmap_data <- Variants_heatmap_data %>% column_to_rownames(var = "Genes")
      rownames(Variants_heatmap_data)
      
      pheatmap::pheatmap(Variants_heatmap_data, cluster_rows = F, cluster_cols = F,
                         color = grDevices::colorRampPalette(c("GhostWhite", "red"))(50),
                         show_rownames = as.logical(input$Variants_heatmap_show_rownames),
                         show_colnames = as.logical(input$Variants_heatmap_show_colnames),
                         breaks = c(seq(0, input$Variants_heatmap_colorValue, input$Variants_heatmap_colorValue / 50)),
                         legend_breaks = seq(0, input$Variants_heatmap_colorValue, 2),
                         legend_labels = c(seq(0, input$Variants_heatmap_colorValue - 2, 2) %>% as.character, paste0(input$Variants_heatmap_colorValue, "+")))
    })
  })
  
  
  output$Heatmap_plot_2 <- renderPlot({
    Heat_Plot_2()
  })
  
  
  output$Variants_heatmap_Download <- downloadHandler(
    filename = function(){
      paste(paste("8", input$Variants_heatmap_types, "Variants_Heatmap_Summerise_plot",  sep = "_"), "pdf", sep = ".")
    },
    content = function(file){
      pdf(file, width = input$Variants_heatmap_download_width, height = input$Variants_heatmap_download_height)
      print(Heat_Plot_2())
      dev.off()
    }
  )
  
  ##########################################################################################
  
  ########################################  VCF shiny 10.mutational_signature_server.R ###########################################
  output$signature_sample_id <- renderUI({
    virtualSelectInput(
      inputId = "signature_sampleID",  label = "Select samples:",
      choices = unique(gsub("\\..*","",names(raw_variants_list()))),
      selected = unique(gsub("\\..*","",names(raw_variants_list()))),
      multiple = T, search = F, width = "100%"
    )
  })
  
  mutational_signature_df <- eventReactive(input$analysis_signature, {
    raw_variants_list <- raw_variants_list()
    user <- lapply(input$signature_sampleID, function(x){
      name <- stringr::str_subset(names(raw_variants_list), pattern = x)
      data <- lapply(name, function(y){
        df1 <- raw_variants_list[[y]][,1:5]
      }) %>% bind_rows()
      data$sample <- x
      return(data)
    }) %>% bind_rows()
    
    if(length(unique(user$sample)) == 1){
      sendSweetAlert(session = session, title = "Warning", text = "The number of samples should be greater than 1 !", type = "warning")
      return()
    }
    user <- user[user[,1] %in% c("chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr2","chr20","chr21","chr22",
                                 "chr3","chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY" ), ]
    
    musica_data <- musicatk::create_musica(x = user, genome = musicatk::select_genome(input$signature_genome))
    musicatk::build_standard_table(musica_data, g = musicatk::select_genome(input$signature_genome), table_name = input$Signature_Build_Table_Name, overwrite = T)
    result_data <- musicatk::discover_signatures(musica = musica_data, table_name =  input$Signature_Build_Table_Name, num_signatures = input$num_signatures,
                                                 algorithm = input$signature_discover_algorithm, nstart = 10)
    result_data
    return(result_data)
  })
  
  # Mutational Signature Plot Display and Download
  #draw
  mutational_signature_plot <- eventReactive(input$analysis_signature, {
    mutational_signature_df <- mutational_signature_df()
    if(input$signature_compare_cosmic == "FALSE"){
      musicatk::plot_signatures(mutational_signature_df, text_size = input$signature_discover_text_size, 
                                #facet_size = input$signature_discover_facet_size
      )
    }else{
      comapre_cosmic <- musicatk::compare_cosmic_v2(mutational_signature_df, threshold = input$signature_discover_compare_threshold, )
    }
  })
  #display
  output$Display_mutational_signature_plot <- renderPlot(
    return(mutational_signature_plot())
  )
  #download
  output$Download_mutational_signature_Plot <- downloadHandler(
    filename = function(){ paste0("10_mutational_signature_",input$Signature_Build_Table_Name, ".pdf")},
    content = function(file){
      pdf(file, width = input$Mutational_Signature_download_width, height = input$Mutational_Signature_download_height)
      print(mutational_signature_plot())
      dev.off()
    }
  )
  
  # Mutational Signature Data Display and Download
  #display
  output$Display_mutational_signature_data <- DT::renderDataTable(
    return(mutational_signature_df()@signatures),
    options = list(
      scrollX = TRUE,
      pageLength = 5
    )
  )
  #download
  output$Download_mutational_signature_data <- downloadHandler(
    filename = function()  {paste0("10_mutational_signature_", input$Signature_Build_Table_Name, ".csv")},
    content = function(file) {
      write.csv(mutational_signature_df()@signatures, file, row.names = T)
    }
  )
  
  
  ############################################################################################################
  
  ######################################## VCF shiny 11.Annotation_server.R  ##############################
  output$Download_Annovar_Database_warning <- renderUI({
    sendSweetAlert(session = session, title = "Warning", text = "If the current network environment is not good, there may be errors in the download. You can choose to re-download or download the annotation file from the Annovar official website to the Annovar/huamandb folder of the VCFshiny installation folder !", type = "warning")
  })
  
  output$Annovar_Download_Name <- renderUI({
    Annovar_database_table <- Annovar_database_table()
    selectInput(inputId = "Annovar_download_name", label = "Select Filter-based Datebase",
                choices = Annovar_database_table[Annovar_database_table$Version == input$Annovar_download_species &
                                                   Annovar_database_table$Database_Type ==input$Annovar_download_database_type, "Name"] %>% unique(),
                selected = "cytoBand", width = "100%")
  })
  
  output$Annovar_anno_gene_Database <- renderUI({
    Annovar_database_table <- Annovar_database_table()
    if("-geneanno" %in% input$Annovar_anno_type){
      database_name <- dir(system.file("Annovar/humandb", package = "VCFshiny"))[grep(dir(system.file("Annovar/humandb", package = "VCFshiny")), pattern = paste0(input$Annovar_species,"(.*).txt$"))] %>%
        stringr::str_remove(pattern = paste0("^", input$Annovar_species,"_")) %>% stringr::str_remove(pattern = ".txt$") %>% unlist()
      database_name1 <- database_name[database_name %in% unique(Annovar_database_table[Annovar_database_table$Version == input$Annovar_download_species & Annovar_database_table$Database_Type == "gene-based", "Name"])]
      
      if (length(database_name) == 0 | length(database_name1) == 0) {
        sendSweetAlert(session = session, title = "Warning", text = "Please download Region-based datebase first !", type = "warning")
      }else{
        selectInput(inputId = "Annovar_gene_datebase", label = "Select Region-based Datebase:", choices = database_name1, width = "100%")
      }
    }
  })
  
  output$Annovar_anno_region_Database <- renderUI({
    Annovar_database_table <- Annovar_database_table()
    if("-regionanno" %in% input$Annovar_anno_type){
      database_name <- dir(system.file("Annovar/humandb", package = "VCFshiny"))[grep(dir(system.file("Annovar/humandb", package = "VCFshiny")), pattern = paste0(input$Annovar_species,"(.*).txt$"))] %>%
        stringr::str_remove(pattern = paste0("^", input$Annovar_species,"_")) %>% stringr::str_remove(pattern = ".txt$") %>% unlist()
      database_name1 <- database_name[database_name %in% unique(Annovar_database_table[Annovar_database_table$Version == input$Annovar_download_species & Annovar_database_table$Database_Type == "region-based", "Name"])]
      
      if (length(database_name) == 0 | length(database_name1) == 0) {
        sendSweetAlert(session = session, title = "Warning", text = "Please download Region-based datebase first !", type = "warning")
      }else{
        selectInput(inputId = "Annovar_region_datebase", label = "Select Region-based Datebase:", choices = database_name1, width = "100%")
      }
    }
  })
  
  
  output$Annovar_anno_Filter_Database <- renderUI({
    Annovar_database_table <- Annovar_database_table()
    if("-filter" %in% input$Annovar_anno_type){
      database_name <- dir(system.file("Annovar/humandb", package = "VCFshiny"))[grepl(dir(system.file("Annovar/humandb", package = "VCFshiny")), pattern =  paste0(input$Annovar_species,"(.*).txt$"))] %>%
        stringr::str_remove(pattern = paste0("^", input$Annovar_species,"_")) %>% stringr::str_remove(pattern = ".txt$") %>% unlist()
      database_name1 <- database_name[database_name %in% unique(Annovar_database_table[Annovar_database_table$Version == input$Annovar_download_species & Annovar_database_table$Database_Type == "filter-based", "Name"])]
      
      if (length(database_name) == 0 | length(database_name1) == 0) {
        sendSweetAlert(session = session, title = "Warning", text = "Please download Filter-based datebase first !", type = "warning")
      }else{
        selectInput(inputId = "Annovar_filter_datebase", label = "Select Filter-based Datebase:", choices = database_name1, width = "100%")
      }
    }
  })
  
  # Annovar Datebase Table 
  Annovar_database_table <- reactive({
    Table <- read.csv(system.file("Annovar","Annovar_Datebase_Table.CSV", package = "VCFshiny"), header = T, sep = ",")
  })
  
  
  output$Annovar_Database_Tab <- DT::renderDataTable({
    Annovar_database_table()
  }, rownames = T, options = list(scrollX = TRUE, pageLength =5))
  
  
  output$Annovar_Database_Tab_Download <- downloadHandler(
    filename = function()  {paste0(input$VariantsAnno_Anno_ID,".txt")},
    content = function(file) {
      write.table(Annovar_database_table(), file, sep = "\t", row.names = F)
    })
  
  download_database <- eventReactive(input$Annovar_download_section, {
    withProgress(message = "Downloading files ...", min = 0, max = 1, {
      cat(">>>  preparing datebase upload  ...\t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
      
      ifelse(input$Annovar_download_name == "cytoBand", download_web <- "ucsc", download_web <- "annovar")
      downdb <- try(system2(command = "perl", args = paste0(system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -buildver ", input$Annovar_download_species,
                                                            " -webfrom ", download_web, " -downdb ", input$Annovar_download_name, " ", system.file("Annovar/humandb", package = "VCFshiny")),
                            stdout = T, stderr = T))
      options(timeout=10000000000000)
      if (grepl("http://", downdb) %>% any()) {
        loop <- downdb[grepl("http://", downdb)]
        for (i in loop) {
          destfile <- stringr::str_remove(i, "http://") %>% basename()
          if(grepl(destfile, pattern = 'Version.txt.gz$')){
            download.file(url = i, destfile = paste0(system.file("Annovar/humandb", package = "VCFshiny"), "/", input$Annovar_download_species, "_", input$Annovar_download_name ,"Version", ".txt.gz"))
            # down_txt <- try(download.file(url = i, destfile = paste0(system.file("Annovar/humandb", package = "VCFshiny"), "/", input$Annovar_download_species, "_", input$Annovar_download_name ,"Version", ".txt.gz")))
            # if(grepl("http://", down_txt) %>% any()){ sendSweetAlert(session = session, title = "Warning", text = paste0("Please download database from: ", down_txt[grepl("http://", down_txt)]), type = "warning")}
            R.utils::gunzip(system.file("Annovar/humandb", paste0(input$Annovar_download_species, "_", input$Annovar_download_name,"Version" ,".txt.gz"), package = "VCFshiny"),  overwrite = T)
          }else if (grepl(destfile, pattern = '.txt.gz$')) {
            download.file(url = i, destfile = paste0(system.file("Annovar/humandb", package = "VCFshiny"), "/", input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.gz"))
            R.utils::gunzip(system.file("Annovar/humandb", paste0(input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.gz"), package = "VCFshiny"),  overwrite = T)
          }else if(grepl(destfile, pattern = '.idx.gz$')){
            download.file(url = i, destfile = paste0(system.file("Annovar/humandb", package = "VCFshiny"), "/",input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.idx.gz"))
            R.utils::gunzip(system.file("Annovar/humandb", paste0(input$Annovar_download_species, "_", input$Annovar_download_name, ".txt.idx.gz"), package = "VCFshiny"),  overwrite = T)
          }else if(grepl(destfile, pattern = '.fa.gz$')){
            download.file(url = i, destfile = paste0(system.file("Annovar/humandb", package = "VCFshiny"), "/",input$Annovar_download_species, "_", input$Annovar_download_name,"Mrna.fa.gz"))
            R.utils::gunzip(system.file("Annovar/humandb", paste0(input$Annovar_download_species, "_", input$Annovar_download_name,"Mrna.fa.gz"), package = "VCFshiny"),  overwrite = T)
          }
        }
      }
      df <- as.data.frame(dir(system.file("Annovar/humandb", package = "VCFshiny")))
    })
  })
  
  
  output$Annovar_Download_Tab <- DT::renderDataTable({
    download_database()
  }, rownames = T, options = list(scrollX = TRUE, pageLength =5))
  
  
  output$Annovar_Download_Tab_download <- downloadHandler(
    filename = function()  {paste0(input$VariantsAnno_Anno_ID,".txt")},
    content = function(file) {
      write.table(download_database(), file, sep = "\t", row.names = F)
    })
  
  annotation_Annovar_list <- eventReactive(input$Annovar_section ,{
    withProgress(message = "processing", min = 0, max = 1, {
      cat(">> preparing use Annovar annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
      
      if(is.null(input$input_file_anno)){
        sendSweetAlert(session = session, title = "Warning", text = "Please Upload data first !", type = "warning")
        return()
      }
      
      if(file.exists(stringr::str_remove(string = input$input_file_anno$name, pattern = ".gz|.zip|.tar.gz"))){
        unlink(stringr::str_remove(string = input$input_file_anno$name, pattern = ".gz|.zip|.tar.gz"), recursive = T)
      }
      
      if(stringr::str_detect(input$input_file_anno$name, pattern = "gz$")){
        utils::untar(input$input_file_anno$datapath, exdir = ".")
      }else if(stringr::str_detect(input$input_file_anno$name, pattern = "zip$")){
        utils::unzip(input$input_file_anno$datapath, exdir = ".")
      }
      incProgress(0.2, detail = "uncompressing file...")
      input_dir <- stringr::str_remove(string = input$input_file_anno$name, pattern = ".gz|.zip|.tar.gz")
      
      file_split_name <- dir(input_dir, pattern = ".vcf|.vcf.gz") %>% stringr::str_remove(".vcf.gz|.vcf")
      input_file_name <- dir(input_dir, pattern = ".vcf|.vcf.gz", full.names = T)
      incProgress(0.4, detail = "annotation data...")
      
      vcf_list <- lapply(input_file_name, function(x){
        name <- paste0(getwd(), "/", stringr::str_remove(string = x, pattern = ".vcf.gz|.vcf"))
        cat(">> preparing use Annovar convert data ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
        system(paste0("perl ", system.file("Annovar","convert2annovar.pl", package = "VCFshiny"), " -format vcf4 -allsample -withfreq ", x , " -out ", name))
        
        cat(">> preparing use Annovar Gene-based annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
        system(paste0("perl ", system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -geneanno -dbtype ", input$Annovar_gene_datebase, " -out ", name,
                      " -buildver ", input$Annovar_species ," ", name, " ", system.file("Annovar/humandb", package = "VCFshiny")))
        df1 <- read.csv(paste0(name, ".variant_function"), header = F, sep = "\t")[,1:7]
        colnames(df1) <- c("Func.refGene", "Gene.refGene", "Chr","Start","End","Ref","Alt")
        df1 <- df1[,c(3:7,1:2)]
        
        if(("-regionanno" %in% input$Annovar_anno_type) & !("-filter" %in% input$Annovar_anno_type)){
          cat(">> preparing use Annovar Region-based annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
          system(paste0("perl ", system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -regionanno -dbtype ", input$Annovar_region_datebase ," -out ", name,
                        " -buildver ", input$Annovar_species ," ", name ," ", system.file("Annovar/humandb", package = "VCFshiny")))
          df2 <- read.csv(paste0(name, ".",input$Annovar_species, "_cytoBand"), header = F, sep = "\t")[,2:7]
          colnames(df2) <-  c("cytoBand", "Chr","Start","End","Ref","Alt")
          df <- dplyr::full_join(df1,df2, by = join_by(Chr, Start, End, Ref, Alt)) %>% na.omit()
          return(df)
        }else if(("-filter" %in% input$Annovar_anno_type) & !("-regionanno" %in% input$Annovar_anno_type)){
          cat(">> preparing use Annovar Filter-based annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
          system(paste0("perl ", system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -filter -dbtype ", input$Annovar_filter_datebase,
                        " -out ", name, " -buildver ", input$Annovar_species ," ",  name," ",  system.file("Annovar/humandb", package = "VCFshiny")))
          df3_filtered <- read.csv(paste0(name, ".", input$Annovar_species, "_", input$Annovar_filter_datebase, "_filtered"), header = F, sep = "\t")[,1:5]
          df3_dropped <- read.csv(paste0(name, ".", input$Annovar_species, "_", input$Annovar_filter_datebase, "_dropped"), header = F, sep = "\t")[,2:7]
          colnames(df3_dropped) <- c("datebase", "Chr","Start","End","Ref","Alt")
          df3_dropped <- df3_dropped %>% dplyr::mutate(datebase = as.character(datebase))
          colnames(df3_filtered) <- c("Chr","Start","End","Ref","Alt")
          df3_filtered$datebase <- "."
          df3_filtered <- df3_filtered %>% dplyr::select(datebase, everything())
          df3 <- rbind(df3_dropped, df3_filtered)
          colnames(df3)[1] <- input$Annovar_filter_datebase
          df <- dplyr::full_join(df1, df3, by = join_by(Chr, Start, End, Ref, Alt)) %>%  na.omit()
          return(df)
        }else{
          cat(">> preparing use Annovar Region-based annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
          system(paste0("perl ", system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -regionanno -dbtype ", input$Annovar_region_datebase ," -out ", name,
                        " -buildver ", input$Annovar_species ," ",  name," ",  system.file("Annovar/humandb", package = "VCFshiny")))
          df2 <- read.csv(paste0(name, ".",input$Annovar_species, "_cytoBand"), header = F, sep = "\t")[,2:7]
          colnames(df2) <-  c("cytoBand", "Chr","Start","End","Ref","Alt")
          
          cat(">> preparing use Annovar Filter-based annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
          system(paste0("perl ", system.file("Annovar", "annotate_variation.pl", package = "VCFshiny"), " -filter -dbtype ", input$Annovar_filter_datebase,
                        " -out ", name, " -buildver ", input$Annovar_species ," ",  name," ",  system.file("Annovar/humandb", package = "VCFshiny")))
          df3_filtered <- read.csv(paste0(name, ".", input$Annovar_species, "_", input$Annovar_filter_datebase, "_filtered"), header = F, sep = "\t")[,1:5]
          df3_dropped <- read.csv(paste0(name, ".", input$Annovar_species, "_", input$Annovar_filter_datebase, "_dropped"), header = F, sep = "\t")[,2:7]
          colnames(df3_dropped) <- c("datebase", "Chr","Start","End","Ref","Alt")
          colnames(df3_filtered) <- c("Chr","Start","End","Ref","Alt")
          df3_filtered$datebase <- "."
          df3_filtered <- df3_filtered %>% dplyr::select(datebase, everything())
          df3 <- rbind(df3_dropped, df3_filtered)
          colnames(df3)[1] <- input$Annovar_filter_datebase
          
          df <- dplyr::full_join(df1,df2, by = join_by(Chr, Start, End, Ref, Alt)) %>% full_join(df3, by = join_by(Chr, Start, End, Ref, Alt)) %>% na.omit()
          return(df)
        }
      })
      unlink(input_dir, recursive = T)
      names(vcf_list) <- file_split_name
      incProgress(0.8, detail = "Data Visualization...")
      return(vcf_list)
    })
  })
  
  annotation_variantsAnno_list <- eventReactive(input$VariantAnnotation_section, {
    withProgress(message = "processing", min = 0, max = 1, {
      cat(">> preparing use Annovar annotation ... \t\t", format(Sys.time(), "%Y-%m-%d %X"), "\n")
      
      if(is.null(input$input_file_Variantanno)){
        sendSweetAlert(session = session, title = "Warning", text = "Please Upload data first !", type = "warning")
        return()
      }
      
      if(file.exists(stringr::str_remove(string = input$input_file_Variantanno$name, pattern = ".gz|.zip|.tar.gz"))){
        unlink(stringr::str_remove(string = input$input_file_Variantanno$name, pattern = ".gz|.zip|.tar.gz"), recursive = T)
      }
      
      if(stringr::str_detect(input$input_file_Variantanno$name, pattern = "gz$")){
        utils::untar(input$input_file_Variantanno$datapath, exdir = ".")
      }else if(stringr::str_detect(input$input_file_Variantanno$name, pattern = "zip$")){
        utils::unzip(input$input_file_Variantanno$datapath, exdir = ".")
      }
      
      incProgress(0.2, detail = "uncompressing file...")
      input_dir <- stringr::str_remove(string = input$input_file_Variantanno$name, pattern = ".gz|.zip|.tar.gz")
      file_split_name <- dir(input_dir, pattern = ".vcf$|.vcf.gz$") %>% stringr::str_remove(".vcf.gz|.vcf")
      input_file_name <- dir(input_dir, pattern = ".vcf$|.vcf.gz$", full.names = T) %>% stringr::str_remove(".vcf.gz$|.vcf$")
      
      incProgress(0.4, detail = "annotation data...")
      annotation_list <- lapply(input_file_name, function(x){
        name <- paste0(x, ".vcf")
        if(!file.exists(name)){
          R.utils::gunzip(paste0(x, ".vcf.gz"), remove = T, overwrite = T)
        }
        
        df <- VariantAnnotation::readVcf(name, genome = input$Variantanno_species)
        if(!("chr1" %in% seqlevels(df))){
          seqlevels(df) <- paste("chr", seqlevels(df), sep = "")
        }
        seqlevels(df, pruning.mode="coarse") <- seqlevels(df)[seqlevels(df) %in% c("chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr2","chr20","chr21","chr22",
                                                                                   "chr3","chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY" )]
        if(input$Variantanno_species == "hg38"){
          df <- VariantAnnotation::locateVariants(MatrixGenerics::rowRanges(df), TxDb.Hsapiens.UCSC.hg38.knownGene , VariantAnnotation::AllVariants())
        }else if(input$Variantanno_species == "hg19"){
          df <- VariantAnnotation::locateVariants(MatrixGenerics::rowRanges(df), TxDb.Hsapiens.UCSC.hg19.knownGene , VariantAnnotation::AllVariants())
        }
        list <- strsplit(names(GRanges(df)), split = "_|/",fixed = F)
        df$Ref <- sapply(list, "[",2)
        df$Alt <- sapply(list, "[",3)
        df <- as.data.frame(df, row.names = NULL)
        df <- df[!(is.na(df$GENEID)),]
        df$SYMBOLID <- AnnotationDbi::select(org.Hs.eg.db, keys = df$GENEID, keytype = "ENTREZID", columns = "SYMBOL")[,2]
        df <- df[, c("seqnames", "start", "end", "Ref", "Alt", "LOCATION", "SYMBOLID", "GENEID", "LOCSTART","LOCEND", "QUERYID", "TXID")]
        colnames(df) <- c("Chr", "Start", "End", "Ref", "Alt", "LOCATION", "SYMBOLID", "GENEID", "LOCSTART","LOCEND", "QUERYID", "TXID")
        df <- df %>% dplyr::group_by(Chr, Start, End, Ref, Alt, LOCATION) %>% dplyr::summarise(SYMBOLID = paste(na.omit(unique(SYMBOLID)), collapse = ";"),
                                                                                               GENEID = paste(na.omit(unique(GENEID)), collapse = ";"),
                                                                                               LOCSTART = paste(na.omit(unique(LOCSTART)), collapse = ";"),
                                                                                               LOCEND = paste(na.omit(unique(LOCEND)), collapse = ";"),
                                                                                               QUERYID = paste(na.omit(unique(QUERYID)), collapse = ";"),
                                                                                               TXID = paste(na.omit(unique(TXID)), collapse = ";"))
        df <- df[df$SYMBOLID != "", ]
        return(df)
      })
      unlink(input_dir, recursive = T)
      names(annotation_list) <- file_split_name
      incProgress(0.8, detail = "Data Visualization...")
      return(annotation_list)
    })
  })
  
  
  output$VariantsAnno_anno_id <- renderUI({
    samples <- names(annotation_variantsAnno_list())
    selectInput("VariantsAnno_Anno_ID", "Select your samples to view:", choices = samples,  width = "100%")
  })
  
  output$variantanno_anno_table <- DT::renderDataTable({
    req(input$VariantsAnno_Anno_ID)
    annotation_variantsAnno_list()[[input$VariantsAnno_Anno_ID]]
  }, rownames = T, options = list(scrollX = TRUE, pageLength =5))
  
  
  output$variantanno_tab_download <- downloadHandler(
    filename = function()  {paste0(input$VariantsAnno_Anno_ID,".txt")},
    content = function(file) {
      write.table(annotation_variantsAnno_list()[[input$VariantsAnno_Anno_ID]], file, sep = "\t", row.names = F)
    })
  
  output$Annovar_anno_id <- renderUI({
    samples <- names(annotation_Annovar_list())
    selectInput("Annovar_Anno_ID", "Select your samples to view:", choices = samples,  width = "40%")
  })
  
  output$Annovar_anno_table <- DT::renderDataTable({
    annotation_Annovar_list()[[input$Annovar_Anno_ID]]
  }, rownames = T, options = list(scrollX = TRUE, pageLength =5))
  
  
  output$Annovar_tab_download <- downloadHandler(
    filename = function()  {paste0(input$Annovar_Anno_ID,".txt")},
    content = function(file) {
      write.table(annotation_Annovar_list()[[input$Annovar_Anno_ID]], file, sep = "\t", row.names = F)
    })
  
  
  observeEvent(input$view_table, {
    updateTabItems(session, "tabs2", selected =  "result2")
    updateSelectInput(session, "select_job_id_r2", choices = setNames( job_data$Job_ID, paste(job_data$Patient_ID,job_data$Job_ID, sep="-")),selected =input$select_job_id_r1 )
  })
  ########################################################################################################

  ###########################################TMB SCORE SHEET #######################################
  observeEvent(input$select_job_id_tmb, {
    req(input$select_job_id_tmb)
    if (input$select_job_id_tmb != "") {
      selected_job <- job_data[job_data$Job_ID == input$select_job_id_tmb, ]
      if (nrow(selected_job) == 1) {
        patient_id <- selected_job$Patient_ID
        output$display_patient_id_tmb <- renderText({ paste("Patient ID:", patient_id) })
        job_id <- input$select_job_id_tmb

  selected_job <- job_data[job_data$Job_ID == input$select_job_id_tmb, ]

  folder_path = paste0("OncoGenAI/results/", job_id, "/")
  genome <- selected_job$Genome
  if(selected_job$Workflow == "IonTorrent"){
    vcf_file_path <- paste0(folder_path,job_id,"_bed_filtered.merged.vcf")  
    #vcf_file_path <- "TSVC_variants.vcf"
    gene_panel_file <- paste0("panel_file/", input$panel_final)

    genome_assembly <- genome # Genome assembly version (hg19/hg38)
    vaf_cutoff <- as.numeric(input$vaf_value) # VAF cutoff value
    if (file.exists(str_replace(vcf_file_path,".vcf","_TMB.csv"))){
      print(str_replace(vcf_file_path,".vcf","_TMB.csv"))
    }else{
      cmd<-paste0("Rscript TMB.R ",vcf_file_path," ",gene_panel_file," ",genome_assembly," ",as.character(vaf_cutoff))
      print(cmd)
      system(cmd)
      }}
  else{
    vcf_file_path <- paste0(folder_path,job_id,"_somatic.vcf")  
    #vcf_file_path <- "TSVC_variants.vcf"
    gene_panel_file <- paste0("panel_file/", input$panel_final)

    genome_assembly <- genome # Genome assembly version (hg19/hg38)
    vaf_cutoff <- as.numeric(input$vaf_value) # VAF cutoff value
    if (file.exists(str_replace(vcf_file_path,".vcf","_TMB.csv"))){
      print(str_replace(vcf_file_path,".vcf","_TMB.csv"))
    }else{
      cmd<-paste0("Rscript TMB.R ",vcf_file_path," ",gene_panel_file," ",genome_assembly," ",as.character(vaf_cutoff))
      print(cmd)
      system(cmd)
      }}
  # vcf_file_path <- paste0(folder_path,job_id,"_bed_filtered.merged.vcf")  
  # #vcf_file_path <- "TSVC_variants.vcf"
  # gene_panel_file <- paste0("panel_file/", input$panel_final)

  # genome_assembly <- genome # Genome assembly version (hg19/hg38)
  # vaf_cutoff <- as.numeric(input$vaf_value) # VAF cutoff value
  # if (file.exists(str_replace(vcf_file_path,".vcf","_TMB.csv"))){
  #     print(str_replace(vcf_file_path,".vcf","_TMB.csv"))
  # }else{
  #   cmd<-paste0("Rscript TMB.R ",vcf_file_path," ",gene_panel_file," ",genome_assembly," ",as.character(vaf_cutoff))
  #   print(cmd)
  #   system(cmd)
  # }

  # Print the input files, genome assembly, and VAF cutoff for debugging
  # cat("VCF File Path: ", vcf_file_path, "\n")
  # cat("Gene Panel File Path: ", gene_panel_file, "\n")
  # cat("Genome Assembly Version: ", genome_assembly, "\n")
  # cat("VAF Cutoff: ", vaf_cutoff, "\n")

  # # Directory path for sourcing other script files (if needed)
  # directory_path <- "TmbR_scripts"
  # files.sources <- list.files(directory_path, full.names = TRUE)
  # # Source each file
  # sapply(files.sources, function(file) source(file, chdir = TRUE))

  # # Read VCF file from the provided path
  # vcf_files <- list(UserVCF = vcf_file_path)

  # # Read VCF files
  # vcfs <- readVcfFiles(vcfFiles = vcf_files, assembly = genome_assembly)

  # # Read the gene panel design file from the provided path
  # design_file_path <- gene_panel_file

  # # Read bed file with the panel sequencing design
  # design <- readDesign(
  #   filename = design_file_path, 
  #   assembly = genome_assembly,
  #   ids = "entrezgene_id"
  # )
  # print("beforeapplyFilters")
  # # Apply filters using the provided VAF cutoff to remove synonymous variants and keep others
  # vcfs_NoSynonymous <- applyFilters(vcfs = vcfs, 
  #                                   assembly = genome_assembly, 
  #                                   design = design,
  #                                   vaf.cutoff = vaf_cutoff,  # Use user-provided VAF cutoff
  #                                   variantType = c("synonymous"))
  # print("applyFilters")
  # # Calculate Tumor Mutation Burden (TMB)
  # TMB_res <- applyTMB(inputForTMB = vcfs_NoSynonymous, 
  #                     assembly = genome_assembly)

  TMB_res <- read.csv(str_replace(vcf_file_path,".vcf","_TMB.csv"))
    output$tmbscoretable <- renderDT({
      datatable(
        TMB_res,  # Reactive data
        escape = FALSE,  # Allows HTML in the "View" button column
        options = list(
          pageLength = 10,  # Number of rows per page
          autoWidth = TRUE  # Adjust column width automatically
        ),
        selection = "none"  # Disables row selection
      )
    }, server = FALSE)
  # Print TMB results
  # print(TMB_res)

  # Save TMB results as a CSV file
  # output_file <- paste0("TMB_results_", basename(vcf_file_path), "_", genome_assembly, ".csv")
  # write.csv(TMB_res, output_file, row.names = FALSE)
  # cat("TMB results saved to:", output_file, "\n")
      } else {
        showNotification("Job ID not found", type = "error")
      }
    } else {
      output$display_patient_id <- renderText({ "" })
    }
  })


  # observe({
  #   # Start the Python server
  #   system("python -m http.server 1235 &", wait = FALSE)
    
  #   # Inform the user that the server is running
  #   output$server_status <- renderText("Python HTTP server started on port 1235.")
  # })
  
  # # Stop the Python server when the Shiny session ends
  # session$onSessionEnded(function() {
  #   # Kill the Python process running on port 1235
  #   # This assumes that the python process is the only one running on port 1235
  #   system("fuser -k 1235/tcp", wait = FALSE)
  # })
#----------------------------------------------------------------------------------------------------------
  
}


options(shiny.port = 1235,shiny.fullstacktrace = F,  shiny.host = "0.0.0.0")

shinyApp(ui, server)

# shiny::runApp(host = "0.0.0.0",   port = 1234 )
