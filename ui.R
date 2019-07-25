library(shinyWidgets)
library(nomensland)
dico <- get_dictionnaire_listes()
lr <- dico$nom_abrege
th <- dico$thematique %>% unique
names(lr) <- dico$nom_liste

lr <- list()
# i <- "Recours Exceptionnel"
for (i in th){
  te <- dico %>% filter(thematique == i)
  
  temp <- list(setNames(te$nom_abrege, te$nom_liste))
  names(temp) <- i
  lr <- append(lr, temp)
}

# listes_api <- referime::get_table('listes_api')
# tab_vide <- get_liste('chip') %>% 
#   as_tibble() %>%
#   filter(row_number() == 0)


dashboardPagePlus(
  


  dashboardHeaderPlus(title= span(tagList(icon("frog"), "Find my NASql")), #titleWidth = 350,   
                      enable_rightsidebar = TRUE,
                      rightSidebarIcon = "gears",
                      left_menu = tagList(
                        
                        
                          
                        dropdownBlock(
                          id = "mydropdown2",
                          title = "Archives",
                          icon = "file-archive",

                          badgeStatus = "warning",
                          #hr(),
                          shinyWidgets::actionBttn(
                            inputId = "button_a1",
                            label = "Dézipper",
                            style = "simple", 
                            color = "success", size = "s"
                            
                          ),
                          #hr(),
                          shinyWidgets::actionBttn(
                            inputId = "button_a0",
                            label = "Clean-up",
                            style = "simple", 
                            color = "warning", size = "s"
                          )),
                        dropdownBlock(
                          id = "mydropdown",
                          title = "Imports",
                          icon = "database",
                          
                          badgeStatus = "primary",
                          hr(),
                          shinyWidgets::actionBttn(
                            inputId = "button_i",
                            label = "Importer les données",
                            style = "simple", 
                            color = "success", size = "s"
                          ), 
                          # prettyCheckboxGroup(
                          #   inputId = "checkgroup2",
                          #   label = "En plus : ",
                          #   thick = TRUE,
                          #   choices = c("+ Valorisation des rsa", "dmip"),
                          #   animation = "rotate",
                          #   status = "info"
                          # ),
                          hr(),
                          awesomeRadio(
                            inputId = "checkgroup3",
                            label = "Calcul des valorisations", 
                            choices = c("Non", "Recette BEE", "Recette BEE + suppléments"),
                            selected = "Non", checkbox = TRUE
                          )
 
                        ),
                        dropdownBlock(
                          id = "help",
                          badgeStatus = "danger",
                          title = "Aide",
                          icon = "question-circle",
                          hr(),
                          tags$li('1.', icon('gears'), ' indiquer les paramètres à droite'),
                          tags$li('2.', icon('file-archive'), 'dézipper vos données'),
                          tags$li('3.', icon('database'), 'importer vos données'),
                          tags$li("4. Vous pouvez utiliser l'app", icon('frog'), ' et :'),
                          tags$li("a1. Requêter les données" , icon('filter')),
                          tags$li("a2. Exporter et importer vos listes de requêtes ", icon('gears'), '>', icon('upload')),
                          tags$li("b. Étudier les parcours" , icon('procedures')),
                          tags$li("c. Calculer la valorisation" , icon('dollar')),
                          tags$li("d. Exporter les données " , icon('gears'), '>', icon('file-export'))
                        ),
                        dropdownBlock(
                          id = "nomen",
                          title = "Nomenclatures",
                          icon = "sitemap",
                          badgeStatus = NULL,
                          appButton(
                            url = "https://guillaumepressiat.shinyapps.io/oncle_cim/",
                            label = "CIM",
                            icon = "fa fa-diagnoses",
                            enable_badge = FALSE,
                            badgeColor = NULL,
                            badgeLabel = NULL
                          ),
                          appButton(
                            url = "https://guillaumepressiat.shinyapps.io/oncle_ccam/",
                            label = "CCAM",
                            icon = "fa fa-stethoscope",
                            enable_badge = FALSE,
                            badgeColor = NULL,
                            badgeLabel = NULL
                          ),
                          appButton(
                            url = "https://guillaumepressiat.shinyapps.io/oncle_ghm/",
                            label = "GHM",
                            icon = "fa fa-align-justify",
                            enable_badge = FALSE,
                            badgeColor = NULL,
                            badgeLabel = NULL
                          ),
                          appButton(
                            url = "https://guillaumepressiat.shinyapps.io/transcodeur/",
                            label = "Transcodeur",
                            icon = "fa fa-code",
                            enable_badge = FALSE,
                            badgeColor = NULL,
                            badgeLabel = NULL
                          ),
                          appButton(
                            url = "https://guillaumepressiat.shinyapps.io/nomensweb/",
                            label = "Toutes Nomenclatures",
                            icon = "fa fa-table",
                            enable_badge = FALSE,
                            badgeColor = NULL,
                            badgeLabel = NULL
                          )))),

  
  sidebar = dashboardSidebar(

    #width = 350,
    sidebarMenu(
      id="menuchoice",
      menuItem(icon = icon("clipboard-check"), text  ="Rsa explore",  tabName="app"),
      menuItem(icon = icon("filter"), text  ="Requêtes", tabName="app2"),
      #menuItem(icon = icon("mortar-pestle"), text  ="Requête saisie", tabName="app3"),
      menuItem(icon = icon("dollar-sign"), text  = "Valorisation", tabName="app4"),
      menuItem(icon = icon("procedures"), text  = "Parcours", tabName="app5"))),
  #menuItem(icon = icon("sitemap"), text  = "Nomenclatures", tabName="app6")    
  #menuItem(icon = icon("leaf"), text  = "map", tabName="app6"))),
  
  
  
  #selectInput("requ", "Requête", choices = as.list(lr), selected = "bpco_exacerbee"),
  rightsidebar = rightSidebar(
    rightSidebarTabContent(
      id = 1,
      title = "Paramètres de l'import",
      icon = "file-import",
      active = TRUE,

                              selectInput("annee", "Selection de l'année", choices = setNames(2016:2019, 2016:2019), selected = 2018),
                              selectInput("mois", "Mois de remontée", choices = setNames(1:12, 1:12), selected = 12),
                              
                              selectInput("finess", "Finess", choices = setNames(c("750712184", 
                                                                                   "920100013"), c("750712184", 
                                                                                                   "920100013")), selected = 1),
                              #selectInput("finess", "Finess", choices = list("750712184"), selected = 1),
                              textInput("path", "path", '~/Documents/data/mco'),
                              
                              shinyWidgets::prettySwitch(
                                inputId = "monet",
                                label = "db ? (non actif)"),
                              
                              shinyWidgets::knobInput(
                                inputId = "max_rows",
                                label = "Nb RSA à importer (n_max)",
                                value = 1e3,
                                min = 1,
                                max = 1.5e6,
                                displayPrevious = TRUE, 
                                lineCap = "default",
                                fgColor = "#3c8dbc",
                                inputColor = "#3c8dbc"
                              )),
    

    rightSidebarTabContent(
      id = 2,
      icon = "upload",
      h4('Import de requêtes'),
      fileInput("filejson", "Choisir requête.json",
                multiple = FALSE,
                accept = c("application/json"))
    ),
    rightSidebarTabContent(
      id = 3,
      title = NULL,
      useShinyjs(),
      icon = "file-export",
      accordion(
        accordionItem(
          id = 10,
          title = 'Requêtes',
          collapsed = FALSE,
          
          h4('Export des rsa requêtés'),
          h5('Via librairie de requêtes'),
          textInput("name_down1", label = NULL, "mes_rsa_requetes"),
          downloadButton('download11',"xls", class = "dowbut"),
          downloadButton('download12',"json", class = "dowbut"),
          downloadButton('download13',"csv", class = "dowbut"),
          h5('Via requête saisies'),
          textInput("name_down2", label = NULL, "mes_rsa_requetes_manuel"),
          downloadButton('download21',"xls", class = "dowbut"),
          downloadButton('download22',"json", class = "dowbut"),
          downloadButton('download23',"csv", class = "dowbut"),
          h5('Via requêtes importées'),
          textInput("name_down5", label = NULL, "mes_rsa_requetes_importee"),
          downloadButton('download51',"xls", class = "dowbut"),
          downloadButton('download52',"json", class = "dowbut"),
          downloadButton('download53',"csv", class = "dowbut"),
          h4('Export des requêtes saisies'),
          textInput("name_down3", label = NULL, "mes_requetes"),
          downloadButton('download31',"xls", class = "dowbut"),
          downloadButton('download32',"json", class = "dowbut"),
          downloadButton('download33',"csv", class = "dowbut")),
        accordionItem(
          id = 11,
          title = 'rsa',
          collapsed = TRUE,
          p('contient les tables rsa, actes, diags, rsa_um, et valo'),
          textInput("name_down4", label = NULL, "mes_rsa"),
          downloadButton('download41',"xls", class = "dowbut"),
          downloadButton('download42',"json", class = "dowbut"),
          downloadButton('download43',"csvzip", class = "dowbut"),
          h6('*attention, les exports xls fonctionnent lorsque nrow < 65536...')
        )))),
  
  dashboardBody(
    shinybusy::use_busy_spinner(spin = "fading-circle"),
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header   .logo {
                                background-color: #383838;
                                }

                                .box-header  {
                                background-color: #383838;
                                }
                                
                                .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
                                  width: 350px;
                                  }
                              
                                .skin-blue .box-header {
                                background-color: #383838;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #383838;
                                }
                                
                                .dowbut{font-size: 10px ;}
                                
                                .datatables {
                                  margin: 2em 0em;
                                }
                                
                                .dataTables_length,
                                .dataTables_filter,
                                .dataTables_info,
                                .dataTables_paginate {
                                  font-size: 90%;
                                }
                                
                                table{
                                    border-collapse:collapse;
                                    border-spacing:0;
                                    empty-cells:show;
                                    margin:0;
                                    border-bottom:1px solid #333399;
                                }
                                
                                td{
                                    vertical-align:top}
                                
                                table td,table th{
                                    font-size:90%;
                                    margin:0;
                                    overflow:visible;
                                    padding:8px 16px;
                                    background-color:white;
                                    border:1px solid #e1e4e5;
                                }
                                
                                table thead th{
                                    font-weight:bold;
                                    border-top:3px solid #333399;
                                    border-bottom:1px solid #333399;
                                }
                                
                                table caption{
                                    color:#000;
                                    font:italic 85%/1 arial,sans-serif;
                                    padding:1em 0;
                                }
                                
                                table tr:nth-child(2n-1) td{
                                    background-color:#f3f6f6;
                                }
                                
                                table tr:nth-child(2n) td{
                                    background-color:white;
                                }
                                
                                .dataTables_wrapper {
                                  overflow-x: auto;
                                }
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #383838;
                                }
                                .dataTables tbody tr {
                                max-height: 35px; /* or whatever height you need to make them all consistent */
                                }
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #707070;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #383838;
                                color: white;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                color: #cba08c;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #a0522d;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background: linear-gradient(to top left, #3c8dbc 0%, #a0522d 100%);
                                }

                                '))), #--!important;
    fluidRow(
      tabItems(
        tabItem(tabName = "app", 
                shinydashboard::box(h5(textOutput("nb_rsa")),
                plotlyOutput('p1'), 
                width = 12, solidHeader = TRUE, collapsible = TRUE, 
                collapsed = FALSE, title = "Description des rsa", status = "primary"),
        
                shinydashboard::box(shinyWidgets::addSpinner(dataTableOutput("rsa")), 
                                    width = 12, 
                                    solidHeader = TRUE, collapsible = TRUE, 
                                    collapsed = TRUE, title = "Mes rsa", status = "primary")),
        
        
        tabItem(tabName = "app2", 
                shinydashboard::box(
                accordion(
                  accordionItem(
                    id = 1,
                    collapsed = FALSE,
                    title = 'Librairie de requêtes',
                  # actionButton("button_i", "Importer les rsa !"),
                  selectizeInput("requ", "Requête", choices = as.list(lr), selected = "chip", multiple = TRUE),
                  # shinyWidgets::materialSwitch(label = "Requêtage actif", inputId = "lance_r", value = FALSE, right = TRUE)),
                  shinyWidgets::actionBttn(
                    inputId = "lance_r",
                    label = "Lancer la requête",
                    style = "float", 
                    color = "success", size = "xs"
                  ),
                  h5(textOutput("nb_requ")),
                  plotlyOutput('p2'),
                  #shinyWidgets::addSpinner(dataTableOutput("rsa_requ"))),
                  dataTableOutput("rsa_requ")),
                    accordionItem(
                      id = 2,
                      title = "Requêtes via saisie",
                      textInput('mydata', label = 'Nom de la table', value = 'listes_api'),
                      # h5("Saisie / édition d'une requête"),
                      editableDTUI("table1"),
                      hr(),
                      shinyWidgets::actionBttn(
                        inputId = "lance_r2",
                        label = "Lancer la requête",
                        style = "float", 
                        color = "success", size = "xs"),
                      # verbatimTextOutput("rsa_requ_main"),
                      dataTableOutput("rsa_requ_main")),
                    accordionItem(
                      id = 3,
                      title = "Requêtes importées",
                      h6("L'import de requêtes se fait via un fichier json dans les menus à droite"),
                      dataTableOutput('lib_requ_imports'),
                      shinyWidgets::actionBttn(
                        inputId = "lance_r3",
                        label = "Lancer la requête",
                        style = "float", 
                        color = "success", size = "xs"),
                      # verbatimTextOutput("rsa_requ_main"),
                      dataTableOutput("rsa_requ_import"))), width = 12, solidHeader = TRUE, collapsible = TRUE,
                collapsed = FALSE, title = 'Requêtes des rsa', status = "primary")),
        tabItem(tabName = "app4",
                shinydashboard::box(dataTableOutput("sv"),
                                                                    width = 12, solidHeader = TRUE, collapsible = TRUE,
                                                                    collapsed = FALSE, title = 'epmsi SV', status = "primary"),#,
                
                shinydashboard::box(dataTableOutput("rav"),
                                    width = 12, solidHeader = TRUE, collapsible = TRUE,
                                    collapsed = FALSE, title = 'epmsi RAV', status = "primary"),

                shinydashboard::box(shinyWidgets::addSpinner(dataTableOutput("rsa_valo")),
                                    width = 12, solidHeader = TRUE, collapsible = TRUE,
                                    collapsed = TRUE, title = 'Rsa et leur valorisation', status = "primary")),
        tabItem(tabName = "app5",
        shinydashboard::box(
          radioButtons('parc_cible', label = "Données du parcours", 
                        choices = c("RSA", 'via librairie de requêtes', 'via requête saisie',
                                    'via requête importée'), inline = TRUE, choiceValues = NULL),
          sunburstR::sunburstOutput("parc"),
          dataTableOutput("parc_dat"),
                            #shinyWidgets::addSpinner(dataTableOutput("parc_req")),
                            #shinydashboard::box(sunburstR::sund2bOutput("parc_rsa_sun"),
                            width = 12, solidHeader = TRUE, collapsible = TRUE,
                            collapsed = FALSE, title = 'Parcours', 
                            status = "primary"))


              
    ))))


