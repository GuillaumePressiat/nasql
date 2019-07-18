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
                          
                          hr(),
                          shinyWidgets::actionBttn(
                            inputId = "button_a1",
                            label = "Dézipper",
                            style = "simple", 
                            color = "success", size = "s"
                          ),
                          hr(),
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
                          title = "Aide",
                          icon = "question-circle",
                          hr(),
                          p('1.', icon('gears'), ' indiquer les paramètres à droite'),
                          p('2.', icon('file-archive'), 'dézipper vos données'),
                          p('3.', icon('database'), 'importer vos données'),
                          p("4. Vous pouvez utiliser l'app", icon('frog'))))),
  
  sidebar = dashboardSidebar(
    #width = 350,
    sidebarMenu(
      id="menuchoice",
      menuItem(icon = icon("clipboard-check"), text  ="Rsa explore",  tabName="app"),
      menuItem(icon = icon("filter"), text  ="Librairie de requêtes", tabName="app2"),
      menuItem(icon = icon("mortar-pestle"), text  ="Requête saisie", tabName="app3"),
      menuItem(icon = icon("dollar-sign"), text  = "Valorisation", tabName="app4"),
      menuItem(icon = icon("procedures"), text  = "Parcours", tabName="app5"))),
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
      title = "Export de données",
      icon = "file-export",
      textInput("caption", "Caption", "Data Summary")
    )),
  dashboardBody(
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header   .logo {
                                background-color: #383838;
                                }

                                .box-header  {
                                background-color: #383838;
                                }
                                
                                .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
                                  width:350px;
                                  }
                              
                                .skin-blue .box-header {
                                background-color: #383838;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #383838;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #383838;
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

                                '))),
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
                  width = 12, solidHeader = TRUE, collapsible = TRUE, 
                  collapsed = FALSE, title = "Description de ces rsa requêtés", status = "primary"),
                shinydashboard::box(shinyWidgets::addSpinner(dataTableOutput("rsa_requ")), 
                                    width = 12, solidHeader = TRUE, collapsible = TRUE, 
                                    collapsed = FALSE, title = 'Rsa requêtés', status = "primary")),
        tabItem(tabName = "app3", 
                shinydashboard::box(
                  textInput('mydata', label = 'Nom de la table', value = 'listes_api'),
                  h5("Saisie / édition d'une requête"),
                  editableDTUI("table1"),
                  shinyWidgets::actionBttn(
                    inputId = "lance_r2",
                    label = "Lancer la requête",
                    style = "float", 
                    color = "success", size = "xs"),
                  # verbatimTextOutput("rsa_requ_main"),
                  dataTableOutput("rsa_requ_main"),
                  width = 12, solidHeader = TRUE, collapsible = TRUE, 
                  collapsed = FALSE, title = 'Saisir une requête ad hoc', status = "primary")    
        ),
        tabItem(tabName = "app4",
                shinydashboard::box(shinyWidgets::addSpinner(dataTableOutput("sv")),
                                                                    width = 12, solidHeader = TRUE, collapsible = TRUE,
                                                                    collapsed = FALSE, title = 'epmsi SV', status = "primary"),#,
                
                shinydashboard::box(shinyWidgets::addSpinner(dataTableOutput("rav")),
                                    width = 12, solidHeader = TRUE, collapsible = TRUE,
                                    collapsed = FALSE, title = 'epmsi RAV', status = "primary"),

                shinydashboard::box(shinyWidgets::addSpinner(dataTableOutput("rsa_valo")),
                                    width = 12, solidHeader = TRUE, collapsible = TRUE,
                                    collapsed = TRUE, title = 'Rsa et leur valorisation', status = "primary")),
        tabItem(tabName = "app5",

        shinydashboard::box(sunburstR::sunburstOutput("parc_rsa_sun"),
                            shinyWidgets::addSpinner(dataTableOutput("parc_rsa")),
        #shinydashboard::box(sunburstR::sund2bOutput("parc_rsa_sun"),
                            width = 12, solidHeader = TRUE, collapsible = TRUE,
                            collapsed = TRUE, title = 'Parcours global rsa', 
        status = "primary"),
        shinydashboard::box(sunburstR::sunburstOutput("parc_req_sun"),
                            shinyWidgets::addSpinner(dataTableOutput("parc_req")),
                            #shinydashboard::box(sunburstR::sund2bOutput("parc_rsa_sun"),
                            width = 12, solidHeader = TRUE, collapsible = TRUE,
                            collapsed = FALSE, title = 'Parcours rsa requêtés', 
                            status = "primary"))#,
        #tabItem(tabName = "app6")
        ))))


