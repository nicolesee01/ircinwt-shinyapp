library(shiny)
library(leaflet)
library(raster)
library(bslib)
library(exactextractr)
library(shinycssloaders)

source("helpers.R")

cifor = raster('/vsicurl/link1') #link1 refers to link for CIFOR wetlands dataset
ic = raster('/vsicurl/link2') #link2 refers to the link for Irrecoverable Carbon dataset

ui = fluidPage(
  theme=bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("Wetlands"),
  sidebarLayout(
    sidebarPanel(
        p("This web app displays maps of wetlands and other factors, such as irrecoverable carbon, in the area of interests."),
        fileInput("area", 
                  label="Upload shapefile (file types: .shp, .dbf, .prj, .shx)",
                  multiple = TRUE,
                  accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')
        )
      ),
    mainPanel(
      tabsetPanel(
        #tab 1: wetland in area
        tabPanel("Wetlands",
                 em("This page displays the area of wetlands in the area uploaded."),
                 br(),
                 br(),
                 p("The proportion of wetlands in an area is:"),
                 textOutput("wtprop"),
                 tags$head(tags$style(
                   "#wtprop {font-size: 18px;}")),
                 br(),
                 p("Map of wetlands in area input"),
                 withSpinner(leafletOutput("ciforinareamap"), type = 1)
                 ),
        
        #tab 2: irrecoverable carbon in area
        tabPanel("Irrecoverable Carbon",
                 em("This page compares the total irrecoverable carbon present in the area uploaded and the irrecoverable carbon present in wetlands in the area uploaded."),
                 br(),
                 br(),
                 
                 h3(strong("Total Irrecoverable Carbon in area")),
                 p("The amount of irrecoverable carbon is:"),
                 textOutput("icamt"),
                 tags$head(tags$style(
                   "#icamt {font-size: 16px;}")),
                 br(),
                 p("Map of irrecoverable carbon in area"),
                 withSpinner(leafletOutput("icinareamap"), type = 1),
                 
                 h3(strong("Irrecoverable Carbon in wetlands")),
                 p("The proportion of the area of irrecoverable carbon in wetlands is:"),
                 textOutput("areaicinwt_prop"),
                 tags$head(tags$style(
                   "#areaicinwt_prop {font-size: 16px;}")),
                 br(),
                 p("The proportion of the amount of irrecoverable carbon in wetlands is:"),
                 textOutput("amticinwt_prop"),
                 tags$head(tags$style(
                   "#amticinwt_prop {font-size: 16px;}")),
                 br(),
                 p("Map of irrecoverable carbon in wetlands in area"),
                 withSpinner(leafletOutput("icinwtinarea"), type = 1)
                 ),
        
        #tab 3: biodiversity
        tabPanel("Biodiversity"
        ),
        
        #tab 4: FLII
        tabPanel("Forest Landscape Integrity Index"
                 )
      )
    )
  ),
  h4(strong("Sources")),
  p(a("CIFOR Global Wetlands", href = "https://www2.cifor.org/global-wetlands/")),
  p("Gumbricht, T.; Román-Cuesta, R.M.; Verchot, L.V.; Herold, M.; Wittmann, F; Householder, E.; Herold, N.; Murdiyarso, D., 2017, \"Tropical and Subtropical Wetlands Distribution\", https://doi.org/10.17528/CIFOR/DATA.00058, Center for International Forestry Research (CIFOR), V7, UNF:6:Bc9aFtBpam27aFOCMgW71Q== [fileUNF]"),
  p(a("Irrecoverable Carbon", href = "https://www.conservation.org/projects/irrecoverable-carbon")),
  p("Noon, M.L., Goldstein, A., Ledezma, J.C. et al. Mapping the irrecoverable carbon in Earth’s ecosystems. Nat Sustain (2021). https://doi.org/10.1038/s41893-021-00803-6"),
  p(a("Forest Landscape Integrity Index", href = "https://www.forestintegrity.com/")),
  p("Grantham, H.S., Duncan, A., Evans, T.D. et al. Anthropogenic modification of forests means only 40% of remaining forests have high ecosystem integrity. Nat Commun 11, 5978 (2020). https://doi.org/10.1038/s41467-020-19493-3")
)

server = function(input, output) {
  map = reactive({
    req(input$area)
    df = input$area
    temp = dirname(df$datapath[1])
    for (i in 1:nrow(df)) {
      file.rename(
        df$datapath[i],
        paste0(temp, "/", df$name[i])
      )
    }
    map = shapefile(paste(temp, 
                          df$name[grep(pattern = "*.shp$", df$name)],  
                          sep = "/")
                    )
    map
  })
  
  #function to generate map of CIFOR in area
  output$ciforinareamap = renderLeaflet({
    if (is.null(map())) {
      return(NULL)
    }
    map = map()
    
    ciforinarea = mask(crop(cifor, extent(map)), map)
    ciforinarea[ciforinarea==0 | ciforinarea==20] = NA
    num = length(unique(values(ciforinarea)))
    pal = colorFactor(rainbow(num), values(ciforinarea), na.color="transparent")
    
    m(map) %>% 
      addRasterImage(ciforinarea, colors = pal, group = "Wetlands") %>% 
      addLegend(pal=pal, 
                values = values(ciforinarea), 
                title = "Category",
                labFormat = labelFormat(
                  transform = function(x) {
                    x = c("Open Waters", "Swamps", "Fens", "Riverine and lacustrine", 
                          "Floodouts", "Floodplains", "General Marshes", 
                          "Marshes in arid climate", "Marshes wet meadows") #input area may not have all these 9 categories
                  }
                )) %>% 
      addLayersControl(
        baseGroups = c("OSM", "Carto", "ESRI World Physical"),
        overlayGroups = c("Area", "Wetlands"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE))
  })
  
  #function to generate proportion of wetlands in area
  output$wtprop = renderText({
    if (is.null(map())) {
      return(NULL)
    }
    map = map()
    
    generate_wtprop(cifor, map)
  })
  
  #function to generate amount of irrecoverable carbon in area
  output$icamt = renderText({
    if (is.null(map())) {
      return(NULL)
    }
    map = map()
    
    print(paste0(generate_icamt(ic, map), "million tons"))
  })
  
  #function to generate map of irrecoverable carbon in area
  output$icinareamap = renderLeaflet({
    if (is.null(map())) {
      return(NULL)
    }
    map = map()
    
    icinarea = mask(crop(ic, extent(map)), map)
    icinarea[icinarea==0] = NA
    icinarea = aggregate(icinarea, 8) #reduce space
    pal1 = colorNumeric(palette = "YlOrRd", 
                        domain = values(icinarea),
                        na.color = "transparent")
    
    m(map) %>% 
      addRasterImage(icinarea, colors = pal1, 
                     group = "Irrecoverable Carbon") %>% 
      addLegend(pal=pal1, 
                values = values(icinarea), 
                title = "Irrecoverable Carbon (t/ha)") %>% 
      addLayersControl(
        baseGroups = c("OSM", "Carto", "ESRI World Physical"),
        overlayGroups = c("Area", "Irrecoverable Carbon"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE))
  })
  
  #function to generate area proportion of irrecoverable carbon that are found in wetlands in area
  output$areaicinwt_prop = renderText({
    if (is.null(map())) {
      return(NULL)
    }
    map = map()
    
    ciforinarea = mask(crop(cifor, extent(map)), map)
    icinarea = mask(crop(ic, extent(map)), map)
    icinarea[icinarea==0] = NA
    ciforinarea[ciforinarea==0 | ciforinarea==20] = NA
    icinarea = resample(icinarea, ciforinarea, method="ngb")
    icinwt = overlay(icinarea, ciforinarea, fun=function(icinarea, ciforinarea){return(icinarea - ciforinarea/ciforinarea + 1)})
    
    p = sum(!is.na(values(icinwt))) #number of cells where there are irrecoverable carbon in wetlands
    q = sum(!is.na(values(icinarea))) #number of cells where there are irrecoverable carbon
    val = round((p/q*100), 2)
    print(paste0(val, "%"))
  })
  
  #function to generate amount proportion of irrecoverable carbon that are found in wetlands in area
  output$amticinwt_prop = renderText({
    if (is.null(map())) {
      return(NULL)
    }
    map = map()
    
    ciforinarea = mask(crop(cifor, extent(map)), map)
    icinarea = mask(crop(ic, extent(map)), map)
    icinarea[icinarea==0] = NA
    ciforinarea[ciforinarea==0 | ciforinarea==20] = NA
    icinarea = resample(icinarea, ciforinarea, method="ngb")
    icinwt = overlay(icinarea, ciforinarea, fun=function(icinarea, ciforinarea){return(icinarea - ciforinarea/ciforinarea + 1)})
    
    x = sum(values(icinarea), na.rm=TRUE) #total sum of irrecoverable carbon in area
    y = sum(values(icinwt), na.rm=TRUE) #total sum of irrecoverable carbon in wetlands in area
    val = round((y/x*100), 2)
    print(paste0(val, "%"))
  })

  #function to generate map where irrecoverable carbon and wetlands overlap
  output$icinwtinarea = renderLeaflet({
    if (is.null(map())) {
      return(NULL)
    }
    map = map()
    
    icinwt = generate_icinwt(cifor, ic, map)
    icinwt = aggregate(icinwt, 8) #reduce space
    pal1 = colorNumeric(palette = "YlOrRd", 
                        domain = values(icinwt),
                        na.color = "transparent")
    
    m(map) %>% 
      addRasterImage(icinwt, colors = pal1, 
                     group = "Irrecoverable Carbon") %>% 
      addLegend(pal=pal1, 
                values = values(icinwt), 
                title = "Irrecoverable Carbon (t/ha)") %>% 
      addLayersControl(
        baseGroups = c("OSM", "Carto", "ESRI World Physical"),
        overlayGroups = c("Area", "Irrecoverable Carbon"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE))
  })
}

shinyApp(ui=ui, server=server)



