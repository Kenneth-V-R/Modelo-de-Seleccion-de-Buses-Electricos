library(shiny)
library(shinydashboard)
library(lpSolve)
library(lpSolveAPI)
library(dplyr)
library(glue)
library(readr)
library(shinythemes)
library(sysfonts)
library(rhandsontable)
library(DataEditR)
library(ggplot2)
library(shinyWidgets)


#Datos
df <- read_csv("Datos_proyecto_final.csv")
df_ISP <- read_csv("Datos_Irradiacion_solar.csv")
ui <- navbarPage(                                                                #Panel de tabs.
  title = "Modelo de selección de buses eléctricos",                             #Título.
                 theme = shinytheme("united"),                                   #Tema.
  tabPanel( "Inicio",                                                            #Titulo del tab #1.
    
    # Boxes need to be put in a row (or column)
  fluidRow(
   box(                                                                          #Caja.
    title = "Parámetros",                                           #Título caja.
    
    helpText("Inserte aquí las características de la ruta en la que deseas implementar buses eléctricos:",color="black"),        #Título caja.
    
    numericInput                                                                 #Parámetro #1.
    (inputId= "Distancia_ruta", label = "Distancia de la ruta(Km):",0),
    numericInput                                                                 #Parámetro #1.
    (inputId= "Tiempo_trayecto", label = "Tiempo de recorrido(min):",0),
    numericInput                                                                 #Parámetro #2.
    (inputId= "Pasajeros_por_bus", label = "Pasajeros/bus:",0),
    
    checkboxInput                                                                 #Parámetro #1.
    (inputId= "Lunes_Viernes", label = "¿Opera de lunes a viernes?",FALSE),
    numericInput
    (inputId= "Tiempo_operativo_LV", label = "Tiempo operativo diario Lunes a Viernes(horas):",0),
    checkboxInput                                                                 #Parámetro #1.
    (inputId= "Sabado_Domingo", label = "¿Opera de sábado a domingo?",FALSE),
    numericInput
    (inputId= "Tiempo_operativo_SD", label = "Tiempo operativo diario Sábado y Domingo(horas):",0),
    
    width=3),
   box(
     title=h3("Title  ", style = 'font-size:42px;color:white;'),
     
     numericInput                                                                 #Parámetro #1.
     (inputId= "Tiempo_garaje", label = "Tiempo de transporte al garaje(minutos):",0),
     numericInput                                                                 #Parámetro #4.
     (inputId= "Costo_kWh",label = "Costo Kilowatt por hora($/kWh): ",0),
     checkboxInput                                                                 #Parámetro #1.
     (inputId= "Hay_panel", label = "¿Planeas usar paneles solares?",FALSE),
     numericInput                                                                 #Parámetro #4.
     (inputId= "Potencia_panel",label = "Potencia de panel solar(W): ",0),
     numericInput                                                                 #Parámetro #4.
     (inputId= "Num_buses",label = "Número de buses: ",0),
     actionButton("Guardar","Guardar"), 
     htmlOutput("Param")
     ,width=3
  ),
   box(  
     title = "Resultados e información del bus seleccionado", htmlOutput("Resultado")),
   box(title="Número de paneles solares requeridos para alimentar la flota de buses eléctricos", htmlOutput("Paneles_solares"))
    
   ) ),
  tabPanel("Modelo Matemático",box(title = "Función objetivo", htmlOutput("EQ")),
           box(title = "Restricciones", htmlOutput("Res")),
           box(title = "Glosario", htmlOutput("GL"))
           )
  , tabPanel("Cargar más datos de buses", 
      box(title = "Cargar Datos de Buses", htmlOutput("Cargar_Datos"),
      fileInput('file1', 'Escoje un archivo CSV',accept=c('text/csv', 
            'text/comma-separated-values,text/plain', '.csv')),tags$hr()
      
        ,actionButton("add", "Agregar datos"),
      actionButton("replace", "Reemplazar datos"),width = 3),
  box(title = "Datos Cargados", htmlOutput("Datos_Cargados"))
  )
  ,tabPanel("Cargar más datos de Irradiación", 
            box(title = "Cargar Datos de Irradiación", htmlOutput("Cargar_Datos_IRR"),
                fileInput('file2', 'Escoje un archivo CSV',accept=c('text/csv', 
                                                                    'text/comma-separated-values,text/plain', '.csv')),tags$hr()
                
                ,
                actionButton("replace2", "Reemplazar datos"),width = 3),
            box(title = "Datos Cargados", htmlOutput("Datos_Cargados2"))
            
            
            )
)



server <- function(input, output,session) {
  
  output$Datos_Cargados <- renderUI({
    
    uploaded_file=input$file1
    if(is.null(uploaded_file)){
      return()
    }
    
    df_new=read_csv(uploaded_file$datapath)
    df_new$Imagen[is.na(df_new$Imagen)] <- "No hay imagen en base de datos"
    df_new$"Página acceso"[is.na(df_new$"Página acceso")] <- "No hay link en base de datos"
    
    renderTable(df_new)
    
  })
  output$Datos_Cargados2 <- renderUI({
    
    uploaded_file=input$file2
    if(is.null(uploaded_file)){
      return()
    }
    
    df_new_ISP=read_csv(uploaded_file$datapath)

    
    renderTable(df_new_ISP)
    
  })
  
  r_df_ISP=reactiveValues(data=df_ISP)
  Error=reactiveValues(data=FALSE)
  r_df=reactiveValues(data=df)
  
  observeEvent(input$replace2, {
    uploaded_file=input$file1
    if(is.null(uploaded_file)){
    }
    else{
      df_new_ISP=read_csv(uploaded_file$datapath)
      r_df_ISP$data <- df_new_ISP
    }
  })
  
  observeEvent(input$replace, {
    uploaded_file=input$file1
    if(is.null(uploaded_file)){
    }
    else{
      df_new=read_csv(uploaded_file$datapath)
      df_new$Imagen[is.na(df_new$Imagen)] <- "No hay imagen en base de datos"
      df_new$"Página acceso"[is.na(df_new$"Página acceso")] <- "No hay link en base de datos"
      r_df$data <- df_new
    }
  })
  
  observeEvent(input$add, {
    uploaded_file=input$file1
    if(is.null(uploaded_file)){
      return(df)
    }
    else{
      df_new=read_csv(uploaded_file$datapath)
      df_new$Imagen[is.na(df_new$Imagen)] <- "No hay imagen en base de datos"
      df_new$"Página acceso"[is.na(df_new$"Página acceso")] <- "No hay link en base de datos"
      df_new$Index=df_new$Index+50
      r_df$data <- rbind(df,df_new)
    }
  }) 
  
  
  
  site <- reactive({tryCatch(expr = {
    
    d = as.numeric(input$Distancia_ruta)
    Tt = as.numeric(input$Tiempo_trayecto)
    if (input$Lunes_Viernes==TRUE){
      To_LV = as.numeric(input$Tiempo_operativo_LV)
    }
    else{
      To_LV=0
    }
    if (input$Sabado_Domingo==TRUE){
      To_SD = as.numeric(input$Tiempo_operativo_SD)
    }
    else{
      To_SD=0
    }
    TG = as.numeric(input$Tiempo_garaje)
    To=max(To_LV,To_SD)
    
    P = as.numeric(input$Pasajeros_por_bus)
    Ce= as.numeric(input$Costo_kWh)
    Num_buses=as.numeric(input$Num_buses)

    
    
    D = d*(To/Tt)
    Td_LV = 24-To_LV
    Td_SD = 24-To_SD
    Td=min(Td_LV,Td_SD)+(TG/60)
    
    
    vect_constrains=c(r_df$data$`Autonomía(Km)[A]`,r_df$data$`Capacidad de pasajeros[P]`,r_df$data$`Tiempo carga(Horas)[t]`,rep(1,each=nrow(r_df$data)))
    
    f.obj <- r_df$data$`Consumo energético(kWh/Km)[E]`
    f.con <- matrix(vect_constrains,4, byrow=TRUE)
    f.dir <- c(">=",">=","<=","=")
    f.rhs <- c(D,P,Td,1)
    
    model <- lp ("min", f.obj, f.con, f.dir, f.rhs,all.bin = TRUE)
    model
    model$solution
    posits <- which(model$solution == 1, arr.ind=TRUE)           #posicion del modelo seleccionado
    df2<-subset(r_df$data,r_df$data$Index==posits)                              #info del modelo seleccionado
    
    
    Costo_combustible=df2$`Consumo energético(kWh/Km)[E]`*D*Ce
    Consumo_electrico_diario=df2$`Consumo energético(kWh/Km)[E]`*D
    D_LV = d*(To_LV/Tt)
    D_SD = d*(To_SD/Tt)
    DT=(D_LV*5)+(D_SD*2)
    CE_Mes=(DT*df2$`Consumo energético(kWh/Km)[E]`*4)+(D_LV*2)
    Error$data=FALSE
    return(list(df2=df2,Consumo_electrico_diario=Consumo_electrico_diario,
                Costo_combustible=Costo_combustible,
                CE_Mes=CE_Mes,Costo_combustible=Costo_combustible,
                Num_buses=Num_buses,df=df))
  },error=function(e){Error$data=TRUE}
    
    
    )
  })

  
  Display_resultados<- eventReactive(input$Guardar,{
    
    df2=site()$df2
    Num_buses=site()$Num_buses
    Consumo_electrico_diario=site()$Consumo_electrico_diario
    Costo_combustible=site()$Costo_combustible*Num_buses
    CE_Mes=site()$CE_Mes*Num_buses
    Costo_combustible=site()$Costo_combustible*Num_buses
    no_img_text=""
    if (df2$Imagen=="No hay imagen en base de datos"){
    
      no_img_text <- "No hay imagen en base de datos"
    }
    html_img_text=""
    if (df2$Imagen!="No hay imagen en base de datos"){
      
      html_img_text <- paste('<img src="',df2$Imagen,'" style="height:200px;">',sep="")
    }
    print_text=HTML(paste("\u25CF El bus óptimo, minimizando el consumo eléctrico es el modelo ", df2$`Modelo[i]`,
               " del fabricante ",df2$Fabricante,
               " y tiene un consumo de electricidad de: ",
               round(df2$`Consumo energético(kWh/Km)[E]`,3), " kWh/Km.<br/>",
               "\u25CF El consumo eléctrico diario es: ",round(Consumo_electrico_diario,2),"kWh.<br/>",
               "\u25CF El costo de electricidad diario es: ",round(Costo_combustible,0),"$.<br/>"),
         "\u25CF El consumo eléctrico mensual es de: ",round(CE_Mes,2),"kWh.<br/>",
         "\u25CF El costo de electricidad mensual es: ",round(Costo_combustible*30,0),"$.<br/>",
          "\u25CF Página del fabricante: ",df2$`Página acceso`,"<br/>",
         "\u25CF Imagen: ",no_img_text,"<br/>",html_img_text
         
         , sep = "")
    
    return(print_text)
  })
    
  output$Resultado <- renderText({tryCatch(expr={
    Display_resultados()},error={function(e){""}}
  )
  })
  

  Display_tabla<- eventReactive(input$Guardar,{
    df2=site()$df2
    Num_buses=site()$Num_buses
    Costo_combustible=site()$Costo_combustible*Num_buses
    CE_Mes=site()$CE_Mes*Num_buses
    Costo_combustible=site()$Costo_combustible*Num_buses
    Potencia_panel=as.numeric(input$Potencia_panel)
    
    Potencia_panel=Potencia_panel/1000
    r_df_ISP$data$"Numero de paneles minimo"<-ceiling(as.numeric((CE_Mes/(r_df_ISP$data$ISP_Max*30)))/(Potencia_panel*0.8))
    r_df_ISP$data$"Numero de paneles max"<-ceiling(as.numeric((CE_Mes/(r_df_ISP$data$ISP_Min*30)))/(Potencia_panel*0.8))
    if(input$Hay_panel==TRUE){
      print_table=r_df_ISP$data[c(2,5,6)]
    }
    else{
      print_table <- "Has seleccionado no usar paneles solares"
    }
    return(print_table)
  })
  
  
  output$Paneles_solares <- renderTable({if (Error$data!=TRUE){
    
    Display_tabla()}
    else{
      "Error: Intenta no dejar campos obligatorios(*) en blanco"
    }

    
  })
  Display_plot<- eventReactive(input$Guardar,{
    df2=site()$df2
    Num_buses=site()$Num_buses
    Costo_combustible=site()$Costo_combustible*Num_buses
    CE_Mes=site()$CE_Mes*Num_buses
    Costo_combustible=site()$Costo_combustible*Num_buses
    Potencia_panel=as.numeric(input$Potencia_panel)
    
    Potencia_panel=Potencia_panel/1000
    df_ISP$"Numero de paneles minimo"<-ceiling(as.numeric((CE_Mes/(df_ISP$ISP_Max*30)))/(Potencia_panel*0.8))
    df_ISP$"Numero de paneles max"<-ceiling(as.numeric((CE_Mes/(df_ISP$ISP_Min*30)))/(Potencia_panel*0.8))
    plot_1=plot(x=df_ISP$MES,y=df_ISP$ISP_Max)
    
    return(plot_1)
  })
  
  output$plot1 <- renderPlot({Display_plot()
    
    
  })
  
  output$EQ <- renderUI({withMathJax("$$\\huge{ 
C_{\\min }=\\sum_{i=1}^{k=50}\\left(x_{\\mathrm{i}} * E_i * C E * D\\right)
}$$")})
  output$Res <- renderUI({
  withMathJax("$$\\Large{ D \\leq A_i}$$ $$\\Large{ R P \\leq P_i}$$
              $$\\Large{T D \\geq t_i}$$ $$\\Large{x_i \\in\\{0,1\\}}$$
              $$\\Large{\\sum_{i=1}^{k=50} x_i=1}$$")
  })
  output$GL <- renderUI({  withMathJax("$$\\large{ C=\\text { Costo diario de combustible }(\\$ / d i  a)}$$
              $$\\large{i=\\text { Modelo de bus }}$$ 
              $$\\large{x_i= \\text { Número de buses del modelo } i}$$ 
              $$\\large{k= \\text { Número de modelos de buses}}$$
              $$\\large{E_i=\\text { Consumo energético del bus modelo } i(\\mathrm{kWh} / \\mathrm{km})}$$
              $$\\large{C E=\\text { Costo de electricidad }(\\$ / k W h)}$$
              $$\\large{D=\\text { Distancia recorrida diariamente }(\\mathrm{km})}$$                     
              $$\\large{R P=\\text { Requerimiento de pasajeros (personas)}}$$                        
              $$\\large{T D=\\text { Tiempo disponible diario }(horas)}$$                        
              $$\\large{A_i=\\text { Autonomía del bus modelo } i(\\mathrm{km})}$$                        
              $$\\large{P_i=\\text { Capacidad de pasajeros del bus modelo } i \\text { (personas) }}$$                        
              $$\\large{t_i=\\text { Tiempo de carga del bus } i \\text { (horas) }}$$   
                                       ")
    })
  observeEvent(input$Guardar, {
    if (Error$data!=TRUE){
    show_alert(
      title = "¡Completado!",
      text = "El modelo halló una solución óptima",
      type = "success"
    )
    }
    else{
      show_alert(
        title = "¡Error!",
        text = "El modelo no pudo encontrar una solución factible",
        type = "error"
      )
    }
  })

}

#Ejecutar app

shinyApp(ui, server)
