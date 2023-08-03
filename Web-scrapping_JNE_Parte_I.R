################################################################################################
################################################################################################
#Web scraping pagina Plataforma Electoral JNE Lista de candidatos a congreso (Todos los partidos)
################################################################################################

#Paquetes
paquetes <- c("xml2", "stringr", "httr","RSelenium")
instalaciones <- paquetes[!paquetes %in% installed.packages()]
for(libs in instalaciones) install.packages(libs)
sapply(paquetes,require,character = T)


#--Perfil del Navegador
referencedirectory <- "D:/CURSO-WEB SCRAPING CON R/"
fprof <- makeFirefoxProfile(list(browser.download.dir = referencedirectory,browser.download.folderList = 2L, browser.download.manager.showWhenStarting = FALSE,
                                 browser.helperApps.neverAsk.saveToDisk="text/plain,text/x-csv,text/csv,application/ms-excel,application/vnd.ms-excel,application/csv,application/x-csv,text/csv,text/comma-separated-values,text/x-comma-separated-values,text/tab-separated-values,application/pdf",browser.tabs.remote.autostart = FALSE,browser.tabs.remote.autostart.2 = FALSE,browser.tabs.remote.desktopbehavior = FALSE))

#Iniciar el firefox
rD <- rsDriver(port = 5158L, browser = "firefox",iedrver = NULL,
               verbose = TRUE, check = TRUE, extraCapabilities = fprof)

#Obtener acceso al cliente
remDr <- rD[["client"]]


#Funcion para un tiempo de espera
testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 
}

#Creamos una lista en donde se guardará la información de las páginas a recorrer

####1####
numero_candidato<-list()
list_numero_candidato<-list()
puesto<-list()
list_puesto<-list()
exp<-list()
list_exp<-list()
est<-list()
list_est<-list()
distr_elect<-list()
list_distr_elect<-list()
total_0<-list()

dni<-list()
list_dni<-list()
sexo<-list()
list_sexo<-list()
ap_paterno<-list()
list_ap_paterno<-list()
ap_materno<-list()
list_ap_materno<-list()
nombres<-list()
list_nombres<-list()
fecha_nacimiento<-list()
list_fecha_nacimiento<-list()
pais<-list()
list_pais<-list()
dpto<-list()
list_dpto<-list()
prov<-list()
list_prov<-list()
org_pol<-list()
list_org_pol<-list()
fecha<-list()
list_fecha<-list()
delito<-list()
list_delito<-list()
falla1<-list()
list_falla1<-list()
mat_demanda<-list()
list_mat_demanda<-list()
nro_expediente<-list()
list_nro_expediente<-list()
juzg_penal<-list()
list_juzg_penal<-list()
falla2<-list()
list_falla2<-list()
total_1<-list()

estud_prim<-list()
list_estud_prim<-list()
estud_prim_concl<-list()
list_estud_prim_concl<-list()
estud_sec<-list()
list_estud_sec<-list()
estud_sec_concl<-list()
list_estud_sec_concl<-list()
estud_tec<-list()
list_estud_tec<-list()
nombre_tec<-list()
list_nombre_tec<-list()
nombre_tec_carrera<-list()
list_nombre_tec_carrera<-list()
nombre_tec_carrera_concl<-list()
list_nombre_tec_carrera_concl<-list()
nombre_tec_carrera_com<-list()
list_nombre_tec_carrera_com<-list()
estud_no_univ<-list()
list_estud_no_univ<-list()
nombre_no_univ<-list()
list_nombre_no_univ<-list()
nombre_no_univ_carrera<-list()
list_nombr_no_univ_carrera<-list()
nombre_no_univ_carrera_concl<-list()
list_nombre_no_univ_carrera_concl<-list()
estud_univ<-list()
list_estud_univ<-list()
nombre_univ<-list()
list_nombre_univ<-list()
nombre_univ_carrera_concl<-list()
list_nombre_univ_carrera_concl<-list()
nombre_univ_carrera_grado<-list()
list_nombre_univ_carrera_grado<-list()
nombre_univ_carrera_egresado<-list()
list_nombre_univ_carrera_egresado<-list()
nombre_univ_carrera_año<-list()
list_nombre_univ_carrera_año<-list()
nombre_univ_carrera_comentario<-list()
list_nombre_univ_carrera_comentario<-list()
estud_post<-list()
list_estud_post<-list()
nombre_post<-list()
list_nombre_post<-list()
nombre_post_carrera_grado<-list()
list_nombre_post_carrera_grado<-list()
nombre_post_carrera_concl<-list()
list_nombre_post_carrera_concl<-list()
nombre_post_carrera_egresado<-list()
list_nombre_post_carrera_egresado<-list()
nombre_post_carrera_maestro<-list()
list_nombre_post_carrera_maestro<-list()
nombre_post_carrera_doctor<-list()
list_nombre_post_carrera_doctor<-list()
nombre_post_carrera_año<-list()
list_nombre_post_carrera_año<-list()
nombre_post_carrera_coment<-list()
list_nombre_post_carrera_coment<-list()

nombre_centro_labor<-list()
list_nombre_centro_labor<-list()
nombre_oficio_labor<-list()
list_nombre_oficio_labor<-list()
nombre_ruc_labor<-list()
list_nombre_ruc_labor<-list()
nombre_direccion_labor<-list()
list_nombre_direccion_labor<-list()
nombre_desde_labor<-list()
list_nombre_desde_labor<-list()
nombre_hasta_labor<-list()
list_nombre_hasta_labor<-list()
nombre_pais_labor<-list()
list_nombre_pais_labor<-list()
nombre_dpto_labor<-list()
list_nombre_dpto_labor<-list()
nombre_prov_labor<-list()
list_nombre_prov_labor<-list()
nombre_dist_labor<-list()
list_nombre_dist_labor<-list()
total_laboral<-list()

rba_publico<-list()
list_rba_publico<-list()
rba_privado<-list()
list_rba_privado<-list()
rba_total<-list()
list_rba_total<-list()
rba_publico_ind<-list()
list_rba_publico_ind<-list()
rba_privado_ind<-list()
list_rba_privado_ind<-list()
rba_total_ind<-list()
list_rba_total_ind<-list()
oia_publico<-list()
list_oia_publico<-list()
oia_privado<-list()
list_oia_privado<-list()
oia_otros<-list()
list_oia_otros<-list()
total_ingresos<-list()
list_total_ingresos<-list()


inm_numero<-list()
list_inm_numero<-list()

inm_numero2<-list()
list_inm_numero2<-list()

inm_numero3<-list()
list_inm_numero3<-list()

inm_numero4<-list()
list_inm_numero4<-list()

inm_numero5<-list()
list_inm_numero5<-list()

inm_numero6<-list()
list_inm_numero6<-list()

inm_numero7<-list()
list_inm_numero7<-list()

inm_numero8<-list()
list_inm_numero8<-list()

inm_numero9<-list()
list_inm_numero9<-list()

inm_numero10<-list()
list_inm_numero10<-list()


inm_bien<-list()
list_inm_bien<-list()

inm_bien2<-list()
list_inm_bien2<-list()

inm_bien3<-list()
list_inm_bien3<-list()

inm_bien4<-list()
list_inm_bien4<-list()

inm_bien5<-list()
list_inm_bien5<-list()

inm_bien6<-list()
list_inm_bien6<-list()

inm_bien7<-list()
list_inm_bien7<-list()

inm_bien8<-list()
list_inm_bien8<-list()

inm_bien9<-list()
list_inm_bien9<-list()

inm_bien10<-list()
list_inm_bien10<-list()


inm_direccion<-list()
list_inm_direccion<-list()

inm_direccion2<-list()
list_inm_direccion2<-list()

inm_direccion3<-list()
list_inm_direccion3<-list()

inm_direccion4<-list()
list_inm_direccion4<-list()

inm_direccion5<-list()
list_inm_direccion5<-list()

inm_direccion6<-list()
list_inm_direccion6<-list()

inm_direccion7<-list()
list_inm_direccion7<-list()

inm_direccion8<-list()
list_inm_direccion8<-list()

inm_direccion9<-list()
list_inm_direccion9<-list()

inm_direccion10<-list()
list_inm_direccion10<-list()


inm_sunarp<-list()
list_inm_sunarp<-list()

inm_sunarp2<-list()
list_inm_sunarp2<-list()

inm_sunarp3<-list()
list_inm_sunarp3<-list()

inm_sunarp4<-list()
list_inm_sunarp4<-list()

inm_sunarp5<-list()
list_inm_sunarp5<-list()

inm_sunarp6<-list()
list_inm_sunarp6<-list()

inm_sunarp7<-list()
list_inm_sunarp7<-list()

inm_sunarp8<-list()
list_inm_sunarp8<-list()

inm_sunarp9<-list()
list_inm_sunarp9<-list()

inm_sunarp10<-list()
list_inm_sunarp10<-list()


inm_partida<-list()
list_inm_partida<-list()

inm_partida2<-list()
list_inm_partida2<-list()

inm_partida3<-list()
list_inm_partida3<-list()

inm_partida4<-list()
list_inm_partida4<-list()

inm_partida5<-list()
list_inm_partida5<-list()

inm_partida6<-list()
list_inm_partida6<-list()

inm_partida7<-list()
list_inm_partida7<-list()

inm_partida8<-list()
list_inm_partida8<-list()

inm_partida9<-list()
list_inm_partida9<-list()

inm_partida10<-list()
list_inm_partida10<-list()


inm_valor<-list()
list_inm_valor<-list()

inm_valor2<-list()
list_inm_valor2<-list()

inm_valor3<-list()
list_inm_valor3<-list()

inm_valor4<-list()
list_inm_valor4<-list()

inm_valor5<-list()
list_inm_valor5<-list()

inm_valor6<-list()
list_inm_valor6<-list()

inm_valor7<-list()
list_inm_valor7<-list()

inm_valor8<-list()
list_inm_valor8<-list()

inm_valor9<-list()
list_inm_valor9<-list()

inm_valor10<-list()
list_inm_valor10<-list()


inm_comentario<-list()
list_inm_comentario<-list()

inm_comentario2<-list()
list_inm_comentario2<-list()

inm_comentario3<-list()
list_inm_comentario3<-list()

inm_comentario4<-list()
list_inm_comentario4<-list()

inm_comentario5<-list()
list_inm_comentario5<-list()

inm_comentario6<-list()
list_inm_comentario6<-list()

inm_comentario7<-list()
list_inm_comentario7<-list()

inm_comentario8<-list()
list_inm_comentario8<-list()

inm_comentario9<-list()
list_inm_comentario9<-list()

inm_comentario10<-list()
list_inm_comentario10<-list()


inm_total<-list()
list_inm_total<-list()


inm_numero_vehiculo<-list()
list_inm_numero_vehiculo<-list()

inm_numero_vehiculo1<-list()
list_inm_numero_vehiculo1<-list()

inm_numero_vehiculo2<-list()
list_inm_numero_vehiculo2<-list()

inm_numero_vehiculo3<-list()
list_inm_numero_vehiculo3<-list()

inm_numero_vehiculo4<-list()
list_inm_numero_vehiculo4<-list()

inm_numero_vehiculo5<-list()
list_inm_numero_vehiculo5<-list()


inm_vehiculo<-list()
list_inm_vehiculo<-list()

inm_vehiculo1<-list()
list_inm_vehiculo1<-list()

inm_vehiculo2<-list()
list_inm_vehiculo2<-list()

inm_vehiculo3<-list()
list_inm_vehiculo3<-list()

inm_vehiculo4<-list()
list_inm_vehiculo4<-list()

inm_vehiculo5<-list()
list_inm_vehiculo5<-list()

inm_placa_vehiculo<-list()
list_inm_placa_vehiculo<-list()

inm_placa_vehiculo1<-list()
list_inm_placa_vehiculo1<-list()

inm_placa_vehiculo2<-list()
list_inm_placa_vehiculo2<-list()

inm_placa_vehiculo3<-list()
list_inm_placa_vehiculo3<-list()

inm_placa_vehiculo4<-list()
list_inm_placa_vehiculo4<-list()

inm_placa_vehiculo5<-list()
list_inm_placa_vehiculo5<-list()

inm_carac_vehiculo<-list()
list_inm_carac_vehiculo<-list()

inm_carac_vehiculo1<-list()
list_inm_carac_vehiculo1<-list()

inm_carac_vehiculo2<-list()
list_inm_carac_vehiculo2<-list()

inm_carac_vehiculo3<-list()
list_inm_carac_vehiculo3<-list()

inm_carac_vehiculo4<-list()
list_inm_carac_vehiculo4<-list()

inm_carac_vehiculo5<-list()
list_inm_carac_vehiculo5<-list()

inm_valor_vehiculo<-list()
list_inm_valor_vehiculo<-list()

inm_valor_vehiculo1<-list()
list_inm_valor_vehiculo1<-list()

inm_valor_vehiculo2<-list()
list_inm_valor_vehiculo2<-list()

inm_valor_vehiculo3<-list()
list_inm_valor_vehiculo3<-list()

inm_valor_vehiculo4<-list()
list_inm_valor_vehiculo4<-list()

inm_valor_vehiculo5<-list()
list_inm_valor_vehiculo5<-list()

inm_com_vehiculo<-list()
list_inm_com_vehiculo<-list()

inm_com_vehiculo1<-list()
list_inm_com_vehiculo1<-list()

inm_com_vehiculo2<-list()
list_inm_com_vehiculo2<-list()

inm_com_vehiculo3<-list()
list_inm_com_vehiculo3<-list()

inm_com_vehiculo4<-list()
list_inm_com_vehiculo4<-list()

inm_com_vehiculo5<-list()
list_inm_com_vehiculo5<-list()

total_listas<-list()
#######################################
#--Descargaramos hoja de vida ##
#######################################

#Cambiar el url de acuerdo al Frame
remDr$navigate("https://plataformahistorico.jne.gob.pe/OrganizacionesPoliticas/BusquedaAvanzada")

#Busca en la página el elemento tipo de elección (congresal)
testit(4)
click_nivel_elec<- remDr$findElement("xpath","/html/body/main/div/div/div[3]/div[8]/select/option[4]")
#Luego utilizas clic element para realizar un click
click_nivel_elec$clickElement()

#Buscamos los elementos de los partidos
rows <- remDr$findElements("xpath","/html/body/main/div/div/div[3]/div[10]/select/option")
con<-1

for (row in rows){
  
  con=con+1
  print(con)
  
  testit(4)
  
  #Fila de congresitas
  #/html/body/main/div/div/div[3]/div[10]/select
  click_nivel_org <- remDr$findElement("xpath",paste0("/html/body/main/div/div/div[3]/div[10]/select/option[",toString(con),"]"))
  click_nivel_org$clickElement()
  
  
  #Busca en la página el elemento tipo de elección (primer boton:BUSCAR CANDIDATOS)
  #/html/body/main/div/div/div[5]/button
  click_nivel_org_button <- remDr$findElement("xpath","/html/body/main/div/div/div[5]/button")
  click_nivel_org_button$clickElement()
  testit(4)
  
  #Hacemos click en el boton "carga mas candidatos" tantas veces para que puedan ser visualizados 
  click_mas_candidatos_button <- remDr$findElement("xpath","/html/body/main/div/div/div[8]/article/div[2]/button")
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  click_mas_candidatos_button$clickElement()
  
  #Creamos un contador que nos permitirá obtener las divisiones 
  count2<-0
  
  #Obtenemos el numero de candidatos por partido, en el caso de accion popular son 160 cand
  candidatos<- remDr$findElements("xpath","/html/body/main/div/div/div[7]/div[1]/p[2]")
  can<-unlist(lapply(candidatos,function(x){x$getElementText()}))
  can<-substr(can,1,3)
  can<-as.numeric(can)
  
  for (i in 1:can){
    count2=count2+1
    print(count2)
    #/html/body/main/div/div/div[8]/article/div[1]/div[3]/div[2]/div[2]/div[2]/button[1]
    
    #Realizamos un store de los siguientes datos: Puesto al que postula, Expediente de inscripción, Estado
    #Puesto al que postula
    #/html/body/main/div/div/div[8]/article/div[1]/div[1]/div[2]/div[2]/p[2]

    numero_candidato[[i]]<-remDr$findElements("xpath",paste0("/html/body/main/div/div/div[8]/article/div[1]/div[",toString(count2),"]/div[2]/div[1]/div[3]/span"))
    list_numero_candidato[[i]]<-unlist(lapply(numero_candidato[[i]],function(x){x$getElementText()}))
    
    puesto[[i]]<-remDr$findElements("xpath",paste0("/html/body/main/div/div/div[8]/article/div[1]/div[",toString(count2),"]/div[2]/div[2]/p[2]"))
    list_puesto[[i]]<-unlist(lapply(puesto[[i]],function(x){x$getElementText()}))
    #Expediente de inscripción
    #/html/body/main/div/div/div[8]/article/div[1]/div[1]/div[2]/div[2]/p[3]
    #/html/body/main/div/div/div[8]/article/div[1]/div[2]/div[2]/div[2]/p[3]
    exp[[i]]<-remDr$findElements("xpath",paste0("/html/body/main/div/div/div[8]/article/div[1]/div[",toString(count2),"]/div[2]/div[2]/p[3]"))
    list_exp[[i]]<-unlist(lapply(exp[[i]],function(x){x$getElementText()}))
    #Estado
    #/html/body/main/div/div/div[8]/article/div[1]/div[1]/div[2]/div[2]/p[4]
    #/html/body/main/div/div/div[8]/article/div[1]/div[2]/div[2]/div[2]/p[4]
    est[[i]]<-remDr$findElements("xpath",paste0("/html/body/main/div/div/div[8]/article/div[1]/div[",toString(count2),"]/div[2]/div[2]/p[4]"))
    list_est[[i]]<-unlist(lapply(est[[i]],function(x){x$getElementText()}))
    #Distrito electoral
    #/html/body/main/div/div/div[8]/article/div[1]/div[1]/div[2]/div[1]/div[1]/p
    #/html/body/main/div/div/div[8]/article/div[1]/div[2]/div[2]/div[1]/div[1]/p
    
    distr_elect[[i]]<-remDr$findElements("xpath",paste0("/html/body/main/div/div/div[8]/article/div[1]/div[",toString(count2),"]/div[2]/div[1]/div[1]/p"))
    list_distr_elect[[i]]<-unlist(lapply(distr_elect[[i]],function(x){x$getElementText()}))
    
    #Crear un dataframe del candidato
    df_0<-data.frame(unlist(list_puesto[[i]]),unlist(list_exp[[i]]),unlist(list_est[[i]]),unlist(list_distr_elect[[i]]))
    
    total_0[[i]]<-df_0
    
    #Click hoja de vida del candidato
    click_nivel_candidatos <- remDr$findElement("xpath",paste0("/html/body/main/div/div/div[8]/article/div[1]/div[",toString(count2),"]/div[2]/div[2]/div[2]/button[1]"))
    testit(4)
    click_nivel_candidatos$clickElement()
    
    
    #Ventana actual
    mainWindow <- remDr$getCurrentWindowHandle()
    print(mainWindow)
    
    allWindows<-remDr$getWindowHandles()
    print(allWindows)
    
    testit(4)
    
    
    if (length(allWindows)==1){
      count2<-count2+1
      click_nivel_candidatos <- remDr$findElement("xpath",paste0("/html/body/main/div/div/div[8]/article/div[1]/div[",toString(count2),"]/div[2]/div[2]/div[2]/button[1]"))
      click_nivel_candidatos$clickElement()
      
      
      #Ventana actual
      mainWindow <- remDr$getCurrentWindowHandle()
      print(mainWindow)
      
      allWindows<-remDr$getWindowHandles()
      print(allWindows)
      
      testit(4)
      
    } else{
      print("Falso")
    }
    
    if (length(allWindows)==1){
      count2<-count2+1
      click_nivel_candidatos <- remDr$findElement("xpath",paste0("/html/body/main/div/div/div[8]/article/div[1]/div[",toString(count2),"]/div[2]/div[2]/div[2]/button[1]"))
      click_nivel_candidatos$clickElement()
      
      
      #Ventana actual
      mainWindow <- remDr$getCurrentWindowHandle()
      print(mainWindow)
      
      allWindows<-remDr$getWindowHandles()
      print(allWindows)
      
      testit(4)
      
    } else{
      print("Falso")
    }
    
    #Cambiamos a la siguiente ventana
    remDr$switchToWindow(allWindows[[2]])
    
    testit(4)
    
    
    #Ya estamos en la hoja de vida del candidato
    #Obtenemos sus datos personales (Nombres, apellidos, sexo,fecha de nacimiento)
    #Dni
    #//*[@id="datos_personales"]/div[2]/div[2]/div[5]/label[2]
    #n<-c('//*[@id="datos_personales"]/div[2]/div[2]/div[5]/label[2]')
    
    dni[[i]]<-remDr$findElements("xpath","//*[@id='datos_personales']/div[2]/div[2]/div[1]/label[2]")
    list_dni[[i]]<-unlist(lapply(dni[[i]],function(x){x$getElementText()}))
    #Sexo
    sexo[[i]]<-remDr$findElements("xpath","//*[@id='datos_personales']/div[2]/div[2]/div[2]/label[2]")
    list_sexo[[i]]<-unlist(lapply(sexo[[i]],function(x){x$getElementText()}))
    #Apellido Paterno
    ap_paterno[[i]]<-remDr$findElements("xpath","//*[@id='datos_personales']/div[2]/div[2]/div[3]/label[2]")
    list_ap_paterno[[i]]<-unlist(lapply(ap_paterno[[i]],function(x){x$getElementText()}))
    #Apellido Materno
    ap_materno[[i]]<-remDr$findElements("xpath","//*[@id='datos_personales']/div[2]/div[2]/div[4]/label[2]")
    list_ap_materno[[i]]<-unlist(lapply(ap_materno[[i]],function(x){x$getElementText()}))
    #Nombres
    nombres[[i]]<-remDr$findElements("xpath","//*[@id='datos_personales']/div[2]/div[2]/div[5]/label[2]")
    list_nombres[[i]]<-unlist(lapply(nombres[[i]],function(x){x$getElementText()}))
    #Fecha de nacimiento
    fecha_nacimiento[[i]]<-remDr$findElements("xpath","//*[@id='datos_personales']/div[2]/div[2]/div[6]/label[2]")
    list_fecha_nacimiento[[i]]<-unlist(lapply(fecha_nacimiento[[i]],function(x){x$getElementText()}))
    
    #Lugar de nacimiento
    #País
    #//*[@id="datos_personales"]/div[3]/div[1]/label[2]
    pais[[i]]<-remDr$findElements("xpath","//*[@id='datos_personales']/div[3]/div[1]/label[2]")
    list_pais[[i]]<-unlist(lapply(pais[[i]],function(x){x$getElementText()}))
    
    #Departamento
    #//*[@id="datos_personales"]/div[3]/div[2]/label[2]
    dpto[[i]]<-remDr$findElements("xpath","//*[@id='datos_personales']/div[3]/div[2]/label[2]")
    list_dpto[[i]]<-unlist(lapply(dpto[[i]],function(x){x$getElementText()}))
    
    #Provincia
    #//*[@id="datos_personales"]/div[3]/div[3]/label[2]
    prov[[i]]<-remDr$findElements("xpath","//*[@id='datos_personales']/div[3]/div[3]/label[2]")
    list_prov[[i]]<-unlist(lapply(prov[[i]],function(x){x$getElementText()}))
    
    #Organización por la cual postula
    #//*[@id="datos_personales"]/div[6]/div/label[2]
    org_pol[[i]]<-remDr$findElements("xpath","//*[@id='datos_personales']/div[6]/div/label[2]")
    list_org_pol[[i]]<-unlist(lapply(org_pol[[i]],function(x){x$getElementText()}))
    
    #Experiencia laboral
    #NOMBRE DEL CENTRO DE PRESTACIÓN DEL SERVICIO O TRABAJO
    
    nombre_centro_labor[[i]]<-remDr$findElements("xpath","//*[@id='atcRegistros']/div[1]/div/label[2]")
    list_nombre_centro_labor[[i]]<-unlist(lapply(nombre_centro_labor[[i]],function(x){x$getElementText()}))
    #OFICIOS / OCUPACIONES / PROFESIONES
    nombre_oficio_labor[[i]]<-remDr$findElements("xpath","//*[@id='atcRegistros']/div[2]/div[1]/label[2]")
    list_nombre_oficio_labor[[i]]<-unlist(lapply(nombre_oficio_labor[[i]],function(x){x$getElementText()}))
    #RUC EMPRESA (OPCIONAL):
    nombre_ruc_labor[[i]]<-remDr$findElements("xpath","//*[@id='atcRegistros']/div[2]/div[2]/label[2]")
    list_nombre_ruc_labor[[i]]<-unlist(lapply(nombre_ruc_labor[[i]],function(x){x$getElementText()}))
    #Dirección
    nombre_direccion_labor[[i]]<-remDr$findElements("xpath","//*[@id='atcRegistros']/div[3]/div[1]/label[2]")
    list_nombre_direccion_labor[[i]]<-unlist(lapply(nombre_direccion_labor[[i]],function(x){x$getElementText()}))
    #Desde (Año)
    nombre_desde_labor[[i]]<-remDr$findElements("xpath","//*[@id='atcRegistros']/div[3]/div[2]/label[2]")
    list_nombre_desde_labor[[i]]<-unlist(lapply(nombre_desde_labor[[i]],function(x){x$getElementText()}))
    #Hasta (Año)
    nombre_hasta_labor[[i]]<-remDr$findElements("xpath","//*[@id='atcRegistros']/div[3]/div[3]/label[2]")
    list_nombre_hasta_labor[[i]]<-unlist(lapply(nombre_hasta_labor[[i]],function(x){x$getElementText()}))
    #Pais
    nombre_pais_labor[[i]]<-remDr$findElements("xpath","//*[@id='atcRegistros']/div[4]/div[1]/label[2]")
    list_nombre_pais_labor[[i]]<-unlist(lapply(nombre_pais_labor[[i]],function(x){x$getElementText()}))
    #Departamento
    nombre_dpto_labor[[i]]<-remDr$findElements("xpath","//*[@id='atcRegistros']/div[4]/div[2]/label[2]")
    list_nombre_dpto_labor[[i]]<-unlist(lapply(nombre_dpto_labor[[i]],function(x){x$getElementText()}))
    #Provincia
    nombre_prov_labor[[i]]<-remDr$findElements("xpath","//*[@id='atcRegistros']/div[4]/div[3]/label[2]")
    list_nombre_prov_labor[[i]]<-unlist(lapply(nombre_prov_labor[[i]],function(x){x$getElementText()}))
    #Distrito
    nombre_dist_labor[[i]]<-remDr$findElements("xpath","//*[@id='atcRegistros']/div[4]/div[4]/label[2]")
    list_nombre_dist_labor[[i]]<-unlist(lapply(nombre_dist_labor[[i]],function(x){x$getElementText()}))
    
    #Creamos una data frame con la experiencia laboral ya que no se puede unir a las demas.
    #Crear un dataframe del candidato
    df_laboral<-data.frame(unlist(list_nombre_centro_labor[[i]]),
                           unlist(list_nombre_oficio_labor[[i]]),unlist(list_nombre_ruc_labor[[i]]),unlist(list_nombre_direccion_labor[[i]]),
                           unlist(list_nombre_desde_labor[[i]]),unlist(list_nombre_hasta_labor[[i]]),unlist(list_nombre_pais_labor[[i]]),
                           unlist(list_nombre_dpto_labor[[i]]),unlist(list_nombre_prov_labor[[i]]),unlist(list_nombre_dist_labor[[i]]))
    
    total_laboral[[i]]<-df_laboral
    
    ###Educación###
    #Cuenta con estudios primarios
    estud_prim[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[3]/div[1]/label[2]")
    list_estud_prim[[i]]<-unlist(lapply(estud_prim[[i]],function(x){x$getElementText()}))
    #Estudios primarios concluidos
    estud_prim_concl[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[3]/div[2]/label[2]")
    list_estud_prim_concl[[i]]<-unlist(lapply(estud_prim_concl[[i]],function(x){x$getElementText()}))
    #Cuenta con estudios secundarios
    estud_sec[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[4]/div[1]/label[2]")
    list_estud_sec[[i]]<-unlist(lapply(estud_sec[[i]],function(x){x$getElementText()}))
    #Estudios secundarios concluidos
    estud_sec_concl[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[4]/div[2]/label[2]")
    list_estud_sec_concl[[i]]<-unlist(lapply(estud_sec_concl[[i]],function(x){x$getElementText()}))
    #####Estudios técnicos#######
    #Cuenta con estudios técnicos
    estud_tec[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[6]/div/label[2]")
    list_estud_tec[[i]]<-unlist(lapply(estud_tec[[i]],function(x){x$getElementText()}))
    #Nombre del centro de estudios
    nombre_tec[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[7]/div[1]/label[2]")
    list_nombre_tec[[i]]<-unlist(lapply(nombre_tec[[i]],function(x){x$getElementText()}))
    #Nombre de la carrera
    nombre_tec_carrera[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[7]/div[2]/label[2]")
    list_nombre_tec_carrera[[i]]<-unlist(lapply(nombre_tec_carrera[[i]],function(x){x$getElementText()}))
    #Concluidos
    nombre_tec_carrera_concl[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[7]/div[3]/label[2]")
    list_nombre_tec_carrera_concl[[i]]<-unlist(lapply(nombre_tec_carrera_concl[[i]],function(x){x$getElementText()}))
    #Comentario
    nombre_tec_carrera_com[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[8]/div/label[2]")
    list_nombre_tec_carrera_com[[i]]<-unlist(lapply(nombre_tec_carrera_com[[i]],function(x){x$getElementText()}))
    
    #####Estudios No universitarios#######
    #Cuenta con estudios no universitarios
    estud_no_univ[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[9]/div/label[2]")
    list_estud_no_univ[[i]]<-unlist(lapply(estud_no_univ[[i]],function(x){x$getElementText()}))
    #Nombre del centro de estudios
    nombre_no_univ[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[10]/div[1]/label[2]")
    list_nombre_no_univ[[i]]<-unlist(lapply(nombre_no_univ[[i]],function(x){x$getElementText()}))
    #Nombre de la carrera
    nombre_no_univ_carrera[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[10]/div[2]/label[2]")
    list_nombr_no_univ_carrera[[i]]<-unlist(lapply(nombre_no_univ_carrera[[i]],function(x){x$getElementText()}))    
    #Concluidos
    nombre_no_univ_carrera_concl[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[10]/div[3]/label[2]")
    list_nombre_no_univ_carrera_concl[[i]]<-unlist(lapply(nombre_no_univ_carrera_concl[[i]],function(x){x$getElementText()}))
    
    #####Estudios universitarios#######
    #Cuenta con estudios universitarios
    estud_univ[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[12]/div/label[2]")
    list_estud_univ[[i]]<-unlist(lapply(estud_univ[[i]],function(x){x$getElementText()}))
    #Nombre del centro de estudios
    nombre_univ[[i]]<-remDr$findElements("xpath","//*[@id='verUniversitario1']/article/div[1]/div[1]/label[2]")
    list_nombre_univ[[i]]<-unlist(lapply(nombre_univ[[i]],function(x){x$getElementText()}))
    #Concluidos
    nombre_univ_carrera_concl[[i]]<-remDr$findElements("xpath","//*[@id='verUniversitario1']/article/div[1]/div[2]/label[2]")
    list_nombre_univ_carrera_concl[[i]]<-unlist(lapply(nombre_univ_carrera_concl[[i]],function(x){x$getElementText()}))
    #Grado o título
    nombre_univ_carrera_grado[[i]]<-remDr$findElements("xpath","//*[@id='verUniversitario1']/article/div[2]/div[1]/label[2]")
    list_nombre_univ_carrera_grado[[i]]<-unlist(lapply(nombre_univ_carrera_grado[[i]],function(x){x$getElementText()}))
    #Egresado
    nombre_univ_carrera_egresado[[i]]<-remDr$findElements("xpath","//*[@id='verUniversitario1']/article/div[2]/div[2]/label[2]")
    list_nombre_univ_carrera_egresado[[i]]<-unlist(lapply(nombre_univ_carrera_egresado[[i]],function(x){x$getElementText()}))
    #Año de obtención
    nombre_univ_carrera_año[[i]]<-remDr$findElements("xpath","//*[@id='verUniversitario1']/article/div[3]/div[1]/label[2]")
    list_nombre_univ_carrera_año[[i]]<-unlist(lapply(nombre_univ_carrera_año[[i]],function(x){x$getElementText()}))
    #Comentario
    nombre_univ_carrera_comentario[[i]]<-remDr$findElements("xpath","//*[@id='verUniversitario1']/article/div[3]/div[2]/label[2]")
    list_nombre_univ_carrera_comentario[[i]]<-unlist(lapply(nombre_univ_carrera_comentario[[i]],function(x){x$getElementText()}))
    
    #####Estudios de postgrado#######
    #Cuenta con estudios de postgrado
    estud_post[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[13]/div/label[2]")
    list_estud_post[[i]]<-unlist(lapply(estud_post[[i]],function(x){x$getElementText()}))
    #Nombre del centro de estudios
    nombre_post[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[14]/div[1]/label[2]")
    list_nombre_post[[i]]<-unlist(lapply(nombre_post[[i]],function(x){x$getElementText()}))
    #Grado o título
    nombre_post_carrera_grado[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[14]/div[2]/label[2]")
    list_nombre_post_carrera_grado[[i]]<-unlist(lapply(nombre_post_carrera_grado[[i]],function(x){x$getElementText()}))
    #Concluidos
    nombre_post_carrera_concl[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[15]/div[1]/label[2]")
    list_nombre_post_carrera_concl[[i]]<-unlist(lapply(nombre_post_carrera_concl[[i]],function(x){x$getElementText()}))
    #Egresado
    nombre_post_carrera_egresado[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[15]/div[2]/label[2]")
    list_nombre_post_carrera_egresado[[i]]<-unlist(lapply(nombre_post_carrera_egresado[[i]],function(x){x$getElementText()}))
    #Grado obtenido
    
    #Maestro
    nombre_post_carrera_maestro[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[16]/div[1]/label[2]")
    list_nombre_post_carrera_maestro[[i]]<-unlist(lapply(nombre_post_carrera_maestro[[i]],function(x){x$getElementText()}))
    #Doctor
    nombre_post_carrera_doctor[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[16]/div[2]/label[2]")
    list_nombre_post_carrera_doctor[[i]]<-unlist(lapply(nombre_post_carrera_doctor[[i]],function(x){x$getElementText()}))
    #Año de obtención
    nombre_post_carrera_año[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[16]/div[3]/label[2]")
    list_nombre_post_carrera_año[[i]]<-unlist(lapply(nombre_post_carrera_año[[i]],function(x){x$getElementText()}))
    #Comentario
    nombre_post_carrera_coment[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[4]/div[17]/div/label[2]")
    list_nombre_post_carrera_coment[[i]]<-unlist(lapply(nombre_post_carrera_coment[[i]],function(x){x$getElementText()}))
    
    
    #Relacion de sentencias
    #Fecha de sentencia firme
    #//*[@id="sije"]/main/section/article/section/div/div/table/tbody/tr/td/section[7]/section/article/div[2]/div[1]/label[2]
    fecha[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[7]/section/article/div[2]/div[1]/label[2]")
    list_fecha[[i]]<-unlist(lapply(fecha[[i]],function(x){x$getElementText()}))
    
    #Delito
    #//*[@id="sije"]/main/section/article/section/div/div/table/tbody/tr/td/section[7]/section/article/div[2]/div[1]/label[2]
    delito[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[7]/section/article/div[2]/div[1]/label[2]")
    list_delito[[i]]<- unlist(lapply(delito[[i]], function(x){x$getElementText()}))
    
    #Falla o pena
    #//*[@id="sije"]/main/section/article/section/div/div/table/tbody/tr/td/section[7]/section/article/div[2]/div[2]/label[2]
    falla1[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[7]/section/article/div[2]/div[2]/label[2]")
    list_falla1[[i]]<- unlist(lapply(falla1[[i]],function(x){x$getElementText()}))
    
    
    #RELACIÓN DE SENTENCIAS QUE DECLAREN FUNDADAS LAS DEMANDAS INTERPUESTAS CONTRA LOS CANDIDATOS(AS) POR INCUMPLIMIENTO DE OBLIGACIONES ALIMENTARIAS, CONTRACTUALES, LABORALES O POR INCURRIR EN VIOLENCIA FAMILIAR, QUE HUBIERAN QUEDADO FIRME
    #MATERIA DE LA DEMANDA
    #//*[@id="sije"]/main/section/article/section/div/div/table/tbody/tr/td/section[8]/section/article/div[1]/div[1]/label[2]
    mat_demanda[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[8]/section/article/div[1]/div[1]/label[2]")
    list_mat_demanda[[i]]<- unlist(lapply(mat_demanda[[i]], function(x){x$getElementText()}))
    #N de expediente
    #//*[@id="sije"]/main/section/article/section/div/div/table/tbody/tr/td/section[8]/section/article/div[1]/div[2]/label[2]
    nro_expediente[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[8]/section/article/div[1]/div[2]/label[2]")
    list_nro_expediente[[i]]<-unlist(lapply(nro_expediente[[i]], function(x){x$getElementText()}))
    #Organo Judicial
    juzg_penal[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[8]/section/article/div[1]/div[3]/label[2]")
    list_juzg_penal[[i]]<-unlist(lapply(juzg_penal[[i]], function(x){x$getElementText()}))
    
    #Falla/pena
    #//*[@id="sije"]/main/section/article/section/div/div/table/tbody/tr/td/section[8]/section/article/div[2]/div/label[2]
    falla2[[i]]<-remDr$findElements("xpath","//*[@id='sije']/main/section/article/section/div/div/table/tbody/tr/td/section[8]/section/article/div[2]/div/label[2]")
    list_falla2[[i]]<-unlist(lapply(falla2[[i]], function(x){x$getElementText()}))
    
    ###DECLARACIÓN JURADA DE INGRESOS DE BIENES Y RENTAS###
    #Remuneracion bruta anual (sector publico)
    #//*[@id='frmIng']/div[2]/table/tbody/tr[1]/td[2]/label
    #/html/body/main/section/article/section/div/div/table/tbody/tr/td/section[9]/section/article[1]/form/div[2]/table/tbody/tr[1]/td[2]/label
    rba_publico[[i]]<-remDr$findElements("xpath","//*[@id='frmIng']/div[2]/table/tbody/tr[1]/td[2]/label")
    list_rba_publico[[i]]<-unlist(lapply(rba_publico[[i]], function(x){x$getElementText()}))
    
    #Remuneracion bruta anual (sector privado)
    #//*[@id="frmIng"]/div[2]/table/tbody/tr[1]/td[3]/label
    
    rba_privado[[i]]<-remDr$findElements("xpath","//*[@id='frmIng']/div[2]/table/tbody/tr[1]/td[3]/label")
    list_rba_privado[[i]]<-unlist(lapply(rba_privado[[i]], function(x){x$getElementText()}))
    
    
    #Remuneracion bruta anual (total S/)
    #//*[@id="frmIng"]/div[2]/table/tbody/tr[1]/td[4]/label
    rba_total[[i]]<-remDr$findElements("xpath","//*[@id='frmIng']/div[2]/table/tbody/tr[1]/td[4]/label")
    list_rba_total[[i]]<-unlist(lapply(rba_total[[i]], function(x){x$getElementText()}))
    
    
    #Renta bruta anual por ejercicio individual (sector publico)
    #//*[@id="frmIng"]/div[2]/table/tbody/tr[2]/td[2]/label
    rba_publico_ind[[i]]<-remDr$findElements("xpath","//*[@id='frmIng']/div[2]/table/tbody/tr[2]/td[2]/label")
    list_rba_publico_ind[[i]]<-unlist(lapply(rba_publico_ind[[i]], function(x){x$getElementText()}))
    
    
    #Renta bruta anual por ejercicio individual (Sector privado)
    #//*[@id="frmIng"]/div[2]/table/tbody/tr[2]/td[3]/label
    rba_privado_ind[[i]]<-remDr$findElements("xpath","//*[@id='frmIng']/div[2]/table/tbody/tr[2]/td[3]/label")
    list_rba_privado_ind[[i]]<-unlist(lapply(rba_privado_ind[[i]], function(x){x$getElementText()}))
    
    
    #Renta bruta anual por ejercicio individual (Total S/)
    #//*[@id="frmIng"]/div[2]/table/tbody/tr[2]/td[4]/label
    rba_total_ind[[i]]<-remDr$findElements("xpath","//*[@id='frmIng']/div[2]/table/tbody/tr[2]/td[4]/label")
    list_rba_total_ind[[i]]<-unlist(lapply(rba_total_ind[[i]], function(x){x$getElementText()}))
    
    #Otros ingresos anuales (sector publico)
    #//*[@id="frmIng"]/div[2]/table/tbody/tr[3]/td[2]/label
    oia_publico[[i]]<-remDr$findElements("xpath","//*[@id='frmIng']/div[2]/table/tbody/tr[3]/td[2]/label")
    list_oia_publico[[i]]<-unlist(lapply(oia_publico[[i]], function(x){x$getElementText()}))
    
    #Otros ingresos anuales (sector privado)
    #//*[@id="frmIng"]/div[2]/table/tbody/tr[3]/td[3]/label
    oia_privado[[i]]<-remDr$findElements("xpath","//*[@id='frmIng']/div[2]/table/tbody/tr[3]/td[3]/label")
    list_oia_privado[[i]]<-unlist(lapply(oia_privado[[i]], function(x){x$getElementText()}))
    
    #Otros ingresos anuales (Total S/)
    #//*[@id="frmIng"]/div[2]/table/tbody/tr[3]/td[4]/label
    oia_otros[[i]]<-remDr$findElements("xpath","//*[@id='frmIng']/div[2]/table/tbody/tr[3]/td[4]/label")
    list_oia_otros[[i]]<-unlist(lapply(oia_otros[[i]], function(x){x$getElementText()}))
    
    #Total de ingresos (S/)
    #//*[@id="frmIng"]/div[3]/div/label[2]
    total_ingresos[[i]]<-remDr$findElements("xpath","//*[@id='frmIng']/div[3]/div/label[2]")
    list_total_ingresos[[i]]<-unlist(lapply(total_ingresos[[i]], function(x){x$getElementText()}))
    
    
    
    ###Bienes Inmuebles del Declarante y Sociedad de Gananciales###
    #N°
    #//*[@id="BienesInmuebles"]/div[2]/table/tbody/tr[1]/td[1]
    
    inm_numero[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[1]/td[1]")
    
    if(length(inm_numero[[i]])==0){
      list_inm_numero[[i]]<-0
    }else{
      list_inm_numero[[i]]<-unlist(lapply(inm_numero[[i]], function(x){x$getElementText()}))
    }
    
    
    inm_numero2[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[2]/td[1]")
    if(length(inm_numero2[[i]])==0){
      list_inm_numero2[[i]]<-0
    }else{
      list_inm_numero2[[i]]<-unlist(lapply(inm_numero2[[i]], function(x){x$getElementText()}))
    }
    
    inm_numero3[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[3]/td[1]")
    if(length(inm_numero3[[i]])==0){
      list_inm_numero3[[i]]<-0
    }else{
      list_inm_numero3[[i]]<-unlist(lapply(inm_numero3[[i]], function(x){x$getElementText()}))
    }
    
    inm_numero4[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[4]/td[1]")
    if(length(inm_numero4[[i]])==0){
      list_inm_numero4[[i]]<-0
    }else{
      list_inm_numero4[[i]]<-unlist(lapply(inm_numero4[[i]], function(x){x$getElementText()}))
    }
    
    inm_numero5[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[5]/td[1]")
    if(length(inm_numero5[[i]])==0){
      list_inm_numero5[[i]]<-0
    }else{
      list_inm_numero5[[i]]<-unlist(lapply(inm_numero5[[i]], function(x){x$getElementText()}))
    }
    
    inm_numero6[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[6]/td[1]")
    if(length(inm_numero6[[i]])==0){
      list_inm_numero6[[i]]<-0
    }else{
      list_inm_numero6[[i]]<-unlist(lapply(inm_numero6[[i]], function(x){x$getElementText()}))
    }
    
    inm_numero7[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[7]/td[1]")
    if(length(inm_numero7[[i]])==0){
      list_inm_numero7[[i]]<-0
    }else{
      list_inm_numero7[[i]]<-unlist(lapply(inm_numero7[[i]], function(x){x$getElementText()}))
    }
    
    inm_numero8[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[8]/td[1]")
    if(length(inm_numero8[[i]])==0){
      list_inm_numero8[[i]]<-0
    }else{
      list_inm_numero8[[i]]<-unlist(lapply(inm_numero8[[i]], function(x){x$getElementText()}))
    }
    
    inm_numero9[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[9]/td[1]")
    if(length(inm_numero9[[i]])==0){
      list_inm_numero9[[i]]<-0
    }else{
      list_inm_numero9[[i]]<-unlist(lapply(inm_numero9[[i]], function(x){x$getElementText()}))
    }
    
    inm_numero10[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[10]/td[1]")
    if(length(inm_numero10[[i]])==0){
      list_inm_numero10[[i]]<-0
    }else{
      list_inm_numero10[[i]]<-unlist(lapply(inm_numero10[[i]], function(x){x$getElementText()}))
    }
    
    
    #Tipo de bien
    #//*[@id="BienesInmuebles"]/div[2]/table/tbody/tr[1]/td[2]/label
    inm_bien[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[1]/td[2]/label")
    if(length(inm_bien[[i]])==0){
      list_inm_bien[[i]]<-0
    }else{
      list_inm_bien[[i]]<-unlist(lapply(inm_bien[[i]], function(x){x$getElementText()}))
    }
    
    inm_bien2[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[2]/td[2]/label")
    if(length(inm_bien2[[i]])==0){
      list_inm_bien2[[i]]<-0
    }else{
      list_inm_bien2[[i]]<-unlist(lapply(inm_bien2[[i]], function(x){x$getElementText()}))
    }
    
    inm_bien3[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[3]/td[2]/label")
    if(length(inm_bien3[[i]])==0){
      list_inm_bien3[[i]]<-0
    }else{
      list_inm_bien3[[i]]<-unlist(lapply(inm_bien3[[i]], function(x){x$getElementText()}))
    }
    
    inm_bien4[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[4]/td[2]/label")
    if(length(inm_bien4[[i]])==0){
      list_inm_bien4[[i]]<-0
    }else{
      list_inm_bien4[[i]]<-unlist(lapply(inm_bien4[[i]], function(x){x$getElementText()}))
    }
    
    inm_bien5[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[5]/td[2]/label")
    if(length(inm_bien5[[i]])==0){
      list_inm_bien5[[i]]<-0
    }else{
      list_inm_bien5[[i]]<-unlist(lapply(inm_bien5[[i]], function(x){x$getElementText()}))
    }
    
    inm_bien6[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[6]/td[2]/label")
    if(length(inm_bien6[[i]])==0){
      list_inm_bien6[[i]]<-0
    }else{
      list_inm_bien6[[i]]<-unlist(lapply(inm_bien6[[i]], function(x){x$getElementText()}))
    }
    
    inm_bien7[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[7]/td[2]/label")
    if(length(inm_bien7[[i]])==0){
      list_inm_bien7[[i]]<-0
    }else{
      list_inm_bien7[[i]]<-unlist(lapply(inm_bien7[[i]], function(x){x$getElementText()}))
    }
    
    inm_bien8[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[8]/td[2]/label")
    if(length(inm_bien8[[i]])==0){
      list_inm_bien8[[i]]<-0
    }else{
      list_inm_bien8[[i]]<-unlist(lapply(inm_bien8[[i]], function(x){x$getElementText()}))
    }
    
    inm_bien9[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[9]/td[2]/label")
    if(length(inm_bien9[[i]])==0){
      list_inm_bien9[[i]]<-0
    }else{
      list_inm_bien9[[i]]<-unlist(lapply(inm_bien9[[i]], function(x){x$getElementText()}))
    }
    
    inm_bien10[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[10]/td[2]/label")
    if(length(inm_bien10[[i]])==0){
      list_inm_bien10[[i]]<-0
    }else{
      list_inm_bien10[[i]]<-unlist(lapply(inm_bien10[[i]], function(x){x$getElementText()}))
    }
    
    #Direccion
    #//*[@id="BienesInmuebles"]/div[2]/table/tbody/tr[1]/td[3]/label
    inm_direccion[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[1]/td[3]/label")
    if(length(inm_direccion[[i]])==0){
      list_inm_direccion[[i]]<-0
    }else{
      list_inm_direccion[[i]]<-unlist(lapply(inm_direccion[[i]], function(x){x$getElementText()}))
    }
    
    inm_direccion2[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[2]/td[3]/label")
    if(length(inm_direccion2[[i]])==0){
      list_inm_direccion2[[i]]<-0
    }else{
      list_inm_direccion2[[i]]<-unlist(lapply(inm_direccion2[[i]], function(x){x$getElementText()}))
    }
    
    inm_direccion3[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[3]/td[3]/label")
    if(length(inm_direccion3[[i]])==0){
      list_inm_direccion3[[i]]<-0
    }else{
      list_inm_direccion3[[i]]<-unlist(lapply(inm_direccion3[[i]], function(x){x$getElementText()}))
    }
    
    inm_direccion4[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[4]/td[3]/label")
    if(length(inm_direccion4[[i]])==0){
      list_inm_direccion4[[i]]<-0
    }else{
      list_inm_direccion4[[i]]<-unlist(lapply(inm_direccion4[[i]], function(x){x$getElementText()}))
    }
    
    inm_direccion5[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[5]/td[3]/label")
    if(length(inm_direccion5[[i]])==0){
      list_inm_direccion5[[i]]<-0
    }else{
      list_inm_direccion5[[i]]<-unlist(lapply(inm_direccion5[[i]], function(x){x$getElementText()}))
    }
    
    inm_direccion6[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[6]/td[3]/label")
    if(length(inm_direccion6[[i]])==0){
      list_inm_direccion6[[i]]<-0
    }else{
      list_inm_direccion6[[i]]<-unlist(lapply(inm_direccion6[[i]], function(x){x$getElementText()}))
    }
    
    inm_direccion7[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[7]/td[3]/label")
    if(length(inm_direccion7[[i]])==0){
      list_inm_direccion7[[i]]<-0
    }else{
      list_inm_direccion7[[i]]<-unlist(lapply(inm_direccion7[[i]], function(x){x$getElementText()}))
    }
    
    inm_direccion8[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[8]/td[3]/label")
    if(length(inm_direccion8[[i]])==0){
      list_inm_direccion8[[i]]<-0
    }else{
      list_inm_direccion8[[i]]<-unlist(lapply(inm_direccion8[[i]], function(x){x$getElementText()}))
    }
    
    inm_direccion9[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[9]/td[3]/label")
    if(length(inm_direccion9[[i]])==0){
      list_inm_direccion9[[i]]<-0
    }else{
      list_inm_direccion9[[i]]<-unlist(lapply(inm_direccion9[[i]], function(x){x$getElementText()}))
    }
    
    inm_direccion10[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[10]/td[3]/label")
    if(length(inm_direccion10[[i]])==0){
      list_inm_direccion10[[i]]<-0
    }else{
      list_inm_direccion10[[i]]<-unlist(lapply(inm_direccion10[[i]], function(x){x$getElementText()}))
    }
    
    
    #Inscrito en SUNARP
    #//*[@id="BienesInmuebles"]/div[2]/table/tbody/tr[1]/td[4]/label
    inm_sunarp[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[1]/td[4]/label")
    if(length(inm_sunarp[[i]])==0){
      list_inm_sunarp[[i]]<-0
    }else{
      list_inm_sunarp[[i]]<-unlist(lapply(inm_sunarp[[i]], function(x){x$getElementText()}))
    }
    
    inm_sunarp2[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[2]/td[4]/label")
    if(length(inm_sunarp2[[i]])==0){
      list_inm_sunarp2[[i]]<-0
    }else{
      list_inm_sunarp2[[i]]<-unlist(lapply(inm_sunarp2[[i]], function(x){x$getElementText()}))
    }
    
    inm_sunarp3[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[3]/td[4]/label")
    if(length(inm_sunarp3[[i]])==0){
      list_inm_sunarp3[[i]]<-0
    }else{
      list_inm_sunarp3[[i]]<-unlist(lapply(inm_sunarp3[[i]], function(x){x$getElementText()}))
    }
    
    inm_sunarp4[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[4]/td[4]/label")
    if(length(inm_sunarp4[[i]])==0){
      list_inm_sunarp4[[i]]<-0
    }else{
      list_inm_sunarp4[[i]]<-unlist(lapply(inm_sunarp4[[i]], function(x){x$getElementText()}))
    }
    
    inm_sunarp5[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[5]/td[4]/label")
    if(length(inm_sunarp5[[i]])==0){
      list_inm_sunarp5[[i]]<-0
    }else{
      list_inm_sunarp5[[i]]<-unlist(lapply(inm_sunarp5[[i]], function(x){x$getElementText()}))
    }
    
    inm_sunarp6[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[6]/td[4]/label")
    if(length(inm_sunarp6[[i]])==0){
      list_inm_sunarp6[[i]]<-0
    }else{
      list_inm_sunarp6[[i]]<-unlist(lapply(inm_sunarp6[[i]], function(x){x$getElementText()}))
    }
    
    inm_sunarp7[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[7]/td[4]/label")
    if(length(inm_sunarp7[[i]])==0){
      list_inm_sunarp7[[i]]<-0
    }else{
      list_inm_sunarp7[[i]]<-unlist(lapply(inm_sunarp7[[i]], function(x){x$getElementText()}))
    }
    
    inm_sunarp8[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[8]/td[4]/label")
    if(length(inm_sunarp8[[i]])==0){
      list_inm_sunarp8[[i]]<-0
    }else{
      list_inm_sunarp8[[i]]<-unlist(lapply(inm_sunarp8[[i]], function(x){x$getElementText()}))
    }
    
    inm_sunarp9[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[9]/td[4]/label")
    if(length(inm_sunarp9[[i]])==0){
      list_inm_sunarp9[[i]]<-0
    }else{
      list_inm_sunarp9[[i]]<-unlist(lapply(inm_sunarp9[[i]], function(x){x$getElementText()}))
    }
    
    inm_sunarp10[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[10]/td[4]/label")
    if(length(inm_sunarp10[[i]])==0){
      list_inm_sunarp10[[i]]<-0
    }else{
      list_inm_sunarp10[[i]]<-unlist(lapply(inm_sunarp10[[i]], function(x){x$getElementText()}))
    }
    
    
    #Partida
    #//*[@id="BienesInmuebles"]/div[2]/table/tbody/tr[1]/td[5]/label
    inm_partida[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[1]/td[5]/label")
    if(length(inm_partida[[i]])==0){
      list_inm_partida[[i]]<-0
    }else{
      list_inm_partida[[i]]<-unlist(lapply(inm_partida[[i]], function(x){x$getElementText()}))
    }
    
    inm_partida2[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[2]/td[5]/label")
    if(length(inm_partida2[[i]])==0){
      list_inm_partida2[[i]]<-0
    }else{
      list_inm_partida2[[i]]<-unlist(lapply(inm_partida2[[i]], function(x){x$getElementText()}))
    }
    
    inm_partida3[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[3]/td[5]/label")
    if(length(inm_partida3[[i]])==0){
      list_inm_partida3[[i]]<-0
    }else{
      list_inm_partida3[[i]]<-unlist(lapply(inm_partida3[[i]], function(x){x$getElementText()}))
    }
    
    inm_partida4[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[4]/td[5]/label")
    if(length(inm_partida4[[i]])==0){
      list_inm_partida4[[i]]<-0
    }else{
      list_inm_partida4[[i]]<-unlist(lapply(inm_partida4[[i]], function(x){x$getElementText()}))
    }
    
    inm_partida5[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[5]/td[5]/label")
    if(length(inm_partida5[[i]])==0){
      list_inm_partida5[[i]]<-0
    }else{
      list_inm_partida5[[i]]<-unlist(lapply(inm_partida5[[i]], function(x){x$getElementText()}))
    }
    
    inm_partida6[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[6]/td[5]/label")
    if(length(inm_partida6[[i]])==0){
      list_inm_partida6[[i]]<-0
    }else{
      list_inm_partida6[[i]]<-unlist(lapply(inm_partida6[[i]], function(x){x$getElementText()}))
    }
    
    inm_partida7[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[7]/td[5]/label")
    if(length(inm_partida7[[i]])==0){
      list_inm_partida7[[i]]<-0
    }else{
      list_inm_partida7[[i]]<-unlist(lapply(inm_partida7[[i]], function(x){x$getElementText()}))
    }
    
    inm_partida8[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[8]/td[5]/label")
    if(length(inm_partida8[[i]])==0){
      list_inm_partida8[[i]]<-0
    }else{
      list_inm_partida8[[i]]<-unlist(lapply(inm_partida8[[i]], function(x){x$getElementText()}))
    }
    
    inm_partida9[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[9]/td[5]/label")
    if(length(inm_partida9[[i]])==0){
      list_inm_partida9[[i]]<-0
    }else{
      list_inm_partida9[[i]]<-unlist(lapply(inm_partida9[[i]], function(x){x$getElementText()}))
    }
    
    inm_partida10[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[10]/td[5]/label")
    if(length(inm_partida10[[i]])==0){
      list_inm_partida10[[i]]<-0
    }else{
      list_inm_partida10[[i]]<-unlist(lapply(inm_partida10[[i]], function(x){x$getElementText()}))
    }
    
    
    #Valor S/
    #//*[@id="BienesInmuebles"]/div[2]/table/tbody/tr[1]/td[6]/label
    inm_valor[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[1]/td[6]/label")
    if(length(inm_valor[[i]])==0){
      list_inm_valor[[i]]<-0
    }else{
      list_inm_valor[[i]]<-unlist(lapply(inm_valor[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor2[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[2]/td[6]/label")
    if(length(inm_valor2[[i]])==0){
      list_inm_valor2[[i]]<-0
    }else{
      list_inm_valor2[[i]]<-unlist(lapply(inm_valor2[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor3[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[3]/td[6]/label")
    if(length(inm_valor3[[i]])==0){
      list_inm_valor3[[i]]<-0
    }else{
      list_inm_valor3[[i]]<-unlist(lapply(inm_valor3[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor4[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[4]/td[6]/label")
    if(length(inm_valor4[[i]])==0){
      list_inm_valor4[[i]]<-0
    }else{
      list_inm_valor4[[i]]<-unlist(lapply(inm_valor4[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor5[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[5]/td[6]/label")
    if(length(inm_valor5[[i]])==0){
      list_inm_valor5[[i]]<-0
    }else{
      list_inm_valor5[[i]]<-unlist(lapply(inm_valor5[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor6[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[6]/td[6]/label")
    if(length(inm_valor6[[i]])==0){
      list_inm_valor6[[i]]<-0
    }else{
      list_inm_valor6[[i]]<-unlist(lapply(inm_valor6[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor7[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[7]/td[6]/label")
    if(length(inm_valor7[[i]])==0){
      list_inm_valor7[[i]]<-0
    }else{
      list_inm_valor7[[i]]<-unlist(lapply(inm_valor7[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor8[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[8]/td[6]/label")
    if(length(inm_valor8[[i]])==0){
      list_inm_valor8[[i]]<-0
    }else{
      list_inm_valor8[[i]]<-unlist(lapply(inm_valor8[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor9[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[9]/td[6]/label")
    if(length(inm_valor9[[i]])==0){
      list_inm_valor9[[i]]<-0
    }else{
      list_inm_valor9[[i]]<-unlist(lapply(inm_valor9[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor10[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[10]/td[6]/label")
    if(length(inm_valor10[[i]])==0){
      list_inm_valor10[[i]]<-0
    }else{
      list_inm_valor10[[i]]<-unlist(lapply(inm_valor10[[i]], function(x){x$getElementText()}))
    }
    
    #Comentario
    #//*[@id="BienesInmuebles"]/div[2]/table/tbody/tr[1]/td[7]/label
    inm_comentario[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[1]/td[7]/label")
    if(length(inm_comentario[[i]])==0){
      list_inm_comentario[[i]]<-0
    }else{
      list_inm_comentario[[i]]<-unlist(lapply(inm_comentario[[i]], function(x){x$getElementText()}))
    }
    
    inm_comentario2[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[2]/td[7]/label")
    if(length(inm_comentario2[[i]])==0){
      list_inm_comentario2[[i]]<-0
    }else{
      list_inm_comentario2[[i]]<-unlist(lapply(inm_comentario2[[i]], function(x){x$getElementText()}))
    }
    
    inm_comentario3[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[3]/td[7]/label")
    if(length(inm_comentario3[[i]])==0){
      list_inm_comentario3[[i]]<-0
    }else{
      list_inm_comentario3[[i]]<-unlist(lapply(inm_comentario3[[i]], function(x){x$getElementText()}))
    }
    
    inm_comentario4[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[4]/td[7]/label")
    if(length(inm_comentario4[[i]])==0){
      list_inm_comentario4[[i]]<-0
    }else{
      list_inm_comentario4[[i]]<-unlist(lapply(inm_comentario4[[i]], function(x){x$getElementText()}))
    }
    
    inm_comentario5[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[5]/td[7]/label")
    if(length(inm_comentario5[[i]])==0){
      list_inm_comentario5[[i]]<-0
    }else{
      list_inm_comentario5[[i]]<-unlist(lapply(inm_comentario5[[i]], function(x){x$getElementText()}))
    }
    
    inm_comentario6[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[6]/td[7]/label")
    if(length(inm_comentario6[[i]])==0){
      list_inm_comentario6[[i]]<-0
    }else{
      list_inm_comentario6[[i]]<-unlist(lapply(inm_comentario6[[i]], function(x){x$getElementText()}))
    }
    
    inm_comentario7[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[7]/td[7]/label")
    if(length(inm_comentario7[[i]])==0){
      list_inm_comentario7[[i]]<-0
    }else{
      list_inm_comentario7[[i]]<-unlist(lapply(inm_comentario7[[i]], function(x){x$getElementText()}))
    }
    
    inm_comentario8[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[8]/td[7]/label")
    if(length(inm_comentario8[[i]])==0){
      list_inm_comentario8[[i]]<-0
    }else{
      list_inm_comentario8[[i]]<-unlist(lapply(inm_comentario8[[i]], function(x){x$getElementText()}))
    }
    
    inm_comentario9[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[9]/td[7]/label")
    if(length(inm_comentario9[[i]])==0){
      list_inm_comentario9[[i]]<-0
    }else{
      list_inm_comentario9[[i]]<-unlist(lapply(inm_comentario9[[i]], function(x){x$getElementText()}))
    }
    
    inm_comentario10[[i]]<-remDr$findElements("xpath","//*[@id='BienesInmuebles']/div[2]/table/tbody/tr[10]/td[7]/label")
    if(length(inm_comentario10[[i]])==0){
      list_inm_comentario10[[i]]<-0
    }else{
      list_inm_comentario10[[i]]<-unlist(lapply(inm_comentario10[[i]], function(x){x$getElementText()}))
    }
    
    #Total de bienes muebles (S/)
    inm_total[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[2]/div/label[2]")
    list_inm_total[[i]]<-unlist(lapply(inm_total[[i]], function(x){x$getElementText()}))
    
    #Vehículos
    #Numero
    #//*[@id="BienesMuebles"]/div[3]/table/tbody/tr[1]/td[1]
    #//*[@id="BienesMuebles"]/div[3]/table/tbody/tr/td[1]
    
    #inm_numero_vehiculo[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr/td[1]")
    
    #if(length(inm_numero_vehiculo[[i]])==0){
    # list_inm_numero_vehiculo[[i]]<-0
    #}else{
    # list_inm_numero_vehiculo[[i]]<-unlist(lapply(inm_numero_vehiculo[[i]], function(x){x$getElementText()}))
    #}
    
    
    inm_numero_vehiculo1[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[1]/td[1]")
    
    if(length(inm_numero_vehiculo1[[i]])==0){
      list_inm_numero_vehiculo1[[i]]<-0
    }else{
      list_inm_numero_vehiculo1[[i]]<-unlist(lapply(inm_numero_vehiculo1[[i]], function(x){x$getElementText()}))
    }
    
    
    inm_numero_vehiculo2[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[2]/td[1]")
    
    if(length(inm_numero_vehiculo2[[i]])==0){
      list_inm_numero_vehiculo2[[i]]<-0
    }else{
      list_inm_numero_vehiculo2[[i]]<-unlist(lapply(inm_numero_vehiculo2[[i]], function(x){x$getElementText()}))
    }
    
    inm_numero_vehiculo3[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[3]/td[1]")
    
    if(length(inm_numero_vehiculo3[[i]])==0){
      list_inm_numero_vehiculo3[[i]]<-0
    }else{
      list_inm_numero_vehiculo3[[i]]<-unlist(lapply(inm_numero_vehiculo3[[i]], function(x){x$getElementText()}))
    }
    
    inm_numero_vehiculo4[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[4]/td[1]")
    
    if(length(inm_numero_vehiculo4[[i]])==0){
      list_inm_numero_vehiculo4[[i]]<-0
    }else{
      list_inm_numero_vehiculo4[[i]]<-unlist(lapply(inm_numero_vehiculo4[[i]], function(x){x$getElementText()}))
    }
    
    
    inm_numero_vehiculo5[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[5]/td[1]")
    
    if(length(inm_numero_vehiculo5[[i]])==0){
      list_inm_numero_vehiculo5[[i]]<-0
    }else{
      list_inm_numero_vehiculo5[[i]]<-unlist(lapply(inm_numero_vehiculo5[[i]], function(x){x$getElementText()}))
    }
    
    #Vehículo
    #//*[@id="BienesMuebles"]/div[3]/table/tbody/tr[1]/td[2]/label
    
    inm_vehiculo[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr/td[2]/label")
    if(length(inm_vehiculo[[i]])==0){
      list_inm_vehiculo[[i]]<-0
    }else{
      list_inm_vehiculo[[i]]<-unlist(lapply(inm_vehiculo[[i]], function(x){x$getElementText()}))
    }
    
    inm_vehiculo1[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[1]/td[2]/label")
    if(length(inm_vehiculo1[[i]])==0){
      list_inm_vehiculo1[[i]]<-0
    }else{
      list_inm_vehiculo1[[i]]<-unlist(lapply(inm_vehiculo1[[i]], function(x){x$getElementText()}))
    }
    
    
    inm_vehiculo2[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[2]/td[2]/label")
    if(length(inm_vehiculo2[[i]])==0){
      list_inm_vehiculo2[[i]]<-0
    }else{
      list_inm_vehiculo2[[i]]<-unlist(lapply(inm_vehiculo2[[i]], function(x){x$getElementText()}))
    }
    
    inm_vehiculo3[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[3]/td[2]/label")
    if(length(inm_vehiculo3[[i]])==0){
      list_inm_vehiculo3[[i]]<-0
    }else{
      list_inm_vehiculo3[[i]]<-unlist(lapply(inm_vehiculo3[[i]], function(x){x$getElementText()}))
    }
    
    inm_vehiculo4[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[4]/td[2]/label")
    if(length(inm_vehiculo4[[i]])==0){
      list_inm_vehiculo4[[i]]<-0
    }else{
      list_inm_vehiculo4[[i]]<-unlist(lapply(inm_vehiculo4[[i]], function(x){x$getElementText()}))
    }
    
    
    inm_vehiculo5[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[5]/td[2]/label")
    if(length(inm_vehiculo5[[i]])==0){
      list_inm_vehiculo5[[i]]<-0
    }else{
      list_inm_vehiculo5[[i]]<-unlist(lapply(inm_vehiculo5[[i]], function(x){x$getElementText()}))
    }
    
    #Placa
    #//*[@id="BienesMuebles"]/div[3]/table/tbody/tr[1]/td[3]/label
    
    inm_placa_vehiculo[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr/td[3]/label")
    if(length(inm_placa_vehiculo[[i]])==0){
      list_inm_placa_vehiculo[[i]]<-0
    }else{
      list_inm_placa_vehiculo[[i]]<-unlist(lapply(inm_placa_vehiculo[[i]], function(x){x$getElementText()}))
    }
    
    
    inm_placa_vehiculo1[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[1]/td[3]/label")
    if(length(inm_placa_vehiculo1[[i]])==0){
      list_inm_placa_vehiculo1[[i]]<-0
    }else{
      list_inm_placa_vehiculo1[[i]]<-unlist(lapply(inm_placa_vehiculo1[[i]], function(x){x$getElementText()}))
    }
    
    inm_placa_vehiculo2[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[2]/td[3]/label")
    if(length(inm_placa_vehiculo2[[i]])==0){
      list_inm_placa_vehiculo2[[i]]<-0
    }else{
      list_inm_placa_vehiculo2[[i]]<-unlist(lapply(inm_placa_vehiculo2[[i]], function(x){x$getElementText()}))
    }
    
    inm_placa_vehiculo3[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[3]/td[3]/label")
    if(length(inm_placa_vehiculo3[[i]])==0){
      list_inm_placa_vehiculo3[[i]]<-0
    }else{
      list_inm_placa_vehiculo3[[i]]<-unlist(lapply(inm_placa_vehiculo3[[i]], function(x){x$getElementText()}))
    }
    
    inm_placa_vehiculo4[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[4]/td[3]/label")
    if(length(inm_placa_vehiculo4[[i]])==0){
      list_inm_placa_vehiculo4[[i]]<-0
    }else{
      list_inm_placa_vehiculo4[[i]]<-unlist(lapply(inm_placa_vehiculo4[[i]], function(x){x$getElementText()}))
    }
    
    inm_placa_vehiculo5[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[5]/td[3]/label")
    if(length(inm_placa_vehiculo5[[i]])==0){
      list_inm_placa_vehiculo5[[i]]<-0
    }else{
      list_inm_placa_vehiculo5[[i]]<-unlist(lapply(inm_placa_vehiculo5[[i]], function(x){x$getElementText()}))
    }
    
    #Caracteristicas
    #//*[@id="BienesMuebles"]/div[3]/table/tbody/tr/td[4]/label
    
    
    inm_carac_vehiculo[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr/td[4]/label")
    if(length(inm_carac_vehiculo[[i]])==0){
      list_inm_carac_vehiculo[[i]]<-0
    }else{
      list_inm_carac_vehiculo[[i]]<-unlist(lapply(inm_carac_vehiculo[[i]], function(x){x$getElementText()}))
    }
    
    inm_carac_vehiculo1[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[1]/td[4]/label")
    if(length(inm_carac_vehiculo1[[i]])==0){
      list_inm_carac_vehiculo1[[i]]<-0
    }else{
      list_inm_carac_vehiculo1[[i]]<-unlist(lapply(inm_carac_vehiculo1[[i]], function(x){x$getElementText()}))
    }
    
    inm_carac_vehiculo2[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[2]/td[4]/label")
    if(length(inm_carac_vehiculo2[[i]])==0){
      list_inm_carac_vehiculo2[[i]]<-0
    }else{
      list_inm_carac_vehiculo2[[i]]<-unlist(lapply(inm_carac_vehiculo2[[i]], function(x){x$getElementText()}))
    }
    
    inm_carac_vehiculo3[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[3]/td[4]/label")
    if(length(inm_carac_vehiculo3[[i]])==0){
      list_inm_carac_vehiculo3[[i]]<-0
    }else{
      list_inm_carac_vehiculo3[[i]]<-unlist(lapply(inm_carac_vehiculo3[[i]], function(x){x$getElementText()}))
    }
    
    inm_carac_vehiculo4[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[4]/td[4]/label")
    if(length(inm_carac_vehiculo4[[i]])==0){
      list_inm_carac_vehiculo4[[i]]<-0
    }else{
      list_inm_carac_vehiculo4[[i]]<-unlist(lapply(inm_carac_vehiculo4[[i]], function(x){x$getElementText()}))
    }
    
    inm_carac_vehiculo5[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[5]/td[4]/label")
    if(length(inm_carac_vehiculo5[[i]])==0){
      list_inm_carac_vehiculo5[[i]]<-0
    }else{
      list_inm_carac_vehiculo5[[i]]<-unlist(lapply(inm_carac_vehiculo5[[i]], function(x){x$getElementText()}))
    }
    
    
    #Valor
    #//*[@id="BienesMuebles"]/div[3]/table/tbody/tr/td[5]/label
    inm_valor_vehiculo[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr/td[5]/label")
    if(length(inm_valor_vehiculo[[i]])==0){
      list_inm_valor_vehiculo[[i]]<-0
    }else{
      list_inm_valor_vehiculo[[i]]<-unlist(lapply(inm_valor_vehiculo[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor_vehiculo1[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[1]/td[5]/label")
    if(length(inm_valor_vehiculo1[[i]])==0){
      list_inm_valor_vehiculo1[[i]]<-0
    }else{
      list_inm_valor_vehiculo1[[i]]<-unlist(lapply(inm_valor_vehiculo1[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor_vehiculo2[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[2]/td[5]/label")
    if(length(inm_valor_vehiculo2[[i]])==0){
      list_inm_valor_vehiculo2[[i]]<-0
    }else{
      list_inm_valor_vehiculo2[[i]]<-unlist(lapply(inm_valor_vehiculo2[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor_vehiculo3[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[3]/td[5]/label")
    if(length(inm_valor_vehiculo3[[i]])==0){
      list_inm_valor_vehiculo3[[i]]<-0
    }else{
      list_inm_valor_vehiculo3[[i]]<-unlist(lapply(inm_valor_vehiculo3[[i]], function(x){x$getElementText()}))
    }
    
    inm_valor_vehiculo4[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[4]/td[5]/label")
    if(length(inm_valor_vehiculo4[[i]])==0){
      list_inm_valor_vehiculo4[[i]]<-0
    }else{
      list_inm_valor_vehiculo4[[i]]<-unlist(lapply(inm_valor_vehiculo4[[i]], function(x){x$getElementText()}))
    } 
    
    inm_valor_vehiculo5[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[5]/td[5]/label")
    if(length(inm_valor_vehiculo5[[i]])==0){
      list_inm_valor_vehiculo5[[i]]<-0
    }else{
      list_inm_valor_vehiculo5[[i]]<-unlist(lapply(inm_valor_vehiculo5[[i]], function(x){x$getElementText()}))
    } 
    
    
    #Comentario
    #//*[@id="BienesMuebles"]/div[3]/table/tbody/tr/td[6]/label
    inm_com_vehiculo[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr/td[6]/label")
    if(length(inm_com_vehiculo[[i]])==0){
      list_inm_com_vehiculo[[i]]<-0
    }else{
      list_inm_com_vehiculo[[i]]<-unlist(lapply(inm_com_vehiculo[[i]], function(x){x$getElementText()}))
    }
    
    inm_com_vehiculo1[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[1]/td[6]/label")
    if(length(inm_com_vehiculo1[[i]])==0){
      list_inm_com_vehiculo1[[i]]<-0
    }else{
      list_inm_com_vehiculo1[[i]]<-unlist(lapply(inm_com_vehiculo1[[i]], function(x){x$getElementText()}))
    }
    
    inm_com_vehiculo2[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[2]/td[6]/label")
    if(length(inm_com_vehiculo2[[i]])==0){
      list_inm_com_vehiculo2[[i]]<-0
    }else{
      list_inm_com_vehiculo2[[i]]<-unlist(lapply(inm_com_vehiculo2[[i]], function(x){x$getElementText()}))
    }
    
    inm_com_vehiculo3[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[3]/td[6]/label")
    if(length(inm_com_vehiculo3[[i]])==0){
      list_inm_com_vehiculo3[[i]]<-0
    }else{
      list_inm_com_vehiculo3[[i]]<-unlist(lapply(inm_com_vehiculo3[[i]], function(x){x$getElementText()}))
    }
    
    inm_com_vehiculo4[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[4]/td[6]/label")
    if(length(inm_com_vehiculo4[[i]])==0){
      list_inm_com_vehiculo4[[i]]<-0
    }else{
      list_inm_com_vehiculo4[[i]]<-unlist(lapply(inm_com_vehiculo4[[i]], function(x){x$getElementText()}))
    }
    
    
    inm_com_vehiculo5[[i]]<-remDr$findElements("xpath","//*[@id='BienesMuebles']/div[3]/table/tbody/tr[5]/td[6]/label")
    if(length(inm_com_vehiculo5[[i]])==0){
      list_inm_com_vehiculo5[[i]]<-0
    }else{
      list_inm_com_vehiculo5[[i]]<-unlist(lapply(inm_com_vehiculo5[[i]], function(x){x$getElementText()}))
    }
    
    
    #Crear un dataframe del candidato
    df_candidatos<-cbind(unlist(list_dni[[i]]),unlist(list_sexo[[i]]),unlist(list_ap_paterno[[i]]),
                         unlist(list_ap_materno[[i]]),unlist(list_nombres[[i]]),unlist(list_fecha_nacimiento[[i]]),unlist(list_pais[[i]]),
                         unlist(list_dpto[[i]]),unlist(list_prov[[i]]),
                         unlist(list_estud_prim[[i]]),unlist(list_estud_prim_concl[[i]]),unlist(list_estud_sec[[i]]),unlist(list_estud_sec_concl[[i]]),
                         unlist(list_estud_tec[[i]]),unlist(list_nombre_tec[[i]]),unlist(list_nombre_tec_carrera[[i]]),
                         unlist(list_nombre_tec_carrera_concl[[i]]),unlist(list_nombre_tec_carrera_com[[i]]),unlist(list_estud_no_univ[[i]]),
                         unlist(list_nombre_no_univ[[i]]),unlist(list_nombr_no_univ_carrera[[i]]),unlist(list_nombre_no_univ_carrera_concl[[i]]),
                         unlist(list_estud_univ[[i]]),unlist(list_nombre_univ[[i]]),unlist(list_nombre_univ_carrera_concl[[i]]),
                         unlist(list_nombre_univ_carrera_grado[[i]]),unlist(list_nombre_univ_carrera_egresado[[i]]),unlist(list_nombre_univ_carrera_año[[i]]),
                         unlist(list_nombre_univ_carrera_comentario[[i]]),unlist(list_estud_post[[i]]),unlist(list_nombre_post[[i]]),
                         unlist(list_nombre_post_carrera_grado[[i]]),unlist(list_nombre_post_carrera_concl[[i]]),unlist(list_nombre_post_carrera_egresado[[i]]),
                         unlist(list_nombre_post_carrera_maestro[[i]]),unlist(list_nombre_post_carrera_doctor[[i]]),unlist(list_nombre_post_carrera_año[[i]]),
                         unlist(list_nombre_post_carrera_coment[[i]]),unlist(list_org_pol[[i]]),unlist(list_fecha[[i]]),unlist(list_delito[[i]]),
                         unlist(list_falla1[[i]]),unlist(list_mat_demanda[[i]]),unlist(list_nro_expediente[[i]]),
                         unlist(list_juzg_penal[[i]]),unlist(list_falla2[[i]]),unlist(list_rba_publico[[i]]),unlist(list_rba_privado[[i]]),unlist(list_rba_total[[i]]),unlist(list_rba_publico_ind[[i]]),
                         unlist(list_rba_privado_ind[[i]]),unlist(list_rba_total_ind[[i]]),unlist(list_oia_publico[[i]]),unlist(list_oia_privado[[i]]),unlist(list_oia_otros[[i]]),unlist(list_total_ingresos[[i]]),
                         unlist(list_inm_numero[[i]]), unlist(list_inm_numero2[[i]]) ,unlist(list_inm_numero3[[i]]), unlist(list_inm_numero4[[i]]),
                         unlist(list_inm_numero5[[i]]), unlist(list_inm_numero6[[i]]),unlist(list_inm_numero7[[i]]),unlist(list_inm_numero8[[i]]),
                         unlist(list_inm_numero9[[i]]),unlist(list_inm_numero10[[i]]),unlist(list_inm_bien[[i]]),unlist(list_inm_bien2[[i]]),unlist(list_inm_bien3[[i]]),
                         unlist(list_inm_bien4[[i]]),unlist(list_inm_bien5[[i]]),unlist(list_inm_bien6[[i]]),unlist(list_inm_bien7[[i]]),unlist(list_inm_bien8[[i]]),unlist(list_inm_bien9[[i]]),
                         unlist(list_inm_bien10[[i]]),unlist(list_inm_direccion[[i]]),unlist(list_inm_direccion2[[i]]),unlist(list_inm_direccion3[[i]]),unlist(list_inm_direccion4[[i]]),
                         unlist(list_inm_direccion5[[i]]),unlist(list_inm_direccion6[[i]]),unlist(list_inm_direccion7[[i]]),unlist(list_inm_direccion8[[i]]),
                         unlist(list_inm_direccion9[[i]]),unlist(list_inm_direccion10[[i]]),unlist(list_inm_sunarp[[i]]),unlist(list_inm_sunarp2[[i]]),
                         unlist(list_inm_sunarp3[[i]]),unlist(list_inm_sunarp4[[i]]),unlist(list_inm_sunarp5[[i]]),unlist(list_inm_sunarp6[[i]]),
                         unlist(list_inm_sunarp7[[i]]),unlist(list_inm_sunarp8[[i]]),unlist(list_inm_sunarp9[[i]]),unlist(list_inm_sunarp10[[i]]),
                         unlist(list_inm_partida[[i]]),unlist(list_inm_partida2[[i]]),unlist(list_inm_partida3[[i]]),unlist(list_inm_partida4[[i]]),unlist(list_inm_partida5[[i]]),
                         unlist(list_inm_partida6[[i]]),unlist(list_inm_partida7[[i]]),unlist(list_inm_partida8[[i]]),unlist(list_inm_partida9[[i]]),unlist(list_inm_partida10[[i]]),
                         unlist(list_inm_valor[[i]]), unlist(list_inm_valor2[[i]]), unlist(list_inm_valor3[[i]]), unlist(list_inm_valor4[[i]]), unlist(list_inm_valor5[[i]]),
                         unlist(list_inm_valor6[[i]]), unlist(list_inm_valor7[[i]]), unlist(list_inm_valor8[[i]]), unlist(list_inm_valor9[[i]]), unlist(list_inm_valor10[[i]]),
                         unlist(list_inm_comentario[[i]]),unlist(list_inm_comentario2[[i]]),unlist(list_inm_comentario3[[i]]),unlist(list_inm_comentario4[[i]]),
                         unlist(list_inm_comentario5[[i]]),unlist(list_inm_comentario6[[i]]),unlist(list_inm_comentario7[[i]]),unlist(list_inm_comentario8[[i]]),
                         unlist(list_inm_comentario9[[i]]),unlist(list_inm_comentario10[[i]]),unlist(list_inm_total[[i]]),
                         unlist(list_inm_numero_vehiculo1[[i]]),unlist(list_inm_numero_vehiculo2[[i]]),unlist(list_inm_numero_vehiculo3[[i]]),
                         unlist(list_inm_numero_vehiculo4[[i]]),unlist(list_inm_numero_vehiculo5[[i]]),unlist(list_inm_vehiculo[[i]]),unlist(list_inm_vehiculo1[[i]]),
                         unlist(list_inm_vehiculo2[[i]]),unlist(list_inm_vehiculo3[[i]]),unlist(list_inm_vehiculo4[[i]]),unlist(list_inm_vehiculo5[[i]]),
                         unlist(list_inm_placa_vehiculo[[i]]),unlist(list_inm_placa_vehiculo1[[i]]),unlist(list_inm_placa_vehiculo2[[i]]),unlist(list_inm_placa_vehiculo3[[i]]),
                         unlist(list_inm_placa_vehiculo4[[i]]),unlist(list_inm_placa_vehiculo5[[i]]),unlist(list_inm_carac_vehiculo[[i]]),unlist(list_inm_carac_vehiculo1[[i]]),
                         unlist(list_inm_carac_vehiculo2[[i]]),unlist(list_inm_carac_vehiculo3[[i]]),unlist(list_inm_carac_vehiculo4[[i]]),unlist(list_inm_carac_vehiculo5[[i]]),
                         unlist(list_inm_valor_vehiculo[[i]]),unlist(list_inm_valor_vehiculo1[[i]]),unlist(list_inm_valor_vehiculo2[[i]]),unlist(list_inm_valor_vehiculo3[[i]]),
                         unlist(list_inm_valor_vehiculo4[[i]]),unlist(list_inm_valor_vehiculo5[[i]]),unlist(list_inm_com_vehiculo[[i]]),unlist(list_inm_com_vehiculo1[[i]]),
                         unlist(list_inm_com_vehiculo2[[i]]),unlist(list_inm_com_vehiculo3[[i]]),unlist(list_inm_com_vehiculo4[[i]]),unlist(list_inm_com_vehiculo5[[i]])
                         
    )
    
    df_candidatos<-as.data.frame(df_candidatos)
    total_1[[i]]<-df_candidatos
    
    
    
    
    #Cerramos la ventana de la hoja de vida del candidato
    remDr$closeWindow()
    
    testit(4)
    
    #Regresamos a la página donde están los candidatos
    remDr$switchToWindow(allWindows[[1]])
    
    testit(4)
    
    #Creamos este loop para que cuando se llegue al total de candidatos total por partido se pase automáticamente
    #al siguiente partido político
    if(count2==can){
      
      for(j in 1:25){
        #Guaradmos en una lista de listas la información de los candidatos al congreso de cada partido
        #Obtendremos 25 listas en las cuales se tiene tantas listas como candidatos tiene el partido
        total_listas[[j]]<-list(total_0,total_1,total_laboral)
        
        numero_candidato<-list()
        list_numero_candidato<-list()
        puesto<-list()
        list_puesto<-list()
        exp<-list()
        list_exp<-list()
        est<-list()
        list_est<-list()
        distr_elect<-list()
        list_distr_elect<-list()
        total_0<-list()
        
        dni<-list()
        list_dni<-list()
        sexo<-list()
        list_sexo<-list()
        ap_paterno<-list()
        list_ap_paterno<-list()
        ap_materno<-list()
        list_ap_materno<-list()
        nombres<-list()
        list_nombres<-list()
        fecha_nacimiento<-list()
        list_fecha_nacimiento<-list()
        pais<-list()
        list_pais<-list()
        dpto<-list()
        list_dpto<-list()
        prov<-list()
        list_prov<-list()
        org_pol<-list()
        list_org_pol<-list()
        fecha<-list()
        list_fecha<-list()
        delito<-list()
        list_delito<-list()
        falla1<-list()
        list_falla1<-list()
        mat_demanda<-list()
        list_mat_demanda<-list()
        nro_expediente<-list()
        list_nro_expediente<-list()
        juzg_penal<-list()
        list_juzg_penal<-list()
        falla2<-list()
        list_falla2<-list()
        total_1<-list()
        
        estud_prim<-list()
        list_estud_prim<-list()
        estud_prim_concl<-list()
        list_estud_prim_concl<-list()
        estud_sec<-list()
        list_estud_sec<-list()
        estud_sec_concl<-list()
        list_estud_sec_concl<-list()
        estud_tec<-list()
        list_estud_tec<-list()
        nombre_tec<-list()
        list_nombre_tec<-list()
        nombre_tec_carrera<-list()
        list_nombre_tec_carrera<-list()
        nombre_tec_carrera_concl<-list()
        list_nombre_tec_carrera_concl<-list()
        nombre_tec_carrera_com<-list()
        list_nombre_tec_carrera_com<-list()
        estud_no_univ<-list()
        list_estud_no_univ<-list()
        nombre_no_univ<-list()
        list_nombre_no_univ<-list()
        nombre_no_univ_carrera<-list()
        list_nombr_no_univ_carrera<-list()
        nombre_no_univ_carrera_concl<-list()
        list_nombre_no_univ_carrera_concl<-list()
        estud_univ<-list()
        list_estud_univ<-list()
        nombre_univ<-list()
        list_nombre_univ<-list()
        nombre_univ_carrera_concl<-list()
        list_nombre_univ_carrera_concl<-list()
        nombre_univ_carrera_grado<-list()
        list_nombre_univ_carrera_grado<-list()
        nombre_univ_carrera_egresado<-list()
        list_nombre_univ_carrera_egresado<-list()
        nombre_univ_carrera_año<-list()
        list_nombre_univ_carrera_año<-list()
        nombre_univ_carrera_comentario<-list()
        list_nombre_univ_carrera_comentario<-list()
        estud_post<-list()
        list_estud_post<-list()
        nombre_post<-list()
        list_nombre_post<-list()
        nombre_post_carrera_grado<-list()
        list_nombre_post_carrera_grado<-list()
        nombre_post_carrera_concl<-list()
        list_nombre_post_carrera_concl<-list()
        nombre_post_carrera_egresado<-list()
        list_nombre_post_carrera_egresado<-list()
        nombre_post_carrera_maestro<-list()
        list_nombre_post_carrera_maestro<-list()
        nombre_post_carrera_doctor<-list()
        list_nombre_post_carrera_doctor<-list()
        nombre_post_carrera_año<-list()
        list_nombre_post_carrera_año<-list()
        nombre_post_carrera_coment<-list()
        list_nombre_post_carrera_coment<-list()
        
        nombre_centro_labor<-list()
        list_nombre_centro_labor<-list()
        nombre_oficio_labor<-list()
        list_nombre_oficio_labor<-list()
        nombre_ruc_labor<-list()
        list_nombre_ruc_labor<-list()
        nombre_direccion_labor<-list()
        list_nombre_direccion_labor<-list()
        nombre_desde_labor<-list()
        list_nombre_desde_labor<-list()
        nombre_hasta_labor<-list()
        list_nombre_hasta_labor<-list()
        nombre_pais_labor<-list()
        list_nombre_pais_labor<-list()
        nombre_dpto_labor<-list()
        list_nombre_dpto_labor<-list()
        nombre_prov_labor<-list()
        list_nombre_prov_labor<-list()
        nombre_dist_labor<-list()
        list_nombre_dist_labor<-list()
        total_laboral<-list()
        
        rba_publico<-list()
        list_rba_publico<-list()
        rba_privado<-list()
        list_rba_privado<-list()
        rba_total<-list()
        list_rba_total<-list()
        rba_publico_ind<-list()
        list_rba_publico_ind<-list()
        rba_privado_ind<-list()
        list_rba_privado_ind<-list()
        rba_total_ind<-list()
        list_rba_total_ind<-list()
        oia_publico<-list()
        list_oia_publico<-list()
        oia_privado<-list()
        list_oia_privado<-list()
        oia_otros<-list()
        list_oia_otros<-list()
        total_ingresos<-list()
        list_total_ingresos<-list()
        
        
        inm_numero<-list()
        list_inm_numero<-list()
        
        inm_numero2<-list()
        list_inm_numero2<-list()
        
        inm_numero3<-list()
        list_inm_numero3<-list()
        
        inm_numero4<-list()
        list_inm_numero4<-list()
        
        inm_numero5<-list()
        list_inm_numero5<-list()
        
        inm_numero6<-list()
        list_inm_numero6<-list()
        
        inm_numero7<-list()
        list_inm_numero7<-list()
        
        inm_numero8<-list()
        list_inm_numero8<-list()
        
        inm_numero9<-list()
        list_inm_numero9<-list()
        
        inm_numero10<-list()
        list_inm_numero10<-list()
        
        
        inm_bien<-list()
        list_inm_bien<-list()
        
        inm_bien2<-list()
        list_inm_bien2<-list()
        
        inm_bien3<-list()
        list_inm_bien3<-list()
        
        inm_bien4<-list()
        list_inm_bien4<-list()
        
        inm_bien5<-list()
        list_inm_bien5<-list()
        
        inm_bien6<-list()
        list_inm_bien6<-list()
        
        inm_bien7<-list()
        list_inm_bien7<-list()
        
        inm_bien8<-list()
        list_inm_bien8<-list()
        
        inm_bien9<-list()
        list_inm_bien9<-list()
        
        inm_bien10<-list()
        list_inm_bien10<-list()
        
        
        inm_direccion<-list()
        list_inm_direccion<-list()
        
        inm_direccion2<-list()
        list_inm_direccion2<-list()
        
        inm_direccion3<-list()
        list_inm_direccion3<-list()
        
        inm_direccion4<-list()
        list_inm_direccion4<-list()
        
        inm_direccion5<-list()
        list_inm_direccion5<-list()
        
        inm_direccion6<-list()
        list_inm_direccion6<-list()
        
        inm_direccion7<-list()
        list_inm_direccion7<-list()
        
        inm_direccion8<-list()
        list_inm_direccion8<-list()
        
        inm_direccion9<-list()
        list_inm_direccion9<-list()
        
        inm_direccion10<-list()
        list_inm_direccion10<-list()
        
        
        inm_sunarp<-list()
        list_inm_sunarp<-list()
        
        inm_sunarp2<-list()
        list_inm_sunarp2<-list()
        
        inm_sunarp3<-list()
        list_inm_sunarp3<-list()
        
        inm_sunarp4<-list()
        list_inm_sunarp4<-list()
        
        inm_sunarp5<-list()
        list_inm_sunarp5<-list()
        
        inm_sunarp6<-list()
        list_inm_sunarp6<-list()
        
        inm_sunarp7<-list()
        list_inm_sunarp7<-list()
        
        inm_sunarp8<-list()
        list_inm_sunarp8<-list()
        
        inm_sunarp9<-list()
        list_inm_sunarp9<-list()
        
        inm_sunarp10<-list()
        list_inm_sunarp10<-list()
        
        
        inm_partida<-list()
        list_inm_partida<-list()
        
        inm_partida2<-list()
        list_inm_partida2<-list()
        
        inm_partida3<-list()
        list_inm_partida3<-list()
        
        inm_partida4<-list()
        list_inm_partida4<-list()
        
        inm_partida5<-list()
        list_inm_partida5<-list()
        
        inm_partida6<-list()
        list_inm_partida6<-list()
        
        inm_partida7<-list()
        list_inm_partida7<-list()
        
        inm_partida8<-list()
        list_inm_partida8<-list()
        
        inm_partida9<-list()
        list_inm_partida9<-list()
        
        inm_partida10<-list()
        list_inm_partida10<-list()
        
        
        inm_valor<-list()
        list_inm_valor<-list()
        
        inm_valor2<-list()
        list_inm_valor2<-list()
        
        inm_valor3<-list()
        list_inm_valor3<-list()
        
        inm_valor4<-list()
        list_inm_valor4<-list()
        
        inm_valor5<-list()
        list_inm_valor5<-list()
        
        inm_valor6<-list()
        list_inm_valor6<-list()
        
        inm_valor7<-list()
        list_inm_valor7<-list()
        
        inm_valor8<-list()
        list_inm_valor8<-list()
        
        inm_valor9<-list()
        list_inm_valor9<-list()
        
        inm_valor10<-list()
        list_inm_valor10<-list()
        
        
        inm_comentario<-list()
        list_inm_comentario<-list()
        
        inm_comentario2<-list()
        list_inm_comentario2<-list()
        
        inm_comentario3<-list()
        list_inm_comentario3<-list()
        
        inm_comentario4<-list()
        list_inm_comentario4<-list()
        
        inm_comentario5<-list()
        list_inm_comentario5<-list()
        
        inm_comentario6<-list()
        list_inm_comentario6<-list()
        
        inm_comentario7<-list()
        list_inm_comentario7<-list()
        
        inm_comentario8<-list()
        list_inm_comentario8<-list()
        
        inm_comentario9<-list()
        list_inm_comentario9<-list()
        
        inm_comentario10<-list()
        list_inm_comentario10<-list()
        
        
        inm_total<-list()
        list_inm_total<-list()
        
        
        inm_numero_vehiculo<-list()
        list_inm_numero_vehiculo<-list()
        
        inm_numero_vehiculo1<-list()
        list_inm_numero_vehiculo1<-list()
        
        inm_numero_vehiculo2<-list()
        list_inm_numero_vehiculo2<-list()
        
        inm_numero_vehiculo3<-list()
        list_inm_numero_vehiculo3<-list()
        
        inm_numero_vehiculo4<-list()
        list_inm_numero_vehiculo4<-list()
        
        inm_numero_vehiculo5<-list()
        list_inm_numero_vehiculo5<-list()
        
        
        inm_vehiculo<-list()
        list_inm_vehiculo<-list()
        
        inm_vehiculo1<-list()
        list_inm_vehiculo1<-list()
        
        inm_vehiculo2<-list()
        list_inm_vehiculo2<-list()
        
        inm_vehiculo3<-list()
        list_inm_vehiculo3<-list()
        
        inm_vehiculo4<-list()
        list_inm_vehiculo4<-list()
        
        inm_vehiculo5<-list()
        list_inm_vehiculo5<-list()
        
        inm_placa_vehiculo<-list()
        list_inm_placa_vehiculo<-list()
        
        inm_placa_vehiculo1<-list()
        list_inm_placa_vehiculo1<-list()
        
        inm_placa_vehiculo2<-list()
        list_inm_placa_vehiculo2<-list()
        
        inm_placa_vehiculo3<-list()
        list_inm_placa_vehiculo3<-list()
        
        inm_placa_vehiculo4<-list()
        list_inm_placa_vehiculo4<-list()
        
        inm_placa_vehiculo5<-list()
        list_inm_placa_vehiculo5<-list()
        
        inm_carac_vehiculo<-list()
        list_inm_carac_vehiculo<-list()
        
        inm_carac_vehiculo1<-list()
        list_inm_carac_vehiculo1<-list()
        
        inm_carac_vehiculo2<-list()
        list_inm_carac_vehiculo2<-list()
        
        inm_carac_vehiculo3<-list()
        list_inm_carac_vehiculo3<-list()
        
        inm_carac_vehiculo4<-list()
        list_inm_carac_vehiculo4<-list()
        
        inm_carac_vehiculo5<-list()
        list_inm_carac_vehiculo5<-list()
        
        inm_valor_vehiculo<-list()
        list_inm_valor_vehiculo<-list()
        
        inm_valor_vehiculo1<-list()
        list_inm_valor_vehiculo1<-list()
        
        inm_valor_vehiculo2<-list()
        list_inm_valor_vehiculo2<-list()
        
        inm_valor_vehiculo3<-list()
        list_inm_valor_vehiculo3<-list()
        
        inm_valor_vehiculo4<-list()
        list_inm_valor_vehiculo4<-list()
        
        inm_valor_vehiculo5<-list()
        list_inm_valor_vehiculo5<-list()
        
        inm_com_vehiculo<-list()
        list_inm_com_vehiculo<-list()
        
        inm_com_vehiculo1<-list()
        list_inm_com_vehiculo1<-list()
        
        inm_com_vehiculo2<-list()
        list_inm_com_vehiculo2<-list()
        
        inm_com_vehiculo3<-list()
        list_inm_com_vehiculo3<-list()
        
        inm_com_vehiculo4<-list()
        list_inm_com_vehiculo4<-list()
        
        inm_com_vehiculo5<-list()
        list_inm_com_vehiculo5<-list()
        
        df_candidatos<-data.frame()
        df_0<-data.frame()
        df_laboral<-data.frame()
      }
      
      break
      
    }else{
      print("sigue")
    }
    
  }
  
  
}


#Cerrar Conexion
remDr$close()
