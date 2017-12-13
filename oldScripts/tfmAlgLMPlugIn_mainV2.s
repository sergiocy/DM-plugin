/*
SCRIPT TFM (12/08/2015)
leer/escribir datos recibidos/enviados 
por un txt 
*/



/* *************************************
--- VARIABLES GLOBALES
************************************ */
//variables para la imagen original
image img
imageDisplay imgDisp 

//variable para la ROI seleccionada
image imgROI
imageDisplay imgROIDisp

//variable para la ``imgROI'' ajustada
image imgROIAjustada

//...definimos las rutas para los archivos de texto que usaremos...
string archivoDatosOUT = "C:/Users/Usuario/Desktop/plugInDM/plugInDevelopingV3/datosOriginales.txt"
string archivoEstimOUT = "C:/Users/Usuario/Desktop/plugInDM/plugInDevelopingV3/estimOriginales.txt"
string archivoDatosIN = "C:/Users/Usuario/Desktop/plugInDM/plugInDevelopingV3/datosAjustados.txt"
string archivoEstimIN = "C:/Users/Usuario/Desktop/plugInDM/plugInDevelopingV3/estimAjustados.txt"
string archivoDatosINColumna = "C:/Users/Usuario/Desktop/plugInDM/plugInDevelopingV3/ColumnaDatosImgDM_IN.txt"
string archivoEstimINColumna = "C:/Users/Usuario/Desktop/plugInDM/plugInDevelopingV3/ColumnaEstimImgDM_IN.txt"
string archivoErrorEstimIN = "C:/Users/Usuario/Desktop/plugInDM/plugInDevelopingV3/errorEstimAjustados.txt"
string archivoErrorEstimINColumna = "C:/Users/Usuario/Desktop/plugInDM/plugInDevelopingV3/ColumnaErrorEstimImgDM_IN.txt"
string archivoResultados = "C:/Users/Usuario/Desktop/plugInDM/plugInDevelopingV3/estimadores_IN.txt"

// definimos el numero de normales como variable global
number nNormales = 0
// y el origen de la ROI que ajustaremos
number origenROI_x, origenROI_y
number marcoROISup, marcoROIInf, marcoROIIzda, marcoROIDcha
//creamos una lista para las manchas
object listaManchas = alloc(ObjectList)
//...y una variable para las selecciones para estimadores iniciales
string cadenaEstimadoresIni = ""

//ids para los listeners que usaremos
number idListenerEstimaciones
number idListenerResultados
/* ******************************************* */



/* ***************************************
--- FUNCION PARA SELECCIONAR ROI DE INTERES
*************************************** */
void seleccionaROI(){
	//asignamos la ROI a la variable (tipo image) global. 
	//De ella obtendremos los datos de cada pixel para el procesado
	imgROI = img[]
	
	//variables para definir la ROI seleccionada y su limite/marco
	ROI ROISelec = newROI()
	
	if(imgDisp.imageDisplayCountROIs()==0 || imgDisp.imageDisplayCountROIs()>1){
		if(imgDisp.imageDisplayCountROIs()==0){
			number nPix_x, nPix_y
			getSize(img,nPix_x,nPix_y)
			ROISelec.ROISetRectangle(0,0,nPix_y-1,nPix_x-1)
		}
		if(imgDisp.imageDisplayCountROIs()>1){
			okDialog("selecciona UNA sola ROI para el ajuste")
			exit(0)
		}	
	}
	else{
		ROISelec = imageDisplayGetROI(imgDisp,0)
	}
	
	ROISelec.ROIGetRectangle(marcoROISup,marcoROIIzda,marcoROIInf,marcoROIDcha)
	ROISelec.ROIGetVertex(0,origenROI_x,origenROI_y)
	
	//presentamos la ROI seleccionda como nueva imagen... */
	showImage(imgROI)
	imgROIDisp = imgROI.imagegetimagedisplay(0)
	setname(imgROI, "ORIGEN: ("+"col "+origenROI_x+", fil "+origenROI_y+") "+"; MARCO: "+"sup "+marcoROISup+", inf "+marcoROIInf+", izda "+marcoROIIzda+", dcha "+marcoROIDcha)
	
}//cierre de ``seleccionaROI'' 
/* **************************************** */


/* ****************************************************
--- FUNCION PARA VISUALIZAR IMAGEN AMPLIADA
************************************************ */
void visualizaROI(image imgLocal){
	//tamanio de la zona de visualizacion 
	number pantallaAncho, pantallaAlto
	//tamanio de la ventana
	number ventanax, ventanay
	//posicion de la ventana
	number ventanaPosx=10, ventanaPosy=25
	
	GetScreenSize(pantallaAncho, pantallaAlto )
	
	showImage(imgLocal)
	
	setWindowPosition(imgLocal,ventanaPosx, ventanaPosy)
	setWindowSize(imgLocal,pantallaAncho-(2*ventanaPosx), pantallaAlto-(2*ventanaPosy))
}


/* **************************************
--- FUNCION PARA ESCRIBIR DATOS DE LA IMAGEN EN FICHERO
**************************************** */
void escribeImg(image imgLocal){
	number refArchivoEscritura = CreateFileForWriting( archivoDatosOUT )
	number nPixX,nPixY
	number nCol,nFil
	getSize(imgLocal,nPixX,nPixY)
	for(nFil=0;nFil<nPixY;nFil++){
		for(nCol=0;nCol<nPixX;nCol++){
			writeFile( refArchivoEscritura, ""+getPixel(imgLocal,nCol,nFil)+" " )
			if(nCol==(nPixX-1)){
				writeFile( refArchivoEscritura, "\n")
			}
		}
	}	
	
	closeFile( refArchivoEscritura )
}//cierre de la funcion ``escribeImg''
/* ****************************************** */	



/* *******************************************
--- FUNCION PARA LANZAR EL SCRIPT EN R
**************************************** */
void lanzaR(){
	if(twoButtonDialog("Confirmar lanzamiento de algoritmo de ajuste.","continuar","cancelar")){
		
		//string llamada="cmd /c Rscript E:/Usuarios/Instala/Desktop/plugInDM/algoritmoLM_V5_DM_Definitivo.R"
		string llamada="cmd /c Rscript C:/Users/Usuario/Desktop/plugInDM/plugInDevelopingV3/scripts/algoritmoLM_V5_DM_Definitivo.R"
		launchExternalProcess(llamada)
		
	}
	else{
		if(doesFileExist(archivoDatosOUT)==1){
			deleteFile( archivoDatosOUT )
		}
		if(doesFileExist(archivoEstimOUT)==1){
			deleteFile( archivoEstimOUT )
		}
		if(doesFileExist(archivoDatosIN)==1){
			deleteFile( archivoDatosIN )
		}
		if(doesFileExist(archivoResultados)==1){
			deleteFile( archivoResultados )
		}
		exit(0)
	}
	
}//cierre de ``lanzaR()''
/* **************************************** */



/* ******************************************
--- FUNCION PARA LEER DATOS DE IMAGEN GENERADOS (CON R)
******************************************** */
void leeImg(){
	//definimos dimensiones de la imagen ajustada: iguales que las de la ROI seleccionada
	number sizex, sizey
	getSize(imgROI,sizex,sizey)
	image imgAjuste := RealImage("ORIGEN: ("+"col "+origenROI_x+", fil "+origenROI_y+") "+"; MARCO: "+"sup "+marcoROISup+", inf "+marcoROIInf+", izda "+marcoROIIzda+", dcha "+marcoROIDcha,4,sizex,sizey)
	
	//definimos la referencia del archivo del que leemos
	number refArchivoLectura = OpenFileForReading( archivoDatosINColumna )
	//variable donde cargaremos las lienas del txt que leemos
	string linea = " "
	number contadorFil=0, contadorCol=0, contador=1, valor
	
	while(contador<=(sizex*sizey)){
		readFileLine(refArchivoLectura,linea)
		valor = linea.val()
			
		setPixel(imgAjuste,contadorCol,contadorFil,valor)
			
		if(contador%sizex==0){
			contadorCol=0
			contadorFil++//controla el numero y cambio de fila
		}
		else{
			contadorCol++//controla el cambio de columna
		}
		contador++//controla el recorrido del txt
	}
		
	closeFile(refArchivoLectura)
	
	imgROIAjustada := imgAjuste
} // fin de la funcion ``leeFichero''
/* ************************************************* */


/* *************************************************
--- FUNCION PARA GENERAR OBJETOS CLASE ``MANCHA''
************************************************* */
void generaManchas(imageDisplay displayLocal){

//img.selectImage()
//ChooseMenuItem( "File", "Import Data...", )
number refArchivoLectura = OpenFileForReading( archivoEstimINColumna )
number refArchivoLecturaErrores = OpenFileForReading( archivoErrorEstimINColumna )
string linea=""
string lineaError=""

string resultados = "ID     mediax     mediay     sigmax     sigmay     coefCorr     peso \n"  

number cuentaLineas=1
number cuentaParam=1
number n=nNormales
number mx, my, sx, sy, rho, peso
number emx, emy, esx, esy, erho, epeso

//iniciamos bucle para recoger los parametros ajustados y asignarlos a objetos de la clase ``Mancha''
while(cuentaLineas<=(n*6)){
	readFileLine(refArchivoLectura,linea)
	readFileLine(refArchivoLecturaErrores,lineaError)
	
	if(cuentaLineas%6==0){
		cuentaParam=1
		peso=linea.val()
		epeso=lineaError.val()
		//cada 6 lineas leidas creamos un objeto...
		object mancha1 = alloc(Mancha).init(mx,my,sx,sy,rho,peso,emx,emy,esx,esy,erho,epeso,origenROI_x, origenROI_y)
		resultados.stringAppend(scriptObjectGetID(mancha1)+"     "+mx+"     "+my+"     "+sx+"     "+sy+"     "+rho+"     "+peso+"\n")
		//...lo asignamos a la lista
		listaManchas.AddObjectToList(mancha1)
		//...y seteamos y graficamos su ROI asociada
		mancha1.asignaROI(displayLocal)
		mancha1.asignaROICentro(displayLocal)
	}
	else{
		if(cuentaParam==1) mx=linea.val()
		if(cuentaParam==1) emx=lineaError.val()
		if(cuentaParam==2) my=linea.val()
		if(cuentaParam==2) emy=lineaError.val()
		if(cuentaParam==3) sx=linea.val()
		if(cuentaParam==3) esx=lineaError.val()
		if(cuentaParam==4) sy=linea.val()
		if(cuentaParam==4) esy=lineaError.val()
		if(cuentaParam==5) rho=linea.val()
		if(cuentaParam==5) erho=lineaError.val()
		
		cuentaParam++
	}
	
	cuentaLineas++
	
}

closeFile(refArchivoLectura)
closeFile(refArchivoLecturaErrores)

number refArchivoResultados = CreateFileForWriting( archivoResultados )
writeFile( refArchivoResultados, resultados)		
closeFile( refArchivoResultados )
}


/* *************************************************
--- FUNCION para borrar los txts usados durante la ejecucion
******************************************************* */
void borrarFicheros(){
	deleteFile( archivoEstimIN )
	deleteFile( archivoDatosINColumna )
	deleteFile( archivoEstimINColumna )
	deleteFile( archivoErrorEstimIN )
	deleteFile( archivoErrorEstimINColumna )
}



/* ***********************************
--- CLASE para listener para la seleccion de manchas y obtencion de parametros iniciales
************************************ ***/
class Seleccion : object{
	
	void estimadores(Object self, Number e_fl, ImageDisplay disp, Number r_fl, Number r_fl2, ROI roiEstim){
		
		image imgSeleccion = getFrontImage()[]
		
		if(roiEstim.ROIIsRectangle()==1){
			number t,b,r,l
			number ox, oy
			string linea = ""
			number mediax, mediay
			number peso=1
			number sizex, sizey
			getSize(imgROI,sizex,sizey)
			
			number sumaOffset = sizex*sizey*min(imgROI)
			number aproxOffset = min(imgROI)
			number maxROI = max(imgROI)
			
			roiEstim.ROISetVolatile(0)
			
			roiEstim.ROIGetRectangle(t,l,b,r)
			roiEstim.ROIGetVertex(0,ox,oy)
			
			//max(imgSeleccion, mediax, mediay) //...obtenemos aqui la posicion del pixel de mas intensidad
			mediax = (r-l)/2
			mediay = (b-t)/2 //...y aqui el punto medio de la region seleccionada
			//result(""+t+"  "+l+"  "+b+"  "+r+"--"+mediax+"  "+mediay+"\n")
			//peso = ((getPixel(imgSeleccion,mediax,mediay)-aproxOffset)/(sum(imgROI)-sumaOffset))*2*3.1416*((r-l)/6)*((b-t)/6)
			peso = ((getPixel(imgSeleccion,mediax,mediay)-aproxOffset)/(sum(imgROI)-sumaOffset))*2*3.1416*((r-l)/4)*((b-t)/4)
			
			//peso = (max(imgSeleccion)-aproxOffset)/(maxROI-aproxOffset)
			
			linea = ""+(ox+mediax+1)+" "+(oy+mediay+1)+" "+((r-l)/4)+" "+((b-t)/4)+" "+0+" "+peso+"\n"
			
			cadenaEstimadoresIni.stringAppend(linea)
			nNormales++
			
			result("\n ----- \n"+cadenaEstimadoresIni+"numero de picos seleccionados: "+nNormales+"\n ------ \n")
		}
		else{
			
			okDialog("la ROI debe ser Rectangular")
			disp.imageDisplayDeleteROI(roiEstim)
		}
		
	
	}//fin ``estimadores()''
	
	void estimadoresAutom(Object self, Number e_fl, ImageDisplay disp, Number r_fl, Number r_fl2, ROI roiEstim){
		
		image imgSeleccion = getFrontImage()[]
		
		if(roiEstim.ROIIsRectangle()==1){
			number t,b,r,l
			number ox, oy
			string linea = ""
			number mediax, mediay, mediax0, mediay0
			number periodox, periodoy
			number peso=1
			
			number sumaOffset = (marcoROIInf-marcoROISup)*(marcoROIDcha-marcoROIIzda)*min(imgROI)
			number aproxOffset = min(imgROI)
			number maxROI = max(imgROI)
			
			getNumber("periodo en x: ", 23, periodox)
			getNumber("periodo en y: ", 23, periodoy)
			
			roiEstim.ROISetVolatile(0)
			
			roiEstim.ROIGetRectangle(t,l,b,r)
			roiEstim.ROIGetVertex(0,ox,oy)
			
			max(imgSeleccion, mediax0, mediay0)
			//peso = ((getPixel(imgSeleccion,mediax,mediay)-aproxOffset)/(sum(imgROI)-sumaOffset))*2*3.1416*((r-l)/6)*((b-t)/6)
			peso = (max(imgSeleccion)-aproxOffset)/(maxROI-aproxOffset)
			
			mediax = mediax0
			mediay = mediay0
			while((oy+mediay+1)<(marcoROIInf-marcoROISup)){
				while((ox+mediax+1)<(marcoROIDcha-marcoROIIzda)){
					linea = ""+(ox+mediax+1)+" "+(oy+mediay+1)+" "+((r-l)/4)+" "+((b-t)/4)+" "+0+" "+peso+"\n"
			
					cadenaEstimadoresIni.stringAppend(linea)
					nNormales++
				
					mediax = mediax+periodox
					
				}
				mediax = mediax0
				mediay = mediay+periodoy
			}
			result("\n ----- \n"+cadenaEstimadoresIni+"numero de picos seleccionados: "+nNormales+"\n ------ \n")
		}
		else{
			
			okDialog("la ROI debe ser Rectangular")
			disp.imageDisplayDeleteROI(roiEstim)
		}

	}//fin de ``estimadoresAuto()''
	
	
	Seleccion(object self){ 
		result("\n evento seleccion lanzado "+self.ScriptObjectGetID())
	}	
	~Seleccion(object self){
		result("\n evento seleccion destruido "+self.ScriptObjectGetID())
	}	
	
}//fin de la clase ``seleccion''	




/* *****************************************************
--- CLASE para el key-listener que finaliza la seleccion de picos
********************************************** */
Class AccionTeclado : object
{
	
	Number SelfTOKEN
	
	number escalaCalibracion, origenCalibracion
	string unidadesCalibracion
	
	void KeepToken(object self, number tok) SelfTOKEN = tok 
	
	void setCalibracion(object self, number escCal, number origCal, string udsCal){
		escalaCalibracion = escCal
		origenCalibracion = origCal
		unidadesCalibracion = udsCal
	}
	
	number controlTecla(Object self, ImageDisplay disp, Object keydesc) 
	{
		number b_keyhandled = 0
		Result("\n Key pressed:"+keydesc.GetKeyDescriptor())
		Result(" ("+keydesc.GetDescription()+")") 
		
		If ( keydesc.MatchesKeyDescriptor("esc"))
		{
			//...destruimos keylistener...
			disp.ImageDisplayRemoveKeyHandler(SelfTOKEN)
			//...destruimos listener para la seleccion de picos...
			disp.imageDisplayRemoveEventListener(idListenerEstimaciones)
			
			b_keyhandled = 1
			//...eliminamos rois en el display...
			while ( 0 < disp.ImageDisplayCountROIs() ){
				ROI r = disp.ImageDisplayGetROI( 0 )
                disp.ImageDisplayDeleteROI( r )
			}
			
			
			//...escribimos en fichero los estimadores...
			number ref = createFileForWriting(archivoEstimOUT)
			writeFile(ref, cadenaEstimadoresIni )
			closeFile(ref)
			
			//////lanzamos script R...
			lanzaR()
			//////leemos los datos ajustados...
			leeImg()
			//////...y mostramos la imagen refinada...
			showImage(imgROIAjustada)
			//////... y generamos los objetos "mancha" sobre ella...
			generaManchas(imgROIDisp)
			
			object acc = alloc(Accion)
			acc.setListaManchas(listaManchas)
			acc.setCalibracion(escalaCalibracion, origenCalibracion, unidadesCalibracion)
			
			number idListenerResultados = imgROIDisp.imageDisplayAddEventListener(acc,"roi_added,roi_end_track:muestraInfo")
			borrarFicheros()
		}
		else{
			okDialog("Para finalizar la seleccion pulsa escape")
		}
		Return b_keyhandled 
	}
	
	AccionTeclado(object self){ 
		result("\n evento KEYLISTENER lanzado "+self.ScriptObjectGetID())
		okDialog("Cuando acabes las selecciones pulsa ESC")
	}	
	~AccionTeclado(object self){
		result("\n evento KEYLISTENER destruido "+self.ScriptObjectGetID())
	}
	
}



/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
///////////////// MAIN //////////////////////////////
/////////////////////////////////////////////////////
void main(){
//////...borramos archivos de la anterior ejecucion...
if(doesFileExist(archivoDatosOUT)==1){
	deleteFile( archivoDatosOUT )
}
if(doesFileExist(archivoEstimOUT)==1){
	deleteFile( archivoEstimOUT )
}
if(doesFileExist(archivoDatosIN)==1){
	deleteFile( archivoDatosIN )
}
if(doesFileExist(archivoResultados)==1){
	deleteFile( archivoResultados )
}


//////asignamos a ``img'' la imagen en pantalla
img.getFrontImage()
imgDisp = img.imageGetImageDisplay(0)
//////...y tomamos la calibracion de la imagen original...
number escalaCalibracion, origenCalibracion
string unidadesCalibracion
img.imageGetDimensionCalibration(0, origenCalibracion, escalaCalibracion,unidadesCalibracion,1 )
	
//////asignamos a ``imgROI'' la ROI seleccionada...
//////...y la seteamos en el imageDisplay ``imgROIDisp''
seleccionaROI()
visualizaROI(imgROI)
//////escribimos en un txt los datos de la ROI
escribeImg( imgROI )


//////introducimos parametros iniciales...
//...objeto listener para seleccionar los picos...
object estimaciones = alloc(Seleccion)
	
if(twoButtonDialog("Introduce los parametros iniciales (seleccion de picos) de forma:","manual","automatica")){
	idListenerEstimaciones = imgROIDisp.imageDisplayAddEventListener(estimaciones,"roi_end_track:estimadores")
}
else{
	okDialog("selecciona el primer elemento de cada conjunto(el mas proximo a la esquina superior izquierda)")
	idListenerEstimaciones = imgROIDisp.imageDisplayAddEventListener(estimaciones,"roi_end_track:estimadoresAutom")
}
	
//...objeto key listener pra finalizar la seleccion de picos...
object controlFinal = Alloc(AccionTeclado)
controlFinal.setCalibracion(escalaCalibracion, origenCalibracion, unidadesCalibracion)
number idControlFinal = imgROIDisp.ImageDisplayAddKeyHandler( controlFinal, "controlTecla" )
controlFinal.setCalibracion(escalaCalibracion, origenCalibracion, unidadesCalibracion)
controlFinal.KeepToken(idControlFinal)
		
}//...fin del main()...

main()


