/*

SCRIPT TFM (Feb 2016)
escribir datos y estimadores iniciales para modelar como mezcla de normales 
 
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
//image imgROIAjustada

//...definimos las rutas para los archivos de texto que usaremos...
string archivoDatosOUT = "C:/Users/Usuario/Desktop/plugInDM/datosImgDM_OUT.txt"
string archivoEstimOUT = "C:/Users/Usuario/Desktop/plugInDM/estimImgDM_OUT.txt"
string archivoDatosIN = "C:/Users/Usuario/Desktop/plugInDM/datosImgDM_IN.txt"
//string archivoEstimIN = "C:/Users/Usuario/Desktop/plugInDM/estimImgDM_IN.txt"
//string archivoDatosINColumna = "C:/Users/Usuario/Desktop/plugInDM/ColumnaDatosImgDM_IN.txt"
//string archivoEstimINColumna = "C:/Users/Usuario/Desktop/plugInDM/ColumnaEstimImgDM_IN.txt"
//string archivoErrorEstimIN = "C:/Users/Usuario/Desktop/plugInDM/errorEstimImgDM_IN.txt"
//string archivoErrorEstimINColumna = "C:/Users/Usuario/Desktop/plugInDM/ColumnaErrorEstimImgDM_IN.txt"
string archivoResultados = "C:/Users/Usuario/Desktop/plugInDM/estimadores_IN.txt"

string archivoCoordROI = "C:/Users/Usuario/Desktop/plugInDM/coordinatesROI.txt"


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
	
	
	//...y escribimos origen y tamanio de la ROI en fichero de texto...
	number refArchivoEscritura = CreateFileForWriting(archivoCoordROI)
	writeFile(refArchivoEscritura, "OX OY tamX tamY \n")
	writeFile(refArchivoEscritura, ""+origenROI_x+" "+origenROI_y+" "+(marcoROIDcha-marcoROIIzda)+" "+(marcoROIInf-marcoROISup)+"\n")
	closeFile(refArchivoEscritura)
	
}//cierre de ``seleccionaROI'' 
/* **************************************** */



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
			
			max(imgSeleccion, mediax, mediay)
			
			//peso = ((getPixel(imgSeleccion,mediax,mediay)-aproxOffset)/(sum(imgROI)-sumaOffset))*2*3.1416*((r-l)/6)*((b-t)/6)
			peso = (max(imgSeleccion)-aproxOffset)/(maxROI-aproxOffset)
			
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
if(doesFileExist(archivoCoordROI)==1){
	deleteFile( archivoCoordROI )
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
//visualizaROI(imgROI)
//////escribimos en un txt los datos de la ROI
escribeImg( imgROI )


//////introducimos parametros iniciales...
//...objeto listener para seleccionar los picos...
object estimaciones = alloc(Seleccion)
	
if(twoButtonDialog("...seleccion de picos...","Si","Salir")){
	idListenerEstimaciones = imgROIDisp.imageDisplayAddEventListener(estimaciones,"roi_end_track:estimadores")
	
	//...objeto key listener pra finalizar la seleccion de picos...
	object controlFinal = Alloc(AccionTeclado)
	//controlFinal.setCalibracion(escalaCalibracion, origenCalibracion, unidadesCalibracion)
	number idControlFinal = imgROIDisp.ImageDisplayAddKeyHandler( controlFinal, "controlTecla" )
	//controlFinal.setCalibracion(escalaCalibracion, origenCalibracion, unidadesCalibracion)
	controlFinal.KeepToken(idControlFinal)
}
else{
	exit(0)
}
	

		
}//...fin del main()...

main()


