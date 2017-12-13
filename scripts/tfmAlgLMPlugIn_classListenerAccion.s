/* ****************************************************************************************************** */
/* ******* CLASE ACCION para implementar el listener que muestra los paraametros de ajuste y distancias **** */
/* ******************************************************************************************************* */
class Accion : object{	
	
	object listaManchas
	//image img
	number escalaCalibracion, origenCalibracion
	string unidadesCalibracion
	
	void setListaManchas(object self, object lista) listaManchas = lista
	//void setImagen(object self, image imagen) img = imagen
	void setCalibracion(object self, number escCal, number origCal, string udsCal){
		escalaCalibracion = escCal
		origenCalibracion = origCal
		unidadesCalibracion = udsCal
	}
	
	void muestraInfo(Object self, Number e_fl, ImageDisplay disp, Number r_fl, Number r_fl2, ROI r){
		
		//condicional para ejecutar la accion de mostrar parametros con una ROI PUNTUAL
		if(r.ROIIsPoint()==1/* & r.ROICountVertices()==1*/){
			//disp.imageDisplayAddROI(r)
			number x,y
			number contador=0
			number idMancha
			r.ROIGetPoint(x,y)
			
			//bucle para recorrer ``listaManchas'' y buscar la que corresponde al punto (x,y)
			for(contador=0 ; contador<listaManchas.sizeOfList() ; contador++){
				
				//asignamos la id de cada mancha en ``listaManchas'' a la variable idMancha
				idMancha = ScriptObjectGetID( listaManchas.ObjectAt( contador ) )
				
				//y accedemos a la ROI asociada al objeto de clase Mancha que contenga al punto
				if(getScriptObjectFromID(idMancha).getROIMancha().ROIContainsPoint(x,y)==1){
					
					getScriptObjectFromID(idMancha).setCuentaClick()
					getScriptObjectFromID(idMancha).creaEtiqueta()
					
					break
				}
				
			}
			
		}//fin del ``if'' para ROI-punto
		
		
		//condicional para ejecutar la accion de medir distancia con una ROI LINEA
		if(r.ROIIsLine()==1/* && r.ROICountVertices()==2*/){
			//result("\n es una linea    "+ROIIsLine(r))
			
			number idMancha1, idMancha2
			number x1, y1, x2, y2, distancia
			
			//number escalaCalibracion, origenCalibracion
			//string unidadesCalibracion
			//img.imageGetDimensionCalibration(0, origenCalibracion, escalaCalibracion,unidadesCalibracion,1 )
			
			number contador = 0, continuaContador = 0
			
			r.ROIGetLine(x1,y1,x2,y2)
			//result("\n extremos: ("+sx+","+sy+") y ("+ex+","+ey+")")
			
			if(listaManchas.sizeOfList()>1){
				//result("\n extremos: ("+x1+","+y1+") y ("+x2+","+y2+")")
				
				//bucle para recorrer ``listaManchas'' y buscar la que corresponde al punto (x1,y1) y (x2,y2)
				for(contador=0; contador<listaManchas.sizeOfList(); contador++){
				
					idMancha1 = ScriptObjectGetID( listaManchas.ObjectAt( contador ) )
				
				//y accedemos a la ROI asociada al objeto de clase Mancha que contenga al punto
					if(getScriptObjectFromID(idMancha1).getROIMancha().ROIContainsPoint(x1,y1)==1){
						
						//result("\n la mancha "+idMancha1+" contiene el punto")
						break
					}
	
				}
				for(contador=0; contador<listaManchas.sizeOfList(); contador++){
				
					idMancha2 = ScriptObjectGetID( listaManchas.ObjectAt( contador ) )
				
				//y accedemos a la ROI asociada al objeto de clase Mancha que contenga al punto
					if(getScriptObjectFromID(idMancha2).getROIMancha().ROIContainsPoint(x2,y2)==1){
						
					//	result("\n la mancha "+idMancha2+" contiene el punto")
						break
					}
	
				}
				
				if(idMancha1==idMancha2 || getScriptObjectFromID(idMancha2).getROIMancha().ROIContainsPoint(x2,y2)==0 || getScriptObjectFromID(idMancha2).getROIMancha().ROIContainsPoint(x2,y2)==0){
					r.ROISetLabel("error")
				}
				else{
					object m1 = getScriptObjectFromID(idMancha1)
					object m2 = getScriptObjectFromID(idMancha2)
					distancia = ((m1.getMux()-m2.getMux())**2+(m1.getMuy()-m2.getMuy())**2)**(1/2)
					r.ROISetLabel("distancia: "+distancia*escalaCalibracion+"  "+unidadesCalibracion)
					result("\n ------- \n")
					result("\n extremos: ("+x1+","+y1+") y ("+x2+","+y2+")")
					result("\n distancia (pixeles): "+distancia+" - "+distancia*escalaCalibracion+"  "+unidadesCalibracion)
					result("\n ------- \n")
				}	
				
			}
				
			
			else{
				okDialog("Solo hay un elemento. No podemos medir distancias.")
				//exit(0)
			}
		}//fin del ``if'' para medida de distancia (ROI-linea)
		
		
	}//fin de metodo ``muestraInfo''
	
	
	Accion(object self){ 
		result("\n evento INFORMACION lanzado "+self.ScriptObjectGetID())
		//result("\n es un punto    "+ROIIsPoint(r))
	}	
	~Accion(object self){
		
		result("\n evento INFORMACION destruido "+self.ScriptObjectGetID())
		
	}	

}//cierre de la clase ``Accion''
