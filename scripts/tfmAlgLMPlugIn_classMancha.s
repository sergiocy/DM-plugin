/* **********************************************************
--- CLASE para objetos ``Mancha'': cada una de las normales
********************************************************* */
class Mancha : object{

	//...variables de clase...
	//...los parmetros de la normal...
	number mu_x, mu_y, sigma_x, sigma_y, rho, peso/*, IDLista*/
	//...y sus desviaciones...
	number emu_x, emu_y, esigma_x, esigma_y, erho, epeso
	number IDMancha, IDROIMancha
	ROI ROIMancha
	ROI ROICentro
	number origenROI_x, origenROI_y
	
	number cuentaClick
	
	
	//...getters y setters...
	void setCuentaClickIni(object self) cuentaClick=2
	number getMux(object self) return mu_x
	number getMuy(object self) return mu_y
	ROI getROIMancha(object self) return ROIMancha
	
	void setCuentaClick(object self) cuentaClick++
	
	
	//...constructor
	object init( object self, number mx, number my, number sx, number sy, number ro, number pes,number emx, number emy, number esx, number esy, number ero, number epes, number oROI_x, number oROI_y ){
	
		mu_x = mx
		mu_y = my
		sigma_x = sx
		sigma_y = sy
		rho = ro
		peso = pes
		emu_x = emx
		emu_y = emy
		esigma_x = esx
		esigma_y = esy
		erho = ero
		epeso = epes
		
		origenROI_x = oROI_x
        origenROI_y = oROI_y
		
		IDMancha = ScriptObjectGetID(self)
		
		//...inicializamos variable ``cuentaClick'' al crear el objeto
		setCuentaClickIni(self)
		
		return self
	}
	
	void asignaROI(object self, imageDisplay displayLocal){
		ROIMancha = newROI()
		ROIMancha.ROIAddVertex(mu_x-sigma_x , mu_y)
		ROIMancha.ROIAddVertex(mu_x , mu_y+sigma_y)
		ROIMancha.ROIAddVertex(mu_x+sigma_x , mu_y)
		ROIMancha.ROIAddVertex(mu_x , mu_y-sigma_y)
		ROIMancha.ROISetColor(0,0,1)
		ROISetIsClosed(ROIMancha,1)
		ROISetVolatile(ROIMancha,0)
		
		ROIMancha.ROISetLabel("ID: "+IDMancha)
		displayLocal.imageDisplayAddROI(ROIMancha)
	}
	
	void asignaROICentro(object self, imageDisplay displayLocal){
		ROICentro = newROI()
		ROICentro.ROISetPoint(mu_x,mu_y)
		ROISetVolatile(ROICentro,0)
		
		displayLocal.imageDisplayAddROI(ROICentro)
	}
		
	void creaEtiqueta(object self){
		
		number selec = cuentaClick%2
		
		if(selec==1){
			number mu_xG = mu_x+origenROI_x
			number mu_yG = mu_y+origenROI_y
			ROIMancha.ROISetLabel("ID: "+IDMancha+"\n media_x: "+mu_x+" +- "+emu_x+"\n media_y: "+mu_y+" +- "+emu_y+"\n media_x (global): "+(mu_xG)+"\n media_y (global): "+mu_yG+"\n peso: "+peso+" +- "+epeso)
			
			result("\n ------ \n")
			result("\n ID: "+IDMancha)
			result("\n media x: " + mu_x + " +- "+emu_x)
			result("\n media y: " + mu_y + " +- " +emu_y)
			result("\n media x (global): " + (mu_x+origenROI_x))
			result("\n media y (global): " + (mu_y+origenROI_y))
			result("\n peso: " + peso + " +- "+epeso)
			result("\n sigmax: "+sigma_x+" +- "+esigma_x)
			result("\n sigmay: "+sigma_y+" +- "+esigma_y)
			result("\n coef. Corr.: "+rho+" +- "+erho)
			result("\n ------ \n")

		}
		if(selec==0){
			ROIMancha.ROISetLabel("ID: "+IDMancha)
		}
		
	}
	
}