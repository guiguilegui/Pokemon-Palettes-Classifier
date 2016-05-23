####################################################
####Compute the most used color for each Pokémon####
####################################################
library(png)

ModeColor=character(251)
ModeR=numeric(251)
ModeG=numeric(251)
ModeB=numeric(251)

####Create function to compute the mode####
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for(i in 1:251){
	Img.Png = readPNG(paste0(sprintf("%03d", i),".png"))


	####Only use points that are not transparent, nor white, nor black (used as the contour of pokémons)####
	Transparents = 	(Img.Png[,,4] == 1)
	Blacks = 		(Img.Png[,,1] == 0			&	Img.Png[,,2] == 0			&	Img.Png[,,3] == 0)
	Whites = 		(Img.Png[,,1] == 1			&	Img.Png[,,2] == 1			&	Img.Png[,,3] == 1)
	AlmostWhites = 	(Img.Png[,,1] == 248/255	&	Img.Png[,,2] == 248/255		&	Img.Png[,,3] == 248/255)#Arcanine,Electrode,Jynx,Flareon,Omanyte,Dratini,Mew,Ariados,Yanma,Slowking 
	#AlmostBlacks1 = (Img.Png[,,1] == 0			&	Img.Png[,,2] == 0			&	Img.Png[,,3] == 1/255)	#Haunter&Gengar
	AlmostBlacks2 = (Img.Png[,,1] == 3/255		&	Img.Png[,,2] == 3/255		&	Img.Png[,,3] == 3/255)	#Contour of Gyarados
	
	#	Used.Points = Transparents & !(Blacks) & !(Whites) & !(AlmostWhites) & !(AlmostBlacks1) & !(AlmostBlacks2)
	Used.Points = Transparents & !(Blacks) & !(Whites) & !(AlmostWhites) & !(AlmostBlacks2)
	ModeColor[i]  = Mode(
		paste0(
			"#",
			format(as.hexmode(Img.Png[,,1][Used.Points]*255), width = 2),
			format(as.hexmode(Img.Png[,,2][Used.Points]*255), width = 2),
			format(as.hexmode(Img.Png[,,3][Used.Points]*255), width = 2)
		)
	) #The most used(mode) color as Hexadecimal

	####Retransform the color in individualRGB####
	ModeR[i] = strtoi(substring(ModeColor[i],2,3), 16) 
	ModeG[i] = strtoi(substring(ModeColor[i],4,5), 16)
	ModeB[i] = strtoi(substring(ModeColor[i],6,7), 16)
}

####create database or most used colors in RGB####
Df.Colors.Mode.RGB = merge(
data.frame(id = 1:251, R = ModeR, G = ModeG, B = ModeB, color = ModeColor),
pokemon.db[,c("id","identifier","Type.I","Type.II")])
