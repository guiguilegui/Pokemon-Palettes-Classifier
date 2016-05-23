
##################################################
####Create Pokemon Types Database using Veekun####
##################################################
library(Kmisc)
options(stringsAsFactors = FALSE)

pokemon_types = read.csv("https://raw.githubusercontent.com/veekun/pokedex/master/pokedex/data/csv/pokemon_types.csv")
pokemon = read.csv("https://raw.githubusercontent.com/veekun/pokedex/master/pokedex/data/csv/pokemon.csv")
types = read.csv("https://raw.githubusercontent.com/veekun/pokedex/master/pokedex/data/csv/types.csv")

####Main Table####
pokemon.db = pokemon

####Get types on the main table####
pokemon_types.I = pokemon_types[pokemon_types$slot == 1,]
pokemon_types.I$identifier = types$identifier[pokemon_types.I$type_id]

pokemon_types.II = pokemon_types[pokemon_types$slot == 2,]
pokemon_types.II$identifier = types$identifier[pokemon_types.II$type_id]

pokemon.db$Type.I  = merge(pokemon.db, pokemon_types.I,  by.x = "id", by.y = "pokemon_id", all.x = TRUE)$identifier.y	#Get Type I
pokemon.db$Type.II = merge(pokemon.db, pokemon_types.II, by.x = "id", by.y = "pokemon_id", all.x = TRUE)$identifier.y	#Get Type II

pokemon.db$Type.II[is.na(pokemon.db$Type.II)]=""	#Put blank as Missing second type

pokemon.db$Type.I[pokemon.db$Type.I		== "fairy"]	= "normal"	#Generation VI introduced the fairy type, which was the normal type before (for type I)
pokemon.db$Type.II[pokemon.db$Type.II	== "fairy"]	= ""	#Generation VI introduced the fairy type, which was "" before (for type II)

#Use abreviations for Types
pokemon.db$Type.I = swap( 
	pokemon.db$Type.I, 
	from = 	c("grass", "fire", "water", "bug", "normal", "poison" ,"electric", "ground", "fighting", "psychic", "rock", "ghost", "ice", "dragon", "dark", "steel"	, "flying"),
	to = 	c("GRS"	 , "FIR",  "WTR",   "BUG", "NRM",    "PSN"    ,"ELC",      "GRD", 	 "FGT",      "PSY",     "RCK",  "GHT",   "ICE", "DRG",    "DRK",  "STL"	,	 "FLY")
)

pokemon.db$Type.II = swap( 
	pokemon.db$Type.II, 
	from = 	c("grass", "fire", "water", "bug", "normal", "poison" ,"electric", "ground", "normal", "fighting", "psychic", "rock", "ghost", "ice", "dragon", "dark", "steel"	, "flying"),
	to = 	c("GRS"	 , "FIR",  "WTR",   "BUG", "NRM",    "PSN"    ,"ELC",      "GRD", 	 "NRM",    "FGT",      "PSY",     "RCK",  "GHT",   "ICE", "DRG",    "DRK",  "STL"	, "FLY")
)


###########################################################################################################
####Generate a Hue-Saturation-Value Plane and look for the most probable type using the nearest Pokémon####
###########################################################################################################

###Create database of a color wheel###
Dots.xy = expand.grid(x=seq(-1, 1, 0.05), y=seq(-1, 1, 0.05)) 	#Generate evenly spaced points
Dots.xy.2 = subset(Dots.xy, x^2+y^2 <= 1)					#Only keep points in a circle

v=1			#Value chosen
Dots = data.frame(			#Transform cartesian coordinates to polar
	h = (Arg(complex(real = Dots.xy.2$x, imaginary = Dots.xy.2$y))+pi)/(2*pi),
	s = Mod(complex(real = Dots.xy.2$x, imaginary = Dots.xy.2$y)),
	v = v
)

Dots.Colors = hsv(Dots[,1],Dots[,2],Dots[,3])				#Compute colors from points chosen

###Create a testing database from the color wheel###
Test.RGB = data.frame(											#Transform colors HSV->RGB
	R = as.integer(as.hexmode(substring(Dots.Colors,2,3))),	
	G = as.integer(as.hexmode(substring(Dots.Colors,4,5))),
	B = as.integer(as.hexmode(substring(Dots.Colors,6,7)))
)

###Create a training database from the Pokémon color database###
Train.RGB = data.frame(											#Use RGB colors from Pokémons
	R = c(Df.Colors.Mode.RGB$R),
	G = c(Df.Colors.Mode.RGB$G),
	B = c(Df.Colors.Mode.RGB$B),
	Type.I=c(as.character(Df.Colors.Mode.RGB$Type.I)),				#Use the type
	Type.II=c(as.character(Df.Colors.Mode.RGB$Type.II))				#Use the type
)
Train.RGB$Type.I = as.factor(Train.RGB$Type.I)						#kknn requires factors
Train.RGB$Type.II = as.factor(Train.RGB$Type.II)						#kknn requires factors


###Predict the type using a KKNN algorithm###
#train.kknn(Type.I~R+G+B, data=Train.RGB,kmax = 50)	#What is the best k?Type.I -> k=10, type.II -> k=22

Dots$Predict = kknn(Type.I ~ R+G+B, Train.RGB, Test.RGB, k=10)$fitted.values

sum(kknn(Type.I ~ R+G+B, Train.RGB, Train.RGB, k=1)$fitted.values==Train.RGB$Type.I)

###Attach the predicted value to the evenly spaced pointsm###
Dots.xy.3 = Dots.xy
Dots.xy.3$Predict = ""
Dots.xy.3[Dots.xy.3$x^2 + Dots.xy.3$y^2 <= 1,"Predict"] = as.character(Dots$Predict)

###Generate a layer for each type###
Dots.xy.4=data.frame()
for(level in unique(Dots.xy.3$Predict)){
	Dots.xy.3$Prob	= ifelse(Dots.xy.3$Predict %in% c(level, ""), 1, 0)
	Dots.xy.3$level	= level
	Dots.xy.4		= rbind(Dots.xy.3, Dots.xy.4)
}

###Monospaced Font###
windowsFonts(Lucida=windowsFont("Lucida Console")) 

###Adjust colors###
if(v<0.45){
	txtcol = "grey70"
	contourcol = "grey30"
}else{
	txtcol="grey10"
	contourcol = "grey70"
}

###Color Wheel Plot###
ggplot() +
	scale_x_continuous(breaks=NULL) + 	#Removes breaks
	scale_y_continuous(breaks=NULL) + 	#Removes breaks
	scale_color_identity() +		 	#Use values without scaling.
	geom_point(data = Dots, aes(x = s*cos(h*2*pi+pi), y = s*sin(h*2*pi+pi), color=hsv(h,s,v)), size=15) +	#Color Wheel
	geom_contour(data = Dots.xy.4, aes( x = x, y = y, z = Prob, group = level), color = contourcol, bins = 1, size = 1.5, breaks = 1) +	#Separation
	geom_text(data = Dots.xy.3, aes(label = Predict, x=x, y=y), color = txtcol, family="Lucida", size=2.5) + #Names
	theme(legend.position="none", axis.title = element_blank()) #Minimalast
