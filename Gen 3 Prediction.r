#################################################
####Predict the type of Generation 3 Pokemons####
#################################################
library(xlsx)
library(png)
library(plotly)
library(kknn)
##############Download generation 3 (Ruby Version) ##############
setwd("C:\\Users\\Guillaume\\Documents\\Pokemon\\Pokemon 2\\Downloaded\\Gen 3")

for(i in 1:386){
	ParsePage = htmlParse(paste0("http://bulbapedia.bulbagarden.net/wiki/File:Spr_3r_", sprintf("%03d", i), ".png"))
	Img.Loc = xpathSApply(ParsePage, '//div[@class="fullImageLink"]//img', xmlGetAttr, 'src')
	download.file(Img.Loc, dest = paste0(sprintf("%03d", i), ".png"), mode = 'wb')
}


###Compute the most used color for each Pokémon####
ModeColorNew = character(386)
ModeRNew = numeric(386)
ModeGNew = numeric(386)
ModeBNew = numeric(386)

####Create function to compute the mode####
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


for(i in 1:386){
	Img.Png = readPNG(paste0(sprintf("%03d", i), ".png"))

	j = i

	####Only use points that are not transparent, nor white, nor black (used as the contour of pokémons)####
	Transparents = Img.Png[,,4] == 1
	Blacks = (Img.Png[,,1] == 0 & Img.Png[,,2] == 0 & Img.Png[,,3] == 0)
	Whites = (Img.Png[,,1] == 1 & Img.Png[,,2] == 1 & Img.Png[,,3] == 1)
	AlmostBlacks3 = (Img.Png[,,1] == 16/255		&	Img.Png[,,2] == 16/255		&	Img.Png[,,3] == 16/255)	#Contour of Electabuzz, Eevee, Hitmonchan, etc.
	
	Used.Points = Transparents & !(Blacks) & !(Whites)& !(AlmostBlacks3)

	ModeColorNew[j]  = Mode(
		paste0(
			"#",
			format(as.hexmode(Img.Png[,,1][Used.Points]*255), width = 2),
			format(as.hexmode(Img.Png[,,2][Used.Points]*255), width = 2),
			format(as.hexmode(Img.Png[,,3][Used.Points]*255), width = 2)
		)
	) #The most used(mode) color as Hexadecimal

	####Retransform the color in individualRGB####
	ModeRNew[j] =strtoi(substring(ModeColorNew[j], 2, 3), 16) 
	ModeGNew[j] =strtoi(substring(ModeColorNew[j], 4, 5), 16)
	ModeBNew[j] =strtoi(substring(ModeColorNew[j], 6, 7), 16)
}

####create database or most used colors in RGB####
Df.Colors.Mode.RGB.New = merge(
data.frame(id = 1:386,R = ModeRNew, G = ModeGNew, B = ModeBNew,color = ModeColorNew),
pokemon.db[,c("id","identifier","Type.I","Type.II")])


####Plot the colors! Woo!####
p=plot_ly(Df.Colors.Mode.RGB.New,	
		x = R+runif(nrow(Df.Colors.Mode.RGB.New))*2-1, #X as red + random noise to avoid overplotting
		y = G+runif(nrow(Df.Colors.Mode.RGB.New))*2-1, #Y as Green + random noise
		z = B+runif(nrow(Df.Colors.Mode.RGB.New))*2-1, #Z as Blue + random noise
		type = "scatter3d",
		mode = "markers",
		marker = list(color = ModeColorNew),
		hoverinfo = "text",
		text = paste0("Number: ", id, "<br>Pokemon: ", identifier, "<br>Color: ",color) #Hover Text
) %>% layout(			#Change title and axis texts
    scene=list(
        xaxis = list(title = 'R'),
		yaxis = list(title = 'G'),
        zaxis = list(title = 'B')
    ),
    title = "Pokémons Main Color"
)
p
###Save as one html file(requires pandoc)###
htmlwidgets::saveWidget(as.widget(p), "PokemonPrimaryColorAllGen3.html") 



Df.Colors.Mode.RGB.New$Predict.I = kknn(Type.I ~ R+G+B, Train.RGB, Df.Colors.Mode.RGB.New, k = 10)$fitted.values
Df.Colors.Mode.RGB.New$Predict.II = kknn(Type.II ~ R+G+B, Train.RGB, Df.Colors.Mode.RGB.New, k = 10)$fitted.values


write.xlsx(Df.Colors.Mode.RGB.New, "DfColorsGen3.xlsx")
