###################################
####Plot the colors in 3D! Woo!####
###################################
library(plotly)

p=plot_ly(Df.Colors.Mode.RGB,
		x = R+runif(nrow(Df.Colors.Mode.RGB))*2-1, #X as red + random noise to avoid overplotting
		y = G+runif(nrow(Df.Colors.Mode.RGB))*2-1, #Y as Green + random noise
		z = B+runif(nrow(Df.Colors.Mode.RGB))*2-1, #Z as Blue + random noise
		type = "scatter3d",
		mode = "markers",
		marker = list(color = ModeColor),
		hoverinfo = "text",
		text = paste0("Number: ", id, "<br>Pokemon: ", identifier, "<br>Color: ", color) #Hover Text
) %>% layout(				#Change title and axis texts
    scene=list(
        xaxis = list(title = 'R'),
		yaxis = list(title = 'G'),
        zaxis = list(title = 'B')
    ),
    title = "Pokémons Main Color")
p

###Save as one html file(requires pandoc)###
htmlwidgets::saveWidget(as.widget(p), "PokemonPrimaryColor.html") 
