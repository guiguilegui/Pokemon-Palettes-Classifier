################################################
####Download Pokémon sprites from Bulbapedia####
################################################

for(i in 1:250){
ParsePage= htmlParse(paste0("http://bulbapedia.bulbagarden.net/wiki/File:Spr_2c_", sprintf("%03d", i), ".png"))
Img.Loc = xpathSApply(ParsePage, '//div[@class="fullImageLink"]//img', xmlGetAttr, 'src')
download.file(Img.Loc, dest = paste0(sprintf("%03d", i),".png"), mode = 'wb')
}


####Create function to compute the mode####
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
