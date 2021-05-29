# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# This code is released as part of the manuscript
# "Protein residue network analysis reveals fundamental properties of the human coagulation factor VIII", submitted in 2021.
#
# Authors: Tiago J. S. Lopes, Ricardo Rios, Tatiane Nogueira, Rodrigo F. Mello
#
# This code will produce the plots depicted in Figures 4d and 4e.
# These functions depict a centrality measure (closeness), 
# of the different domains of the FVIII protein.
#
# The output are two SVG files.

source("../src/generate_net_and_centralities.R")

plot_fviii_centrality = function(x_values, y_values, ylabel){

  statistics = data.frame(x_values=x_values, y_values = y_values, group=NA)
  
  # 1 - 336 A1 domain
  pos = which(x_values <= 336)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  
  statistics$group[pos] = "A1"
  
  plot(tmp, xlim=c(0, max(x_values)), type="l", pch=19, col="#dfcce4", las=1, 
       ylab = ylabel, xlab="", xaxt="n", cex.lab=1.4)
  axis(1,  c(1, 336, 372, 710, 740, 2019, 2172))
  
  # 337 - 372 a1
  pos = which(x_values >= 337 & x_values <= 372)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "a1"
  
  lines(tmp, col="black")
  
  # 373 - 710 A2 domain
  pos = which(x_values >= 373 & x_values <= 710)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "A2"
  
  lines(tmp, col="#ffdaa2")
  
  # 711 - 740 a2
  pos = which(x_values >= 711 & x_values <= 740)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "a2"
  
  lines(tmp, col="black")
  
  # 741 - 1648 B domain
  #pos = which(x_values >= 741 & x_values <= 1648)
  #tmp = cbind(x_values[pos], y_values[pos])
  #tmp = tmp[order(tmp[,1]),]
  #statistics$group[pos] = "B"
  
  #lines(tmp, col="#fffbcc")
  
  # 1649 - 1689 a3
  #pos = which(x_values >= 1649 & x_values <= 1689)
  #tmp = cbind(x_values[pos], y_values[pos])
  #tmp = tmp[order(tmp[,1]),]
  #statistics$group[pos] = "a3"
  
  #lines(tmp, col="black")
  
  # 1690 - 2019 A3 domain
  pos = which(x_values >= 1690 & x_values <= 2019)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "A3"
  
  lines(tmp, col="#fcd4d1")
  
  # 2020 - 2172 - C1 domain
  pos = which(x_values >= 2020 & x_values <= 2172)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "C1"
  
  lines(tmp, col="#bce4e5")
  
  # 2173 - 2332 - C2 domain
  pos = which(x_values >= 2173 & x_values <= 2332)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "C2"
  
  lines(tmp, col="#adc5e7")

  abline(v=c(336, 372, 710, 740, 2019, 2172))

  #statistics = statistics[-which(statistics$group == "B" | statistics$group == "a3"),]
  
  statistics$group <- factor(statistics$group , levels=c("A1", "a1", "A2", "a2", "A3", "C1", "C2"))
  
  if(length(which(!is.finite(statistics$y_values))) > 0){
    statistics = statistics[-which(!is.finite(statistics$y_values)),]
  }

  print(ylabel)
  print(TukeyHSD(aov(statistics$y_values~statistics$group)))
}



boxplot_fviii_centrality = function(x_values, y_values, ylabel){
  
  statistics = data.frame(x_values=x_values, y_values = y_values, group=NA)
  
  # 1 - 336 A1 domain
  pos = which(x_values <= 336)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  
  statistics$group[pos] = "A1"
  
  # 337 - 372 a1
  pos = which(x_values >= 337 & x_values <= 372)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "a1"
  
  # 373 - 710 A2 domain
  pos = which(x_values >= 373 & x_values <= 710)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "A2"
  
  # 711 - 740 a2
  pos = which(x_values >= 711 & x_values <= 740)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "a2"
  
  # 741 - 1648 B domain
  pos = which(x_values >= 741 & x_values <= 1648)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "B"
  
  # 1649 - 1689 a3
  pos = which(x_values >= 1649 & x_values <= 1689)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "a3"

  # 1690 - 2019 A3 domain
  pos = which(x_values >= 1690 & x_values <= 2019)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "A3"
  
  # 2020 - 2172 - C1 domain
  pos = which(x_values >= 2020 & x_values <= 2172)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "C1"
  
  # 2173 - 2332 - C2 domain
  pos = which(x_values >= 2173 & x_values <= 2332)
  tmp = cbind(x_values[pos], y_values[pos])
  tmp = tmp[order(tmp[,1]),]
  statistics$group[pos] = "C2"
  
  statistics = statistics[-which(statistics$group == "B" | statistics$group == "a3"),]
  
  statistics$group <- factor(statistics$group , levels=c("A1", "a1", "A2", "a2", "A3", "C1", "C2"))
  
  
  if(length(which(!is.finite(statistics$y_values))) > 0){
    statistics = statistics[-which(!is.finite(statistics$y_values)),]
  }
  
  
  boxplot(statistics$y_values~statistics$group, col=c("#dfcce4", "white", "#ffdaa2", "white", "#fcd4d1",
                                                      "#bce4e5", "#adc5e7"), las=1, ylab=ylabel, cex.lab=1.4, xlab="")
}

###############################################################################
##
## This is the main part of the code. From here we call the functions to
## generate the figures.
##
###############################################################################

svg("Figure_4d_plot.svg", width = 10, height = 5)

plot_fviii_centrality(net.centr$node, net.centr$cl, "Closeness")

dev.off()

svg("Figure_4e_boxplot.svg", width = 7, height = 5)

boxplot_fviii_centrality(net.centr$node, net.centr$cl, "Closeness")

dev.off()




















































