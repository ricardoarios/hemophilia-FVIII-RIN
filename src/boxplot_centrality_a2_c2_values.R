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
# This code will produce the output of Figure 2 of the manuscript.
# Here we depict the centrality measures of the residues of the A2 and the C2 domains, 
# after a massive alanine screening assay (see Methods in the manuscript).
#
# It also tests the significance of the results.
#
# The output is a SVG file.

# Prepare the dataset

source("../src/generate_net_and_centralities.R")

net.centr$a2_level = NA
net.centr$c2_level = NA
net.centr$a2_ag_level = NA
net.centr$c2_ag_level = NA

source("../src/match_a2_c2_net.R")

## I plotted and saved the image file manually

svg(filename = "Figure_2_boxplots.svg", width = 15, height = 5)
par(mfrow=c(2,6))

# FVIII Activity

boxplot(net.centr$dg~net.centr$a2_level, col="white", las=1, ylab="Degree", xlab="", names=c("", ""))
stripchart(net.centr$dg~net.centr$a2_level, col=c("cornflowerblue", "lightsalmon1"), add=T, method="jitter", pch=19, vertical=T)

print("here")

boxplot(log(net.centr$bt)~net.centr$a2_level, col="white", las=1, ylab="log(Betweenness)", xlab="", names=c("", ""))
stripchart(log(net.centr$bt)~net.centr$a2_level, col=c("cornflowerblue", "lightsalmon1"), add=T, method="jitter", pch=19, vertical=T)

boxplot(net.centr$cl~net.centr$a2_level, col="white", las=1, ylab="Closeness", xlab="", names=c("", ""))
stripchart(net.centr$cl~net.centr$a2_level, col=c("cornflowerblue", "lightsalmon1"), add=T, method="jitter", pch=19, vertical=T)

boxplot(net.centr$dg~net.centr$a2_ag_level, col="white", las=1, ylab="Degree", xlab="", names=c("", ""))
stripchart(net.centr$dg~net.centr$a2_ag_level, col=c("palegreen2", "khaki"), add=T, method="jitter", pch=19, vertical=T)

boxplot(log(net.centr$bt)~net.centr$a2_ag_level, col="white", las=1, ylab="log(Betweenness)", xlab="", names=c("", ""))
stripchart(log(net.centr$bt)~net.centr$a2_ag_level, col=c("palegreen2", "khaki"), add=T, method="jitter", pch=19, vertical=T)

boxplot(net.centr$cl~net.centr$a2_ag_level, col="white", las=1, ylab="Closeness", xlab="", names=c("", ""))
stripchart(net.centr$cl~net.centr$a2_ag_level, col=c("palegreen2", "khaki"), add=T, method="jitter", pch=19, vertical=T)


boxplot(net.centr$dg~net.centr$c2_level, col="white", las=1, ylab="Degree", xlab="", names=c("", ""))
stripchart(net.centr$dg~net.centr$c2_level, col=c("cornflowerblue", "lightsalmon1"), add=T, method="jitter", pch=19, vertical=T)

boxplot(log(net.centr$bt)~net.centr$c2_level, col="white", las=1, ylab="log(Betweenness)", xlab="", names=c("", ""))
stripchart(log(net.centr$bt)~net.centr$c2_level, col=c("cornflowerblue", "lightsalmon1"), add=T, method="jitter", pch=19, vertical=T)

boxplot(net.centr$cl~net.centr$c2_level, col="white", las=1, ylab="Closeness", xlab="", names=c("", ""))
stripchart(net.centr$cl~net.centr$c2_level, col=c("cornflowerblue", "lightsalmon1"), add=T, method="jitter", pch=19, vertical=T)

boxplot(net.centr$dg~net.centr$c2_ag_level, col="white", las=1, ylab="Degree", xlab="", names=c("", ""))
stripchart(net.centr$dg~net.centr$c2_ag_level, col=c("palegreen2", "khaki"), add=T, method="jitter", pch=19, vertical=T)

boxplot(log(net.centr$bt)~net.centr$c2_ag_level, col="white", las=1, ylab="log(Betweenness)", xlab="", names=c("", ""))
stripchart(log(net.centr$bt)~net.centr$c2_ag_level, col=c("palegreen2", "khaki"), add=T, method="jitter", pch=19, vertical=T)

boxplot(net.centr$cl~net.centr$c2_ag_level, col="white", las=1, ylab="Closeness", xlab="", names=c("", ""))
stripchart(net.centr$cl~net.centr$c2_ag_level, col=c("palegreen2", "khaki"), add=T, method="jitter", pch=19, vertical=T)

dev.off()


## Significance test

print(wilcox.test(net.centr$dg~net.centr$a2_level))
print(wilcox.test(log(net.centr$bt)~net.centr$a2_level))
print(wilcox.test(net.centr$cl~net.centr$a2_level))
print(wilcox.test(net.centr$dg~net.centr$a2_ag_level))
print(wilcox.test(log(net.centr$bt)~net.centr$a2_ag_level))
print(wilcox.test(net.centr$cl~net.centr$a2_ag_level))


print(wilcox.test(net.centr$dg~net.centr$c2_level))
print(wilcox.test(log(net.centr$bt)~net.centr$c2_level))
print(wilcox.test(net.centr$cl~net.centr$c2_level))
print(wilcox.test(net.centr$dg~net.centr$c2_ag_level))
print(wilcox.test(log(net.centr$bt)~net.centr$c2_ag_level))
print(wilcox.test(net.centr$cl~net.centr$c2_ag_level))

