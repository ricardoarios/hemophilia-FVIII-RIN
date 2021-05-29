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
# This code will produce the scatterplot of Figure 3e.
# Here we depict the relative exposure surface area and the predicted chromogenic activities
# of mutant constructs.
#
# It saves the output to a SVG file.

rsa = read.table("../datasets/Relative_Surf_Area_2R7E.csv", sep="\t", header = T)
pred = read.table("../datasets/RIN_pred_chromo.csv", header=T, sep="\t", quote="")

rsa$pred = NA
rsa$domain = NA

rsa$pred[match(pred$pos, rsa$pos)] = pred$pred
rsa$domain[match(pred$pos, rsa$pos)] = pred$domain

svg("Figure_3e_scatterplot.svg", width = 7, height=5)

plot(rsa$rsa[which(rsa$domain == "A1")], rsa$pred[which(rsa$domain == "A1")], las=1, 
     ylab="Predicted Chromogenic Activity", xlab="Relative Exposure", col="#dfcce4",
     ylim=c(0,1), xlim=c(0,1.2), pch=19)
grid()

points(rsa$rsa[which(rsa$domain == "A3")], rsa$pred[which(rsa$domain == "A3")], col="#fcd4d1", pch=19)
points(rsa$rsa[which(rsa$domain == "C1")], rsa$pred[which(rsa$domain == "C1")], col="#bce4e5", pch=19)


tmp = rsa[which(rsa$domain == "A1" | rsa$domain == "A3" | rsa$domain == "C1" ),]

reg1 = lm(tmp$pred~tmp$rsa)
abline(reg1, col="red")

dev.off()

print(paste("Correlation (Spearman):", cor(tmp$rsa, tmp$pred, method = "spearman", use="comp")))

