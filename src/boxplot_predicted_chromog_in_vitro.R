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
# This code will produce the Figure 3c of the manuscript.
# Here we depict a boxplot and a stripchart of the predicted chromogenic activity
# of mutant constructs, compared to reported in vitro activities.
# 
# The output is a SVG file.

mydata = read.table("../datasets/10_folds_classification_result.csv", header=T, sep="\t", quote="")

## Boxplot of median probability

pos = which(mydata$median >= 0.65 | mydata$median <= 0.35)

svg("Figure_3c_boxplot.svg", width = 8, height = 8)

boxplot(mydata$median[pos]~mydata$truth[pos], col="white", las=1, cex.axis=1.3, cex.lab=1.3, xlab="", ylab="Chromogenic Score")

stripchart(mydata$median[pos]~mydata$truth[pos], add=T, method="jitter", vertical=T, pch=19, col=c("red", "blue"))
stripchart(mydata$median[-pos]~mydata$truth[-pos], add=T, method="jitter", vertical=T, pch=19, col="grey")

dev.off()