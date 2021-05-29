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
# This code will produce the scatterplot of Figure 3b.
# Here we depict the correlation of the output of different classifiers.
# Each classifier produces the predicted probability of severe or mild/moderate hemophilia symptoms.
#
# The output is a SVG file.

mydata = read.table("../datasets/10_folds_classification_result.csv", header=T, sep="\t", quote="")

### Correlation plots

svg("Figure_3b_correlations.svg", width = 12, height = 4)

par(mfrow=c(1,3))

cor1 = format(round(cor(mydata$High.prob.naive, mydata$High.prob.xgboost), 2), nsmall=2)

plot(mydata$High.prob.naive, mydata$High.prob.xgboost, las=1, xlab = "Naive Bayes", ylab="XGBoost", 
     cex.lab=1.3, cex.axis=1.3, cex.main=1.3,
     main=paste("Correlation:", cor1))
abline(lm(mydata$High.prob.xgboost~mydata$High.prob.naive), col="red")
grid(lwd = 1.2)

cor2 = format(round(cor(mydata$High.prob.svrrad, mydata$High.prob.xgboost), 2), nsmall=2)

plot(mydata$High.prob.svrrad, mydata$High.prob.xgboost, las=1, xlab = "SVM (Radial)", ylab="XGBoost", 
     cex.lab=1.3, cex.axis=1.3, cex.main=1.3,
     main=paste("Correlation:", cor2))
abline(lm(mydata$High.prob.xgboost~mydata$High.prob.svrrad), col="red")
grid(lwd = 1.2)

cor3 = format(round(cor(mydata$High.prob.naive, mydata$High.prob.svrrad), 2), nsmall=2)

plot(mydata$High.prob.svrrad, mydata$High.prob.naive, las=1, xlab = "SVM (Radial)", ylab="Naive Bayes", 
     cex.lab=1.3, cex.axis=1.3, cex.main=1.3,
     main=paste("Correlation:", cor3))
abline(lm(mydata$High.prob.naive~mydata$High.prob.svrrad), col="red")
grid(lwd = 1.2)

dev.off()