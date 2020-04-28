#Load Package
library(PortfolioAnalytics)

#Initialize Portfolio Object
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

#Load edhec data
data("edhec")

#Use the first 4 columns in edhec for a returns object
returns<-edhec[,1:4]

colnames(returns)<-c("CA","CTAG","DS","EM")

#Print first 5 rows of returns
print(head(returns,5))
#Assign names of the securities to fund.names
fund.names<-colnames(returns)

#Create a portfolio Object

#The portfolio object is instantiated with the portfolio.spec function. The main argument to
#portfolio.spec is assets, this is a required argument. The assets argument can be a scalar value
#for the number of assets, a character vector of fund names, or a named vector of initial weights. If
#initial weights are not specified, an equal weight portfolio will be assumed.
#The pspec object is an S3 object of class ”portfolio”. When first created, the portfolio object
#has an element named assets with the initial weights, an element named category_labels, an
#element named weight_seq with sequence of weights if specified, an empty constraints list and an
#empty objectives list.

#Specify a portfolio object by passing a charactor vector for the 
#assets argument
portfolio.spec(assets = fund.names)
print.default(pspec)

#Add constraint to the Portfolio Object

#Use add.contraint

#Sum of Weight Constraint

#The weight_sum constraint specifies the constraint on the sum of the weights. Aliases for the
#weight_sum constraint type include weight and leverage. Here we add a constraint that the
#weights must sum to 1, or the full investment constraint.


#There are two special cases for the leverage constraint:
# 1.The sum of the weights equal 1, i.e. the full investment constraint. 
#The full investment constraint can be specified with type="full_investment". This automatically sets min_sum=1
#and max_sum=1.
#2.The sum of the weights equal 0, i.e. the dollar neutral or active constraint. This constraint
#can be specified with type="dollar_neutral" or type="active".


pspec<-add.constraint(portfolio=pspec, type="weight_sum",min_sum=1,max_sum=1)

#Add Box Constraint

#Box constraints allows the user to specify upper and lower bounds on the weights of the assets
#Add minimum weight of any asset to be >= 0.05 and max weight <=0.4
#If min and max are not specified, a minimum weight of 0 and maximum weight of 1 are assumed.

pspec<-add.constraint(portfolio=pspec, type="box",min=0.05,max=0.4)

#Add Group Constraint

#Group constraints allow the user to specify the the sum of weights by group.

#The following code groups the assets such that the first 3 assets are grouped together labeled GroupA and the fourth
#asset is in its own group labeled GroupB. 

#The group_min argument specifies that the sum of the weights in GroupA must be greater than or equal to 0.1 
#and the sum of the weights in GroupB must be greater than or equal to 0.15. The group_max argument specifies 
#that the sum of the weights in GroupA must be less than or equal to 0.85 and the sum of the weights in GroupB must
#be less than or equal to 0.55.The group_labels argument is optional and is useful if groups is not
#a named list for labeling groups in terms of market capitalization, sector, etc

pspec<-add.constraint(portfolio=pspec,type="group",groups=list(groupA=c(1,2,3),grouB=4),group_min=c(0.1,0.15),group_max=c(0.85,0.55))






