#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*Tutorial notebook*)


(* ::Subchapter:: *)
(*CmdStan package*)


<< CmdStan`


?CmdStan`*


(* ::Chapter:: *)
(*Linear regression*)


(* ::Text:: *)
(*exportFig=True  (* if uncommented saves some figures into ./figures, used by README.org *)*)


(* ::Subchapter:: *)
(*Stan code*)


SetDirectory[$TemporaryDirectory]


stanCode="data
{
  int<lower = 0> N;
  vector[N] x;
  vector[N] y;
}
parameters
{
  real alpha;
  real beta;
  real<lower = 0> sigma;
}
model {
  y ~normal(alpha + beta * x, sigma);
}";



stanCodeFile=ExportStanCode["linear_regression.stan",stanCode]


(* ::Subchapter:: *)
(*Stan exe*)


stanExeFile = CompileStanCode[stanCodeFile,StanVerbose->False]


(* ::Subchapter:: *)
(*Linear regression data*)


\[Sigma]=3;\[Alpha]=1;\[Beta]=2;
n=20;
X=Range[n];
Y=\[Alpha]+\[Beta]*X+RandomVariate[NormalDistribution[0,\[Sigma]],n];
p=Show[Plot[\[Alpha]+\[Beta]*x,{x,Min[X],Max[X]}],ListPlot[Transpose@{X,Y},PlotStyle->Red]]


If[ValueQ[exportFig],Export["~/GitHub/MathematicaStan/figures/linRegData.png",p]]


(* ::Subchapter:: *)
(*Stan data.R file*)


stanData=<|"N"->n,"x"->X,"y"->Y|>;
stanDataFile=ExportStanData[stanExeFile,stanData]


(* ::Subchapter:: *)
(*Run Stan: "method=Optimize"*)


stanResultFile=RunStan[stanExeFile,OptimizeDefaultOptions]


RunStan[stanExeFile,OptimizeDefaultOptions,StanVerbose->False]


OptimizeDefaultOptions


(* ::Subchapter:: *)
(*Load result*)


stanResult=ImportStanResult[stanResultFile]


GetStanResultMeta[stanResult,"lp__"]
\[Alpha]e=GetStanResult[stanResult,"alpha"]
\[Beta]e=GetStanResult[stanResult,"beta"]
\[Sigma]e=GetStanResult[stanResult,"sigma"]


p=Show[Plot[{\[Alpha]e+\[Beta]e*x,\[Alpha]+\[Beta]*x},{x,Min[X],Max[X]},PlotLegends->"Expressions"],ListPlot[Transpose@{X,Y},PlotStyle->Red]]


If[ValueQ[exportFig],Export["~/GitHub/MathematicaStan/figures/linRegEstimate.png",p]]


(* ::Subchapter:: *)
(*Run Stan: "method=Variational"*)


myOpt=VariationalDefaultOptions


myOpt=SetStanOption[myOpt,"output.file",FileNameJoin[{Directory[],"myOutputFile.csv"}]]


myOpt=SetStanOption[myOpt,"method.adapt.iter",123]


GetStanOption[myOpt,"method.adapt.iter"]


myOpt=RemoveStanOption[myOpt,"method.adapt.iter"]


myOutputFile=RunStan[stanExeFile,myOpt,StanVerbose->False]


myResult=ImportStanResult[myOutputFile]


GetStanResult[Mean,myResult,"alpha"]
GetStanResult[Variance,myResult,"alpha"]
GetStanResult[Histogram,myResult,"alpha"]


(* ::Chapter:: *)
(**)


(* ::Chapter:: *)
(*Linear regression, more than one predictor*)


(* ::Subchapter:: *)
(*Stan code*)


stanCode="data {
  int<lower=0> N;   // number of data items
  int<lower=0> K;   // number of predictors
  matrix[N, K] x;   // predictor matrix
  vector[N] y;      // outcome vector
}
parameters {
  real alpha;           // intercept
  vector[K] beta;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
}
model {
  y ~ normal(x * beta + alpha, sigma);  // likelihood
}";


stanCodeFile=ExportStanCode["linear_regression_vect.stan",stanCode];
stanExeFile = CompileStanCode[stanCodeFile,StanVerbose->False];


(* ::Subchapter:: *)
(*Generate data*)


\[Sigma]=3;\[Alpha]=1;\[Beta]1=2;\[Beta]2=0.1;\[Beta]3=0.01;
n=20;
X=Range[n];
Y=\[Alpha]+\[Beta]1*X+\[Beta]2*X^2+\[Beta]3*X^3+RandomVariate[NormalDistribution[0,\[Sigma]],n];
p=Show[Plot[\[Alpha]+\[Beta]1*x+\[Beta]2*x^2+\[Beta]3*x^3,{x,Min[X],Max[X]}],ListPlot[Transpose@{X,Y},PlotStyle->Red]]


If[ValueQ[exportFig],Export["~/GitHub/MathematicaStan/figures/linReg2Data.png",p]]


(* ::Subchapter:: *)
(*Export data*)


stanData=<|"N"->n,"K"->3,"x"->Transpose[{X,X^2,X^3}],"y"->Y|>;
stanDataFile=ExportStanData[stanExeFile,stanData]


(* ::Subchapter:: *)
(*Run Stan: "method=Sample"*)


stanResultFile=RunStan[stanExeFile,SampleDefaultOptions]


(* ::Subchapter:: *)
(*Load the CSV file*)


stanResult=ImportStanResult[stanResultFile]


p=GraphicsRow[GetStanResult[Histogram[#,ImageSize->200]&,stanResult,"beta"]]


If[ValueQ[exportFig],Export["~/GitHub/MathematicaStan/figures/linReg2Histo.png",p]]


StanResultKeys[stanResult]
StanResultMetaKeys[stanResult]


StanResultReducedKeys[stanResult]
StanResultReducedMetaKeys[stanResult]


GetStanResult[Mean,stanResult,"beta.2"]
GetStanResult[Mean,stanResult,"beta"]


Map[#->GetStanResult[stanResult,#]&,StanResultKeys[stanResult]];
Map[ListLinePlot[Values[#],ImageSize->200,PlotLabel->Keys[#]]&,%];
GraphicsGrid[Partition[%,3,3,{1,1},Null]]
