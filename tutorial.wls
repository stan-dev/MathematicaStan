#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*Tutorial notebook*)


(* ::Subchapter:: *)
(*CmdStan package*)


(* ::Input:: *)
(*<<CmdStan`*)


(* ::Input:: *)
(*?CmdStan`**)


(* ::Chapter:: *)
(*Linear regression*)


(* ::Subchapter:: *)
(*Stan code*)


(* ::Input:: *)
(*SetDirectory[$TemporaryDirectory]*)


(* ::Input:: *)
(*stanCode="data*)
(*{*)
(*  int<lower = 0> N;*)
(*  vector[N] x;*)
(*  vector[N] y;*)
(*}*)
(*parameters*)
(*{*)
(*  real alpha;*)
(*  real beta;*)
(*  real<lower = 0> sigma;*)
(*}*)
(*model {*)
(*  y ~normal(alpha + beta * x, sigma);*)
(*}";*)
(**)


(* ::Input:: *)
(*stanCodeFile=ExportStanCode["linear_regression.stan",stanCode]*)


(* ::Subchapter:: *)
(*Stan exe*)


(* ::Input:: *)
(*stanExeFile = CompileStanCode[stanCodeFile,StanVerbose->False]*)


(* ::Subchapter:: *)
(*Linear regression data*)


(* ::Input:: *)
(*\[Sigma]=3;\[Alpha]=1;\[Beta]=2;*)
(*n=20;*)
(*X=Range[n];*)
(*Y=\[Alpha]+\[Beta]*X+RandomVariate[NormalDistribution[0,\[Sigma]],n];*)
(*Show[Plot[\[Alpha]+\[Beta]*x,{x,Min[X],Max[X]}],ListPlot[Transpose@{X,Y},PlotStyle->Red]]*)
(**)
(**)


(* ::Input:: *)
(*Export["~/GitHub/MathematicaStan/figures/linRegData.png",%]*)


(* ::Subchapter:: *)
(*Stan data.R file*)


(* ::Input:: *)
(*stanData=<|"N"->n,"x"->X,"y"->Y|>;*)
(*stanDataFile=ExportStanData[stanExeFile,stanData]*)


(* ::Subchapter:: *)
(*Run Stan: "method=Optimize"*)


(* ::Input:: *)
(*stanResultFile=RunStan[stanExeFile,OptimizeDefaultOptions]*)


(* ::Input:: *)
(*RunStan[stanExeFile,OptimizeDefaultOptions,StanVerbose->False]*)


(* ::Input:: *)
(*OptimizeDefaultOptions*)


(* ::Subchapter:: *)
(*Load result*)


(* ::Input:: *)
(*stanResult=ImportStanResult[stanResultFile]*)


(* ::Input:: *)
(*GetStanResultMeta[stanResult,"lp__"]*)
(*\[Alpha]e=GetStanResult[stanResult,"alpha"]*)
(*\[Beta]e=GetStanResult[stanResult,"beta"]*)
(*\[Sigma]e=GetStanResult[stanResult,"sigma"]*)


(* ::Input:: *)
(*Show[Plot[{\[Alpha]e+\[Beta]e*x,\[Alpha]+\[Beta]*x},{x,Min[X],Max[X]},PlotLegends->"Expressions"],ListPlot[Transpose@{X,Y},PlotStyle->Red]]*)
(*Export["~/GitHub/MathematicaStan/figures/linRegEstimate.png",%]*)
(**)


(* ::Subchapter:: *)
(*Run Stan: "method=Variational"*)


(* ::Input:: *)
(*myOpt=VariationalDefaultOptions*)


(* ::Input:: *)
(*myOpt=SetStanOption[myOpt,"output.file",FileNameJoin[{Directory[],"myOutputFile.csv"}]]*)


(* ::Input:: *)
(*myOpt=SetStanOption[myOpt,"method.adapt.iter",123]*)


(* ::Input:: *)
(*GetStanOption[myOpt,"method.adapt.iter"]*)


(* ::Input:: *)
(*myOpt=RemoveStanOption[myOpt,"method.adapt.iter"]*)


(* ::Input:: *)
(*myOutputFile=RunStan[stanExeFile,myOpt,StanVerbose->False]*)


(* ::Input:: *)
(*myResult=ImportStanResult[myOutputFile]*)


(* ::Input:: *)
(*GetStanResult[Mean,myResult,"alpha"]*)
(*GetStanResult[Variance,myResult,"alpha"]*)
(*GetStanResult[Histogram,myResult,"alpha"]*)


(* ::Chapter:: *)
(**)


(* ::Chapter:: *)
(*Linear regression, more than one predictor*)


(* ::Subchapter:: *)
(*Stan code*)


(* ::Input:: *)
(*stanCode="data {*)
(*  int<lower=0> N;   // number of data items*)
(*  int<lower=0> K;   // number of predictors*)
(*  matrix[N, K] x;   // predictor matrix*)
(*  vector[N] y;      // outcome vector*)
(*}*)
(*parameters {*)
(*  real alpha;           // intercept*)
(*  vector[K] beta;       // coefficients for predictors*)
(*  real<lower=0> sigma;  // error scale*)
(*}*)
(*model {*)
(*  y ~ normal(x * beta + alpha, sigma);  // likelihood*)
(*}";*)


(* ::Input:: *)
(*stanCodeFile=ExportStanCode["linear_regression_vect.stan",stanCode];*)
(*stanExeFile = CompileStanCode[stanCodeFile,StanVerbose->False];*)


(* ::Subchapter:: *)
(*Generate data*)


(* ::Input:: *)
(*\[Sigma]=3;\[Alpha]=1;\[Beta]1=2;\[Beta]2=0.1;\[Beta]3=0.01;*)
(*n=20;*)
(*X=Range[n];*)
(*Y=\[Alpha]+\[Beta]1*X+\[Beta]2*X^2+\[Beta]3*X^3+RandomVariate[NormalDistribution[0,\[Sigma]],n];*)
(*Show[Plot[\[Alpha]+\[Beta]1*x+\[Beta]2*x^2+\[Beta]3*x^3,{x,Min[X],Max[X]}],ListPlot[Transpose@{X,Y},PlotStyle->Red]]*)
(**)
(**)


(* ::Input:: *)
(*Export["~/GitHub/MathematicaStan/figures/linReg2Data.png",%]*)


(* ::Subchapter:: *)
(*Export data*)


(* ::Input:: *)
(*stanData=<|"N"->n,"K"->3,"x"->Transpose[{X,X^2,X^3}],"y"->Y|>;*)
(*stanDataFile=ExportStanData[stanExeFile,stanData]*)


(* ::Subchapter:: *)
(*Run Stan: "method=Sample"*)


(* ::Input:: *)
(*stanResultFile=RunStan[stanExeFile,SampleDefaultOptions]*)


(* ::Subchapter:: *)
(*Load the CSV file*)


(* ::Input:: *)
(*stanResult=ImportStanResult[stanResultFile]*)


(* ::Input:: *)
(*p=GraphicsRow[GetStanResult[Histogram[#,ImageSize->200]&,stanResult,"beta"]]*)


(* ::Input:: *)
(*Export["~/GitHub/MathematicaStan/figures/linReg2Histo.png",p]*)


(* ::Input:: *)
(*StanResultKeys[stanResult]*)
(*StanResultMetaKeys[stanResult]*)


(* ::Input:: *)
(*StanResultReducedKeys[stanResult]*)
(*StanResultReducedMetaKeys[stanResult]*)


(* ::Input:: *)
(*GetStanResult[Mean,stanResult,"beta.2"]*)
(*GetStanResult[Mean,stanResult,"beta"]*)


(* ::Input:: *)
(*Map[#->GetStanResult[stanResult,#]&,StanResultKeys[stanResult]];*)
(*Map[ListLinePlot[Values[#],PlotLabel->Keys[#]]&,%];*)
(*GraphicsGrid[Partition[%,3,3,{1,1},Null]]*)
(**)
