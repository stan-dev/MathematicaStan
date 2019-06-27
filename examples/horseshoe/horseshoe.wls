#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*Horseshoe demo*)


<<CmdStan`


(* ::Subchapter:: *)
(*Compile code*)


SetDirectory[$TemporaryDirectory]


stanCode="data {
  int<lower=0> n;
  int<lower=0> p;
  matrix[n,p] X;
  vector[n] y;
}
parameters {
  vector[p] beta;
  vector<lower=0>[p] lambda;
  real<lower=0> tau;
  real<lower=0> sigma;
}
model {
  lambda ~ cauchy(0, 1);
  tau ~ cauchy(0, 1);
  for (i in 1:p)
    beta[i] ~ normal(0, lambda[i] * tau);
  y ~ normal(X * beta, sigma);
}";
stanCodeFileName=ExportStanCode["horseshoe.stan",stanCode]


stanExeFileName=CompileStanCode[stanCodeFileName,StanVerbose->True]


(* ::Subchapter:: *)
(*Read and export input data*)


yTest=Import[FileNameJoin[{NotebookDirectory[], "y-test.dat"}],"List"];
yTrain=Import[FileNameJoin[{NotebookDirectory[],"y-train.dat"}],"List"];
XTest= Import[FileNameJoin[{NotebookDirectory[],"X-test.dat"}],"Table"];
XTrain= Import[FileNameJoin[{NotebookDirectory[],"X-train.dat"}],"Table"];


stanDataFilename=ExportStanData[stanCodeFileName,
<|
"n"->Dimensions[XTrain][[1]],
"p"->Dimensions[XTrain][[2]],
"X"->XTrain,
"y"->yTrain|>
]


(* ::Subchapter:: *)
(*Run*)


opt=SampleDefaultOptions;
opt=SetStanOption[opt,"random seed",5]


stanResultFileName=RunStan[stanCodeFileName,opt]


result=ImportStanResult[stanResultFileName]


(* ::Subchapter:: *)
(*Explore result*)


GetStanResult[result,"beta.4"][[1;;10]]
MapStanResult[Histogram,result,"beta.4"]


(* ::Section:: *)
(*Check generalization error*)


Norm[XTrain.MapStanResult[Mean,result,"beta"]-yTrain]/Length[yTrain]
Norm[XTest.MapStanResult[Mean,result,"beta"]-yTest]/Length[yTest]


(* ::Subsection:: *)
(*With OLS*)


betaOLS=LeastSquares[XTrain,yTrain];
Norm[XTrain.betaOLS-yTrain]/Length[yTrain]
Norm[XTest.betaOLS-yTest]/Length[yTest]


(* ::Subsection:: *)
(*For OLS smaller training error but a bigger generalization one*)
