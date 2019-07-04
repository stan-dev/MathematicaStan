#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*AR demo*)


<<CmdStan`
SetDirectory[NotebookDirectory[]]


Hyperlink["https://mc-stan.org/docs/2_19/stan-users-guide/autoregressive-section.html"]


(* ::Chapter:: *)
(*AR (1) Models*)


(* ::Subchapter:: *)
(*Stan code*)


stanCode="data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  for (n in 2:N)
    y[n] ~ normal(alpha + beta * y[n-1], sigma);
}";
stanCodeFileName=ExportStanCode["AR1.stan",stanCode]
stanExeFileName=CompileStanCode[stanCodeFileName,StanVerbose->False]


(* ::Subchapter:: *)
(*Generate some data and test it*)


\[Alpha]=2;\[Beta]=0.61;\[Sigma]=0.5;
n=500;
(*Y=NestList[(\[Alpha]+\[Beta]*#+RandomVariate[NormalDistribution[0, \[Sigma]]])&,0,n-1];*)
Y=First[RandomFunction[ARProcess[\[Alpha],{\[Beta]},\[Sigma]],{0,n-1}]["ValueList"]];
ListLinePlot[Y, PlotStyle -> Red]


stanData = <|"N" -> n, "y" -> Y|>;
stanDataFile = ExportStanData[stanExeFileName, stanData]


stanResultFileName=RunStan[stanExeFileName,SampleDefaultOptions,StanVerbose->False]


stanResult=ImportStanResult[stanResultFileName]


infered=Association[Thread[StanResultKeys[stanResult] -> Map[GetStanResult[Mean,stanResult,#]&,StanResultKeys[stanResult]]]]
truth=Association[Thread[StanResultKeys[stanResult] -> {\[Alpha],\[Beta],\[Sigma]}]]


Map[(keyName=#;GetStanResult[Histogram[#,ImageSize->300,PlotLabel->keyName]&,stanResult,#])&,StanResultKeys[stanResult]]
