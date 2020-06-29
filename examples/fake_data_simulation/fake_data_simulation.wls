#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Chapter:: *)
(*Replication of*)
(*http://modernstatisticalworkflow.blogspot.com/2017/04/an-easy-way-to-simulate-fake-data-from.html*)


(* ::Subsubsection:: *)
(*(all possible errors/misinterpretations in *this* Mathematica notebook are mine)*)


(* ::Subchapter:: *)
(*Stan init*)


<<CmdStan`


SetDirectory[$TemporaryDirectory]


(* ::Subchapter:: *)
(*Stan code*)


code="
// saved as categorical_model.stan
data {
  int N;
  int P; // number of categories to be estimated
  int y[N]; // outcomes
  int<lower = 0, upper = 1> run_estimation; // a switch to evaluate the likelihood
  real<lower = 0> prior_sd; // standard deviation of the prior on theta
}
parameters {
  vector[P-1] theta_raw;
}
transformed parameters {
  vector[P] theta;
  theta[1] = 0.0;
  theta[2:P] = theta_raw;
}
model {
  // prior
  theta_raw ~ normal(0, prior_sd);
  
  // likelihood, which we only evaluate conditionally
  if(run_estimation==1){
    y ~ categorical(softmax(theta));
  }
}
generated quantities {
  vector[N+10] y_sim;
  for(i in 1:N+10) {
    y_sim[i] = categorical_rng(softmax(theta));
  }
}";
codeFile=FileNameJoin[{NotebookDirectory[],FileBaseName[NotebookFileName[]]<>".stan"}]


(* ::Text:: *)
(*note:*)
(*I modified  *)
(*  vector[N] y_sim*)
(* to *)
(*  vector[N+10] y_sim*)
(*  *)
(* to make clear the difference between the N theta draws and the N+10 category draws (given a fixed theta)*)
(*  *)


(* ::Subchapter:: *)
(*Write code file*)


ExportStanCode[codeFile,code]


(* ::Subchapter:: *)
(*Compile it!*)


CompileStanCode[f]


(* ::Subchapter:: *)
(*Input data: create and save*)


data=<|
"N"->1000,
"P"->5,
"y"->ConstantArray[0,1000],  (* fake data *)
"run_estimation"->0,
"prior_sd"->100|>
ExportStanData[codeFile,data]


(* ::Subchapter:: *)
(*Run stan and load result*)


resultFile = RunStan[codeFile,SampleDefaultOptions]
result = ImportStanResult[resultFile]


(* ::Subchapter:: *)
(*Theta*)


GraphicsRow@GetStanResult[ListPlot[#,PlotRange->All]&,result,"theta"][[2;;-1]]


(* ::Subchapter:: *)
(*Simulated y_sym*)


ysim=GetStanResult[result,"y_sim"];
Dimensions[ysim]


(* ::Text:: *)
(*From Dimension[] we can check that each column is N+10 draws for a given theta vector*)


(* ::Text:: *)
(*The problem with the current vague prior with \[Sigma]=100 leads, after the softmax,  to one category embedding all the probability mass*)
(*Here the first 5 columns (= 5 first realizations of theta with their associated 1010 realization of y_sim)  counts:*)


Table[Counts[ysim[[All,i]]],{i,5}]


(* ::Subchapter:: *)
(*The fix*)


(* ::Text:: *)
(*The prior must have a smaller range, here \[Sigma] = 1, in order to have more uniform category probabilities*)


data=<|
"N"->1000,
"P"->5,
"y"->ConstantArray[0,1000],  (* fake data *)
"run_estimation"->0,
"prior_sd"->1|>; (* <- here new \[Sigma]=1 *)
ExportStanData[codeFile,data];


resultFile = RunStan[codeFile,SampleDefaultOptions]
result = ImportStanResult[resultFile]


(* ::Subchapter:: *)
(*Theta*)


GraphicsRow@GetStanResult[ListPlot[#,PlotRange->All]&,result,"theta"][[2;;-1]]


(* ::Subchapter:: *)
(*The "new" simulated y_sym*)


ysim=GetStanResult[result,"y_sim"];
Dimensions[ysim]


(* ::Text:: *)
(*We see that there is no more a "dominant" category*)


Table[Counts[ysim[[All,i]]],{i,5}]


(* ::Subchapter:: *)
(*Test theta inference*)


(* ::Text:: *)
(*Ground truth for column #1*)


thetaGroundTruth=GetStanResult[result,"theta"][[All,1]]


data=<|
"N"->1000,
"P"->5,
"y"->ysim[[1;;1000,1]],  (* column #1 data, take 1:N sample (and not our previous 1:N+10) *)
"run_estimation"->1, (* switch, now do inference *)
"prior_sd"->1|>; (* <- here new \[Sigma]=1 *)
ExportStanData[codeFile,data];


resultFile = RunStan[codeFile,SampleDefaultOptions]
result = ImportStanResult[resultFile]


GraphicsRow@GetStanResult[Histogram,result,"theta"][[2;;-1]]
thetaGroundTruth[[2;;-1]]



