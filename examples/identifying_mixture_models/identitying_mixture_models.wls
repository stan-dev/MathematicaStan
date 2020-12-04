#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Chapter:: *)
(*Replication/Inspired by*)
(*https://mc-stan.org/users/documentation/case-studies/identifying_mixture_models.html*)


(* ::Subsubsection:: *)
(*(all possible errors in *this*  Mathematica notebook are mine)*)


(* ::Subsection:: *)
(*TODO: check if log_mix is a (1-\[Theta]), \[Theta] mixture or the reverse... I was expecting a \[Theta] , (1-\[Theta])... but for \[Lambda]=0.4, I get  \[Lambda]=0.6 estimation.*)


(* ::Chapter:: *)
(*Stan init*)


(* ::Subsection:: *)
(*load Stan package*)


<<CmdStan`


SetDirectory[$TemporaryDirectory]


(* ::Subsubsection:: *)
(*Insure reproducibility *)


mySeed=1234;
mySampleOptions=SetStanOption[#,"random.seed",mySeed]&@SampleDefaultOptions
myVariationalOptions=SetStanOption[#,"random.seed",mySeed]&@VariationalDefaultOptions


(* ::Text:: *)
(*Do not forget MMA seed!*)


SeedRandom[mySeed]


(* ::Subsubsection:: *)
(*plot options*)


SetOptions[ListLinePlot,LabelStyle->Directive[Bold],ImageSize->400,Frame->True,PlotRange->{Automatic,{-5,5}}];
SetOptions[ListPlot,LabelStyle->Directive[Bold],ImageSize->400,Frame->True,PlotRange->{Automatic,{-5,5}}];


(* ::Chapter:: *)
(*GaussMix: degenerate implementation*)


GaussMixFile=FileNameJoin[{NotebookDirectory[],"Gauss_mix.stan"}]


(* ::Subchapter:: *)
(*Data generation*)


\[Mu]={-2.75,2.75};
\[Sigma]={1,1};
\[Lambda]=0.4;


n=10; (* <- and not 1000 as in the original post *)
z=RandomVariate[BernoulliDistribution[\[Lambda]],n]+1;
y=RandomVariate[NormalDistribution[\[Mu][[#]],\[Sigma][[#]]]]& /@ z;

GaussMixData=<|"N"->n,"y"->y|>;
ExportStanData[GaussMixFile,GaussMixData]


(* ::Subchapter:: *)
(*Stan code*)


(* ::Text:: *)
(*Everything is symmetric, priors etc... priors ares exchangeable*)
(**)
(*Subscript[\[Sigma], i] ~ \[ScriptCapitalN](0,2)*)
(*Subscript[\[Mu], i] ~ \[ScriptCapitalN](0,2)*)
(*\[Theta] ~ \[ScriptCapitalB](5,5) <- 2-mixture \[Theta], 1-\[Theta]*)


GaussMixCode="
data {
 int<lower = 0> N;
 vector[N] y;
}

parameters {
  vector[2] mu;
  real<lower=0> sigma[2];
  real<lower=0, upper=1> theta;
}

model {
 sigma ~ normal(0, 2);
 mu ~ normal(0, 2);
 theta ~ beta(5, 5);
 for (n in 1:N)
   target += log_mix(theta,
                     normal_lpdf(y[n] | mu[1], sigma[1]),
                     normal_lpdf(y[n] | mu[2], sigma[2]));
}
";
ExportStanCode[GaussMixFile,GaussMixCode]
CompileStanCode[GaussMixFile]


(* ::Subchapter:: *)
(*Run*)


GaussMixResultFile = RunStan[GaussMixFile, mySampleOptions]
stanResult = ImportStanResult[GaussMixResultFile]
GraphicsRow[{ListLinePlot[GetStanResult[stanResult,"mu.1"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\)"],ListLinePlot[GetStanResult[stanResult,"mu.2"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\)"],Histogram[GetStanResult[stanResult,"theta"],PlotLabel->"\[Theta]"]}]


(* ::Section:: *)
(*and Variational?*)


(* ::Text:: *)
(*Even more vicious as mode switching is not clearly visible...*)


GaussMixResultFile = RunStan[GaussMixFile, myVariationalOptions]
stanResult = ImportStanResult[GaussMixResultFile]
GraphicsRow[{ListLinePlot[GetStanResult[stanResult,"mu.1"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\)"],ListLinePlot[GetStanResult[stanResult,"mu.2"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\)"],Histogram[GetStanResult[stanResult,"theta"],PlotLabel->"\[Theta]"]}]


(* ::Chapter:: *)
(*GaussMix: breaking symmetries*)


GaussMixNonExchangeableFile=FileNameJoin[{NotebookDirectory[],"Gauss_mix_nonexchangeable.stan"}]


(* ::Subchapter:: *)
(*Data generation: same data as before*)


GaussMixNonExchangeableData=GaussMixData;
ExportStanData[GaussMixNonExchangeableFile,GaussMixData]


(* ::Subchapter:: *)
(*Stan code*)


(* ::Text:: *)
(*Priors ares non-exchangeable:*)


(* ::Text:: *)
(*Everything is symmetric, priors etc... priors ares exchangeable*)
(**)
(*Subscript[\[Sigma], i] ~ \[ScriptCapitalN](0,2)*)
(*Subscript[\[Mu], 1] ~ \[ScriptCapitalN](+4,0.5), Subscript[\[Mu], 2] ~ \[ScriptCapitalN](-4,0.5)*)
(*\[Theta] ~ \[ScriptCapitalB](5,5) <- 2-mixture \[Theta], 1-\[Theta]*)


GaussMixNonExchangeableCode="
data {
 int<lower = 0> N;
 vector[N] y;
}

parameters {
  vector[2] mu;
  real<lower=0> sigma[2];
  real<lower=0, upper=1> theta;
}

model {
 sigma ~ normal(0, 2);
 mu[1] ~ normal(4, 0.5);    // <- here (CAVEAT: for ordered(mu), we impose mu1<m2. Here this is the opposite)
 mu[2] ~ normal(-4, 0.5);   // <- here
 theta ~ beta(5, 5);
 for (n in 1:N)
   target += log_mix(theta,
                     normal_lpdf(y[n] | mu[1], sigma[1]),
                     normal_lpdf(y[n] | mu[2], sigma[2]));
}
";
ExportStanCode[GaussMixNonExchangeableFile,GaussMixNonExchangeableCode]
CompileStanCode[GaussMixNonExchangeableFile]


GaussMixNonExchangeableResultFile = RunStan[GaussMixNonExchangeableFile, mySampleOptions]
stanResult = ImportStanResult[GaussMixNonExchangeableResultFile]
GraphicsRow[{ListLinePlot[GetStanResult[stanResult,"mu.1"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\)"],ListLinePlot[GetStanResult[stanResult,"mu.2"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\)"],Histogram[GetStanResult[stanResult,"theta"],PlotLabel->"\[Theta]"]}]


(* ::Section:: *)
(*and Variational?*)


(* ::Text:: *)
(*Ok...*)


GaussMixNonExchangeableResultFile = RunStan[GaussMixNonExchangeableFile, myVariationalOptions]
stanResult = ImportStanResult[GaussMixNonExchangeableResultFile]
GraphicsRow[{ListLinePlot[GetStanResult[stanResult,"mu.1"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\)"],ListLinePlot[GetStanResult[stanResult,"mu.2"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\)"],Histogram[GetStanResult[stanResult,"theta"],PlotLabel->"\[Theta]"]}]


(* ::Input:: *)
(*\[DoubleDot]*)


(* ::Chapter:: *)
(*GaussMix: forcing an ordering*)


GaussMixOrderedFile=FileNameJoin[{NotebookDirectory[],"Gauss_mix_ordered.stan"}]


(* ::Subchapter:: *)
(*Data generation: same data as before*)


GaussMixOrderedData=GaussMixData;
ExportStanData[GaussMixOrderedFile,GaussMixData]


(* ::Subchapter:: *)
(*Stan code*)


(* ::Text:: *)
(*Priors ares non-exchangeable:*)


(* ::Text:: *)
(*Everything is symmetric, priors etc... priors ares exchangeable*)
(**)
(*Subscript[\[Sigma], i] ~ \[ScriptCapitalN](0,2)*)
(*Subscript[\[Mu], i] ~ \[ScriptCapitalN](0,2) and Subscript[\[Mu], 1]<Subscript[\[Mu], 2]*)
(*\[Theta] ~ \[ScriptCapitalB](5,5) <- 2-mixture \[Theta], 1-\[Theta]*)


GaussMixOrderedCode="
data {
 int<lower = 0> N;
 vector[N] y;
}

parameters {
  ordered[2] mu;           // <-- Here
  real<lower=0> sigma[2];
  real<lower=0, upper=1> theta;
}

model {
 sigma ~ normal(0, 2);
 mu ~ normal(0, 2);    // <- here, symmetry
 theta ~ beta(5, 5);
 for (n in 1:N)
   target += log_mix(theta,
                     normal_lpdf(y[n] | mu[1], sigma[1]),
                     normal_lpdf(y[n] | mu[2], sigma[2]));
}
";
ExportStanCode[GaussMixOrderedFile,GaussMixOrderedCode]
CompileStanCode[GaussMixOrderedFile]


GaussMixOrderedResultFile = RunStan[GaussMixOrderedFile, mySampleOptions]
stanResult = ImportStanResult[GaussMixOrderedResultFile]
GraphicsRow[{ListLinePlot[GetStanResult[stanResult,"mu.1"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\)"],ListLinePlot[GetStanResult[stanResult,"mu.2"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\)"],Histogram[GetStanResult[stanResult,"theta"],PlotLabel->"\[Theta]"]}]


(* ::Section:: *)
(*and Variational?*)


GaussMixOrderedResultFile = RunStan[GaussMixOrderedFile, myVariationalOptions]
stanResult = ImportStanResult[GaussMixOrderedResultFile]
GraphicsRow[{ListLinePlot[GetStanResult[stanResult,"mu.1"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\)"],ListLinePlot[GetStanResult[stanResult,"mu.2"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\)"],Histogram[GetStanResult[stanResult,"theta"],PlotLabel->"\[Theta]"]}]


(* ::Chapter:: *)
(*GaussMix: degenerate + overlaping*)


GaussMixOverlapFile=FileNameJoin[{NotebookDirectory[],"Gauss_mix_overlap.stan"}]


(* ::Subchapter:: *)
(*Data generation*)


\[Mu]={-0.75,0.75}; (* <-- here *)
\[Sigma]={1,1};
\[Lambda]=0.4;


n=1000; (* <- and not 1000 as in the original post *)
z=RandomVariate[BernoulliDistribution[\[Lambda]],n]+1;
y=RandomVariate[NormalDistribution[\[Mu][[#]],\[Sigma][[#]]]]& /@ z;

GaussMixOverlapData=<|"N"->n,"y"->y|>;
ExportStanData[GaussMixOverlapFile,GaussMixOverlapData]


(* ::Subchapter:: *)
(*Stan code*)


(* ::Text:: *)
(*Everything is symmetric, priors etc... priors ares exchangeable*)
(**)
(*Subscript[\[Sigma], i] ~ \[ScriptCapitalN](0,2)*)
(*Subscript[\[Mu], i] ~ \[ScriptCapitalN](0,2)*)
(*\[Theta] ~ \[ScriptCapitalB](5,5) <- 2-mixture \[Theta], 1-\[Theta]*)


GaussMixOverlapCode="
data {
 int<lower = 0> N;
 vector[N] y;
}

parameters {
  vector[2] mu;
  real<lower=0> sigma[2];
  real<lower=0, upper=1> theta;
}

model {
 sigma ~ normal(0, 2);
 mu ~ normal(0, 2);
 theta ~ beta(5, 5);
 for (n in 1:N)
   target += log_mix(theta,
                     normal_lpdf(y[n] | mu[1], sigma[1]),
                     normal_lpdf(y[n] | mu[2], sigma[2]));
}
";
ExportStanCode[GaussMixOverlapFile,GaussMixOverlapCode]
CompileStanCode[GaussMixOverlapFile]


(* ::Subchapter:: *)
(*Run*)


GaussMixOverlapResultFile = RunStan[GaussMixOverlapFile, mySampleOptions]
stanResult = ImportStanResult[GaussMixOverlapResultFile]
GraphicsRow[{
ListLinePlot[GetStanResult[stanResult,"mu.1"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\)"],ListLinePlot[GetStanResult[stanResult,"mu.2"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\)"],
Histogram[GetStanResult[stanResult,"theta"],PlotLabel->"\[Theta]"],
ListPlot[Transpose@{GetStanResult[stanResult,"mu.1"],GetStanResult[stanResult,"mu.2"]}]}]


(* ::Section:: *)
(*and Variational?*)


GaussMixOverlapResultFile = RunStan[GaussMixOverlapFile, myVariationalOptions]
stanResult = ImportStanResult[GaussMixOverlapResultFile]
GraphicsRow[{ListLinePlot[GetStanResult[stanResult,"mu.1"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\)"],ListLinePlot[GetStanResult[stanResult,"mu.2"],PlotLabel->"\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\)"],Histogram[GetStanResult[stanResult,"theta"],PlotLabel->"\[Theta]"]}]



