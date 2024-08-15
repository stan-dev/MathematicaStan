(* ::Package:: *)

(* Notebook to reproduce the small paper usage example *)


(* ::Input:: *)
(*<<CmdStan`*)


(* ::Input:: *)
(*stanCode="*)
(*data { *)
(*  int<lower=0> N; *)
(*  array[N] int<lower=0, upper=1> X;*)
(*} *)
(**)
(*parameters {*)
(*  real<lower=0, upper=1> p;*)
(*}*)
(**)
(*model {*)
(*  p ~ uniform(0, 1);*)
(*  X ~ bernoulli(p);*)
(*}*)
(*";*)
(*stanCodeFile = ExportStanCode["my_program.stan", stanCode]*)


(* ::Input:: *)
(*stanExeFile=CompileStanCode[stanCodeFile]*)


(* ::Input:: *)
(*n = 100; p = 0.4;*)
(*SeedRandom[1234]*)
(*stanData=<|"N"->n,"X"->RandomVariate[BernoulliDistribution[p],n]|>;*)
(*stanDataFile=ExportStanData[stanExeFile,stanData]*)


(* ::Input:: *)
(*stanResultFile=RunStan[stanExeFile,SampleDefaultOptions]*)


(* ::Input:: *)
(*stanResult=ImportStanResult[stanResultFile]*)


(* ::Input:: *)
(*pSample = GetStanResult[stanResult,"p"];*)


(* ::Input:: *)
(*Mean[pSample]*)


(* ::Input:: *)
(*StandardDeviation[pSample]*)


(* ::Input:: *)
(*alphaPosterior = 1 + Total[stanData["X"]]*)
(*betaPosterior = 1 + Length[stanData["X"]]-Total[stanData["X"]]*)
(*posteriorDist=BetaDistribution[alphaPosterior,betaPosterior]*)


(* ::Input:: *)
(*p=Show[Histogram[pSample,Automatic,"PDF",Frame->True,ChartStyle->Orange],Plot[PDF[posteriorDist,p],{p,0.01,1},PlotStyle->Black,PlotRange->All,PlotLabels->{"True"}]];*)
(*p2 = Legended[p,LineLegend[{Orange,Black},{"Sample histogram","Theory"}]]*)


(* ::Input:: *)
(*Export[FileNameJoin[{NotebookDirectory[],"fig100.png"}],p2]*)
