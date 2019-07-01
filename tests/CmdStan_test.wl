(* ::Package:: *)

SetDirectory["/tmp"]


allTests={};

doTest[f_,output_]:=Block[{testResult},
testResult=VerificationTest[f,output];
AppendTo[allTests,testResult];

If[testResult["Outcome"]=="Success",
	
	Print["OK: ",f],
	
	Print[Style["FAILURE",40,Bold,Red]];
	Print["Result  : ",f];
	Print["Expected: ",output];
	];

AppendTo[allTests,testResult];
Return[f];
]; 


(* ::Chapter:: *)
(*Tests*)


On[Assert]
<<CmdStan`
?"CmdStan`*"


host=Import["!hostname 2>&1","Text"];
If[host=="is231575",
SetCmdStanDirectory["/home/picaud/ExternalSoftware/cmdstan"]
];


doTest[FileExistsQ@$CmdStanConfigurationFile,True];


doTest[CmdStan`Private`generateStanExecFileName["/tmp/bernoulli"], If[$OperatingSystem == "Windows","/tmp/bernoulli.exe","/tmp/bernoulli"]]; 

doTest[CmdStan`Private`generateStanDataFileName["/tmp/bernoulli.stan"],"/tmp/bernoulli.data.R"];
doTest[CmdStan`Private`generateStanOutputFileName["/tmp/bernoulli.stan",0], "/tmp/bernoulli.csv"];
doTest[CmdStan`Private`generateStanOutputFileName["/tmp/bernoulli.stan",1], "/tmp/bernoulli_1.csv"];


doTest[CmdStan`Private`CheckFileNameExtensionQ["/tmp/bernoulli.data.R","data.R"],True];
doTest[CmdStan`Private`CheckFileNameExtensionQ["/tmp/bernoulli.data","data"],True];
doTest[CmdStan`Private`CheckFileNameExtensionQ["/tmp/bernoulli",""],True]


OptimizeDefaultOptions


doTest[opt=CmdStan`Private`completeStanOptionWithDataFileName["/tmp/bernoulli.stan",OptimizeDefaultOptions],StanOptions[ <|"method"->{"optimize",<||>},"data"->{Null,<|"file"->{"/tmp/bernoulli.data.R",<||>}|>}|>]];
doTest[GetStanOption[opt,"data.file"],"/tmp/bernoulli.data.R"];
doTest[opt=CmdStan`Private`completeStanOptionWithOutputFileName["/tmp/bernoulli.stan",OptimizeDefaultOptions,0],StanOptions[ <|"method"->{"optimize",<||>},"output"->{Null,<|"file"->{"/tmp/bernoulli.csv",<||>}|>}|>]];
doTest[GetStanOption[opt,"output.file"],"/tmp/bernoulli.csv"];
doTest[opt=CmdStan`Private`completeStanOptionWithOutputFileName["/tmp/bernoulli.stan",OptimizeDefaultOptions,3],StanOptions[ <|"method"->{"optimize",<||>},"output"->{Null,<|"file"->{"/tmp/bernoulli_3.csv",<||>}|>}|>]];
doTest[GetStanOption[opt,"output.file"],"/tmp/bernoulli_3.csv"];


(* ::Subchapter:: *)
(*Create stan code*)


stanCode="data { 
  int<lower=0> N; 
  int<lower=0,upper=1> y[N];
} 
parameters {
  real<lower=0,upper=1> theta;
} 
model {
  theta ~ beta(1,1);
  for (n in 1:N) 
    y[n] ~ bernoulli(theta);
}";

doTest[ExportStanCode["bernoulli.st an",stanCode],$Failed];
doTest[stanCodeFileName=ExportStanCode["bernoulli.stan",stanCode], "/tmp/bernoulli.stan"];


(* ::Subchapter:: *)
(*Compile Stan code*)


<<CmdStan`


stanCodeFileName


?CompileStanCode


doTest[CompileStanCode[stanCodeFileName,StanVerbose->False],"/tmp/bernoulli"];
doTest[CompileStanCode[stanCodeFileName,StanVerbose->True],"/tmp/bernoulli"];


(* ::Subchapter:: *)
(*StanOptions*)


(* ::Text:: *)
(*A Revoir: GetStanOptionVariational et StanOptionVariational redondant + utiliser une Association TODO*)


<<CmdStan`


?GetStanOption


doTest[opt=SetStanOption[VariationalDefaultOptions,"method.optimize.iter", 2016],StanOptions[<|"method"->{"variational",<|"optimize"->{Null,<|"iter"->{2016,<||>}|>}|>}|>]];
doTest[RemoveStanOption[opt,"method.optimize.iter"],StanOptions[<|"method"->{"variational",<||>}|>]];


(* ::Subchapter:: *)
(*Export data for Bernoulli*)


n=1000;
data=Table[Random[BernoulliDistribution[0.2019]],n];
doTest[ExportStanData[stanCodeFileName,<|"N"->n,"y"->data|>],"/tmp/bernoulli.data.R"];


(* ::Subchapter:: *)
(*RunStan*)


<<CmdStan`


?RunStan


doTest[RunStan[stanCodeFileName,SampleDefaultOptions,StanVerbose->False],"/tmp/bernoulli.csv"];


doTest[RunStan[stanCodeFileName,VariationalDefaultOptions,StanVerbose->False],"/tmp/bernoulli.csv"];


doTest[outputOptimize=RunStan[stanCodeFileName,OptimizeDefaultOptions],"/tmp/bernoulli.csv"];


(* ::Subchapter:: *)
(*Test import result*)


<<CmdStan`


res=ImportStanResult[outputOptimize]
doTest[res // First // Keys,{"filename","meta","parameter","internal"}];


doTest[MatchQ[GetStanResult[res,"theta"],{_Real}],True]


GetStanResult[res,"theta"]


doTest[GetStanResultMeta[res,"lp_"],$Failed];
doTest[MatchQ[GetStanResultMeta[res,"lp__"],{_Real}],True];


(* ::Subchapter:: *)
(*More tests with mixture (to test Matrix import instead of scalar)*)


stanCode="data {
  int<lower=0> N;  // number of data points
  int<lower=1> D;  // number of dimensions
  int<lower=1> K;  // number of clusters
  vector[D] y[N];  // observations
}
transformed data {
  real<upper=0> neg_log_K;
  neg_log_K <- -log(K);
}
parameters {
  vector[D] mu[K]; // cluster means
}
transformed parameters {
  real<upper=0> soft_z[N,K]; // log unnormalized cluster assigns
  for (n in 1:N)
    for (k in 1:K)
      soft_z[n,k] <- neg_log_K - 0.5 * dot_self(mu[k] - y[n]);
}
model {
  for (k in 1:K)
    mu[k] ~ normal(0,1);  // prior
  for (n in 1:N)
    increment_log_prob(log_sum_exp(soft_z[n])); // likelihood
}";
stanCodeFileName=ExportStanCode["soft-k-means.stan",stanCode]
stanExeFileName=CompileStanCode[stanCodeFileName,StanVerbose->False]


(* ::Section:: *)
(*Create some fake data*)


y=Partition[{0.201299862948504, -0.201547030912402, -0.307722736221928, 
-0.113909121429762, -0.317385076246514, 0.486887253813445, 0.216396070510692, 
-0.522504962507574, 0.0999884487329546, -0.374560249344118, 1.25230228963982, 
1.32117236839097, -1.78912278548354, -1.12320907618655, 0.298757334386702, 
-0.643018973086524, -0.970193822958063, 1.8359248505943, -1.08397171023892, 
-0.681463669080701, 1.37841166199148, 0.250145455841524, 1.55188888550783, 
-0.94837059139759, -0.0785677710615314, 0.630262017843849, 0.0177509756762819, 
0.0587175513653272, 1.5253088832499, 2.02971210407677, 1.48153547159389, 
1.32561539997964, 1.61868319591135, 2.57511950976033, 1.90533033463626, 
-0.0392859071131051, 0.982223977047954, 0.224932037330876, 3.96756623395838, 
0.56961898141743, 1.19038971367083, 1.46030520624806, -0.988041068794612, 
-0.13710499660733, -0.516173823435649, 1.73514001999303, 1.42450213310636, 
2.42588178197086, 1.36078286104211, 2.00757973002483, 2.38142457257356, 
1.9219710523873, 2.08190781987337, 0.385829858429519, 1.26125524130875, 
-0.133405525984022, 1.38778980737774, 1.08058596458552, 0.574498349889681, 
3.32234174024842, 0.0497299209801201, 2.9224726963958, 1.77478377276629, 
1.86667338790026, 0.926994133015224, 2.88792811797693, 0.77455586366817, 
-0.113502944729109, 3.03022684464381, 2.41360211089075, -0.286314710512999, 
0.33626664500527, -0.949983239516864, 0.601482350292746, 0.731212313372704, 
-0.203496038340392, -0.11075627749627, 0.449160875122631, -2.01370769271222, 
-1.87291161344744, -1.17886113657795, -1.4965120578889, -0.161628522682308, 
-0.274340276559541, -0.865522537953758, -0.415682006424134, 1.74089559055984, 
-1.73086729463254, 0.959102863168718, -1.82543152975414, -1.73311360385246 },8];
Dimensions[y]
toExport=<|"N"->Length[y],"D"->Last[Dimensions[y]],"K"->5,"y"->y|>;


ExportStanData[stanExeFileName,toExport]


doTest[stanOutputFilename=RunStan[stanCodeFileName,VariationalDefaultOptions,StanVerbose->False],"/tmp/soft-k-means.csv"];


result=ImportStanResult[stanOutputFilename]
doTest[Dimensions[GetStanResult[result,"mu"]],{toExport["K"],Last[Dimensions[y]],1001}]


(* ::Chapter:: *)
(*Results*)


TestReport[allTests]
