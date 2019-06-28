(* ::Package:: *)

(* :Title: MathematicaStan, a MMA interface to CmdStan *)
(* :Context: CmdStan` *)
(* :Author: Vincent Picaud *)
(* :Date: 2019 *)
(* :Package Version: 1.0 *)
(* :Mathematica Version: 11+ *)
(* :Keywords: Stan, CmdStan, Bayesian *)


BeginPackage["CmdStan`"];

Unprotect @@ Names["CmdStan`*"];
ClearAll @@ Names["CmdStan`*"];


(* ::Chapter:: *)
(*Public declarations*)


(* ::Subchapter:: *)
(*Messages*)


CmdStan::cmdStanDirectoryNotDefined="CmdStan directory does not exist, use SetCmdStanDirectory[dir] to define it. This is something like SetCmdStanDirectory[\"~/GitHub/cmdstan/\"]";
CmdStan::usage="Reserved symbol for error messages";
CmdStan::incorrectFileExtension="Expected \".`1`\", got \".`2`\"";
CmdStan::stanExeNotFound="Stan executable \"`1`\" not found.";
CmdStan::stanOutputFileNotFound="Stan output file \"`1`\" not found.";
CmdStan::stanDataFileNotFound="Stan data file \"`1`\" not found.";
CmdStan::OSsupport="MathematicaStan does not support this OS=`1`";
CmdStan::optionSupport="The option \"`1`\" is not supported in this context";


(* ::Subchapter:: *)
(*Options*)


StanVerbose::usage="A boolean option to define verbosity.";


(* ::Subchapter:: *)
(*Functions*)


$CmdStanConfigurationFile;
GetCmdStanDirectory;
SetCmdStanDirectory;


ExportStanCode;
CompileStanCode;


StanOptions;

OptimizeDefaultOptions;
SampleDefaultOptions;
VariationalDefaultOptions;

StanOptionExistsQ;
GetStanOption;
SetStanOption;
RemoveStanOption;


ExportStanData;


RunStan;


StanResult;
ImportStanResult;


GetStanResult;
GetStanResultMetaData;


MapStanResult;
MapStanResultMetaData;


(* ::Chapter:: *)
(*Private*)


Begin["`Private`"];


(* ::Subchapter:: *)
(*Stan options*)


StanOptions::usage="A structure that holds options";

OptimizeDefaultOptions::usage="Default options for likelihood optimization";
SampleDefaultOptions::usage="Default options for HMC sampling";
VariationalDefaultOptions::usage="Default options for Variational Bayes";

OptimizeDefaultOptions=StanOptions[<|"method"->{"optimize",<||>}|>];
SampleDefaultOptions=StanOptions[<|"method"->{"sample",<||>}|>];
VariationalDefaultOptions=StanOptions[<|"method"->{"variational",<||>}|>];


(* ::Subchapter:: *)
(*Files & Directories*)


$CmdStanConfigurationFile::usage="User configuration file name. Mainly used to store CmdStan directory.";
$CmdStanConfigurationFile=FileNameJoin[{$UserBaseDirectory,"ApplicationData","CmdStanConfigurationFile.txt"}];


GetCmdStanDirectoryQ[]:=FileExistsQ[$CmdStanConfigurationFile]&&DirectoryQ[Import[$CmdStanConfigurationFile]]


GetCmdStanDirectory::usage="GetCmdStanDirectory[] returns CmdStan directory";
GetCmdStanDirectory[]=If[!GetCmdStanDirectoryQ[],Message[CmdStan::cmdStanDirectoryNotDefined];$Failed,Import[$CmdStanConfigurationFile]];


SetCmdStanDirectory::usage="SetCmdStanDirectory[directory_String] modifies CmdStanDirectory";
SetCmdStanDirectory[directory_String]:=(Export[$CmdStanConfigurationFile,directory];directory)


(* ::Subchapter:: *)
(*File extensions helper*)


FileMultipleExtension[fileName_String]:=
	Module[{res},
	       res=StringSplit[FileNameTake@fileName,"."];
	       If[Length[res]==1,
		  res="",
		  res=StringTake[Fold[#1<>"."<>#2&,"",res[[2;;-1]]],2;;-1]
	       ];
	       res
	];


CheckFileNameExtensionQ[fileName_String, expectedExt_String] :=
	Module[{ext,ok},
	       ext = FileMultipleExtension[fileName];
	       ok = ext == expectedExt;
	       If[Not@ok,Message[CmdStan::incorrectFileExtension, expectedExt, ext]];
	       ok];


generateStanExecFileName[stanFileName_String] :=
	Module[{stanExecFileName},
     	       stanExecFileName = FileNameJoin[{DirectoryName[stanFileName], FileBaseName[stanFileName]}];
      	       (* Jeff Patterson Windows fix: originaly with StringReplace[stanExecFileName,"\\"->"/"] TODO*)
      	       If[$OperatingSystem == "Windows",stanExecFileName = stanExecFileName <> ".exe"];  
	       stanExecFileName     	   
	];

generateStanDataFileName[stanFileName_String] :=
	Module[{stanDataFileName},
	       (* caveat: use FixedPoint beacause of .data.R *)
     	       stanDataFileName = FileNameJoin[{DirectoryName[stanFileName], FixedPoint[FileBaseName,stanFileName]}];
     	       stanDataFileName = stanDataFileName <> ".data.R";
      	       stanDataFileName     	   
	];

generateStanOutputFileName[stanFileName_String,processId_Integer?NonNegative] :=
	Module[{stanOutputFileName},
     	       stanOutputFileName = FileNameJoin[{DirectoryName[stanFileName], FileBaseName[stanFileName]}];
     	       If[processId>0,
     	          stanOutputFileName = stanOutputFileName <> "_" <> ToString[processId]
     	       ];
     	       stanOutputFileName = stanOutputFileName <> ".csv";
      	       
	       stanOutputFileName     	   
	];


(* ::Subchapter:: *)
(*Stan code*)


ExportStanCode::usage="ExportStanCode[stanCodeFileName_String, stanCode_String] exports Stan code, return filename WITH path (MMA export generally only returns the file name)";

ExportStanCode[stanCodeFileName_String, stanCode_String]:=
	Module[{oldCode},

	       If[!CheckFileNameExtensionQ[stanCodeFileName,"stan"],Return[$Failed]];

	       (* Note: If file content == stanCode -> do nothing *)
	       If[FileExistsQ[stanCodeFileName],
		  oldCode=Import[stanCodeFileName,"String"],
		  oldCode=""
	       ];
	       If[oldCode!=stanCode,
		  Export[stanCodeFileName,stanCode,"Text"]
	       ];
	       
	       FileNameJoin[{Directory[],stanCodeFileName}]
	];


(* ::Subchapter:: *)
(*Stan code compilation*)


CompileStanCode::usage = "CompileStanCode[stanCodeFileName_String,opts] generates Stan executable (takes some time). Default options {StanVerbose -> True}";

Options[CompileStanCode] = {StanVerbose -> True};

CompileStanCode[stanCodeFileName_String, opts : OptionsPattern[]] :=
	Module[{command, stanExecFileName, tmpFile, verbose, runprocessResult},
	       
	       If[Not@CheckFileNameExtensionQ[stanCodeFileName, "stan"], Return[$Failed]];
	       
	       stanExecFileName = generateStanExecFileName[stanCodeFileName];
	       (* Maybe useful for Windows https://mathematica.stackexchange.com/q/140700/42847 *)
	       command = {"make","-C",GetCmdStanDirectory[],stanExecFileName};
	       
	       verbose = OptionValue[StanVerbose];
	       If[verbose,Print["Running: ",StringRiffle[command," "]]];
	       runprocessResult = RunProcess[command];
	       If[verbose,Print[runprocessResult["StandardOutput"]]];
	       
	       If[runprocessResult["ExitCode"]==0, stanExecFileName, Print[runprocessResult["StandardError"]]; $Failed]   	       
	];


(* ::Section:: *)
(*Convert data to RData, dispatched according to input type*)


(* ::Subsection:: *)
(*Helper that forces scientific notation (use CForm for that)*)


RDumpToStringHelper[V_?VectorQ]:="c("<>StringTake[ToString[Map[CForm,V]],{2,-2}]<>")";


(* ::Subsection:: *)
(*Matrix*)


(* CAVEAT: needs to transpose the matrix to get the right ordering: column major *)
RDumpToString[MatName_String,M_?MatrixQ]:=
	MatName<>" <- structure("<>RDumpToStringHelper[Flatten[Transpose[M]]] <>
	       ", .Dim = "<>RDumpToStringHelper[Dimensions[M]]<>")\n";


(* ::Subsection:: *)
(*Vector*)


RDumpToString[VectName_String,V_?VectorQ]:=VectName<>" <- "<>RDumpToStringHelper[V]<>"\n";


(* ::Subsection:: *)
(*Scalar*)


RDumpToString[VarName_String,Var_?NumberQ]:=VarName<>" <- " <>ToString[Var]<>"\n";


ExportStanData::usage =
"ExportStanData[fileNameDataR_?StringQ,Rdata_Association] creates a .data.R file from an association <|\"variable_name\"->value...|>. value can be a scalar, a vector or a matrix";

ExportStanData[stanFileName_String,Rdata_Association]:=
	Module[{str,stanOutputFileName},
	       (* Add .data.R extension if required *)
	       stanOutputFileName=generateStanDataFileName[stanFileName];
	       (* Open file and save data *)
	       str=OpenWrite[stanOutputFileName];
	       If[str===$Failed,Return[$Failed]];
	       WriteString[str,StringJoin[KeyValueMap[RDumpToString[#,#2]&,Rdata]]];
	       Close[str];
	       stanOutputFileName
	];


(* ::Subchapter:: *)
(*Stan option management (Refactoring: simplify and use StanOption[Association]*)


(* ::Section:: *)
(*Command line string*)


stanOptionToCommandLineString[opt_]:=
	Module[{optList,f,stack={}},
	       f[key_String->{value_,recurse_}]:=key<>"="<>ToString[value]<>" "<>stanOptionToCommandLineString[recurse];
	       f[key_String->{Null,recurse_}]:=key<>" "<>stanOptionToCommandLineString[recurse];
	       f[other_List]:=stanOptionToCommandLineString[other];

	       optList=Normal[opt];
	       Scan[AppendTo[stack,f[#]]&,optList];
	       StringJoin[stack]
	];


Format[StanOptions[opt_Association]] := stanOptionToCommandLineString[opt];


(* ::Section:: *)
(*Split option string*)


splitOptionString[keys_String]:=StringSplit[keys,"."];


(* ::Section:: *)
(*Check option (it it exists)*)


(* ::Text:: *)
(*Needs FoldWhile (see https://mathematica.stackexchange.com/questions/19102/foldwhile-and-foldwhilelist )*)


foldWhile[f_,test_,start_,secargs_List]:=
	Module[{last=start},Fold[If[test[##],last=f[##],Return[last,Fold]]&,start,secargs]];


stanOptionExistsQ[StanOptions[opt_Association],{keys__String}]:=
	Module[{status},foldWhile[Last[#1][#2]&,(status=KeyExistsQ[Last[#1],#2])&,{"",opt},{keys}];status]


StanOptionExistsQ::usage="StanOptionExistsQ[opt_StanOptions,optionString_String] check if the option exists";
stanOptionExistsQ[opt_StanOptions,optionString_String]:=stanOptionExistsQ[opt,splitOptionString[optionString]];


(* ::Section:: *)
(*Get option*)


getStanOption[StanOptions[opt_Association],{keys__String}]:=
	Module[{status,extracted},
	       extracted=foldWhile[Last[#1][#2]&,(status=KeyExistsQ[Last[#1],#2])&,{"",opt},{keys}];
	       If[status,extracted,$Failed]
	];
getStanOptionValue[opt_StanOptions,{keys__String}]:=With[{result=getStanOption[opt,{keys}]},If[result===$Failed,result,First[result]]];


GetStanOption::usage="GetStanOption[opt_StanOptions, optionString_String] return Stan option value, $Failed if the option is not defined";

GetStanOption[opt_StanOptions, optionString_String]:=getStanOptionValue[opt,splitOptionString[optionString]];


(* ::Section:: *)
(*Set option*)


(* ::Text:: *)
(*Compared to the SO answer the Merge associated function nestedMerge[] merge the assoc[[All, 2]] association, value is set to the last not Null element (this is the role of nestedMergeHelper[])*)


nestedMerge[assoc : {__Association}] := Merge[assoc, nestedMerge];
nestedMergeHelper[arg_List] := With[{cleaned = Select[arg, Not[# === Null] &]}, If[cleaned == {}, Null, Last[cleaned]]];
nestedMerge[assoc : {{_, __Association} ..}] := {nestedMergeHelper[assoc[[All, 1]]], Merge[assoc[[All, 2]], nestedMerge]};


setStanOption[StanOptions[org_Association], {keys__String}, value_] := Module[{tmp},
									      tmp = {org, Fold[ <|#2 -> If[AssociationQ[#], {Null, #}, #]|> &, {value, <||>}, Reverse@{keys}]};
									      StanOptions[nestedMerge[tmp]]
								       ];


SetStanOption::usage="SetStanOption[opt_StanOptions, optionString_String, value_] add or overwrite the given Stan option.";
SetStanOption[opt_StanOptions, optionString_String, value_] := setStanOption[opt,splitOptionString[optionString],value];


(* ::Section:: *)
(*Delete option*)


removeStanOption[StanOptions[org_Association], {oneKey_}]:=StanOptions[KeyDrop[org,oneKey]];

removeStanOption[StanOptions[org_Association], {keys__,last_}]:=
	Module[{extracted,buffer,indices},
	       If[StanOptionExistsQ[StanOptions[org], {keys,last}]===False,Return[StanOptions[org]]]; (* nothing to do the key path does not exist *)
	       buffer=org;
	       indices=Riffle[{keys},ConstantArray[2,Length[{keys}]]];
	       KeyDropFrom[buffer[[Apply[Sequence,indices]]],last];
	       buffer=FixedPoint[DeleteCases[# ,{Null,<||>},-1]&,buffer];
	       StanOptions[buffer]
	];


RemoveStanOption::usage="RemoveStanOption[opt_StanOptions, optionString_String] remove the given option.";

RemoveStanOption[opt_StanOptions, optionString_String]:=removeStanOption[opt,splitOptionString[optionString]];


(* ::Section:: *)
(*Some helpers*)


completeStanOptionWithDataFileName[stanFileName_String, stanOption_StanOptions] :=
    	Module[{stanDataFileName},
	       
      	       (* 
      	,* Check if there is a data file name in option, 
      	,* if not, try to create one from scratch 
      	,*)
      	       stanDataFileName = GetStanOption[stanOption,"data.file"];
      	       If[stanDataFileName === $Failed,
		  stanDataFileName = generateStanDataFileName[stanFileName];
               ];
               Assert[CheckFileNameExtensionQ[stanDataFileName, "data.R"]];
	       
               SetStanOption[stanOption,"data.file", stanDataFileName]
        ];

completeStanOptionWithOutputFileName[stanFileName_String, stanOption_StanOptions, processId_?IntegerQ] :=
   	Module[{stanOutputFileName},
	       
               (* 
      	,* Check if there is a output file name in option, 
      	,* if not, try to create one from scratch 
      	,*)
               stanOutputFileName = GetStanOption[stanOption,"output.file"];
               
               If[stanOutputFileName === $Failed,
                  stanOutputFileName = generateStanOutputFileName[stanFileName,processId];
               ];
               Assert[CheckFileNameExtensionQ[stanOutputFileName, "csv"]];
	       
               SetStanOption[stanOption,"output.file", stanOutputFileName]
	];


(* ::Subchapter:: *)
(*Stan Run*)


RunStan::usage="RunStan[stanFileName_String, stanOption_StanOptions, opts : OptionsPattern[]] runs Stan.";

Options[RunStan] = {StanVerbose -> True};

RunStan[stanFileName_String, stanOption_StanOptions, opts : OptionsPattern[]] :=
  	Module[{pathExecFileName, mutableOption, command, output, verbose, runprocessResult },
   	       (* Generate Executable file name (absolute path) 
   		*)
   	       pathExecFileName = generateStanExecFileName[stanFileName];
   	       If[pathExecFileName === $Failed, Return[$Failed]];
	       
   	       (* Generate Data file name (absolute path) and add it to stanOption list *)
   	       mutableOption = completeStanOptionWithDataFileName[pathExecFileName, stanOption];
   	       If[mutableOption === $Failed, Return[$Failed]];
	       
   	       (* Generat Output file name *)
   	       mutableOption = completeStanOptionWithOutputFileName[stanFileName, mutableOption, 0]; (* 0 means -> only ONE output (sequential) *)
   	       
   	       (* Extract stanOptions and compute! *)
   	       command = {pathExecFileName};
   	       command = Join[command,StringSplit[stanOptionToCommandLineString[mutableOption]," "]];
	       
               verbose = OptionValue[StanVerbose];
               If[verbose, Print["Running: ", StringRiffle[command, " "]]];
               runprocessResult = RunProcess[command];
               If[verbose, Print[runprocessResult["StandardOutput"]]];
      	       
   	       If[runprocessResult["ExitCode"] == 0, GetStanOption[mutableOption,"output.file"], Print[runprocessResult["StandardError"]]; $Failed]   	          
  	];


(* ::Subchapter:: *)
(*TODO add Stan Parallel HMC*)


(* TODO: pour l'instant rien fait.... s'inspirer de RunStanOptimize etc...*)
RunStanSample[stanFileName_String,NJobs_/; NumberQ[NJobs] && (NJobs > 0)]:=
	Module[{id,i,pathExecFileName,mutableOption,bufferMutableOption,shellScript="",finalOutputFileName,finalOutputFileNameID,output},

	       (* Initialize with user stanOption  *)
	       mutableOption=Join[immutableStanOptionSample,StanOptionSample[]];

	       If[GetStanOptionPosition["id",mutableOption]!={},
		  Message[RunStanSample::optionNotSupported,"id"];
		  Return[$Failed];
	       ];
               
	       (* Generate Executable file name (absolute path) 
		*)
	       pathExecFileName=generateStanExecFileName[stanFileName];
	       If[pathExecFileName===$Failed,Return[$Failed]];

	       (* Generate Data filen ame (absolute path) and add it to stanOption list
		*)
	       mutableOption=completeStanOptionWithDataFileName[pathExecFileName,mutableOption];
	       If[mutableOption===$Failed,Return[$Failed]];

	       (* Generate script header
		*)
	       If[$OperatingSystem=="Windows",

		  (* OS = Windows 
		   *)
		  Message[CmdStan::OSsupport,$OperatingSystem];
		  Return[$Failed],
		  
		  (* OS = Others (Linux) 
		   *)
		  shellScript=shellScript<>"\n#!/bin/bash";
	       ];

	       (* Generate the list of commands: one command per id
		*  - process id : "id" stanOption
		*  - output filename : "output file" stanOption
		*)
	       For[id=1,id<=NJobs,id++,
		   (* Create output_ID.csv filename *)
		   bufferMutableOption=completeStanOptionWithOutputFileName[stanFileName,mutableOption,id];

		   (* Create the ID=id stanOption *)
		   bufferMutableOption=SetStanOption[{{"id",id}}, bufferMutableOption];

		   (* Form a complete shell comand including the executable *)
		   If[$OperatingSystem=="Windows",

		      (* OS = Windows 
		       *)
		      Message[CmdStan::OSsupport,$OperatingSystem];
		      Return[$Failed],
		      
		      (* OS = Others (Linux) 
		       *)
		      shellScript=shellScript<>"\n{ ("<>pathExecFileName<>" "<>stanOptionToCommandLineString[bufferMutableOption]<>") } &";
		   ];
	       ]; (* For id *)

	       (* Wait for jobs
		*)
	       If[$OperatingSystem=="Windows",

		  (* OS = Windows 
		   *)
		  Message[CmdStan::OSsupport,$OperatingSystem];
		  Return[$Failed],
		  
		  (* OS = Others (Linux) 
		   *)
		  shellScript=shellScript<>"\nwait";
	       ];

	       (* Recreate the correct output file name (id=0 and id=1)
		* id=0 generate the final output file name + bash script filename
		* id=1 generate ths csv header
		*)
	       finalOutputFileName=GetStanOption["output.file",completeStanOptionWithOutputFileName[stanFileName,mutableOption,0]];

	       If[$OperatingSystem=="Windows",

		  (* OS = Windows 
		   *)
		  Message[CmdStan::OSsupport,$OperatingSystem];
		  Return[$Failed],

		  (* OS = Others (Linux) 
		   *)
		  For[id=1,id<=NJobs,id++,
		      finalOutputFileNameID=GetStanOption["output.file",completeStanOptionWithOutputFileName[stanFileName,mutableOption,id]];  
		      If[id==1,    
			 (* Create a unique output file *)
			 shellScript=shellScript<>"\ngrep lp__ " <> finalOutputFileNameID <> " > " <> finalOutputFileName;
		      ];
		      shellScript=shellScript<>"\nsed '/^[#l]/d' " <>  finalOutputFileNameID <> " >> " <> finalOutputFileName;
		  ];
		  (* Export the final script *)
		  finalOutputFileNameID=StanRemoveFileNameExt[finalOutputFileName]<>".sh"; (* erase with script file name *)
		  Export[finalOutputFileNameID,shellScript,"Text"];
		  (* Execute it! *)
		  output=Import["!sh "<>finalOutputFileNameID<>" 2>&1","Text"];
	       ];
	       
	       Return[output];
	];


(* ::Chapter:: *)
(*Import CSV file*)


(* ::Subchapter:: *)
(*Structure*)


StanResult::usage="A structure to store Stan Result";


(* ::Section:: *)
(*Helper for pretty prints of variables*)


makePairNameIndex[varName_String]:=StringSplit[varName,"."] /. {name_String,idx___}:> {name,ToExpression /@ {idx}};


varNameAsString[data_Association]:=
	Module[{tmp},
	       tmp=GroupBy[makePairNameIndex /@ Keys[data],First->Last];
	       tmp=Map[Max,Map[Transpose,tmp],{2}];
	       tmp=Map[First[#]<>" "<>StringRiffle[Last[#],"x"]&,Normal[tmp]];
	       tmp=StringRiffle[tmp,", "];
	       tmp
	];

(*varNameAsString[result_StanResult]:=varNameAsString[First[result]["data"]];*)


(* ::Subchapter:: *)
(*Import routine*)


ImportStanResult::usage="ImportStanResult[outputCSV_?StringQ] import csv stan output file and return a StanResult structure.";

ImportStanResult[outputCSV_?StringQ]:= 
	Module[{data,headerData,headerMeta,stringData,numericData,output},
	       If[!CheckFileNameExtensionQ[outputCSV,"csv"],Return[$Failed]];
	       If[!FileExistsQ[outputCSV],Message[CmdStan::stanOutputFileNotFound,outputCSV];Return[$Failed];];

	       data=Import[outputCSV];
	       data=GroupBy[data,Head[First[#]]&]; (* split string vs numeric *)
	       stringData=data[String];
	       data=KeyDrop[data,String];
	       numericData=Transpose[First[data[]]];
	       data=GroupBy[stringData,StringTake[First[#],{1}]&]; (* split # vs other (header) *)
	       Assert[Length[Keys[data]]==2]; (* # and other *)
	       stringData=data["#"]; (* get all strings beginning by # *)
	       data=First[KeyDrop[data,"#"]];(* get other string = one line which is header *)
	       headerMeta=Select[First[data],(StringTake[#,{-1}]=="_")&];
	       headerData=Select[First[data],(StringTake[#,{-1}]!="_")&];
	       output=<||>;
		     output["filename"]=outputCSV;
	       output["metaData"]=Association[Thread[headerMeta->numericData[[1;;Length[headerMeta]]]]];
	       output["data"]=Association[Thread[headerData->numericData[[Length[headerMeta]+1;;-1]]]];
	       output["internal"]=<|"pretty_print_data"->varNameAsString[output["data"]],
	       "pretty_print_metaData"->varNameAsString[output["metaData"]],
	       "comments"->StringJoin[Riffle[Map[ToString,stringData,{2}],"\n"]]|>;

		   StanResult[output]
	];


Format[StanResult[opt_Association]]:="    file: "<>opt["filename"]<>"\nmetaData: "<>opt["internal"]["pretty_print_metaData"]<>"\n    data: "<>opt["internal"]["pretty_print_data"];


(* ::Subchapter:: *)
(*Get Result*)


(* ::Section:: *)
(*Helper*)


(* ::Text:: *)
(*Given an association try to find the value from the key*)
(*If the key does not exist try to find an array (in the form of key.X.X...)*)
(*If does not exit $Failed*)


createArray[data_Association,varName_String]:=
	Module[{extracted,index,values,dim,array},
	       extracted=KeySelect[data,First[StringSplit[#,"."]]==varName&];
	       If[extracted==<||>,Print["missing key"];Return[$Failed]];
	       If[Keys[extracted]=={varName},Print["is a scalar and not an array"];Return[$Failed]];
	       index=GroupBy[makePairNameIndex /@ Keys[extracted],First->Last];
	       index=index[varName];
	       values=Values[extracted];
	       dim=Map[Max,Transpose[index]];
	       array=ConstantArray["NA",dim];
	       Scan[(array[[Apply[Sequence,Keys[#]]]]=Values[#])&,Thread[index->values]];
	       array
	];


getStanResult[data_Association,varName_String]:=If[KeyExistsQ[data,varName],data[varName],createArray[data,varName]];


(* ::Section:: *)
(*Public*)


GetStanResult::usage="GetStanResult[result_StanResult,varName_String] returns the varName variable";
"GetStanResult[result_StanResult,varName_String,(f_Function|f_Symbol)] returns the varName variable and maps f to each sample, by examples: f=Mean, Variance, ... or Histogram";
GetStanResult[result_StanResult,varName_String] := getStanResult[First[result]["data"],varName];
GetStanResult[result_StanResult,varName_String,(f_Function|f_Symbol)] := Map[f,getStanResult[First[result]["data"],varName],{-2}];


GetStanResultMetaData::usage="StanResultMetaData[res_StanResult,metaVarName_String] return meta data";
GetStanResultMetaData[result_StanResult,metaVarName_String] := getStanResult[First[result]["metaData"],metaVarName];


(* ::Section:: *)
(*Map*)


MapStanResult::usage="MapStanResult[(f_Function|f_Symbol),result_StanResult,varName_String] maps f to each component of the varName variable. Examples: f=Mean, Variance, ... or Histogram";
MapStanResult[(f_Function|f_Symbol),result_StanResult,varName_String] := Map[f,GetStanResult[result,varName],{-2}];

MapStanResultMetaData::usage="MapStanResultMetaData[(f_Function|f_Symbol),result_StanResult,varName_String] maps f to each component of the meta varName variable. Examples: f=Mean, Variance, ... or Histogram";
MapStanResultMetaData[(f_Function|f_Symbol),result_StanResult,varName_String] := Map[f,GetStanResultMetaData[result,varName],{-2}];


(* ::Subsection:: *)
(*With extra function*)


End[]; (* Private *)


Protect @@ Names["CmdStan`*"];

EndPackage[];
