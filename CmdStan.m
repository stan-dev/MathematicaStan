(*================================================================
 * CmdStan Package
 * A Mathematica package to interact with CmdStan
 * https://github.com/vincent-picaud/MathematicaStan
 * Author: Picaud Vincent, picaud.vincent at gmail.com
 *================================================================
 *)

BeginPackage["CmdStan`"];

Unprotect @@ Names["CmdStan`*"];
ClearAll @@ Names["CmdStan`*"];

(*==================================================================*)
(* Exported functions                                               *)
(*==================================================================*)

(*------------------------------------------------------------------*)
(* Argument Patterns                                                *)
(*------------------------------------------------------------------*)

StanOptionQ::usage = "StanOptionQ[any] checks form {name, value}";
StanOptionListQ::usage = "StanOptionQ[any] checks form {{name, value},{name, value}, ...}";
StanIndexedHeaderElementQ::usage = "StanIndexedHeaderElementQ[any] checks form {Integer, name}";

StanIndexedHeaderQ::usage = "StanIndexedHeaderQ[any] checks form {{Integer, name},{Integer, name}, ...}";
StanImportQ::usage = "StanImportQ[any] checks form {{IndexedHeader,MatrixQ,StringQ}}";

(*------------------------------------------------------------------*)
(* RDump                                                            *)
(*------------------------------------------------------------------*)

RDumpExport::usage = "RDumpExport[fileName,data] creates and dumps data into fileName.data.R.\n\nfileName: file name to create, the \".data.R\" extension is added if not initially present.\ndata: is defined by {{\"Name1\",Value1},{\"Name2\",Value2,...}}, where\n  NameX is a String,\n  ValueX is a Number, a Vector or a Matrix.\nReturn: the actual file name.";

(*------------------------------------------------------------------*)
(* Directory & Compile & Run                                        *)
(*------------------------------------------------------------------*)

StanSetDirectory::usage = "StanSetDirectory[installDir] defines where cmdstan is installed, usually something like XXX/cmdstan/.\nSee StanDirectory[].";
StanDirectory::usage = "StanDirectory[] returns the current location of cmdstan.\nSee StanSetDirectory[installDir].";

StanCompile::usage = "StanCompile[stanCodeFileName] compiles the stanCodeFileName Stan source code.\nstanCodeFileName: the Stan source code file, if not initially present the \".stan\" extension is added\n\nReturns a String containing the CmdStan shell command output.";

StanFindOption::usage = "StanFindOption[name,option] finds \"name\" in the \"option\" list.\nReturns option value if present, $Failed if not defined.";

StanSetOption::usage = "StanSetOption[optionListToAdd,optionList] merge \"optionListToAdd\" to the initial \"optionList\".\nNote:If an option was already defined in \"optionList\" its value is overridden.\nReturn: the completed option list.";

StanRun::usage="StanRun[stanExeFileName,option] executes the stanExeFileName with the given option.\nExample: StanRun[\"bernoulli.exe\",StanOptionSample]\nNote:\n   1/ the .exe extension is added if absent\n   2/ if \"output file\" option is not defined, \"output.csv\" is used.\nReturn: a String containing the shell command output";

StanOptionVariational::usage = "Default options for \"variational\" method.";
StanOptionSample::usage = "Default options for \"sample\" method.";
StanOptionOptimize::usage = "Default options for \"optimize\" method.";

(*------------------------------------------------------------------*)
(* Import & Variable extraction                                     *)
(*------------------------------------------------------------------*)

StanImport::usage = "StanImport[outputCSV] imports a Stan output.csv file.\nReturns {indexedHeader,dataMatrix,parameters}.\n  indexedHeader is a list of {column number,column name}.\n  dataMatrix is a Matrix containing the CSV data.\n  parameters is a String containing the unparsed CSV comments.";

StanFindVariableColumn::usage = "StanFindVariableColumn[variableName,indexedHeader]\nNote: StanFindVariableColumn[variableName,import] is also defined, for convenience, when import=StanImport[\"output.csv\"].";

StanFindVariableIndex::usage = "StanFindVariableIndex[variableName,indexedHeader]\nNote: StanFindVariableIndex[variableName,import] is also defined, for convenience, when import=StanImport[\"output.csv\"].";

StanVariableSample::usage = "StanVariableSample[variableName,stanimport,sampleNumber]Extract variable sample.\nNote this is only a wrapper to the more generic function StanVariable[variableName,indexedHeader,dataRow]";
							
StanVariable::usage = "StanVariable[variableName,indexedHeader,dataRow] extracts sample associated to variableName.";

StanVariableSampleMap::usage = "StanVariableSampleMap[variableName, stanimport, funcToMap] maps funcToMap to extracted data sample.";

(*==================================================================*)
(* Error messages                                                   *)
(*==================================================================*)

CmdStanError::stanInternal="Internal error... in function \"`1`\" ";

CmdStanError::stanDirNotFound="CmdStan install directory \"`1`\" not found.";
CmdStanError::stanDirUndefined="CmdStan install directory is undefined.";

CmdStanError::stanCodeNotFound="Stan code \"`1`\" not found.";
CmdStanError::stanExeNotFound="Stan executable \"`1`\" not found.";

CmdStanError::stanCSVFileNotFound="CSV output file \"`1`\" not found.";
CmdStanError::stanCSVFileNotWellFormed="CSV output file \"`1`\" is not well formed.";
CmdStanError::stanCSVFileVarUnknownType="variable \"`1`\" has unknown type.";

CmdStanError::stanOptionMultipleOccurrences="option \"`1`\" has mutliple occurences \"`2`\"";

CmdStanError::stanDataFileNotFound="data file \"`1`\" not found.";
CmdStanError::stanVariableNotFound="variable \"`1`\" not found.";

Begin["`Private`"];

(*==================================================================*)
(* Argument Patterns                                                *)
(*==================================================================*)

(* check form: {name,value} *)
StanOptionQ[any_]:=(Length[any]==2)&&StringQ[any[[1]]];

(* check form: {{name,value},{name,value}...} *)
StanOptionListQ[any_]:=VectorQ[any,StanOptionQ];

(* check form: {Integer,name} *)
StanIndexedHeaderElementQ[any_] := (Length[any] == 2) && 
								   IntegerQ[any[[1]]] && StringQ[any[[2]]]

StanIndexedHeaderQ[any_] := VectorQ[any, StanIndexedHeaderElementQ]

StanImportQ[any_] := (Length[any] == 3) && 
					 StanIndexedHeaderQ[any[[1]]] && MatrixQ[any[[2]], NumberQ] && 
					 StringQ[any[[3]]]

(*==================================================================*)
(* Exportation subroutines *)
(*==================================================================*)

(* Helper to create c(5,6...) from a vector *)
RDumpToStringHelper[V_/;VectorQ[V]]:="c("<>StringTake[ToString[Map[CForm,V]],{2,-2}]<>")";

(* Dump a matrix CAVEAT: transpose *)
RDumpToString[{MatName_,M_}/;StringQ[MatName]&&MatrixQ[M]]:=
		MatName<>" <- structure("<>RDumpToStringHelper[Flatten[Transpose[M]]]<>", .Dim = "<>RDumpToStringHelper[Dimensions[M]]<>")\n";

(* Dump a vector *)
RDumpToString[{VectName_,V_}/;StringQ[VectName]&&VectorQ[V]]:=VectName<>" <- "<>RDumpToStringHelper[V]<>"\n";
(* Dump a scalar *)
RDumpToString[{VarName_,Var_}/;StringQ[VarName]&&NumberQ[Var]]:=VarName<>" <- " <>ToString[Var]<>"\n";

(* Export *)
RDumpExport[fileNameDataR_?StringQ,ListOfNameValue_]:=
		Module[{str,fileNameDataRExt},
			   (* Add .data.R extension if required *)
			   fileNameDataRExt=fileNameDataR;
			   If[FileExtension[fileNameDataRExt]=="",fileNameDataRExt=fileNameDataRExt<>".data.R"];
			   (* Opean file and save data *)
			   str=OpenWrite[fileNameDataRExt];
			   If[str===$Failed,Return[$Failed]];
			   WriteString[str,StringJoin[Map[RDumpToString,ListOfNameValue]]];
			   Close[str];
			   Return[fileNameDataRExt];
		];

(*==================================================================*)
(* Importation subroutines *)
(*==================================================================*)

(* Import data, skipping comment/parameters : #... *)
StanImport[outputCSV_?StringQ]:=
		Module[{parameters,header,data,raw},
			   If[!FileExistsQ[outputCSV],Message[CmdStanError::stanCSVFileNotFound,outputCSV];Return[$Failed];];

			   raw=StringSplit[Import[outputCSV,"Text"],"\n"];
			   parameters = StringJoin[Riffle[Select[raw,StringTake[#,1]=="#"&],"\n"]];
			   data = ImportString[StringJoin[StringJoin[Riffle[Select[raw,StringTake[#,1]!="#"&],"\n"]]],"CSV"];
			   
			   If[(Length[data]<=1)||(Length[Dimensions[data]]!=2),Message[CmdStanError::stanCSVFileNotWellFormed,outputCSV];Return[$Failed];];

			   (* Create indexed header, index=column number *)
			   header=data[[1]];
			   header=Table[{i,header[[i]]},{i,1,Length[header]}];

			   data=N[Drop[data,1]];

			   Return[{header,data,parameters}]
		];

(* Extract column only: Number if scalar, list if array 

StanFindVariableColumn["energy__",t[[1]]]
{7}
StanFindVariableColumn["mu",t[[1]]]
{8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,\
31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47}
 *)
StanFindVariableColumn[variableName_?StringQ, indexedHeader_?StanIndexedHeaderQ] := 
		Module[{column},
			   column = 
			   Select[indexedHeader, 
					  StringMatchQ[#[[2]], 
								   variableName | (variableName ~~ "." ~~ __)] &];
			   If[Length[column] >= 1, Return[column[[All, 1]]]];

			   (* variable not found *)
			   Message[CmdStanError::stanVariableNotFound,variableName];
			   Return[$Failed];
		]

 StanFindVariableColumn[variableName_?StringQ,stanimport_?StanImportQ] :=
		StanFindVariableColumn[variableName,stanimport[[1]]]; 

(* Only relevant for array
usage example:
StanFindVariableIndex[StanFindVariableColumn["mu",t[[1]]],t[[1]]]
{{1,1},{2,1},{3,1},{4,1},{5,1},{1,2},{2,2},{3,2},{4,2},{5,2},{1,3},{2,\
3},{3,3},{4,3},{5,3},{1,4},{2,4},{3,4},{4,4},{5,4},{1,5},{2,5},{3,5},{\
4,5},{5,5},{1,6},{2,6},{3,6},{4,6},{5,6},{1,7},{2,7},{3,7},{4,7},{5,7}\
,{1,8},{2,8},{3,8},{4,8},{5,8}}
 *)
StanFindVariableIndex[variableName_?StringQ, indexedHeader_?StanIndexedHeaderQ] := 
		Module[{extractedColumn,localIndex},
			   extractedColumn=StanFindVariableColumn[variableName,indexedHeader];
			   (* Not found? *)
			   If[extractedColumn===$Failed,Return[$Failed]];
			   (* Extract coordinates *)
			   localIndex = indexedHeader[[extractedColumn]][[All, 2]];
			   localIndex = Map[StringSplit[#, "."] &, localIndex];
			   localIndex = localIndex[[All, 2 ;; Dimensions[localIndex][[2]]]];
			   localIndex = ToExpression[localIndex];
			   Return[localIndex];
		];
 
 StanFindVariableIndex[variableName_?StringQ,
					   stanimport_?StanImportQ] :=
		StanFindVariableIndex[variableName,stanimport[[1]]]; 

 (* Extract variable value *)
 StanVariable[variableName_?StringQ,
			  indexedHeader_?StanIndexedHeaderQ,
			  dataRow_?(VectorQ[#, NumberQ] &)] := 
		Module[{variableColumn, varianbleIndex},
			   variableColumn = 
			   StanFindVariableColumn[variableName, indexedHeader];
			   (* not found *)
			   If[variableColumn === $Failed, Return[$Failed]];
			   (* scalar *)
			   If[Length[variableColumn] == 1, 
				  Return[dataRow[[variableColumn[[1]]]]]];
			   (* arrays: use SpareArray, then Normal, SLOW but always works *)
			   varianbleIndex = 
			   StanFindVariableIndex[variableName, indexedHeader];
			   Return[Normal[SparseArray[Thread [varianbleIndex -> dataRow[[variableColumn]]]]]];
		];

 StanVariableSample[variableName_?StringQ,
					stanimport_?StanImportQ,
					sampleNumber_?IntegerQ] :=
		StanVariable[variableName,stanimport[[1]],stanimport[[2,sampleNumber]]];

 (* Map a function on all variable sample columns *)
 StanVariableSampleMap[variableName_?StringQ, stanimport_?StanImportQ, 
					   funcToMap_] := 
		Module[{variableColumn, extractedData}, 
			   variableColumn = 
			   StanFindVariableColumn[variableName, stanimport[[1]]];
			   (*not found*)
			   If[variableColumn === $Failed, Return[$Failed]];
			   extractedData = 
			   ConstantArray[0, Dimensions[stanimport[[2]]][[2]]];
			   Map[(extractedData[[#]] = funcToMap[stanimport[[2]][[All, #]]]) &, 
																			   variableColumn];
			   Return[StanVariable[variableName, stanimport[[1]], extractedData]]
		];

 (*==================================================================*)
 (* Option management                                                *)
 (*==================================================================*)

 StanFindOptionPosition[name_?StringQ,option_?StanOptionListQ]:=
		Module[{position},

			   position=Position[option,{name,_}];
			   
			   If[position=={},Return[{}]]; 

			   If[Length[position]>1,
				  Message[CmdStanError::stanOptionMultipleOccurrences,name,Map[option[[#]][[1]]&,position]];
				  Return[$Failed]
			   ];

			   Return[position[[1,1]]];
		];

 StanFindOption[name_?StringQ,option_?StanOptionListQ]:=
		Module[{position},
			   position=StanFindOptionPosition[name,option];
			   
			   If[(position=={})||(position===$Failed),Return[$Failed]];
			   Return[option[[position,2]]];
		];

 StanSetOption[optionListToAdd_?StanOptionListQ, optionList_?StanOptionListQ] := 
		Module[{position, completedOptionList, name, value},
			   
			   If[optionListToAdd == {}, Return[optionList]];
			   
			   completedOptionList = optionList;
			   For[i = 1, i <= Length[optionListToAdd], i++,
				   {name, value} = optionListToAdd[[i]];
				   position = StanFindOptionPosition[name, completedOptionList];

				   (* overwrite value if defined, append otherwise *)
				   If[NumberQ[position],
					  completedOptionList[[position, 2]] = value,
					  completedOptionList = Join[completedOptionList,{{name, value}}]];
			   ];
			   
			   Return[completedOptionList];
		]


StanOptionListToString[option_?StanOptionListQ]:=Fold[(#1 <> " " <> #2[[1]] <> "=" <> ToString[#2[[2]]]) &, "", option];

 (*==================================================================*)
 (* Exec subroutines                                                 *)
 (*==================================================================*)

 StanCheckDirectory[dir_]:=
		If[dir=="",Message[CmdStanError::stanDirUndefined];Return[$Failed],
		   If[!DirectoryQ[dir],Message[CmdStanError::stanDirNotFound,dir];Return[$Failed],Return[dir]];
		];

 (* You can modify me (initial configuration) *)
 stanDir="~/GitHub/cmdstan"; 

 StanSetDirectory[dir_]:=If[StringQ[StanCheckDirectory[dir]],stanDir=dir;Return[dir],Return[$Failed];];
 StanDirectory[]:=stanDir;

 StanRemoveFileNameExt[fileName_?StringQ]:=FileNameJoin[{FileNameDrop[fileName,-1],FileBaseName[fileName]}];

 (* stanCodeFileName (with or without .stan extension) *)
 StanCompile[stanCodeFileName_?StringQ]:=
		Module[{currentDir=Directory[],codeFileNameWithExt,pathCodeFileName,command,output},

			   (* Find stan code: code.stan and create path/code.exe (no .stan extension) *)
			   
			   If[FileExtension[stanCodeFileName]=="stan",
				  codeFileNameWithExt=stanCodeFileName,
				  codeFileNameWithExt=stanCodeFileName<>".stan";
			   ];

			   pathCodeFileName=AbsoluteFileName[codeFileNameWithExt];

			   If[pathCodeFileName===$Failed,Message[CmdStanError::stanCodeNotFound,codeFileNameWithExt];Return[$Failed]];

			   pathCodeFileName=StanRemoveFileNameExt[pathCodeFileName];

			   If[$OperatingSystem=="Windows",pathCodeFileName=pathCodeFileName<>".exe"];
			   
			   (* Check stan directory *)

			   If[StanCheckDirectory[stanDir]===$Failed,Return[$Failed]];

			   (* Go into Stan directory and compile! *)

			   SetDirectory[StanDirectory[]];
			   command="make "<>pathCodeFileName;
                           (*Print[StanDirectory[]];     *)
                           output=Import["!"<>command<>" 2>&1","Text"];
			   SetDirectory[currentDir];

			   Return[output];
		];

 StanRun[stanExeFileName_?StringQ,option_?MatrixQ]:=
		Module[{exeFileNameWithExt,pathExeFileName,dataFile,outputFile,mutableOption,command,output},

			   (* Check that prog(.exe) exists *)

			   If[($OperatingSystem=="Windows")&&(FileExtension[stanExeFileName]==""),
				  exeFileNameWithExt=stanExeFileName<>".exe",
				  exeFileNameWithExt=stanExeFileName
			   ];

			   pathExeFileName=AbsoluteFileName[exeFileNameWithExt];

			   If[pathExeFileName===$Failed,Message[CmdStanError::stanExeNotFound,exeFileNameWithExt];Return[$Failed]];

			   (* Check if there is a data file in option, if not, try to create one from scratch *)
			   
			   mutableOption=option;
			   
			   dataFile=StanFindOption["data file",mutableOption];

			   If[dataFile===$Failed,
				  dataFile=StanRemoveFileNameExt[pathExeFileName]<>".data.R";
				  mutableOption=StanSetOption[{{"data file",dataFile}},mutableOption]
			   ];

			   dataFile=AbsoluteFileName[dataFile];

			   If[dataFile===$Failed,Message[CmdStanError::stanDataFileNotFound,StanFindOption["data file",mutableOption]];Return[$Failed]];

			   (* Check output file *)
			   
			   outputFile=StanFindOption["output file",mutableOption];

			   If[outputFile===$Failed,
				  outputFile=FileNameJoin[{Directory[],"output.csv"}];
				  mutableOption=StanSetOption[{{"output file",outputFile}},mutableOption]
			   ];
			   
			   
			   (* Extract options and compute!
				*)
			   command=pathExeFileName<>StanOptionListToString[mutableOption];
			   (*Print["DEBUG ",command];*)
			   output=Import["!"<>command<>" 2>&1","Text"];
			   
			   Return[output];
		];

 (* Default options *)

 StanOptionVariational={{"method","variational"}};
 StanOptionSample={{"method","sample"}};
 StanOptionOptimize={{"method","optimize"}};

 End[];

 Protect @@ Names["CmdStan`*"];

 EndPackage[];
