(* ::Package:: *)

(*****************************************************************
 * CmdStan Package
 *
 * A Mathematica package to interact with CmdStan
 *
 * Author: Picaud Vincent, picaud.vincent at gmail.com
 *
 * AUTOMATICALLY GENERATED from cmdStan.org 
 ******************************************************************)

BeginPackage["CmdStan`"];

Unprotect @@ Names["CmdStan`*"];
ClearAll @@ Names["CmdStan`*"];

Begin["`Private`"];


StanDirectory::notFound="CmdStan install directory \"`1`\" not found.";
StanDirectory::undefined="CmdStan install directory is undefined.";

StanCheckDirectory[dir_]:=
	Which[
		dir=="",          Message[StanDirectory::undefined];Return[$Failed],
		!DirectoryQ[dir], Message[StanDirectory::notFound,dir];Return[$Failed],
		True,             Return[dir]
	];

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 * !! YOU CAN MODIFY ME (initial configuration)                  !!
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 *)
stanDir="~/GitHub/cmdstan"; 

CmdStan`StanSetDirectory::usage=
"StanSetDirectory[directory_?StringQ] modifies CmdStan command path. Also see StanDirectory[]";
(*
*)
StanSetDirectory[dir_?StringQ]:=
If[StringQ[StanCheckDirectory[dir]],
stanDir=dir; Return[dir],
Return[$Failed];
];

CmdStan`StanDirectory::usage=
"StanDirectory[] returns current CmdStan path.";
(*
*)
StanDirectory[]:=stanDir;
CmdStan`StanCodeExport::usage=
"StanCodeExport[stanCodeFileName_?StringQ, stanCode_?StringQ] exports Stan code into an \"stanCodeFileName.stan\" file, if the output file is identical to \"stanCode\" does nothing (this prevents from useless recompilations)";
(*
 *)
StanCodeExport[stanCodeFileName_?StringQ, stanCode_?StringQ]:=
  Module[{codeFileNameWithExt,oldCode},

  If[FileExtension[stanCodeFileName]=="stan",
    codeFileNameWithExt=stanCodeFileName,
    codeFileNameWithExt=stanCodeFileName<>".stan"
    ];

  (* If file content == stanCode -> do nothing 
   *)
  If[FileExistsQ[codeFileNameWithExt],
   oldCode=Import[codeFileNameWithExt,"String"];
   If[oldCode==stanCode,
     Return[codeFileNameWithExt];
     ];
  ];

  Export[codeFileNameWithExt,stanCode,"Text"];

  Return[codeFileNameWithExt];
];
  
  
(* Private *)
StanRemoveFileNameExt[fileName_?StringQ]:=FileNameJoin[{FileNameDrop[fileName,-1],FileBaseName[fileName]}];

CmdStan`StanCompile::usage=
"StanCompile[stanCodeFileName_?StringQ]"<>
"\nNote: if the program file has no extension, the \".stan\" extension is added"					
(*
 *)
StanCompile::stanCodeNotFound="Stan code \"`1`\" not found.";
(*
 *)
StanCompile[stanCodeFileName_?StringQ]:=
	Module[{currentDir=Directory[],codeFileNameWithExt,pathCodeFileName,command,output},

	       (* Find Stan code: code.stan and create path/code.exe (no .stan extension) *)
	       
	       If[FileExtension[stanCodeFileName]=="stan",
		  codeFileNameWithExt=stanCodeFileName,
		  codeFileNameWithExt=stanCodeFileName<>".stan";
	       ];

	       pathCodeFileName=AbsoluteFileName[codeFileNameWithExt];

	       If[pathCodeFileName===$Failed,Message[StanCompile::stanCodeNotFound,codeFileNameWithExt];Return[$Failed]];

	       pathCodeFileName=StanRemoveFileNameExt[pathCodeFileName];

	       (* Jeff Patterson Windows fix *)
	       If[$OperatingSystem=="Windows",pathCodeFileName=StringReplace[pathCodeFileName,"\\"->"/"]<>".exe"];
	       
	       (* Check Stan directory *)

	       If[StanCheckDirectory[stanDir]===$Failed,Return[$Failed]];

	       (* Go into Stan directory and compile! *)

	       SetDirectory[StanDirectory[]];
	       command="make "<>pathCodeFileName;

               output=Import["!"<>command<>" 2>&1","Text"];
	       SetDirectory[currentDir];

	       Return[output];
	];


(*
 * Default option values
 *)
immutableStanOptionVariational={{"method","variational"}};
immutableStanOptionSample={{"method","sample"}};
immutableStanOptionOptimize={{"method","optimize"}};

(*
 * Current option values
 *)
currentStanOptionVariational={};
currentStanOptionSample={};
currentStanOptionOptimize={};


(* check form: {name,value} *)
StanOptionQ[any_]:=
	(Length[any]==2)&&
	StringQ[any[[1]]];

(* check form: {{name,value},{name,value}...} *)
StanOptionListQ[any_]:=VectorQ[any,StanOptionQ];

(* Find option position, $Failed if not found 
*)
StanGetOptionPosition::multipleOccurrences="option \"`1`\" has mutliple occurences \"`2`\"";
(*
 *)
StanGetOptionPosition[name_?StringQ,option_?StanOptionListQ]:=
	Module[{position},

	       position=Position[option,{name,_}];
	       
	       If[position=={},Return[{}]]; 

	       If[Length[position]>1,
		  Message[StanGetOptionPosition::multipleOccurrences,
			  name,
			  Map[option[[#]][[1]]&,position]];
		  Return[$Failed]
	       ];
	       
	       Return[position[[1,1]]];
	];

StanGetOption[name_?StringQ,option_?StanOptionListQ]:=
		Module[{position},
			   position=StanGetOptionPosition[name,option];
			   
			   If[(position=={})||(position===$Failed),
				  Return[$Failed]
			   ];

			   Return[option[[position,2]]];
		];

StanRemoveOption[name_?StringQ,option_?StanOptionListQ]:=
		Module[{position},
			   position=StanGetOptionPosition[name,option];

			   (* If not found, do nothing *)
			   If[position===$Failed,
				  Return[$Failed],
				  Return[Drop[option,position]];
			   ];
		];

StanSetOption[optionListToAdd_?StanOptionListQ, optionList_?StanOptionListQ] := 
	Module[{position, completedOptionList, name, value,i},
	       
	       If[optionListToAdd == {}, Return[optionList]];
	       
	       completedOptionList = optionList;
	       For[i = 1, i <= Length[optionListToAdd], i++,
		   {name, value} = optionListToAdd[[i]];
		   position = StanGetOptionPosition[name, completedOptionList];

		   (* overwrite value if defined, append otherwise *)
		   If[NumberQ[position],
		      completedOptionList[[position, 2]] = value,
		      completedOptionList = Join[completedOptionList,{{name, value}}]];
	       ];
	       
	       Return[completedOptionList];
	]

StanOptionListToString[option_?StanOptionListQ]:=
	Fold[(#1 <> " " <> #2[[1]] <> "=" <> ToString[#2[[2]]]) &, "", option];

(*
* User interface
*)

CmdStan`StanGetOptionVariational::usage=
"StanGetOptionVariational[name_?StringQ] returns option associated value for the Variational method, $Failed if not found";
StanGetOptionVariational[name_?StringQ]:=StanGetOption[name,currentStanOptionVariational];

CmdStan`StanRemoveOptionVariational::usage=
"StanRemoveOptionVariational[optionName_?StringQ] returns option list where the \"optionName\" option has been removed,"<>
" if \"optionName\" is not found return an unmodified option list.";
StanRemoveOptionVariational[optionName_?StringQ]:=
		(currentStanOptionVariational=StanRemoveOption[optionName,currentStanOptionVariational]);

CmdStan`StanSetOptionVariational::usage=
"StanSetOptionVariational[name_?StringQ,value_] sets option -> value for the Variational method";
StanSetOptionVariational[name_?StringQ,value_]:=
currentStanOptionVariational=StanSetOption[{{name,value}},currentStanOptionVariational];


CmdStan`StanOptionVariational::usage=
"StanOptionVariational[] returns complete list of options for the Variational method";
StanOptionVariational[]:=currentStanOptionVariational;

CmdStan`StanResetOptionVariational::usage=
"StanResetOptionVariational[] resets to default and returns complete list of default options for the Variational method";
StanResetOptionVariational[]:=currentStanOptionVariational={};

(*~~~~~~~~~~~~~~~~*)

CmdStan`StanGetOptionSample::usage=
"StanGetOptionSample[name_?StringQ] returns option associated value for the Sample method, $Failed if not found";
StanGetOptionSample[name_?StringQ]:=StanGetOption[name,currentStanOptionSample];

CmdStan`StanRemoveOptionSample::usage=
"StanRemoveOptionSample[optionName_?StringQ] returns option list where the \"optionName\" option has been removed,"<>
" if \"optionName\" is not found return an unmodified option list.";
StanRemoveOptionSample[optionName_?StringQ]:=
		(currentStanOptionSample=StanRemoveOption[optionName,currentStanOptionSample]);

CmdStan`StanSetOptionSample::usage=
"StanSetOptionSample[name_?StringQ,value_] sets option -> value for the Sample method";
StanSetOptionSample[name_?StringQ,value_]:=
currentStanOptionSample=StanSetOption[{{name,value}},currentStanOptionSample];

CmdStan`StanOptionSample::usage=
"StanOptionSample[] returns complete list of options for the Sample method";
StanOptionSample[]:=currentStanOptionSample;

CmdStan`StanResetOptionSample::usage=
"StanResetOptionSample[] resets to default and returns complete list of default options for the Sample method";
StanResetOptionSample[]:=currentStanOptionSample={};

(*~~~~~~~~~~~~~~~~*)

CmdStan`StanGetOptionOptimize::usage=
"StanGetOptionOptimize[name_?StringQ] returns option associated value for the Optimize method, $Failed if not found";
StanGetOptionOptimize[name_?StringQ]:=StanGetOption[name,currentStanOptionOptimize];

CmdStan`StanRemoveOptionOptimize::usage=
"StanRemoveOptionOptimize[optionName_?StringQ] returns option list where the \"optionName\" option has been removed,"<>
" if \"optionName\" is not found return an unmodified option list.";
StanRemoveOptionOptimize[optionName_?StringQ]:=
		(currentStanOptionOptimize=StanRemoveOption[optionName,currentStanOptionOptimize]);

CmdStan`StanSetOptionOptimize::usage=
"StanSetOptionOptimize[name_?StringQ,value_] sets option -> value for the Optimize method";
StanSetOptionOptimize[name_?StringQ,value_]:=
currentStanOptionOptimize=StanSetOption[{{name,value}},currentStanOptionOptimize];

CmdStan`StanOptionOptimize::usage=
"StanOptionOptimize[] returns complete list of options for the Optimize method";
StanOptionOptimize[]:=currentStanOptionOptimize;

CmdStan`StanResetOptionOptimize::usage=
"StanResetOptionOptimize[] resets to default and returns complete list of default options for the Optimize method";
StanResetOptionOptimize[]:=currentStanOptionOptimize={};
(* Private *)
StanRun::stanExeNotFound="Stan executable \"`1`\" not found.";
StanRun::stanDataFileNotFound="Stan executable \"`1`\" not found.";
(* Private *)
StanRunExecFilename[stanExeFileName_?StringQ]:=
  Module[{exeFileNameWithExt,pathExeFileName},

  (* Check that prog(.exe) exists *)

  If[($OperatingSystem=="Windows")&&(FileExtension[stanExeFileName]==""),
  exeFileNameWithExt=stanExeFileName<>".exe",
  exeFileNameWithExt=stanExeFileName
  ];

  pathExeFileName=AbsoluteFileName[exeFileNameWithExt];

  If[pathExeFileName===$Failed,
  Message[StanRun::stanExeNotFound,exeFileNameWithExt];
  Return[$Failed]
  ];

  Return[pathExeFileName];
  ];
(* private *)
(* CAVEAT: pathExeFileName created from StanRunExecFilename[stanExeFileName_?StringQ]
*         and NOT stanExeFileName
*)
StanRunDataFilename[pathExeFileName_?StringQ,option_?MatrixQ]:=
	Module[{dataFile,dataFileTmp},

	(* Check if there is a data file name in option, 
	* if not, try to create one from scratch 
	*)
	dataFile=StanGetOption["data file",option];
	
        If[dataFile===$Failed,
          dataFile=StanRemoveFileNameExt[pathExeFileName]<>".data.R";
	];

	(* Check if file exists *)
	dataFileTmp=AbsoluteFileName[dataFile];

	If[dataFileTmp===$Failed,
          Message[StanRun::stanDataFileNotFound,dataFile];
          Return[$Failed]
        ];

    Return[StanSetOption[{{"data file",dataFileTmp}},option]];
];
(*
 * Private interface, for the user one, see: StanRunVariational, StanRunSample...
 *)
StanRun[stanExeFileName_?StringQ,option_?MatrixQ]:=
	Module[{pathExeFileName,dataFile,outputFile,mutableOption,command,output},

	       (* Generate Executable Filename (absolute path) 
	       *)
	       pathExeFileName=StanRunExecFilename[stanExeFileName];
	       If[pathExeFileName===$Failed,Return[$Failed]];

	       (* Generate Data Filename (absolute path) and add it to option list
	       *)
	       mutableOption=StanRunDataFilename[pathExeFileName,option];
               If[mutableOption===$Failed,Return[$Failed]];

	       (* Check output file *)
	       
	       outputFile=StanGetOption["output file",mutableOption];

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

(*
 * User interface
 *)
CmdStan`StanRunVariational::usage="StanRunVariational[stanExeFileName_?StringQ]"
(*
 *)
StanRunVariational[stanExeFileName_?StringQ]:=
	StanRun[stanExeFileName,Join[immutableStanOptionVariational,StanOptionVariational[]]];

CmdStan`StanRunSample::usage="StanRunSample[stanExeFileName_?StringQ] \n\n   TODO: parallel sampling";
(*
 *)
StanRunSample[stanExeFileName_?StringQ]:=
	StanRun[stanExeFileName,Join[immutableStanOptionSample,StanOptionSample[]]];

CmdStan`StanRunOptimize::usage="StanRunOptimize[stanExeFileName_?StringQ]"
(*
 *)
StanRunOptimize[stanExeFileName_?StringQ]:=
	StanRun[stanExeFileName,Join[immutableStanOptionOptimize,StanOptionOptimize[]]];





RDumpToStringHelper[V_?VectorQ]:=
(* CAVEAT: use CForm to force scientific notation *)
"c("<>StringTake[ToString[Map[CForm,V]],{2,-2}]<>")";
(*
 *)
RDumpToString[{MatName_,M_}/;StringQ[MatName]&&MatrixQ[M]]:=
(* CAVEAT: needs to transpose the matrix to get the right ordering *)
	MatName<>" <- structure("<>RDumpToStringHelper[Flatten[Transpose[M]]] <>
		", .Dim = "<>RDumpToStringHelper[Dimensions[M]]<>")\n";
(*
 *)
RDumpToString[{VectName_,V_}/;StringQ[VectName]&&VectorQ[V]]:=
	VectName<>" <- "<>RDumpToStringHelper[V]<>"\n";
(*
 *)
RDumpToString[{VarName_,Var_}/;StringQ[VarName]&&NumberQ[Var]]:=
	VarName<>" <- " <>ToString[Var]<>"\n";

CmdStan`RDumpExport::usage =
"RDumpExport[fileNameDataR_?StringQ,listOfNameValue_]"<>
"\n\nCreates a file and dump data in RDump format."<>
"\n\nNote:"<>
"\n - input data \"listOfNameValue\" is of the form "<>
"\n   {{\"MatrixName\",{{...}}},{\"ScalarName\",5.6},{\"VectorName\",{..}},...}"<>
"\n - if \"fileName\" has no extension, \".data.R\" is automatically added.";
(*
 *)
RDumpExport[fileNameDataR_?StringQ,listOfNameValue_]:=
	Module[{str,fileNameDataRExt},
	       (* Add .data.R extension if required *)
	       fileNameDataRExt=fileNameDataR;
	       If[FileExtension[fileNameDataRExt]=="",fileNameDataRExt=fileNameDataRExt<>".data.R"];
	       (* Opean file and save data *)
	       str=OpenWrite[fileNameDataRExt];
	       If[str===$Failed,Return[$Failed]];
	       WriteString[str,StringJoin[Map[RDumpToString,listOfNameValue]]];
	       Close[str];
	       Return[fileNameDataRExt];
	];

CmdStan`StanImport::usage = 
"StanImport[outputCSV_?StringQ]" <>
"\n\nRead Stan output CSV file." <>
"\n\nReturn: {indexedHeader,dataMatrix,parameters}." <>
"\n  indexedHeader is a list of {column number,column name}." <>
"\n  dataMatrix is a Matrix containing the CSV data." <>
"\n  parameters is a String containing the unparsed CSV comments.";
(*
*)
StanImport::CSVFileNotFound="CSV output file \"`1`\" not found.";
StanImport::CSVFileNotWellFormed="CSV output file \"`1`\" is not well formed.";
(*
*)
StanImport[outputCSV_?StringQ]:=
        Module[{parameters,header,data,raw},
               If[!FileExistsQ[outputCSV],Message[StanImport::CSVFileNotFound,outputCSV];Return[$Failed];];

               raw=StringSplit[Import[outputCSV,"Text"],"\n"];
               parameters = StringJoin[Riffle[Select[raw,StringTake[#,1]=="#"&],"\n"]];
               data = ImportString[StringJoin[StringJoin[Riffle[Select[raw,StringTake[#,1]!="#"&],"\n"]]],"CSV"];
               
               If[(Length[data]<=1)||(Length[Dimensions[data]]!=2),
                  Message[StanImport::CSVFileNotWellFormed,outputCSV];
                  Return[$Failed];
               ];

               header=data[[1]];
               header=Table[{header[[i]],i},{i,1,Length[header]}];

               (*
		* Each column is a variable, this is the right convention with Mathematica
		* as Mean[Array] or Standardize[Array] directly operate on columns
                *)
               data=N[Drop[data,1]];

               Return[{header,data,parameters}]
        ];

(*
 * Patterns
 *)
StanImportHeaderElementQ[any_] :=
        (Length[any] == 2) && StringQ[any[[1]]] && IntegerQ[any[[2]]];

StanImportHeaderQ[any_] :=
        VectorQ[any, StanImportHeaderElementQ];

StanImportQ[any_] :=
        (Length[any] == 3) && 
        StanImportHeaderQ[any[[1]]] && 
        MatrixQ[any[[2]], NumberQ] && 
        StringQ[any[[3]]];

(*
 * Portable access to structure members
 *)
StanImportHeaderElementQ[any_] :=
        (Length[any] == 2) && StringQ[any[[1]]] && IntegerQ[any[[2]]];

CmdStan`StanImportHeader::usage = 
"StanImportHeader[stanImport_?StanImportQ] portable access to header.";
(*
*)
StanImportHeader[stanImport_?StanImportQ] := stanImport[[1]];

CmdStan`StanImportData::usage = 
"StanImportData[stanImport_?StanImportQ] portable access to sample data.";
(*
*)
StanImportData[stanImport_?StanImportQ] := stanImport[[2]];

CmdStan`StanImportComment::usage = 
"StanImportComment[stanImport_?StanImportQ] portable access to (unparsed) csv file comments.";
(*
*)
StanImportComment[stanImport_?StanImportQ] := stanImport[[3]];



CmdStan`StanFindVariableColumn::usage = 
"StanFindVariableColumn[variableName_?StringQ, indexedHeader_?StanImportHeaderQ]" <>
"\nStanFindVariableColumn[variableName_?StringQ,stanImport_?StanImportQ]" <>
"\n\nExtract variable columns" <>
"\n\nUsage example:" <>
"\n\nIn[] : StanFindVariableColumn[\"energy__\",t[[1]]]" <>
"\nOut[]: {7}" <>
"\n\nIn[] : StanFindVariableColumn[\"mu\",t[[1]]]" <>
"\nOut[]: {8,9,10,11,12,13,14,15,16,17,18,19,...}" <>
"\n\nExtension:" <>
"\nStanFindVariableColumn[variableName_?StringQ,stanImport_?StanImportQ]";
(*
*)
StanFindVariableColumn::variableNotFound="variable \"`1`\" not found.";
(*
*)
StanFindVariableColumn[variableName_?StringQ, indexedHeader_?StanImportHeaderQ] := 
        Module[{column},
               column = 
               Select[indexedHeader, 
                      (* CAVEAT: assume head { string, number } *)
                      StringMatchQ[#[[1]], 
                                   variableName | (variableName ~~ "." ~~ __)] &];
               
               If[Length[column] >= 1, Return[column[[All, 2]]]];

               (* variable not found *)
               Message[StanFindVariableColumn::variableNotFound,variableName];
               Return[$Failed];
        ];
(*
 *)
StanFindVariableColumn[variableName_?StringQ,stanImport_?StanImportQ] :=
        StanFindVariableColumn[variableName,StanImportHeader[stanImport]]; 

CmdStan`StanFindVariableIndex::usage=
"StanFindVariableIndex[variableName_?StringQ, indexedHeader_?StanImportHeaderQ]" <> 
"\nStanFindVariableIndex[variableName_?StringQ, stanImport_?StanImportQ]" <> 
"\n\nExtract variable indices" <>
"\n\nCAVEAT: only relevant for array" <>
"\n\nUsage example:" <>
"\nIn[] : StanFindVariableIndex[StanFindVariableColumn[\"mu\",stanImport]" <>
"\nOut[]: {{1,1},{2,1},{3,1},{4,1},{5,1},{1,2},{2,2},{3,2},{4,2},{5,2},{1,3},...}";
(*
*)
StanFindVariableIndex[variableName_?StringQ, indexedHeader_?StanImportHeaderQ] := 
                Module[{extractedColumn,localIndex},
                           extractedColumn=StanFindVariableColumn[variableName,indexedHeader];
                           (* Not found? *)
                           If[extractedColumn===$Failed,Return[$Failed]];
                           (* Extract coordinates *)
                           localIndex = indexedHeader[[extractedColumn]][[All, 1]];
                           localIndex = Map[StringSplit[#, "."] &, localIndex];
                           localIndex = localIndex[[All, 2 ;; Dimensions[localIndex][[2]]]];
                           localIndex = ToExpression[localIndex];
                           Return[localIndex];
                ];
(*
 *)
StanFindVariableIndex[variableName_?StringQ,stanImport_?StanImportQ] :=
        StanFindVariableIndex[variableName,StanImportHeader[stanImport]]; 

(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

CmdStan`StanVariableColumn::usage= 
"StanVariableColumn[variableName_?StringQ,indexedHeader_?StanImportHeaderQ,rawData_?ListQ]" <>
"\nStanVariableColumn[variableName_?StringQ,stanImport_?StanImportQ]" <>
"\n\nReturn the data list associated to \"variableName\"";
(*
 *)
StanVariableColumn[variableName_?StringQ,indexedHeader_?StanImportHeaderQ,data_?ListQ] := 
        Module[{column},
               column=StanFindVariableColumn[variableName,indexedHeader];
	       (* Error? *)
               If[column===$Failed,Return[$Failed]];
	       (* Ok, continue processing *)
               Return[data[[All,column]]];
        ];
(*
 *)           
StanVariableColumn[variableName_?StringQ,stanImport_?StanImportQ] :=
        StanVariableColumn[variableName,StanImportHeader[stanImport],StanImportData[stanImport]];

(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

CmdStan`StanVariableToImport::usage = 
"StanVariableToImport[variableName_,stanImport_?StanImportQ] returns a StanImport structure containing only the \"variableName\" variable" <>
"\n\nReturn: the restricted StanImport structure.";
(*
*)
StanVariableToImport[variableName_,stanImport_?StanImportQ] :=
		Module[{selected,newHeader,newData},

			   selected=StanFindVariableColumn[variableName,stanImport];
			   If[selected===$Failed,Return[$Failed]];

			   newHeader=StanImportHeader[stanImport][[selected,1]];
			   newHeader=Transpose[{newHeader,Range[Length[newHeader]]}];

			   newData=StanImportData[stanImport][[All,selected]];

			   Return[{newHeader,newData,StanImportComment[stanImport]}];
		];

(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

CmdStan`StanVariable::usage= 
"StanVariable[variableName_?StringQ,indexedHeader_?StanImportHeaderQ,data_?VectorQ]"<>
"\n\nReturns the data associated to \"variableName\"";
(*
 *)
StanVariable[variableName_?StringQ,indexedHeader_?StanImportHeaderQ,data_?VectorQ] := 
        Module[{column,variableIndex,createArray},
               column=StanFindVariableColumn[variableName,indexedHeader];
               (* Error? *)
               If[column===$Failed,Return[$Failed]];
			   (* Ok, continue processing *)
			   (* Scalar ?*)
			   If[Length[column]==1,Return[Flatten[data[[column]]]]];
			   (*
				* The variable to return is an array
				* => Use SpareArray, then Normal, SLOW but always works 
				*)
			   variableIndex = StanFindVariableIndex[variableName, indexedHeader];

			   Return[Normal[SparseArray[Thread [variableIndex ->data[[column]]]]]];
		];

CmdStan`StanVariable::usage= 
"StanVariable[variableName_?StringQ,stanImport_?StanImportQ,sampleIdx_?IntegerQ]"
"\n\nReturns the data associated to \"variableName\" from the \"sampleIdx\" row of the matrix data";
(*
 *)           
StanVariable[variableName_?StringQ,stanImport_?StanImportQ,sampleIdx_?IntegerQ] :=
        StanVariable[variableName,StanImportHeader[stanImport],StanImportData[stanImport][[sampleIdx,All]]];

CmdStan`StanVariableFunc::usage=
"StanVariableFunc[variableName_?StringQ,stanImport_?StanImportQ,func_] applies the function \"func\" to variable columns and return the result"<>
"\n\nUsage example:"<>
"\n   StanVariableFunc[variableName,stanImport,Mean]";
(*
*)
StanVariableFunc[variableName_?StringQ,stanImport_?StanImportQ,func_] := 
		Module[{restrictedData,result},
			   restrictedData=StanVariableToImport[variableName,stanImport];
			   If[restrictedData===$Failed,Return[$Failed]];
			   result=func[StanImportData[restrictedData]];
			   result=StanVariable[variableName,
								   StanImportHeader[restrictedData],
								   result];
			   Return[result];
		];

CmdStan`StanVariableBoxPlot::usage=
"StanVariableBoxPlot[variableName_?StringQ,stanImport_?StanImportQ] box plot of the given variable";
(*
*)
StanVariableBoxPlot[variableName_?StringQ,stanImport_?StanImportQ] := 
    Module[{variableOutput, variableIndex, plot},
    
        variableOutput = StanVariableToImport[variableName, stanImport];

        If[variableOutput===$Failed,Return[$Failed]];

        variableIndex = Map[ToString,
                            Flatten[StanFindVariableIndex[variableName, variableOutput]]];

        plot = BoxWhiskerChart[Transpose[StanVariableColumn[variableName, variableOutput]],
                                ChartLabels->variableIndex,
                                PlotLabel->variableName];

        Return[plot];
]

End[];

Protect @@ Names["CmdStan`*"];

EndPackage[];
