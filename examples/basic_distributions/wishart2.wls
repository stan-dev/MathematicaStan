#!/usr/bin/env wolframscript
(* ::Package:: *)

<<CmdStan`


stanFileName=FileNameJoin[{NotebookDirectory[],FileBaseName[NotebookFileName[]]<>".stan"}];
CompileStanCode[stanFileName,StanVerbose->False];
stanResultFileName=RunStan[stanFileName,SampleDefaultOptions,StanVerbose->False];
stanResult=ImportStanResult[stanResultFileName]
GetStanResult[Histogram,stanResult,"W"] // TableForm



