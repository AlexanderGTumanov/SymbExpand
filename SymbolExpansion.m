(* ::Package:: *)

version=1.0;
Print["Symbol Expansion ",ToString@NumberForm[version,{20,1}]]
Print["by Alexander G Tumanov"]

ClearAll[SMB,RulesSymbol];
SetAttributes[SMB,{Flat,OneIdentity}]
RulesSymbol = {
	SMB[a1___,a2_ a3_,a4___]:>SMB[a1,a2,a4]+SMB[a1,a3,a4],
	SMB[a1___,a2_^n_,a3___]:>n SMB[a1,a2,a3],
	SMB[a1___,a2_,a3___]:>0/;NumericQ[a2],
	\!\(\*
TagBox[
StyleBox[
RowBox[{"SMB", "[", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "AA_"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "BB_"}], "]"}]}], "]"}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\):>SMB[AA+BB],
	\!\(\*
TagBox[
StyleBox[
RowBox[{"SMB", "[", 
RowBox[{"Plus", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "AA_"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "BB_"}], "]"}], ",", 
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "CC_"}], "]"}]}], "]"}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\):>SMB[AA+BB+CC],
	SMB[Plus[AA_,BB_]]:>SMB[-AA-BB]/;TrueQ[AA<0],
	SMB[Plus[AA_,BB_,CC_]]:>SMB[-AA-BB-CC]/;TrueQ[AA<0]
};
RulesSymbol::usage = "RulesSymbol gives the set of rules used for symbol simplification.";

Clear[SimplifySymbol,MultiplySymbol]
SimplifySymbol[expr_] := expr/.SMB[XX___]:>SMB@@(Factor/@{XX})//.RulesSymbol//Collect[#,SMB[___],Simplify]&
MultiplySymbol[expr_] := 
	Block[{exprC=0,exprR=expr},
		While[!TrueQ[exprC==exprR],
			exprC=exprR;exprR=exprR//.{Ca_SMB Cb_SMB:>Block[{tab,temp,La=Length[Ca],Lb=Length[Cb]},
				tab=Subsets[Range[La+Lb],{Lb}];
				Sum[temp=Ca;Do[temp=Insert[temp,Cb[[j]],tab[[k,j]]],{j,Lb}];temp,{k,1,Length[tab]}]
			],
			Ca_SMB^n_:>Ca^(n-2) Block[{tab,temp,La=Length[Ca]},
				tab=Subsets[Range[La+La],{La}];
				Sum[temp=Ca;Do[temp=Insert[temp,Ca[[j]],tab[[k,j]]],{j,La}];temp,{k,1,Length[tab]}]
			]/;n>1}//Expand
		];
	exprR]
SimplifySymbol::usage = "SimplifySymbol[expr] simplifies expr, including the letters within each symbol.";  
MultiplySymbol::usage = "MultiplySymbol[expr] computes the product of all SMB functions within expr.";  

Clear[PolySymbol,Symbolize,Convert,ParallelConvert]
PolySymbol[1,x_] := 1-x
PolySymbol[n_,x_] := -SMB[1-x,SMB@@(x 1^Range[n-1])]
Symbolize = {Log[a_]:>SMB[a],PolyLog[n_,a_]:>PolySymbol[n,a]};
Convert[expr_] :=
	Block[{exprCD=Expand[expr/.Symbolize]/.SMB[a___]:>CD[\[ScriptF][a]]//.{CD[a___]CD[b___]:>CD@@Join[{a},{b}],CD[a___]^n_:>CD@@Join@@Table[{a},{i,n}]}/.CD[a___]:>CD@@Sort[{a}],CC},
		CC=Union@Cases[{exprCD},CD[___],\[Infinity]];(exprCD/.CD[___]:>0)+Sum[Coefficient[exprCD,CC[[i]]]CC[[i]]/.CD[a___]:>Times@@(SMB@@#&/@{a})//MultiplySymbol//SimplifySymbol,{i,Length[CC]}]
	]//.RulesSymbol//Collect[#,SMB[___],Simplify]&
ParallelConvert[expr_] :=
	Block[{exprCD=Expand[expr/.Symbolize]/.SMB[a___]:>CD[\[ScriptF][a]]//.{CD[a___]CD[b___]:>CD@@Join[{a},{b}],CD[a___]^n_:>CD@@Join@@Table[{a},{i,n}]}/.CD[a___]:>CD@@Sort[{a}],CC},
		CC=Union@Cases[{exprCD},CD[___],\[Infinity]];(exprCD/.CD[___]:>0)+ParallelSum[Coefficient[exprCD,CC[[i]]]CC[[i]]/.CD[a___]:>Times@@(SMB@@#&/@{a})//MultiplySymbol//SimplifySymbol,{i,Length[CC]}]
	]//.RulesSymbol//Collect[#,SMB[___],Simplify]&
Convert::usage = "Convert[expr] returns the symbol of expr.";
ParallelConvert::usage = "ParallelConvert[expr] returns the symbol of expr. This function uses parallel evaluation.";
	
Clear[ClipFirstEntry,ClipLastEntry]
ClipLastEntry[expr_,a_] := (expr/.SMB->SYMB/.SYMB[Sequence@@a]:>1/.SYMB[AA___,Sequence@@a]:>SMB[AA]/.SYMB[___]:>0)/;ListQ[a]
ClipLastEntry[expr_,a_] := ClipLastEntry[expr,{a}]/;!ListQ[a]
ClipFirstEntry[expr_,a_] := (expr/.SMB->SYMB/.SYMB[Sequence@@a]:>1/.SYMB[Sequence@@a,AA___]:>SMB[AA]/.SYMB[___]:>0)/;ListQ[a]
ClipFirstEntry[expr_,a_] := ClipFirstEntry[expr,{a}]/;!ListQ[a]
ClipLastEntry::usage = "ClipLastEntry[expr,x] removes x from the end of each symbol that ends with it. Any symbol that doesn't end with x is set to zero. ClipLastEntry[expr,{x[1],...,x[n]}] removes n last entries instead.";
ClipFirstEntry::usage = "ClipFirstEntry[expr,x] removes x from the end of each symbol that starts with it. Any symbol that doesn't start with x is set to zero. ClipFirstEntry[expr,{x[1],...,x[n]}] removes n first entries instead.";

Clear[StandardizeArgument]
StandardizeArgument[expr_,T_] := StandardizeArgument[expr,T] =
	Block[{logTpower=Coefficient[PowerExpand@FullSimplify[SeriesCoefficient[Log[expr],{T,0,0}]],Log[T]]},
		{logTpower,FullSimplify@SeriesCoefficient[expr/T^logTpower,{T,0,0}]}
	]
StandardizeArgument::usage = "StandardizeArgument[expr, x] rewrites expr in the form \[Beta] \!\(\*SuperscriptBox[\(x\), \(\[Alpha]\)]\) (1 + ...), preparing it for expansion in x. The function returns {\[Alpha], \[Beta]}.";  

Clear[SMBtoEQN]
SMBtoEQN[expr_] :=
	Block[{CC=Union@Cases[expr,SMB[___],\[Infinity]],DD=Union@Cases[expr,\[ScriptCapitalR][___],\[Infinity]]},
		Union@Flatten@Join[{expr/.{SMB[___]:>0,\[ScriptCapitalR][___]->0}},Table[Coefficient[expr/.\[ScriptCapitalR][___]:>0,CC[[ii]]],{ii,Length[CC]}],Table[Coefficient[expr/.SMB[___]:>0,DD[[jj]]],{jj,Length[DD]}],Flatten@Table[Coefficient[Coefficient[expr,CC[[ii]]],DD[[jj]]],{ii,Length[CC]},{jj,Length[DD]}]]
	]
SMBtoEQN[expr_,vars_] :=
	Block[{CC=Union@Cases[expr,SMB[___],\[Infinity]],EE},
		EE=Union@Flatten@Join[{expr/.{SMB[___]:>0,\[ScriptCapitalR][___]->0}},Table[Coefficient[expr/.\[ScriptCapitalR][___]:>0,CC[[ii]]],{ii,Length[CC]}],Table[Coefficient[expr/.SMB[___]:>0,DD[[jj]]],{jj,Length[DD]}],Flatten@Table[Coefficient[Coefficient[expr,CC[[ii]]],DD[[jj]]],{ii,Length[CC]},{jj,Length[DD]}]];
		Do[EE=Union@Flatten[CoefficientList[#,vars[[jj]]]&/@EE],{jj,Length[vars]}];
		EE
	]/;ListQ[vars]
SMBtoEQN[expr_,vars_] := SMBtoEQN[expr,{vars}]/;!ListQ[vars]
SMBtoEQN::usage = "SMBtoEQN[expr] decomposes the equation expr = 0 into its independent components, corresponding to individual symbol terms. This function also accounts for cases where the space is not pure, requiring all rational prefactors in front of symbols to be wrapped by the \[ScriptCapitalR][_] function. SMBtoEQN[expr, vars] further splits the equation over the variables in vars.";  

Clear[ParallelSMBtoEQN]
ParallelSMBtoEQN[expr_] :=
	Block[{CC=Union@Cases[expr,SMB[___],\[Infinity]],DD=Union@Cases[expr,\[ScriptCapitalR][___],\[Infinity]]},
		Union@Join[{expr/.{SMB[___]:>0,\[ScriptCapitalR][___]->0}},ParallelTable[Coefficient[expr/.\[ScriptCapitalR][___]:>0,CC[[ii]]],{ii,Length[CC]}],ParallelTable[Coefficient[expr/.SMB[___]:>0,DD[[jj]]],{jj,Length[DD]}],Flatten@ParallelTable[Coefficient[Coefficient[expr,CC[[ii]]],DD[[jj]]],{ii,Length[CC]},{jj,Length[DD]}]]
	]
ParallelSMBtoEQN[expr_,vars_] :=
	Block[{CC=Union@Cases[expr,SMB[___],\[Infinity]],EE},
		EE=Union@Join[{expr/.{SMB[___]:>0,\[ScriptCapitalR][___]->0}},ParallelTable[Coefficient[expr/.\[ScriptCapitalR][___]:>0,CC[[ii]]],{ii,Length[CC]}],ParallelTable[Coefficient[expr/.SMB[___]:>0,DD[[jj]]],{jj,Length[DD]}],Flatten@ParallelTable[Coefficient[Coefficient[expr,CC[[ii]]],DD[[jj]]],{ii,Length[CC]},{jj,Length[DD]}]];
		Do[EE=Union@Flatten[CoefficientList[#,vars[[jj]]]&/@EE],{jj,Length[vars]}];
		EE
	]/;ListQ[vars]
ParallelSMBtoEQN[expr_,vars_] := ParallelSMBtoEQN[expr,{vars}]/;!ListQ[vars]
ParallelSMBtoEQN::usage = "ParallelSMBtoEQN[expr] decomposes the equation expr = 0 into its independent components, corresponding to individual symbol terms. This function also accounts for cases where the space is not pure, requiring all rational prefactors in front of symbols to be wrapped by the \[ScriptCapitalR][_] function. ParallelSMBtoEQN[expr, vars] further splits the equation over the variables in vars. This function uses parallel evaluation.";

Clear[RemoveNumericFactors]
RemoveNumericFactors[list_] := ((LL@@(Factor/@list))//.LL[AA___,nn_ BB_,CC___]:>LL[AA,BB,CC]/;NumericQ[nn]/.LL->List)/;ListQ[list]
RemoveNumericFactors::usage = "RemoveNumericFactors[list] removes overall numeric factors from a list of symbols.";

Clear[ToList,ToSymbol,ToBasis,Impose]
ToSymbol[expr_] := Table[\[ScriptS][ii],{ii,Length[expr]}] . expr/;ListQ[expr]
ToList[expr_] := Complement[Union@CoefficientList[expr/.\[ScriptS][a_]:>\[ScriptS]^a,\[ScriptS]],{0}]/;!ListQ[expr]
ToBasis[expr_] := 
	Block[{CC=If[ListQ[expr],ToSymbol[expr],expr],SS},
		SS=Flatten@Solve[SMBtoEQN[CC]==0];
		ToList[CC/.(Select[SS,!TrueQ[#[[2]]==0]&]/.Rule[a_,_]:>Rule[a,0])]
	]//RemoveNumericFactors//If[ListQ[expr],#,ToSymbol[#]]&
Impose[expr_,constraints_] := 
	Block[{CC=If[ListQ[expr],ToSymbol[expr],expr]},
		CC/.Flatten@Solve[constraints==0]
	]//ToBasis//If[ListQ[expr],ToList[#],#]&
ToSymbol::usage = "ToSymbol[list] converts a list of vectors in the symbol space into a linear combination of these vectors with coefficients \[ScriptS][i].";
ToList::usage = "ToList[symbol] converts a symbol, expressed as a linear combination of symbol space vectors with coefficients \[ScriptS][i], into a list of these vectors.";
ToBasis::usage = "ToBasis[expr] reduces expr, whether given as a list of symbols or as a linear combination of these symbols with coefficients \[ScriptS][i], to a linearly independent subset.";
Impose::usage = "Impose[expr, constraints] restricts expr, whether given as a list of symbols or as a linear combination of these symbols with coefficients \[ScriptS][i], to the subset of symbols that satisfy the constraints.";
	
Clear[CountLogsFront,CountLogsBack,RemoveLogs,ParallelRemoveLogs]
CountLogsFront[smb_,T_] := Block[{nLog=0},While[If[nLog==Length[smb],False,TrueQ[smb[[nLog+1]]==T]],nLog++];nLog]
CountLogsBack[smb_,T_] := Block[{nLog=0},While[If[nLog==Length[smb],False,TrueQ[smb[[-nLog-1]]==T]],nLog++];nLog]
RemoveLogs[expr_,T_] := (expr/.SMB[___]:>0) + 
	Block[{CC=Union@Cases[{expr},SMB[___],\[Infinity]]},
		Sum[Coefficient[expr,CC[[ii]]]If[CountLogsFront[CC[[ii]],T]==0,CC[[ii]],
			If[CountLogsFront[CC[[ii]],T]==Length[CC[[ii]]],log[T]^Length[CC[[ii]]]/Length[CC[[ii]]]!,
				Sum[(-1)^(CountLogsFront[CC[[ii]],T]-ll) log[T]^ll/ll! Block[{SS=Subsets[Range[Length[CC[[ii]]]-ll-1],{Length[CC[[ii]]]-CountLogsFront[CC[[ii]],T]-1}]},
					Sum[Block[{smbb={}},
						Do[smbb=Join[smbb,{CC[[ii,CountLogsFront[CC[[ii]],T]+kk]]},Table[T,{If[kk==Length[CC[[ii]]]-CountLogsFront[CC[[ii]],T],Length[CC[[ii]]]-ll,SS[[jj,kk]]]-If[kk==1,0,SS[[jj,kk-1]]]-1}]],{kk,Length[CC[[ii]]]-CountLogsFront[CC[[ii]],T]}];
						SMB@@smbb
					],{jj,Length[SS]}]
				],{ll,0,CountLogsFront[CC[[ii]],T]}]
			]
		],{ii,Length[CC]}]
	]
ParallelRemoveLogs[expr_,T_] := (expr/.SMB[___]:>0) + 
	Block[{CC=Union@Cases[{expr},SMB[___],\[Infinity]]},
		ParallelSum[Coefficient[expr,CC[[ii]]]If[CountLogsFront[CC[[ii]],T]==0,CC[[ii]],
			If[CountLogsFront[CC[[ii]],T]==Length[CC[[ii]]],log[T]^Length[CC[[ii]]]/Length[CC[[ii]]]!,
				Sum[(-1)^(CountLogsFront[CC[[ii]],T]-ll) log[T]^ll/ll! Block[{SS=Subsets[Range[Length[CC[[ii]]]-ll-1],{Length[CC[[ii]]]-CountLogsFront[CC[[ii]],T]-1}]},
					Sum[Block[{smbb={}},
						Do[smbb=Join[smbb,{CC[[ii,CountLogsFront[CC[[ii]],T]+kk]]},Table[T,{If[kk==Length[CC[[ii]]]-CountLogsFront[CC[[ii]],T],Length[CC[[ii]]]-ll,SS[[jj,kk]]]-If[kk==1,0,SS[[jj,kk-1]]]-1}]],{kk,Length[CC[[ii]]]-CountLogsFront[CC[[ii]],T]}];
						SMB@@smbb
					],{jj,Length[SS]}]
				],{ll,0,CountLogsFront[CC[[ii]],T]}]
			]
		],{ii,Length[CC]}]
	]
RemoveLogs::usage = "RemoveLogs[expr, x] rewrites expr to explicitly manifest its logarithmic divergences around the branch point x = 0, expressing them as powers of log[x]. If x = 0 is not a branch point, it returns expr unchanged.";  
ParallelRemoveLogs::usage = "ParallelRemoveLogs[expr, x] rewrites expr to explicitly manifest its logarithmic divergences around the branch point x = 0, expressing them as powers of log[x]. If x = 0 is not a branch point, it returns expr unchanged. This function utilizes parallel evaluation.";  

Clear[SymbolD]
SymbolD[expr_,TT_] := expr/;ListQ[TT]&&TrueQ[TT[[2]]==0]
SymbolD[expr_,TT_] := (SymbolD[expr,TT] =
	Block[{eexpr=RemoveLogs[expr,TT[[1]]]/.log[TT[[1]]]->log[Ttemp]},
		D[eexpr/.SMB[___]:>0,TT] + Block[{CC=Union@Cases[{eexpr},SMB[___],\[Infinity]]},
			Sum[SymbolD[D[Coefficient[eexpr,CC[[ii]]],TT[[1]]]CC[[ii]],{TT[[1]],TT[[2]]-1}] + If[Length[CC[[ii]]]==1,
				D[Coefficient[eexpr,CC[[ii]]]D[Log@Last[CC[[ii]]],TT[[1]]],{TT[[1]],TT[[2]]-1}],If[TrueQ[Last[CC[[ii]]]==TT[[1]]],
					1/TT[[2]] SymbolD[Coefficient[eexpr,CC[[ii]]]SMB@@Drop[CC[[ii]],-1],TT],SymbolD[Coefficient[eexpr,CC[[ii]]]D[Log@Last[CC[[ii]]],TT[[1]]]SMB@@Drop[CC[[ii]],-1],{TT[[1]],TT[[2]]-1}]
				]
			],{ii,Length[CC]}]
		]
	]/.Ttemp->TT[[1]])/;ListQ[TT]&&TrueQ[TT[[2]]>0]
SymbolD::usage = "SymbolD[expr,{x,n}] returns n-th derivative of expr over x.";

Clear[ParallelSymbolD]
ParallelSymbolD[expr_,TT_] := expr/;ListQ[TT]&&TrueQ[TT[[2]]==0]
ParallelSymbolD[expr_,TT_] := (ParallelSymbolD[expr,TT] = 
	Block[{eexpr=ParallelRemoveLogs[expr,TT[[1]]]/.log[TT[[1]]]->log[Ttemp]},
		D[eexpr/.SMB[___]:>0,TT] + Block[{CC=Union@Cases[{eexpr},SMB[___],\[Infinity]]},
			ParallelSum[SymbolD[D[Coefficient[eexpr,CC[[ii]]],TT[[1]]]CC[[ii]],{TT[[1]],TT[[2]]-1}] + If[Length[CC[[ii]]]==1,
				D[Coefficient[eexpr,CC[[ii]]]D[Log@Last[CC[[ii]]],TT[[1]]],{TT[[1]],TT[[2]]-1}],If[TrueQ[Last[CC[[ii]]]==TT[[1]]],
					1/TT[[2]] SymbolD[Coefficient[eexpr,CC[[ii]]]SMB@@Drop[CC[[ii]],-1],TT],SymbolD[Coefficient[eexpr,CC[[ii]]]D[Log@Last[CC[[ii]]],TT[[1]]]SMB@@Drop[CC[[ii]],-1],{TT[[1]],TT[[2]]-1}]
				]
			],{ii,Length[CC]}]
		]
	]/.Ttemp->TT[[1]])/;ListQ[TT]&&TrueQ[TT[[2]]>0]
ParallelSymbolD::usage = "ParallelSymbolD[expr,{x,n}] returns n-th derivative of expr over x. This function uses parallel evaluation.";

Clear[SymbolSeriesCoefficient,SymbolSeries]
SymbolSeriesCoefficient[expr_,TT_] := SymbolSeriesCoefficient[expr,TT] =
	1/TT[[2]]! Block[{EE=SymbolD[expr,TT],CC},CC=Union@Cases[EE,SMB[___],\[Infinity]];
		Normal@Series[EE/.SMB[___]:>0/.log[TT[[1]]]->log[Ttemp],{TT[[1]],0,0}] +
		Sum[Normal@Series[Coefficient[EE,CC[[jj]]]/.log[TT[[1]]]->log[Ttemp],{TT[[1]],0,0}]SMB@@((If[TT[[2]]==0,TT[[1]]^StandardizeArgument[#,TT[[1]]][[1]],1]StandardizeArgument[#,TT[[1]]][[2]])&/@(List@@(CC[[jj]]))),{jj,Length[CC]}]
	]//.RulesSymbol/.Ttemp->TT[[1]]//RemoveLogs[#,TT[[1]]]&
SymbolSeries[expr_,TT_] := Sum[TT[[1]]^ii SymbolSeriesCoefficient[expr,{TT[[1]],ii}],{ii,0,TT[[2]]}]
SymbolSeriesCoefficient::usage = "SymbolSeriesCoefficient[expr,{x,n}] returns the coefficient of \!\(\*SuperscriptBox[\(x\), \(n\)]\) of the series expansion around x = 0.";
SymbolSeries::usage = "SymbolSeries[expr,{x,n}] returns the expansion of expr around x = 0 up to order n.";

Clear[ParallelSymbolSeriesCoefficient,ParallelSymbolSeries]
ParallelSymbolSeriesCoefficient[expr_,TT_] := ParallelSymbolSeriesCoefficient[expr,TT] = 
	1/TT[[2]]! Block[{EE=ParallelSymbolD[expr,TT],CC},CC=Union@Cases[EE,SMB[___],\[Infinity]];
		Normal@Series[EE/.SMB[___]:>0/.log[TT[[1]]]->log[Ttemp],{TT[[1]],0,0}] +
		ParallelSum[Normal@Series[Coefficient[EE,CC[[jj]]]/.log[TT[[1]]]->log[Ttemp],{TT[[1]],0,0}]SMB@@((If[TT[[2]]==0,TT[[1]]^StandardizeArgument[#,TT[[1]]][[1]],1]StandardizeArgument[#,TT[[1]]][[2]])&/@(List@@(CC[[jj]]))),{jj,Length[CC]}]
	]//.RulesSymbol/.Ttemp->TT[[1]]//ParallelRemoveLogs[#,TT[[1]]]&
ParallelSymbolSeries[expr_,TT_] := Sum[TT[[1]]^ii ParallelSymbolSeriesCoefficient[expr,{TT[[1]],ii}],{ii,0,TT[[2]]}]
ParallelSymbolSeriesCoefficient::usage = "ParallelSymbolSeriesCoefficient[expr,{x,n}] returns the coefficient of \!\(\*SuperscriptBox[\(x\), \(n\)]\) of the series expansion around x = 0. This function uses parallel evaluation.";
ParallelSymbolSeries::usage = "ParallelSymbolSeries[expr,{x,n}] returns the expansion of expr around x = 0 up to order n. This function uses parallel evaluation.";
