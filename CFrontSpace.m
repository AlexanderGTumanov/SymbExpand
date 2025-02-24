(* ::Package:: *)

Block[{$Path = DirectoryName[$InputFileName]},
	<<SymbolExpansion`
]

version=1.0;
Print["C front space ",ToString@NumberForm[version,{20,1}]]
Print["by Alexander G Tumanov"]

Clear[UtoL,LtoU,LtoO,UtoO]
UtoL = {u[1]->1/(\[ScriptB] \[ScriptC]),u[2]->1/(\[ScriptA] \[ScriptC]),u[3]->1/(\[ScriptA] \[ScriptB]),1-u[1]->\[ScriptD]/(\[ScriptB] \[ScriptC]),1-u[2]->\[ScriptE]/(\[ScriptA] \[ScriptC]),1-u[3]->\[ScriptF]/(\[ScriptA] \[ScriptB])};
LtoU = {\[ScriptA]->Sqrt[u[1]/(u[2]u[3])],\[ScriptB]->Sqrt[u[2]/(u[3]u[1])],\[ScriptC]->Sqrt[u[3]/(u[1]u[2])],\[ScriptD]->(1-u[1])/u[1],\[ScriptE]->(1-u[2])/u[2],\[ScriptF]->(1-u[3])/u[3]};
UtoO = {u[1]->1/(1+S^2+T^2),u[2]->S^2/((1+T^2) (1+S^2+T^2)),u[3]->T^2/(1+T^2)}/.T->Sqrt[TT];
LtoO = {\[ScriptA]->(1+TT)/(S Sqrt[TT]),\[ScriptB]->S/Sqrt[TT],\[ScriptC]->(Sqrt[TT] (1+S^2+TT))/S,\[ScriptD]->S^2+TT,\[ScriptE]->(1+TT (2+S^2+TT))/S^2,\[ScriptF]->1/TT};
UtoL::usage = "UtoL is a set of replacements that converts an expression written in terms of \!\(\*SubscriptBox[\(u\), \(1\)]\),\!\(\*SubscriptBox[\(u\), \(2\)]\),\!\(\*SubscriptBox[\(u\), \(3\)]\) (u,v,w) into the one written in terms of the alphabet letters \[ScriptA],\[ScriptB],\[ScriptC],\[ScriptD],\[ScriptE],\[ScriptF].";
LtoU::usage = "UtoL is a set of replacements that converts an expression written in terms of the alphabet letters \[ScriptA],\[ScriptB],\[ScriptC],\[ScriptD],\[ScriptE],\[ScriptF] into the one written in terms of \!\(\*SubscriptBox[\(u\), \(1\)]\),\!\(\*SubscriptBox[\(u\), \(2\)]\),\!\(\*SubscriptBox[\(u\), \(3\)]\) (u,v,w).";
UtoO::usage = "UtoL is a set of replacements that converts an expression written in terms of \!\(\*SubscriptBox[\(u\), \(1\)]\),\!\(\*SubscriptBox[\(u\), \(2\)]\),\!\(\*SubscriptBox[\(u\), \(3\)]\) (u,v,w) into the one written in terms of the OPE variables S, F, and TT = \!\(\*SuperscriptBox[\(T\), \(2\)]\).";
LtoU::usage = "UtoL is a set of replacements that converts an expression written in terms of the alphabet letters \[ScriptA],\[ScriptB],\[ScriptC],\[ScriptD],\[ScriptE],\[ScriptF] into the one written in terms of the OPE variables S, F, and TT = \!\(\*SuperscriptBox[\(T\), \(2\)]\).";

Clear[DShift,DFlip,DihedralOrbit]
DShift = {\[ScriptA]->\[ScriptB],\[ScriptB]->\[ScriptC],\[ScriptC]->\[ScriptA],\[ScriptD]->\[ScriptE],\[ScriptE]->\[ScriptF],\[ScriptF]->\[ScriptD]};
DFlip = {\[ScriptA]->\[ScriptB],\[ScriptB]->\[ScriptA],\[ScriptC]->\[ScriptC],\[ScriptD]->\[ScriptE],\[ScriptE]->\[ScriptD],\[ScriptF]->\[ScriptF]};
DihedralOrbit[symb_] := Join[{symb,symb/.DFlip},{symb,symb/.DFlip}/.DShift,{symb,symb/.DFlip}/.DShift/.DShift]/;FreeQ[symb,u[_]]
DihedralOrbit[symb_] := Flatten@Table[{symb,symb/.{u[1]->u[2],u[2]->u[1]}}/.u[i_]:>u@Mod[i+kk,3,1],{kk,3}]/;!FreeQ[symb,u[_]]
DihedralOrbit::usage = "DihedralOrbit[expr] returns the list of images of expr under dihedral transformations.";

Clear[IncreaseTranscendentality]
IncreaseTranscendentality[expr_] :=
	Block[{CC=If[ListQ[expr],expr,ToList[expr]]},
		Flatten@Table[{CC[[ii]]/.SMB[AA__]:>SMB[AA,\[ScriptA]],CC[[ii]]/.SMB[AA__]:>SMB[AA,\[ScriptB]],CC[[ii]]/.SMB[AA__]:>SMB[AA,\[ScriptC]],CC[[ii]]/.SMB[AA__]:>SMB[AA,\[ScriptD]],CC[[ii]]/.SMB[AA__]:>SMB[AA,\[ScriptE]],CC[[ii]]/.SMB[AA__]:>SMB[AA,\[ScriptF]]},{ii,Length[expr]}]
	]//If[ListQ[expr],#,ToSymbol[#]]&
IncreaseTranscendentality::usage = "IncreaseTranscendentality[expr] increases the transcendentality of all elements in expr, whether given as a list of symbols or as a linear combination of these symbols with coefficients \[ScriptS][i], by appending an additional entry to each symbol.";

Clear[CDihedral,CClassicality,CIntegrability,CNonAdjacency,CFirstEntry]
CDihedral[expr_] :=
	Block[{CC=If[ListQ[expr],ToSymbol[expr],expr]},
		Flatten@SMBtoEQN@(DihedralOrbit[CC]-CC)
	]
CClassicality[expr_] :=
	Block[{CC=If[ListQ[expr],ToSymbol[expr],expr]/.SMB->SYMB/.SYMB[AA___]:>0/;Length[{AA}]!=4},
		Flatten@SMBtoEQN[(CC/.SYMB->SMB)-(CC/.SYMB[a_,b_,c_,d_]:>SMB[b,a,c,d])-(CC/.SYMB[a_,b_,c_,d_]:>SMB[a,b,d,c])+(CC/.SYMB[a_,b_,c_,d_]:>SMB[b,a,d,c])-(CC/.SYMB[a_,b_,c_,d_]:>SMB[c,d,a,b])+(CC/.SYMB[a_,b_,c_,d_]:>SMB[d,c,a,b])+(CC/.SYMB[a_,b_,c_,d_]:>SMB[c,d,b,a])-(CC/.SYMB[a_,b_,c_,d_]:>SMB[d,c,b,a])]
	]
CIntegrability[expr_] :=
	Block[{CC=If[ListQ[expr],ToSymbol[expr],expr]},
		Flatten@SMBtoEQN@Join[{
			ClipLastEntry[CC,{\[ScriptA],\[ScriptD]}]-ClipLastEntry[CC,{\[ScriptD],\[ScriptA]}],
			ClipLastEntry[CC,{\[ScriptB],\[ScriptE]}]-ClipLastEntry[CC,{\[ScriptE],\[ScriptB]}],
			ClipLastEntry[CC,{\[ScriptC],\[ScriptF]}]-ClipLastEntry[CC,{\[ScriptF],\[ScriptC]}],
			ClipLastEntry[CC,{\[ScriptD],\[ScriptE]}]-ClipLastEntry[CC,{\[ScriptE],\[ScriptD]}],
			ClipLastEntry[CC,{\[ScriptD],\[ScriptF]}]-ClipLastEntry[CC,{\[ScriptF],\[ScriptD]}],
			ClipLastEntry[CC,{\[ScriptE],\[ScriptF]}]-ClipLastEntry[CC,{\[ScriptF],\[ScriptE]}]
		},{
			ClipLastEntry[CC,{\[ScriptA],\[ScriptB]}]+ClipLastEntry[CC,{\[ScriptA],\[ScriptC]}]-ClipLastEntry[CC,{\[ScriptB],\[ScriptA]}]-ClipLastEntry[CC,{\[ScriptC],\[ScriptA]}],
			ClipLastEntry[CC,{\[ScriptA],\[ScriptB]}]-ClipLastEntry[CC,{\[ScriptB],\[ScriptA]}]-ClipLastEntry[CC,{\[ScriptB],\[ScriptC]}]+ClipLastEntry[CC,{\[ScriptC],\[ScriptB]}],
			ClipLastEntry[CC,{\[ScriptA],\[ScriptE]}]-ClipLastEntry[CC,{\[ScriptE],\[ScriptA]}]-ClipLastEntry[CC,{\[ScriptA],\[ScriptF]}]+ClipLastEntry[CC,{\[ScriptF],\[ScriptA]}]-ClipLastEntry[CC,{\[ScriptB],\[ScriptD]}]+ClipLastEntry[CC,{\[ScriptD],\[ScriptB]}]+ClipLastEntry[CC,{\[ScriptB],\[ScriptF]}]-ClipLastEntry[CC,{\[ScriptF],\[ScriptB]}]+ClipLastEntry[CC,{\[ScriptC],\[ScriptD]}]-ClipLastEntry[CC,{\[ScriptD],\[ScriptC]}]-ClipLastEntry[CC,{\[ScriptC],\[ScriptE]}]+ClipLastEntry[CC,{\[ScriptE],\[ScriptC]}]+2 ClipLastEntry[CC,{\[ScriptC],\[ScriptB]}]-2 ClipLastEntry[CC,{\[ScriptB],\[ScriptC]}]
		}]
	]
CNonAdjacency[expr_] :=
	Block[{CC=If[ListQ[expr],ToSymbol[expr],expr]},
		Flatten@SMBtoEQN@Join[{
			ClipLastEntry[CC,{\[ScriptD],\[ScriptE]}],
			ClipLastEntry[CC,{\[ScriptD],\[ScriptF]}],
			ClipLastEntry[CC,{\[ScriptE],\[ScriptD]}],
			ClipLastEntry[CC,{\[ScriptE],\[ScriptF]}],
			ClipLastEntry[CC,{\[ScriptF],\[ScriptD]}],
			ClipLastEntry[CC,{\[ScriptF],\[ScriptE]}]
		},{
			ClipLastEntry[CC,{\[ScriptA],\[ScriptD]}],
			ClipLastEntry[CC,{\[ScriptD],\[ScriptA]}],
			ClipLastEntry[CC,{\[ScriptB],\[ScriptE]}],
			ClipLastEntry[CC,{\[ScriptE],\[ScriptB]}],
			ClipLastEntry[CC,{\[ScriptC],\[ScriptF]}],
			ClipLastEntry[CC,{\[ScriptF],\[ScriptC]}]
		},{
			ClipLastEntry[CC,{\[ScriptB],\[ScriptA],\[ScriptA]}]+ClipLastEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptA]}]+ClipLastEntry[CC,{\[ScriptB],\[ScriptC],\[ScriptA]}],
			ClipLastEntry[CC,{\[ScriptA],\[ScriptA],\[ScriptB]}]+ClipLastEntry[CC,{\[ScriptA],\[ScriptB],\[ScriptB]}]+ClipLastEntry[CC,{\[ScriptA],\[ScriptC],\[ScriptB]}],
			ClipLastEntry[CC,{\[ScriptC],\[ScriptA],\[ScriptB]}]+ClipLastEntry[CC,{\[ScriptC],\[ScriptB],\[ScriptB]}]+ClipLastEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptB]}],
			ClipLastEntry[CC,{\[ScriptB],\[ScriptA],\[ScriptC]}]+ClipLastEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptC]}]+ClipLastEntry[CC,{\[ScriptB],\[ScriptC],\[ScriptC]}],
			ClipLastEntry[CC,{\[ScriptA],\[ScriptA],\[ScriptC]}]+ClipLastEntry[CC,{\[ScriptA],\[ScriptB],\[ScriptC]}]+ClipLastEntry[CC,{\[ScriptA],\[ScriptC],\[ScriptC]}],
			ClipLastEntry[CC,{\[ScriptC],\[ScriptA],\[ScriptA]}]+ClipLastEntry[CC,{\[ScriptC],\[ScriptB],\[ScriptA]}]+ClipLastEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptA]}]
		}]
	]
CFirstEntry[expr_] :=
	Block[{CC=If[ListQ[expr],ToSymbol[expr],expr]},
		Flatten@SMBtoEQN@Join[{
			ClipFirstEntry[CC,\[ScriptD]],
			ClipFirstEntry[CC,\[ScriptE]],
			ClipFirstEntry[CC,\[ScriptF]]
		},{
			ClipFirstEntry[CC,{\[ScriptB],\[ScriptD]}]-ClipFirstEntry[CC,{\[ScriptC],\[ScriptD]}],
			ClipFirstEntry[CC,{\[ScriptA],\[ScriptF]}]-ClipFirstEntry[CC,{\[ScriptB],\[ScriptF]}],
			ClipFirstEntry[CC,{\[ScriptB],\[ScriptC]}]-ClipFirstEntry[CC,{\[ScriptC],\[ScriptB]}]
		},{
			ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptD]}]-2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptB],\[ScriptD]}]+ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD]}],
			ClipFirstEntry[CC,{\[ScriptA],\[ScriptA],\[ScriptE]}]+2 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptC]}]-ClipFirstEntry[CC,{\[ScriptC],\[ScriptA],\[ScriptE]}]-2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptB],\[ScriptB]}]-ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptA]}]+ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC]}],
			2 ClipFirstEntry[CC,{\[ScriptA],\[ScriptA],\[ScriptE]}]-ClipFirstEntry[CC,{\[ScriptA],\[ScriptA],\[ScriptF]}]-ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptD]}]+ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptF]}]+2 ClipFirstEntry[CC,{\[ScriptB],\[ScriptF],\[ScriptA]}]-2 ClipFirstEntry[CC,{\[ScriptB],\[ScriptF],\[ScriptB]}]-2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptA],\[ScriptE]}]+ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD]}]+2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptB]}]-2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC]}]-2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptA]}]+2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC]}]
		},{
			2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptE],\[ScriptA]}]+2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptE],\[ScriptC]}]+ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptA],\[ScriptA]}]-2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptA]}]-3 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptB]}]-3 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptC]}],
			2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD],\[ScriptB]}]+2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD],\[ScriptC]}]+ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptB],\[ScriptB]}]-3 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptA]}]-2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptB]}]-3 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptC]}],
			2 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptB],\[ScriptD]}]-7 ClipFirstEntry[CC,{\[ScriptC],\[ScriptB],\[ScriptD],\[ScriptD]}]+12 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptB],\[ScriptD]}]-2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptC],\[ScriptD]}]+7 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD],\[ScriptD]}]+7 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptB],\[ScriptF]}]-14 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptA]}]-7 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptE]}]+7 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptD],\[ScriptB]}]-7 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptD],\[ScriptC]}]+6 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptD]}],
			2 ClipFirstEntry[CC,{\[ScriptA],\[ScriptA],\[ScriptA],\[ScriptF]}]+14 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptB],\[ScriptD]}]-14 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptB],\[ScriptF]}]-24 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptF],\[ScriptA]}]+24 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptF],\[ScriptB]}]-12 ClipFirstEntry[CC,{\[ScriptB],\[ScriptF],\[ScriptA],\[ScriptA]}]+24 ClipFirstEntry[CC,{\[ScriptB],\[ScriptF],\[ScriptB],\[ScriptA]}]-12 ClipFirstEntry[CC,{\[ScriptB],\[ScriptF],\[ScriptB],\[ScriptB]}]+96 ClipFirstEntry[CC,{\[ScriptC],\[ScriptB],\[ScriptD],\[ScriptC]}]-ClipFirstEntry[CC,{\[ScriptC],\[ScriptB],\[ScriptD],\[ScriptD]}]+18 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptB],\[ScriptC]}]-18 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptC],\[ScriptB]}]-20 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptC],\[ScriptD]}]+18 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptC],\[ScriptE]}]-51 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD],\[ScriptB]}]-45 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD],\[ScriptC]}]+ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD],\[ScriptD]}]+27 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptE],\[ScriptA]}]-27 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptE],\[ScriptC]}]-45 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptB],\[ScriptB]}]+9 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptB],\[ScriptF]}]+85 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptA]}]-6 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptB]}]+51 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptC]}]-ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptE]}]+ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptD],\[ScriptB]}]-ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptD],\[ScriptC]}]+9 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptA],\[ScriptA]}]-2 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptA],\[ScriptF]}]-18 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptA]}]+21 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptB]}]+9 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptC]}]-6 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptD]}],
			60 ClipFirstEntry[CC,{\[ScriptA],\[ScriptA],\[ScriptA],\[ScriptF]}]+362 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptB],\[ScriptD]}]-372 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptB],\[ScriptF]}]-648 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptF],\[ScriptA]}]+648 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptF],\[ScriptB]}]-336 ClipFirstEntry[CC,{\[ScriptB],\[ScriptF],\[ScriptA],\[ScriptA]}]+672 ClipFirstEntry[CC,{\[ScriptB],\[ScriptF],\[ScriptB],\[ScriptA]}]-336 ClipFirstEntry[CC,{\[ScriptB],\[ScriptF],\[ScriptB],\[ScriptB]}]-48 ClipFirstEntry[CC,{\[ScriptC],\[ScriptB],\[ScriptC],\[ScriptE]}]+2544 ClipFirstEntry[CC,{\[ScriptC],\[ScriptB],\[ScriptD],\[ScriptC]}]+5 ClipFirstEntry[CC,{\[ScriptC],\[ScriptB],\[ScriptD],\[ScriptD]}]+552 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptB],\[ScriptC]}]-12 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptB],\[ScriptD]}]-552 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptC],\[ScriptB]}]-554 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptC],\[ScriptD]}]+504 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptC],\[ScriptE]}]-1380 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD],\[ScriptB]}]-1164 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD],\[ScriptC]}]-5 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD],\[ScriptD]}]+732 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptE],\[ScriptA]}]-780 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptE],\[ScriptC]}]-1188 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptB],\[ScriptB]}]+241 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptB],\[ScriptF]}]+2374 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptA]}]-168 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptB]}]+1356 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptC]}]+5 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptE]}]-5 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptD],\[ScriptB]}]+5 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptD],\[ScriptC]}]+240 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptA],\[ScriptA]}]-90 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptA],\[ScriptF]}]-480 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptA]}]+624 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptB]}]+288 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptC]}]-186 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptD]}],
			14 ClipFirstEntry[CC,{\[ScriptA],\[ScriptA],\[ScriptA],\[ScriptF]}]+96 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptB],\[ScriptD]}]-98 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptB],\[ScriptF]}]+336 ClipFirstEntry[CC,{\[ScriptB],\[ScriptB],\[ScriptF],\[ScriptB]}]-336 ClipFirstEntry[CC,{\[ScriptB],\[ScriptF],\[ScriptB],\[ScriptB]}]+672 ClipFirstEntry[CC,{\[ScriptC],\[ScriptB],\[ScriptD],\[ScriptC]}]-12 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptB],\[ScriptD]}]-12 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptC],\[ScriptD]}]+336 ClipFirstEntry[CC,{\[ScriptC],\[ScriptC],\[ScriptD],\[ScriptB]}]-7 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptB],\[ScriptF]}]-336 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptA]}]-672 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptB]}]-336 ClipFirstEntry[CC,{\[ScriptC],\[ScriptD],\[ScriptC],\[ScriptC]}]+49 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptA],\[ScriptF]}]+336 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptB]}]-48 ClipFirstEntry[CC,{\[ScriptC],\[ScriptE],\[ScriptC],\[ScriptD]}]
		}]
	]
CDihedral::usage = "CDihedral[expr] generates the dihedral constraints on expr.";
CClassicality::usage = "CClassicality[expr] generates the classicality constraints on the transcendentality-4 part of expr.";
CIntegrability::usage = "CIntegrability[expr] generates the integrability constraints on the last two symbol entries of expr.";
CNonAdjacency::usage = "CNonAdjacency[expr] generates the non-adjacency constraints on the last two and three symbol entries of expr.";
CFirstEntry::usage = "CFirstEntry[expr] generates the first-entry constraints on expr up to transcendentality 4.";

Clear[\[ScriptCapitalC]]
\[ScriptCapitalC][1] = {SMB[\[ScriptA]],SMB[\[ScriptB]],SMB[\[ScriptC]]};
\[ScriptCapitalC][n_] := (\[ScriptCapitalC][n] =
	Block[{CC=IncreaseTranscendentality[\[ScriptCapitalC][n-1]]},
		Impose[CC,Join[CFirstEntry[CC],CIntegrability[CC],CNonAdjacency[CC]]]
	])/;n>1
\[ScriptCapitalC]::usage = "\[ScriptCapitalC][n] returns a list of basis elements in the C-space of transcendentality n. First-entry constraints are implemented only up to n = 4; beyond this point, the function produces slightly larger spaces.";
