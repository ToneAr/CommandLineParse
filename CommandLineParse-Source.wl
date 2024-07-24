ClearAll[CommandLineParse];

(* ::Section:: *)(* Catch-all *)
CommandLineParse[___] := $Failed;

(* ::Section:: *)(* Command String *)

	(* ::Subsection:: *)(* Command parse *)
	CommandLineParse[str_String?(Not@StringContainsQ[#, "-"]&)] := (
		CommandLineParse[StringSplit[str, " "]]
	);

	(* ::Subsection:: *)(* Compound command flag lookup *)
	CommandLineParse[str_String?(StringContainsQ[#, (";"|"&&")]&), flag_String] := Module[{
		noFlagFail =  Failure["flagnotfound",<|
					"MessageTemplate" -> "Flag `1` not found in the commands: `2`.",
					"MessageParameters"->{ flag, str }
				|>
			]
		},
		Dataset[<|StringTrim[ StringSplit[#, "-"][[1]] ] -> Normal[
					CommandLineParse[#,flag]
				]
			|>& /@ StringSplit[str, (";"|"&&")]
		] /. {
			{
				OrderlessPatternSequence[
					rest:<|_String-> (_String|_List|_Missing) |>...,
					missing:<| _String -> _?FailureQ |>..
				]
			} :> ({ rest } /. {} -> noFlagFail) ,
			{} ->noFlagFail
		}
	];

	(* ::Subsection:: *)(* Compound command parse *)
	CommandLineParse[str_String?(StringContainsQ[#, (";"|"&&")]&)] := (
		Dataset[
			<|
				<|StringTrim@(StringSplit[#, "-"][[1]]) -> Normal[
						With[{res = CommandLineParse[#]},
							If[!FailureQ[res],
								res[[All,1]],
								res
							]
						]
					]
				|>& /@ StringSplit[str, (";"|"&&")]
			|>
		]
	);

	(* ::Subsection:: *)(* Flag lookup *)
	CommandLineParse[ cmd_String, flagList:{ flag___String }:{} ]/;(
		Not@StringContainsQ[StringPart[cmd,1],"-"]
		&&
		StringContainsQ[cmd, " "]
		&&
		AllTrue[flagList, (StringPart[#,1]==="-")&]
	) := CommandLineParse[StringSplit[cmd, " "], flagList];
	CommandLineParse[ cmd_String, flag_String ] :=
		CommandLineParse[StringSplit[cmd, " "], {flag}];

(* ::Section:: *)(* Command List *)

	(* ::Subsection:: *)(* Flag lookup *)
	CommandLineParse[ cmd_List:$CommandLine, flag_String]:=
		CommandLineParse[ cmd, {flag}];
	CommandLineParse[ cmd_List:$CommandLine, flagList:{ flag___String }:{}]/;(
		Select[cmd, StringContainsQ["-"]]==={}
	) := Failure["noflag",<|
		"MessageTemplate" -> "Command contains no flags.",
		"MessageParameters"->{ cmd }
	|>];

(* ::Section:: *)(* Main *)
CommandLineParse[ cmd_List:$CommandLine, flagList:{ flag___String }:{}]/;(
	Not@StringContainsQ[StringPart[First[cmd],1],"-"]
	&&
	Select[cmd, StringContainsQ["-"]]=!={}
	&&
	AllTrue[flagList, (StringPart[#,1]==="-")&]
):= Block[{flags,
		res = CreateDataStructure["ExtensibleVector"],
		cache = CreateDataStructure["ExtensibleVector"],
		command = StringTrim@(StringSplit[StringRiffle[cmd], " -"][[1]])
	},
	Do[
		If[StringMatchQ[StringPart[elem,1],"-"],
			If[Not@cache["EmptyQ"],
				res["Append",cache//Normal]
			];
			cache["DropAll"];
			cache["Append",elem];
			If[elem===cmd[[-1]],
				res["Append",cache//Normal]
			],
			If[Not@cache["EmptyQ"],
				cache["Append",elem];
				If[elem===cmd[[-1]],
					res["Append",cache//Normal]
				]
			]
		],
		{elem,cmd}
	];
	res = Map[
		Function[
			<|command-><|
				"Flag" -> #[[1]],
				"Parameter" -> #[[2;;]] /. {
					{a_}:>a,
					{} -> Missing[]
				}
			|>|>
		],
		Normal@res
	]//Dataset;
	flags = res[All, command, "Flag"];

	If[flagList==={},
		res,
		First[Reap @ Table[
			If[MemberQ[flags,flg],
				Normal[
					res[All,command][Select[#Flag==flg &], "Parameter"]
						/. Missing[] -> Nothing
				]
				,
				Sow[
					Failure["flagnotfound",<|
						"MessageTemplate" -> "Flag `1` not found in the command list `2`.",
						"MessageParameters"->{ flg, cmd }
					|>]
				]
			],
			{flg, flagList}
		]] /. {
			{{a_}}:>a,
			{{a__}}:>{a},
			{{}}:>Missing["noarg"],
			{a_Failure}:>a
		}
	]
];