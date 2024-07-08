ClearAll[CommandLineParse];

(* ::Section:: *)(* Catch-all *)
CommandLineParse[___] := $Failed;

(* ::Section:: *)(* Command String *)

	(* ::Subsection:: *)(* Command parse *)
	CommandLineParse[str_String?(Not@StringContainsQ[#, "-"]&)] := (
		CommandLineParse[StringSplit[str, " "]]
	);

	(* ::Subsection:: *)(* Compound command flag lookup *)
	CommandLineParse[str_String?(StringContainsQ[#, (";"|"&&")]&), flag_String] := (
		Dataset[<|StringTrim[ StringSplit[#, "-"][[1]] ] -> Normal[
					CommandLineParse[#,flag]
				]
			|>& /@ StringSplit[str, (";"|"&&")]
		] /. {
			{
				OrderlessPatternSequence[
					rest:<|_String-> _String|Missing[] |>...,
					missing:<| _String->_?FailureQ |>...
				]
			} :> { rest },
			{} -> Failure["flagnotfound",<|
					"MessageTemplate" -> "Flag `1` not found in the commands: `2`.",
					"MessageParameters"->{ flag, str }
				|>
			]
		}
	);

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
):= Block[{res, flags,
		cache={},
		command = StringTrim@(StringSplit[StringRiffle[cmd], "-"][[1]])
	},
	res = Reap[
		Do[
			If[ StringMatchQ[StringPart[elem, 1], "-"],
				If[cache =!={},
					Sow[cache]
				];
				cache = {};
				AppendTo[cache, elem];
				If[ elem === cmd[[-1]],
					Sow[cache]
				]
				,
				If[cache =!={},
					AppendTo[cache, elem];
					If[ elem === cmd[[-1]],
						Sow[cache]
					]
				]
			],
			{elem, cmd}
		]
	][[2,1]];

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
		res
	]//Dataset;

	flags = res[All, command, "Flag"];

	If[flagList==={},
		res,
		Catch @ Table[
			If[MemberQ[flags,flg],
				Normal[
					res[All,command][Select[#Flag==flg &], "Parameter"]
						/. Missing[] -> Nothing
				]
				,
				Throw[
					Failure["flagnotfound",<|
						"MessageTemplate" -> "Flag `1` not found in the command list `2`.",
						"MessageParameters"->{ flg, cmd }
					|>]
				]
			],
			{flg, flagList}
		] /. {
			{{a_}}:>a,
			{{a__}}:>{a},
			{{}} :> Missing[]
		}
	]
];
