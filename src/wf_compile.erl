%%%-------------------------------------------------------------------
%%% @doc Bytecode compiler for workflow pattern terms
%%%
%%% This module compiles wf_term() AST into wf_bc() bytecode using a
%%% single recursive pass over the AST, emitting opcodes with symbolic
%%% labels, then resolving all labels to integer IP addresses.
%%%
%%% == Compilation Pipeline ==
%%%
%%% From ARCHITECTURE.md:309-340:
%%%   1. Validate term (optional, via wf_validate:structural_check/1)
%%%   2. Single recursive pass over AST, emit opcodes with labels
%%%   3. Resolve labels to integer IPs (linear scan)
%%%   4. Validate bytecode (no unresolved labels)
%%%   5. Return {ok, wf_bc()}
%%%
%%% == Label Management ==
%%%
%%% Labels are generated using erlang:make_ref() to guarantee uniqueness.
%%% During compilation, opcodes may reference labels (e.g., {seq_next, Label}).
%%% In the resolution pass, labels are replaced with integer IP addresses.
%%%
%%% Example compilation of seq(par([task(a), task(b)]), task(c)):
%%%
%%%   Unresolved bytecode:
%%%     [{seq_enter, 0},
%%%      {par_fork, [Label1, Label2]},
%%%      {label, Label1}, {task_exec, a}, {done},
%%%      {label, Label2}, {task_exec, b}, {done},
%%%      {seq_next, Label3},
%%%      {label, Label3}, {task_exec, c}]
%%%
%%%   Resolved bytecode (Label1=2, Label2=5, Label3=8):
%%%     [{seq_enter, 0},
%%%      {par_fork, [2, 5]},
%%%      {task_exec, a}, {done},
%%%      {task_exec, b}, {done},
%%%      {seq_next, 8},
%%%      {task_exec, c}]
%%%
%%% == Opcode Emission ==
%%%
%%% Each kernel primitive emits specific opcodes:
%%%   - task/2: task_exec
%%%   - seq/2: seq_enter, seq_next with label
%%%   - par/1: par_fork with branch labels, done per branch, join_wait all
%%%   - x_or/1: xor_choose with alt labels, done per alt (no join)
%%%   - join/2: par_fork with branch labels, done per branch, join_wait Policy
%%%   - loop/2: loop_check, body, loop_back with label, exit label
%%%   - defer/1: NOT IMPLEMENTED (not in spec)
%%%   - cancel/2: cancel_scope enter, body, cancel_scope exit
%%%   - mi/2: mi_spawn, body, done, join_wait all
%%%
%%% == Error Handling ==
%%%
%%% Compiler throws badarg for contract violations:
%%%   - par with < 2 branches
%%%   - x_or with < 2 alternatives
%%%   - join with < 2 branches
%%%   - Invalid policy format
%%%
%%% Compiler returns {error, Reason} for structural issues:
%%%   - Unresolved labels after resolution
%%%
%%% == Dependencies ==
%%%
%%% - wf_term.erl (item 002): wf_term() type definition
%%% - wf_vm.erl (item 004): wf_bc() and opcode() type definitions
%%% - wf_validate.erl (item 013): Structural validation (optional)
%%%
%%% @see wf_vm:opcode/0 for opcode definitions
%%% @see wf_term:wf_term/0 for AST type definition
%%% @end
-module(wf_compile).

%% Export main compilation function
-export([compile/1]).

%% Export label resolution helper functions (may be used by external tools)
-export([replace_labels/2, resolve_opcode_labels/2, resolve_label/2]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Unresolved opcode: May contain label references
%% During compilation, opcodes may have label operands (refs).
%% After resolution, all labels are replaced with integer IPs.
-type unresolved_opcode() ::
    {seq_enter, non_neg_integer()} |
    {seq_next, label() | non_neg_integer()} |
    {par_fork, [label() | non_neg_integer()]} |
    {join_wait, wf_vm:join_policy()} |
    {xor_choose, [label() | non_neg_integer()]} |
    {loop_back, label() | non_neg_integer()} |
    {loop_check, wf_vm:loop_policy()} |
    {cancel_scope, {enter | exit, term()}} |
    {mi_spawn, wf_vm:mi_policy()} |
    {task_exec, atom()} |
    {done} |
    {label, reference()}.  %% Label marker (removed after resolution)

%% Unresolved bytecode: May contain labels and label markers
-type unresolved_bytecode() :: [unresolved_opcode()].

%% Label type (same as wf_vm:label() but local to avoid unused warning)
-type label() :: {label, reference()}.

%% Compilation error reasons
-type compile_error() ::
    {invalid_term, term()} |
    {invalid_branch_count, {atom(), non_neg_integer(), non_neg_integer()}} |
    {invalid_policy, term()} |
    {unresolved_labels, [label()]} |
    {too_deep, non_neg_integer()}.

%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc Compile wf_term() AST into wf_bc() bytecode
%%%
%%% Performs single recursive pass over AST, emitting opcodes with
%%% symbolic labels, then resolves all labels to integer IP addresses.
%%%
%%% From item.json:6 and ARCHITECTURE.md:392-429
%%%
%%% @param Term Workflow pattern term (wf_term())
%%% @return {ok, Bytecode} on success, {error, Reason} on failure
%%% @end
-spec compile(wf_term:wf_term()) -> {ok, wf_vm:wf_bc()} | {error, compile_error()}.
compile(Term) ->
    try
        %% Pass 1: Compile AST to bytecode with labels, collecting metadata
        {UnresolvedBytecode, MetadataMap} = compile_term_with_metadata(Term),

        %% Pass 2: Resolve labels to integer IPs
        ResolvedBytecode = resolve_labels(UnresolvedBytecode),

        %% Pass 3: Validate no unresolved labels remain
        case validate_bytecode(ResolvedBytecode) of
            ok ->
                {ok, {ResolvedBytecode, MetadataMap}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        throw:ThrowReason ->
            {error, ThrowReason};
        error:ErrorReason ->
            {error, {invalid_term, ErrorReason}}
    end.

%%%===================================================================
%%% Internal Functions - Compilation
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc Compile wf_term() to unresolved bytecode with metadata map
%%%
%%% Returns {bytecode(), metadata_map()} where metadata maps task names
%%% to their task_metadata() maps containing functions and other config.
%%% @end
-spec compile_term_with_metadata(wf_term:wf_term()) -> {unresolved_bytecode(), wf_vm:task_metadata_map()}.
compile_term_with_metadata({task, _Name, _Metadata} = Term) ->
    compile_task_with_metadata(Term);

compile_term_with_metadata({seq, _Left, _Right} = Term) ->
    compile_seq_with_metadata(Term);

compile_term_with_metadata({par, _Branches} = Term) ->
    compile_par_with_metadata(Term);

compile_term_with_metadata({x_or, _Alternatives} = Term) ->
    compile_xor_with_metadata(Term);

compile_term_with_metadata({join, _Policy, _Branches} = Term) ->
    compile_join_with_metadata(Term);

compile_term_with_metadata({loop, _Policy, _Body} = Term) ->
    compile_loop_with_metadata(Term);

compile_term_with_metadata({defer, _Alternatives} = Term) ->
    compile_defer_with_metadata(Term);

compile_term_with_metadata({cancel, _ScopeId, _Body} = Term) ->
    compile_cancel_with_metadata(Term);

compile_term_with_metadata({mi, _Policy, _Body} = Term) ->
    compile_mi_with_metadata(Term);

compile_term_with_metadata(InvalidTerm) ->
    error({badarg, {invalid_term, InvalidTerm}}).

%%%-------------------------------------------------------------------
%%% @doc Compile wf_term() to unresolved bytecode (with labels)
%%%
%%% This is the main recursive compilation function that dispatches
%%% on the term type and calls the appropriate compile_* function.
%%% Each compile_* function returns a list of unresolved opcodes.
%%% @end
-spec compile_term(wf_term:wf_term()) -> unresolved_bytecode().
-compile({nowarn_unused_function, compile_term/1}).
compile_term(Term) ->
    {Bytecode, _Metadata} = compile_term_with_metadata(Term),
    Bytecode.

%%%-------------------------------------------------------------------
%%% Metadata-preserving compilation functions
%%% These return {bytecode(), metadata_map()} tuples
%%%-------------------------------------------------------------------

%% Compile task with metadata collection
compile_task_with_metadata({task, Name, Metadata}) when is_atom(Name) ->
    %% Emit task_exec opcode and store metadata
    {[{task_exec, Name}], #{Name => Metadata}}.

%% Compile sequence with metadata merging
compile_seq_with_metadata({seq, Left, Right}) ->
    %% Recursively compile branches, collecting metadata
    {LeftCode, LeftMeta} = compile_term_with_metadata(Left),
    {RightCode, RightMeta} = compile_term_with_metadata(Right),

    %% Build sequence bytecode with label
    RightLabel = make_label(),
    SeqCode = [{seq_enter, 0}] ++
        LeftCode ++
        [{seq_next, RightLabel}] ++
        [RightLabel] ++
        RightCode,

    %% Merge metadata from both branches
    {SeqCode, maps:merge(LeftMeta, RightMeta)}.

%% Compile parallel with metadata collection from all branches
compile_par_with_metadata({par, Branches}) when length(Branches) >= 2 ->
    %% Compile all branches, collecting metadata
    BranchResults = [compile_term_with_metadata(B) || B <- Branches],
    BranchCodes = [Code || {Code, _Meta} <- BranchResults],
    BranchMetadatas = [Meta || {_Code, Meta} <- BranchResults],

    %% Generate labels
    BranchLabels = [make_label() || _ <- Branches],
    JoinLabel = make_label(),

    %% Build branch bytecode with label markers and done opcodes
    BranchBytecode = lists:flatmap(fun({Code, Label}) ->
        [Label | Code] ++ [{done}]
    end, lists:zip(BranchCodes, BranchLabels)),

    %% Build full bytecode
    ParCode = [{par_fork, BranchLabels}] ++
        BranchBytecode ++
        [JoinLabel] ++
        [{join_wait, all}],

    %% Merge all branch metadata
    MergedMetadata = lists:foldl(fun maps:merge/2, #{}, BranchMetadatas),
    {ParCode, MergedMetadata}.

%% Compile exclusive choice with metadata collection from all alternatives
compile_xor_with_metadata({x_or, Alternatives}) when length(Alternatives) >= 2 ->
    %% Compile all alternatives, collecting metadata
    AltResults = [compile_term_with_metadata(A) || A <- Alternatives],
    AltCodes = [Code || {Code, _Meta} <- AltResults],
    AltMetadatas = [Meta || {_Code, Meta} <- AltResults],

    %% Generate labels
    AltLabels = [make_label() || _ <- Alternatives],

    %% Build alternative bytecode with label markers and done opcodes
    AltBytecode = lists:flatmap(fun({Code, Label}) ->
        [Label | Code] ++ [{done}]
    end, lists:zip(AltCodes, AltLabels)),

    %% Build full bytecode (no join for xor)
    XorCode = [{xor_choose, AltLabels}] ++ AltBytecode,

    %% Merge all alternative metadata
    MergedMetadata = lists:foldl(fun maps:merge/2, #{}, AltMetadatas),
    {XorCode, MergedMetadata}.

%% Compile join with metadata collection from all branches
compile_join_with_metadata({join, Policy, Branches}) when length(Branches) >= 2 ->
    %% Validate policy
    validate_join_policy(Policy),

    %% Compile all branches, collecting metadata
    BranchResults = [compile_term_with_metadata(B) || B <- Branches],
    BranchCodes = [Code || {Code, _Meta} <- BranchResults],
    BranchMetadatas = [Meta || {_Code, Meta} <- BranchResults],

    %% Generate labels
    BranchLabels = [make_label() || _ <- Branches],
    JoinLabel = make_label(),

    %% Build branch bytecode
    BranchBytecode = lists:flatmap(fun({Code, Label}) ->
        [Label | Code] ++ [{done}]
    end, lists:zip(BranchCodes, BranchLabels)),

    %% Build full bytecode
    JoinCode = [{par_fork, BranchLabels}] ++
        BranchBytecode ++
        [JoinLabel] ++
        [{join_wait, Policy}],

    %% Merge all branch metadata
    MergedMetadata = lists:foldl(fun maps:merge/2, #{}, BranchMetadatas),
    {JoinCode, MergedMetadata}.

%% Compile loop with metadata propagation
compile_loop_with_metadata({loop, Policy, Body}) ->
    %% Compile body, collecting metadata
    {BodyCode, BodyMeta} = compile_term_with_metadata(Body),

    %% Generate labels
    LoopHeadLabel = make_label(),
    ExitLabel = make_label(),

    %% Build loop bytecode
    LoopCode = [LoopHeadLabel] ++
        [{loop_check, Policy}] ++
        BodyCode ++
        [{loop_back, LoopHeadLabel}] ++
        [ExitLabel],

    {LoopCode, BodyMeta}.

%% Compile defer (not implemented)
compile_defer_with_metadata({defer, _Alternatives}) ->
    throw({error, {not_implemented, defer}}).

%% Compile cancel with metadata propagation
compile_cancel_with_metadata({cancel, ScopeId, Body}) ->
    %% Compile body, collecting metadata
    {BodyCode, BodyMeta} = compile_term_with_metadata(Body),

    %% Build cancel bytecode (no metadata to merge)
    CancelCode = [{cancel_scope, {enter, ScopeId}}] ++
        BodyCode ++
        [{cancel_scope, {exit, ScopeId}}],

    {CancelCode, BodyMeta}.

%% Compile multiple instances with metadata propagation
compile_mi_with_metadata({mi, Policy, Body}) ->
    %% Validate policy
    validate_mi_policy(Policy),

    %% Compile body, collecting metadata
    {BodyCode, BodyMeta} = compile_term_with_metadata(Body),

    %% Build MI bytecode
    MiCode = [{mi_spawn, Policy}] ++
        BodyCode ++
        [{done}] ++
        [{join_wait, all}],

    {MiCode, BodyMeta}.

%% @private Validate join policy format
%% Throws badarg if policy is invalid
-spec validate_join_policy(wf_vm:join_policy()) -> ok.
validate_join_policy(all) -> ok;
validate_join_policy(sync_merge) -> ok;
validate_join_policy(first_complete) -> ok;
validate_join_policy({first_n, N}) when is_integer(N), N > 0 -> ok;
validate_join_policy({n_of_m, N, M}) when is_integer(N), is_integer(M), N > 0, M > 0, N =< M -> ok;
validate_join_policy(InvalidPolicy) ->
    error({badarg, {invalid_join_policy, InvalidPolicy}}).

%% @private Validate MI policy format
%% Throws badarg if policy is invalid
-spec validate_mi_policy(wf_vm:mi_policy()) -> ok.
validate_mi_policy({fixed, N}) when is_integer(N), N > 0 -> ok;
validate_mi_policy({dynamic, Min, Max}) when is_integer(Min), is_integer(Max), Min > 0, Max >= Min -> ok;
validate_mi_policy(InvalidPolicy) ->
    error({badarg, {invalid_mi_policy, InvalidPolicy}}).

%%%===================================================================
%%% Internal Functions - Label Management
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc Generate a unique label using make_ref()
%%%
%%% Labels are unique references that will be resolved to integer IPs
%%% in the second pass. Using make_ref() guarantees uniqueness without
%%% requiring state threading through all compile functions.
%%%
%%% @return Label tuple
%%% @end
-spec make_label() -> label().
make_label() ->
    {label, erlang:make_ref()}.

%%%-------------------------------------------------------------------
%%% @doc Build label map from unresolved bytecode
%%%
%%% Scans bytecode and records the IP address of each {label, Label}
%%% marker. Returns a map from label reference to integer IP.
%%%
%%% From research.md:587-601
%%%
%%% @param Bytecode Unresolved bytecode with label markers
%%% @return Label map #{Label => IP}
%%% @end
-spec build_label_map(unresolved_bytecode()) -> #{reference() => non_neg_integer()}.
build_label_map(Bytecode) ->
    build_label_map(Bytecode, 0, #{}).

build_label_map([], _IP, Map) ->
    Map;
build_label_map([{label, Ref} | Rest], IP, Map) ->
    build_label_map(Rest, IP + 1, Map#{Ref => IP});
build_label_map([_Opcode | Rest], IP, Map) ->
    build_label_map(Rest, IP + 1, Map).

%%%-------------------------------------------------------------------
%%% @doc Resolve all labels in bytecode to integer IPs
%%%
%%% Replaces label references in opcode operands with integer IPs using
%%% the label map. Removes {label, Label} markers from final bytecode.
%%%
%%% From research.md:603-622
%%%
%%% @param Bytecode Unresolved bytecode
%%% @return Resolved bytecode (wf_vm:wf_bc())
%%% @end
-spec resolve_labels(unresolved_bytecode()) -> wf_vm:wf_bc().
resolve_labels(Bytecode) ->
    %% Build label map - this gives us the position of each label marker
    LabelMap = build_label_map(Bytecode),

    %% Get sorted list of label positions for adjustment calculation
    LabelPositions = lists:sort(maps:values(LabelMap)),

    %% Resolve labels to adjusted IPs (accounting for label markers that will be removed)
    Resolved = replace_labels_adjusted(Bytecode, LabelMap, LabelPositions),

    %% Remove label markers
    lists:filter(fun({label, _}) -> false; (_) -> true end, Resolved).

%% @private Replace labels with IPs adjusted for future label marker removal
%% This is a two-pass algorithm: first resolve labels, then remove markers
-spec replace_labels_adjusted(unresolved_bytecode(), #{reference() => non_neg_integer()},
                              [non_neg_integer()]) -> unresolved_bytecode().
replace_labels_adjusted(Bytecode, LabelMap, LabelPositions) ->
    [resolve_opcode_labels_adjusted(Op, LabelMap, LabelPositions) || Op <- Bytecode].

%% @private Resolve labels in a single opcode with position adjustment
-spec resolve_opcode_labels_adjusted(unresolved_opcode(), #{reference() => non_neg_integer()},
                                     [non_neg_integer()]) -> wf_vm:opcode().
resolve_opcode_labels_adjusted({seq_next, {label, Ref}}, LabelMap, LabelPositions) ->
    OriginalPos = maps:get(Ref, LabelMap),
    %% Count how many label markers come before OriginalPos
    NumLabelsBefore = length([Pos || Pos <- LabelPositions, Pos < OriginalPos]),
    AdjustedPos = OriginalPos - NumLabelsBefore,
    {seq_next, AdjustedPos};
resolve_opcode_labels_adjusted({par_fork, Labels}, LabelMap, LabelPositions) when is_list(Labels) ->
    ResolvedLabels = [case L of
        {label, Ref} ->
            OriginalPos = maps:get(Ref, LabelMap),
            NumLabelsBefore = length([Pos || Pos <- LabelPositions, Pos < OriginalPos]),
            OriginalPos - NumLabelsBefore;
        Ref when is_reference(Ref) ->
            OriginalPos = maps:get(Ref, LabelMap),
            NumLabelsBefore = length([Pos || Pos <- LabelPositions, Pos < OriginalPos]),
            OriginalPos - NumLabelsBefore;
        IP when is_integer(IP) -> IP
    end || L <- Labels],
    {par_fork, ResolvedLabels};
resolve_opcode_labels_adjusted({xor_choose, Labels}, LabelMap, LabelPositions) when is_list(Labels) ->
    ResolvedLabels = [case L of
        {label, Ref} ->
            OriginalPos = maps:get(Ref, LabelMap),
            NumLabelsBefore = length([Pos || Pos <- LabelPositions, Pos < OriginalPos]),
            OriginalPos - NumLabelsBefore;
        Ref when is_reference(Ref) ->
            OriginalPos = maps:get(Ref, LabelMap),
            NumLabelsBefore = length([Pos || Pos <- LabelPositions, Pos < OriginalPos]),
            OriginalPos - NumLabelsBefore;
        IP when is_integer(IP) -> IP
    end || L <- Labels],
    {xor_choose, ResolvedLabels};
resolve_opcode_labels_adjusted({loop_back, {label, Ref}}, LabelMap, LabelPositions) ->
    OriginalPos = maps:get(Ref, LabelMap),
    NumLabelsBefore = length([Pos || Pos <- LabelPositions, Pos < OriginalPos]),
    AdjustedPos = OriginalPos - NumLabelsBefore,
    {loop_back, AdjustedPos};
resolve_opcode_labels_adjusted(Opcode, _LabelMap, _LabelPositions) ->
    Opcode.

%% @doc Replace label references in opcodes with integer IPs
-spec replace_labels(unresolved_bytecode(), #{reference() => non_neg_integer()}) -> unresolved_bytecode().
replace_labels(Bytecode, LabelMap) ->
    lists:map(fun(Opcode) -> resolve_opcode_labels(Opcode, LabelMap) end, Bytecode).

%% @doc Resolve labels in a single opcode
-spec resolve_opcode_labels(unresolved_opcode(), #{reference() => non_neg_integer()}) -> wf_vm:opcode().
resolve_opcode_labels({seq_next, {label, Ref}}, LabelMap) ->
    {seq_next, maps:get(Ref, LabelMap)};
resolve_opcode_labels({par_fork, Labels}, LabelMap) when is_list(Labels) ->
    ResolvedLabels = [case L of
        {label, Ref} -> maps:get(Ref, LabelMap);
        Ref when is_reference(Ref) -> maps:get(Ref, LabelMap);
        IP when is_integer(IP) -> IP
    end || L <- Labels],
    {par_fork, ResolvedLabels};
resolve_opcode_labels({xor_choose, Labels}, LabelMap) when is_list(Labels) ->
    ResolvedLabels = [case L of
        {label, Ref} -> maps:get(Ref, LabelMap);
        Ref when is_reference(Ref) -> maps:get(Ref, LabelMap);
        IP when is_integer(IP) -> IP
    end || L <- Labels],
    {xor_choose, ResolvedLabels};
resolve_opcode_labels({loop_back, {label, Ref}}, LabelMap) ->
    {loop_back, maps:get(Ref, LabelMap)};
resolve_opcode_labels(Opcode, _LabelMap) ->
    %% No labels to resolve (or already resolved)
    Opcode.

%% @doc Resolve a single label (if it's a label ref)
-spec resolve_label(label() | non_neg_integer(), #{reference() => non_neg_integer()}) -> non_neg_integer().
resolve_label({label, Ref}, LabelMap) ->
    maps:get(Ref, LabelMap);
resolve_label(Ref, LabelMap) when is_reference(Ref) ->
    maps:get(Ref, LabelMap);
resolve_label(IP, _LabelMap) when is_integer(IP) ->
    IP.

%%%===================================================================
%%% Internal Functions - Validation
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc Validate that bytecode has no unresolved labels
%%%
%%% Scans bytecode and ensures no label references remain. All labels
%%% should have been resolved to integer IPs.
%%%
%%% From research.md:624-636
%%%
%%% @param Bytecode Bytecode to validate
%%% @return ok if valid, {error, {unresolved_labels, Labels}} if invalid
%%% @end
-spec validate_bytecode(wf_vm:wf_bc()) -> ok | {error, {unresolved_labels, [label()]} | term()}.
validate_bytecode(Bytecode) ->
    Unresolved = [L || Op <- Bytecode, is_unresolved(Op), L <- [extract_label(Op)]],
    case Unresolved of
        [] -> ok;
        _ -> {error, {unresolved_labels, Unresolved}}
    end.

%% @doc Check if opcode contains unresolved label
-spec is_unresolved(wf_vm:opcode()) -> boolean().
is_unresolved({_, {label, _Ref}}) -> true;
is_unresolved({_, Labels}) when is_list(Labels) ->
    lists:any(fun({label, _}) -> true; (_) -> false end, Labels);
is_unresolved(_) -> false.

%% @doc Extract label from unresolved opcode
-spec extract_label(wf_vm:opcode()) -> label().
extract_label({_, {label, _} = Label}) -> Label;
extract_label({_, [Label | _]}) when element(1, Label) =:= label -> Label.
