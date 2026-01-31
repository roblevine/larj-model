%% larj_model.pl - Main Entry Point for Domain Model Processing
%
%  This module provides the main interface for processing domain
%  knowledge artifacts and producing structured DDD domain models.
%
%  Usage:
%    ?- use_module(larj_model).
%    ?- process_domain_model(InputFile, OutputFile).
%
%  @author Generated for larj-model
%  @version 1.0

:- module(larj_model, [
    % Main processing
    process_domain_model/2,
    process_domain_model/3,

    % Model building (re-exported)
    define_context/3,
    add_context_term/3,
    link_contexts/4,
    define_moment_interval/4,
    add_mi_detail/4,
    add_mi_status_state/3,
    link_plan_actual/3,
    define_role/4,
    link_role_to_player/2,
    link_role_to_mi/2,
    define_party/4,
    define_place/4,
    define_thing/4,
    link_ppt_to_description/2,
    link_ppt_to_role/2,
    define_description/4,
    add_description_default/3,
    define_aggregate/3,
    add_aggregate_member/2,
    add_aggregate_invariant/2,
    define_service/3,
    add_service_operation/4,
    link_service_to_object/2,
    mark_core_domain/1,
    mark_supporting_domain/1,
    mark_generic_domain/1,
    add_attribute/4,
    add_behaviour/4,

    % Model querying
    objects_in_context/2,
    moment_intervals_in_context/2,
    roles_in_context/2,
    ppts_in_context/2,
    descriptions_in_context/2,
    aggregates_in_context/2,
    services_in_context/2,
    archetype_of/2,

    % Model validation
    validate_model/1,

    % Model export
    export_model_to_file/2,
    export_context_to_file/3,
    export_model_term/2,
    export_context_term/2,
    generate_model_report/2,
    generate_context_report/2,
    export_to_workflowy/2,

    % Model visualization
    generate_dot/2,
    generate_dot/3,
    generate_dot_file/2,
    generate_dot_file/3,
    generate_context_dot/3,
    generate_context_map_dot/2,

    % Model validation (comprehensive)
    validate_full_model/1,
    check_archetype_coverage/1,
    check_link_consistency/1,
    generate_validation_report/2,

    % Model reset
    clear_model/0
]).

:- use_module(ddd_schema).
:- use_module(model_builder).
:- use_module(model_export).
:- use_module(model_validate).
:- use_module(model_visualize).

%% --------------------------------------------------------------------------
%% Main Processing Interface
%% --------------------------------------------------------------------------

%% process_domain_model(+InputFile, +OutputFile) is det
%
%  Processes a domain model from an input file and writes the
%  Prolog representation to an output file.
%
%  The input file should contain domain knowledge in a structured
%  format (Prolog facts or a model-building script).
%
%  @param InputFile   Path to the input file
%  @param OutputFile  Path to write the output Prolog facts
process_domain_model(InputFile, OutputFile) :-
    process_domain_model(InputFile, OutputFile, []).

%% process_domain_model(+InputFile, +OutputFile, +Options) is det
%
%  Processes a domain model with options.
%
%  Options:
%    - model_name(Name)     : Name for the model (default: extracted from file)
%    - validate(true/false) : Whether to validate the model (default: true)
%    - format(Format)       : Output format: prolog, workflowy, report (default: prolog)
%
%  @param InputFile   Path to the input file
%  @param OutputFile  Path to write the output
%  @param Options     List of processing options
process_domain_model(InputFile, OutputFile, Options) :-
    % Clear any existing model
    clear_model,

    % Load the input file
    load_domain_input(InputFile),

    % Extract model name
    option_or_default(model_name(ModelName), Options, default_model),

    % Validate if requested
    (   option_or_default(validate(Validate), Options, true),
        Validate == true
    ->  validate_loaded_model
    ;   true
    ),

    % Export in requested format
    option_or_default(format(Format), Options, prolog),
    export_in_format(ModelName, OutputFile, Format).

%% --------------------------------------------------------------------------
%% Input Loading
%% --------------------------------------------------------------------------

%% load_domain_input(+InputFile) is det
%
%  Loads domain input from a file.
%  Supports Prolog files (.pl) containing model-building predicates.
load_domain_input(InputFile) :-
    atom_string(InputFileAtom, InputFile),
    (   exists_file(InputFileAtom)
    ->  consult(InputFileAtom)
    ;   throw(error(file_not_found(InputFile), context(load_domain_input/1, _)))
    ).

%% --------------------------------------------------------------------------
%% Validation
%% --------------------------------------------------------------------------

%% validate_loaded_model is semidet
%
%  Validates all bounded contexts in the loaded model.
validate_loaded_model :-
    forall(
        bounded_context(ContextId, _, _),
        (   validate_model(ContextId)
        ->  true
        ;   format(user_error, 'Warning: Validation failed for context ~w~n', [ContextId])
        )
    ).

%% --------------------------------------------------------------------------
%% Export Dispatch
%% --------------------------------------------------------------------------

%% export_in_format(+ModelName, +OutputFile, +Format) is det
%
%  Exports the model in the specified format.
export_in_format(ModelName, OutputFile, prolog) :-
    export_model_to_file(ModelName, OutputFile).
export_in_format(ModelName, OutputFile, workflowy) :-
    export_to_workflowy(ModelName, Text),
    open(OutputFile, write, Stream),
    write(Stream, Text),
    close(Stream).
export_in_format(ModelName, OutputFile, report) :-
    generate_model_report(ModelName, Report),
    open(OutputFile, write, Stream),
    write(Stream, Report),
    close(Stream).
export_in_format(_, _, Format) :-
    throw(error(unknown_format(Format), context(export_in_format/3, _))).

%% --------------------------------------------------------------------------
%% Utility Predicates
%% --------------------------------------------------------------------------

%% option_or_default(+Option, +Options, +Default) is det
%
%  Extracts an option value or returns the default.
option_or_default(Option, Options, _Default) :-
    memberchk(Option, Options),
    !.
option_or_default(Option, _Options, Default) :-
    Option =.. [_, Default].

%% exists_file(+Path) is semidet
%
%  Checks if a file exists at the given path.
exists_file(Path) :-
    catch(
        (open(Path, read, S), close(S)),
        _,
        fail
    ).

%% --------------------------------------------------------------------------
%% Re-export clear_model from ddd_schema
%% --------------------------------------------------------------------------

% clear_model/0 is already exported from ddd_schema
