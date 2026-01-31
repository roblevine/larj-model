%% test_ecommerce_validation.pl - Validation Tests for E-Commerce Model
%
%  Runs comprehensive validation queries against the e-commerce example model
%  to demonstrate the validation framework and verify model correctness.
%
%  Usage:
%    ?- run_all_validation_tests.
%
%  @author Generated for larj-model
%  @version 1.0

:- use_module('../src/ddd_schema').
:- use_module('../src/model_builder').
:- use_module('../src/model_validate').
:- use_module('../src/model_visualize').
:- use_module('../examples/ecommerce_model').

%% --------------------------------------------------------------------------
%% Test Runner
%% --------------------------------------------------------------------------

%% run_all_validation_tests is det
%
%  Runs all validation tests and prints results.
run_all_validation_tests :-
    writeln('=== E-Commerce Model Validation Tests ==='),
    nl,

    % First, build the model
    writeln('Building e-commerce model...'),
    build_ecommerce_model,
    writeln('Model built successfully.'),
    nl,

    % Run individual test categories
    run_context_tests,
    run_archetype_tests,
    run_mi_tests,
    run_role_tests,
    run_ppt_tests,
    run_description_tests,
    run_aggregate_tests,
    run_link_pattern_tests,
    run_error_detection_tests,
    run_query_helper_tests,

    % Generate full validation report
    nl,
    writeln('=== Full Validation Report ==='),
    generate_validation_report(ecommerce, Report),
    writeln(Report),

    nl,
    writeln('=== All Tests Complete ===').

%% --------------------------------------------------------------------------
%% Context Tests
%% --------------------------------------------------------------------------

run_context_tests :-
    writeln('--- Bounded Context Tests ---'),

    % Test: All contexts are defined
    writeln('Test: Bounded contexts exist'),
    findall(Id, bounded_context(Id, _, _), Contexts),
    length(Contexts, CtxCount),
    format('  Found ~w bounded contexts: ~w~n', [CtxCount, Contexts]),
    assertion(CtxCount >= 3),

    % Test: Context terms are defined
    writeln('Test: Context terms exist'),
    findall(ctx_term(Ctx, Term), context_term(Ctx, Term, _), Terms),
    length(Terms, TermCount),
    format('  Found ~w context terms~n', [TermCount]),
    assertion(TermCount > 0),

    % Test: Context relationships exist
    writeln('Test: Context relationships exist'),
    findall(rel(Up, Down, Pat), context_relationship(Up, Down, Pat, _), Rels),
    length(Rels, RelCount),
    format('  Found ~w context relationships: ~w~n', [RelCount, Rels]),
    assertion(RelCount >= 2),

    nl.

%% --------------------------------------------------------------------------
%% Archetype Classification Tests
%% --------------------------------------------------------------------------

run_archetype_tests :-
    writeln('--- Archetype Classification Tests ---'),

    % Test: Archetype coverage
    writeln('Test: Archetype coverage'),
    (check_archetype_coverage(Details) ->
        format('  PASS: All objects uniquely classified~n  Details: ~w~n', [Details])
    ;
        format('  FAIL: Some objects have multiple or no classifications~n', [])
    ),

    % Test: Count objects by archetype
    writeln('Test: Count objects by archetype'),
    findall(Id, moment_interval(Id, _), MIs),
    findall(Id, role(Id, _), Roles),
    findall(Id, party_place_thing(Id, _), PPTs),
    findall(Id, description(Id, _), Descs),
    length(MIs, MICount),
    length(Roles, RoleCount),
    length(PPTs, PPTCount),
    length(Descs, DescCount),
    format('  Moment-Intervals: ~w (~w)~n', [MICount, MIs]),
    format('  Roles: ~w (~w)~n', [RoleCount, Roles]),
    format('  Party/Place/Things: ~w (~w)~n', [PPTCount, PPTs]),
    format('  Descriptions: ~w (~w)~n', [DescCount, Descs]),
    assertion(MICount > 0),
    assertion(RoleCount > 0),
    assertion(PPTCount > 0),
    assertion(DescCount > 0),

    nl.

%% --------------------------------------------------------------------------
%% Moment-Interval Tests
%% --------------------------------------------------------------------------

run_mi_tests :-
    writeln('--- Moment-Interval Tests ---'),

    % Test: Temporal nature specified
    writeln('Test: Temporal nature specified'),
    (check_mi_temporal_nature(Details) ->
        format('  PASS: All MIs have temporal type~n  Details: ~w~n', [Details])
    ;
        format('  FAIL: Some MIs missing temporal type~n', [])
    ),

    % Test: Lifecycle states defined
    writeln('Test: Lifecycle states'),
    check_mi_lifecycle(LifecycleDetails),
    format('  Lifecycle details: ~w~n', [LifecycleDetails]),

    % Test: MI details
    writeln('Test: MI details (line items)'),
    check_mi_details(DetailInfo),
    format('  MI details: ~w~n', [DetailInfo]),

    % Test: Plan/actual relationships
    writeln('Test: Plan/actual relationships'),
    (check_mi_plan_actual(PlanActualDetails) ->
        format('  PASS: Plan/actual links valid~n  Details: ~w~n', [PlanActualDetails])
    ;
        format('  WARN: Some plan/actual issues~n', [])
    ),

    % Query: List all MI status transitions
    writeln('Query: MI status transitions'),
    forall(
        (moment_interval(MIId, _), mi_status(MIId, Status, status(Transitions, _))),
        format('  ~w: ~w -> ~w~n', [MIId, Status, Transitions])
    ),

    nl.

%% --------------------------------------------------------------------------
%% Role Tests
%% --------------------------------------------------------------------------

run_role_tests :-
    writeln('--- Role Tests ---'),

    % Test: All roles have players
    writeln('Test: Roles have players'),
    (check_role_players(PlayerDetails) ->
        format('  PASS: All roles have valid players~n  Details: ~w~n', [PlayerDetails])
    ;
        format('  WARN: Some roles missing players~n', [])
    ),

    % Test: Role contexts
    writeln('Test: Role contexts specified'),
    check_role_context(ContextDetails),
    format('  Role contexts: ~w~n', [ContextDetails]),

    % Test: Role MI participation
    writeln('Test: Role MI participation'),
    check_role_mi_links(MILinkDetails),
    format('  Role-MI links: ~w~n', [MILinkDetails]),

    % Query: List role-player-MI chains
    writeln('Query: Role participation chains'),
    forall(
        (role(RoleId, _), role_played_by(RoleId, PPTId), role_participates_in(RoleId, MIId)),
        format('  ~w (played by ~w) participates in ~w~n', [RoleId, PPTId, MIId])
    ),

    nl.

%% --------------------------------------------------------------------------
%% Party/Place/Thing Tests
%% --------------------------------------------------------------------------

run_ppt_tests :-
    writeln('--- Party/Place/Thing Tests ---'),

    % Test: Subtypes assigned
    writeln('Test: PPT subtypes assigned'),
    (check_ppt_subtype(SubtypeDetails) ->
        format('  PASS: All PPTs have subtypes~n  Details: ~w~n', [SubtypeDetails])
    ;
        format('  FAIL: Some PPTs missing subtypes~n', [])
    ),

    % Test: PPT identities
    writeln('Test: PPT identity attributes'),
    check_ppt_identity(IdDetails),
    format('  Identity details: ~w~n', [IdDetails]),

    % Test: PPT description links
    writeln('Test: PPT description links'),
    check_ppt_description_links(DescLinkDetails),
    format('  Description links: ~w~n', [DescLinkDetails]),

    % Test: PPT roles
    writeln('Test: PPT roles'),
    check_ppt_roles(RoleDetails),
    format('  PPT roles: ~w~n', [RoleDetails]),

    % Query: List all PPT attributes
    writeln('Query: PPT attributes'),
    forall(
        (party_place_thing(PPTId, _), ppt_attribute(PPTId, AttrName, _)),
        format('  ~w.~w~n', [PPTId, AttrName])
    ),

    nl.

%% --------------------------------------------------------------------------
%% Description Tests
%% --------------------------------------------------------------------------

run_description_tests :-
    writeln('--- Description Tests ---'),

    % Test: Description reuse
    writeln('Test: Description reuse'),
    check_description_reuse(ReuseDetails),
    format('  Reuse details: ~w~n', [ReuseDetails]),

    % Test: Description defaults
    writeln('Test: Description defaults'),
    check_description_defaults(DefaultDetails),
    format('  Default details: ~w~n', [DefaultDetails]),

    % Test: Description instance links
    writeln('Test: Description instance links'),
    check_description_instance_links(InstanceDetails),
    format('  Instance links: ~w~n', [InstanceDetails]),

    % Query: List all description defaults
    writeln('Query: Description default values'),
    forall(
        desc_default_value(DescId, AttrName, Value),
        format('  ~w.~w = ~w~n', [DescId, AttrName, Value])
    ),

    nl.

%% --------------------------------------------------------------------------
%% Aggregate Tests
%% --------------------------------------------------------------------------

run_aggregate_tests :-
    writeln('--- Aggregate Tests ---'),

    % Test: Aggregate boundaries
    writeln('Test: Aggregate boundaries'),
    (check_aggregate_boundaries(BoundaryDetails) ->
        format('  PASS: All aggregates have roots~n  Details: ~w~n', [BoundaryDetails])
    ;
        format('  FAIL: Some aggregates missing roots~n', [])
    ),

    % Test: MI aggregate cohesion
    writeln('Test: MI-Detail cohesion'),
    (check_mi_aggregate_cohesion(CohesionDetails) ->
        format('  PASS: MI and details in same aggregates~n  Details: ~w~n', [CohesionDetails])
    ;
        format('  WARN: Some MI-detail cohesion issues~n', [])
    ),

    % Test: Aggregate invariants
    writeln('Test: Aggregate invariants'),
    check_aggregate_invariants(InvariantDetails),
    format('  Invariant details: ~w~n', [InvariantDetails]),

    % Test: Cross-aggregate references
    writeln('Test: Cross-aggregate references'),
    check_cross_aggregate_refs(RefDetails),
    format('  Cross-references: ~w~n', [RefDetails]),

    % Query: List aggregate structure
    writeln('Query: Aggregate structure'),
    forall(
        (aggregate(AggId, Ctx), aggregate_root(AggId, Root)),
        (
            findall(M, aggregate_member(AggId, M), Members),
            format('  ~w [~w]: root=~w, members=~w~n', [AggId, Ctx, Root, Members])
        )
    ),

    nl.

%% --------------------------------------------------------------------------
%% Link Pattern Tests
%% --------------------------------------------------------------------------

run_link_pattern_tests :-
    writeln('--- Link Pattern Tests (Blue->Green->Yellow->Pink) ---'),

    % Test: Blue -> Green (Description -> PPT)
    writeln('Test: Blue -> Green links'),
    check_blue_green_links(BlueGreenResult),
    format('  Result: ~w~n', [BlueGreenResult]),

    % Test: Green -> Yellow (PPT -> Role)
    writeln('Test: Green -> Yellow links'),
    check_green_yellow_links(GreenYellowResult),
    format('  Result: ~w~n', [GreenYellowResult]),

    % Test: Yellow -> Pink (Role -> MI)
    writeln('Test: Yellow -> Pink links'),
    check_yellow_pink_links(YellowPinkResult),
    format('  Result: ~w~n', [YellowPinkResult]),

    % Test: Pink -> Detail
    writeln('Test: Pink -> Detail links'),
    check_pink_detail_links(PinkDetailResult),
    format('  Result: ~w~n', [PinkDetailResult]),

    % Test: Overall link consistency
    writeln('Test: Overall link consistency'),
    (check_link_consistency(ConsistencyDetails) ->
        format('  PASS: Link pattern valid~n  Details: ~w~n', [ConsistencyDetails])
    ;
        format('  WARN: Some link pattern issues~n', [])
    ),

    nl.

%% --------------------------------------------------------------------------
%% Error Detection Tests
%% --------------------------------------------------------------------------

run_error_detection_tests :-
    writeln('--- Common Error Detection ---'),

    % Test: MI modeled as Thing
    writeln('Check: MI modeled as Thing'),
    detect_mi_as_thing(MIAsThingIssues),
    (MIAsThingIssues = [] ->
        writeln('  No issues found')
    ;
        format('  Issues: ~w~n', [MIAsThingIssues])
    ),

    % Test: Missing role separation
    writeln('Check: Missing role separation'),
    detect_missing_role_separation(RoleSepIssues),
    (RoleSepIssues = [] ->
        writeln('  No issues found')
    ;
        format('  Issues: ~w~n', [RoleSepIssues])
    ),

    % Test: Description as Thing
    writeln('Check: Description modeled as Thing'),
    detect_description_as_thing(DescAsThingIssues),
    (DescAsThingIssues = [] ->
        writeln('  No issues found')
    ;
        format('  Issues: ~w~n', [DescAsThingIssues])
    ),

    % Test: Thing as Description
    writeln('Check: Thing modeled as Description'),
    detect_thing_as_description(ThingAsDescIssues),
    (ThingAsDescIssues = [] ->
        writeln('  No issues found')
    ;
        format('  Issues: ~w~n', [ThingAsDescIssues])
    ),

    % Test: Oversized aggregates
    writeln('Check: Oversized aggregates'),
    detect_oversized_aggregate(OversizedIssues),
    (OversizedIssues = [] ->
        writeln('  No issues found')
    ;
        format('  Issues: ~w~n', [OversizedIssues])
    ),

    % Test: Role without player
    writeln('Check: Role without player'),
    detect_role_without_player(RoleNoPlayerIssues),
    (RoleNoPlayerIssues = [] ->
        writeln('  No issues found')
    ;
        format('  Issues: ~w~n', [RoleNoPlayerIssues])
    ),

    nl.

%% --------------------------------------------------------------------------
%% Query Helper Tests
%% --------------------------------------------------------------------------

run_query_helper_tests :-
    writeln('--- Query Helper Tests ---'),

    % Query: Orphan objects
    writeln('Query: Orphan objects (not in any aggregate)'),
    list_orphan_objects(Orphans),
    format('  Orphan objects: ~w~n', [Orphans]),

    % Query: Unlinked roles
    writeln('Query: Unlinked roles'),
    list_unlinked_roles(UnlinkedRoles),
    format('  Unlinked roles: ~w~n', [UnlinkedRoles]),

    % Query: MIs without participants
    writeln('Query: MIs without participants'),
    list_mi_without_participants(MIsNoParticipants),
    format('  MIs without participants: ~w~n', [MIsNoParticipants]),

    % Query: PPTs without roles
    writeln('Query: PPTs without roles'),
    list_ppt_without_roles(PPTsNoRoles),
    format('  PPTs without roles: ~w~n', [PPTsNoRoles]),

    nl.

%% --------------------------------------------------------------------------
%% Visualization Test
%% --------------------------------------------------------------------------

run_visualization_test :-
    writeln('--- Visualization Test ---'),

    % Generate DOT file
    writeln('Generating DOT file...'),
    generate_dot_file(ecommerce, '/tmp/ecommerce_model.dot', []),
    writeln('  Created: /tmp/ecommerce_model.dot'),

    % Generate context map
    writeln('Generating context map DOT...'),
    generate_context_map_dot(ecommerce, ContextMapDot),
    open('/tmp/ecommerce_context_map.dot', write, Stream),
    write(Stream, ContextMapDot),
    close(Stream),
    writeln('  Created: /tmp/ecommerce_context_map.dot'),

    writeln('To render: dot -Tpng /tmp/ecommerce_model.dot -o /tmp/ecommerce_model.png'),

    nl.

%% --------------------------------------------------------------------------
%% Utility Predicates
%% --------------------------------------------------------------------------

%% assertion(+Goal) is det
%
%  Simple assertion that prints PASS/FAIL.
assertion(Goal) :-
    (call(Goal) ->
        true
    ;
        format('  ASSERTION FAILED: ~w~n', [Goal]),
        fail
    ).
