%% model_validate.pl - Domain Model Validation Queries
%
%  Comprehensive validation queries based on DDD archetype rules.
%  Tests model correctness, completeness, and adherence to patterns.
%
%  @author Generated for larj-model
%  @version 1.0

:- module(model_validate, [
    % Full validation
    validate_full_model/1,
    validate_context/2,

    % Archetype classification validation
    check_archetype_coverage/1,
    check_temporal_concepts/1,
    check_role_separation/1,
    check_description_extraction/1,
    check_link_consistency/1,

    % Moment-Interval validation
    check_mi_temporal_nature/1,
    check_mi_lifecycle/1,
    check_mi_details/1,
    check_mi_plan_actual/1,

    % Role validation
    check_role_players/1,
    check_role_context/1,
    check_role_mi_links/1,

    % Party/Place/Thing validation
    check_ppt_subtype/1,
    check_ppt_identity/1,
    check_ppt_description_links/1,
    check_ppt_roles/1,

    % Description validation
    check_description_reuse/1,
    check_description_defaults/1,
    check_description_instance_links/1,

    % Aggregate validation
    check_aggregate_boundaries/1,
    check_mi_aggregate_cohesion/1,
    check_cross_aggregate_refs/1,
    check_aggregate_invariants/1,

    % Link pattern validation
    check_blue_green_links/1,
    check_green_yellow_links/1,
    check_yellow_pink_links/1,
    check_pink_detail_links/1,

    % Common error detection
    detect_mi_as_thing/1,
    detect_missing_role_separation/1,
    detect_description_as_thing/1,
    detect_thing_as_description/1,
    detect_oversized_aggregate/1,
    detect_role_without_player/1,

    % Query helpers
    list_orphan_objects/1,
    list_unlinked_roles/1,
    list_mi_without_participants/1,
    list_ppt_without_roles/1,

    % Validation report generation
    generate_validation_report/2
]).

:- use_module(ddd_schema).
:- use_module(model_builder).

%% --------------------------------------------------------------------------
%% Full Model Validation
%% --------------------------------------------------------------------------

%% validate_full_model(-Results) is det
%
%  Runs all validation checks and returns a structured result.
%
%  @param Results  List of validation results: result(Check, Status, Details)
validate_full_model(Results) :-
    findall(Result, run_validation_check(Result), Results).

run_validation_check(result(archetype_coverage, Status, Details)) :-
    (check_archetype_coverage(Details) -> Status = pass ; Status = fail).
run_validation_check(result(temporal_concepts, Status, Details)) :-
    (check_temporal_concepts(Details) -> Status = pass ; Status = fail).
run_validation_check(result(role_separation, Status, Details)) :-
    (check_role_separation(Details) -> Status = pass ; Status = fail).
run_validation_check(result(link_consistency, Status, Details)) :-
    (check_link_consistency(Details) -> Status = pass ; Status = fail).
run_validation_check(result(mi_lifecycle, Status, Details)) :-
    (check_mi_lifecycle(Details) -> Status = pass ; Status = fail).
run_validation_check(result(role_players, Status, Details)) :-
    (check_role_players(Details) -> Status = pass ; Status = fail).
run_validation_check(result(aggregate_boundaries, Status, Details)) :-
    (check_aggregate_boundaries(Details) -> Status = pass ; Status = fail).

%% validate_context(+ContextId, -Results) is det
%
%  Validates a single bounded context.
validate_context(ContextId, Results) :-
    bounded_context(ContextId, _, _),
    findall(Result, run_context_check(ContextId, Result), Results).

run_context_check(ContextId, result(objects_classified, Status, Objects)) :-
    objects_in_context(ContextId, Objects),
    (Objects \= [] -> Status = pass ; Status = warning).
run_context_check(ContextId, result(terms_defined, Status, Terms)) :-
    findall(T, context_term(ContextId, T, _), Terms),
    (Terms \= [] -> Status = pass ; Status = warning).

%% --------------------------------------------------------------------------
%% Archetype Classification Validation
%% --------------------------------------------------------------------------

%% check_archetype_coverage(-Details) is semidet
%
%  Verifies all objects are classified under exactly one archetype.
check_archetype_coverage(Details) :-
    findall(obj(Id, Archetypes), (
        (moment_interval(Id, _) ; role(Id, _) ; party_place_thing(Id, _) ; description(Id, _)),
        findall(A, (
            (moment_interval(Id, _), A = moment_interval) ;
            (role(Id, _), A = role) ;
            (party_place_thing(Id, _), A = party_place_thing) ;
            (description(Id, _), A = description)
        ), Archetypes)
    ), AllObjects),
    findall(Id, (
        member(obj(Id, Archs), AllObjects),
        length(Archs, L),
        L \= 1
    ), MultiClassified),
    Details = coverage(AllObjects, multi_classified(MultiClassified)),
    MultiClassified = [].

%% check_temporal_concepts(-Details) is semidet
%
%  Verifies all moment-intervals have temporal nature specified.
check_temporal_concepts(Details) :-
    findall(Id, (
        moment_interval(Id, _),
        \+ mi_temporal_type(Id, _)
    ), MissingTemporal),
    Details = missing_temporal(MissingTemporal),
    MissingTemporal = [].

%% check_role_separation(-Details) is semidet
%
%  Verifies roles are properly separated from their players.
check_role_separation(Details) :-
    findall(RoleId, (
        role(RoleId, _),
        \+ role_played_by(RoleId, _)
    ), OrphanRoles),
    Details = orphan_roles(OrphanRoles),
    OrphanRoles = [].

%% check_description_extraction(-Details) is semidet
%
%  Verifies descriptions have applies_to links.
check_description_extraction(Details) :-
    findall(Id, (
        description(Id, _),
        \+ desc_applies_to(Id, _)
    ), UnlinkedDescs),
    Details = unlinked_descriptions(UnlinkedDescs),
    UnlinkedDescs = [].

%% check_link_consistency(-Details) is semidet
%
%  Verifies the Blue→Green→Yellow→Pink link pattern.
%  Succeeds if no critical issues (ok or warnings are acceptable).
check_link_consistency(Details) :-
    check_blue_green_links(BlueGreen),
    check_green_yellow_links(GreenYellow),
    check_yellow_pink_links(YellowPink),
    Details = links(BlueGreen, GreenYellow, YellowPink),
    % Allow ok(_) or warnings(_), but not issues(_)
    \+ BlueGreen = issues(_),
    \+ GreenYellow = issues(_).

%% --------------------------------------------------------------------------
%% Moment-Interval Validation
%% --------------------------------------------------------------------------

%% check_mi_temporal_nature(-Details) is semidet
%
%  Verifies all MIs have moment or interval specified.
check_mi_temporal_nature(Details) :-
    findall(mi(Id, Type), (
        moment_interval(Id, _),
        (mi_temporal_type(Id, Type) -> true ; Type = missing)
    ), MITypes),
    findall(Id, member(mi(Id, missing), MITypes), Missing),
    Details = mi_temporal(MITypes, missing(Missing)),
    Missing = [].

%% check_mi_lifecycle(-Details) is semidet
%
%  Verifies MIs have status states defined.
check_mi_lifecycle(Details) :-
    findall(mi(Id, Statuses), (
        moment_interval(Id, _),
        findall(S, mi_status(Id, S, _), Statuses)
    ), MIStatuses),
    findall(Id, (
        member(mi(Id, []), MIStatuses)
    ), NoLifecycle),
    Details = mi_lifecycle(MIStatuses, no_lifecycle(NoLifecycle)).

%% check_mi_details(-Details) is semidet
%
%  Lists MIs and their details.
check_mi_details(Details) :-
    findall(mi(Id, DetailList), (
        moment_interval(Id, _),
        findall(D, mi_detail(Id, D, _), DetailList)
    ), MIDetails),
    Details = mi_details(MIDetails).

%% check_mi_plan_actual(-Details) is semidet
%
%  Verifies plan/actual relationships are properly linked.
check_mi_plan_actual(Details) :-
    findall(link(Plan, Actual, Type),
        mi_plan_actual(Plan, Actual, Type),
        Links),
    findall(Plan, (
        mi_plan_actual(Plan, Actual, _),
        \+ moment_interval(Actual, _)
    ), InvalidLinks),
    Details = plan_actual(Links, invalid(InvalidLinks)),
    InvalidLinks = [].

%% --------------------------------------------------------------------------
%% Role Validation
%% --------------------------------------------------------------------------

%% check_role_players(-Details) is semidet
%
%  Verifies all roles have valid players.
check_role_players(Details) :-
    findall(role(RoleId, PlayerId),
        role_played_by(RoleId, PlayerId),
        RolePlayers),
    findall(RoleId, (
        role(RoleId, _),
        \+ role_played_by(RoleId, _)
    ), NoPlayer),
    findall(RoleId, (
        role_played_by(RoleId, PlayerId),
        \+ party_place_thing(PlayerId, _)
    ), InvalidPlayer),
    Details = role_players(RolePlayers, no_player(NoPlayer), invalid_player(InvalidPlayer)),
    NoPlayer = [],
    InvalidPlayer = [].

%% check_role_context(-Details) is semidet
%
%  Verifies roles have context information.
check_role_context(Details) :-
    findall(role(Id, Context), (
        role(Id, _),
        (role_attribute(Id, role_context, attr(_, _, Context)) -> true ; Context = missing)
    ), RoleContexts),
    findall(Id, member(role(Id, missing), RoleContexts), Missing),
    Details = role_contexts(RoleContexts, missing(Missing)).

%% check_role_mi_links(-Details) is semidet
%
%  Verifies roles participate in moment-intervals.
check_role_mi_links(Details) :-
    findall(role(RoleId, MIs), (
        role(RoleId, _),
        findall(MI, role_participates_in(RoleId, MI), MIs)
    ), RoleMIs),
    findall(RoleId, member(role(RoleId, []), RoleMIs), NoMI),
    Details = role_mi_links(RoleMIs, no_participation(NoMI)).

%% --------------------------------------------------------------------------
%% Party/Place/Thing Validation
%% --------------------------------------------------------------------------

%% check_ppt_subtype(-Details) is semidet
%
%  Verifies all PPTs have subtypes assigned.
check_ppt_subtype(Details) :-
    findall(ppt(Id, Subtype), (
        party_place_thing(Id, _),
        (ppt_subtype(Id, Subtype) -> true ; Subtype = missing)
    ), PPTSubtypes),
    findall(Id, member(ppt(Id, missing), PPTSubtypes), Missing),
    Details = ppt_subtypes(PPTSubtypes, missing(Missing)),
    Missing = [].

%% check_ppt_identity(-Details) is semidet
%
%  Verifies PPTs have identifier attributes.
check_ppt_identity(Details) :-
    findall(ppt(Id, HasId), (
        party_place_thing(Id, _),
        (   (ppt_attribute(Id, identifier, _) ;
             ppt_attribute(Id, id, _) ;
             ppt_attribute(Id, sku, _) ;
             ppt_attribute(Id, customer_id, _))
        ->  HasId = yes
        ;   HasId = no
        )
    ), PPTIds),
    findall(Id, member(ppt(Id, no), PPTIds), NoId),
    Details = ppt_identity(PPTIds, no_identity(NoId)).

%% check_ppt_description_links(-Details) is semidet
%
%  Lists PPT to Description links.
check_ppt_description_links(Details) :-
    findall(link(PPTId, DescId),
        ppt_described_by(PPTId, DescId),
        Links),
    findall(PPTId, (
        party_place_thing(PPTId, _),
        \+ ppt_described_by(PPTId, _)
    ), Unlinked),
    Details = ppt_desc_links(Links, unlinked(Unlinked)).

%% check_ppt_roles(-Details) is semidet
%
%  Lists PPTs and their roles.
check_ppt_roles(Details) :-
    findall(ppt(Id, Roles), (
        party_place_thing(Id, _),
        findall(R, ppt_plays_role(Id, R), Roles)
    ), PPTRoles),
    findall(Id, member(ppt(Id, []), PPTRoles), NoRoles),
    Details = ppt_roles(PPTRoles, no_roles(NoRoles)).

%% --------------------------------------------------------------------------
%% Description Validation
%% --------------------------------------------------------------------------

%% check_description_reuse(-Details) is semidet
%
%  Verifies descriptions apply to multiple instances (conceptually).
check_description_reuse(Details) :-
    findall(desc(Id, AppliesTo), (
        description(Id, _),
        findall(T, desc_applies_to(Id, T), AppliesTo)
    ), DescApplies),
    Details = description_reuse(DescApplies).

%% check_description_defaults(-Details) is semidet
%
%  Verifies descriptions have default values.
check_description_defaults(Details) :-
    findall(desc(Id, Defaults), (
        description(Id, _),
        findall(default(N, V), desc_default_value(Id, N, V), Defaults)
    ), DescDefaults),
    findall(Id, member(desc(Id, []), DescDefaults), NoDefaults),
    Details = description_defaults(DescDefaults, no_defaults(NoDefaults)).

%% check_description_instance_links(-Details) is semidet
%
%  Verifies descriptions link to PPT types.
check_description_instance_links(Details) :-
    findall(desc(Id, Type), (
        description(Id, _),
        (desc_applies_to(Id, Type) -> true ; Type = none)
    ), DescLinks),
    findall(Id, member(desc(Id, none), DescLinks), NoLink),
    Details = description_links(DescLinks, no_link(NoLink)).

%% --------------------------------------------------------------------------
%% Aggregate Validation
%% --------------------------------------------------------------------------

%% check_aggregate_boundaries(-Details) is semidet
%
%  Verifies aggregates have roots and members.
check_aggregate_boundaries(Details) :-
    findall(agg(Id, Root, Members), (
        aggregate(Id, _),
        (aggregate_root(Id, Root) -> true ; Root = none),
        findall(M, aggregate_member(Id, M), Members)
    ), Aggregates),
    findall(Id, member(agg(Id, none, _), Aggregates), NoRoot),
    Details = aggregate_boundaries(Aggregates, no_root(NoRoot)),
    NoRoot = [].

%% check_mi_aggregate_cohesion(-Details) is semidet
%
%  Verifies MI + Details are in the same aggregate.
check_mi_aggregate_cohesion(Details) :-
    findall(issue(MI, Detail, MIAgg, DetailAgg), (
        mi_detail(MI, Detail, _),
        aggregate_root(MIAgg, MI),
        \+ aggregate_member(MIAgg, Detail),
        (aggregate_root(DetailAgg, Detail) -> true ; DetailAgg = none)
    ), Issues),
    Details = mi_cohesion_issues(Issues),
    Issues = [].

%% check_cross_aggregate_refs(-Details) is semidet
%
%  Lists cross-aggregate references (should be by ID only).
check_cross_aggregate_refs(Details) :-
    findall(ref(From, To, Type), (
        (   role_played_by(From, To), Type = role_to_ppt
        ;   role_participates_in(From, To), Type = role_to_mi
        ;   ppt_described_by(From, To), Type = ppt_to_desc
        )
    ), Refs),
    Details = cross_refs(Refs).

%% check_aggregate_invariants(-Details) is semidet
%
%  Lists aggregate invariants.
check_aggregate_invariants(Details) :-
    findall(agg(Id, Invariants), (
        aggregate(Id, _),
        findall(I, aggregate_invariant(Id, I), Invariants)
    ), AggInvariants),
    findall(Id, member(agg(Id, []), AggInvariants), NoInvariants),
    Details = aggregate_invariants(AggInvariants, no_invariants(NoInvariants)).

%% --------------------------------------------------------------------------
%% Link Pattern Validation
%% --------------------------------------------------------------------------

%% check_blue_green_links(-Result) is det
%
%  Verifies Description → PPT links.
%  Checks both explicit ppt_described_by links and type-based matches.
check_blue_green_links(Result) :-
    % Find explicit links via ppt_described_by
    findall(link(Desc, PPT), (
        description(Desc, _),
        ppt_described_by(PPT, Desc)
    ), ExplicitLinks),
    % Find type-based links (Description applies to PPT type)
    findall(link(Desc, PPT), (
        description(Desc, _),
        desc_applies_to(Desc, PPTTypeOrId),
        party_place_thing(PPT, _),
        (   PPT = PPTTypeOrId                    % Direct ID match
        ;   ppt_subtype(PPT, PPTTypeOrId)        % Subtype match
        )
    ), TypeLinks),
    append(ExplicitLinks, TypeLinks, AllLinks),
    sort(AllLinks, ValidLinks),
    findall(Desc, (
        description(Desc, _),
        \+ desc_applies_to(Desc, _),
        \+ ppt_described_by(_, Desc)
    ), Unlinked),
    (Unlinked = [] -> Result = ok(ValidLinks) ; Result = issues(Unlinked)).

%% check_green_yellow_links(-Result) is det
%
%  Verifies PPT → Role links.
check_green_yellow_links(Result) :-
    findall(link(PPT, Role), ppt_plays_role(PPT, Role), Links),
    findall(Role, (
        role(Role, _),
        \+ role_played_by(Role, _)
    ), OrphanRoles),
    (OrphanRoles = [] -> Result = ok(Links) ; Result = issues(OrphanRoles)).

%% check_yellow_pink_links(-Result) is det
%
%  Verifies Role → MI links.
check_yellow_pink_links(Result) :-
    findall(link(Role, MI), role_participates_in(Role, MI), Links),
    findall(MI, (
        moment_interval(MI, _),
        \+ role_participates_in(_, MI)
    ), NoParticipants),
    (NoParticipants = [] -> Result = ok(Links) ; Result = warnings(NoParticipants)).

%% check_pink_detail_links(-Result) is det
%
%  Verifies MI → Detail links.
check_pink_detail_links(Result) :-
    findall(link(MI, Detail), mi_detail(MI, Detail, _), Links),
    Result = ok(Links).

%% --------------------------------------------------------------------------
%% Common Error Detection
%% --------------------------------------------------------------------------

%% detect_mi_as_thing(-Issues) is det
%
%  Detects business events incorrectly modeled as things.
detect_mi_as_thing(Issues) :-
    findall(issue(Id, 'Thing with temporal attributes should be MI'), (
        party_place_thing(Id, _),
        ppt_subtype(Id, thing),
        (   ppt_attribute(Id, date, _)
        ;   ppt_attribute(Id, datetime, _)
        ;   ppt_attribute(Id, timestamp, _)
        ;   ppt_attribute(Id, status, _)
        )
    ), Issues).

%% detect_missing_role_separation(-Issues) is det
%
%  Detects PPTs directly participating in MIs without roles.
detect_missing_role_separation(Issues) :-
    findall(issue(PPT, MI, 'PPT participates directly in MI without Role'), (
        participates_in(PPT, MI, _),
        party_place_thing(PPT, _),
        moment_interval(MI, _)
    ), Issues).

%% detect_description_as_thing(-Issues) is det
%
%  Detects catalog entries incorrectly tracked individually.
detect_description_as_thing(Issues) :-
    findall(issue(Id, 'Thing with only type/category attributes may be Description'), (
        party_place_thing(Id, _),
        ppt_attribute(Id, type, _),
        \+ ppt_attribute(Id, serial_number, _),
        \+ ppt_attribute(Id, identifier, _)
    ), Issues).

%% detect_thing_as_description(-Issues) is det
%
%  Detects individual items incorrectly modeled as descriptions.
detect_thing_as_description(Issues) :-
    findall(issue(Id, 'Description with unique identifier should be Thing'), (
        description(Id, _),
        (   desc_attribute(Id, serial_number, _)
        ;   desc_attribute(Id, unique_id, _)
        )
    ), Issues).

%% detect_oversized_aggregate(-Issues) is det
%
%  Detects aggregates containing multiple MIs.
detect_oversized_aggregate(Issues) :-
    findall(issue(AggId, MIs, 'Aggregate contains multiple MIs'), (
        aggregate(AggId, _),
        findall(MI, (
            (aggregate_root(AggId, MI) ; aggregate_member(AggId, MI)),
            moment_interval(MI, _)
        ), MIs),
        length(MIs, L),
        L > 1
    ), Issues).

%% detect_role_without_player(-Issues) is det
%
%  Detects roles without associated PPTs.
detect_role_without_player(Issues) :-
    findall(issue(RoleId, 'Role has no player'), (
        role(RoleId, _),
        \+ role_played_by(RoleId, _)
    ), Issues).

%% --------------------------------------------------------------------------
%% Query Helpers
%% --------------------------------------------------------------------------

%% list_orphan_objects(-Orphans) is det
%
%  Lists objects not connected to any aggregate.
list_orphan_objects(Orphans) :-
    findall(obj(Id, Type), (
        (   moment_interval(Id, _), Type = moment_interval
        ;   role(Id, _), Type = role
        ;   party_place_thing(Id, _), Type = party_place_thing
        ;   description(Id, _), Type = description
        ),
        \+ aggregate_root(_, Id),
        \+ aggregate_member(_, Id)
    ), Orphans).

%% list_unlinked_roles(-Roles) is det
%
%  Lists roles without players or MI participation.
list_unlinked_roles(Roles) :-
    findall(role(Id, Issues), (
        role(Id, _),
        findall(Issue, (
            (\+ role_played_by(Id, _), Issue = no_player) ;
            (\+ role_participates_in(Id, _), Issue = no_mi)
        ), Issues),
        Issues \= []
    ), Roles).

%% list_mi_without_participants(-MIs) is det
%
%  Lists MIs without role participation.
list_mi_without_participants(MIs) :-
    findall(Id, (
        moment_interval(Id, _),
        \+ role_participates_in(_, Id)
    ), MIs).

%% list_ppt_without_roles(-PPTs) is det
%
%  Lists PPTs that don't play any roles.
list_ppt_without_roles(PPTs) :-
    findall(Id, (
        party_place_thing(Id, _),
        \+ ppt_plays_role(Id, _)
    ), PPTs).

%% --------------------------------------------------------------------------
%% Validation Report Generation
%% --------------------------------------------------------------------------

%% generate_validation_report(+ModelName, -Report) is det
%
%  Generates a comprehensive validation report.
generate_validation_report(ModelName, Report) :-
    validate_full_model(Results),

    % Count passes and failures
    findall(R, (member(R, Results), R = result(_, pass, _)), Passes),
    findall(R, (member(R, Results), R = result(_, fail, _)), Failures),
    length(Passes, PassCount),
    length(Failures, FailCount),

    % Collect error detections
    detect_mi_as_thing(MIAsThingIssues),
    detect_role_without_player(RoleNoPlayerIssues),
    detect_oversized_aggregate(OversizedAggIssues),
    list_orphan_objects(Orphans),
    list_unlinked_roles(UnlinkedRoles),
    list_mi_without_participants(MIsNoParticipants),

    % Build report
    format(atom(Header), '# Validation Report: ~w~n~n', [ModelName]),
    format(atom(Summary), '## Summary~n- Checks Passed: ~w~n- Checks Failed: ~w~n~n', [PassCount, FailCount]),

    format(atom(ResultSection), '## Check Results~n~w~n', [Results]),

    format(atom(IssuesSection), '## Issues Detected~n- MI as Thing: ~w~n- Role without Player: ~w~n- Oversized Aggregates: ~w~n~n',
        [MIAsThingIssues, RoleNoPlayerIssues, OversizedAggIssues]),

    format(atom(WarningsSection), '## Warnings~n- Orphan Objects: ~w~n- Unlinked Roles: ~w~n- MIs without Participants: ~w~n',
        [Orphans, UnlinkedRoles, MIsNoParticipants]),

    atomic_list_concat([Header, Summary, ResultSection, IssuesSection, WarningsSection], Report).
