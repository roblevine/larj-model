%% model_visualize.pl - Domain Model Visualization with Graphviz
%
%  Generates UML-style class diagrams from domain models using Graphviz DOT format.
%  Supports archetype colour coding and relationship visualization.
%
%  @author Generated for larj-model
%  @version 1.0

:- module(model_visualize, [
    % Main visualization predicates
    generate_dot/2,
    generate_dot/3,
    generate_dot_file/2,
    generate_dot_file/3,

    % Context-specific visualization
    generate_context_dot/3,
    generate_context_dot_file/3,

    % Aggregate visualization
    generate_aggregate_dot/2,

    % Context map visualization
    generate_context_map_dot/2,

    % Rendering helpers
    render_to_png/2,
    render_to_svg/2,
    render_to_pdf/2
]).

:- use_module(ddd_schema).
:- use_module(model_builder).

%% --------------------------------------------------------------------------
%% Colour Definitions (Archetype Colours)
%% --------------------------------------------------------------------------

archetype_colour(moment_interval, '#FFB6C1').  % Pink (Light Pink)
archetype_colour(mi_detail, '#FF69B4').        % Hot Pink (darker for details)
archetype_colour(role, '#FFFF99').             % Yellow (Light Yellow)
archetype_colour(party_place_thing, '#90EE90'). % Green (Light Green)
archetype_colour(description, '#ADD8E6').      % Blue (Light Blue)
archetype_colour(service, '#DDA0DD').          % Plum (for services)
archetype_colour(aggregate, '#F5F5F5').        % White Smoke (aggregate boundary)

archetype_border(moment_interval, '#FF1493').  % Deep Pink
archetype_border(mi_detail, '#C71585').        % Medium Violet Red
archetype_border(role, '#FFD700').             % Gold
archetype_border(party_place_thing, '#228B22'). % Forest Green
archetype_border(description, '#4169E1').      % Royal Blue
archetype_border(service, '#9932CC').          % Dark Orchid
archetype_border(aggregate, '#696969').        % Dim Gray

%% --------------------------------------------------------------------------
%% Main DOT Generation
%% --------------------------------------------------------------------------

%% generate_dot(+ModelName, -DotCode) is det
%
%  Generates Graphviz DOT code for the entire domain model.
%
%  @param ModelName  Name for the diagram title
%  @param DotCode    The generated DOT code as an atom
generate_dot(ModelName, DotCode) :-
    generate_dot(ModelName, DotCode, []).

%% generate_dot(+ModelName, -DotCode, +Options) is det
%
%  Generates DOT code with options.
%
%  Options:
%    - show_attributes(true/false)  : Show attribute compartments (default: true)
%    - show_behaviours(true/false)  : Show behaviour compartments (default: true)
%    - show_aggregates(true/false)  : Show aggregate boundaries (default: true)
%    - show_contexts(true/false)    : Show bounded context groupings (default: true)
%    - rankdir(TB/LR/BT/RL)        : Graph direction (default: TB)
generate_dot(ModelName, DotCode, Options) :-
    option_default(show_attributes, Options, true, ShowAttrs),
    option_default(show_behaviours, Options, true, ShowBehavs),
    option_default(show_aggregates, Options, true, ShowAggs),
    option_default(show_contexts, Options, true, ShowContexts),
    option_default(rankdir, Options, 'TB', RankDir),

    % Generate header
    format(atom(Header), 'digraph "~w" {~n', [ModelName]),
    format(atom(GraphAttrs), '  rankdir=~w;~n  compound=true;~n  node [shape=record, fontname="Helvetica", fontsize=10];~n  edge [fontname="Helvetica", fontsize=9];~n  labelloc="t";~n  label="Domain Model: ~w";~n~n', [RankDir, ModelName]),

    % Generate legend
    generate_legend(Legend),

    % Generate nodes grouped by bounded context if requested
    (ShowContexts == true ->
        generate_context_subgraphs(ShowAttrs, ShowBehavs, ContextSubgraphs),
        MINodes = '', RoleNodes = '', PPTNodes = '', DescNodes = '', ServiceNodes = ''
    ;
        ContextSubgraphs = '',
        generate_mi_nodes(ShowAttrs, ShowBehavs, MINodes),
        generate_role_nodes(ShowAttrs, ShowBehavs, RoleNodes),
        generate_ppt_nodes(ShowAttrs, ShowBehavs, PPTNodes),
        generate_desc_nodes(ShowAttrs, ShowBehavs, DescNodes),
        generate_service_nodes(ServiceNodes)
    ),

    % Generate aggregate subgraphs if requested (only when not using context grouping)
    (ShowAggs == true, ShowContexts == false ->
        generate_aggregate_subgraphs(AggSubgraphs)
    ;
        AggSubgraphs = ''
    ),

    % Generate relationships
    generate_relationships(Relationships),

    % Combine all parts
    atomic_list_concat([
        Header,
        GraphAttrs,
        Legend,
        '\n  // Bounded Contexts\n',
        ContextSubgraphs,
        MINodes,
        RoleNodes,
        PPTNodes,
        DescNodes,
        ServiceNodes,
        AggSubgraphs,
        '\n  // Relationships\n',
        Relationships,
        '}\n'
    ], DotCode).

%% generate_dot_file(+ModelName, +FilePath) is det
%
%  Generates DOT code and writes to file.
generate_dot_file(ModelName, FilePath) :-
    generate_dot_file(ModelName, FilePath, []).

generate_dot_file(ModelName, FilePath, Options) :-
    generate_dot(ModelName, DotCode, Options),
    open(FilePath, write, Stream),
    write(Stream, DotCode),
    close(Stream).

%% --------------------------------------------------------------------------
%% Legend Generation
%% --------------------------------------------------------------------------

generate_legend(Legend) :-
    archetype_colour(moment_interval, MICol),
    archetype_colour(role, RoleCol),
    archetype_colour(party_place_thing, PPTCol),
    archetype_colour(description, DescCol),
    format(atom(Legend), '
  // Legend
  subgraph cluster_legend {
    label="Archetype Legend";
    fontsize=12;
    style=rounded;
    color="#999999";

    legend_mi [label="Moment-Interval", style=filled, fillcolor="~w", shape=box];
    legend_role [label="Role", style=filled, fillcolor="~w", shape=box];
    legend_ppt [label="Party/Place/Thing", style=filled, fillcolor="~w", shape=box];
    legend_desc [label="Description", style=filled, fillcolor="~w", shape=box];

    legend_mi -> legend_role [style=invis];
    legend_role -> legend_ppt [style=invis];
    legend_ppt -> legend_desc [style=invis];
  }

', [MICol, RoleCol, PPTCol, DescCol]).

%% --------------------------------------------------------------------------
%% Node Generation
%% --------------------------------------------------------------------------

%% generate_mi_nodes(+ShowAttrs, +ShowBehavs, -Nodes) is det
%
%  Generates DOT nodes for all moment-intervals.
generate_mi_nodes(ShowAttrs, ShowBehavs, Nodes) :-
    findall(NodeCode, (
        moment_interval(Id, _),
        generate_class_node(Id, moment_interval, ShowAttrs, ShowBehavs, NodeCode)
    ), NodeCodes),
    atomic_list_concat(NodeCodes, Nodes).

%% generate_role_nodes(+ShowAttrs, +ShowBehavs, -Nodes) is det
generate_role_nodes(ShowAttrs, ShowBehavs, Nodes) :-
    findall(NodeCode, (
        role(Id, _),
        generate_class_node(Id, role, ShowAttrs, ShowBehavs, NodeCode)
    ), NodeCodes),
    atomic_list_concat(NodeCodes, Nodes).

%% generate_ppt_nodes(+ShowAttrs, +ShowBehavs, -Nodes) is det
generate_ppt_nodes(ShowAttrs, ShowBehavs, Nodes) :-
    findall(NodeCode, (
        party_place_thing(Id, _),
        generate_class_node(Id, party_place_thing, ShowAttrs, ShowBehavs, NodeCode)
    ), NodeCodes),
    atomic_list_concat(NodeCodes, Nodes).

%% generate_desc_nodes(+ShowAttrs, +ShowBehavs, -Nodes) is det
generate_desc_nodes(ShowAttrs, ShowBehavs, Nodes) :-
    findall(NodeCode, (
        description(Id, _),
        generate_class_node(Id, description, ShowAttrs, ShowBehavs, NodeCode)
    ), NodeCodes),
    atomic_list_concat(NodeCodes, Nodes).

%% generate_service_nodes(-Nodes) is det
generate_service_nodes(Nodes) :-
    archetype_colour(service, SvcCol),
    archetype_border(service, SvcBorder),
    findall(NodeCode, (
        domain_service(Id, _),
        findall(Op, (
            service_operation(Id, Op, _),
            Op \= description
        ), Ops),
        format_list_items(Ops, '()', OpsStr),
        format(atom(NodeCode), '  ~w [label="{\\<\\<service\\>\\>\\n~w|~w}", style=filled, fillcolor="~w", color="~w"];~n',
            [Id, Id, OpsStr, SvcCol, SvcBorder])
    ), NodeCodes),
    atomic_list_concat(NodeCodes, Nodes).

%% generate_class_node(+Id, +Archetype, +ShowAttrs, +ShowBehavs, -NodeCode) is det
%
%  Generates a UML-style class node.
generate_class_node(Id, Archetype, ShowAttrs, ShowBehavs, NodeCode) :-
    archetype_colour(Archetype, FillCol),
    archetype_border(Archetype, BorderCol),

    % Get stereotype
    archetype_stereotype(Archetype, Stereotype),

    % Get attributes
    (ShowAttrs == true ->
        get_object_attributes(Id, Archetype, AttrList),
        format_list_items(AttrList, '', AttrsStr)
    ;
        AttrsStr = ''
    ),

    % Get behaviours
    (ShowBehavs == true ->
        get_object_behaviours(Id, Archetype, BehavList),
        format_list_items(BehavList, '()', BehavsStr)
    ;
        BehavsStr = ''
    ),

    % Build label
    (ShowAttrs == true, ShowBehavs == true ->
        format(atom(Label), '{~w\\n~w|~w|~w}', [Stereotype, Id, AttrsStr, BehavsStr])
    ; ShowAttrs == true ->
        format(atom(Label), '{~w\\n~w|~w}', [Stereotype, Id, AttrsStr])
    ; ShowBehavs == true ->
        format(atom(Label), '{~w\\n~w|~w}', [Stereotype, Id, BehavsStr])
    ;
        format(atom(Label), '~w\\n~w', [Stereotype, Id])
    ),

    format(atom(NodeCode), '  ~w [label="~w", style=filled, fillcolor="~w", color="~w"];~n',
        [Id, Label, FillCol, BorderCol]).

archetype_stereotype(moment_interval, '\\<\\<moment-interval\\>\\>').
archetype_stereotype(role, '\\<\\<role\\>\\>').
archetype_stereotype(party_place_thing, '\\<\\<entity\\>\\>').
archetype_stereotype(description, '\\<\\<description\\>\\>').
archetype_stereotype(service, '\\<\\<service\\>\\>').

%% get_object_attributes(+Id, +Archetype, -Attributes) is det
get_object_attributes(Id, moment_interval, Attrs) :-
    findall(Name, mi_attribute(Id, Name, _), Attrs).
get_object_attributes(Id, role, Attrs) :-
    findall(Name, role_attribute(Id, Name, _), Attrs).
get_object_attributes(Id, party_place_thing, Attrs) :-
    findall(Name, ppt_attribute(Id, Name, _), Attrs).
get_object_attributes(Id, description, Attrs) :-
    findall(Name, desc_attribute(Id, Name, _), Attrs).

%% get_object_behaviours(+Id, +Archetype, -Behaviours) is det
get_object_behaviours(Id, moment_interval, Behavs) :-
    findall(Name, mi_behaviour(Id, Name, _), Behavs).
get_object_behaviours(Id, role, Behavs) :-
    findall(Name, role_behaviour(Id, Name, _), Behavs).
get_object_behaviours(Id, party_place_thing, Behavs) :-
    findall(Name, ppt_behaviour(Id, Name, _), Behavs).
get_object_behaviours(Id, description, Behavs) :-
    findall(Name, desc_behaviour(Id, Name, _), Behavs).

%% format_list_items(+List, +Suffix, -FormattedStr) is det
format_list_items([], _, '').
format_list_items(List, Suffix, Str) :-
    List \= [],
    maplist(format_item(Suffix), List, Formatted),
    atomic_list_concat(Formatted, '\\l', Joined),
    atom_concat(Joined, '\\l', Str).

format_item(Suffix, Item, Formatted) :-
    format(atom(Formatted), '~w~w', [Item, Suffix]).

%% --------------------------------------------------------------------------
%% Bounded Context Subgraphs
%% --------------------------------------------------------------------------

%% generate_context_subgraphs(+ShowAttrs, +ShowBehavs, -Subgraphs) is det
%
%  Generates DOT subgraphs for each bounded context, grouping all objects.
generate_context_subgraphs(ShowAttrs, ShowBehavs, Subgraphs) :-
    findall(SubgraphCode, (
        bounded_context(ContextId, ContextName, Scope),
        generate_single_context_subgraph(ContextId, ContextName, Scope, ShowAttrs, ShowBehavs, SubgraphCode)
    ), SubgraphCodes),
    atomic_list_concat(SubgraphCodes, Subgraphs).

generate_single_context_subgraph(ContextId, ContextName, Scope, ShowAttrs, ShowBehavs, SubgraphCode) :-
    % Determine context colour based on domain classification
    context_fill_colour(ContextId, FillCol),
    context_border_colour(ContextId, BorderCol),

    % Generate all nodes for this context
    findall(NodeCode, (
        moment_interval(Id, ContextId),
        generate_class_node(Id, moment_interval, ShowAttrs, ShowBehavs, NodeCode)
    ), MINodes),
    findall(NodeCode, (
        role(Id, ContextId),
        generate_class_node(Id, role, ShowAttrs, ShowBehavs, NodeCode)
    ), RoleNodes),
    findall(NodeCode, (
        party_place_thing(Id, ContextId),
        generate_class_node(Id, party_place_thing, ShowAttrs, ShowBehavs, NodeCode)
    ), PPTNodes),
    findall(NodeCode, (
        description(Id, ContextId),
        generate_class_node(Id, description, ShowAttrs, ShowBehavs, NodeCode)
    ), DescNodes),
    findall(NodeCode, (
        domain_service(Id, ContextId),
        generate_service_node(Id, NodeCode)
    ), ServiceNodes),

    atomic_list_concat(MINodes, MINodesStr),
    atomic_list_concat(RoleNodes, RoleNodesStr),
    atomic_list_concat(PPTNodes, PPTNodesStr),
    atomic_list_concat(DescNodes, DescNodesStr),
    atomic_list_concat(ServiceNodes, ServiceNodesStr),

    % Generate aggregates within this context
    findall(AggCode, (
        aggregate(AggId, ContextId),
        generate_inline_aggregate(AggId, AggCode)
    ), AggCodes),
    atomic_list_concat(AggCodes, AggsStr),

    format(atom(SubgraphCode), '
  subgraph cluster_ctx_~w {
    label="~w\\n[~w]";
    fontsize=14;
    fontname="Helvetica-Bold";
    style="rounded,filled";
    color="~w";
    bgcolor="~w";
    penwidth=2;

    // Moment-Intervals
~w
    // Roles
~w
    // Entities
~w
    // Descriptions
~w
    // Services
~w
    // Aggregates
~w  }
', [ContextId, ContextName, Scope, BorderCol, FillCol,
    MINodesStr, RoleNodesStr, PPTNodesStr, DescNodesStr, ServiceNodesStr, AggsStr]).

%% context_fill_colour(+ContextId, -Colour) is det
%  Returns fill colour based on domain classification.
context_fill_colour(ContextId, '#FFF8DC') :-  % Cornsilk (core)
    core_domain(ContextId), !.
context_fill_colour(ContextId, '#F0FFF0') :-  % Honeydew (supporting)
    supporting_domain(ContextId), !.
context_fill_colour(ContextId, '#F5F5F5') :-  % White smoke (generic)
    generic_domain(ContextId), !.
context_fill_colour(_, '#FAFAFA').            % Default light grey

%% context_border_colour(+ContextId, -Colour) is det
context_border_colour(ContextId, '#DAA520') :-  % Goldenrod (core)
    core_domain(ContextId), !.
context_border_colour(ContextId, '#2E8B57') :-  % Sea green (supporting)
    supporting_domain(ContextId), !.
context_border_colour(ContextId, '#708090') :-  % Slate gray (generic)
    generic_domain(ContextId), !.
context_border_colour(_, '#A9A9A9').            % Default dark grey

%% generate_inline_aggregate(+AggId, -Code) is det
%  Generates a note-style representation of aggregate invariants.
generate_inline_aggregate(AggId, Code) :-
    aggregate_root(AggId, RootId),
    findall(Inv, aggregate_invariant(AggId, Inv), Invariants),
    (Invariants \= [] ->
        format_invariants(Invariants, InvStr),
        format(atom(Code), '    ~w_invariants [label="~w invariants:\\l~w", shape=note, style=filled, fillcolor="#FFFACD", fontsize=8];~n    ~w_invariants -> ~w [style=dotted, arrowhead=none];~n',
            [AggId, AggId, InvStr, AggId, RootId])
    ;
        Code = ''
    ).

format_invariants([], '').
format_invariants([H|T], Str) :-
    format_invariants(T, Rest),
    format(atom(Str), '• ~w\\l~w', [H, Rest]).

%% generate_service_node(+Id, -NodeCode) is det
generate_service_node(Id, NodeCode) :-
    archetype_colour(service, SvcCol),
    archetype_border(service, SvcBorder),
    findall(Op, (
        service_operation(Id, Op, _),
        Op \= description
    ), Ops),
    format_list_items(Ops, '()', OpsStr),
    format(atom(NodeCode), '    ~w [label="{\\<\\<service\\>\\>\\n~w|~w}", style=filled, fillcolor="~w", color="~w"];~n',
        [Id, Id, OpsStr, SvcCol, SvcBorder]).

%% --------------------------------------------------------------------------
%% Aggregate Subgraphs (standalone, when contexts not shown)
%% --------------------------------------------------------------------------

generate_aggregate_subgraphs(Subgraphs) :-
    archetype_colour(aggregate, AggCol),
    archetype_border(aggregate, AggBorder),
    findall(SubgraphCode, (
        aggregate(AggId, _),
        aggregate_root(AggId, RootId),
        findall(MemberId, aggregate_member(AggId, MemberId), Members),
        format_aggregate_members([RootId|Members], MemberStr),
        format(atom(SubgraphCode), '
  subgraph cluster_~w {
    label="~w";
    style="dashed,rounded";
    color="~w";
    bgcolor="~w";
~w  }
', [AggId, AggId, AggBorder, AggCol, MemberStr])
    ), SubgraphCodes),
    atomic_list_concat(SubgraphCodes, Subgraphs).

format_aggregate_members(Members, Str) :-
    findall(Line, (
        member(M, Members),
        format(atom(Line), '    ~w;~n', [M])
    ), Lines),
    atomic_list_concat(Lines, Str).

%% --------------------------------------------------------------------------
%% Relationship Generation
%% --------------------------------------------------------------------------

generate_relationships(Relationships) :-
    % Description -> PPT (instanceOf - the PPT is an instance of this type/category)
    findall(Edge, (
        ppt_described_by(PPTId, DescId),
        format(atom(Edge), '  ~w -> ~w [label="«instanceOf»", style=dashed, arrowhead=empty];~n', [PPTId, DescId])
    ), DescEdges),

    % PPT -> Role (implements - the entity implements this role/interface)
    findall(Edge, (
        ppt_plays_role(PPTId, RoleId),
        format(atom(Edge), '  ~w -> ~w [label="«implements»", style=dashed, arrowhead=empty];~n', [PPTId, RoleId])
    ), PlaysEdges),

    % Role -> MI (handles - the role handles/processes this event)
    findall(Edge, (
        role_participates_in(RoleId, MIId),
        format(atom(Edge), '  ~w -> ~w [label="handles", arrowhead=vee];~n', [RoleId, MIId])
    ), ParticipatesEdges),

    % MI -> MI-Detail (composition - the MI contains these details)
    findall(Edge, (
        mi_detail(MIId, DetailId, _),
        format(atom(Edge), '  ~w -> ~w [arrowhead=diamond, style=bold, label="1..*"];~n', [MIId, DetailId])
    ), ContainsEdges),

    % MI -> MI (generates/triggers)
    findall(Edge, (
        mi_plan_actual(PlanId, ActualId, Type),
        plan_actual_label(Type, Label),
        format(atom(Edge), '  ~w -> ~w [label="~w", style=dashed, arrowhead=vee];~n', [PlanId, ActualId, Label])
    ), PlanActualEdges),

    % Service -> Objects (uses - the service uses these objects)
    findall(Edge, (
        service_coordinates(SvcId, ObjId),
        format(atom(Edge), '  ~w -> ~w [label="«uses»", style=dotted, arrowhead=open];~n', [SvcId, ObjId])
    ), ServiceEdges),

    % Context relationships (inter-context dependencies)
    findall(Edge, (
        context_relationship(UpCtx, DownCtx, Pattern, _),
        relationship_style(Pattern, Style, Label),
        % Connect using representative nodes from each context
        find_context_representative(UpCtx, UpNode),
        find_context_representative(DownCtx, DownNode),
        format(atom(Edge), '  ~w -> ~w [label="~w", ~w, ltail=cluster_ctx_~w, lhead=cluster_ctx_~w];~n',
            [UpNode, DownNode, Label, Style, UpCtx, DownCtx])
    ), ContextEdges),

    % Combine all edges
    append([DescEdges, PlaysEdges, ParticipatesEdges, ContainsEdges, PlanActualEdges, ServiceEdges, ContextEdges], AllEdges),
    atomic_list_concat(AllEdges, Relationships).

%% plan_actual_label(+Type, -Label) is det
%  Maps plan/actual relationship types to standard OO terminology.
plan_actual_label(plan_to_actual, 'generates').
plan_actual_label(prior_to_next, 'triggers').
plan_actual_label(generates, 'produces').

%% find_context_representative(+ContextId, -NodeId) is det
%  Finds a representative node from a context for inter-context edges.
find_context_representative(ContextId, NodeId) :-
    (moment_interval(NodeId, ContextId) -> true
    ; role(NodeId, ContextId) -> true
    ; party_place_thing(NodeId, ContextId) -> true
    ; description(NodeId, ContextId) -> true
    ; NodeId = ContextId  % Fallback to context ID itself
    ), !.

%% --------------------------------------------------------------------------
%% Context-Specific Visualization
%% --------------------------------------------------------------------------

%% generate_context_dot(+ContextId, +ModelName, -DotCode) is det
%
%  Generates DOT code for a single bounded context.
generate_context_dot(ContextId, ModelName, DotCode) :-
    bounded_context(ContextId, ContextName, _),
    format(atom(Title), '~w - ~w', [ModelName, ContextName]),

    format(atom(Header), 'digraph "~w" {~n', [Title]),
    format(atom(GraphAttrs), '  rankdir=TB;~n  node [shape=record, fontname="Helvetica", fontsize=10];~n  edge [fontname="Helvetica", fontsize=9];~n  label="~w";~n~n', [Title]),

    % Generate nodes for this context only
    findall(NodeCode, (
        moment_interval(Id, ContextId),
        generate_class_node(Id, moment_interval, true, true, NodeCode)
    ), MINodes),
    findall(NodeCode, (
        role(Id, ContextId),
        generate_class_node(Id, role, true, true, NodeCode)
    ), RoleNodes),
    findall(NodeCode, (
        party_place_thing(Id, ContextId),
        generate_class_node(Id, party_place_thing, true, true, NodeCode)
    ), PPTNodes),
    findall(NodeCode, (
        description(Id, ContextId),
        generate_class_node(Id, description, true, true, NodeCode)
    ), DescNodes),

    atomic_list_concat(MINodes, MINodesStr),
    atomic_list_concat(RoleNodes, RoleNodesStr),
    atomic_list_concat(PPTNodes, PPTNodesStr),
    atomic_list_concat(DescNodes, DescNodesStr),

    generate_relationships(Relationships),

    atomic_list_concat([
        Header,
        GraphAttrs,
        MINodesStr,
        RoleNodesStr,
        PPTNodesStr,
        DescNodesStr,
        Relationships,
        '}\n'
    ], DotCode).

generate_context_dot_file(ContextId, ModelName, FilePath) :-
    generate_context_dot(ContextId, ModelName, DotCode),
    open(FilePath, write, Stream),
    write(Stream, DotCode),
    close(Stream).

%% --------------------------------------------------------------------------
%% Aggregate Visualization
%% --------------------------------------------------------------------------

%% generate_aggregate_dot(+AggregateId, -DotCode) is det
%
%  Generates DOT code for a single aggregate.
generate_aggregate_dot(AggregateId, DotCode) :-
    aggregate(AggregateId, _),
    aggregate_root(AggregateId, RootId),
    findall(M, aggregate_member(AggregateId, M), Members),

    format(atom(Header), 'digraph "~w" {~n', [AggregateId]),
    format(atom(GraphAttrs), '  rankdir=TB;~n  node [shape=record, fontname="Helvetica", fontsize=10];~n  label="Aggregate: ~w";~n~n', [AggregateId]),

    % Generate root node
    archetype_of(RootId, RootArchetype),
    generate_class_node(RootId, RootArchetype, true, true, RootNode),

    % Generate member nodes
    findall(MemberNode, (
        member(MemberId, Members),
        archetype_of(MemberId, MemberArchetype),
        generate_class_node(MemberId, MemberArchetype, true, true, MemberNode)
    ), MemberNodes),
    atomic_list_concat(MemberNodes, MemberNodesStr),

    % Generate invariants
    findall(InvLine, (
        aggregate_invariant(AggregateId, Inv),
        format(atom(InvLine), '  inv_~w [label="~w", shape=note, style=filled, fillcolor="#FFFACD"];~n', [AggregateId, Inv])
    ), InvLines),
    atomic_list_concat(InvLines, InvsStr),

    atomic_list_concat([
        Header,
        GraphAttrs,
        RootNode,
        MemberNodesStr,
        InvsStr,
        '}\n'
    ], DotCode).

%% --------------------------------------------------------------------------
%% Context Map Visualization
%% --------------------------------------------------------------------------

%% generate_context_map_dot(+ModelName, -DotCode) is det
%
%  Generates a DOT diagram showing bounded context relationships.
generate_context_map_dot(ModelName, DotCode) :-
    format(atom(Header), 'digraph "~w Context Map" {~n', [ModelName]),
    format(atom(GraphAttrs), '  rankdir=LR;~n  node [shape=box, style="rounded,filled", fontname="Helvetica", fontsize=12, fillcolor="#E8E8E8"];~n  edge [fontname="Helvetica", fontsize=10];~n  label="~w - Context Map";~n~n', [ModelName]),

    % Generate context nodes
    findall(NodeCode, (
        bounded_context(Id, Name, Scope),
        format(atom(NodeCode), '  ~w [label="~w\\n(~w)"];~n', [Id, Name, Scope])
    ), ContextNodes),
    atomic_list_concat(ContextNodes, ContextNodesStr),

    % Generate relationship edges
    findall(EdgeCode, (
        context_relationship(Up, Down, Pattern, _),
        relationship_style(Pattern, Style, Label),
        format(atom(EdgeCode), '  ~w -> ~w [label="~w", ~w];~n', [Up, Down, Label, Style])
    ), EdgeCodes),
    atomic_list_concat(EdgeCodes, EdgesStr),

    atomic_list_concat([
        Header,
        GraphAttrs,
        ContextNodesStr,
        EdgesStr,
        '}\n'
    ], DotCode).

relationship_style(shared_kernel, 'style=bold, dir=both', 'Shared Kernel').
relationship_style(customer_supplier, 'style=solid', 'Customer/Supplier').
relationship_style(conformist, 'style=dashed', 'Conformist').
relationship_style(anticorruption_layer, 'style=dotted', 'ACL').
relationship_style(separate_ways, 'style=invis', 'Separate Ways').

%% --------------------------------------------------------------------------
%% Rendering Helpers (require Graphviz installed)
%% --------------------------------------------------------------------------

%% render_to_png(+DotFile, +PngFile) is det
%
%  Renders DOT file to PNG using Graphviz.
render_to_png(DotFile, PngFile) :-
    format(atom(Cmd), 'dot -Tpng "~w" -o "~w"', [DotFile, PngFile]),
    shell(Cmd).

%% render_to_svg(+DotFile, +SvgFile) is det
%
%  Renders DOT file to SVG using Graphviz.
render_to_svg(DotFile, SvgFile) :-
    format(atom(Cmd), 'dot -Tsvg "~w" -o "~w"', [DotFile, SvgFile]),
    shell(Cmd).

%% render_to_pdf(+DotFile, +PdfFile) is det
%
%  Renders DOT file to PDF using Graphviz.
render_to_pdf(DotFile, PdfFile) :-
    format(atom(Cmd), 'dot -Tpdf "~w" -o "~w"', [DotFile, PdfFile]),
    shell(Cmd).

%% --------------------------------------------------------------------------
%% Utility Predicates
%% --------------------------------------------------------------------------

option_default(Key, Options, Default, Value) :-
    Term =.. [Key, Value],
    (memberchk(Term, Options) -> true ; Value = Default).
