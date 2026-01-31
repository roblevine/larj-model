%% test_requirements.pl - SDD Test-to-Requirement Traceability
%
%  Maps test cases to SPEC.md acceptance criteria.
%  Run with: ?- run_tests.
%
%  @author Generated for larj-model SDD
%  @version 1.0

:- use_module('../src/ddd_schema').
:- use_module('../src/model_builder').
:- use_module('../src/model_validate').
:- use_module('../src/model_visualize').
:- use_module('../src/model_export').

%% --------------------------------------------------------------------------
%% REQ-001: Domain Input Processing
%% --------------------------------------------------------------------------

:- begin_tests(req_001_domain_input).

% SPEC: AC-001-02
test(ac_001_02_clear_model_retracts_all) :-
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(order, test_ctx, moment, 'Order'),
    clear_model,
    \+ bounded_context(test_ctx, _, _),
    \+ moment_interval(order, _).

% SPEC: AC-001-04
test(ac_001_04_duplicate_context_throws, [throws(_)]) :-
    clear_model,
    define_context(dup_ctx, 'First', 'Scope 1'),
    define_context(dup_ctx, 'Second', 'Scope 2').

:- end_tests(req_001_domain_input).

%% --------------------------------------------------------------------------
%% REQ-002: Bounded Context Management
%% --------------------------------------------------------------------------

:- begin_tests(req_002_bounded_context).

% SPEC: AC-002-01
test(ac_002_01_define_context) :-
    clear_model,
    define_context(sales_ctx, 'Sales Context', 'Handles sales operations'),
    bounded_context(sales_ctx, 'Sales Context', 'Handles sales operations').

% SPEC: AC-002-02
test(ac_002_02_add_context_term) :-
    clear_model,
    define_context(sales_ctx, 'Sales', 'Scope'),
    add_context_term(sales_ctx, opportunity, 'A potential sale'),
    context_term(sales_ctx, opportunity, 'A potential sale').

% SPEC: AC-002-03
test(ac_002_03_link_contexts_customer_supplier) :-
    clear_model,
    define_context(order_ctx, 'Orders', 'Order management'),
    define_context(shipping_ctx, 'Shipping', 'Shipping management'),
    link_contexts(order_ctx, shipping_ctx, customer_supplier, 'Orders drive shipping'),
    context_relationship(order_ctx, shipping_ctx, customer_supplier, _).

% SPEC: AC-002-03 (all patterns)
test(ac_002_03_link_contexts_all_patterns) :-
    clear_model,
    define_context(ctx_a, 'A', 'A'),
    define_context(ctx_b, 'B', 'B'),
    define_context(ctx_c, 'C', 'C'),
    define_context(ctx_d, 'D', 'D'),
    define_context(ctx_e, 'E', 'E'),
    define_context(ctx_f, 'F', 'F'),
    link_contexts(ctx_a, ctx_b, shared_kernel, ''),
    link_contexts(ctx_b, ctx_c, customer_supplier, ''),
    link_contexts(ctx_c, ctx_d, conformist, ''),
    link_contexts(ctx_d, ctx_e, anticorruption_layer, ''),
    link_contexts(ctx_e, ctx_f, separate_ways, ''),
    context_relationship(ctx_a, ctx_b, shared_kernel, _),
    context_relationship(ctx_b, ctx_c, customer_supplier, _),
    context_relationship(ctx_c, ctx_d, conformist, _),
    context_relationship(ctx_d, ctx_e, anticorruption_layer, _),
    context_relationship(ctx_e, ctx_f, separate_ways, _).

% SPEC: AC-002-04
test(ac_002_04_query_contexts) :-
    clear_model,
    define_context(ctx_1, 'Context 1', 'Scope 1'),
    define_context(ctx_2, 'Context 2', 'Scope 2'),
    findall(Id, bounded_context(Id, _, _), Ids),
    length(Ids, 2),
    memberchk(ctx_1, Ids),
    memberchk(ctx_2, Ids).

:- end_tests(req_002_bounded_context).

%% --------------------------------------------------------------------------
%% REQ-003: Four-Archetype Classification System
%% --------------------------------------------------------------------------

:- begin_tests(req_003_archetypes).

% SPEC: AC-003-01
test(ac_003_01_define_moment_interval_moment) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(sale, ctx, moment, 'Track sales'),
    moment_interval(sale, ctx),
    mi_temporal_type(sale, moment).

test(ac_003_01_define_moment_interval_interval) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(rental, ctx, interval, 'Track rentals'),
    moment_interval(rental, ctx),
    mi_temporal_type(rental, interval).

% SPEC: AC-003-02
test(ac_003_02_define_role) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_role(buyer, ctx, 'Purchase context', 'The party making the purchase'),
    role(buyer, ctx).

% SPEC: AC-003-03
test(ac_003_03_define_party) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_party(customer, ctx, person, 'An individual customer'),
    party_place_thing(customer, ctx),
    ppt_subtype(customer, person).

test(ac_003_03_define_organisation) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_party(corp, ctx, organisation, 'A corporate customer'),
    party_place_thing(corp, ctx),
    ppt_subtype(corp, organisation).

test(ac_003_03_define_place) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_place(warehouse, ctx, storage, 'Storage facility'),
    party_place_thing(warehouse, ctx),
    ppt_subtype(warehouse, place).

test(ac_003_03_define_thing) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_thing(product, ctx, physical_good, 'A product'),
    party_place_thing(product, ctx),
    ppt_subtype(product, thing).

% SPEC: AC-003-04
test(ac_003_04_define_description) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_description(product_category, ctx, product, 'Product catalog entry'),
    description(product_category, ctx),
    desc_applies_to(product_category, product).

% SPEC: AC-003-05
test(ac_003_05_archetype_inference) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Orders'),
    define_role(purchaser, ctx, 'Sales', 'Buyer'),
    define_party(customer, ctx, person, 'Customer'),
    define_description(tier, ctx, customer, 'Tier'),
    archetype_of(order, moment_interval),
    archetype_of(purchaser, role),
    archetype_of(customer, party_place_thing),
    archetype_of(tier, description).

:- end_tests(req_003_archetypes).

%% --------------------------------------------------------------------------
%% REQ-004: Archetype Linking Pattern
%% --------------------------------------------------------------------------

:- begin_tests(req_004_linking).

% SPEC: AC-004-01
test(ac_004_01_link_description_to_ppt) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_thing(product, ctx, goods, 'A product'),
    define_description(category, ctx, product, 'Category'),
    link_ppt_to_description(product, category),
    ppt_described_by(product, category).

% SPEC: AC-004-02
test(ac_004_02_link_ppt_to_role_bidirectional) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_party(person, ctx, person, 'A person'),
    define_role(employee, ctx, 'Work', 'Employee role'),
    link_role_to_player(employee, person),
    role_played_by(employee, person),
    ppt_plays_role(person, employee).

% SPEC: AC-004-03
test(ac_004_03_link_role_to_mi) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_role(attendee, ctx, 'Event', 'Event attendee'),
    define_moment_interval(meeting, ctx, interval, 'Meeting'),
    link_role_to_mi(attendee, meeting),
    role_participates_in(attendee, meeting).

% SPEC: AC-004-04
test(ac_004_04_add_mi_detail) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(invoice, ctx, moment, 'Invoice'),
    add_mi_detail(invoice, line_item, [attr(amount, decimal, true)], [calc_tax]),
    mi_detail(invoice, line_item, _).

% SPEC: AC-004-05
test(ac_004_05_link_plan_to_actual) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(quote, ctx, moment, 'Quote'),
    define_moment_interval(order, ctx, moment, 'Order'),
    link_plan_actual(quote, order, plan_to_actual),
    mi_plan_actual(quote, order, plan_to_actual).

:- end_tests(req_004_linking).

%% --------------------------------------------------------------------------
%% REQ-005: Aggregate Boundaries
%% --------------------------------------------------------------------------

:- begin_tests(req_005_aggregates).

% SPEC: AC-005-01
test(ac_005_01_define_aggregate_with_root) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    define_aggregate(order_agg, ctx, order),
    aggregate(order_agg, ctx),
    aggregate_root(order_agg, order).

% SPEC: AC-005-02
test(ac_005_02_add_aggregate_member) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    define_moment_interval(line, ctx, moment, 'Line'),
    define_aggregate(order_agg, ctx, order),
    add_aggregate_member(order_agg, line),
    aggregate_member(order_agg, line).

% SPEC: AC-005-03
test(ac_005_03_add_aggregate_invariant) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    define_aggregate(order_agg, ctx, order),
    add_aggregate_invariant(order_agg, 'Order must have at least one line'),
    aggregate_invariant(order_agg, 'Order must have at least one line').

% SPEC: AC-005-04
test(ac_005_04_query_aggregates_in_context) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    define_moment_interval(payment, ctx, moment, 'Payment'),
    define_aggregate(order_agg, ctx, order),
    define_aggregate(payment_agg, ctx, payment),
    aggregates_in_context(ctx, Aggs),
    length(Aggs, 2),
    memberchk(order_agg, Aggs),
    memberchk(payment_agg, Aggs).

:- end_tests(req_005_aggregates).

%% --------------------------------------------------------------------------
%% REQ-006: Domain Services
%% --------------------------------------------------------------------------

:- begin_tests(req_006_services).

% SPEC: AC-006-01
test(ac_006_01_define_domain_service) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_service(order_svc, ctx, 'Handles order operations'),
    domain_service(order_svc, ctx).

% SPEC: AC-006-02
test(ac_006_02_add_service_operation) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_service(order_svc, ctx, 'Order service'),
    add_service_operation(order_svc, place_order, [customer_id, items], [order_id]),
    service_operation(order_svc, place_order, operation([customer_id, items], [order_id], _)).

% SPEC: AC-006-03
test(ac_006_03_link_service_to_objects) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    define_service(order_svc, ctx, 'Order service'),
    link_service_to_object(order_svc, order),
    service_coordinates(order_svc, order).

:- end_tests(req_006_services).

%% --------------------------------------------------------------------------
%% REQ-007: Domain Classification
%% --------------------------------------------------------------------------

:- begin_tests(req_007_classification).

% SPEC: AC-007-01
test(ac_007_01_mark_core_domain) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    mark_core_domain(ctx),
    core_domain(ctx).

% SPEC: AC-007-02
test(ac_007_02_mark_supporting_domain) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    mark_supporting_domain(ctx),
    supporting_domain(ctx).

% SPEC: AC-007-03
test(ac_007_03_mark_generic_domain) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    mark_generic_domain(ctx),
    generic_domain(ctx).

% SPEC: AC-007-04
test(ac_007_04_mutual_exclusivity) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    % Start as core
    mark_core_domain(ctx),
    core_domain(ctx),
    \+ supporting_domain(ctx),
    \+ generic_domain(ctx),
    % Change to supporting - should remove core
    mark_supporting_domain(ctx),
    supporting_domain(ctx),
    \+ core_domain(ctx),
    \+ generic_domain(ctx),
    % Change to generic - should remove supporting
    mark_generic_domain(ctx),
    generic_domain(ctx),
    \+ core_domain(ctx),
    \+ supporting_domain(ctx),
    % Change back to core - should remove generic
    mark_core_domain(ctx),
    core_domain(ctx),
    \+ supporting_domain(ctx),
    \+ generic_domain(ctx).

:- end_tests(req_007_classification).

%% --------------------------------------------------------------------------
%% REQ-009: Graphviz Visualization
%% --------------------------------------------------------------------------

:- begin_tests(req_009_visualization).

% SPEC: AC-009-01
test(ac_009_01_generate_dot_produces_valid_output) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    generate_dot(test_model, DotCode),
    atom(DotCode),
    sub_atom(DotCode, _, _, _, 'digraph'),
    sub_atom(DotCode, _, _, _, 'order').

% SPEC: AC-009-02
test(ac_009_02_archetype_colors_present) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    define_role(buyer, ctx, 'Sales', 'Buyer'),
    define_party(customer, ctx, person, 'Customer'),
    define_description(tier, ctx, customer, 'Tier'),
    generate_dot(test_model, DotCode),
    sub_atom(DotCode, _, _, _, '#FFB6C1'),  % Pink for MI
    sub_atom(DotCode, _, _, _, '#FFFF99'),  % Yellow for Role
    sub_atom(DotCode, _, _, _, '#90EE90'),  % Green for PPT
    sub_atom(DotCode, _, _, _, '#ADD8E6').  % Blue for Description

% SPEC: AC-009-06
test(ac_009_06_legend_generated) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    generate_dot(test_model, DotCode),
    sub_atom(DotCode, _, _, _, 'cluster_legend'),
    sub_atom(DotCode, _, _, _, 'Archetype Legend').

% SPEC: AC-009-09
test(ac_009_09_context_map_generated) :-
    clear_model,
    define_context(order_ctx, 'Orders', 'Order scope'),
    define_context(shipping_ctx, 'Shipping', 'Ship scope'),
    link_contexts(order_ctx, shipping_ctx, customer_supplier, 'Orders trigger shipping'),
    generate_context_map_dot(test_model, DotCode),
    atom(DotCode),
    sub_atom(DotCode, _, _, _, 'order_ctx'),
    sub_atom(DotCode, _, _, _, 'shipping_ctx'),
    sub_atom(DotCode, _, _, _, 'Customer/Supplier').

:- end_tests(req_009_visualization).

%% --------------------------------------------------------------------------
%% REQ-011: Attributes and Behaviours
%% --------------------------------------------------------------------------

:- begin_tests(req_011_attributes).

% SPEC: AC-011-03
test(ac_011_03_mi_status_states) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    add_mi_status_state(order, pending, [confirmed, cancelled]),
    add_mi_status_state(order, confirmed, [shipped]),
    add_mi_status_state(order, shipped, [delivered]),
    mi_status(order, pending, _),
    mi_status(order, confirmed, _),
    mi_status(order, shipped, _).

:- end_tests(req_011_attributes).

%% --------------------------------------------------------------------------
%% REQ-012: Query Interface
%% --------------------------------------------------------------------------

:- begin_tests(req_012_queries).

% SPEC: AC-012-01
test(ac_012_01_objects_in_context) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    define_role(buyer, ctx, 'Sales', 'Buyer'),
    define_party(customer, ctx, person, 'Customer'),
    define_description(tier, ctx, customer, 'Tier'),
    objects_in_context(ctx, Objects),
    length(Objects, 4),
    memberchk(obj(order, moment_interval), Objects),
    memberchk(obj(buyer, role), Objects),
    memberchk(obj(customer, party_place_thing), Objects),
    memberchk(obj(tier, description), Objects).

% SPEC: AC-012-02
test(ac_012_02_query_by_archetype) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    define_moment_interval(shipment, ctx, interval, 'Shipment'),
    define_role(buyer, ctx, 'Sales', 'Buyer'),
    define_party(customer, ctx, person, 'Customer'),
    define_description(tier, ctx, customer, 'Tier'),
    moment_intervals_in_context(ctx, MIs),
    roles_in_context(ctx, Roles),
    ppts_in_context(ctx, PPTs),
    descriptions_in_context(ctx, Descs),
    length(MIs, 2),
    length(Roles, 1),
    length(PPTs, 1),
    length(Descs, 1).

% SPEC: AC-012-03
test(ac_012_03_query_aggregates_and_services) :-
    clear_model,
    define_context(ctx, 'Test', 'Scope'),
    define_moment_interval(order, ctx, moment, 'Order'),
    define_aggregate(order_agg, ctx, order),
    define_service(order_svc, ctx, 'Order service'),
    aggregates_in_context(ctx, Aggs),
    services_in_context(ctx, Svcs),
    length(Aggs, 1),
    length(Svcs, 1),
    memberchk(order_agg, Aggs),
    memberchk(order_svc, Svcs).

:- end_tests(req_012_queries).

%% --------------------------------------------------------------------------
%% Full Model Integration Test
%% --------------------------------------------------------------------------

:- begin_tests(integration).

test(full_model_workflow) :-
    clear_model,

    % Define contexts
    define_context(sales_ctx, 'Sales', 'Sales operations'),
    define_context(inventory_ctx, 'Inventory', 'Stock management'),
    add_context_term(sales_ctx, opportunity, 'Potential sale'),
    link_contexts(sales_ctx, inventory_ctx, customer_supplier, 'Sales reserves stock'),

    % Define archetypes in sales context
    define_moment_interval(order, sales_ctx, moment, 'Customer order'),
    add_mi_detail(order, order_line, [attr(qty, integer, true), attr(price, decimal, true)], []),
    add_mi_status_state(order, pending, [confirmed, cancelled]),

    define_role(purchaser, sales_ctx, 'Buying context', 'Party placing the order'),
    define_party(customer, sales_ctx, person, 'Individual customer'),
    define_description(customer_tier, sales_ctx, customer, 'Customer classification'),
    add_description_default(customer_tier, discount_percent, 0),

    % Link archetypes
    link_ppt_to_description(customer, customer_tier),
    link_role_to_player(purchaser, customer),
    link_role_to_mi(purchaser, order),

    % Define aggregate
    define_aggregate(order_agg, sales_ctx, order),
    add_aggregate_member(order_agg, order_line),
    add_aggregate_invariant(order_agg, 'Order total must equal sum of lines'),

    % Define service
    define_service(order_svc, sales_ctx, 'Order processing'),
    add_service_operation(order_svc, place_order, [customer_id, items], [order_id]),
    link_service_to_object(order_svc, order),

    % Classify domains
    mark_core_domain(sales_ctx),
    mark_supporting_domain(inventory_ctx),

    % Verify model
    bounded_context(sales_ctx, _, _),
    bounded_context(inventory_ctx, _, _),
    context_relationship(sales_ctx, inventory_ctx, customer_supplier, _),
    moment_interval(order, sales_ctx),
    mi_detail(order, order_line, _),
    role(purchaser, sales_ctx),
    party_place_thing(customer, sales_ctx),
    description(customer_tier, sales_ctx),
    ppt_described_by(customer, customer_tier),
    role_played_by(purchaser, customer),
    role_participates_in(purchaser, order),
    aggregate(order_agg, sales_ctx),
    aggregate_root(order_agg, order),
    aggregate_member(order_agg, order_line),
    domain_service(order_svc, sales_ctx),
    core_domain(sales_ctx),
    supporting_domain(inventory_ctx),

    % Generate visualization
    generate_dot(integration_test, DotCode),
    atom(DotCode),
    sub_atom(DotCode, _, _, _, 'order'),
    sub_atom(DotCode, _, _, _, 'purchaser'),
    sub_atom(DotCode, _, _, _, 'customer').

:- end_tests(integration).

%% --------------------------------------------------------------------------
%% Test Runner
%% --------------------------------------------------------------------------

%% run_all_tests is det
%
%  Runs all requirement-mapped test suites.
run_all_tests :-
    format('~n=== Running SDD Requirement Tests ===~n~n', []),
    run_tests.
