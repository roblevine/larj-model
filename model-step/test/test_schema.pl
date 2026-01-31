%% test_schema.pl - Tests for DDD Schema Module
%
%  Unit tests for the domain model schema and builder.
%
%  Run with: ?- run_tests.
%
%  @author Generated for larj-model
%  @version 1.0

:- use_module('../src/ddd_schema').
:- use_module('../src/model_builder').

:- begin_tests(bounded_context).

test(define_context_creates_context) :-
    clear_model,
    define_context(test_ctx, 'Test Context', 'Test scope'),
    bounded_context(test_ctx, 'Test Context', 'Test scope').

test(define_context_fails_on_duplicate, [throws(_)]) :-
    clear_model,
    define_context(test_ctx, 'Test Context', 'Test scope'),
    define_context(test_ctx, 'Another', 'Another scope').

test(add_context_term_adds_term) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    add_context_term(test_ctx, widget, 'A test widget'),
    context_term(test_ctx, widget, 'A test widget').

test(link_contexts_creates_relationship) :-
    clear_model,
    define_context(upstream, 'Upstream', 'Up scope'),
    define_context(downstream, 'Downstream', 'Down scope'),
    link_contexts(upstream, downstream, customer_supplier, 'Test relationship'),
    context_relationship(upstream, downstream, customer_supplier, 'Test relationship').

:- end_tests(bounded_context).

:- begin_tests(moment_interval).

test(define_moment_interval_creates_mi) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(test_order, test_ctx, moment, 'Order tracking'),
    moment_interval(test_order, test_ctx),
    mi_temporal_type(test_order, moment).

test(add_mi_detail_creates_detail) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(test_order, test_ctx, moment, 'Order'),
    add_mi_detail(test_order, order_line, [attr(qty, integer, true)], []),
    mi_detail(test_order, order_line, detail([attr(qty, integer, true)], [])).

test(add_mi_status_state_creates_status) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(test_order, test_ctx, moment, 'Order'),
    add_mi_status_state(test_order, pending, [confirmed, cancelled]),
    mi_status(test_order, pending, status([confirmed, cancelled], '')).

test(link_plan_actual_creates_link) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(quote, test_ctx, moment, 'Quote'),
    define_moment_interval(order, test_ctx, moment, 'Order'),
    link_plan_actual(quote, order, plan_to_actual),
    mi_plan_actual(quote, order, plan_to_actual).

:- end_tests(moment_interval).

:- begin_tests(role).

test(define_role_creates_role) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_role(customer_role, test_ctx, 'Sales context', 'Customer role'),
    role(customer_role, test_ctx).

test(link_role_to_player_creates_link) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_party(person, test_ctx, person, 'A person'),
    define_role(customer_role, test_ctx, 'Sales', 'Customer'),
    link_role_to_player(customer_role, person),
    role_played_by(customer_role, person).

test(link_role_to_mi_creates_participation) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_role(customer_role, test_ctx, 'Sales', 'Customer'),
    define_moment_interval(order, test_ctx, moment, 'Order'),
    link_role_to_mi(customer_role, order),
    role_participates_in(customer_role, order).

:- end_tests(role).

:- begin_tests(party_place_thing).

test(define_party_creates_ppt) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_party(customer, test_ctx, person, 'A customer'),
    party_place_thing(customer, test_ctx),
    ppt_subtype(customer, person).

test(define_place_creates_ppt) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_place(warehouse, test_ctx, storage, 'A warehouse'),
    party_place_thing(warehouse, test_ctx),
    ppt_subtype(warehouse, place).

test(define_thing_creates_ppt) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_thing(product, test_ctx, physical_good, 'A product'),
    party_place_thing(product, test_ctx),
    ppt_subtype(product, thing).

:- end_tests(party_place_thing).

:- begin_tests(description).

test(define_description_creates_desc) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_description(product_type, test_ctx, product, 'Product types'),
    description(product_type, test_ctx).

test(add_description_default_creates_default) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_description(product_type, test_ctx, product, 'Product types'),
    add_description_default(product_type, weight_unit, kg),
    desc_default_value(product_type, weight_unit, kg).

:- end_tests(description).

:- begin_tests(aggregate).

test(define_aggregate_creates_aggregate) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(order, test_ctx, moment, 'Order'),
    define_aggregate(order_agg, test_ctx, order),
    aggregate(order_agg, test_ctx),
    aggregate_root(order_agg, order).

test(add_aggregate_member_adds_member) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(order, test_ctx, moment, 'Order'),
    define_role(line_item, test_ctx, 'Order', 'Line item'),
    define_aggregate(order_agg, test_ctx, order),
    add_aggregate_member(order_agg, line_item),
    aggregate_member(order_agg, line_item).

test(add_aggregate_invariant_adds_invariant) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(order, test_ctx, moment, 'Order'),
    define_aggregate(order_agg, test_ctx, order),
    add_aggregate_invariant(order_agg, 'Order must have items'),
    aggregate_invariant(order_agg, 'Order must have items').

:- end_tests(aggregate).

:- begin_tests(service).

test(define_service_creates_service) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_service(order_svc, test_ctx, 'Order processing'),
    domain_service(order_svc, test_ctx).

test(add_service_operation_adds_operation) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_service(order_svc, test_ctx, 'Order processing'),
    add_service_operation(order_svc, place_order, [customer, items], [order_id]),
    service_operation(order_svc, place_order, operation([customer, items], [order_id], '')).

:- end_tests(service).

:- begin_tests(archetype_classification).

test(archetype_of_moment_interval) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(order, test_ctx, moment, 'Order'),
    archetype_of(order, moment_interval).

test(archetype_of_role) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_role(customer_role, test_ctx, 'Sales', 'Customer'),
    archetype_of(customer_role, role).

test(archetype_of_ppt) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_party(customer, test_ctx, person, 'Customer'),
    archetype_of(customer, party_place_thing).

test(archetype_of_description) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_description(product_type, test_ctx, product, 'Types'),
    archetype_of(product_type, description).

:- end_tests(archetype_classification).

:- begin_tests(query_helpers).

test(objects_in_context_returns_all) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(order, test_ctx, moment, 'Order'),
    define_role(customer_role, test_ctx, 'Sales', 'Customer'),
    define_party(customer, test_ctx, person, 'Customer'),
    define_description(product_type, test_ctx, product, 'Types'),
    objects_in_context(test_ctx, Objects),
    length(Objects, 4).

test(moment_intervals_in_context) :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(order, test_ctx, moment, 'Order'),
    define_moment_interval(shipment, test_ctx, interval, 'Shipment'),
    moment_intervals_in_context(test_ctx, MIs),
    length(MIs, 2),
    memberchk(order, MIs),
    memberchk(shipment, MIs).

:- end_tests(query_helpers).

%% run_all_tests is det
%
%  Runs all test suites.
run_all_tests :-
    run_tests.
