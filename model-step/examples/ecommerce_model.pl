%% ecommerce_model.pl - Example E-Commerce Domain Model
%
%  Demonstrates usage of the DDD schema and model builder
%  with a simple e-commerce domain.
%
%  @author Generated for larj-model
%  @version 1.0

:- use_module('../src/ddd_schema').
:- use_module('../src/model_builder').
:- use_module('../src/model_export').

%% build_ecommerce_model is det
%
%  Builds the complete e-commerce domain model.
build_ecommerce_model :-
    clear_model,
    build_contexts,
    build_context_relationships,
    build_order_context_objects,
    build_inventory_context_objects,
    build_customer_context_objects,
    build_aggregates,
    build_services,
    classify_domains.

%% --------------------------------------------------------------------------
%% Bounded Contexts
%% --------------------------------------------------------------------------

build_contexts :-
    % Order Management Context - Core Domain
    define_context(
        order_ctx,
        'Order Management',
        'Handles order creation, processing, and fulfillment'
    ),
    add_context_term(order_ctx, order, 'A customer request to purchase products'),
    add_context_term(order_ctx, order_line, 'A single product and quantity within an order'),
    add_context_term(order_ctx, fulfillment, 'The process of preparing and shipping an order'),

    % Inventory Context - Supporting Domain
    define_context(
        inventory_ctx,
        'Inventory Management',
        'Tracks product stock levels and availability'
    ),
    add_context_term(inventory_ctx, stock, 'Quantity of a product available for sale'),
    add_context_term(inventory_ctx, reservation, 'Stock held for a pending order'),

    % Customer Context - Supporting Domain
    define_context(
        customer_ctx,
        'Customer Management',
        'Manages customer information and relationships'
    ),
    add_context_term(customer_ctx, customer, 'A person or organisation that purchases products'),
    add_context_term(customer_ctx, account, 'A customer access and billing record').

%% --------------------------------------------------------------------------
%% Context Relationships
%% --------------------------------------------------------------------------

build_context_relationships :-
    % Order context is upstream to Inventory (reserves stock)
    link_contexts(
        order_ctx,
        inventory_ctx,
        customer_supplier,
        'Orders reserve inventory; inventory confirms availability'
    ),
    % Customer context supplies customer data to Order context
    link_contexts(
        customer_ctx,
        order_ctx,
        customer_supplier,
        'Customer data used for order placement and shipping'
    ).

%% --------------------------------------------------------------------------
%% Order Context Objects
%% --------------------------------------------------------------------------

build_order_context_objects :-
    % Moment-Interval: Order
    define_moment_interval(
        order,
        order_ctx,
        moment,
        'Business transaction tracking and legal compliance'
    ),
    add_attribute(order, order_number, string, true),
    add_attribute(order, order_date, datetime, true),
    add_attribute(order, total_amount, decimal, true),
    add_attribute(order, shipping_address, address, true),
    add_behaviour(order, place_order, [customer_id, items], [order_id]),
    add_behaviour(order, add_line, [product_id, quantity], [order_line_id]),
    add_behaviour(order, calculate_total, [], [total]),
    add_behaviour(order, complete, [], [completed_order]),
    add_behaviour(order, cancel, [reason], [cancelled_order]),

    % Status states for Order
    add_mi_status_state(order, pending, [confirmed, cancelled]),
    add_mi_status_state(order, confirmed, [processing, cancelled]),
    add_mi_status_state(order, processing, [shipped, cancelled]),
    add_mi_status_state(order, shipped, [delivered]),
    add_mi_status_state(order, delivered, []),
    add_mi_status_state(order, cancelled, []),

    % MI-Detail: OrderLine
    add_mi_detail(
        order,
        order_line,
        [attr(product_id, string, true),
         attr(quantity, integer, true),
         attr(unit_price, decimal, true),
         attr(line_total, decimal, true)],
        [beh(calculate_line_total, [], [total])]
    ),

    % Moment-Interval: Shipment
    define_moment_interval(
        shipment,
        order_ctx,
        interval,
        'Track delivery from dispatch to receipt'
    ),
    add_attribute(shipment, shipment_number, string, true),
    add_attribute(shipment, dispatched_at, datetime, true),
    add_attribute(shipment, delivered_at, datetime, false),
    add_attribute(shipment, carrier, string, true),
    add_attribute(shipment, tracking_number, string, false),
    add_behaviour(shipment, dispatch, [order_id, carrier], [shipment_id]),
    add_behaviour(shipment, update_tracking, [tracking_info], []),
    add_behaviour(shipment, mark_delivered, [], [delivered_shipment]),

    % Link Order to Shipment (generates relationship)
    link_plan_actual(order, shipment, generates),

    % Role: Purchaser (customer in order context)
    define_role(
        purchaser,
        order_ctx,
        'Customer placing an order',
        'The party responsible for the order and payment'
    ),
    add_attribute(purchaser, customer_reference, string, true),
    add_behaviour(purchaser, get_order_history, [], [orders]),
    add_behaviour(purchaser, calculate_lifetime_value, [], [value]),
    link_role_to_mi(purchaser, order).

%% --------------------------------------------------------------------------
%% Inventory Context Objects
%% --------------------------------------------------------------------------

build_inventory_context_objects :-
    % Moment-Interval: StockReservation
    define_moment_interval(
        stock_reservation,
        inventory_ctx,
        interval,
        'Track stock held for pending orders'
    ),
    add_attribute(stock_reservation, reservation_id, string, true),
    add_attribute(stock_reservation, reserved_at, datetime, true),
    add_attribute(stock_reservation, expires_at, datetime, true),
    add_attribute(stock_reservation, quantity, integer, true),
    add_behaviour(stock_reservation, reserve, [product_id, qty, order_id], [reservation_id]),
    add_behaviour(stock_reservation, release, [], []),
    add_behaviour(stock_reservation, confirm, [], [confirmed_reservation]),

    add_mi_status_state(stock_reservation, pending, [confirmed, released, expired]),
    add_mi_status_state(stock_reservation, confirmed, []),
    add_mi_status_state(stock_reservation, released, []),
    add_mi_status_state(stock_reservation, expired, []),

    % Thing: Product (in inventory context)
    define_thing(
        product,
        inventory_ctx,
        physical_good,
        'A sellable item tracked in inventory'
    ),
    add_attribute(product, sku, string, true),
    add_attribute(product, name, string, true),
    add_attribute(product, current_stock, integer, true),
    add_attribute(product, reorder_level, integer, true),
    add_behaviour(product, adjust_stock, [quantity, reason], [new_level]),
    add_behaviour(product, check_availability, [quantity], [available]),

    % Description: ProductCategory
    define_description(
        product_category,
        inventory_ctx,
        product,
        'Product classification for inventory management'
    ),
    add_attribute(product_category, category_code, string, true),
    add_attribute(product_category, category_name, string, true),
    add_description_default(product_category, reorder_level, 10),
    add_description_default(product_category, lead_time_days, 7),
    add_behaviour(product_category, find_available_products, [], [products]),
    add_behaviour(product_category, calculate_total_stock, [], [total]),

    % Link Product to ProductCategory
    link_ppt_to_description(product, product_category),

    % Role: StockItem (product as tracked inventory)
    define_role(
        stock_item,
        inventory_ctx,
        'Product as inventory item',
        'Product viewed for stock management purposes'
    ),
    add_attribute(stock_item, location, string, true),
    add_behaviour(stock_item, get_stock_movements, [], [movements]),
    link_role_to_player(stock_item, product),
    link_role_to_mi(stock_item, stock_reservation).

%% --------------------------------------------------------------------------
%% Customer Context Objects
%% --------------------------------------------------------------------------

build_customer_context_objects :-
    % Party: Customer (person or organisation)
    define_party(
        customer,
        customer_ctx,
        party,
        'A buyer of products'
    ),
    add_attribute(customer, customer_id, string, true),
    add_attribute(customer, name, string, true),
    add_attribute(customer, email, string, true),
    add_attribute(customer, phone, string, false),
    add_behaviour(customer, update_contact_info, [email, phone], []),
    add_behaviour(customer, get_addresses, [], [addresses]),

    % Description: CustomerTier
    define_description(
        customer_tier,
        customer_ctx,
        customer,
        'Customer classification for pricing and service levels'
    ),
    add_attribute(customer_tier, tier_code, string, true),
    add_attribute(customer_tier, tier_name, string, true),
    add_description_default(customer_tier, discount_percent, 0),
    add_description_default(customer_tier, free_shipping_threshold, 100),
    add_behaviour(customer_tier, get_eligible_customers, [], [customers]),

    link_ppt_to_description(customer, customer_tier),

    % Role: AccountHolder
    define_role(
        account_holder,
        customer_ctx,
        'Customer with active account',
        'Customer who has registered and can place orders'
    ),
    add_attribute(account_holder, account_number, string, true),
    add_attribute(account_holder, registered_at, datetime, true),
    add_attribute(account_holder, last_login, datetime, false),
    add_behaviour(account_holder, authenticate, [credentials], [session]),
    add_behaviour(account_holder, get_saved_addresses, [], [addresses]),

    link_role_to_player(account_holder, customer),

    % Link AccountHolder to Purchaser (cross-context role mapping)
    % In a real system, this would use an anti-corruption layer
    link_ppt_to_role(customer, purchaser).

%% --------------------------------------------------------------------------
%% Aggregates
%% --------------------------------------------------------------------------

build_aggregates :-
    % Order Aggregate: Order + OrderLines
    define_aggregate(order_aggregate, order_ctx, order),
    add_aggregate_member(order_aggregate, order_line),
    add_aggregate_invariant(order_aggregate, 'Order total must equal sum of line totals'),
    add_aggregate_invariant(order_aggregate, 'Order must have at least one line item'),

    % Shipment Aggregate
    define_aggregate(shipment_aggregate, order_ctx, shipment),
    add_aggregate_invariant(shipment_aggregate, 'Shipment must reference valid order'),

    % Product Aggregate
    define_aggregate(product_aggregate, inventory_ctx, product),
    add_aggregate_member(product_aggregate, stock_item),
    add_aggregate_invariant(product_aggregate, 'Stock level cannot be negative'),

    % Reservation Aggregate
    define_aggregate(reservation_aggregate, inventory_ctx, stock_reservation),
    add_aggregate_invariant(reservation_aggregate, 'Reserved quantity cannot exceed available stock'),

    % Customer Aggregate
    define_aggregate(customer_aggregate, customer_ctx, customer),
    add_aggregate_member(customer_aggregate, account_holder),
    add_aggregate_invariant(customer_aggregate, 'Customer must have valid email').

%% --------------------------------------------------------------------------
%% Domain Services
%% --------------------------------------------------------------------------

build_services :-
    % Order Processing Service
    define_service(
        order_processing_service,
        order_ctx,
        'Orchestrates order placement and fulfillment'
    ),
    add_service_operation(
        order_processing_service,
        place_order,
        [customer_id, cart_items, shipping_address],
        [order_id, confirmation]
    ),
    add_service_operation(
        order_processing_service,
        fulfill_order,
        [order_id],
        [shipment_id]
    ),
    link_service_to_object(order_processing_service, order),
    link_service_to_object(order_processing_service, shipment),

    % Inventory Allocation Service
    define_service(
        inventory_allocation_service,
        inventory_ctx,
        'Manages stock reservations for orders'
    ),
    add_service_operation(
        inventory_allocation_service,
        allocate_stock,
        [order_id, items],
        [reservations]
    ),
    add_service_operation(
        inventory_allocation_service,
        release_allocation,
        [reservation_id],
        []
    ),
    link_service_to_object(inventory_allocation_service, stock_reservation),
    link_service_to_object(inventory_allocation_service, product).

%% --------------------------------------------------------------------------
%% Domain Classification
%% --------------------------------------------------------------------------

classify_domains :-
    % Order Management is Core Domain
    mark_core_domain(order_ctx),
    mark_core_domain(order),
    mark_core_domain(shipment),

    % Inventory is Supporting Domain
    mark_supporting_domain(inventory_ctx),
    mark_supporting_domain(product),
    mark_supporting_domain(stock_reservation),

    % Customer Management is Supporting Domain
    mark_supporting_domain(customer_ctx),
    mark_supporting_domain(customer),

    % Descriptions are Generic Domain
    mark_generic_domain(product_category),
    mark_generic_domain(customer_tier).

%% --------------------------------------------------------------------------
%% Example Queries
%% --------------------------------------------------------------------------

%% example_queries is det
%
%  Demonstrates querying the built model.
example_queries :-
    writeln('=== E-Commerce Domain Model Queries ==='),
    nl,

    % List all bounded contexts
    writeln('Bounded Contexts:'),
    forall(bounded_context(Id, Name, _),
        format('  - ~w: ~w~n', [Id, Name])),
    nl,

    % List moment-intervals in order context
    writeln('Moment-Intervals in Order Context:'),
    forall(moment_interval(Id, order_ctx),
        format('  - ~w~n', [Id])),
    nl,

    % List roles and their players
    writeln('Roles and Players:'),
    forall((role(RoleId, _), role_played_by(RoleId, PlayerId)),
        format('  - ~w played by ~w~n', [RoleId, PlayerId])),
    nl,

    % Show aggregate structure
    writeln('Aggregates:'),
    forall(aggregate(AggId, ContextId), (
        aggregate_root(AggId, RootId),
        format('  - ~w (root: ~w) in ~w~n', [AggId, RootId, ContextId]),
        forall(aggregate_member(AggId, MemberId),
            format('      member: ~w~n', [MemberId]))
    )),
    nl,

    % Show core domain elements
    writeln('Core Domain:'),
    forall(core_domain(Id),
        format('  - ~w~n', [Id])),
    nl.

%% run_example is det
%
%  Builds the model and runs example queries.
run_example :-
    build_ecommerce_model,
    example_queries.

%% export_example(+FilePath) is det
%
%  Builds the model and exports it to a file.
export_example(FilePath) :-
    build_ecommerce_model,
    export_model_to_file(ecommerce, FilePath).

%% export_workflowy_example(-Text) is det
%
%  Builds the model and exports it in Workflowy format.
export_workflowy_example(Text) :-
    build_ecommerce_model,
    export_to_workflowy(ecommerce, Text).
