:- use_module('../src/model_builder').

build_model :-
    clear_model,

    % Bounded Context
    define_context(library_ctx, 'Library Management', 'Handles book lending operations'),

    % Moment-Intervals (Pink) - temporal business events
    define_moment_interval(loan, library_ctx, interval, 'Track book borrowing period'),
    add_mi_status_state(loan, active, [returned, overdue]),
    add_mi_status_state(loan, overdue, [returned]),
    add_mi_status_state(loan, returned, []),

    % Roles (Yellow) - how parties participate
    define_role(borrower, library_ctx, 'Lending context', 'Member borrowing a book'),

    % Party/Place/Thing (Green) - entities with identity
    define_party(member, library_ctx, person, 'Library member'),
    define_thing(book, library_ctx, physical_item, 'Physical book in the library'),

    % Descriptions (Blue) - catalog/template entries
    define_description(genre, library_ctx, book, 'Book genre classification'),
    define_description(membership_tier, library_ctx, member, 'Member tier with benefits'),
    add_description_default(membership_tier, loan_limit, 5),

    % Links (Blue -> Green -> Yellow -> Pink)
    link_ppt_to_description(book, genre),
    link_ppt_to_description(member, membership_tier),
    link_role_to_player(borrower, member),
    link_role_to_mi(borrower, loan),

    % Aggregates
    define_aggregate(loan_aggregate, library_ctx, loan),
    add_aggregate_invariant(loan_aggregate, 'Loan must have a book and borrower'),

    % Domain classification
    mark_core_domain(library_ctx),
    mark_core_domain(loan).
