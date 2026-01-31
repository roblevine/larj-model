:- use_module('../src/ddd_schema').
:- use_module('../src/model_builder').

build_model :-
    clear_model,

    % Bounded Contexts
    define_context(index_design, 'Index Design', 'Core index methodology and calculation'),
    define_context(governance, 'Index Governance', 'Oversight and compliance'),
    define_context(distribution, 'Index Distribution', 'Licensing and data distribution'),

    % Moment-Intervals (Pink) - Index Design Context
    define_moment_interval(rebalancing, index_design, interval, 'Periodic index reconstitution'),
    add_mi_status_state(rebalancing, scheduled, [in_review, announced, effective, completed]),
    add_mi_status_state(rebalancing, in_review, [announced]),
    add_mi_status_state(rebalancing, announced, [effective]),
    add_mi_status_state(rebalancing, effective, [completed]),
    add_attribute(rebalancing, review_date, date, 'T-10 business days'),
    add_attribute(rebalancing, announcement_date, date, 'T-5 business days'),
    add_attribute(rebalancing, effective_date, date, 'Rebalancing implementation date'),
    add_attribute(rebalancing, frequency, atom, 'annual/semi_annual/quarterly/monthly'),

    define_moment_interval(corporate_action, index_design, moment, 'Corporate event affecting index'),
    add_mi_status_state(corporate_action, announced, [processed]),
    add_attribute(corporate_action, action_type, atom, 'split/rights_issue/spinoff/merger/delisting'),
    add_attribute(corporate_action, effective_date, date, 'Corporate action date'),
    add_attribute(corporate_action, adjustment_factor, decimal, 'Price or share adjustment'),

    define_moment_interval(index_calculation, index_design, interval, 'Daily index level calculation'),
    add_attribute(index_calculation, calculation_time, timestamp, 'Time of calculation'),
    add_attribute(index_calculation, index_level, decimal, 'Calculated index value'),
    add_attribute(index_calculation, divisor, decimal, 'Index divisor at calculation'),
    add_attribute(index_calculation, total_return, decimal, 'Total return including dividends'),

    % Moment-Intervals - Governance Context
    define_moment_interval(governance_review, governance, interval, 'Periodic governance oversight'),
    add_mi_status_state(governance_review, scheduled, [in_progress, completed]),
    add_mi_status_state(governance_review, in_progress, [completed]),
    add_attribute(governance_review, review_date, date, 'Committee review date'),
    add_attribute(governance_review, review_type, atom, 'quarterly/annual/ad_hoc'),
    add_attribute(governance_review, decisions, list, 'Methodology decisions made'),

    define_moment_interval(methodology_change, governance, moment, 'Index methodology modification'),
    add_mi_status_state(methodology_change, proposed, [approved, rejected]),
    add_attribute(methodology_change, change_description, string, 'Description of methodology change'),
    add_attribute(methodology_change, approval_date, date, 'Committee approval date'),
    add_attribute(methodology_change, effective_date, date, 'Change implementation date'),

    % Moment-Intervals - Distribution Context
    define_moment_interval(license_agreement, distribution, interval, 'Index licensing contract'),
    add_mi_status_state(license_agreement, negotiation, [active, terminated]),
    add_mi_status_state(license_agreement, active, [terminated]),
    add_attribute(license_agreement, start_date, date, 'License start date'),
    add_attribute(license_agreement, end_date, date, 'License end date'),
    add_attribute(license_agreement, fee_structure, atom, 'asset_based/derivative/data'),
    add_attribute(license_agreement, fee_rate, decimal, 'Basis points or fixed fee'),

    define_moment_interval(data_distribution, distribution, interval, 'Index data dissemination'),
    add_attribute(data_distribution, distribution_time, timestamp, 'Data release time'),
    add_attribute(data_distribution, data_type, atom, 'levels/constituents/analytics'),
    add_attribute(data_distribution, format, atom, 'file format or protocol'),

    % Roles (Yellow) - Index Design Context
    define_role(index_constituent, index_design, 'Security inclusion', 'Security selected for index'),
    add_attribute(index_constituent, weight, decimal, 'Security weight in index'),
    add_attribute(index_constituent, shares, decimal, 'Number of shares in index'),
    add_attribute(index_constituent, selection_score, decimal, 'Factor or ranking score'),
    add_behaviour(index_constituent, calculate_contribution, [price], [contribution]),

    define_role(universe_member, index_design, 'Eligible security', 'Security eligible for selection'),
    add_attribute(universe_member, market_cap, decimal, 'Free-float market capitalisation'),
    add_attribute(universe_member, liquidity_score, decimal, 'Trading liquidity metric'),
    add_attribute(universe_member, factor_scores, list, 'Value/quality/momentum scores'),
    add_behaviour(universe_member, evaluate_eligibility, [], [eligible]),

    % Roles - Governance Context
    define_role(committee_member, governance, 'Governance participation', 'Member of index committee'),
    add_attribute(committee_member, role_type, atom, 'chair/executive/independent'),
    add_attribute(committee_member, voting_rights, boolean, 'Has voting rights'),
    add_behaviour(committee_member, vote_on_change, [proposal_id], [vote]),

    define_role(index_administrator, governance, 'Index administration', 'Entity administering index'),
    add_attribute(index_administrator, registration_status, atom, 'authorized/registered/exempt'),
    add_attribute(index_administrator, jurisdiction, string, 'Regulatory jurisdiction'),
    add_behaviour(index_administrator, approve_methodology, [change_id], [approval]),

    % Roles - Distribution Context
    define_role(licensee, distribution, 'License holder', 'Entity licensing index'),
    add_attribute(licensee, license_type, atom, 'etf/derivative/data'),
    add_attribute(licensee, aum_tracked, decimal, 'Assets tracking index'),
    add_behaviour(licensee, report_usage, [], [usage_data]),

    define_role(data_vendor, distribution, 'Data distribution', 'Vendor distributing index data'),
    add_attribute(data_vendor, distribution_channels, list, 'Bloomberg/Reuters/Direct'),
    add_behaviour(data_vendor, distribute_data, [data], [confirmation]),

    % Party/Place/Thing (Green) - Index Design Context
    define_thing(index, index_design, financial_instrument, 'Passive index product'),
    add_attribute(index, base_date, date, 'Index inception date'),
    add_attribute(index, base_value, decimal, 'Starting index level'),
    add_attribute(index, currency, atom, 'Index base currency'),
    add_attribute(index, constituent_count, integer, 'Number of constituents'),

    define_thing(security, index_design, financial_instrument, 'Tradeable security'),
    add_attribute(security, ticker, string, 'Trading symbol'),
    add_attribute(security, exchange, string, 'Primary exchange'),
    add_attribute(security, security_type, atom, 'equity/bond/reit'),
    add_attribute(security, country, string, 'Country of domicile'),

    define_thing(index_methodology, index_design, document, 'Index construction rules'),
    add_attribute(index_methodology, version, string, 'Methodology version'),
    add_attribute(index_methodology, last_updated, date, 'Last modification date'),
    add_attribute(index_methodology, selection_rules, list, 'Constituent selection criteria'),
    add_attribute(index_methodology, weighting_scheme, atom, 'market_cap/equal/factor'),

    % Party/Place/Thing - Governance Context
    define_party(fund_manager, governance, organisation, 'Investment management company'),
    add_attribute(fund_manager, aum_total, decimal, 'Total assets under management'),
    add_attribute(fund_manager, regulatory_status, string, 'Regulatory registrations'),

    define_party(regulator, governance, organisation, 'Financial market regulator'),
    add_attribute(regulator, jurisdiction, string, 'Regulatory jurisdiction'),
    add_attribute(regulator, framework, string, 'Applicable regulation'),

    define_thing(governance_framework, governance, policy, 'Index governance policies'),
    add_attribute(governance_framework, committee_charter, string, 'Committee responsibilities'),
    add_attribute(governance_framework, conflict_policy, string, 'Conflict of interest policy'),

    % Party/Place/Thing - Distribution Context
    define_party(etf_provider, distribution, organisation, 'ETF issuer'),
    add_attribute(etf_provider, products_offered, list, 'ETF products'),

    define_party(derivative_exchange, distribution, organisation, 'Futures/options exchange'),
    add_attribute(derivative_exchange, contract_specs, list, 'Derivative specifications'),

    % Descriptions (Blue) - Index Design Context
    define_description(index_type, index_design, index, 'Index classification'),
    add_description_default(index_type, rebalancing_frequency, quarterly),
    add_description_default(index_type, buffer_percentage, 10),
    add_attribute(index_type, asset_class, atom, 'equity/fixed_income/multi_asset'),
    add_attribute(index_type, strategy, atom, 'market_cap/factor/esg/thematic'),

    define_description(security_classification, index_design, security, 'Security categorisation'),
    add_description_default(security_classification, min_liquidity, 1000000),
    add_description_default(security_classification, min_market_cap, 100000000),
    add_attribute(security_classification, gics_sector, string, 'GICS sector code'),
    add_attribute(security_classification, market_classification, atom, 'developed/emerging/frontier'),

    define_description(weighting_methodology, index_design, index_methodology, 'Weight calculation method'),
    add_attribute(weighting_methodology, method_name, atom, 'Weighting approach'),
    add_attribute(weighting_methodology, cap_rules, list, 'Maximum weight constraints'),
    add_attribute(weighting_methodology, optimisation_target, string, 'For optimised weights'),

    % Descriptions - Governance Context
    define_description(committee_type, governance, fund_manager, 'Governance committee structure'),
    add_description_default(committee_type, meeting_frequency, quarterly),
    add_description_default(committee_type, quorum_requirement, 0.6),
    add_attribute(committee_type, member_requirements, list, 'Composition requirements'),

    % Descriptions - Distribution Context
    define_description(license_model, distribution, etf_provider, 'Licensing structure'),
    add_description_default(license_model, base_fee_bps, 3),
    add_attribute(license_model, fee_calculation, string, 'Fee formula'),
    add_attribute(license_model, minimum_fee, decimal, 'Annual minimum'),

    % Link objects (Blue→Green→Yellow→Pink)
    % Index Design Links
    link_ppt_to_description(index, index_type),
    link_ppt_to_description(security, security_classification),
    link_ppt_to_description(index_methodology, weighting_methodology),
    
    link_role_to_player(index_constituent, security),
    link_role_to_player(universe_member, security),
    
    link_role_to_mi(index_constituent, rebalancing),
    link_role_to_mi(index_constituent, index_calculation),
    link_role_to_mi(universe_member, rebalancing),

    % Governance Links
    link_ppt_to_description(fund_manager, committee_type),
    
    link_role_to_player(committee_member, fund_manager),
    link_role_to_player(index_administrator, fund_manager),
    
    link_role_to_mi(committee_member, governance_review),
    link_role_to_mi(index_administrator, methodology_change),

    % Distribution Links
    link_ppt_to_description(etf_provider, license_model),
    
    link_role_to_player(licensee, etf_provider),
    link_role_to_player(licensee, derivative_exchange),
    link_role_to_player(data_vendor, etf_provider),
    
    link_role_to_mi(licensee, license_agreement),
    link_role_to_mi(data_vendor, data_distribution),

    % Plan-Actual Links
    link_plan_actual(governance_review, methodology_change, plan_to_actual),

    % Aggregates
    define_aggregate(index_agg, index_design, index),
    add_aggregate_member(index_agg, index_methodology),
    add_aggregate_member(index_agg, index_constituent),
    add_aggregate_invariant(index_agg, 'Total constituent weights must equal 100%'),
    add_aggregate_invariant(index_agg, 'Minimum 20 constituents for UCITS compliance'),

    define_aggregate(rebalancing_agg, index_design, rebalancing),
    add_aggregate_member(rebalancing_agg, universe_member),
    add_aggregate_member(rebalancing_agg, index_constituent),
    add_aggregate_invariant(rebalancing_agg, 'Selected constituents must be from eligible universe'),

    define_aggregate(governance_agg, governance, governance_framework),
    add_aggregate_member(governance_agg, committee_member),
    add_aggregate_member(governance_agg, governance_review),
    add_aggregate_invariant(governance_agg, 'Quorum required for methodology changes'),

    % Context Links
    link_contexts(governance, index_design, customer_supplier, 'Governance provides oversight for index design'),
    link_contexts(index_design, distribution, customer_supplier, 'Index design provides data for distribution'),

    % Domain classification
    mark_core_domain(index_design),
    mark_supporting_domain(governance),
    mark_supporting_domain(distribution).