# Building a Passive Index: Financial Design

## Executive Summary

A passive index is a rules-based methodology for selecting and weighting securities that serves as either a benchmark for measuring performance or the basis for an investable product. This document details the financial design elements required to construct a passive index suitable for commercial distribution.

---

## 1. Foundational Concepts

### 1.1 What Is a Passive Index?

A passive index is a systematic, transparent, and replicable method for representing the performance of a defined market segment. Unlike active management, which relies on discretionary security selection, a passive index follows predetermined rules that determine:

- **Universe definition**: Which securities are eligible for inclusion
- **Selection criteria**: Which eligible securities are actually included
- **Weighting methodology**: How much of each security the index holds
- **Rebalancing rules**: When and how the index is reconstituted

### 1.2 Strategic Rationale for Index Creation

Fund managers build proprietary indices for several reasons:

| Objective | Description |
|-----------|-------------|
| **Fee Retention** | Licensing third-party indices (MSCI, FTSE, S&P) incurs ongoing costs of 2-5 basis points of AUM |
| **Product Differentiation** | Custom indices enable unique investment propositions |
| **Factor Exposure** | Capture specific risk premia (value, momentum, quality, low volatility) |
| **ESG Integration** | Embed sustainability criteria directly into index methodology |
| **Thematic Investing** | Target emerging themes (AI, clean energy, ageing demographics) |

### 1.3 Regulatory Context

Index creation in major jurisdictions falls under specific regulatory frameworks:

- **EU Benchmarks Regulation (BMR)**: Requires authorisation or registration for index administrators
- **IOSCO Principles**: International standards for financial benchmarks covering governance, methodology, and accountability
- **FCA Handbook**: UK-specific requirements for benchmark administrators post-Brexit

---

## 2. Universe Definition

The universe is the starting pool from which index constituents are selected.

### 2.1 Geographic Scope

Define the markets included using standard classifications:

- **MSCI Market Classification**: Developed, Emerging, and Frontier markets
- **FTSE Country Classification**: Alternative geographic taxonomy
- **Index-specific definitions** based on listing location, incorporation jurisdiction, revenue source geography, or primary trading venue

### 2.2 Asset Class and Security Types

Specify eligible instrument types:

- **Equities**: Ordinary shares, depositary receipts (ADRs/GDRs), REITs
- **Fixed Income**: Government bonds, corporate bonds, asset-backed securities
- **Typical Exclusions**: Preferred shares, convertibles, warrants, ETFs, closed-end funds

### 2.3 Liquidity Filters

Establish minimum liquidity thresholds to ensure replicability:

| Metric | Typical Threshold | Purpose |
|--------|-------------------|---------|
| Average Daily Trading Value (ADTV) | >$1M USD (6-month avg) | Ensures tradability |
| Free-float Market Cap | >$100M USD | Excludes micro-caps |
| Trading Days | >90% of available days | Confirms active trading |
| Bid-Ask Spread | <2% median spread | Limits transaction costs |

---

## 3. Constituent Selection

### 3.1 Market Capitalisation Approach

The simplest selection method ranks securities by market capitalisation and selects the top N:

1. Calculate free-float adjusted market cap for each security: FF_MCap = Shares Outstanding × Free Float Factor × Price

2. Rank universe by FF_MCap descending

3. Select top N securities OR securities representing X% cumulative market cap

4. Apply buffer rules to reduce turnover: existing constituents remain if rank ≤ N + buffer; non-constituents added only if rank ≤ N - buffer

### 3.2 Factor-Based Selection

For smart beta indices, selection incorporates factor scores:

**Value Factor**: Weighted average of book-to-price ratio, earnings-to-price ratio, cash flow-to-price ratio, and dividend yield (all normalised)

**Quality Factor**: Weighted average of return on equity (ROE), debt-to-equity ratio (inverted), earnings variability (inverted), and accruals ratio (inverted)

**Momentum Factor**: 12-month price return excluding most recent month, optionally risk-adjusted by dividing return by volatility

**Low Volatility Factor**: Inverse of realised volatility over trailing period (typically 12 months), or beta relative to parent index

**Size Factor**: Inverse of market capitalisation, tilting toward smaller companies within the eligible universe

### 3.3 ESG Integration

ESG-integrated indices apply additional screens through three primary approaches:

**Exclusionary Screening**: Activity-based exclusions (tobacco, weapons, thermal coal), controversy-based exclusions (UN Global Compact violations), and threshold-based exclusions (>10% revenue from excluded activities)

**Best-in-Class Selection**: Rank by ESG score within each sector, select top 50% by ESG score, maintain sector neutrality versus parent index

**Tilting Approach**: Retain all constituents, overweight high ESG scores, underweight low ESG scores

### 3.4 Multi-Factor Selection

Composite factor indices combine multiple factors:

**Composite Score Construction**: Assign weights to individual factors (e.g., 25% Value, 25% Quality, 25% Momentum, 25% Low Volatility), normalise each factor score to z-scores within universe, calculate weighted average composite score

**Selection Methods**: Top N by composite score, or stratified selection ensuring minimum representation from each factor

---

## 4. Weighting Methodology

### 4.1 Market Capitalisation Weighting

The default approach, proportional to company size:

Weight_i = FF_MCap_i / Σ FF_MCap (sum across all constituents)

**Advantages**: Reflects market consensus, low turnover, high capacity

**Disadvantages**: Concentrates in largest/most expensive stocks, procyclical

### 4.2 Equal Weighting

Each constituent receives identical weight:

Weight_i = 1 / N (where N = number of constituents)

**Advantages**: Diversification, small-cap tilt, avoids concentration

**Disadvantages**: High turnover, capacity constraints, higher transaction costs

### 4.3 Factor Weighting

Weights proportional to factor scores:

Weight_i = Factor_Score_i / Σ Factor_Score (sum across all constituents)

Typically subject to constraints: maximum weight per security (typically 5%), minimum weight per security (typically 0.05%), and sector deviation limits (±5% versus benchmark)

### 4.4 Fundamental Weighting

Weights based on economic footprint rather than market price:

Weight_i = Fundamental_Metric_i / Σ Fundamental_Metric

**Common Metrics**: Revenue, book value, dividends, cash flow, or composite of multiple fundamentals

**Rationale**: Breaks link between weight and potentially mispriced market capitalisation

### 4.5 Risk-Based Weighting

**Minimum Variance**: Optimises weights to minimise portfolio volatility subject to constraints. Requires covariance matrix estimation.

**Risk Parity**: Weight_i = (1/σ_i) / Σ (1/σ) where σ_i = volatility of security i. Equalises risk contribution rather than capital allocation.

**Maximum Diversification**: Maximises ratio of weighted average volatility to portfolio volatility, increasing exposure to lowly correlated assets.

### 4.6 Capping Rules

Regulatory and diversification requirements impose weight limits:

**UCITS Concentration Rules**: No single security >10% of index, securities >5% cannot sum to >40%, minimum 20 constituents for diversification

**Capping Algorithm**: Calculate uncapped weights, identify securities exceeding cap, redistribute excess weight pro-rata to uncapped securities, iterate until all constraints satisfied

**Sector Caps**: Limit exposure to any single sector (e.g., maximum 30% in Technology)

---

## 5. Rebalancing Rules

### 5.1 Rebalancing Frequency

| Frequency | Use Case | Turnover Impact |
|-----------|----------|-----------------|
| Annual | Low-turnover strategies | Minimal |
| Semi-annual | Standard market cap indices | Low |
| Quarterly | Factor indices, ESG | Moderate |
| Monthly | Momentum strategies | High |

### 5.2 Rebalancing Calendar

A consistent schedule comprises three key dates:

**Review Date (T-10 business days)**: Capture constituent data, run selection algorithm, calculate pro-forma weights

**Announcement Date (T-5 business days)**: Publish constituent changes, disclose additions and deletions, provide weight information

**Effective Date (T)**: Index reflects new composition, typically end of day pricing, aligned with options/futures expiry where relevant

### 5.3 Buffer Rules

Buffers reduce unnecessary turnover at ranking boundaries. For an index targeting 100 constituents with 10% buffer: additions require a security to rank ≤90, deletions occur only if existing constituent ranks >110. This prevents securities oscillating in and out at the boundary.

### 5.4 Packeting

For indices with liquidity constraints, changes may be implemented gradually:

**Partial Inclusion**: New constituents added at 50% target weight initially, increased to 100% at subsequent rebalance

**Staged Deletion**: Removed constituents reduced to 50% weight initially, fully removed at next rebalance

### 5.5 Corporate Actions Handling

| Event | Index Treatment |
|-------|-----------------|
| Stock split | Adjust shares, no index impact |
| Rights issue | Adjust for dilution at subscription price |
| Spin-off | Add new entity, adjust parent |
| M&A (cash) | Remove target at offer price |
| M&A (stock) | Remove target, adjust acquirer weight |
| Delisting | Remove at last traded price |
| Bankruptcy | Remove at zero or last price |
| IPO | Add at next scheduled rebalance if eligible |
| Special dividend | Adjust for capital return |

---

## 6. Index Calculation Methodology

### 6.1 Price Index

**Formula**: Index_Level = Σ(Price_i × Shares_i × FX_i) / Divisor

Where the sum is across all constituents, and FX converts local currency to index base currency.

### 6.2 Total Return Index

Incorporates dividend reinvestment:

TR_Index_t = TR_Index_(t-1) × (1 + Price_Return + Dividend_Return)

Where Dividend_Return = Gross_Dividends × (1 - Withholding_Tax_Rate) / Previous_Index_Level

### 6.3 Return Variants

| Variant | Dividend Treatment | Use Case |
|---------|-------------------|----------|
| Price Return | Excluded | Derivative pricing |
| Gross Total Return | Fully reinvested | Institutional benchmarking |
| Net Total Return | Reinvested less withholding tax | Fund performance comparison |

### 6.4 Divisor Methodology

The divisor maintains index continuity when constituent changes occur.

**Events Requiring Adjustment**: Index rebalancing (additions, deletions, weight changes), corporate actions (splits, rights issues, spin-offs), share changes (buybacks, secondary offerings), initial index launch

**Principle**: Index level should not change due to non-market events.

**Adjustment Formula**: New_Divisor = Old_Divisor × (New_Market_Value / Old_Market_Value)

**Example**: Before rebalance with Market Value = $1,000,000, Divisor = 1,000, Index = 1,000. After rebalance with Market Value = $1,050,000 due to additions, New Divisor = 1,000 × ($1,050,000 / $1,000,000) = 1,050. Index Level = $1,050,000 / 1,050 = 1,000 (unchanged).

### 6.5 Currency Variants

**Local Currency**: Each constituent valued in its trading currency, returns reflect local market performance

**Base Currency (Unhedged)**: All constituents converted to single currency at spot rates, returns include currency effects

**Base Currency (Hedged)**: Currency exposure hedged using forward contracts, isolates local equity returns

---

## 7. Governance Framework

### 7.1 Index Governance Committee

A formal governance structure is required under most regulatory frameworks:

**Composition**: Independent Chair (non-executive), Head of Index Management, Chief Compliance Officer, Risk Management representative, External independent member (recommended)

**Responsibilities**: Approve index methodology changes, review annual methodology effectiveness, oversee complaint handling, approve discretionary decisions, review conflicts of interest

**Meeting Frequency**: Quarterly scheduled meetings plus ad hoc meetings for urgent methodology decisions

### 7.2 Methodology Documentation

Regulatory requirements mandate comprehensive methodology documents covering:

1. **Index Overview**: Objective and use cases, key features, base date and value

2. **Universe Definition**: Eligible securities, geographic coverage, exclusion criteria

3. **Constituent Selection**: Selection rules, buffer provisions, number of constituents

4. **Weighting Scheme**: Weighting methodology, capping rules, constraint handling

5. **Index Calculation**: Formula, divisor methodology, price sources

6. **Index Maintenance**: Rebalancing schedule, corporate actions treatment, correction policy

7. **Governance**: Oversight committee, methodology review process, complaint procedure

### 7.3 Conflicts of Interest

Index administrators must identify and manage conflicts:

**Self-Indexing Conflict**: Fund manager creates index it will track. Mitigations include independent governance committee, pre-defined rules-based methodology, and regular third-party audit.

**Front-Running Conflict**: Proprietary trading desks learn of changes early. Mitigations include information barriers, simultaneous public announcement, and transaction monitoring.

**Selective Disclosure**: Favoured clients receive early information. Mitigations include standardised announcement schedule, equal access policies, and audit trail of disclosures.

---

## 8. Licensing and Distribution

### 8.1 Index Licensing Models

**Asset-Based License**: Fee of 1-5 basis points of tracking AUM, used for ETFs, index funds, and segregated mandates

**Derivative License**: Per contract or notional-based fee, used for futures, options, and swaps

**Data License**: Fixed annual or per-user fee, used for benchmarking, analytics, and research

**Branded Product**: Embedded in product fees, used for proprietary funds using own index

### 8.2 Distribution Content

**Index Levels**: Price return, gross total return, net total return (daily, real-time where applicable)

**Constituent Data**: Weights, shares, prices, corporate actions

**Analytics**: Performance attribution, risk metrics, factor exposures

**Documentation**: Methodology, factsheets, quarterly reviews

---

## 9. Regulatory Compliance

### 9.1 EU Benchmarks Regulation (BMR)

**Administrator Obligations**: Authorisation or registration with national competent authority, governance and control framework, conflicts of interest management, methodology documentation and publication, complaint handling procedure, record keeping (minimum 5 years)

**Benchmark Statement Requirements**: Measurement objective, circumstances affecting reliability, key elements of methodology, review and adjustment procedures, cessation procedures

### 9.2 IOSCO Principles for Financial Benchmarks

**Governance (Principles 1-5)**: Overall responsibility of administrator, oversight function independence, conflicts of interest framework, control framework adequacy

**Quality of Methodology (Principles 6-10)**: Methodology design and data sufficiency, hierarchy of data inputs, transparency of methodology, periodic review process

**Accountability (Principles 16-19)**: Complaint handling, audit arrangements, audit trail maintenance, regulatory cooperation

---

## Appendix A: Glossary

| Term | Definition |
|------|------------|
| **AUM** | Assets Under Management |
| **Basis Point (bp)** | One hundredth of one percent (0.01%) |
| **Divisor** | Factor maintaining index continuity through corporate actions |
| **Free Float** | Shares available for public trading, excluding strategic holdings |
| **GICS** | Global Industry Classification Standard (MSCI/S&P) |
| **ICB** | Industry Classification Benchmark (FTSE) |
| **Smart Beta** | Indices using factor-based rather than market-cap weighting |
| **Tracking Error** | Standard deviation of difference between fund and index returns |
| **UCITS** | EU regulatory framework for retail investment funds |

---

## Appendix B: Sample Index Calculation

**Example**: Equal-weighted index with three constituents

**Constituents at Rebalance**:
- Stock A: Price $100, Weight 33.33%
- Stock B: Price $50, Weight 33.33%
- Stock C: Price $25, Weight 33.33%

**Base Value**: 1,000 at launch

**Initial Setup** (Notional Market Value: $1,000,000):
- Stock A: 3,333 shares × $100 = $333,333
- Stock B: 6,667 shares × $50 = $333,333
- Stock C: 13,333 shares × $25 = $333,333
- Total Market Value: $1,000,000
- Initial Divisor: $1,000,000 / 1,000 = 1,000

**Day 2 - Prices Change**:
- Stock A: $105 → 3,333 × $105 = $350,000
- Stock B: $48 → 6,667 × $48 = $320,000
- Stock C: $27 → 13,333 × $27 = $360,000
- Total Market Value: $1,030,000
- Index Level: $1,030,000 / 1,000 = 1,030

**Return**: (1,030 - 1,000) / 1,000 = 3.0%

---

## Appendix C: Regulatory Contacts by Jurisdiction

| Jurisdiction | Regulator | Applicable Regulation |
|--------------|-----------|----------------------|
| European Union | ESMA + National Authorities | Benchmarks Regulation (BMR) |
| United Kingdom | FCA | UK BMR (onshored) |
| United States | SEC / CFTC | Securities laws (no specific index regulation) |
| Switzerland | FINMA | Financial Market Infrastructure Act |
| Singapore | MAS | Securities and Futures Act |
| Hong Kong | SFC | General supervision |
| Japan | FSA / JFSA | Financial Instruments and Exchange Act |

---

*Document Version: 1.2*  
*Last Updated: January 2025*