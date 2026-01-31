# CLAUDE.md - larj-model

## Overview
Prolog project coding guidelines

# LLM Instructions for Prolog Code Generation and Review

**Source:** Covington, M.A., Bagnara, R., O'Keefe, R.A., Wielemaker, J., & Price, S. (2011). *Coding Guidelines for Prolog* — Theory and Practice of Logic Programming.

---

## 1. Foundational Principles

When generating or reviewing Prolog code, understand that Prolog requires disciplined coding practices more than many other languages due to:

- **Absence of prescriptive typing** — no compiler-enforced type system to catch argument errors
- **Multi-modal predicates** — arguments can serve as inputs, outputs, or both
- **Unification semantics** — pattern matching and logical variable binding introduce subtle correctness concerns
- **Backtracking behaviour** — control flow can revisit predicates unexpectedly

**Core objective:** Code must be readable, maintainable, and correct under all intended call patterns. A program "works" if developers and maintainers can approach expected behaviour over its intended lifespan.

---

## 2. Code Layout Rules

### 2.1 Line Length and Clause Structure

- Limit lines to a reasonable width (80 characters typical, 72 for documentation)
- Begin each clause on a new line
- Indent all lines of a clause except the first (the head)
- Place each subgoal on a separate line for readability

**Correct pattern:**

```prolog
same_length([], []).
same_length([_|L1], [_|L2]) :-
    same_length(L1, L2).
```

### 2.2 Vertical Spacing

- No blank lines between clauses of the same predicate
- One blank line before the first clause of a related predicate
- Two blank lines before an unrelated predicate group

### 2.3 Comma and Spacing Conventions

Apply consistent spacing to distinguish comma usage:

- **Conjunction (and-then):** Follow comma with newline and indentation
- **Argument separator:** Follow comma with a single space
- **Data structure elements:** Optionally omit space for tight lists like `[1,2,3]`

### 2.4 Conditional Layout (If-Then-Else)

Use consistent indentation for `->` and `;` constructs:

```prolog
(   test_1
->  action_if_true
;   test_2
->  action_if_test2
;   default_action
)
```

### 2.5 Repeat-Cut Structure

Indent an additional level between `repeat` and corresponding `!`:

```prolog
process_queries :-
    repeat,
        read_query(Q),
        handle(Q),
        Q = [quit],
    !,
    write('All done.'), nl.
```

---

## 3. Naming Conventions

### 3.1 Identifier Style

- **Prefer underscores** over camelCase: `is_well_formed` not `isWellFormed`
- **Variables:** Begin with uppercase, use underscores: `Result_So_Far`
- **Atoms/predicates:** Begin with lowercase, use underscores: `remove_duplicates`
- Ensure all names are **pronounceable** and **unambiguous**

### 3.2 Predicate Naming Semantics

| Predicate Purpose | Naming Pattern | Examples |
|-------------------|----------------|----------|
| Property/relation | Noun, adjective, or indicative verb | `sorted_list`, `contains_duplicates` |
| Procedural action | Imperative verb phrase | `remove_duplicates`, `print_contents` |
| Auxiliary predicate | Base name + suffix | `foo_loop`, `foo_case`, `foo_aux` |

### 3.3 Argument Order Hints

- Name predicates to clarify argument order: `mother_child(M, C)` not `mother_of(A, B)`
- Use `_to_` pattern for conversions: `list_to_tree(+L, ?T)`

### 3.4 Variable Naming Patterns

**List decomposition:**

```prolog
[Tree|Trees]    % singular for head, plural for tail
[T|Ts]          % abbreviated form with context
```

**Threaded state variables:**

```prolog
foo(State0, State) :-
    step1(State0, State1),
    step2(State1, State).
```

Alternative with `_in`/`_out`:

```prolog
foo(State_in, State_out) :-
    step1(State_in, State_tmp),
    step2(State_tmp, State_out).
```

### 3.5 Module and Type Prefixes

- When modules are used with imports, include type in predicate name: `ord_union`, `list_assoc`
- Avoid redundancy like `quaternion:quaternion_magnitude` when module prefixes are explicit

---

## 4. Documentation Standards

### 4.1 Predicate Header Comments

Every exported predicate requires a structured comment block:

```prolog
%% remove_duplicates(+List, -Processed_List) is det
%
%  Removes duplicates in List, giving Processed_List.
%  Elements match if unifiable; the last match is preserved.
```

### 4.2 Mode Specifiers

Use argument mode annotations consistently:

| Symbol | Meaning |
|--------|---------|
| `+` | Must be instantiated on entry (input) |
| `-` | Must be uninstantiated on entry (output) |
| `?` | May be either; behaviour may vary |
| `*` | Must be ground (no unbound variables) |
| `@` | Will not be further instantiated |
| `:` | Meta-argument (implies `+`) |

### 4.3 Determinism Specifiers

| Specifier | Meaning |
|-----------|---------|
| `det` | Exactly one solution, no choice points |
| `semidet` | Zero or one solution |
| `nondet` | Zero or more solutions |
| `multi` | One or more solutions |

### 4.4 Auxiliary vs. Main Predicates

- Use `%%` for main predicate comments (exported/public)
- Use `%` for auxiliary predicate comments (internal)

### 4.5 Inline Comments

- Use `%TBD:` for incomplete sections
- Use `%FIXME:` for known issues
- Use `%DEBUG:` for temporary debugging code
- Use `%HACK:` for suboptimal solutions requiring future attention

---

## 5. Language Idioms and Correctness

### 5.1 Steadfastness Requirement

**Critical:** All predicates must be steadfast — they must behave correctly when output arguments are pre-instantiated:

```prolog
?- foo(X), X = expected.   % Must succeed iff
?- foo(expected).          % this succeeds
```

### 5.2 Argument Ordering Convention

Order arguments as: **inputs → intermediate results → final outputs**

```prolog
%% transform(+Input, +Options, -Output)
transform(Input, Options, Output) :- ...
```

### 5.3 Cut Usage Guidelines

| Rule | Rationale |
|------|-----------|
| Never add cuts to fix unknown problems | Masks real bugs, often far from actual error |
| Make cuts visible | Place red cuts on their own line |
| Prefer if-then-else over cut-fail | More declarative, clearer intent |
| Avoid cut at end of last clause | Usually indicates logic error |

**Anti-pattern to detect:**

```prolog
foo(X) :- condition1(X), action1(X).
foo(X) :- action2(X), !.  % Cut at end of last clause — suspicious
```

### 5.4 Tail Recursion

Prefer tail recursion for memory efficiency in high-iteration loops:

```prolog
% Tail recursive (good for large lists)
sum_list([], Acc, Acc).
sum_list([H|T], Acc, Result) :-
    NewAcc is Acc + H,
    sum_list(T, NewAcc, Result).
```

### 5.5 List Operations

- Work at the **beginning** of lists (O(1) access vs O(n) for end)
- Avoid `append/3` in performance-critical code — use difference lists
- Use built-in sorting predicates rather than implementing quicksort

**Difference list pattern for O(1) concatenation:**

```prolog
% Represent [a,b,c] as [a,b,c|X]-X
concat_diff(A-B, B-C, A-C).
```

### 5.6 Database Assertions

- Avoid `asserta/assertz` and `retract` unless data must survive backtracking
- Wrap database modifications in interface predicates for validation and thread safety

### 5.7 Run-time Type Checking

Implement explicit argument validation for critical predicates:

```prolog
public_predicate(Arg1, Arg2) :-
    validate_args(Arg1, Arg2),
    internal_predicate(Arg1, Arg2).
```

---

## 6. Development and Testing Practices

### 6.1 Debugging Strategy

- Master the standard Prolog debugger (spy, trace, leap)
- Use `print/1-2` instead of `write/1-2` for debugging output (customisable abbreviation)
- Tag debug output for selective enabling: topic + severity level
- Never rely on `write` sprinkled through code — use logging infrastructure

### 6.2 Testing Requirements

**Every predicate must be tested by forcing backtracking:**

```prolog
?- count_up(5).       % Test normal execution
?- count_up(5), fail. % Test backtracking behaviour
```

### 6.3 Error Handling Philosophy

**In error situations, crash — do not silently fail:**

```prolog
% BAD: Fails silently on bad input
square_root(georgia, _) :- fail.

% GOOD: Throws informative error
square_root(X, _) :-
    \+ number(X),
    throw(error(type_error(number, X), context(square_root/2, _))).
```

### 6.4 Error Message Requirements

Error messages must include:

- Predicate name where error was detected
- The offending data value
- Explanation of what was expected

**Good:** `mysort/3: 'x(y,z,w)' cannot be sorted because it is not a list`

**Bad:** `error: illegal data, wrong type`

### 6.5 Code Organisation

- Isolate non-portable code in wrapper predicates
- Isolate magic numbers as facts
- Use version control (Git, etc.) for all source, tests, and documentation
- Implement systematic regression testing (e.g., SWI-Prolog's `plunit`)

### 6.6 Common Error Patterns to Detect

- Cut at end of last clause (usually wrong)
- `repeat` not followed by cut (infinite loop risk)
- `append([SingleElement], List, Result)` — use `[Element|List]` instead

---

## 7. Code Review Checklist for LLM

When reviewing Prolog code, verify:

1. **Layout:** Consistent indentation, one subgoal per line, appropriate spacing
2. **Naming:** Pronounceable, underscore-style, semantically accurate
3. **Documentation:** Mode/determinism annotations, argument descriptions
4. **Steadfastness:** Output arguments behave correctly when pre-bound
5. **Cut discipline:** Cuts are justified, visible, and not masking bugs
6. **Recursion:** Tail recursion used where memory efficiency matters
7. **Error handling:** Crashes with informative messages rather than silent failure
8. **Testability:** Each predicate can be tested under backtracking

---

## 8. Code Generation Template

When generating Prolog predicates, follow this structure:

```prolog
%% predicate_name(+Input, -Output) is det
%
%  Brief description of what the predicate computes.
%  Longer explanation if needed.
%
%  @param Input  Description of input argument
%  @param Output Description of output argument
%  @throws error_type If validation fails

predicate_name(Input, Output) :-
    validate_input(Input),
    predicate_name_impl(Input, Output).

% predicate_name_impl(+Input, -Output) is det
%
%  Internal implementation.
predicate_name_impl(Input, Output) :-
    % Implementation here
    ...
```

---

