# Netz Query DSL Design Notes

This document sketches a possible future direction for `netz`: a Cypher-inspired graph DSL designed for Emacs Lisp.

The current implementation is considered a first pass. We are not committed to existing APIs, data model decisions, or the current `:match` filter implementation. The goal is to design the DSL and data model together around the experience we want.

## North Star

Cypher has a good model for graph work, but its ASCII-art syntax is not a good fit for Emacs Lisp. The goal is to keep the useful ideas:

- node patterns
- relationship patterns
- named bindings
- property predicates
- `MATCH` / `WHERE` / `RETURN`
- path queries
- graph mutation via `CREATE` / `MERGE` / `SET` / `DELETE`

But express them as normal Lisp data using keyword clauses.

Example:

```elisp
(netz-query graph
  (:match
   (:node person :label "Person" :name "Tosh")
   (:edge person project :type "WORKS_ON" :direction :out)
   (:node project :label "Project"))
  (:where
   (> (netz-prop project :stars) 10))
  (:return person project))
```

Mutation example:

```elisp
(netz-query graph
  (:merge
   (:node person :id "person:tosh" :label "Person"))
  (:set person :name "Tosh" :active t)
  (:merge
   (:node project :id "project:netz" :label "Project"))
  (:merge
   (:edge person project :type "WORKS_ON"))
  (:return person project))
```

## Preferred Syntax Style

Use keyword clause style:

```elisp
(netz-query :g
  (:match
   (:node person :label "Person" :name "Tosh")
   (:edge person project :type "WORKS_ON" :direction :out)
   (:node project :label "Project"))
  (:return person project))
```

This style is preferred because it is:

- plain Lisp data
- easy to parse
- macro-friendly without requiring too much syntax magic
- close to existing `netz` plist-oriented style
- extensible with future clauses
- Cypher-inspired without copying Cypher syntax

## Query Model

The main abstraction should be:

> A query is a pipeline over binding rows.

For example:

```elisp
(:match
 (:node person :label "Person")
 (:edge person project :type "WORKS_ON")
 (:node project :label "Project"))
```

Conceptually:

1. Start with one empty row.
2. Match `person` nodes, producing many rows.
3. For each row, match edges from `person` to `project`.
4. For each row, constrain `project` to `:label "Project"`.
5. Return selected bindings.

This is simple, powerful, and close to how Cypher feels semantically.

## Core Pattern Forms

### Node Pattern

```elisp
(:node binding &rest properties)
```

Example:

```elisp
(:node n :label "Note" :title "Graph DSL")
```

This means: bind `n` to nodes whose properties match the given plist.

Matching by id:

```elisp
(:node start :id "note:1")
```

Anonymous or ignored nodes could use `_`:

```elisp
(:node _ :label "Tag")
```

### Edge Pattern

```elisp
(:edge source target &rest properties)
```

Undirected / any-direction match:

```elisp
(:edge a b :type "RELATED")
```

Directed match:

```elisp
(:edge a b :type "LINKS_TO" :direction :out)
```

Supported directions should likely be:

```elisp
:direction :out
:direction :in
:direction :any
```

Where `:any` may be the default for ergonomic graph exploration.

## Path Patterns

Path support can come after basic node/edge matching.

Example:

```elisp
(netz-query graph
  (:match
   (:node start :id "note:1")
   (:path start target
    :edge (:type "LINKS_TO")
    :direction :out
    :depth (1 3)))
  (:return target))
```

Potential path options:

```elisp
:path source target
:edge edge-property-pattern
:direction :out | :in | :any
:depth (min max)
```

## Where Clause

Prefer ordinary Elisp in `:where` rather than a custom predicate DSL at first.

```elisp
(:where
 (string-prefix-p "netz" (plist-get project :name)))
```

This likely means `netz-query` should be a macro, so query variables can become lexical bindings within the `:where` form.

A helper such as `netz-prop` could make this nicer:

```elisp
(:where
 (> (netz-prop project :stars) 10))
```

## Return Clause

Basic form:

```elisp
(:return person project)
```

The result should probably be a list of binding plists:

```elisp
((:person <node> :project <node>)
 (:person <node> :project <node>))
```

Possible future return variants:

```elisp
(:return :ids person project)
(:return :count)
(:return :graph :new-graph)
(:return :pluck person :name)
```

## Mutation Clauses

Mutation should use the same pattern language.

### Create

Always creates new graph entities:

```elisp
(netz-query graph
  (:create
   (:node person :id "person:tosh" :label "Person" :name "Tosh")
   (:node project :id "project:netz" :label "Project")
   (:edge person project :type "WORKS_ON"))
  (:return person project))
```

### Merge

Find-or-create semantics:

```elisp
(netz-query graph
  (:merge
   (:node person :id "person:tosh" :label "Person"))
  (:set person :name "Tosh" :active t)
  (:return person))
```

### Set

Update properties on a bound entity:

```elisp
(:set person :name "Tosh" :active t)
```

### Delete

Delete matched entities:

```elisp
(netz-query graph
  (:match
   (:node n :label "Temp"))
  (:delete n))
```

### Detach Delete

Delete a node and its relationships:

```elisp
(netz-query graph
  (:match
   (:node n :id "note:1"))
  (:detach-delete n))
```

## Data Model Reconsiderations

Since the existing implementation is not binding, we should revisit the graph internals.

### Edges Should Have Their Own IDs

The current implementation uses `(source target)` as the edge id. This prevents multiple distinct relationships between the same two nodes unless encoded awkwardly.

For Cypher-like graph work, this should be valid:

```elisp
(:edge a b :type "LIKES")
(:edge a b :type "KNOWS")
(:edge a b :type "WORKS_WITH")
```

A better edge structure:

```elisp
(:id edge-id
 :source node-id
 :target node-id
 :type "WORKS_ON"
 ...)
```

### Separate Storage From Indexes

A graph could be represented as a struct:

```elisp
(cl-defstruct netz-graph
  name
  path
  nodes
  edges
  out-index
  in-index)
```

Where:

- `nodes` maps node id to node plist
- `edges` maps edge id to edge plist
- `out-index` maps source node id to edge ids
- `in-index` maps target node id to edge ids

Nodes and edges can remain plists for flexibility:

```elisp
(:id "n1" :label "Note" :title "Hello")
```

```elisp
(:id "e1" :source "n1" :target "n2" :type "LINKS_TO")
```

This keeps graph internals structured while preserving flexible Lisp-native entities.

### Direction Should Be Explicit

Edges should always be stored as directed: `source -> target`.

Queries and traversals can choose direction:

```elisp
:direction :out
:direction :in
:direction :any
```

This avoids ambiguity while still allowing ergonomic undirected exploration.

### Replace the Current `:match` Implementation

The current `(:match :type "A")` implementation is clever, but we do not need to preserve it.

Instead of using keyword-named functions that return lambdas, use property patterns directly:

```elisp
(:node n :label "Person" :name "Tosh")
(:edge n m :type "KNOWS")
```

More complex predicates can live in `:where`:

```elisp
(:where
 (> (netz-prop n :age) 30))
```

## MVP

A good first implementation target:

```elisp
(netz-query graph
  (:match
   (:node var &rest props)
   (:edge from to &rest props))
  (:where elisp-form) ;; optional
  (:return vars...))
```

Example:

```elisp
(netz-query :notes
  (:match
   (:node note :label "Note")
   (:edge note tag :type "TAGGED")
   (:node tag :label "Tag" :name "emacs"))
  (:return note))
```

This should return all notes tagged `emacs`.

## Implementation Plan

Suggested layers:

1. New graph core
   - `netz-create-graph`
   - `netz-add-node`
   - `netz-add-edge`
   - `netz-node`
   - `netz-edge`
   - `netz-node-edges`

2. Pattern matcher
   - evaluate `(:node var props...)`
   - evaluate `(:edge a b props...)`
   - produce binding rows

3. `netz-query`
   - parse clauses
   - run matcher
   - apply optional `:where`
   - apply `:return`

4. Mutation clauses
   - `:create`
   - `:merge`
   - `:set`
   - `:delete`
   - `:detach-delete`

5. Path support
   - BFS/DFS over indexes
   - depth-limited traversal

## Open Questions

- Should `:edge` default to `:direction :any` or `:direction :out`?
- Should `netz-query` be purely a macro, or should it macro-expand into calls to runtime query functions?
- Should result rows be plists, alists, hash tables, or structs?
- Should node labels and edge types be strings, symbols, keywords, or unrestricted values?
- Should schema/validation be part of the core or a later layer?
- How much indexing should exist by default beyond in/out edge indexes?
