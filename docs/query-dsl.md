# Netz Query DSL

`netz-query` is a Cypher-inspired graph DSL for Emacs Lisp. It uses ordinary Lisp data and keyword clauses instead of ASCII-art graph syntax.

## Goals

The DSL provides:

- node patterns
- edge patterns
- named bindings
- property predicates
- `MATCH` / `WHERE` / `RETURN`
- path queries
- graph mutation via `CREATE` / `MERGE` / `SET` / `DELETE`

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
   (:edge person project :id "works:tosh:netz" :type "WORKS_ON"))
  (:return person project))
```

## Syntax Style

Queries use keyword clause style:

```elisp
(netz-query :g
  (:match
   (:node person :label "Person" :name "Tosh")
   (:edge person project :type "WORKS_ON" :direction :out)
   (:node project :label "Project"))
  (:return person project))
```

This style is:

- plain Lisp data
- easy to parse
- macro-friendly
- plist-oriented
- extensible with additional clauses

## Query Model

A query is a pipeline over binding rows.

For example:

```elisp
(:match
 (:node person :label "Person")
 (:edge person project :type "WORKS_ON")
 (:node project :label "Project"))
```

Conceptually:

1. Start with one empty row.
2. Match `person` nodes, producing rows.
3. Match edges from `person` to `project` for each row.
4. Constrain `project` to `:label "Project"` for each row.
5. Return selected bindings.

## Core Pattern Forms

### Node Pattern

```elisp
(:node binding &rest properties)
```

Example:

```elisp
(:node n :label "Note" :title "Graph DSL")
```

This binds `n` to nodes whose properties match the given plist.

Matching by id:

```elisp
(:node start :id "note:1")
```

Anonymous or ignored nodes use `_`:

```elisp
(:node _ :label "Tag")
```

### Edge Pattern

```elisp
(:edge source target &rest properties)
```

Any-direction match:

```elisp
(:edge a b :type "RELATED")
```

Directed match:

```elisp
(:edge a b :type "LINKS_TO" :direction :out)
```

Supported directions:

```elisp
:direction :out
:direction :in
:direction :any
```

`:any` is the default.

Bind the matched edge with `:as`:

```elisp
(:edge person project :as rel :type "WORKS_ON" :direction :out)
```

## Path Patterns

Path patterns find reachable nodes within a depth range.

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

Supported path options:

```elisp
:path source target
:edge edge-property-pattern
:direction :out | :in | :any
:depth (min max)
:as route-binding
```

Use `:as` to bind the full route:

```elisp
(netz-query graph
  (:match
   (:node start :id "person:tosh")
   (:path start target
    :as route
    :direction :any
    :depth (1 5)))
  (:return target route))
```

A route is a plist:

```elisp
(:start <start-node>
 :end <end-node>
 :nodes (<ordered-node-plists>)
 :edges (<ordered-edge-plists>)
 :steps ((:from <node> :edge <edge> :to <node> :traversed :out) ...)
 :length 2
 :graph <netz-graph>)
```

Order guarantee:

- `:nodes` are in traversal order
- `:edges` are in traversal order
- edge at index `i` connects node at index `i` to node at index `i+1`
- `:steps` records the exact traversal, including `:traversed :out` or `:traversed :in`
- `:graph` contains the route nodes and edges for use with graph-level APIs

Checking whether two nodes are connected:

```elisp
(> (netz-query graph
     (:match
      (:node a :id "person:tosh")
      (:node b :id "tag:emacs")
      (:path a b :direction :any :depth (1 5)))
     (:return :count))
   0)
```

## Where Clause

`:where` is ordinary Elisp evaluated with query variables lexically bound.

```elisp
(:where
 (string-prefix-p "netz" (plist-get project :name)))
```

`netz-prop` provides property access:

```elisp
(:where
 (> (netz-prop project :stars) 10))
```

## Return Clause

Basic form:

```elisp
(:return person project)
```

The result is a list of binding plists:

```elisp
((:person <node> :project <node>)
 (:person <node> :project <node>))
```

Supported return variants:

```elisp
(:return :ids person project)
(:return :count)
(:return :graph :new-graph)
(:return :pluck person :name)
```

## Mutation Clauses

Mutation uses the same pattern language.

### Create

`CREATE` creates graph entities and errors on duplicate ids:

```elisp
(netz-query graph
  (:create
   (:node person :id "person:tosh" :label "Person" :name "Tosh")
   (:node project :id "project:netz" :label "Project")
   (:edge person project :id "works:tosh:netz" :type "WORKS_ON"))
  (:return person project))
```

### Merge

`MERGE` has find-or-create semantics by `:id`:

```elisp
(netz-query graph
  (:merge
   (:node person :id "person:tosh" :label "Person"))
  (:set person :name "Tosh" :active t)
  (:return person))
```

### Set

`SET` updates properties on a bound entity:

```elisp
(:set person :name "Tosh" :active t)
```

### Delete

`DELETE` deletes matched entities:

```elisp
(netz-query graph
  (:match
   (:node n :label "Temp"))
  (:delete n))
```

### Detach Delete

`DETACH DELETE` deletes a node and its relationships:

```elisp
(netz-query graph
  (:match
   (:node n :id "note:1"))
  (:detach-delete n))
```

## Data Model

### Edges Have IDs

Edges have their own ids. Multiple distinct relationships between the same two nodes are valid:

```elisp
(:edge a b :type "LIKES")
(:edge a b :type "KNOWS")
(:edge a b :type "WORKS_WITH")
```

Edge structure:

```elisp
(:id edge-id
 :source node-id
 :target node-id
 :type "WORKS_ON"
 ...)
```

### Separate Storage From Indexes

A graph is represented as a struct:

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

Nodes and edges remain plists for flexibility:

```elisp
(:id "n1" :label "Note" :title "Hello")
```

```elisp
(:id "e1" :source "n1" :target "n2" :type "LINKS_TO")
```

### Direction Is Explicit

Edges are always stored as directed: `source -> target`.

Queries and traversals choose direction:

```elisp
:direction :out
:direction :in
:direction :any
```

## Implementation Summary

Implemented features:

1. Graph core
   - `netz-create-graph`
   - `netz-add-node`
   - `netz-add-edge`
   - `netz-get-node`
   - `netz-get-edge`
   - `netz-node-edge-ids`

2. Pattern matcher
   - `(:node var props...)`
   - `(:edge a b props...)`
   - binding rows
   - repeated binding consistency
   - anonymous `_` bindings

3. `netz-query`
   - keyword clauses
   - matcher execution
   - optional `:where`
   - return variants

4. Mutation clauses
   - `:create`
   - `:merge`
   - `:set`
   - `:delete`
   - `:detach-delete`

5. Path support
   - depth-limited traversal over indexes
   - edge property constraints
   - zero-depth paths
   - cycle-safe traversal with no repeated edges per route
   - full route binding via `:as`
