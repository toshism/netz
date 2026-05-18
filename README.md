[![tests](https://github.com/toshism/netz/actions/workflows/test.yml/badge.svg)](https://github.com/toshism/netz/actions/workflows/test.yml)

# NETZ

Emacs generic graph store.

`netz` is a Lisp-native directed property graph store for Emacs. Graphs live in memory and can be persisted to disk. Nodes, edges, query rows, and paths are all ordinary plist-friendly data structures so they are easy to inspect, transform, and use from other Emacs packages.

## Overview

`netz` provides a directed property graph core and a Cypher-inspired query DSL. The documented behavior is covered by tests.

## Data Model

A graph is a `cl-defstruct` named `netz-graph` with separate storage and indexes:

```elisp
(cl-defstruct netz-graph
  name
  path
  nodes
  edges
  out-index
  in-index)
```

Nodes are property lists with a required `:id`:

```elisp
(:id "person:tosh" :label "Person" :name "Tosh")
```

Edges are directed property lists with required `:id`, `:source`, and `:target`:

```elisp
(:id "works:tosh:netz"
 :source "person:tosh"
 :target "project:netz"
 :type "WORKS_ON")
```

Edges have their own IDs, so multiple distinct relationships between the same nodes are supported:

```elisp
(:id "e1" :source "a" :target "b" :type "LIKES")
(:id "e2" :source "a" :target "b" :type "KNOWS")
```

Indexes are maintained automatically:

- `out-index`: source node id -> outgoing edge ids
- `in-index`: target node id -> incoming edge ids

## Quick Start

```elisp
(require 'netz)
(require 'netz-query)

(setq graph (netz-create-graph :example))

(netz-add-node graph '(:id "person:tosh" :label "Person" :name "Tosh"))
(netz-add-node graph '(:id "project:netz" :label "Project" :name "netz" :stars 12))
(netz-add-edge graph '(:id "works:tosh:netz"
                       :source "person:tosh"
                       :target "project:netz"
                       :type "WORKS_ON"))

(netz-query graph
  (:match
   (:node person :label "Person")
   (:edge person project :type "WORKS_ON" :direction :out)
   (:node project :label "Project"))
  (:where
   (> (netz-prop project :stars) 10))
  (:return person project))
```

Returns plist binding rows:

```elisp
((:person (:id "person:tosh" ...)
  :project (:id "project:netz" ...)))
```

## Core API

Graph arguments can generally be graph objects or registered graph names.

### Graphs

#### `netz-create-graph` `(name &key path save)`

Create and register an empty graph.

```elisp
(netz-create-graph :notes)
(netz-create-graph :notes :path "/tmp/notes.graph" :save t)
```

`netz-make-graph` is an alias for `netz-create-graph`.

#### `netz-get-graph` `(name)`

Return a registered graph by name.

```elisp
(netz-get-graph :notes)
```

#### `netz-save-graph` `(graph)`

Save a graph to its path.

```elisp
(netz-save-graph graph)
(netz-save-graph :notes)
```

#### `netz-load-graph` `(path)`

Load and register a graph from disk. Indexes are rebuilt on load.

```elisp
(netz-load-graph "/tmp/notes.graph")
```

#### `netz-reload-graph` `(graph)`

Reload a graph from its path.

#### `netz-copy-graph` `(graph new-name &optional new-path)`

Copy nodes and edges into a registered graph named `new-name`.

### Nodes

#### `netz-add-node` `(graph node)`

Add a node plist. Existing nodes are merged, with incoming properties winning.

```elisp
(netz-add-node graph '(:id "note:1" :label "Note" :title "Hello"))
```

#### `netz-get-node` `(graph node-id)`

```elisp
(netz-get-node graph "note:1")
```

#### `netz-get-nodes` `(graph)`

Return a list of all node plists.

#### `netz-delete-node` `(graph node-or-id &key detach)`

Delete a node. Nodes with incident edges require `:detach t`.

```elisp
(netz-delete-node graph "note:1" :detach t)
```

### Edges

#### `netz-add-edge` `(graph edge)`

Add a directed edge plist.

```elisp
(netz-add-edge graph '(:id "e1" :source "a" :target "b" :type "LINKS_TO"))
```

#### `netz-get-edge` `(graph edge-id)`

```elisp
(netz-get-edge graph "e1")
```

#### `netz-get-edges` `(graph)`

Return a list of all edge plists.

#### `netz-delete-edge` `(graph edge-or-id)`

```elisp
(netz-delete-edge graph "e1")
```

#### `netz-node-edge-ids` `(graph node-id &key direction)`

Return edge ids for a node. Direction is one of `:out`, `:in`, or `:any`.

```elisp
(netz-node-edge-ids graph "a" :direction :out)
```

## Query DSL

`netz-query` is a macro over binding rows. A query usually contains `:match`, optional `:where`, and `:return` clauses.

```elisp
(netz-query graph
  (:match
   (:node person :label "Person")
   (:edge person project :type "WORKS_ON" :direction :out)
   (:node project :label "Project"))
  (:return person project))
```

You can also query by registered graph name:

```elisp
(netz-query :notes
  (:match (:node note :label "Note"))
  (:return note))
```

### Node Patterns

```elisp
(:node binding &rest properties)
```

Examples:

```elisp
(:node note :label "Note")
(:node start :id "note:1")
(:node _ :label "Tag") ; anonymous binding
```

Repeated bindings are constrained to the same entity.

### Edge Patterns

```elisp
(:edge source target &rest properties)
```

By default edges match in any direction. Use `:direction` for explicit traversal:

```elisp
(:edge a b :type "LINKS_TO")                 ; any direction
(:edge a b :type "LINKS_TO" :direction :out)
(:edge a b :type "LINKS_TO" :direction :in)
```

Bind the matched edge with `:as`:

```elisp
(:edge person project :as rel :type "WORKS_ON" :direction :out)
```

### Where Clause

`:where` is ordinary Elisp evaluated with query variables lexically bound.

```elisp
(netz-query graph
  (:match (:node project :label "Project"))
  (:where (> (netz-prop project :stars) 10))
  (:return project))
```

### Return Clause

Binding rows:

```elisp
(:return person project)
;; => ((:person <node> :project <node>) ...)
```

Count:

```elisp
(:return :count)
```

IDs:

```elisp
(:return :ids person project)
```

Pluck a property:

```elisp
(:return :pluck person :name)
```

Return a graph containing matched row entities:

```elisp
(:return :graph :new-graph-name)
```

## Path Queries

Path queries find reachable nodes within a depth range.

```elisp
(netz-query graph
  (:match
   (:node start :id "person:tosh")
   (:path start target
    :direction :out
    :depth (1 3)))
  (:return target))
```

Options:

```elisp
:path source target
:edge (:type "LINKS_TO")
:direction :out | :in | :any
:depth (min max)
:as route
```

### Full Route Binding

Use `:as` to bind the full path route:

```elisp
(netz-query graph
  (:match
   (:node start :id "person:tosh")
   (:path start target
    :as route
    :direction :out
    :depth (1 3))
   (:node target :label "Tag"))
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
- `:steps` records the exact traversal, including whether an edge was traversed `:out` or `:in`

The embedded `:graph` contains the route's nodes and edges so existing graph tools can operate on the path as a graph.

### Checking Whether Two Nodes Are Connected

```elisp
(netz-query graph
  (:match
   (:node a :id "person:tosh")
   (:node b :id "tag:emacs")
   (:path a b :as route :direction :any :depth (1 5)))
  (:return route))
```

Returns route rows if connected, or `nil` if no path exists.

For yes/no:

```elisp
(> (netz-query graph
     (:match
      (:node a :id "person:tosh")
      (:node b :id "tag:emacs")
      (:path a b :direction :any :depth (1 5)))
     (:return :count))
   0)
```

## Mutations

Mutation clauses use the same pattern language.

### Create

Creates entities and errors on duplicate ids.

```elisp
(netz-query graph
  (:create
   (:node person :id "person:tosh" :label "Person" :name "Tosh")
   (:node project :id "project:netz" :label "Project" :name "netz")
   (:edge person project :id "works:tosh:netz" :type "WORKS_ON"))
  (:return person project))
```

### Merge

Find-or-create by `:id`.

```elisp
(netz-query graph
  (:merge
   (:node person :id "person:tosh" :label "Person"))
  (:set person :name "Tosh" :active t)
  (:return person))
```

### Set

Update properties on a bound node or edge.

```elisp
(:set person :name "Tosh" :active t)
(:set rel :since 2026)
```

### Delete

Delete matched entities.

```elisp
(netz-query graph
  (:match (:node tmp :label "Temp"))
  (:delete tmp))
```

### Detach Delete

Delete matched nodes and their incident edges.

```elisp
(netz-query graph
  (:match (:node note :id "note:1"))
  (:detach-delete note))
```

## Operational Characteristics

- Queries scan nodes/edges for property matching beyond id/index lookups.
- Path queries return every route found within the depth range and can grow quickly on dense graphs.
- Mutation queries mutate directly.

See `docs/query-dsl.md` for the query DSL reference.
