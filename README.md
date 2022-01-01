[![test](https://github.com/toshism/netz/actions/workflows/test.yml/badge.svg)](https://github.com/toshism/netz/actions/workflows/test.yml)

# NETZ

*work in progress*

emacs generic graph store

## Functions

Graph can be the name of a graph or the actual graph structure itself.

### Graphs

* [`netz-make-graph`](#netz-make-graph-name-optional-path-save) `(name &optional path save)`
* [`netz-save-graph`](#netz-save-graph-graph) `(graph)`
* [`netz-load-graph`](#netz-load-graph-path) `(path)`
* [`netz-reload-graph`](#netz-reload-graph-graph) `(graph)`
* [`netz-get-graph`](#netz-get-graph-name) `(name)`
* [`netz-copy-graph`](#netz-copy-graph-graph-new-name-optional-new-path) `(graph new-name &optional new-path)`
* [`netz-filtered-graph`](#netz-filtered-graph-filter-graph-new-graph) `(filter graph new-graph)`

### Nodes

* [`netz-add-node`](#netz-add-node-node-graph) `(node graph)`
* [`netz-get-node`](#netz-get-node-node-id-graph) `(node-id graph)`
* [`netz-get-nodes`](#netz-get-nodes-graph) `(graph)`
* [`netz-delete-node`](#netz-delete-node-node-graph) `(node graph)`

### Edges

* [`netz-add-edge`](#netz-add-edge-edge-graph) `(edge graph)`
* [`netz-get-edge`](#netz-get-edge-edge-id-graph) `(edge-id graph)`
* [`netz-get-edges`](#netz-get-edges-graph) `(graph)`
* [`netz-delete-edge`](#netz-delete-edge-edge-graph) `(edge graph)`

### Relationships

* [`netz-connect-nodes`](#netz-connect-nodes-source-target-edge-params-graph) `(source target edge-params graph)` -- probably just make this netz-add-edge?
* [`netz-get-node-hood`](#netz-get-node-hood-id-graph-optional-new-graph-edge-filter-directed) `(id graph &optional new-graph edge-filter directed)`
* [`netz-bfs-shortest-path`](#netz-bfs-shortest-path-source-target-graph-optional-directed) `(source target graph &optional directed)`
* [`netz-get-related-by`](#netz-get-related-by-node-graph-key-by-new-name-directed) `(node graph &key by new-name directed)`

### Filtering

- :match (property value)
- :and (:match ...)
- :or (:match ..)

## Graphs

Functions for dealing with a graph object.

### netz-make-graph `(name &optional path save)`

Create and return a new graph with `name`. Optionally provide a system `path` of the location for the save file. If `save` is non-nil the graph will be immediately written to the file `path`.

Default `path` is `(user-emacs-directory)/.netz-graph/(name)`.

Create graph named `:test` in memory. Graphs can be named with keywords.
``` emacs-lisp
(netz-make-graph :test)
```

Create graph named `test` and save it to provided path. Graphs can be named with strings.
``` emacs-lisp
(netz-make-graph "test" "/home/toshism/graphs/test-graph" t)
```

### netz-save-graph `(graph)`

Save `graph` to disk. `graph` can be the name or the full graph structure.

Save `graph` named `:test`
``` emacs-lisp
(netz-save-graph :test)
```

Save provided `graph`.

``` emacs-lisp
(setq test-graph (make-graph :test))
(netz-save-graph test-graph)
```

### netz-load-graph `(path)`

Load graph from provided file `path`.

``` emacs-lisp
(netz-load-graph "/home/toshism/.emacs.d/.netz-graph/test")
```

``` emacs-lisp
(setq test-graph (make-graph :test))
(netz-load-graph (plist-get test-graph :path))
```

### netz-reload-graph `(graph)`

Reload and return `graph` from disk.

``` emacs-lisp
(netz-reload-graph :test)
```

### netz-get-graph `(name)`

Return graph with `name`.

``` emacs-lisp
(netz-get-graph :test)
```

### netz-copy-graph `(graph new-name &optional new-path)`

Create and return new `graph` named `new-name` with optional `new-path`.

``` emacs-lisp
(netz-copy-graph :test :new-test)
```

### netz-filtered-graph `(filter graph new-graph)`

Create and return a `graph` with name `new-graph` filtered by `filter`.

``` emacs-lisp
(netz-filtered-graph (:edges (:or (:match :type "LINKS_TO")
                                  (:match :type "TAGGED")))
                     :test :linked-graph)
```

## Nodes

Nodes are a plist with a single mandatory unique property named `:id`.
Example:
`'(:id 1 :label "Tag" :name "emacs")`

Nodes also store a record of connected edges in the `:edges` property. This is automatically maintained by netz when relationships are managed through the provided functions.

### netz-add-node `(node graph)`

Add `node` to `graph`. If node already exists it will be merged with the existing node. New values take precedence. See `netz-merge-plists` for details. Returns the updated `graph`.

Add a node with `:id` of 1 and a `:label` of "Note" to `:test` graph.
``` emacs-lisp
(netz-add-node '(:id 1 :label "Note") :test)
```

If we then call

``` emacs-lisp
(netz-add-node '(:id 1 :label "Tag" :name "test") :test)
```
Node with id 1 will be equal to `(:id 1 :label "Tag" :name "test")`

### netz-get-node `(node-id graph)`

Return node with `node-id` from `graph`.

``` emacs-lisp
(netz-get-node 1 :test)
```

### netz-get-nodes `(graph)`

Return a hash table of all nodes from `graph` with node `:id`s as the keys.

``` emacs-lisp
(netz-get-nodes :test)
```

### netz-delete-node `(node graph)`

Delete `node` from `graph`.

``` emacs-lisp
(netz-delete-node '(:id 1) :test)
```

## Edges

Edges are a plist with a single mandatory unique proprty named `:id` which is a list of node ids. `:id` is a list of source and target ids.
Example:
`'(:id (1 2) :type "TAGGED")`

### netz-add-edge `(edge graph)`

Add `edge` to `graph`.

``` emacs-lisp
(netz-add-edge '(:id (1 2) :type "TAGGED") :test)
```

### netz-get-edge `(edge-id graph)`

Get edge with `edge-id` from `graph`.

``` emacs-lisp
(netz-get-edge '(1 2) :test)
```

### netz-get-edges `(graph)`

Return a hash table of all edges from `graph` with edge `:id`s as the keys.

``` emacs-lisp
(netz-get-edges :test)
```

### netz-delete-edge `(edge graph)`

Delete `edge` from `graph`.

``` emacs-lisp
(netz-delete-edge '(:id (1 2) :type "TAGGED") :test)
```

## Relationships

### netz-connect-nodes `(source target edge-params graph)`

Connect `source` to `target` with edge having `edge-params` in `graph`.

``` emacs-lisp
(setq one (netz-get-node 1 :test)
(setq two (netz-get-node 2 :test)
(netz-connect-nodes one two '(:type "LINKS_TO") :test)
```

### netz-get-node-hood `(id graph &optional new-graph edge-filter directed)`

Get the neighborhood for node with `id` in `graph`.
*optional*
If `new-graph` is nil modify graph in place, if it is non-nil return a new graph named `new-graph`.
`edge-filter` limits the neighborhood by properties of the edges.
`directed` is a boolean value to specify if the neighborhood should consider edge direction in a directed graph.

``` emacs-lisp
(netz-get-node-hood 1 :test)
```

Return a new graph named "hood" for node id 1 and connected nodes through a "RELATED" property.

``` emacs-lisp
(netz-get-node-hood 1 :test :hood (:match :type "RELATED"))
```
See `filtering` for more details on filtering syntax.

### netz-bfs-shortest-path `(source target graph &optional directed)`

Return the shortest path, as a list of node ids, between `source` and `target` in `graph`.
*optional*
If `directed` is nil ignore direction.

``` emacs-lisp
(setq start (netz-get-node 1 :test))
(setq end (netz-get-node 2 :test))
(netz-bfs-shortest-path start end :test)
```

### netz-get-related-by `(node graph &key by new-name directed)`

Return a graph named `new-name` that contains nodes related to `node` with edges properties `by` in `graph`, optionally `directed`.

Sounds confusing but it's similar to neighborhood.

``` emacs-lisp
(netz-get-related-by (netz-get-node 1 :test) :test '(:match :type "LINKS_TO") :links-to t)
```
