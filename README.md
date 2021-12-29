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

* netz-add-node (node graph)
* netz-get-node (node-id graph)
* netz-get-nodes (graph)
* netz-delete-node (node graph)

### Edges

* netz-add-edge (edge graph)
* netz-get-edge (edge-id graph)
* netz-get-edges (graph)
* netz-delete-edge (edge graph)

### Relationships

* netz-connect-nodes (source target edge-params graph) -- probably just make this netz-add-edge?
* netz-get-node-hood (id graph &optional new-graph edge-filter directed)
* netz-bfs-shortest-path (source target graph &optional directed)
* netz-get-related-by (node graph &key by new-name directed)

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
