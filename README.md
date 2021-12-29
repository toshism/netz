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
* netz-reload-graph (graph)
* netz-get-graph (name)
* netz-copy-graph (graph new-name &optional new-path)
* netz-filtered-graph (filter graph new-graph)

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

Default `path` is `user-emacs-directory`/.netz-graph/`name`.

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

``` emacs-lisp
(netz-save-graph :test)
```

### netz-load-graph `(path)`

Load graph from provided file `path`.

``` emacs-lisp
(netz-load-graph "/home/toshism/.emacs.d/.netz-graph/test")
```
